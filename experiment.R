library("mlr3verse")
library("mlr3extralearners")
library("here")
library("mlr3tuningspaces")
library("mlr3batchmark")
library("batchtools")
library("mlr3oml")
library("mlr3tuning")
library("data.table")
library("mlr3misc")
library("mgcv")
library("ranger")
library("xgboost")
library("glmnet")
library("rpart")

source("LearnerRegrGamCustom.R")

### Defining the parametes

SEED = 1 # is set here but is also used in batchtools
set.seed(SEED)
EVALS_XGBOOST = 500

TUNING_BATCH_SIZE = 10

LEARNERS = c("ranger", "rpart", "xgboost", "cv_glmnet", "gam_custom")

# Define tasks and resmaplings
task_ids = read.csv("ids.csv")[["id"]]
tasks = lapply(task_ids, function(task_id) tsk("oml", task_id = task_id))
resamplings = lapply(task_ids, function(task_id) rsmp("oml", task_id = task_id))
lapply(seq_along(resamplings), function(i) resamplings[[i]]$id = tasks[[i]]$id)

#' @description
#' This function is used to create the inner resampling for the tuning.
#' It uses the same heuristic as the one we use to create the outer resampling

make_inner_resampling = function(task) {
  if (task$nrow <= 1000) {
    resampling = rsmp("cv", folds = 10)
  }  else if (task$nrow <= 10000) {
    resampling = rsmp("cv", folds = 3)
  } else if (task$nrow <= 100000) {
    resampling = rsmp("holdout")
  }  else {
    stop("too many observations")
  }

  return(resampling)
}

# @description
# Creates the gam formula with the follwing rule:
# * numerics / integers with more than 20 unique values get a smooth (s()) effect with the default 10 knots
# * Other numerics (and factors of course) get no smooth effect
# Problem: preprocess and formula
make_gam_formula = function(task) {
  feature_types = task$feature_types
  features = feature_types$id
  # Simple heuristic to determine the knots (otherwise we get more params than obs)
  if (task$nrow / task$ncol < 10) {
    k = "5"
  } else {
    k = "10"
  }
  effects = lapply(features, function(feature) {
    type = feature_types[id == feature, "type"][[1L]]
    column = task$data(cols = feature)[[1L]]
    n_unique = length(unique(column))
    if (type %in% c("integer", "numeric") && n_unique >= 20) {
      effect = sprintf("s(%s, k = )", feature, k)
    } else {
      effect = feature
    }
    return(effect)
  })
  effects = unlist(effects)
  form = as.formula(sprintf("%s ~ %s", task$target_names, paste0(effects, collapse = " + ")),
    env = NULL
  )
  return(form)
}

# @description
# This learner is used to construct the actual mlr3::Learner from the ids passed through LEARNERS.
# We robustify the learner and in the case of xgboost we wrap the (robustified) learner in an mlr3tuning::AutoTuner.
make_learner = function(learner_id, task) {
  if (learner_id == "gam_custom") {
    base_learner = LearnerRegrGamCustom$new()
  } else {
    base_learner = lrn(paste0("regr.", learner_id))
  }
  if (learner_id == "rpart") {
    # xval = 10 is the default and this is switched off by mlr3 (we set it to 0 to save computation because this
    # learner is used in many tests etc.)
    base_learner$param_set$set_values(
      xval = 10
    )
    learner = as_learner(
      ppl("robustify", learner = base_learner, task = task) %>>% base_learner
    )
  } else if (learner_id == "ranger") {
    learner = as_learner(
      ppl("robustify", learner = base_learner, task = task) %>>% base_learner
    )
  } else if (learner_id == "cv_glmnet") {
    if (task$nrow <= 1000) {
      nfolds = 20
    } else {
      nfolds = 10
    }
    base_learner$param_set$set_values(
      alpha = 0, # we use ridge regression
      nfolds = nfolds,
      standardize = TRUE # is default anyway
    )
    learner = as_learner(
      ppl("robustify", learner = base_learner, task = task) %>>% base_learner
    )
  } else if (learner_id == "xgboost") {
    batch_size = TUNING_BATCH_SIZE

    learner_tuned = auto_tuner(
      tuner = tnr("random_search", batch_size = batch_size), # this does not restrict us as long as we use less than 100 cores
      # (we still stop after term_evals iteration, this just controls tradeoff between parallelization and how
      # exact we respect the stopping criterion)
      search_space = lts("regr.xgboost.default"),
      learner = base_learner,
      resampling = make_inner_resampling(task),
      measure = msr("regr.mse"),
      term_evals = EVALS_XGBOOST
    )
    learner = as_learner(
      ppl("robustify", learner = learner_tuned, task = task) %>>% learner_tuned
    )
  } else if (learner_id == "gam_custom") {
    base_learner = LearnerRegrGamCustom$new()
    base_learner$param_set$set_values(
      formula_fn = make_gam_formula
    )
    learner = as_learner(
      ppl("robustify", learner = base_learner, task = task) %>>% base_learner
    )
  } else {
    stopf("Unsupported learner %s.", learner_id)
  }
  learner$fallback = lrn("regr.featureless")
  learner$encapsulate = c(train = "try", predict = "try")
  # This is important! Otherwise we are running into bugs from mlr3pipelines regarding the hashes.
  learner$id = paste0(learner_id, ".", task$id)
  return(learner)
}

designs = lapply(LEARNERS, function(learner_id) {
  learners = lapply(seq_along(tasks), function(i) {
    make_learner(learner_id, tasks[[i]])
  })

  design = data.table(
    task = tasks,
    learner = learners,
    resampling = resamplings
  )
  setattr(design, "class", c("benchmark_grid", "data.table", "data.frame"))
  design
})

names(designs) = LEARNERS

design = rbindlist(designs)
saveRDS(design, "design.rds")

setattr(design, "class", c("benchmark_grid", "data.table", "data.frame"))

reg_path = "/gscratch/sfische6/experiments"
if (dir.exists(reg_path)) {
  stop("Directory already exists.")
} else {
  # We need to set the working directory to parallelize xgboost
  reg = makeExperimentRegistry(
    reg_path, 
    seed = SEED, 
    packages = c("mlr3verse", "mlr3misc", "checkmate", "R6", "paradox"),
    work.dir = "/home/sfische6/paper_2023_regression_suite"
    )
}

saveRDS(design, "design.rds")

batchmark(design, store_models = FALSE, reg = reg)