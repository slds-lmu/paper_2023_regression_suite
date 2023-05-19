library("mlr3verse")
library("mlr3extralearners")
library("here")
library("mlr3tuningspaces")
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

if (is.null(getOption("mlr3oml.cache"))) {
  # within the project directory
  options(mlr3oml.cache = here("data", "original"))
}

### Defining the parametes

TEST = TRUE # whether to conduct a test run from which to estimate the total runtime
SEED = 1 # is set here but is also used in batchtools
set.seed(SEED)
if (TEST) {
  EVALS_XGBOOST = 2
} else {
  EVALS_XGBOOST = 500
}

LEARNERS = c("ranger", "rpart", "xgboost", "gam", "cv_glmnet")

task_ids_all = read.csv("ids.csv")[["id"]]
tbl_task_all = list_oml_tasks(task_id = task_ids_all)
data_ids_all = tbl_task_all$data_id
tbl_data_all = list_oml_data(data_id = data_ids_all)

task_ids = task_ids_all

tasks = lapply(task_ids, function(task_id) tsk("oml", task_id = task_id))

# We use the same resamplings in the test run and the real run.
# However later, we will only submit the first iteration of each resampling.
resamplings = lapply(task_ids, function(task_id) rsmp("oml", task_id = task_id))

#' @description
#' This function is used to create the inner resampling for the tuning.
#' It uses the same heuristic as the one we use to create the outer resampling
make_inner_resampling = function(task) {
  if (TEST) {
    return(rsmp("holdout"))
  }
  if (task$nrow <= 1000) {
    resampling = rsmp("repeated_cv", repeats = 10, folds = 10)
  } else if (task$nrow <= 100000) {
    resampling = rsmp("cv", folds = 10)
  } else {
    resampling = rsmp("holdout")
  }

  return(resampling)
}

#' @description
#' Creates the gam formula with the follwing rule:
#' * numerics / integers with more than 20 unique values get a smooth (s()) effect with the default 10 knots
#' * Other numerics (and factors of course) get no smooth effect
make_gam_formula = function(task) {
  feature_types = task$feature_types
  features = feature_types$id
  effects = lapply(features, function(feature) {
    type = feature_types[id == feature, "type"][[1L]]
    column = task$data(cols = feature)[[1L]]
    n_unique = length(unique(column))
    if (type %in% c("integer", "numeric") && n_unique >= 20) {
      effect = sprintf("s(%s)", feature)
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
  base_learner = lrn(paste0("regr.", learner_id))
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

  } else if (learner_id == "gam") {
    # This was incorrect in mlr3extralearners, new release will fix this
    base_learner$feature_types = union(base_learner$feature_types, "factor")
    base_learner$param_set$set_values(
      formula = make_gam_formula(task)
    )
    learner = as_learner(
      ppl("robustify", learner = base_learner, task = task) %>>% base_learner
    )
  } else if (learner_id == "xgboost") {
    learner_tuned = auto_tuner(
      tuner = tnr("random_search", batch_size = 100), # this does not restrict us as long as we use less than 100 cores
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
  } else {
    stopf("Unsupported learner %s.", learner_id)
  }
  learner$fallback = lrn("regr.featureless")
  learner$encapsulate = c(train = "try", predict = "try")
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

if (TEST) {
  saveRDS(design, "design-test.rds")
}

setattr(design, "class", c("benchmark_grid", "data.table", "data.frame"))

if (TEST) {
  unlink("experiments-test", recursive = TRUE)
  reg = makeExperimentRegistry("experiments-test", seed = SEED, packages = "mlr3verse")
} else {
  if (!dir.exists("experiments")) {
    reg = makeExperimentRegistry("experiments", seed = SEED, packages = "mlr3verse")
  } else {
    reg = loadRegistry("experiments", writeable = TRUE)
  }
A

if (path.expand("~") == "/home/sebi") {
  reg$cluster.functions = makeClusterFunctionsInteractive()
}

batchmark(design, store_models = FALSE, reg = reg)

if (TEST) {

  job_table = getJobTable(reg = reg)
  job_table = unwrap(job_table)
  job_table = job_table[,
    .(job.id, learner_id, task_id, resampling_id, repl, learner_hash)
  ]
  # We get one ID of each experiment.
  # W
  unique_ids = job_table[repl == 1, .(job.id)][[1L]]
  # submitJobs(unique_ids)
  submitJobs(unique_ids, reg = reg)
  # submitJos()
} else {
  stop("Not defined yet.")

}

waitForJobs()

bmr = reduceResultsBatchmark()
