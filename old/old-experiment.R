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

### Defining the parametes

TEST = FALSE # whether to conduct a test run from which to estimate the total runtime
SEED = 1 # is set here but is also used in batchtools
set.seed(SEED)
if (TEST) {
  EVALS_XGBOOST = 2
} else {
  EVALS_XGBOOST = 500
}

TUNING_BATCH_SIZE = 10

if (TEST) {
  LEARNERS = c("ranger", "rpart", "xgboost", "gam", "cv_glmnet")
} else {
  # might add the gam later again when the rest is done
  LEARNERS = c("ranger", "rpart", "xgboost", "cv_glmnet")
}

# Define tasks and resmaplings
task_ids = read.csv("ids.csv")[["id"]]
tasks = lapply(task_ids, function(task_id) tsk("oml", task_id = task_id))
resamplings = lapply(task_ids, function(task_id) rsmp("oml", task_id = task_id))

#' @description
#' This function is used to create the inner resampling for the tuning.
#' It uses the same heuristic as the one we use to create the outer resampling
make_inner_resampling = function(task) {
  if (TEST) {
    return(rsmp("holdout"))
  }
  if (task$nrow <= 10000) {
    resampling = rsmp("cv", folds = 10)
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
  } else if (learner_id == "xgboost") {
    if (TEST) {
      batch_size = 2
    } else {
      batch_size = TUNING_BATCH_SIZE
    }

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
  } else {
    stopf("Unsupported learner %s.", learner_id)
  }
  #  } else if (learner_id == "gam") {
  #    # This was incorrect in mlr3extralearners, new release will fix this
  #    base_learner$feature_types = union(base_learner$feature_types, "factor")
  #    base_learner$param_set$set_values(
  #      formula = make_gam_formula(task)
  #    )
  #    learner = as_learner(
  #      ppl("robustify", learner = base_learner, task = task) %>>% base_learner
  #    )
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

setattr(design, "class", c("benchmark_grid", "data.table", "data.frame"))

if (TEST) {
  test_path = "/gscratch/sfische6/experiments-test"
  if (dir.exists(test_path)) {
    reg = loadRegistry(test_path, writeable = TRUE)
  } else {
    reg = makeExperimentRegistry(
      test_path,
      seed = SEED,
      package = "mlr3verse",
      work.dir = "/home/sfische6/paper_2023_regression_suite"
    )
  }
} else {
  reg_path = "/gscratch/sfische6/experiments-final"
  if (dir.exists(reg_path)) {
    reg = loadRegistry(reg_path, writeable = TRUE)
  } else {
    # We need to set the working directory to parallelize xgboost
    reg = makeExperimentRegistry(
      reg_path, 
      seed = SEED, 
      package = "mlr3verse",
      work.dir = "/home/sfische6/paper_2023_regression_suite"
      )
  }
}

batchmark(design, store_models = FALSE, reg = reg)



if (TEST) {
  resources_small = list(walltime = 100L, memory = 1000, ntasks = 1L, ncpus = 1L, nodes = 1L, clusters = "beartooth")
  resources_medium = list(walltime = 600L, memory = 3000, ntasks = 1L, ncpus = 1L, nodes = 1L, clusters = "beartooth")
  resources_large = list(walltime = 1200L, memory = 6000, ntasks = 1L, ncpus = 1L, nodes = 1L, clusters = "beartooth")

  is_gam = grepl(mlr3misc::ids(design$learner), pattern = "regr.gam")
  task_table = data.table(
    task_id = mlr3misc::ids(design[is_gam, ]$task),
    nrow = mlr3misc::map_int(design[is_gam, ]$task, "nrow"),
    ncol = mlr3misc::map_int(design[is_gam, ]$task, "ncol")
  )

  job_table = getJobTable(reg = reg)
  job_table = unwrap(job_table)
  job_table = job_table[,
    .(job.id, learner_id, task_id, resampling_id, repl, learner_hash)
  ]
  job_table = merge(job_table, task_table, by = "task_id", all.x = TRUE)
  # We get one ID of each experiment.
  # W
  unique_ids = job_table[repl == 1, .(job.id)][[1L]]
  # submitJobs(unique_ids)
  
  # TODO: Also differentiate according to Learner maybe
  for (unique_id in unique_ids) {
    nrow = job_table[job.id == unique_id, "nrow"][[1L]]
    learner_id = job_table[job.id == unique_id, "learner_id"][[1L]]
    is_xgboost = grepl(learner_id, pattern = "regr.xgboost")
    is_ranger = grepl(learner_id, pattern = "regr.ranger")
    is_gam = grepl(learner_id, pattern = "regr.gam")
    if (nrow <= 1000) {
      resources = resources_small
    } else if (nrow <= 10000) {
      resources = resources_medium
    } else if (nrow <= 100000) {
      resources = resources_large
    } else {
      stop("Too many observations")
    }
    if (is_xgboost) {
      resources$ncpus = 2L
      resources$walltime = resources$walltime * 6
    }
    if (is_gam) {
      resources$walltime = resources$walltime * 2
    }
    if (is_ranger) {
      resources$walltime = resources$walltime * 3
    }
    submitJobs(unique_id, resources = resources)
  }
} else {
  stop("Not defined yet.")
}