library(mlr3oml)
library(checkmate)
library(testthat)
library(mlr3misc)
library(batchtools)

# Here we test two things
# 1. The design is correctly implemented
# 2. The design is correctly translated into batchtools experiments,
#    We test this correctness because earlier there were some problems with the hashes
#    of the learners, see e.g. https://github.com/mlr-org/mlr3pipelines/issues/720
# 
# The correctness of the tasks is tested in test-tasks.R

# Note that when we check the hashes, we can only compare the lengths and not the hashes itself
# (e.g. for set equivalence)
# The reason is that the hashes can change after deserialization (we load the design from an .rds file)

# The design was used to create the experiments from the reg
design = readRDS(here::here("results/design.rds"))
reg = loadRegistry(getOption("registry_path"))


# 36 tasks on 5 learners
stopifnot(nrow(design) == 5 * 35)

learner_ids = ids(design$learner)
stopifnot(length(unique(learner_ids)) == length(learner_ids))
learner_hashes = map_chr(design$learner, "hash")
stopifnot(length(learner_hashes) == length(unique(learner_hashes)))
stopifnot(length(learner_hashes) == length(learner_ids))

resampling_hashes = map_chr(design$resampling, "hash")
task_hashes = map_chr(design$task, "hash")
stopifnot(length(unique(resampling_hashes)) == length(unique(task_hashes)))


learner_prefixes = c("xgboost", "gam", "cv_glmnet", "rpart", "ranger")

subsets = lapply(learner_prefixes, function(id) {
  which(grepl(learner_ids, pattern = id))
})

ids_stripped = strsplit(learner_ids, split = ".", fixed = TRUE)
ids_stripped = map_chr(ids_stripped, 1)
stopifnot(all(table(ids_stripped) == 35))
expect_set_equal(ids_stripped, learner_prefixes)

jt = unwrap(getJobTable())

stopifnot(length(unique(jt$resampling_hash)) == length(unique(resampling_hashes)))

# We have no un-anticipated hash colissions
# We achieved that by setting different learner ids for all learners in the design
stopifnot(length(unique(jt$learner_hash)) == nrow(design))

stopifnot(length(unique(jt$task_hash)) == nrow(design) / length(learner_prefixes))


# Test that the cv_glmnet is properly defined
get_exports = function(learner_prefix, task_id) {
  jt = unwrap(getJobTable())
  jt_subset = jt[startsWith(learner_id, learner_prefix) & task_id  == get("task_id", parent.env(environment()))]
  learner_hash = jt_subset[1, "learner_hash"][[1]][[1]]
  resampling_hash = jt_subset[1, "resampling_hash"][[1]][[1]]

  learner = mlr3batchmark:::get_export(learner_hash, reg = batchtools::getDefaultRegistry())
  resampling = mlr3batchmark:::get_export(resampling_hash, reg = batchtools::getDefaultRegistry())
  
  job = makeJob(jt_subset$job.id[[1]])
  task = job$problem$data

  list(task = task, learner = learner, resampling = resampling)
}

task_ids = unique(unwrap(getJobTable())$task_id)
jt = unwrap(getJobTable())
for (learner_prefix in learner_prefixes) {
    for (task_id in task_ids) {
        exports = get_exports(learner_prefix, task_id)
        learner = exports$learner
        task = exports$task
        resampling = exports$resampling
        if (task$hash != resampling$task_hash) {
            browser()
        }
        stopifnot(task$hash == resampling$task_hash)
        if (learner_prefix == "xgboost") {
            at = learner$graph$pipeops$regr.xgboost.tuned$learner
            if (task$nrow <= 1000) {
                if(!inherits(at$instance_args$resampling, "ResamplingCV")) {
                    browser()
                }
                stopifnot(inherits(at$instance_args$resampling, "ResamplingCV"))
                stopifnot(at$instance_args$resampling$param_set$values$folds == 10)
            }  else if (task$nrow <= 10000) {
                if(!inherits(at$instance_args$resampling, "ResamplingCV")) {
                    browser()
                }
                stopifnot(inherits(at$instance_args$resampling, "ResamplingCV"))
                stopifnot(at$instance_args$resampling$param_set$values$folds == 3)
            } else {
                stopifnot(inherits(at$instance_args$resampling, "ResamplingHoldout"))
            }
            stopifnot(at$instance_args$terminator$param_set$values$n_evals == 500)

        } else if (learner_prefix == "ranger") {
            # Here we don't change any params

        } else if (learner_prefix == "rpart") {
            stopifnot(learner$base_learner()$param_set$values$xval == 10)
        } else if (learner_prefix == "cv_glmnet") {
            stopifnot(learner$base_learner()$param_set$values$alpha == 0)
            nfolds = if (task$nrow <= 10000) 20 else 10
            stopifnot(learner$base_learner()$param_set$values$standardize)
        } else if (learner_prefix == "gam") {
            stopifnot(is.function(learner$base_learner()$param_set$values$formula_fn))

        } else {
            stop("Wrong learner prefix.")
        }
    }
}