library(mlr3oml)
library(checkmate)
library(testthat)
library(mlr3misc)
ids = read.csv("ids.csv")[[1L]]

otasks = lapply(ids, otsk)

options(mlr3oml.cache = TRUE)


# the resamplings have the correct splits.
for (otask in otasks) {
  if (otask$data$nrow <= 1000) {
    expect_true(otask$estimation_procedure$type == "crossvalidation")
    expect_true(otask$estimation_procedure$parameter[name == "number_repeats", "value"][[1L]] == 10)
    expect_true(otask$estimation_procedure$parameter[name == "number_folds", "value"][[1L]] == 10)
  } else if (otask$data$nrow <= 100000) {
    expect_true(otask$estimation_procedure$type == "crossvalidation")
    expect_true(otask$estimation_procedure$parameter[name == "number_repeats", "value"][[1L]] == 1)
    expect_true(otask$estimation_procedure$parameter[name == "number_folds", "value"][[1L]] == 10)
  } else {
    expect_true(otask$estimation_procedure$type == "holdout")
    expect_true(otask$estimation_procedure$parameter[name == "percentage", "value"][[1L]] == 33)
  }
}

design = readRDS("design-test.rds")

learner_ids = ids(design$learner)
learner_ids_base = c("xgboost", "gam", "cv_glmnet", "rpart", "ranger")

subsets = lapply(learner_ids_base, function(id) {
  which(grepl(learner_ids, pattern = id))
})

names(subsets) = learner_ids_base

# each learner is defined on all 40 tasks
for (learner_id_base in learner_ids_base) {
  design_subset = design[subsets[[learner_id_base]]]
  expect_true(nrow(design_subset) == 40)
  task_ids = ids(design_subset$task)
  expect_true(length(unique(task_ids)) == length(task_ids))

  for (i in seq_len(nrow(design_subset))) {
    learner = design_subset[i, "learner"][[1L]][[1L]]
    task = design_subset[i, "task"][[1L]][[1L]]
    resampling = design_subset[i, "resampling"][[1L]][[1L]]

    base_learner = learner$base_learner()
    expect_true(learner$base_learner()$id == paste0("regr.", learner_id_base))
    if (base_learner$id == "regr.gam") {
      expect_true(!is.null(base_learner$param_set$values$formula))
    } else if (base_learner$id == "regr.xgboost") {
      expect_true(length(learner$graph$pipeops$regr.xgboost.tuned$learner$instance_args$search_space$ids()) == 8)
    } else if (base_learner$id == "regr.cv_glmnet") {
      expect_true(base_learner$param_set$values$alpha == 0)
      if (task$nrow <= 1000) {
        expect_true(base_learner$param_set$values$nfolds == 20)
      } else {
        expect_true(base_learner$param_set$values$nfolds == 10)

      }

    } else if (base_learner$id == "regr.rpart") {
      expect_true(base_learner$param_set$values$xval == 10)
    } else if (base_learner$id == "regr.ranger") {
      # We change not values
    } else {
      stop("Something went wrong.")
    }
  }
}

otasks = lapply(ids, otsk)

for (otask in otasks) {
  task = as_task(otask)
  expect_set_equal(task$feature_names, otask$feature_names)
  expect_equal(task$target_names, otask$target_names)
}



learners = design$learner

for (learner in learners) {
  learener_id_base = learner_ids_base[grpel(learner_ids_base, )]
}

# gam formula correct
design_gam = design[subsets$gam, ]

learner_gam = design_gam[7, "learner"][[1L]][[1L]]
learner_gam_formula = learner_gam$param_set$values$regr.gam.formula
learner_gam_formula

task = design_gam[7, "task"][[1L]][[1L]]

lengths(task$backend$distinct(task$backend$rownames, cols = c(task$target_names, task$feature_names)))
