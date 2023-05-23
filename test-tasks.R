library(checkmate)
library(mlr3misc)
library(mlr3oml)
library(mlr3)
ids = read.csv("ids.csv")[[1L]]

stopifnot(length(ids) == 36)

otasks = lapply(ids, otsk)

ids = map_chr(otasks, "name")
stopifnot(length(ids) == length(unique(ids)))


# the resamplings have the correct splits.
for (otask in otasks) {
  stopifnot(otask$nrow <= 100000)
  stopifnot(otask$ncol <= 1000)
  if (otask$data$nrow <= 1000) {
    stopifnot(otask$estimation_procedure$type == "crossvalidation")
    stopifnot(otask$estimation_procedure$parameter[name == "number_repeats", "value"][[1L]] == 10)
    stopifnot(otask$estimation_procedure$parameter[name == "number_folds", "value"][[1L]] == 10)
  } else if (otask$data$nrow <= 100000) {
    stopifnot(otask$estimation_procedure$type == "crossvalidation")
    stopifnot(otask$estimation_procedure$parameter[name == "number_repeats", "value"][[1L]] == 1)
    stopifnot(otask$estimation_procedure$parameter[name == "number_folds", "value"][[1L]] == 10)
  } else {
    stopifnot(otask$estimation_procedure$type == "holdout")
    stopifnot(otask$estimation_procedure$parameter[name == "percentage", "value"][[1L]] == 33)
  }
}

for (otask in otasks) {
  task = as_task(otask)
  checkmate::expect_set_equal(task$feature_names, otask$feature_names)
  checkmate::expect_set_equal(task$target_names, otask$target_names)
}
