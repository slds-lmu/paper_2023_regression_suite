source(here::here("experiment", "LearnerRegrGamCustom.R"))
library(mlr3verse)


learner = LearnerRegrGamCustom$new()

task = tsk("mtcars")

learner$param_set$set_values(
    formula_fn = function(task) return(mpg ~ am)
)

learner$train(task)
pred = learner$predict(task)
stopifnot(inherits(pred, "Prediction"))

stopifnot(all(as.character(learner$model$formula) == c("~", "mpg", "am")))
stopifnot("factor" %in% learner$feature_types)

learner$param_set$set_values(
    formula_fn = function(task) return(a ~ b)
)
attempt = try({learner$train(task)}, silent = TRUE)
stopifnot(inherits(attempt, "try-error"))

learner$param_set$set_values(
    formula_fn = mpg ~ am
)
attempt = try({learner$train(task)}, silent = TRUE)
stopifnot(inherits(attempt, "try-error"))