library(mlr3benchmark)
library(mlr3batchmark)
library(batchtools)
library(ggplot2)

aggr2 = readRDS(here::here("results", "aggr.rds"))

aggr2$learner_id = sapply(aggr2$learner_id, function(learner_id) {
    if (learner_id == "ranger") {
        "Random Forest"
    } else if (learner_id == "xgboost") {
        "XGBoost"
    } else if (learner_id == "gam") {
        "GAM"
    } else if (learner_id == "cv_glmnet") {
        "Ridge Regression"
    } else if (learner_id == "rpart") {
        "Regression Tree"
    } else {
        stop("No valid id.")
    }
})

aggr2$learner_id = as.factor(aggr2$learner_id)

bma2 = BenchmarkAggr$new(aggr2)

plot_cd = autoplot(bma2, type = "cd")

ggsave(plot = plot_cd, here::here("results", "critical_difference.png"), dpi = 300)

