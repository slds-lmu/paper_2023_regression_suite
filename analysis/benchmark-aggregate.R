library(mlr3benchmark)
library(mlr3batchmark)
library(batchtools)
library(ggplot2)

reg = loadRegistry(getOption("registry_path_local"))

ids = getJobTable() |> unwrap()

bmr = reduceResultsBatchmark(ids)

aggr = bmr$aggregate(msr("regr.rmse"))
aggr = aggr[, .(task_id, learner_id, regr.rmse)]
colnames(aggr)[3] = "rmse"
aggr$learner_id = sapply(aggr$learner_id, \(x) gsub("\\..*", "", x, perl = TRUE))
aggr$task_id = as.factor(aggr$task_id)
aggr$learner_id = as.factor(aggr$learner_id)

bma = BenchmarkAggr$new(aggr, minimize = TRUE)
saveRDS(bma, here::here("results", "bma.rds"))
saveRDS(aggr, here::here("results", "aggr.rds"))
