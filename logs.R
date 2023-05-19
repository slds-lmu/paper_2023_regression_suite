library("batchtools")
library("mlr3verse")
library("mlr3batchmark")

reg = loadRegistry("experiments-test")

done_ids = findDone()[[1L]]

bmr = reduceResultsBatchmark()

results = lapply(done_ids, function(id) try(loadResult(id), silent = TRUE))
results_class = sapply(done_ids, \(result) class(result) == "try-error")


sum(sapply(results, function(result) class(result) == "try-error"))
whic

done_logs = lapply(done_ids, function(id) try(getLog(id), silent = TRUE))


kk
names(done_logs)
done_logs = done_logs[["751"]]
nolog = lapply(done_logs, function(log) if (any(grepl(log, pattern = "try"))) log else NULL)
names(nolog) = done_ids



lapply(done_logs, function(x) any(grepl(x = x, pattern = "Warning")))


