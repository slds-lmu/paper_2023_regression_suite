library(batchtools)
reg = loadRegistry("/gscratch/sfische6/experiments")


done = findDone()[[1L]]

for (i in done) {
    if (any(grepl(getLog(i), pattern = "Error"))) i
}

expireds = findExpired()[[1L]]

for (expired in expireds) {
    print(getLog(expired))
}



library(batchtools)

unwrap(getJobTable(findNotDone()))[startsWith(learner_id, "xgboost")]


info = readRDS("submission_table.rds")
info = info[job.id %in% findDone()[[1L]] & startsWith(learner_id, "xgboost"), ]

jt = unwrap(getJobTable(findDone()))[startsWith(learner_id, "xgboost"), ]

m = merge(
    jt[, c("job.id", "time.running")],
    unwrap(info)[, c("walltime", "job.id")],
    by = "job.id"
)

sum(unwrap(info)$walltime) /  sum(jt$time.running)

in