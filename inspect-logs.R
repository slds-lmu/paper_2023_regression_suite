library(batchtools)
reg = loadRegistry("/gscratch/sfische6/experiments", writeable = TRUE)


done = findDone()[[1L]]

inspect_ids = list()
for (i in done) {
    if (any(grepl(getLog(i), pattern = "Warning|Error"))) {
        inspect_ids[[length(inspect_ids) + 1]] = i
    }
}


gam_custom_ids = getJobTable() |> unwrap()
gam_custom_ids = gam_custom_ids[startsWith(learner_id, "gam_custom")]$job.id

inspect_ids = setdiff(inspect_ids, gam_custom_ids) |> unlist()

# 1374 - renv issue
# 1375 - renv
# 3921 - rerun
# 3923 - rerun
# 3924
# 3925
# 3926
# 3927
# 3928

to_return = c(3921, 3923, 3924, 3925, 3926, 3927, 3928)


gam_ids = getJobTable() |> unwrap()
gam_ids = gam_ids[startsWith(learner_id, "gam"), job.id]
inspect_ids = setdiff(inspect_ids, gam_ids) |> unlist()

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