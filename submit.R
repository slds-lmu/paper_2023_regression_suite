library(batchtools)
library(mlr3misc)
library(data.table)
library(checkmate)

reg = loadRegistry("/gscratch/sfische6/experiments", writeable = TRUE)

info = readRDS("submission_table.rds")

jt = getJobTable(reg = reg)
assert_set_equal(jt$job.id, info$job.id)
assert_true(nrow(jt) == nrow(info))

unique_chunks = unique(info$chunk)

summary = data.table(
    learner_id = info$learner_id,
    task_id = info$task_id,
    walltime = unlist(map(info$new_resources, "walltime")),
    ncpus = unlist(map(info$new_resources, "ncpus")),
    memory = unlist(map(info$new_resources, "memory")),
    chunk = info$chunk
)

info = info[job.id %in% findNotSubmitted()[[1L]], ]
unique_chunks = unique(info$chunk)

for (unique_chunk in unique_chunks) {
    job_subset = info[chunk == unique_chunk, ]
    submission_table = data.table(job.id = job_subset$job.id, chunk = unique_chunk)
    resources = job_subset$new_resources[[1L]]
    resources$walltime = resources$walltime * 2
    submitJobs(submission_table, resources = resources)
}

## To continue

library(batchtools)
reg = loadRegistry("/gscratch/sfische6/experiments", writeable = TRUE)

expired = findExpired()[[1L]]
info = readRDS("submission_table.rds")

info = info[job.id %in% expired, ]

for (unique_chunk in unique_chunks) {
    job_subset = info[chunk == unique_chunk, ]
    submission_table = data.table(job.id = job_subset$job.id, chunk = unique_chunk)
    resources = job_subset$new_resources[[1L]]
    resources$walltime = resources$walltime * 2
    submitJobs(submission_table, resources = resources)
}



