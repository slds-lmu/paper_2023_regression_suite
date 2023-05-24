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

stopifnot(max(table(unique_chunks)) <= 100)

summary = data.table(
    learner_id = info$learner_id,
    task_id = info$task_id,
    walltime = unlist(map(info$new_resources, "walltime")),
    ncpus = unlist(map(info$new_resources, "ncpus")),
    memory = unlist(map(info$new_resources, "memory")),
    chunk = info$chunk
)


expired_or_not_submitted = union(findNotSubmitted()[[1]], findExpired()[[1]])
info = info[job.id %in% expired_or_not_submitted, ]
unique_chunks = unique(info$chunk)

# maybe we should sort the chunks by runtime

i = 1
for (unique_chunk in unique_chunks) {
    job_subset = info[chunk == unique_chunk, ]
    submission_table = data.table(job.id = job_subset$job.id, chunk = unique_chunk)
    resources = job_subset$new_resources[[1L]]
    resources$ncpus = 16
    resources$partition = "teton"
    resources$walltime = resources$walltime * 1.5
    resources$memory = 5000
    submitJobs(submission_table, resources = resources)
}

