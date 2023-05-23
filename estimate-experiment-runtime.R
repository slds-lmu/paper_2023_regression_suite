library(batchtools)
library(data.table)
library(mlr3oml)

reg_test = loadRegistry("/gscratch/sfische6/experiments-test")

# What we do here:

# We estimate the runtimes for the jobs using the test experiment:
# 1. We load the job table of the test experiments and subset it to those that finished 
#    (one relication for each taks-learner combo).
# 2. Join the table with the number of observations for the tasks (this we use as a proxy for the RAM usage)
# 3. We adjust the estimated runtimes
#    a) We multiply the runtime for xgboost to reflect the tuning (we ran the test experiment with 2 xgboost evals not 500)
#    b) We make more conservative estimates, i.e. for each task we take the maximum runtime of the same learner with 
#       tasks with less than task$nrow observations
#    c) We adjust for the parallelization with future (10 cpus for xgboost)

learner_ids = c("cv_glmnet", "xgboost", "rpart", "ranger", "gam")

translate_id = function(id) {
    if (grepl(id, pattern = "ranger")) {
        return("ranger")
    } else if (grepl(id, pattern = "xgboost", fixed = TRUE)) {
        return("xgboost")
    } else if (grepl(id, pattern = "rpart")) {
        return("rpart")
    } else if (grepl(id, pattern = "gam")) {
    # here we use "gam" and not "gam_custom", because the test experiment was with id "gam" but the
    # actual experiment with "gam_custom"
    # The regex works for both
        return("gam")
    } else if (grepl(id, pattern = "cv_glmnet")) {
        return("cv_glmnet")
    } else {
        stop("No valid id")
    }
}

test_table_path = "test-table-done.csv"

if (!file.exists(test_table_path)) {
    jt_test = unwrap(getJobTable(reg = reg_test)[job.id %in% findDone()[[1L]]])
    jt_test = jt_test[, c("job.id", "task_id", "learner_id", "memory", "ncpus", "walltime", "time.queued", "time.running")]
    
    jt_test$learner_id = sapply(jt_test$learner_id, translate_id)
    stopifnot(length(unique(jt_test$learner_id)) == 5)
    fwrite(jt_test, test_table_path)
} else {
    jt_test = fread(test_table_path)
}

if (file.exists("task_table.csv")) {
    task_table = fread("task_table.csv")
} else {
    ids = read.csv("ids.csv")[[1L]]
    task_table = list_oml_tasks(ids)
    # This is the mlr3 task id (for which we use the OpenML name)
    task_table$task_id = task_table$name
    task_table$name = NULL
    fwrite(task_table, "task_table.csv")
}


jt_test = merge(jt_test, task_table, by = "task_id", all.x = TRUE)

get_multiplier = function(nobs) {
    # The number of folds for tuning depends on the number of obsrevations of the task
    if (nobs <= 1000) {
        folds = 10
    } else if (nobs <= 10000) {
        folds = 3
    } else if (nobs <= 100000) {
        folds = 1
    } else {
        # Should not happen
        stop("Too many obs.")
    }
    nsearch = 500 
    # We parallelize using future using 10 cores and assume here that this gives 5x speedup
    cpus = 10
    multiplier = folds * nsearch / (cpus / 2)
    return(multiplier)
}

multiplier = sapply(jt_test$NumberOfInstances, get_multiplier)

jt_test$estimated_time = ifelse(jt_test$learner_id == "xgboost", multiplier * jt_test$time.running, jt_test$time.running)

# Make the runtime estimations more conservative:
task_ids = task_table$task_id

# For ranger and xgboost we are even more conservative to adjust killing of jobs
# for each learner-tas combo we use the maximum runtime of all jos with the same learner on tasks with less observations
# than the task at hand.
# This also accounts at least a little bit for the fact that we might have sampled small nrounds in the test run for "xgboost"

for (lid in c("ranger", "xgboost")) {
    for (tid in task_ids) {
        ncurrent = jt_test[learner_id == lid & task_id == tid, ]$NumberOfInstances[[1L]]
        max_runtime_smaller = max(jt_test[learner_id == lid & NumberOfInstances <= ncurrent, ]$estimated_time)
        jt_test[learner_id == lid & task_id == tid, estimated_time := max_runtime_smaller]
    }
}

# Now we have the jt_test table with the runtime estimates
# In the next step, we merge it with the real job table (by matching by task and learner)
# From jt_test we use the memory and the estimated_time to construct the resources
# for the final job submission


reg_final = loadRegistry("/gscratch/sfische6/experiments")

submission_table = unwrap(getJobTable(reg = reg_final))
submission_table$learner_id = sapply(submission_table$learner_id, translate_id)

submission_table = merge(
    submission_table,
    jt_test[, c("learner_id", "task_id", "estimated_time")], 
    by = c("learner_id", "task_id"), 
    all.x = TRUE
)

resources_default = list(
    small  = list(memory = 1000, ntasks = 1L, ncpus = 1L, nodes = 1L, clusters = "beartooth"),
    medium = list(memory = 3000, ntasks = 1L, ncpus = 1L, nodes = 1L, clusters = "beartooth"),
    large  = list(memory = 5000, ntasks = 1L, ncpus = 1L, nodes = 1L, clusters = "beartooth")
)

# We add a new column "chunk" which we will use to group existing jobs
# We also add the column "new_resources" where we will specify with which resources
# The jobs will be submitted.
# These resources are being determined by the memory 

submission_table$chunk = 0
submission_table$new_resources = list()

current_chunk = 1

for (lid in learner_ids) {
    for (tid in task_ids) {
        # We get the starting point from the resources from a simple heuristic
        n = task_table[task_id == tid, "NumberOfInstances"][[1L]][[1L]]
        if (n <= 1000) {
          resources = resources_default$small
        } else if (n <= 10000) {
          resources = resources_default$medium
        } else if (n <= 100000) {
          resources = resources_default$large
        } else {
          # Should not happen
          stop("Too many observations")
        }
        # Now we adjust the cpus and the memory for xgboost
        if (lid == "xgboost") {
            resources$ncpus = 10
            resources$memory = resources$memory * 10
        }

        # Now we determine the chunks
        # We use the total estimated runtime as a heuristic of whether we should group the jobs.
        # If the total estimated runtime of a resampling is > 1 hour we submit the jobs individually, 
        # Otherwise we submit them together
        job_subset = submission_table[task_id == tid & learner_id == lid, ]
        total_time = sum(job_subset$estimated_time)
        njobs = nrow(job_subset)

        # We set the maximum runtime of a job to be 1 hour
        nchunks = ceiling(total_time / 3600 )

        if (nchunks == 0) browser()
        chunked_ids = current_chunk + batchtools::chunk(seq_len(njobs), nchunks)
        
        time_per_chunked_job = total_time / length(unique(chunked_ids))
        
        if (time_per_chunked_job <= 1) browser()
        
        resources$walltime = time_per_chunked_job * 1.5
        
        submission_table[task_id == tid & learner_id == lid, chunk := chunked_ids]
        # We actually would only need to assign the resources for chunked jobs once but we assign it to 
        # each row anyway
        submission_table[task_id == tid & learner_id == lid, new_resources := rep(list(resources), njobs)]
        
        current_chunk = current_chunk + nchunks
    }
}
saveRDS(submission_table, "submission_table.rds")

r = range(unwrap(submission_table)$estimated_time)
stopifnot(r[2] / 3600 <= 48)
stopifnot(r[1] >= min(jt_test$time.running, na.rm = TRUE))
