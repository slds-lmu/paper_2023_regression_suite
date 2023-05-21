library(batchtools)
library(data.table)

reg = loadRegistry("/gscratch/sfische6/experiments-test")

# What we do here:

# We estimate the runtimes for the jobs using the test experiment:
# 1. We load the job table of the test experiments and subset it to those that finished
# 2. Join the table with the number of observations for the tasks (this we use as a proxy for the RAM usage)
# 3. We adjust the estimated runtimes to reflect the actual jobs, i.e. for xgboost we multiply by 65 because we do the tuning
# 4. We remove the jobs that estimate a gam

# to 

done = findDone()[[1L]]
jt = getJobTable()
jt = unwrap(jt)
ii = jt$job.id %in% done
jt = jt[ii, ]
jt = jt[, c("job.id", "task_id", "learner_id", "memory", "ncpus", "walltime", "time.queued", "time.running")]

translate_id = function(id) {
    if (endsWith(id, "ranger")) {
        return("ranger")
    } else if (endsWith(id, "xgboost.tuned")) {
        return("xgboost")
    } else if (endsWith(id, "rpart")) {
        return("rpart")
    } else if (endsWith(id, "gam")) {
        return("gam")
    } else if (endsWith(id, "cv_glmnet")) {
        return("cv_glmnet")
    }
}
jt$learner_id = sapply(jt$learner_id, translate_id)
jt = jt[learner_id != "gam", ]

task_table = readRDS("tbl.rds")[, c("name", "NumberOfInstances", "NumberOfFeatures")]
task_table$task_id = task_table$name
task_table$name = NULL

jt = merge(jt, task_table, by = "task_id", all.x = TRUE)

# 500 iterations but 10 cpus
# We run 500 search iterations of xgboost
jt$estimated_time = ifelse(jt$learner_id == "xgboost", jt$time.queued * (500), jt$time.queued)
# We run 10-fold CV for datasets with less than 10 000 observations for tuning, otherwise holdout like in test run
jt$estimated_time = ifelse(jt$NumberOfInstances <= 10000, jt$estimated_time * 10, jt$estimated_time * 1)
# We parallelize using future using 10 cores and assume here that this gives 5x speedup
jt$estimated_time = jt$estimated_time = jt$estimated_time / 5
# In some cases we might have used a small iteration for nrounds.
# Her we make the runtime estimated for the ranger and xgboost more conservative by using the highest estimated out of all
# smaller datasets
# highest estimated runtime for this learner-task combination on datasets of n with smaller size
# We only do this for ranger and xgboost as the other learners made no problems earlier.
# This will not have a large effect on the very small datasets but we already have conservative estimated for fututre
learner_ids = c("cv_glmnet", "xgboost", "rpart", "ranger")
task_ids = unique(job_table_total$task_id)
for (lid in c("xgboost", "ranger")) {
    for (tid in task_ids) {
        ncurrent = jt[learner_id == lid & task_id == tid, ]$NumberOfInstances[[1L]]
        max_runtime_smaller = max(jt[learner_id == lid & NumberOfInstances <= ncurrent, ]$estimated_time)
        jt[learner_id == lid & task_id == tid, estimated_time := max_runtime_smaller]
    }
}

job_table_total = unwrap(getJobTable())
job_table_total$learner_id = sapply(job_table_total$learner_id, translate_id)
job_table_total = job_table_total[learner_id != "gam", ]

job_table_total = merge(
    job_table_total[, -c("memory")],
    jt[, c("learner_id", "task_id", "estimated_time", "memory")], by = c("learner_id", "task_id"), 
  all.x = TRUE
)


resources_small = list(memory = 1000, ntasks = 1L, ncpus = 1L, nodes = 1L, clusters = "beartooth")
resources_medium = list(memory = 3000, ntasks = 1L, ncpus = 1L, nodes = 1L, clusters = "beartooth")
resources_large = list(memory = 5000, ntasks = 1L, ncpus = 1L, nodes = 1L, clusters = "beartooth")


job_table_total$chunk = 0
job_table_total$new_resources = list()

current_chunk = 1

for (learner_id in learner_ids) {
    for (task_id in task_ids) {
        ii = task_table$task_id == task_id
        n = task_table[(ii), "NumberOfInstances"][[1L]][[1L]]

        if (n <= 1000) {
          resources = resources_small
        } else if (n <= 10000) {
          resources = resources_medium
        } else if (n <= 100000) {
          resources = resources_large
        } else {
          stop("Too many observations")
        }
        if (learner_id == "xgboost") {
            resources$ncpus = 10
            resources$memory = resources$memory * 10
        }
        ii = job_table_total$task_id == task_id & job_table_total$learner_id == learner_id
        job_subset = job_table_total[(ii), ]
        total_time = sum(job_subset$estimated_time)
        njobs = sum(ii)
        
        # We either chunk all the jobs from the resampling into one job (if it takes less ) or we send them individually
        if ((total_time / 3600) <= 1) {
            resources$walltime = sum(job_subset$estimated_time) * 1.5
            job_table_total[(ii), chunk := current_chunk]            
            job_table_total[(ii), new_resources := rep(list(resources), njobs)]            
            current_chunk = current_chunk + 1
        } else {
            resources$walltime = job_subset$estimated_time[[1L]] * 1.5
            job_table_total[(ii), chunk := seq(current_chunk, len = njobs)]            
            job_table_total[(ii), new_resources := rep(list(resources), njobs)]            
            current_chunk = current_chunk + njobs
        }
    }
}

reg1 = loadRegistry("/gscratch/sfische6/experiments")
jt1 = getJobTable(reg = reg1)
jt1 = unwrap(jt1)
jt1$learner_id = sapply(jt1$learner_id, translate_id)


jt_final = merge(
    jt1, 
    job_table_total[, c("learner_id", "task_id", "repl", "chunk",  "new_resources")], 
    by = c("learner_id", "task_id", "repl"),
    all.x = TRUE
)

saveRDS(jt_final, "submission_table.rds")