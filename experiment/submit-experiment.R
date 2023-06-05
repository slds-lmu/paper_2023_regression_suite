# File to submit the experiments after running `experiment/experiments.R`
library(batchtools)

reg = loadRegistry(getOption("registry_path"), writeable = TRUE)

submission_table = readRDS(here::here("submission_info.rds"))

resources = submission_table$resources
chunked_ids = submission_table$chunked_ids

for (chnk in unique(submission_table$chunk)) {
  submitJobs(chunked_ids[chunk == chnk, c("job.id", "chunk")], resources[[chunk]])
}
