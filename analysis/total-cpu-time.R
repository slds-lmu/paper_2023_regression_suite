# Calculates the total CPU hours for the experiments
library(batchtools)

reg = loadRegistry(getOption("registry_path"), writeable = TRUE)

jt = unwrap(getJobTable())

round(sum(jt$time.running * jt$ncpus) / 3600)
