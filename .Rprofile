source("renv/activate.R")

# Avoid any threading issues when submitting jobs on the cluster
Sys.setenv(OMP_NUM_THREADS = 1)
Sys.setenv(OMP_THREAD_LIMIT = 1)

# This ensures that xgboost is tuned in parallel
future::plan("multicore")

if (dir.exists("/gscratch/sfische6")) {
  options(mlr3oml.cache = "/gscratch/sfische6/openml")
} else {
  options(mlr3oml.cache = TRUE)
}

options(
  # Registry path for the cluster
  registry_path = "/home/sebi/r/gscratch/experiments",
  # Registry path in case you want to run the analysis on your local machine.
  registry_path_local = "/home/sebi/r/gscratch/experiments"
)
