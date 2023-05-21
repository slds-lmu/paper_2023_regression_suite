source("renv/activate.R")

Sys.setenv(OMP_NUM_THREADS = 1)
Sys.setenv(OMP_THREAD_LIMIT = 1)

# This ensures that xgboost is tuned in parallel
future::plan("multicore")
options(
    mlr3oml.cache = "/gscratch/sfische6/openml"
)
