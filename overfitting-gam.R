library(mlr3verse)
library(batchtools)
library(mlr3batchmark)
library(mlr3misc)

design = readRDS("design.rds")

design = design[startsWith(ids(learner), "gam_custom") | startsWith(ids(learner), "cv_glmnet"), ]
design = design[ids(task) != "synchronous_machine", ]

design$resampling = replicate(rsmp("insample"), n = 70)
for (i in seq_len(nrow(design))) {
  resampling = design[i, "resampling"][[1]][[1L]]
  task = design[i, "task"][[1]][[1L]]
  resampling$instantiate(task)
}

reg = makeExperimentRegistry(
  "/gscratch/sfische6/experiments-gam/", 
  seed = 1,
  packages = c("mlr3verse", "mlr3misc", "checkmate", "R6", "paradox"),
  work.dir = "/home/sfische6/paper_2023_regression_suite"
)
batchmark(design)

submitJobs(
  findJobs(),
  resources = list(memory = 4000, ntasks = 1L, ncpus = 1L, nodes = 1L, clusters = "teton", walltime = 1800)
)


