library(batchtools)
reg = loadRegistry("/gscratch/sfische6/experiments-final")

jt = unwrap(getJobTable())
jt = jt[endsWith(learner_id, "tuned")]

g = function(x) {
    mlr3batchmark:::get_export(x, reg)$graph$pipeops$regr.xgboost.tuned$learner$instance_args$resampling
}

x = lapply(unique(jt$learner_hash), g)
names(x) = unique(jt$learner_hash)
table(jt$learner_hash)


design = readRDS("design-final.rds")
y = map_chr(design$learner, function(learner) class(learner$graph$pipeops$regr.xgboost.tuned$learner$instance_args$resampling)[[1]])

design_hashes = unique(map_chr(design$learner, "hash"))
reg_hashes = unique(unwrap(getJobTable())$learner_hash)

design = design[endsWith(mlr3misc::ids(learner), "tuned"), ]
lapply(hashes, g)


library(batchtools)
reg = loadRegistry("/gscratch/sfische6/experiments-final")
orig_hashes = readRDS("experiment-final-hashes.rds")
hashes = unwrap(getJobTable())$learner_hash
design = readRDS("design-final.rds")
design_hashes = mlr3misc::map_chr(design$learner, "hash")
checkmate::assert_set_equal(new_hashes, orig_hashes)
