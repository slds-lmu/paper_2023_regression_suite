library(batchtools)
library(data.table)

reg = loadRegistry(getOption("registry_path"), writeable = TRUE)

jt = unwrap(getJobTable())
jt$learner_id = gsub("\\..*", "", jt$learner_id, perl = TRUE)
st = readRDS("submission_table.rds")

st = st[, c("chunk", "new_resources", "learner_id", "task_id", "repl")]

x = merge(jt, st, by = c("learner_id", "task_id", "repl"))

x = x[, c("job.id", "chunk", "new_resources")]
x = x[order(job.id), ]

new_chunk = c()
new_chunk_counter = 1

for (chnk in unique(x$chunk)) {
  new_chunk = c(new_chunk, rep(new_chunk_counter, nrow(x[chunk == chnk, ])))
  new_chunk_counter = new_chunk_counter + 1
}

x$chunk = new_chunk


resources = lapply(unique(x$chunk), function(id) x[id, "new_resources"][[1L]][[1L]])
table = x[, c("job.id", "chunk")]

saveRDS(list(resources = resources, chunked_ids = table), "submission_info.rds")
