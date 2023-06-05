# Creates the starting point for the table containing the raw RMSE results
# The final table was created manually from this table.
library(data.table)
library(xtable)

aggr = readRDS(here::here("results", "aggr.rds"))
bma = readRDS(here::here("results", "bma.rds"))

aggr = dcast(aggr, task_id ~ learner_id, value.var = "rmse")
learner_ids = c("cv_glmnet", "gam", "ranger", "xgboost", "rpart")

aggr = aggr[, c("task_id", "xgboost", "ranger", "gam", "cv_glmnet", "rpart")]

aggr$factor = NA_real_

f = function(x) {
    a = 0
    if (x < 1) {
        while (x < 1) {
            x = x * 10
            a = a - 1
        }
    } else {
        while (x > 10) {
            x = x / 10
            a = a + 1
        }
    }
    return(a)
}

aggr_prev = aggr

for (i in seq_len(nrow(aggr))) {
  print(unlist(aggr[i, ..learner_ids]))
  worst_score = max(unlist(aggr[i, ..learner_ids]))
  fact = f(worst_score)
  aggr[i, "factor"] = fact
  for (lid in learner_ids) {
    aggr[i, c(lid)] = aggr[i, lid, with = FALSE][[1L]] / 10^fact
  }
}

mean(abs(aggr_prev$xgboost - aggr$xgboost * 10^aggr$factor))

aggr$factor = as.character(aggr$factor)

g = function(x) {
  chars = strsplit(x, "")[[1L]]
  paste0(chars[1:5], collapse = "")
}

for (lid in learner_ids) {
    aggr[[lid]] = sapply(as.character(aggr[[lid]]), g)

}


xtable(aggr)


latex_table = xtable(aggr)
latex_table


ranks = t(bma$rank_data())
ranks_dt  = as.data.table(ranks)
ranks_dt$task_id = rownames(ranks)
ranks_dt = ranks_dt[, c("task_id", "xgboost", "ranger", "gam", "cv_glmnet", "rpart")]

xtable(ranks_dt, digits = 0)
