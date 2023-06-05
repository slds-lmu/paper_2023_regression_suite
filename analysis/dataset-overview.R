library(data.table)
library(mlr3verse)
library(mlr3oml)
library(mlr3misc)
library(mlr3benchmark)
library(ggplot2)
library(data.table)
library(xtable)


# This file creates the dataset overview from the appendix of the paper.

ids = read.csv(here::here("ids.csv"))[[1L]]

task_table = list_oml_tasks(task_id = ids)

task_table = task_table[, .(task_id, data_id, name, n = NumberOfInstances, p = NumberOfFeatures)]

task_table = task_table[order(name)]

aggr = readRDS(here::here("results/aggr.rds"))

linear_model = ppl("robustify", learner = lrn("regr.lm")) %>>% lrn("regr.lm") |> as_learner()
msr_rsq = msr("regr.rsq")

r_squared = mlr3misc::map_dbl(ids, function(id) {
  task = tsk("oml", task_id = id)
  linear_model$train(task)
  pred = linear_model$predict(task)
  pred$score(msr_rsq)
})

# task_table$r_squared = round(r_squared, 2)
task_table = task_table[, c("name", "data_id", "task_id", "n", "p")]
task_table = task_table[order(name), ]

xtable(task_table)

x_breaks <- c(500, 1000, 3000, 10000, 30000, 100000)
x_labels <- as.character(x_breaks)

p = ggplot(data = task_table, aes(x = n, y = p)) +
  geom_point(size = 2) +
  scale_x_log10(breaks = x_breaks, labels = x_labels, limits = c(500, 100000)) +
  scale_y_log10() +
  labs(
    x = "Number of Observations",
    y = "Number of Features"
  ) +
  theme_bw(base_size = 18)


ggsave(p, filename = here::here("results/dataset-overview.pdf"), dpi = 300)