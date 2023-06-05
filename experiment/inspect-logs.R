# Helper file to check for unexpected warnings / errors in the logs of the experiments
library(batchtools)
reg = loadRegistry(getOption("registry_path"), writeable = TRUE)

done = findDone()[[1L]]

inspect_ids = list()
for (i in done) {
    if (any(grepl(getLog(i), pattern = "Warning|Error"))) {
        inspect_ids[[length(inspect_ids) + 1]] = i
    }
}

inspect_ids = unlist(inspect_ids)

print(inspect_ids)