library(batchtools)
reg = loadRegistry("/gscratch/sfische6/experiments")


done = findDone()[[1L]]

for (i in done) {
    if (any(grepl(getLog(i), pattern = "Error"))) i
}

expireds = findExpired()[[1L]]

for (expired in expireds) {
    print(getLog(expired))
}