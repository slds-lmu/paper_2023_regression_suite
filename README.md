# OpenML-CTR23

This is the code accompanying the paper *OpenML-CTR23 -- A curated tabular regression benchmarking suite*.

The benchmarking suite on OpenML can be found [here](https://www.openml.org/search?type=study&sort=tasks_included&study_type=task&id=353).


## Overview of files and folders

* The `renv.lock` file described the computational environment used for the experiments.
* The files in `./experiment` define the experiments that were run on the cluster.
* The code in the `./analysis` subdirectory produces the results from the paper, including tables and plots.
* `submission_info.rds` contains information needed to submit the jobs to a cluster.
* `ids.csv` contains the OpenML task IDs.
* The folder `results` contains the results as a csv file, as well as some plots.
* `module_load.sh` loads some modules that were needed on the cluster.
  This will probably not work for you.

## Reproducing the Results

### Running the experiments

1. Install the `renv` package and restore the computational environment using `renv::restore()`.
1. Adjust the configuration: To be able to run the experiments on a different cluster, the
following options have to be adjusted:
   * The `registry_path` in the `.Rprofile` file must link to a path where you want to store the batchtools registry on your cluster.
   * Adjust the batchtools configuration in `batchtools.conf.R`, to your cluster.
     This includes changing the template `slurm_wyoming.tmpl`.
     For more information see the [batchtools documentation](https://mllg.github.io/batchtools/articles/batchtools.html).
1. Run the file `./experiment/experiment.R` from the root of this project.
1. Run the file `./experiment/submit-experiment.R` from the root of this project.
   It sources the resource configuration and chunking from `./submission_table.rds`
   If this does not work you probably did not adjust the batchtools configuration correctly.
1. Wait for the results to be computed.

### Analyzing the results

In case everything worked, you should now have the experiment results in the path specified in the `registry_path` option.
If you want to analyze the results locally, you can move the registry to your local machine and adjust the `registry_path_local` option in the `.Rprofile` file to the corresponding folder.
You then first need to run the `./analysis/benchmark-agggrgate.R` script to produce the files `./results/aggr.rds` and `./results/bma.rds` that some of the other analysis files require.
