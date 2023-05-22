## CDC Wonder Mortality Analyses
## Adith Arun, May 17 2023

This repository performs the [Caballo et. al 2023 JAMA](https://jamanetwork.com/journals/jama/fullarticle/2804822) analyses in R (except for the heatmap figures). 

### Structure of this repository
The `data` folder contains the data as downloaded from the CDC and the life expectancy data as a .dta file
The `code` folder has all the scripts that do the data preprocessing, statistical analyses and computations, and plotting 

The `results` folder is created when running the scripts and stores the final dataframes that we use for plotting
The `plots` folder is created when running the scripts and stores the plots for each of the scripts in each of its subfolders

### How to use this?
Navigate to `code` and then do `Rscript excess-mortality-by-age.R`, `Rscript excess-mortality-by-year.R`, `Rscript life-years-lost-by-age.R`, `Rscript life-years-lost-by-year.R`. 

Swap out the files in `data`. The header portions of the above R scripts contain a lot of the things that need to be modified like input and output file paths and different age buckets. Note that it is overwhemlingly likely that some alteration of the internal code and plotting will be necessary for each individual project. 

For reference, the file `preprocess-function.R` contains functions that are used by all four scripts. 



