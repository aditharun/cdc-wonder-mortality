library(tidyverse)

df <- "../raw-data/cvd/cvd-deaths.csv" %>% read_csv()
age_first <- 20
project <- "cvd"
first_year <- 2000

popfilepath <- "../raw-data/cvd/master-population.csv"
outdir <- "../processed-data-files"
