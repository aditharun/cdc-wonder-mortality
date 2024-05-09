library(tidyverse)

df <- "../raw-data/ihdcvd/ihdcvd-deaths.csv" %>% read_csv()
age_first <- 20
project <- "ihdcvd"
first_year <- 2000

popfilepath <- "../raw-data/ihdcvd/master-population.csv"
outdir <- "../processed-data-files"
