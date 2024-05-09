library(tidyverse)

df <- "../raw-data/cvcvd/cvcvd-deaths.csv" %>% read_csv()
age_first <- 20
project <- "cvcvd"
first_year <- 2000

popfilepath <- "../raw-data/cvcvd/master-population.csv"
outdir <- "../processed-data-files"
