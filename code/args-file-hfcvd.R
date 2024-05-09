library(tidyverse)

df <- "../raw-data/hfcvd/hfcvd-deaths.csv" %>% read_csv()
age_first <- 20
project <- "hfcvd"
first_year <- 2000

popfilepath <- "../raw-data/hfcvd/master-population.csv"
outdir <- "../processed-data-files"
