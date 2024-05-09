library(tidyverse)

df <- "../raw-data/htncvd/htncvd-deaths.csv" %>% read_csv()
age_first <- 20
project <- "htncvd"
first_year <- 2000

popfilepath <- "../raw-data/htncvd/master-population.csv"
outdir <- "../processed-data-files"
