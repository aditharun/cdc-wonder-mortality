library(tidyverse)

df <- "../raw-data/cancer/cancer-deaths.csv" %>% read_csv()
age_first <- 20
project <- "cancer"
first_year <- 1999

popfilepath <- "../raw-data/cancer/master-population.csv"
outdir <- "../processed-data-files"
