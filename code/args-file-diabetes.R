library(tidyverse)

df <- "../raw-data/diabetes/diabetes-deaths.csv" %>% read_csv()
age_first <- 30
project <- "diabetes"
first_year <- 1999

popfilepath <- "../raw-data/diabetes/master-population.csv"
outdir <- "../processed-data-files"
