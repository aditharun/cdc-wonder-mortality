library(tidyverse)

args = commandArgs(trailingOnly=TRUE)

proj <- opt$project

df <- "../raw-data/mcd/htn-deaths.csv" %>% read_csv()
icdcodes <- c("I10", "I11", "I12", "I13", "I15")
age_first <- 20
project <- "MCD_HTN"

popfilepath <- "../raw-data/mcd/master-population.csv"
outdir <- "../processed-data-files"
