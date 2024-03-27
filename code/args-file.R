library(tidyverse)

args = commandArgs(trailingOnly=TRUE)
proj <- opt$project

if (proj == "IHD"){

	df <- "../raw-data/ihd-deaths.csv" %>% read_csv()
	icdcodes <- c("I20", "I21", "I22", "I24", "I25", "I46")
	age_first <- 35
	project <- "IHD"
	first_year <- 1999

} else{

	df <- "../raw-data/htn-deaths.csv" %>% read_csv()
	icdcodes <- c("I10", "I11", "I12", "I13", "I15")
	age_first <- 20
	project <- "HTN"
	first_year <- 2000
}

popfilepath <- "../raw-data/master-population.csv"
outdir <- "../processed-data-files"

#ihd_icdcodes <- c("I20","I20.0","I20.1","I20.8","I20.9","I21","I21.0","I21.1","I21.2","I21.3","I21.4","I21.9","I22","I22.0","I22.1","I22.8","I22.9","I24","I24.1","I24.8","I24.9","I25","I25.0","I25.1","I25.2","I25.3","I25.4","I25.5","I25.6","I25.8","I25.9","I46","I46.0","I46.1","I46.9")
