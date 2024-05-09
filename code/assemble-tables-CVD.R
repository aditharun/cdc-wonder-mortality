library(tidyverse)
library(cowplot)

pdir <- "../../cdc-wonder-output"

projectdirs <- c("Overall CVD"="cvd", "Ischemic Heart Disease"="ihdcvd", "Hypertension"="htncvd", "Cerebrovascular Disease"="cvcvd", "Heart Failure"="hfcvd")


tdir <- projectdirs %>% unname()

orderlist <- names(projectdirs)

specifictables <- c("excess_aadr_year.csv", "ypll_sex_year.csv")

aadr <- file.path(pdir, tdir, "tables", specifictables[1])

lly <- file.path(pdir, tdir, "tables", specifictables[2])



get_female_df <- function(aadr, lly, projectdirs){
	df.mrr <- lapply(seq_along(aadr), function(x) aadr[x] %>% read_csv() %>% select(Year, Female_mrr) %>% magrittr::set_colnames(c("Year", "MRR")) %>% mutate(`Cause of Death` = names(projectdirs)[x])) %>% do.call(rbind, .)

	df.lly <- lapply(seq_along(lly), function(x) lly[x] %>% read_csv() %>% select(Year, excess_ypll_number_Female) %>% magrittr::set_colnames(c("Year", "EYLL")) %>% mutate(`Cause of Death` = names(projectdirs)[x])) %>% do.call(rbind, .)

	return(list(mrr= df.mrr, lly = df.lly))

}


get_male_df <- function(aadr, lly, projectdirs){
	df.mrr <- lapply(seq_along(aadr), function(x) aadr[x] %>% read_csv() %>% select(Year, Male_mrr) %>% magrittr::set_colnames(c("Year", "MRR")) %>% mutate(`Cause of Death` = names(projectdirs)[x])) %>% do.call(rbind, .)

	df.lly <- lapply(seq_along(lly), function(x) lly[x] %>% read_csv() %>% select(Year, excess_ypll_number_Male) %>% magrittr::set_colnames(c("Year", "EYLL")) %>% mutate(`Cause of Death` = names(projectdirs)[x])) %>% do.call(rbind, .)

	return(list(mrr= df.mrr, lly = df.lly))

}




female.df <- get_female_df(aadr, lly, projectdirs)
male.df <- get_male_df(aadr, lly, projectdirs)

outputdir <- pdir %>% file.path(., "CVD-tables/")

if (!dir.exists(outputdir)){
	dir.create(outputdir)
}

female.df$mrr %>% pivot_wider(names_from = Year, values_from = MRR) %>% write_csv(., paste0(outputdir, "female-MRR.csv"))

female.df$lly %>% pivot_wider(names_from = Year, values_from = EYLL) %>% write_csv(., paste0(outputdir, "female-EYLL.csv"))

male.df$mrr %>% pivot_wider(names_from = Year, values_from = MRR) %>% write_csv(., paste0(outputdir, "male-MRR.csv"))

male.df$lly %>% pivot_wider(names_from = Year, values_from = EYLL) %>% write_csv(., paste0(outputdir, "male-EYLL.csv"))









