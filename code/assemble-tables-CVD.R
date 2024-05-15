library(tidyverse)
library(cowplot)

pdir <- "../../cdc-wonder-output"

projectdirs <- c("Overall CVD"="cvd", "Ischemic Heart Disease"="ihdcvd", "Hypertension"="htncvd", "Cerebrovascular Disease"="cvcvd", "Heart Failure"="hfcvd")


tdir <- projectdirs %>% unname()

orderlist <- names(projectdirs)

specifictables <- c("excess_aadr_year.csv", "ypll_sex_year.csv")

aadr <- file.path(pdir, tdir, "tables", specifictables[1])

lly <- file.path(pdir, tdir, "tables", specifictables[2])

deaths <- file.path(pdir, tdir, "tables", "excess_deaths.csv")

get_female_df <- function(aadr, lly, deaths, projectdirs){
	df.mrr <- lapply(seq_along(aadr), function(x) aadr[x] %>% read_csv() %>% select(Year, Female_mrr) %>% magrittr::set_colnames(c("Year", "MRR")) %>% mutate(`Cause of Death` = names(projectdirs)[x])) %>% do.call(rbind, .)

	df.aamr <- lapply(seq_along(aadr), function(x) aadr[x] %>% read_csv() %>% select(Year, Female_excess_aamr) %>% magrittr::set_colnames(c("Year", "Excess_AAMR")) %>% mutate(`Cause of Death` = names(projectdirs)[x])) %>% do.call(rbind, .)

	df.lly <- lapply(seq_along(lly), function(x) lly[x] %>% read_csv() %>% select(Year, excess_ypll_number_Female) %>% magrittr::set_colnames(c("Year", "EYLL")) %>% mutate(`Cause of Death` = names(projectdirs)[x])) %>% do.call(rbind, .)

	df.deaths <- lapply(seq_along(deaths), function(x) deaths[x] %>% read_csv() %>% filter(Gender == "Female") %>% select(-Gender) %>% magrittr::set_colnames(c("Year", "Excess_Deaths")) %>% mutate(`Cause of Death` = names(projectdirs)[x])) %>% do.call(rbind, .)

	return(list(mrr= df.mrr, lly = df.lly, aamr = df.aamr, deaths = df.deaths))

}


get_male_df <- function(aadr, lly, deaths, projectdirs){
	df.mrr <- lapply(seq_along(aadr), function(x) aadr[x] %>% read_csv() %>% select(Year, Male_mrr) %>% magrittr::set_colnames(c("Year", "MRR")) %>% mutate(`Cause of Death` = names(projectdirs)[x])) %>% do.call(rbind, .)

	df.aamr <- lapply(seq_along(aadr), function(x) aadr[x] %>% read_csv() %>% select(Year, Male_excess_aamr) %>% magrittr::set_colnames(c("Year", "Excess_AAMR")) %>% mutate(`Cause of Death` = names(projectdirs)[x])) %>% do.call(rbind, .)

	df.lly <- lapply(seq_along(lly), function(x) lly[x] %>% read_csv() %>% select(Year, excess_ypll_number_Male) %>% magrittr::set_colnames(c("Year", "EYLL")) %>% mutate(`Cause of Death` = names(projectdirs)[x])) %>% do.call(rbind, .)

	df.deaths <- lapply(seq_along(deaths), function(x) deaths[x] %>% read_csv() %>% filter(Gender == "Male") %>% select(-Gender) %>% magrittr::set_colnames(c("Year", "Excess_Deaths")) %>% mutate(`Cause of Death` = names(projectdirs)[x])) %>% do.call(rbind, .)

	return(list(mrr= df.mrr, lly = df.lly, aamr = df.aamr, deaths = df.deaths))

}




female.df <- get_female_df(aadr, lly, deaths, projectdirs)
male.df <- get_male_df(aadr, lly, deaths, projectdirs)

outputdir <- pdir %>% file.path(., "CVD-tables/")

if (!dir.exists(outputdir)){
	dir.create(outputdir)
}


female.df$mrr %>% pivot_wider(names_from = Year, values_from = MRR) %>% mutate_if(is.numeric, ~round(., 2)) %>% write_csv(., paste0(outputdir, "female-MRR.csv"))

female.df$lly %>% pivot_wider(names_from = Year, values_from = EYLL) %>% mutate_if(is.numeric, ~round(., 0)) %>% write_csv(., paste0(outputdir, "female-EYLL.csv"))

male.df$mrr %>% pivot_wider(names_from = Year, values_from = MRR) %>% mutate_if(is.numeric, ~round(., 2)) %>% write_csv(., paste0(outputdir, "male-MRR.csv"))

male.df$lly %>% pivot_wider(names_from = Year, values_from = EYLL) %>% mutate_if(is.numeric, ~round(., 0)) %>% write_csv(., paste0(outputdir, "male-EYLL.csv"))

male.df$aamr %>% pivot_wider(names_from = Year, values_from = Excess_AAMR) %>% mutate_if(is.numeric, ~round(., 2)) %>% write_csv(., paste0(outputdir, "male-excess-AAMR.csv"))

female.df$aamr %>% pivot_wider(names_from = Year, values_from = Excess_AAMR) %>% mutate_if(is.numeric, ~round(., 2)) %>% write_csv(., paste0(outputdir, "female-excess-AAMR.csv"))

male.df$deaths %>% pivot_wider(names_from = Year, values_from = Excess_Deaths) %>% mutate_if(is.numeric, ~round(., 0)) %>% write_csv(., paste0(outputdir, "male-excess-deaths.csv"))

female.df$deaths %>% pivot_wider(names_from = Year, values_from = Excess_Deaths) %>% mutate_if(is.numeric, ~round(., 0)) %>% write_csv(., paste0(outputdir, "female-excess-deaths.csv"))




