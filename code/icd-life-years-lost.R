library(tidyverse)
library(haven)
library(readxl)

source("preprocess-function.R")

args = commandArgs(trailingOnly=TRUE)

project <- args[1]

first_year <- args[2]

years <- seq(first_year, 2022, 1)

plotdir <- file.path("../../cdc-wonder-output", project, "icd")

lifeexp_file <- "../data/file_life_expectancy_1999_to_2020.dta"
#https://www.cdc.gov/nchs/data/nvsr/nvsr72/nvsr72-12.pdf - 2021 life expectancy data, use for 2021 and 2022
lifeexp_file2 <- "../data/file_life_expectancy_2021.xlsx" 

outputdir <- file.path("../results", project, "icd")

age_intervals <- c(0, 1, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70, 75, 80, 85, Inf)


#manually change for each project for now, need to come up with more elegant solution later

if (project == "HTN"){

	icdnames <- c("I110" = "HTN Heart Disease", "I120" = "HTN CKD w/ ESRD", "I119" = "HTN w/o HF", "I129" = "HTN w/o ESRD", "I131" = "HTN w/o HF", "I132" = "HTN w/ Heart Disease +\nHF + ESRD")

} 

if (project == "MCD_HTN"){

    icdnames <- c("I110" = "HTN Heart Disease", "I120" = "HTN CKD w/ ESRD", "I119" = "HTN w/o HF", "I129" = "HTN w/o ESRD", "I131" = "HTN w/o HF", "I132" = "HTN w/ Heart Disease +\nHF + ESRD")

} 

if (project == "IHD"){

 	icdnames <- c("I209" = "Angina Pectoris", "I214" = "NSTEMI", "I219" = "AMI unspec.", "I248" = "Other Acute IHD", "I249" = "Acute IHD unspec.", "I250" = "Atherscl HD w/o\nAngina Pectoris", "I251" = "Atherosclerotic HD w/ Angina Pectoris", "I253" = "Heart Aneurysm", "I255" = "Ischemic Cardiomyopathy", "I258" = "Atherscl of CABG/\nTransplanted Heart", "I259" = "Chronic IHD", "I461" = "Cardiac Arrest due\nto underlying cause", "I469" = "Cardia Arrest\ncause unspec.")
}



icdlength <- length(icdnames)

cbb <- c("#E69F00", "#56B4E9", "#009E73", "maroon", "#D55E00", "#CC79A7", 
         "#0072B2", "#F0E442", "#8DD3C7", "#FFD700", "#A9A9A9", 
         "#9467BD", "#F28E2B", "#1F77B4", "#7F7F7F")

shapechoices <- c(15, 16, 17, 18, 3, 2, 19, 20, 21, 22, 23, 24, 25)


deaths_file <- file.path("../processed-data-files", project, "export_icd_deaths_race_gender_age_year.tsv")

pll <- deaths_file %>% read_tsv()

life_exp <- read_dta(lifeexp_file) %>% mutate(Gender=factor(gender)) %>% mutate(Gender=ifelse(Gender=="1", "Female", "Male")) %>% select(-gender) %>% mutate(Gender = as.character(Gender))

life_exp <- rbind(lifeexp_file2 %>% read_excel(), lifeexp_file2 %>% read_excel() %>% mutate(year = 2022), life_exp) %>% as_tibble()

colnames(pll)[which(colnames(pll)=="Five-Year Age Groups")] <- "agegroup"
colnames(pll)[which(colnames(pll)=="Cause of death Code")] <- "icd"

pll <- pll %>% filter(agegroup!="Not Stated") %>% filter(Population!="Not Applicable") %>% select(-ends_with("code")) %>% type.convert(as.is=TRUE)

pll <- pll %>% mutate(age = sub("-.*", "", agegroup) %>% as.numeric()) 
#%>% mutate(age = ifelse(is.na(age), 85, age))

pll_combined <- pll %>% left_join(life_exp, by=c("age"="age_cat", "Year"="year", "Gender"="Gender"))

pll_combined <- pll_combined %>% mutate(yrs_lost=life_expectancy*((Deaths/Population)*100000) )

excess_ypll <- pll_combined %>% filter(!is.na(life_expectancy)) %>% group_by(Race, Gender, agegroup, age, Year, icd) %>% summarize(yrs_lost = mean(yrs_lost)) %>% ungroup() %>% group_by(Gender, agegroup, age, Year, icd) %>% summarize(excess_yrs_lost = yrs_lost[Race=="Black or African American"] - yrs_lost[Race=="White"]) %>% ungroup()


excess <- excess_ypll %>% mutate(icd = sub("\\..*", "", icd)) %>% group_by(icd, Year, Gender) %>% summarize(excess = mean(excess_yrs_lost))

#take icd10 codes with complete data
#validicd <- excess %>% group_by(icd, Gender) %>% summarize(n = n()) %>% mutate(diff = n - length(years)) %>% filter(diff == 0) %>% pull(icd)

#excess <- excess %>% filter(icd %in% validicd)

sizing_theme <- theme(axis.text = element_text(size=12), axis.title=element_text(size=16), legend.text=element_text(size=12), legend.title=element_text(size=16), plot.title=element_text(size=18, hjust=0.5), strip.background = element_rect(color="black", fill = "transparent"), strip.text = element_text(size = 16)) 

year_label <- scale_x_continuous(breaks=years, labels= function(x) ifelse(x %% 2 == 1, x, ""))

panel_theme <- theme_bw() + theme(panel.grid.major.x = element_blank(), panel.grid.minor=element_blank())

yrs_lost_icd_fig <- excess %>% ggplot(aes(x=Year, y=excess, color=icd, shape=icd)) + geom_line(size = 1) + geom_point(size = 3.75) + year_label + panel_theme + ylab("Excess Years of Potential Life Lost") + facet_wrap(~Gender, nrow = 1) + sizing_theme + scale_y_continuous(breaks = scales::pretty_breaks(n = 8)) + scale_color_manual(name = "", values = cbb[1:icdlength], labels = icdnames) + scale_shape_manual(name = "", values = shapechoices[1:icdlength], labels = icdnames)

save_rds(plt = yrs_lost_icd_fig, folder = outputdir)


save_plot(plt = yrs_lost_icd_fig, folder = plotdir, width.in = 14)

































