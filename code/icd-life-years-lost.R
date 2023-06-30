library(tidyverse)
library(haven)
library(readxl)

source("preprocess-function.R")

args = commandArgs(trailingOnly=TRUE)

years <- seq(1999, 2020, 1)

project <- args[1]

plotdir <- file.path("../../cdc-wonder-output", project, "icd")

lifeexp_file <- "../data/file_life_expectancy_1999_to_2020.dta"

outputdir <- file.path("../results", project, "icd")

age_intervals <- c(0, 1, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70, 75, 80, 85, Inf)


#manually change for each project for now, need to come up with more elegant solution later

if (project == "HTN"){

	icdnames <- c("I10" = "Essential HTN", "I11" = "HTN Heart Disease", "I12" = "HTN CKD", "I13" = "HTN Heart + CKD")

} 

if (project == "IHD"){

	icdnames <- c("I20" = "UA", "I21" = "MI", "I25" = "Chronic IHD", "I46" = "Cardiac Arrest", "I24" = "Other Acute IHD")

}

icdlength <- length(icdnames)

cbb <- c("#E69F00", "#56B4E9", "#009E73", "maroon", "#D55E00", "#CC79A7")
shapechoices <- c(15, 16, 17, 18, 3, 2)

deaths_file <- file.path(file.path("../data", project), "export_icd_deaths_race_gender_age_year.tsv")
pll <- deaths_file %>% read_tsv()

life_exp <- read_dta(lifeexp_file) %>% mutate(Gender=factor(gender)) %>% mutate(Gender=ifelse(Gender=="1", "Female", "Male")) %>% select(-gender) %>% mutate(Gender = as.character(Gender))


colnames(pll)[which(colnames(pll)=="Five-Year Ages")] <- "agegroup"
colnames(pll)[which(colnames(pll)=="Cause of death Code")] <- "icd"

pll <- pll %>% filter(agegroup!="Not Stated") %>% filter(Population!="Not Applicable") %>% select(-ends_with("code")) %>% type.convert(as.is=TRUE)

pll <- pll %>% mutate(age = sub("-.*", "", agegroup) %>% as.numeric()) %>% mutate(age = ifelse(is.na(age), 85, age))

pll_combined <- pll %>% left_join(life_exp, by=c("age"="age_cat", "Year"="year", "Gender"="Gender"))

pll_combined <- pll_combined %>% mutate(yrs_lost=life_expectancy*((Deaths/Population)*100000) )

excess_ypll <- pll_combined %>% filter(!is.na(life_expectancy)) %>% group_by(Race, Gender, agegroup, age, Year, icd) %>% summarize(yrs_lost = mean(yrs_lost)) %>% ungroup() %>% group_by(Gender, agegroup, age, Year, icd) %>% summarize(excess_yrs_lost = yrs_lost[Race=="Black"] - yrs_lost[Race=="White"]) %>% ungroup()


excess <- excess_ypll %>% mutate(icd = sub("\\..*", "", icd)) %>% group_by(icd, Year, Gender) %>% summarize(excess = mean(excess_yrs_lost))

#take icd10 codes with complete data
validicd <- excess %>% group_by(icd, Gender) %>% summarize(n = n()) %>% mutate(diff = n - length(years)) %>% filter(diff == 0) %>% pull(icd)

excess <- excess %>% filter(icd %in% validicd)

sizing_theme <- theme(axis.text = element_text(size=12), axis.title=element_text(size=16), legend.text=element_text(size=12), legend.title=element_text(size=16), plot.title=element_text(size=18, hjust=0.5), strip.background = element_rect(color="black", fill = "transparent"), strip.text = element_text(size = 16)) 

year_label <- scale_x_continuous(breaks=years, labels= function(x) ifelse(x %% 2 == 1, x, ""))

panel_theme <- theme_bw() + theme(panel.grid.major.x = element_blank(), panel.grid.minor=element_blank())

yrs_lost_icd_fig <- excess %>% ggplot(aes(x=Year, y=excess, color=icd, shape=icd)) + geom_line(size = 1) + geom_point(size = 3.75) + year_label + panel_theme + ylab("Excess Years of Potential Life Lost") + facet_wrap(~Gender, nrow = 1) + sizing_theme + scale_y_continuous(limits = c(0, max(excess$excess, na.rm=TRUE)*1.15), breaks=seq(0, max(excess$excess, na.rm=TRUE)*1.15, 50)) + scale_color_manual(name = "", values = cbb[1:icdlength], labels = icdnames) + scale_shape_manual(name = "", values = shapechoices[1:icdlength], labels = icdnames)

save_rds(plt = yrs_lost_icd_fig, folder = outputdir)


save_plot(plt = yrs_lost_icd_fig, folder = plotdir, width.in = 14)

































