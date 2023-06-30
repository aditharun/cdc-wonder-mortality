library(tidyverse)
library(haven)
library(readxl)

source("preprocess-function.R")

args = commandArgs(trailingOnly=TRUE)

years <- seq(1999, 2021, 1)

project <- args[1]

plotdir <- file.path("../../cdc-wonder-output", project, "icd")

outputdir <- file.path("../results", project, "icd")

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

#

ageadj_file <- file.path(file.path("../data", project), "export_icd_age_adjusted_deaths_race_gender_year_se.tsv")

ageadj <- read_tsv(ageadj_file) %>% select(`Cause of death Code`, Race, Gender, Year, `Age Adjusted Rate`, Deaths, Population) 

ageadj <- ageadj %>% group_by(Gender, Year, `Cause of death Code`) %>% summarize(excess = `Age Adjusted Rate`[Race == "Black"] - `Age Adjusted Rate`[Race == "White"], ratio = `Age Adjusted Rate`[Race == "Black"] / `Age Adjusted Rate`[Race == "White"] )

ageadj <- ageadj %>% mutate(icd = sub("\\..*", "", `Cause of death Code`)) %>% select(icd, Year, excess, Gender, ratio) %>% group_by(icd, Year, Gender) %>% summarize(excess = mean(excess), rate = mean(ratio))

#take icd10 codes with complete data
validicd <- ageadj %>% group_by(icd, Gender) %>% summarize(n = n()) %>% mutate(diff = n - length(years)) %>% filter(diff == 0) %>% pull(icd)

ageadj <- ageadj %>% filter(icd %in% validicd)

sizing_theme <- theme(axis.text = element_text(size=12), axis.title=element_text(size=16), legend.text=element_text(size=12), legend.title=element_text(size=16), plot.title=element_text(size=18, hjust=0.5), strip.background = element_rect(color="black", fill = "transparent"), strip.text = element_text(size = 16)) 

year_label <- scale_x_continuous(breaks=years, labels= function(x) ifelse(x %% 2 == 1, x, ""))

panel_theme <- theme_bw() + theme(panel.grid.major.x = element_blank(), panel.grid.minor=element_blank())


ageadj_icd_fig <- ageadj %>% ggplot(aes(x=Year, y=excess, color=icd, shape=icd)) + geom_line(size = 1) + geom_point(size = 3.75) + year_label + panel_theme + ylab("Age-Adjusted Excess Death Rate") + facet_wrap(~Gender, nrow = 1) + sizing_theme + scale_y_continuous(breaks=scales::pretty_breaks(n = 8)) + scale_color_manual(name = "", values = cbb[1:icdlength], labels = icdnames) + scale_shape_manual(name = "", values = shapechoices[1:icdlength], labels = icdnames)

mortality_ratio_icd_fig <- ageadj %>% ggplot(aes(x=Year, y=rate, color=icd, shape=icd)) + geom_line(size = 1) + geom_point(size = 3.75) + year_label + panel_theme + ylab("Mortality Rate Ratio (Black / White)") + facet_wrap(~Gender, nrow = 1) + sizing_theme + scale_y_continuous(breaks=scales::pretty_breaks(n = 8)) + scale_color_manual(name = "", values = cbb[1:icdlength], labels = icdnames) + scale_shape_manual(name = "", values = shapechoices[1:icdlength], labels = icdnames) 


save_rds(plt = ageadj_icd_fig, folder = outputdir)
save_rds(plt = mortality_ratio_icd_fig, folder = outputdir)


save_plot(plt = ageadj_icd_fig, folder = plotdir, width.in = 14)
save_plot(plt = mortality_ratio_icd_fig, folder = plotdir, width.in = 14)














