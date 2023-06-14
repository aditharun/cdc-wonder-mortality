library(tidyverse)
library(haven)

args = commandArgs(trailingOnly=TRUE)

source("preprocess-function.R")

project <- args[1]

age_intervals <- c(0, 1, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70, 75, 80, 85)

#CDC Wonder input file
inputfile <- file.path(file.path("../data", project), "export_deaths_crude_race_gender_age_year.tsv")
lifeexp_file <- "../data/file_life_expectancy_1999_to_2020.dta"

#agefiles input
agefiles <- list.files(file.path("../data", project), "^deaths_crude_race_gender_age_", full.names=TRUE)
agefiles <- agefiles[grepl("year", agefiles)]

#output file name and directory
outputdir <- file.path("../results", project)
outputpath <- file.path(outputdir, "life-years-lost-by-age.rds")

create_output_dir(outputdir)

#plot path
plotdir <- file.path("../plots", project, "life-years-lost-by-age")

#palette of colors
cbb <- c("#E69F00", "#56B4E9", "#009E73", "#0072B2", "#D55E00", "#CC79A7")


life_exp <- read_dta(lifeexp_file) %>% mutate(Gender=factor(gender)) %>% mutate(Gender=ifelse(Gender=="1", "Female", "Male")) %>% select(-gender) %>% mutate(Gender = as.character(Gender))

############ CODE ###############


#inlcude 85+ but throw out NA in population data 

process_df_ten_year_groups_tsv <- function(data){

data <- data %>% filter(is.na(Notes)) %>% select(-Notes) 

data$age_cat <- data$`Ten-Year Age Groups`

data$age <- data$`Ten-Year Age Groups Code`

data <- data %>% mutate(age = ifelse(age=="NS", NA, age)) %>% type.convert() %>% filter(age <= 84) %>% mutate(age=str_extract(age, "\\d+"))

data <- data %>% select(-ends_with("code"))

data <- data %>% type.convert()

return(data)

}

process_df_tsv <- function(data, age_intervals){

	if (identical(which(colnames(data) == "Single-Year Ages Code"), integer(0))) {

	return( process_df_ten_year_groups_tsv(data) )

	}

	data <- data %>% filter(is.na(Notes)) %>% select(-Notes)

	agecodes <- data$`Single-Year Ages Code`

	data <- data %>% select(-ends_with("code"))

	data$age <- agecodes

	data <- data %>% mutate(age=ifelse(age=="NS", NA, age))

	data <- data %>% type.convert()

	data <- data %>% filter(age <= 84)

	data <- data %>% mutate(age_cat = cut(age, breaks=age_intervals, right=FALSE)) %>% mutate(age_cat=as.character(age_cat))

	data <- data %>% type.convert(as.is=TRUE)

	#age buckets formatting
	data <- data %>% mutate(age_lb = as.numeric(sub("\\[(\\d+),.*", "\\1", age_cat)), age_ub = as.numeric(sub(".*,(\\d+)\\)", "\\1", age_cat)), age_cat = paste0(age_lb, "-", age_ub-1), age_cat=ifelse(age_cat=="0-0", "< 1", age_cat)) %>% mutate(age=age_lb) %>% select(-c(age_lb, age_ub))


	return(data)

}


process_df_ten_year_groups <- function(data){

data <- data %>% filter(Notes == "") %>% select(-Notes) 

data$age_cat <- data$`Ten-Year Age Groups`

data$age <- data$`Ten-Year Age Groups Code`

data <- data %>% mutate(age = ifelse(age=="NS", NA, age)) %>% type.convert() %>% filter(age <= 84) %>% mutate(age=str_extract(age, "\\d+"))

data <- data %>% select(-ends_with("code"))

data <- data %>% type.convert()

return(data)

}

process_df <- function(data, age_intervals){

if (identical(which(colnames(data) == "Single-Year Ages Code"), integer(0))) {

return( process_df_ten_year_groups(data) )

}

data <- data %>% filter(Notes == "") %>% select(-Notes)

agecodes <- data$`Single-Year Ages Code`

data <- data %>% select(-ends_with("code"))

data$age <- agecodes

data <- data %>% mutate(age=ifelse(age=="NS", NA, age))

data <- data %>% type.convert()

data <- data %>% filter(age <= 84)

data <- data %>% mutate(age_cat = cut(age, breaks=age_intervals, right=FALSE)) %>% mutate(age_cat=as.character(age_cat))

data <- data %>% type.convert(as.is=TRUE)

#age buckets formatting
data <- data %>% mutate(age_lb = as.numeric(sub("\\[(\\d+),.*", "\\1", age_cat)), age_ub = as.numeric(sub(".*,(\\d+)\\)", "\\1", age_cat)), age_cat = paste0(age_lb, "-", age_ub-1), age_cat=ifelse(age_cat=="0-0", "< 1", age_cat)) %>% mutate(age=age_lb) %>% select(-c(age_lb, age_ub))


return(data)

}


compute_statistics <- function(data, life_exp){

data <- data %>% filter(is.finite(as.numeric(Population))) %>% filter(is.finite(as.numeric(Deaths))) %>% filter(is.finite(as.numeric(`Crude Rate`))) %>% type.convert(as.is=TRUE)

data_agg <- data %>% group_by(age_cat, age, Gender, Race, Year) %>% summarize(deaths=sum(Deaths), population=sum(Population),cruderate=mean(`Crude Rate`) ) %>% ungroup() 


#need to account for 10 year grouping already done for me

data_combined <- data_agg %>% left_join(life_exp, by=c("age"="age_cat", "Year"="year", "Gender"="Gender")) %>% filter(age < 84)

data_combined <- data_combined %>% mutate(yrs_lost=life_expectancy*((deaths/population)*100000) )


excess_ypll <- data_combined %>% group_by(Race, Gender, age_cat, age) %>% summarize(yrs_lost = mean(yrs_lost)) %>% ungroup() %>% group_by(Gender, age_cat, age) %>% summarize(excess_yrs_lost = yrs_lost[Race==1] - yrs_lost[Race==3], ratio_excess_yrs_lost = yrs_lost[Race==1] / yrs_lost[Race==3], black_years_lost = yrs_lost[Race==1], white_years_lost = yrs_lost[Race==3]) %>% ungroup()

return(excess_ypll)

}


wrapper_pll_by_age <- function(relfilepath, life_exp, age_intervals){

	data <- preprocess_cdc_wonder(relfilepath)

	relfilepath <- basename(relfilepath)

	data <- process_df_tsv(data, age_intervals)

	excess_ypll <- compute_statistics(data, life_exp)

	excess_ypll <- excess_ypll %>% mutate(Year = gsub("deaths_crude_race_gender_age_year_|\\.txt", "", relfilepath))

	excess_ypll

}

data <- preprocess_cdc_wonder(inputfile)

excess_ypll <- data %>% process_df_tsv(age_intervals) %>% compute_statistics(life_exp)


age.data <- do.call(rbind, lapply(agefiles, function(x) wrapper_pll_by_age(x, life_exp, age_intervals)))


saveRDS(object = list(age.data=age.data, excess_deaths_age=excess_ypll), file=outputpath)



#Graphical Component
agevector <- excess_ypll %>% select(age_cat, age) %>% distinct() %>% arrange(age)

sizing_theme <- theme(axis.text = element_text(size=12), axis.title=element_text(size=16), legend.text=element_text(size=12), legend.title=element_text(size=16), plot.title=element_text(size=18, hjust=0.5)) 

panel_theme <- theme_bw() + theme(panel.grid.major.x = element_blank(), panel.grid.minor=element_blank())

indiv_yrs_lost_fig <- excess_ypll %>% select(age_cat, age, Gender, black_years_lost, white_years_lost) %>% ungroup() %>% pivot_longer(-c(age_cat, age, Gender)) %>% ggplot(aes(x=age, y=value, color=name)) + geom_line(size = 0.5) + ylab("YPLL per 100K Individuals") + scale_color_manual(values = c("black_years_lost"="maroon", "white_years_lost"="navy"), labels = c("black_years_lost" = "Black", "white_years_lost" = "White")) + panel_theme + facet_wrap(~Gender, nrow=1) + xlab("Age group (years)") + geom_point(size = 2.5) + theme(axis.text.x = element_text(angle=45, hjust=1)) + scale_x_continuous(limits=c(0,80), breaks=agevector$age, labels=agevector$age_cat) + theme(legend.title = element_blank())


excess_pll_fig <- excess_ypll %>% ggplot(aes(x=age, y=excess_yrs_lost, group=Gender)) + geom_line(size=0.5) + ylab("Excess YPLL per 100K individuals") + scale_y_continuous(limits = c(min(c(0, floor(min(excess_ypll$excess_yrs_lost)/1000)*1000)), max(excess_ypll$excess_yrs_lost)*1.15), breaks=seq(min(c(0, floor(min(excess_ypll$excess_yrs_lost)/1000)*1000)), max(excess_ypll$excess_yrs_lost)*1.15, 500)) + sizing_theme + scale_color_manual(values=c("maroon", "navy")) + ggtitle("Excess Years of Potential Life Lost Rate Among the Black Population by Age") + panel_theme + scale_x_continuous(limits=c(0,80), breaks=agevector$age, labels=agevector$age_cat) + theme(axis.text.x = element_text(angle=45, hjust=1)) + xlab("Age group (years)") + geom_point(aes(color=Gender), size=2.5) + geom_hline(yintercept=1, linetype="dashed")


mortality_rate_ratio_fig <- excess_ypll %>% ggplot(aes(x=age, y=ratio_excess_yrs_lost, group=Gender)) + geom_line(size=.5) + ylab("YPLL Rate Ratio (Black / White)") + scale_y_continuous(limits = c(0.75, max(excess_ypll$ratio_excess_yrs_lost + 0.5)), breaks=seq(0.75, max(excess_ypll$ratio_excess_yrs_lost + 0.5), 0.25)) + geom_hline(yintercept=1, linetype="dashed") + sizing_theme + scale_color_manual(values=c("maroon", "navy")) + ggtitle("Black-White Years of Potential Life Lost Rate Ratio by Age") + panel_theme + scale_x_continuous(limits=c(0,80), breaks=agevector$age, labels=agevector$age_cat) + theme(axis.text.x = element_text(angle=45, hjust=1)) + xlab("Age group (years)") + geom_point(aes(color=Gender), size=2.5)


excess_pll_year_fig <- age.data %>% ggplot(aes(x=age, y=excess_yrs_lost, group=Year)) + geom_line(aes(color=Year), size=0.5) + ylab("Excess YPLL per 100K individuals") + scale_y_continuous(limits = c(min(c(0, floor(min(age.data$excess_yrs_lost)/10)*10)), max(age.data$excess_yrs_lost)*1.15), breaks=seq(min(c(0, floor(min(age.data$excess_yrs_lost)/10)*10)), max(age.data$excess_yrs_lost)*1.15, 500)) + geom_hline(yintercept=1, linetype="dashed") + sizing_theme + scale_color_manual(name="Year Range", values=cbb[1:6]) + ggtitle("Excess Years of Potential Life Lost Rate Among Black Population by Age and Gender") + panel_theme + scale_x_continuous(limits=c(0,80), breaks=agevector$age, labels=agevector$age_cat) + theme(axis.text.x = element_text(angle=45, hjust=1)) + xlab("Age group (years)") + geom_point(aes(color=Year), size=2.5) + facet_wrap(~Gender) + theme(strip.text = element_text(size=11), strip.background=element_rect(fill="transparent", color="transparent"))



save_plot(excess_pll_fig, plotdir)
save_plot(mortality_rate_ratio_fig, plotdir)
save_plot(excess_pll_year_fig, plotdir)
save_plot(indiv_yrs_lost_fig, plotdir)


















