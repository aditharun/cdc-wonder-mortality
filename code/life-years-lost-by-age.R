library(tidyverse)
library(haven)

args = commandArgs(trailingOnly=TRUE)

source("preprocess-function.R")

project <- args[1]

age_intervals <- c(0, 1, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70, 75, 80, 85, Inf)


datadir <- "../../../pk-output"

#CDC Wonder input file
inputfile <- file.path(file.path(datadir, project), "export_deaths_crude_race_gender_age_year.tsv")

lifeexp_file <- "../data/file_life_expectancy_1999_to_2020.dta"

#agefiles input
agefiles <- list.files(file.path(datadir, project), "^deaths_crude_race_gender_age_", full.names=TRUE)
agefiles <- agefiles[grepl("year", agefiles)]

#output file name and directory
outputdir <- file.path("../results", project, "life-years-lost-by-age")

#table dir

#plot path
plotdir <- file.path("../../cdc-wonder-output", project, "life-years-lost-by-age")


tabledir <- file.path(dirname(plotdir), "tables")
create_output_dir(tabledir)

#palette of colors
cbb <- c("#E69F00", "#56B4E9", "#009E73", "#0072B2", "#D55E00", "#CC79A7")


life_exp <- read_dta(lifeexp_file) %>% mutate(Gender=factor(gender)) %>% mutate(Gender=ifelse(Gender=="1", "Female", "Male")) %>% select(-gender) %>% mutate(Gender = as.character(Gender))

#remove age > 85
life_exp <- life_exp %>% filter(age_cat < 85)

############ CODE ###############



process_df_ten_year_groups_tsv <- function(data){


data$age_cat <- data$`Ten-Year Age Groups`

data$age <- data$`Ten-Year Age Groups Code`

data <- data %>% mutate(age = ifelse(age==999, NA, age)) %>% type.convert() %>% mutate(age=str_extract(age, "\\d+"))

data <- data %>% select(-ends_with("code"))

data <- data %>% type.convert()

return(data)

}

process_df_tsv <- function(data, age_intervals){

	if (identical(which(colnames(data) == "Single-Year Ages Code"), integer(0))) {

	return( process_df_ten_year_groups_tsv(data) )

	}

	agecodes <- data$`Single-Year Ages Code`

	data <- data %>% select(-ends_with("code"))

	data$age <- agecodes

	data <- data %>% mutate(age=ifelse(age==999, NA, age))

	data <- data %>% type.convert()

	data <- data %>% mutate(age_cat = cut(age, breaks=age_intervals, right=FALSE)) %>% mutate(age_cat=as.character(age_cat))

	data <- data %>% type.convert(as.is=TRUE)

	#age buckets formatting
	data <- data %>% mutate(age_lb = as.numeric(sub("\\[(\\d+),.*", "\\1", age_cat)), age_ub = as.numeric(sub(".*,(\\d+)\\)", "\\1", age_cat)), age_cat = paste0(age_lb, "-", age_ub-1), age_cat=ifelse(age_cat=="0-0", "< 1", age_cat)) %>% mutate(age=age_lb) %>% select(-c(age_lb, age_ub))

	data <- data %>% mutate(age_cat = ifelse(age == 85, "85+", age_cat))

	return(data)

}

compute_statistics <- function(data, life_exp){

data <- data %>% filter(is.finite(as.numeric(Population))) %>% filter(is.finite(as.numeric(Deaths))) %>% filter(is.finite(as.numeric(`Crude Rate`))) %>% type.convert(as.is=TRUE)

data_agg <- data %>% group_by(age_cat, age, Gender, Race, Year) %>% summarize(deaths=sum(Deaths), population=sum(Population),cruderate=mean(`Crude Rate`) ) %>% ungroup() 



data_combined <- data_agg %>% left_join(life_exp, by=c("age"="age_cat", "Year"="year", "Gender"="Gender"))

data_combined <- data_combined %>% mutate(yrs_lost=life_expectancy*((deaths/population)*100000) )



excess_ypll <- data_combined %>% filter(!is.na(life_expectancy)) %>% group_by(Race, Gender, age_cat, age) %>% summarize(yrs_lost = mean(yrs_lost), pop = sum(population)) %>% ungroup() %>% group_by(Gender, age_cat, age) %>% summarize(excess_yrs_lost = yrs_lost[Race==1] - yrs_lost[Race==3], ratio_excess_yrs_lost = yrs_lost[Race==1] / yrs_lost[Race==3], black_years_lost = yrs_lost[Race==1], white_years_lost = yrs_lost[Race==3], exc_pll_number = excess_yrs_lost * pop[Race==1] * (1/100000) ) %>% ungroup()

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

#Tables

excess_pll_age_table <- excess_ypll %>% select(age_cat, Gender, excess_yrs_lost, exc_pll_number) %>% magrittr::set_colnames(c("Age", "Gender", "excess_ypll_rate", "excess_ypll_number")) %>%
    pivot_longer(cols = starts_with("excess"), 
                 names_to = "AgeType", 
                 values_to = "Value") %>%
    pivot_wider(names_from = c(AgeType, Gender), 
                values_from = Value) %>%
    rename_with(~ str_replace_all(.x, "exc_", ""), 
                starts_with("exc")) 

write_csv(excess_pll_age_table, file = file.path(tabledir, "ypll_sex_age.csv"))

ypll_table_age <- excess_ypll %>% select(Gender, age_cat, black_years_lost, white_years_lost) 

write_csv(ypll_table_age, file = file.path(tabledir, "ypll_race_age.csv"))


#Graphical Component
agevector <- excess_ypll %>% select(age_cat, age) %>% distinct() %>% arrange(age)

#remove 85+ 
agevector <- agevector %>% filter(age < 85)
excess_ypll <- excess_ypll %>% filter(age < 85)
age.data <- age.data %>% filter(age < 85)

sizing_theme <- theme(axis.text = element_text(size=12), axis.title=element_text(size=16), legend.text=element_text(size=12), legend.title=element_text(size=16), plot.title=element_text(size=18, hjust=0.5)) 

panel_theme <- theme_bw() + theme(panel.grid.major.x = element_blank(), panel.grid.minor=element_blank())


indiv_yrs_lost_fig <- excess_ypll %>% select(age_cat, age, Gender, black_years_lost, white_years_lost) %>% ungroup() %>% pivot_longer(-c(age_cat, age, Gender)) %>% ggplot(aes(x=age, y=value, color=name)) + geom_line(size = 0.5) + ylab("YPLL per 100K Individuals") + scale_color_manual(values = c("black_years_lost"="maroon", "white_years_lost"="navy"), labels = c("black_years_lost" = "Black", "white_years_lost" = "White")) + panel_theme + facet_wrap(~Gender, nrow=1) + xlab("Age group (years)") + geom_point(size = 2.5) + theme(axis.text.x = element_text(angle=45, hjust=1)) + scale_x_continuous(limits=c(min(agevector$age),max(agevector$age)), breaks=agevector$age, labels=agevector$age_cat) + theme(legend.title = element_blank())


excess_pll_fig <- excess_ypll %>% ggplot(aes(x=age, y=excess_yrs_lost, group=Gender)) + geom_line(size=0.5) + scale_y_continuous(breaks = scales::pretty_breaks(n = 8)) + sizing_theme + scale_color_manual(values=c("maroon", "navy")) + panel_theme + scale_x_continuous(limits=c(min(agevector$age),max(agevector$age)), breaks=agevector$age, labels=agevector$age_cat) + theme(axis.text.x = element_text(angle=45, hjust=1)) + xlab("Age group (years)") + geom_point(aes(color=Gender), size=2.5) + geom_hline(yintercept=1, linetype="dashed") + ylab("Excess YPLL per 100K individuals") + ggtitle("Excess Years of Potential Life Lost") 


mortality_rate_ratio_fig <- excess_ypll %>% ggplot(aes(x=age, y=ratio_excess_yrs_lost, group=Gender)) + geom_line(size=.5) + ylab("YPLL Rate Ratio (Black / White)") + scale_y_continuous(limits = c(0.75, max(excess_ypll$ratio_excess_yrs_lost + 0.5)), breaks=seq(0.75, max(excess_ypll$ratio_excess_yrs_lost + 0.5), 0.25)) + geom_hline(yintercept=1, linetype="dashed") + sizing_theme + scale_color_manual(values=c("maroon", "navy")) + ggtitle("Black-White Years of Potential Life Lost Rate Ratio by Age") + panel_theme + scale_x_continuous(limits=c(min(agevector$age),max(agevector$age)), breaks=agevector$age, labels=agevector$age_cat) + theme(axis.text.x = element_text(angle=45, hjust=1)) + xlab("Age group (years)") + geom_point(aes(color=Gender), size=2.5)


excess_pll_year_fig <- age.data %>% mutate(Year = str_remove(Year, ".tsv")) %>%  ggplot(aes(x=age, y=excess_yrs_lost, group=Year)) + geom_line(aes(color=Year), size=0.5) + ylab("Excess YPLL per 100K individuals") + scale_y_continuous(limits = c(min(c(0, floor(min(age.data$excess_yrs_lost)/10)*10)), max(age.data$excess_yrs_lost)*1.15), breaks=seq(min(c(0, floor(min(age.data$excess_yrs_lost)/10)*10)), max(age.data$excess_yrs_lost)*1.15, 500)) + geom_hline(yintercept=1, linetype="dashed") + sizing_theme + scale_color_manual(name="Year Range", values=cbb[1:6]) + ggtitle("Excess Years of Potential Life Lost Rate Among Black Population by Age and Gender") + panel_theme + scale_x_continuous(limits=c(min(agevector$age),max(agevector$age)), breaks=agevector$age, labels=agevector$age_cat) + theme(axis.text.x = element_text(angle=45, hjust=1)) + xlab("Age group (years)") + geom_point(aes(color=Year), size=2.5) + facet_wrap(~Gender) + theme(strip.text = element_text(size=11), strip.background=element_rect(fill="transparent", color="transparent"))



save_plot(excess_pll_fig, plotdir)
save_plot(mortality_rate_ratio_fig, plotdir)
save_plot(excess_pll_year_fig, plotdir)
save_plot(indiv_yrs_lost_fig, plotdir)


save_rds(excess_pll_fig, outputdir)
save_rds(mortality_rate_ratio_fig, outputdir)
save_rds(excess_pll_year_fig, outputdir)
save_rds(indiv_yrs_lost_fig, outputdir)
















