library(tidyverse)
library(haven)

source("preprocess-function.R")

age_intervals <- c(0, 1, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70, 75, 80, 85)

#CDC Wonder input file
inputfile <- file.path("../data", "export_deaths_crude_race_gender_age_year.txt")
lifeexp_file <- "../data/file_life_expectancy_1999_to_2020.dta"

#output file name and directory
outputdir <- "../results"
outputpath <- file.path(outputdir, "life-years-lost-by-age.rds")

#plot path
plotdir <- "../plots/life-years-lost-by-age"

#palette of colors
cbb <- c("#E69F00", "#56B4E9", "#009E73", "#0072B2", "#D55E00", "#CC79A7")


agefiles <- list.files("../data", "^deaths_crude_race_gender_age_", full.names=TRUE)
agefiles <- agefiles[grepl("year", agefiles)]

life_exp <- read_dta(lifeexp_file) %>% mutate(Gender=factor(gender)) %>% mutate(Gender=ifelse(Gender=="1", "Female", "Male")) %>% select(-gender) %>% mutate(Gender = as.character(Gender))



############ CODE ###############
process_df <- function(data, age_intervals){
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
	data_agg <- data %>% group_by(age_cat, age, Gender, Race, Year) %>% summarize(deaths=sum(Deaths), population=sum(Population),cruderate=mean(`Crude Rate`) ) %>% ungroup()

	data_combined <- data_agg %>% left_join(life_exp, by=c("age"="age_cat", "Year"="year", "Gender"="Gender")) %>% filter(age < 84)

	data_combined <- data_combined %>% mutate(yrs_lost=life_expectancy*((deaths/population)*100000) )

	excess_ypll <- data_combined %>% group_by(Race, Gender, age_cat, age) %>% summarize(yrs_lost = mean(yrs_lost)) %>% ungroup() %>% group_by(Gender, age_cat, age) %>% summarize(excess_yrs_lost = yrs_lost[Race==1] - yrs_lost[Race==3], ratio_excess_yrs_lost = yrs_lost[Race==1] / yrs_lost[Race==3]) %>% ungroup()

	return(excess_ypll)

}


wrapper_pll_by_age <- function(relfilepath, life_exp, age_intervals){

	data <- preprocess_cdc_wonder_file(relfilepath)

	relfilepath <- basename(relfilepath)

	data <- process_df(data, age_intervals)

	excess_ypll <- compute_statistics(data, life_exp)

	excess_ypll <- excess_ypll %>% mutate(Year = gsub("deaths_crude_race_gender_age_year_|\\.txt", "", relfilepath))

	excess_ypll

}

data <- preprocess_cdc_wonder_file(inputfile)

excess_ypll <- data %>% process_df(age_intervals) %>% compute_statistics(life_exp)

age.data <- do.call(rbind, lapply(agefiles, function(x) wrapper_pll_by_age(x, life_exp, age_intervals)))

if (!dir.exists(outputdir)){

	dir.create(outputdir)

}

saveRDS(object = list(age.data=age.data, excess_deaths_age=excess_ypll), file=outputpath)





#Graphical Component
agevector <- excess_ypll %>% select(age_cat, age) %>% distinct() %>% arrange(age)

sizing_theme <- theme(axis.text = element_text(size=12), axis.title=element_text(size=16), legend.text=element_text(size=12), legend.title=element_text(size=16), plot.title=element_text(size=18, hjust=0.5)) 

panel_theme <- theme_bw() + theme(panel.grid.major.x = element_blank(), panel.grid.minor=element_blank())

excess_pll_fig <- excess_ypll %>% ggplot(aes(x=age, y=excess_yrs_lost, group=Gender)) + geom_line(size=0.5) + ylab("Excess YPLL per 100K individuals") + scale_y_continuous(limits = c(-15000, 60000), breaks=seq(-15000, 60000, 5000)) + sizing_theme + scale_color_manual(values=c("maroon", "navy")) + ggtitle("Excess Years of Potential Life Lost Rate Among the Black Population by Age") + panel_theme + scale_x_continuous(limits=c(0,80), breaks=agevector$age, labels=agevector$age_cat) + theme(axis.text.x = element_text(angle=45, hjust=1)) + xlab("Age group (years)") + geom_point(aes(color=Gender), size=2.5) + geom_hline(yintercept=1, linetype="dashed")


mortality_rate_ratio_fig <- excess_ypll %>% ggplot(aes(x=age, y=ratio_excess_yrs_lost, group=Gender)) + geom_line(size=.5) + ylab("YPLL Rate Ratio (Black / White)") + scale_y_continuous(limits = c(0.7, 2.5), breaks=seq(0.7, 2.5, 0.1)) + geom_hline(yintercept=1, linetype="dashed") + sizing_theme + scale_color_manual(values=c("maroon", "navy")) + ggtitle("Black-White Years of Potential Life Lost Rate Ratio by Age") + panel_theme + scale_x_continuous(limits=c(0,80), breaks=agevector$age, labels=agevector$age_cat) + theme(axis.text.x = element_text(angle=45, hjust=1)) + xlab("Age group (years)") + geom_point(aes(color=Gender), size=2.5)


excess_pll_year_fig <- age.data %>% ggplot(aes(x=age, y=excess_yrs_lost, group=Year)) + geom_line(aes(color=Year), size=0.5) + ylab("Excess YPLL per 100K individuals") + scale_y_continuous(limits = c(-15000, 70000), breaks=seq(-15000, 5000, 70000)) + geom_hline(yintercept=1, linetype="dashed") + sizing_theme + scale_color_manual(name="Year Range", values=cbb[1:6]) + ggtitle("Excess Years of Potential Life Lost Rate Among Black Population by Age and Gender") + panel_theme + scale_x_continuous(limits=c(0,80), breaks=agevector$age, labels=agevector$age_cat) + theme(axis.text.x = element_text(angle=45, hjust=1)) + xlab("Age group (years)") + geom_point(aes(color=Year), size=2.5) + facet_wrap(~Gender) + theme(strip.text = element_text(size=11), strip.background=element_rect(fill="transparent", color="transparent"))



save_plot(excess_pll_fig, plotdir)
save_plot(mortality_rate_ratio_fig, plotdir)
save_plot(excess_pll_year_fig, plotdir)


















