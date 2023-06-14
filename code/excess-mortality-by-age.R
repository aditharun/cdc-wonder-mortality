library(tidyverse)
library(haven)

args = commandArgs(trailingOnly=TRUE)

source("preprocess-function.R")

project <- args[1]

#create age categorical buckets
age_intervals <- c(0, 1, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70, 75, 80, 85)

#files with age related data
#starts with deaths_crude_race_gender_age but does not contain year in the file name

agefiles <- list.files(file.path("../data", project), "^deaths_crude_race_gender_age_", full.names=TRUE)
agefiles <- agefiles[!grepl("year", agefiles)]

#CDC Wonder input filea
# "export_deaths_crude_race_gender_age.txt"
inputfile <- file.path(file.path("../data", project), "export_deaths_crude_race_gender_age.tsv")

#output file name and directory
outputdir <- file.path("../results", project)
outputpath <- file.path(outputdir, "excess-mortality-by-age.rds")

create_output_dir(outputdir)

#palette of colors
cbb <- c("#E69F00", "#56B4E9", "#009E73", "#0072B2", "#D55E00", "#CC79A7")

#plot path
plotdir <- file.path("../plots", project, "excess-mortality-by-age")

######### CODE ####################

process_df_tsv <- function(data, age_intervals){

	data <- data %>% filter(is.na(Notes)) %>% select(-Notes)

	agecodes <- data$`Single-Year Ages Code`

	data <- data %>% select(-ends_with("code"))

	data$age <- agecodes

	data <- data %>% mutate(age=ifelse(age == 999, NA, age))

	data <- data %>% type.convert()

	data <- data %>% filter(age <= 84)

	data <- data %>% mutate(age_cat = cut(age, breaks=age_intervals, right=FALSE)) %>% mutate(age_cat=as.character(age_cat))

	data <- data %>% type.convert(as.is=TRUE)

	#age buckets formatting
	data <- data %>% mutate(age_lb = as.numeric(sub("\\[(\\d+),.*", "\\1", age_cat)), age_ub = as.numeric(sub(".*,(\\d+)\\)", "\\1", age_cat)), age_cat = paste0(age_lb, "-", age_ub-1), age_cat=ifelse(age_cat=="0-0", "< 1", age_cat)) %>% mutate(age=age_lb) %>% select(-c(age_lb, age_ub))

	return(data)

}


#creates age categorical buckets and filters age <= 84 and removes extraneous columns and corrects for age missing values
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

compute_statistics <- function(data){

	data <- data %>% filter(is.finite(as.numeric(Deaths))) %>% filter(is.finite(as.numeric(Population))) %>% type.convert(as.is=TRUE)

	data_agg <- data %>% group_by(age_cat, age, Gender, Race) %>% summarize(deaths=sum(Deaths), population=sum(Population),cruderate=mean(`Crude Rate`) ) %>% ungroup()

	excess_deaths_age <- data_agg %>% group_by(age_cat, age, Gender) %>% summarize(unadj_excess_deaths_number = deaths[Race==1] - (population[Race==1]*(deaths[Race==3]/population[Race==3])), unadj_excess_deaths_rate = ((deaths[Race==1]/population[Race==1])*100000) - ((deaths[Race==3]/population[Race==3])*100000), ratio_unadj_excess_deaths_rate = ((deaths[Race==1]/population[Race==1])*100000) / ((deaths[Race==3]/population[Race==3])*100000), black_death_rate = (deaths[Race==1]/population[Race==1])*100000, white_death_rate = (deaths[Race==3] / population[Race==3])*100000)

	return(list(data_agg=data_agg, excess_deaths_age = excess_deaths_age))
}

wrapper_age_excess_deaths <- function(relativefilepath, age_intervals){

	data <- preprocess_cdc_wonder(relativefilepath)

	relativefilepath <- basename(relativefilepath)

	data <- process_df_tsv(data, age_intervals)

	excess_deaths_age <- compute_statistics(data)$excess_deaths_age

	excess_deaths_age <- excess_deaths_age %>% mutate(Year = gsub("deaths_crude_race_gender_age_|\\.txt", "", relativefilepath))

	return(excess_deaths_age)
}


data <- preprocess_cdc_wonder(inputfile)

excess_deaths_age <- data %>% process_df_tsv(age_intervals) %>% compute_statistics() %>% purrr::pluck("excess_deaths_age")

age.data <- do.call(rbind, lapply(agefiles, function(x) wrapper_age_excess_deaths(x, age_intervals)))

saveRDS(object = list(age.data=age.data, excess_deaths_age=excess_deaths_age), file=outputpath)


#Graphical Component
agevector <- excess_deaths_age %>% select(age_cat, age) %>% distinct() %>% arrange(age)

sizing_theme <- theme(axis.text = element_text(size=12), axis.title=element_text(size=16), legend.text=element_text(size=14), legend.title=element_text(size=16), plot.title=element_text(size=18, hjust=0.5)) 

panel_theme <- theme_bw() + theme(panel.grid.major.x = element_blank(), panel.grid.minor=element_blank())

adj_y <- function(data, var){

	vals <- data %>% pull(var)

	lim_max <- max(vals) * 1.15

	if (max(vals) < 10){

		lim_min <- 0.75

		bkpoints <- seq(lim_min, lim_max, 0.5)

	} else{

		lim_min <- 0

		bkpoints <- seq(lim_min, lim_max, 25)

	}

	lims <- c(lim_min, lim_max)


	return(list(lims=lims, bk=bkpoints))


}

indiv_death_rate_fig <- excess_deaths_age %>% select(age_cat, age, Gender, white_death_rate, black_death_rate) %>% ungroup() %>% pivot_longer(-c(age_cat, age, Gender)) %>% ggplot(aes(x=age, y=value, color=name)) + geom_line(size = 0.5) + ylab("Deaths per 100K individuals") + scale_color_manual(values = c("black_death_rate"="maroon", "white_death_rate"="navy"), labels = c("black_death_rate" = "Black", "white_death_rate" = "White")) + panel_theme + facet_wrap(~Gender, nrow=1) + xlab("Age group (years)") + geom_point(size = 2.5) + theme(axis.text.x = element_text(angle=45, hjust=1)) + scale_x_continuous(limits=c(0,80), breaks=agevector$age, labels=agevector$age_cat) + theme(legend.title = element_blank())


excess_death_rate_age_fig <- excess_deaths_age %>% ggplot(aes(x=age, y=unadj_excess_deaths_rate, group=Gender, color=Gender)) + geom_line(size=0.5) + ylab("Excess deaths per 100K individuals") + geom_hline(yintercept=1, linetype="dashed") + sizing_theme + scale_color_manual(values=c("maroon", "navy")) + ggtitle("Excess Mortality Rate Among the Black Population by Age") + panel_theme + scale_x_continuous(limits=c(0,80), breaks=agevector$age, labels=agevector$age_cat) + theme(axis.text.x = element_text(angle=45, hjust=1)) + xlab("Age group (years)") + geom_point(aes(color=Gender), size=2.5) + scale_y_continuous(limits = adj_y(excess_deaths_age, "unadj_excess_deaths_rate")$lims, breaks=adj_y(excess_deaths_age, "unadj_excess_deaths_rate")$bk)


mortality_rate_ratio_fig <- excess_deaths_age %>% ggplot(aes(x=age, y=ratio_unadj_excess_deaths_rate, group=Gender, color=Gender)) + geom_line(size=.5) + ylab("Mortality Rate Ratio (Black / White)") + geom_hline(yintercept=1, linetype="dashed") + sizing_theme + scale_color_manual(values=c("maroon", "navy")) + ggtitle("Black-White Mortality Rate Ratio by Age") + panel_theme + scale_x_continuous(limits=c(0,80), breaks=agevector$age, labels=agevector$age_cat) + theme(axis.text.x = element_text(angle=45, hjust=1)) + xlab("Age group (years)") + geom_point(aes(color=Gender), size=2.5) + scale_y_continuous(limits = adj_y(excess_deaths_age, "ratio_unadj_excess_deaths_rate")$lims, breaks=adj_y(excess_deaths_age, "ratio_unadj_excess_deaths_rate")$bk) 


excess_death_rate_age_year_fig <- age.data %>% ggplot(aes(x=age, y=unadj_excess_deaths_rate, group=Year)) + geom_line(aes(color=Year), size=0.5) + ylab("Excess deaths per 100K individuals") + geom_hline(yintercept=1, linetype="dashed") + sizing_theme + scale_color_manual(name="Year Range", values=cbb[1:6]) + ggtitle("Excess Mortality Rate Among Black Population by Age and Gender") + panel_theme + scale_x_continuous(limits=c(0,80), breaks=agevector$age, labels=agevector$age_cat) + theme(axis.text.x = element_text(angle=45, hjust=1)) + xlab("Age group (years)") + geom_point(aes(color=Year), size=2.5) + facet_wrap(~Gender) + theme(strip.text = element_text(size=11), strip.background=element_rect(fill="transparent", color="transparent")) + scale_y_continuous(limits = adj_y(age.data, "unadj_excess_deaths_rate")$lims, breaks=adj_y(age.data, "unadj_excess_deaths_rate")$bk)





save_plot(excess_death_rate_age_fig, plotdir)
save_plot(mortality_rate_ratio_fig, plotdir)
save_plot(excess_death_rate_age_year_fig, plotdir)
save_plot(indiv_death_rate_fig, plotdir)








