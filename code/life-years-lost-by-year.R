library(tidyverse)
library(haven)
library(splines)

args = commandArgs(trailingOnly=TRUE)

source("preprocess-function.R")

project <- args[1]

years <- seq(1999, 2021, 1)

inputfile <- file.path(file.path("../data", project), "export_age_deaths_race_gender_year_se.tsv")

lifeexp_file <- "../data/file_life_expectancy_1999_to_2020.dta"

outputdir <- file.path("../results", project)
outputfile <- "life-years-lost-by-year.rds"

create_output_dir(outputdir)

#create age categorical buckets
age_intervals <- c(0, 1, 5, 10, 15, 20, 25, 30, 35, 40, 45, 50, 55, 60, 65, 70, 75, 80, 85, Inf)

#plot path
plotdir <- file.path("../plots", project, "life-years-lost-by-year")

#palette of colors
cbb <- c("#E69F00", "#56B4E9", "#009E73", "#0072B2", "#D55E00", "#CC79A7")


##### CODE ######

data <- preprocess_cdc_wonder(inputfile)

agecodes <- data$`Single-Year Ages Code`

data <- data %>% select(-ends_with("code"))

cols.to.rename <- c("Single-Year Ages", "Crude Rate Upper 95% Confidence Interval", "Crude Rate Standard Error", "Crude Rate Lower 95% Confidence Interval")

new.col.names <- c("agegroup", "cruderate_ci_ub", "cruderate_se", "cruderate_ci_lb")

data <- rename_columns(data, cols.to.rename, new.col.names)

data$age <- agecodes

data <- data %>% mutate(age=ifelse(age==999, NA, age))

data <- data %>% type.convert()

data <- data %>% mutate(age_cat = cut(age, breaks=age_intervals, right=FALSE)) %>% mutate(age_cat=as.character(age_cat))

data <- data %>% mutate(age_cat = ifelse(age == 85, "85+", age_cat))

data <- data %>% type.convert(as.is=TRUE)

data <- data %>% filter(!is.na(Population))

data_by_age_gender_race <- data %>% group_by(age_cat, Gender, Race, Year) %>% summarize(deaths=sum(Deaths), population=sum(Population),cruderate=mean(`Crude Rate`) ) %>% ungroup() 

#combine with life expectancy table
life_exp <- read_dta(lifeexp_file) %>% mutate(Gender=factor(gender)) %>% mutate(Gender=ifelse(Gender=="1", "Female", "Male")) %>% select(-gender) %>% mutate(Gender = as.character(Gender))


data_combined <- data_by_age_gender_race %>% mutate(age_bkt_lb = as.numeric(sub("\\[(\\d+),.*", "\\1", age_cat))) %>% left_join(life_exp, by=c("age_bkt_lb"="age_cat", "Year"="year", "Gender"="Gender")) %>% filter(!is.na(life_expectancy))

#variable life expectancy is the average remaining years people would have lived if didn't die:

data_combined <- data_combined %>% mutate(yrs_lost=life_expectancy*((deaths/population)*100000) )

#Estimating yrs_lost SE.

zcrit <- qnorm(0.975)


#Wald interval, which is based on the assumption that the sampling distribution of the proportion is approximately normal. The CI is calculated as proportion +/- z*standard_error
#standard error of binomial is sqrt(p*(1-p) / n)

lly <- data_combined %>% mutate(prop = deaths / population, se = sqrt(prop * (1-prop) * (1/population) ), yrs_lost_se = (life_expectancy * se * 100000 )^2 ) %>% group_by(Gender, Race, Year) %>% summarize(yrs_lost=mean(yrs_lost), n=n(), yrs_lost_se=sum(yrs_lost_se), population=sum(population)) %>% ungroup() %>% mutate(yrs_lost_se = sqrt(yrs_lost_se/n))


excess_pll <- lly %>% group_by(Gender, Year) %>% summarize(excess_yrs_lost = yrs_lost[Race==1] - yrs_lost[Race==3], excess_yrs_lost_se = sqrt(sum(yrs_lost_se^2) ), excess_yrs_lost_lb = excess_yrs_lost - (excess_yrs_lost_se * zcrit), excess_yrs_lost_ub = excess_yrs_lost + (excess_yrs_lost_se * zcrit), ratio_excess_yrs_lost = yrs_lost[Race==1]/yrs_lost[Race==3], exc_ypll_number = excess_yrs_lost * population[Race==1] * (1/100000), yrs_lost_black = yrs_lost[Race==1], yrs_lost_white=yrs_lost[Race==3]) %>% ungroup()

women.pll <- gender_arima_years_lost(lly, excess_pll, "Female", 2012, zcrit)

men.pll <- gender_arima_years_lost(lly, excess_pll, "Male", c(2007, 2011), zcrit)


excess_pll %>% group_by(Gender) %>% summarize(exc_ypll_number = sum(exc_ypll_number), excess_yrs_lost = mean(excess_yrs_lost), yrs_lost_black = mean(yrs_lost_black), yrs_lost_white = mean(yrs_lost_white))


excess_pll %>% group_by(Year) %>% summarize(exc_ypll_number = sum(exc_ypll_number))


excess_pll_w_pred <- excess_pll %>% left_join(women.pll$arima %>% select(-excess_yrs_lost), by=c("Gender"="Gender", "Year"="Year")) %>% left_join(., men.pll$arima %>% select(-excess_yrs_lost), by=c("Gender"="Gender", "Year"="Year")) %>% mutate(pred_excess_yrs_lost = ifelse(is.na(pred_excess_yrs_lost.x), pred_excess_yrs_lost.y, pred_excess_yrs_lost.x)) %>% select(-c(pred_excess_yrs_lost.y, pred_excess_yrs_lost.x))

saveRDS(object=list(excess_pll_w_pred = excess_pll_w_pred), file=file.path(outputdir, outputfile))

#GRAPHICAL COMPONENT

sizing_theme <- theme(axis.text = element_text(size=12), axis.title=element_text(size=16), legend.text=element_text(size=14), legend.title=element_text(size=16), plot.title=element_text(size=18, hjust=0.5)) 

year_label <- scale_x_continuous(breaks=years, labels= function(x) ifelse(x %% 2 == 1, x, ""))

panel_theme <- theme_bw() + theme(panel.grid.major.x = element_blank(), panel.grid.minor=element_blank())

indiv_ypll_fig <- excess_pll_w_pred %>% select(Gender, Year, yrs_lost_black, yrs_lost_white) %>% ungroup() %>% pivot_longer(-c(Gender, Year)) %>% ggplot(aes(x=Year, y=value, color=name)) + geom_line(size = 0.5) + ylab("YPLL per 100K Individuals") + scale_color_manual(values = c("yrs_lost_black"="maroon", "yrs_lost_white"="navy"), labels = c("yrs_lost_black" = "Black", "yrs_lost_white" = "White")) + panel_theme + facet_wrap(~Gender, nrow=1) + xlab("Year") + geom_point(size = 2.5) + year_label + theme(legend.title = element_blank())

excess_pll_rate_fig <- ggplot() + geom_line(data=excess_pll_w_pred, aes(x=Year, y=pred_excess_yrs_lost, color=Gender), size=1) + geom_line(data=excess_pll_w_pred %>% filter(Year>=2019) %>% mutate(diff=ifelse(Year==2019, pred_excess_yrs_lost, excess_yrs_lost)), aes(x=Year, y=diff, color=Gender), linetype="dashed", size=1) + geom_hline(yintercept=0, linetype="dotted")  + scale_y_continuous(limits = c(min(c(0, floor(min(excess_pll_w_pred$pred_excess_yrs_lost/1000, na.rm=TRUE)*1000))), max(excess_pll_w_pred$pred_excess_yrs_lost, na.rm=TRUE)*1.1), breaks=seq(min(c(0, floor(min(excess_pll_w_pred$pred_excess_yrs_lost/1000, na.rm=TRUE)*1000))), max(excess_pll_w_pred$pred_excess_yrs_lost, na.rm=TRUE)*1.1, 250)) + ylab("Excess YPLL per 100K individuals") + xlab("Year") + year_label + panel_theme 

excess_pll_rate_fig <- excess_pll_rate_fig + geom_point(data = excess_pll_w_pred %>% filter(Year %in% c(2012) & Gender == "Female" | Year %in% c(2007, 2011) & Gender == "Male"), aes(x=Year, y=pred_excess_yrs_lost, color=Gender, shape=Gender), size=3.75) + scale_color_manual(values=c("maroon", "navy")) + sizing_theme  + ggtitle("Estimated Excess Years of Potential Life Lost Rate Among the Black Population") 

mortality_rate_ratio_fig <- excess_pll_w_pred %>% ggplot(aes(x=Year, y=ratio_excess_yrs_lost, color=Gender)) + geom_line(size=1.25) + ylab("YPLL Rate Ratio (Black / White)") + scale_y_continuous(limits = c(0.75, max(excess_pll_w_pred$ratio_excess_yrs_lost + 0.5, na.rm=TRUE)), breaks=seq(0.75, max(excess_pll_w_pred$ratio_excess_yrs_lost + 0.5, na.rm=TRUE), 0.2)) + geom_hline(yintercept=1, linetype="dashed") + sizing_theme + scale_color_manual(values=c("maroon", "navy")) + ggtitle("Black-White YPLL rate ratio") + year_label + panel_theme

excess_pll_fig <- excess_pll_w_pred %>% ggplot(aes(x=Year, y=exc_ypll_number, color=Gender)) + geom_line(size=1.25) + ylab("YPLL") + scale_y_continuous(limits = c(0,max(excess_pll_w_pred$exc_ypll_number, na.rm=TRUE)*1.1), breaks=seq(0, max(excess_pll_w_pred$exc_ypll_number, na.rm=TRUE)*1.1, 25000)) + sizing_theme + scale_color_manual(values=c("maroon", "navy")) + ggtitle("Estimated Excess Years of Potential Life Lost Among the Black Population") + year_label + panel_theme


cum_sex <- excess_pll_w_pred %>% group_by(Gender) %>% arrange(Year) %>% summarize(pll = cumsum(exc_ypll_number), Year=Year) %>% ungroup() %>% mutate(pll=pll/1000000)

cumulative_sex_pll_fig <- cum_sex %>% ggplot(aes(x=Year, y=pll, color=Gender)) + geom_point() + geom_line() + year_label + panel_theme + sizing_theme + ggtitle("Estimated Cumulative Excess Years of Potential Life\nLost Among the Black Population") + ylab("Excess Years of Potential Life Lost (Millions)") + scale_y_continuous(limits=c(0,max(cum_sex$pll, na.rm=TRUE)*1.1), breaks=seq(0,max(cum_sex$pll, na.rm=TRUE)*1.1, 1)) + scale_color_manual(name="", values=c("maroon", "navy"))


combpll <- excess_pll_w_pred %>% group_by(Year) %>% summarize(pll = sum(exc_ypll_number)) %>% ungroup() 

combined_pll_fig <- combpll %>% ggplot(aes(x=Year, y=pll)) + geom_point() + geom_line() + year_label + panel_theme + sizing_theme + ggtitle("Estimated Excess Years of Potential Life Lost Among the Black Population\nMales and Females Combined") + ylab("YPLL") + scale_y_continuous(limits=c(0,max(combpll$pll, na.rm=TRUE)*1.1), breaks=seq(0,max(combpll$pll, na.rm=TRUE)*1.1,50000)) + scale_color_manual(name="", values=c("maroon", "navy"))


cumcomb <- excess_pll_w_pred %>% group_by(Year) %>% summarize(pll = sum(exc_ypll_number)) %>% ungroup() %>% arrange(Year) %>% mutate(pll_cum=cumsum(pll)) %>% mutate(pll_cum = pll_cum / 1e6) 

cumulative_combined_pll_fig <- cumcomb %>% ggplot(aes(x=Year, y=pll_cum)) + geom_point() + geom_line() + year_label + panel_theme + sizing_theme + ggtitle("Estimated Excess Years of Potential Life Lost Among the Black Population\nMales and Females Combined") + ylab("Excess Years of Potential Life Lost (Millions)") + scale_y_continuous(limits=c(0,max(cumcomb$pll_cum, na.rm=TRUE)*1.1), breaks=seq(0,max(cumcomb$pll_cum, na.rm=TRUE)*1.1, 1)) + scale_color_manual(name="", values=c("maroon", "navy"))



yrslost <- excess_pll_w_pred %>% select(Gender, yrs_lost_white, yrs_lost_black, Year) %>% pivot_longer(-c(Gender, Year)) %>% mutate(Race=gsub(".*_", "", name) %>% str_to_title(.)) %>% select(-name) 

yrs_lost_race_sex_fig <- yrslost %>% ggplot(aes(x=Year, y=value, color=Race, shape=Gender, linetype=Gender)) + geom_line(size=0.5) + geom_point(size=2) + panel_theme + sizing_theme + year_label + scale_y_continuous(limits=c(0, max(yrslost$value, na.rm=TRUE)*1.1), breaks=seq(0, max(yrslost$value, na.rm=TRUE)*1.1, 1000)) + ylab("YPLL per 100K individuals") + ggtitle("Estimated Years of Potential Life Lost") + scale_color_manual(values=cbb[1:2])




save_plot(excess_pll_rate_fig, plotdir)
save_plot(mortality_rate_ratio_fig, plotdir)
save_plot(excess_pll_fig, plotdir)
save_plot(cumulative_sex_pll_fig, plotdir)
save_plot(combined_pll_fig, plotdir)
save_plot(cumulative_combined_pll_fig, plotdir)
save_plot(yrs_lost_race_sex_fig, plotdir)
save_plot(indiv_ypll_fig, plotdir)





























