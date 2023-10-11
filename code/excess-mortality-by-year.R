library(tidyverse)
library(haven)
library(splines)

args = commandArgs(trailingOnly=TRUE)

source("preprocess-function.R")

project <- args[1]

years <- seq(1999, 2021, 1)

inputfile <- file.path(file.path("../data", project), "export_age_adjusted_deaths_race_gender_year_se.tsv")
inputfile2 <- file.path(file.path("../data", project), "export_deaths_race_gender_age_year.tsv")

#output directory and file
outputdir <- file.path("../results", project, "excess-mortality-by-year")

#table directory

#plot output dir
plotdir <- file.path("../../cdc-wonder-output", project, "excess-mortality-by-year")

#palette of colors
cbb <- c("#E69F00", "#56B4E9", "#009E73", "#0072B2", "#D55E00", "#CC79A7")

tabledir <- file.path(dirname(plotdir), "tables")
create_output_dir(tabledir)

######## CODE ######
#data <- preprocess_cdc_wonder_file(inputfile)
data <- preprocess_cdc_wonder(inputfile)

cols.to.rename <- c("Crude Rate Lower 95% Confidence Interval", "Crude Rate Upper 95% Confidence Interval", "Crude Rate Standard Error", "Age Adjusted Rate Lower 95% Confidence Interval", "Age Adjusted Rate Upper 95% Confidence Interval", "Age Adjusted Rate Standard Error")

new.col.names <- c("cruderate_ci_lb", "cruderate_ci_ub", "cruderate_se", "ageadjustedrate_ci_lb", "ageadjustedrate_ci_ub", "ageadjustedrate_se")

data <- rename_columns(data, cols.to.rename, new.col.names)

data <- data %>% select(-ends_with("code"))

data <- data %>% type.convert()

#significance level of 0.05
zcrit <- qnorm(0.975)

data <- data %>% filter(is.finite(as.numeric(ageadjustedrate_se))) %>% filter(is.finite(as.numeric(`Age Adjusted Rate`))) 

excess_deaths_year <- data %>% group_by(gender, Gender, Year) %>% summarize(diff_black=`Age Adjusted Rate`[Race==1] - `Age Adjusted Rate`[Race==3], diff_black_SE = sqrt(ageadjustedrate_se[Race==1]^2 + ageadjustedrate_se[Race==3]^2), diff_black_lb = diff_black - (diff_black_SE * zcrit), diff_black_ub = diff_black + (diff_black_SE * zcrit), adj_excess_deaths_rate = diff_black, ratio_adj_excess_deaths_rate = `Age Adjusted Rate`[Race==1]/`Age Adjusted Rate`[Race==3], ageadjrate_black=`Age Adjusted Rate`[Race==1], ageadjrate_white = `Age Adjusted Rate`[Race==3]) %>% ungroup() 


#men and women trends
women.excess.age.adj.deaths <- gender_arima_age_adjusted_death(data, excess_deaths_year, "Female", 2015, zcrit)

men.excess.age.adj.deaths <- gender_arima_age_adjusted_death(data, excess_deaths_year, "Male", c(2007, 2011), zcrit)

excess_deaths_year %>% group_by(Gender) %>% summarize(diff_black=mean(diff_black)); data %>% group_by(Race, Gender) %>% summarize(age_adj_rate=mean(`Age Adjusted Rate`)) %>% ungroup()

data2 <- preprocess_cdc_wonder(inputfile2)
colnames(data2)[which(colnames(data2)=="Five-Year Ages")] <- "agegroup"
data2 <- data2 %>% filter(agegroup!="Not Stated") %>% filter(Population!="Not Applicable") %>% select(-ends_with("code")) %>% type.convert(as.is=TRUE)
data2 <- data2 %>% mutate(age=as.integer(factor(agegroup))-1)

data2 <- data2 %>% filter(is.finite(as.numeric(Population))) %>% filter(is.finite(as.numeric(Deaths))) %>% filter(is.finite(as.numeric(`Crude Rate`))) %>% type.convert(as.is=TRUE)

excess_deaths <- data2 %>% group_by(Gender, agegroup, age, Year) %>% summarize(expected_deaths_black = Population[Race==1]*(Deaths[Race==3]/Population[Race==3]), age_specific_rate_ratio=`Crude Rate`[Race==1]/`Crude Rate`[Race==3], observed_rate_black=(Deaths[Race==1]/Population[Race==1])*100000, observed_rate_white=(Deaths[Race==3]/Population[Race==3])*100000, expected_rate_black=(expected_deaths_black/Population[Race==1])*100000, excess_rate_black = observed_rate_black - expected_rate_black, excess_deaths_black = (excess_rate_black * Population[Race==1])/100000, observed_vs_expected_rate_ratio = observed_rate_black / expected_rate_black) %>% ungroup()

excess_deaths %>% group_by(Gender) %>% summarize(excess_deaths_black=sum(excess_deaths_black))

ageadj_mortality_rate <- left_join(excess_deaths_year, women.excess.age.adj.deaths$gender_adj_rate_arima %>% select(-diff_black), by=c("gender"="gender", "Year"="Year", "Gender"="Gender")) %>% left_join(., men.excess.age.adj.deaths$gender_adj_rate_arima %>% select(-diff_black), by=c("gender"="gender", "Year"="Year", "Gender"="Gender")) %>% mutate(pred_diff_black = ifelse(is.na(pred_diff_black.x), pred_diff_black.y, pred_diff_black.x)) %>% select(-c(pred_diff_black.y, pred_diff_black.x))


#Create Tables

aadr_indiv_table <- ageadj_mortality_rate %>% select(Gender, Year, ageadjrate_black, ageadjrate_white) %>%
  pivot_longer(cols = starts_with("age"), 
               names_to = "AgeType", 
               values_to = "Value") %>%
  pivot_wider(names_from = c(AgeType, Gender), 
              values_from = Value) %>%
  rename_with(~ str_replace_all(.x, "ageadjrate_", ""), 
              starts_with("age"))


excess_aamr_table <- ageadj_mortality_rate %>% mutate(mrr = ageadjrate_black / ageadjrate_white) %>% select(mrr, diff_black, Gender, Year) %>% magrittr::set_colnames(c("mrr", "excess_aamr", "Gender", "Year")) %>%
  pivot_longer(cols = c(excess_aamr, mrr), 
               names_to = "Metric", 
               values_to = "Value") %>%
  pivot_wider(names_from = c(Gender, Metric), 
              values_from = Value) %>%
  select(Year, ends_with("excess_aamr"), ends_with("mrr"))


write_csv(aadr_indiv_table, file = file.path(tabledir, "aadr_race_sex_year.csv"))
write_csv(excess_aamr_table, file = file.path(tabledir, "excess_aadr_year.csv"))


#GRAPHICAL COMPONENT

sizing_theme <- theme(axis.text = element_text(size=12), axis.title=element_text(size=16), legend.text=element_text(size=12), legend.title=element_text(size=16), plot.title=element_text(size=18, hjust=0.5)) 

year_label <- scale_x_continuous(breaks=years, labels= function(x) ifelse(x %% 2 == 1, x, ""))

panel_theme <- theme_bw() + theme(panel.grid.major.x = element_blank(), panel.grid.minor=element_blank())


indiv_ageadj_rate_fig <- ageadj_mortality_rate %>% select(Gender, Year, ageadjrate_black, ageadjrate_white) %>% ungroup() %>% pivot_longer(-c(Gender, Year)) %>% ggplot(aes(x=Year, y=value, color=name)) + geom_line(size = 0.5) + ylab("Age Adjusted Mortality Rate") + scale_color_manual(values = c("ageadjrate_black"="maroon", "ageadjrate_white"="navy"), labels = c("ageadjrate_black" = "Black", "ageadjrate_white" = "White")) + panel_theme + facet_wrap(~Gender, nrow=1) + xlab("Year") + geom_point(size = 2.5) + year_label + theme(legend.title = element_blank())


#PRED ARIMA
#Don't write this out for now
excess_death_rate_arima_fig <- ggplot() + geom_line(data=ageadj_mortality_rate, aes(x=Year, y=pred_diff_black, color=Gender), size=1) + geom_line(data=ageadj_mortality_rate %>% filter(Year>=2019) %>% mutate(diff_black=ifelse(Year==2019, pred_diff_black, diff_black)), aes(x=Year, y=diff_black, color=Gender), linetype="dashed", size=1) + geom_hline(yintercept=0, linetype="dotted") + year_label + panel_theme + geom_point(data = ageadj_mortality_rate %>% filter(Year %in% c(2015) & Gender == "Female" | Year %in% c(2007, 2011) & Gender == "Male"), aes(x=Year, y=pred_diff_black, color=Gender, shape=Gender), size=3.75) + scale_color_manual(values=c("maroon", "navy")) + sizing_theme  + ggtitle("Excess Age Adjusted Mortality Rate") + scale_y_continuous(breaks = scales::pretty_breaks(n = 8)) + ylab("Excess Deaths per 100K Individuals") + xlab("Year") 

#NO PRED ARIMA, just empirical data
excess_death_rate_fig <- ggplot(data=ageadj_mortality_rate, aes(x=Year, y=diff_black, color=Gender)) + geom_line(size=1) + geom_point(size = 3) + geom_hline(yintercept=0, linetype="dotted") + year_label + panel_theme  + scale_color_manual(values=c("maroon", "navy")) + sizing_theme  + ggtitle("Excess Age Adjusted Mortality Rate") + scale_y_continuous(breaks = scales::pretty_breaks(n = 8)) + ylab("Excess Deaths per 100K Individuals") + xlab("Year") 

mortality_rate_ratio_fig <- excess_deaths_year %>% ggplot(aes(x=Year, y=ratio_adj_excess_deaths_rate, color=Gender)) + geom_line(size=1.25) + panel_theme + sizing_theme + year_label + ylab("Mortality Rate Ratio (Black / White)") + scale_y_continuous(breaks = scales::pretty_breaks(n=8)) + geom_hline(yintercept=1, linetype="dashed") + scale_color_manual(values=c("maroon", "navy")) + ggtitle("Age Adjusted Mortality Rate Ratio") 


edy <- excess_deaths_year %>% select(Gender, ageadjrate_white, ageadjrate_black, Year) %>% pivot_longer(-c(Gender, Year)) %>% mutate(Race=gsub(".*_", "", name) %>% str_to_title(.)) %>% select(-name)

age_adj_deaths_race_sex_fig <-  edy %>% ggplot(aes(x=Year, y=value, color=Race, shape=Gender, linetype=Gender)) + geom_line(size=0.5) + geom_point(size=2) + panel_theme + sizing_theme + year_label + scale_y_continuous(breaks=scales::pretty_breaks(n=8)) + ylab("Mortality Rate per 100K Individuals") + ggtitle("Age Adjusted Mortality Rate") + scale_color_manual(values=cbb[1:2])


edb <- excess_deaths %>% group_by(Year, Gender) %>% summarize(excess_deaths_black=sum(excess_deaths_black))

mortality_excess_numbers_fig <-  edb %>% ggplot(aes(x=Year, y=excess_deaths_black, color=Gender)) + geom_point() + geom_line() + panel_theme + sizing_theme + year_label + scale_color_manual(name="", values=c("maroon", "navy")) + scale_y_continuous(limits=c(0,max(edb$excess_deaths_black, na.rm=TRUE)*1.15), breaks=seq(0,max(edb$excess_deaths_black, na.rm=TRUE)*1.15, 1000)) + ggtitle("Estimated Excess Deaths Among the Black Population") + ylab("Number of Deaths")

cumedb <- excess_deaths %>% group_by(Year, Gender) %>% summarize(excess_deaths_black=sum(excess_deaths_black)) %>% ungroup() %>% group_by(Gender) %>% arrange(Year) %>% summarize(excess_deaths_black = cumsum(excess_deaths_black), Year=Year) %>% ungroup() %>% mutate(edb=excess_deaths_black/1000)

cumulative_excess_deaths_fig <- cumedb %>% ggplot(aes(x=Year, y=edb, color=Gender)) + geom_point() + geom_line() + year_label + panel_theme + sizing_theme + ggtitle("Cumulative Number of Excess Deaths") + ylab("Excess Number of Deaths (Thousands)") + scale_y_continuous(breaks = scales::pretty_breaks(n = 9)) + scale_color_manual(name="", values=c("maroon", "navy"))

mexcess <- excess_deaths %>% group_by(Year) %>% summarize(excess_deaths_black=sum(excess_deaths_black)) 

mortality_excess_net_fig <- mexcess %>% ggplot(aes(x=Year, y=excess_deaths_black)) + geom_point() + geom_line() + panel_theme + sizing_theme + year_label + scale_color_manual(name="", values=c("black")) + scale_y_continuous(limits=c(0,max(mexcess$excess_deaths_black, na.rm=TRUE)*1.15), breaks=seq(0, max(mexcess$excess_deaths_black, na.rm=TRUE)*1.15, 1000)) + ggtitle("Estimated Excess Deaths Among the Black Population\nMales and Females Combined") + ylab("Number of Deaths")

cum_excess <- excess_deaths %>% group_by(Year, Gender) %>% summarize(excess_deaths_black=sum(excess_deaths_black)) %>% ungroup() %>% group_by(Gender) %>% arrange(Year) %>% summarize(excess_deaths_black = cumsum(excess_deaths_black), Year=Year) %>% ungroup() %>% group_by(Year) %>% mutate(edb=sum(excess_deaths_black)) %>% mutate(edb=edb/1000) 

cumulative_excess_net_fig <- cum_excess %>% ggplot(aes(x=Year, y=edb)) + geom_point() + geom_line() + year_label + panel_theme + sizing_theme + ggtitle("Cumulative Excess Deaths") + ylab("Excess Number of Deaths (Thousands)") + scale_y_continuous(breaks=scales::pretty_breaks(n = 8)) + scale_color_manual(name="", values=c("black"))


agevector <- excess_deaths %>% select(agegroup, age) %>% distinct()

#agevector <- agevector %>% mutate(age = str_extract(agegroup, "\\d+") %>% as.numeric())

gender_deaths <- excess_deaths %>% group_by(Gender) %>% summarize(totaldeath = sum(excess_deaths_black)) %>% ungroup()

edb_gender <- excess_deaths %>% group_by(age,agegroup, Gender) %>% summarize(excess_deaths_black = sum(excess_deaths_black)) %>% ungroup() 

excess_deaths_age_gender_fig <- edb_gender %>% ggplot(aes(x=age, y=excess_deaths_black, color=Gender, fill=Gender)) + geom_col(position="dodge", alpha=0.5) + panel_theme + sizing_theme + scale_color_manual(name="", values=c("maroon", "navy")) + scale_fill_manual(name="", values=c("maroon", "navy")) + ylab("Excess Deaths") + xlab("") + scale_y_continuous(limits=c(floor(min(edb_gender$excess_deaths_black, na.rm=TRUE)/1000)*1000,max(edb_gender$excess_deaths_black, na.rm=TRUE)*1.15), breaks=seq(floor(min(edb_gender$excess_deaths_black, na.rm=TRUE)/1000)*1000, max(edb_gender$excess_deaths_black, na.rm=TRUE)*1.15, 5000)) + scale_x_continuous(breaks=agevector$age, labels=agevector$agegroup) + theme(axis.text.x = element_text(angle=45, hjust=1))


edb_percent <- excess_deaths %>% group_by(age,agegroup, Gender) %>% summarize(excess_deaths_black = sum(excess_deaths_black)) %>% ungroup() %>% left_join(gender_deaths, by=c("Gender"="Gender")) %>% mutate(percent=(excess_deaths_black / totaldeath)*100)

percent_total_excess_deaths_age_gender_fig <- edb_percent %>% ggplot(aes(x=age, y=percent, color=Gender, fill=Gender)) + geom_col(position="dodge", alpha=0.5) + panel_theme + sizing_theme + scale_color_manual(name="", values=c("maroon", "navy")) + scale_fill_manual(name="", values=c("maroon", "navy")) + ylab("Percent of Total Excess Deaths") + xlab("") + scale_y_continuous(limits=c(floor(min(edb_percent$percent)/10)*10, max(edb_percent$percent)*1.1), breaks=seq(floor(min(edb_percent$percent)/10)*10, max(edb_percent$percent)*1.1, 5)) + scale_x_continuous(breaks=agevector$age, labels=agevector$agegroup) + theme(axis.text.x = element_text(angle=45, hjust=1))


save_plot(excess_deaths_age_gender_fig, plotdir)
save_plot(percent_total_excess_deaths_age_gender_fig, plotdir)
save_plot(cumulative_excess_net_fig, plotdir)
save_plot(mortality_excess_net_fig, plotdir)
save_plot(excess_death_rate_fig, plotdir)
save_plot(mortality_rate_ratio_fig, plotdir)
save_plot(age_adj_deaths_race_sex_fig, plotdir)
save_plot(mortality_excess_numbers_fig, plotdir)
save_plot(cumulative_excess_deaths_fig, plotdir)
save_plot(indiv_ageadj_rate_fig, plotdir)


save_rds(excess_deaths_age_gender_fig, outputdir)
save_rds(percent_total_excess_deaths_age_gender_fig, outputdir)
save_rds(cumulative_excess_net_fig, outputdir)
save_rds(mortality_excess_net_fig, outputdir)
save_rds(excess_death_rate_fig, outputdir)
save_rds(mortality_rate_ratio_fig, outputdir)
save_rds(age_adj_deaths_race_sex_fig, outputdir)
save_rds(mortality_excess_numbers_fig, outputdir)
save_rds(cumulative_excess_deaths_fig, outputdir)
save_rds(indiv_ageadj_rate_fig, outputdir)













































