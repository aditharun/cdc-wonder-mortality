library(tidyverse)
library(haven)
library(splines)

source("preprocess-function.R")


project <- "hypertension"

#input file
inputfile <- file.path(file.path("../data", project), "export_age_adjusted_deaths_race_gender_year_se.txt")
inputfile2 <- file.path(file.path("../data", project), "export_deaths_race_gender_age_year.txt")

#output directory and file
outputdir <- file.path("../results", project)
outputfile <- file.path(outputdir, "excess-mortality-by-year.rds")

create_output_dir(outputdir)

#plot output dir
plotdir <- file.path("../plots", project, "excess-mortality-by-year")

#palette of colors
cbb <- c("#E69F00", "#56B4E9", "#009E73", "#0072B2", "#D55E00", "#CC79A7")



######## CODE ######
data <- preprocess_cdc_wonder_file(inputfile)

cols.to.rename <- c("Crude Rate Lower 95% Confidence Interval", "Crude Rate Upper 95% Confidence Interval", "Crude Rate Standard Error", "Age Adjusted Rate Lower 95% Confidence Interval", "Age Adjusted Rate Upper 95% Confidence Interval", "Age Adjusted Rate Standard Error")

new.col.names <- c("cruderate_ci_lb", "cruderate_ci_ub", "cruderate_se", "ageadjustedrate_ci_lb", "ageadjustedrate_ci_ub", "ageadjustedrate_se")

data <- rename_columns(data, cols.to.rename, new.col.names)

data <- data %>% filter(Notes == "") %>% select(-Notes)

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

data2 <- preprocess_cdc_wonder_file(inputfile2)
colnames(data2)[which(colnames(data2)=="Ten-Year Age Groups")] <- "agegroup"
data2 <- data2 %>% filter(agegroup!="Not Stated") %>% filter(Population!="Not Applicable") %>% filter(Notes == "") %>% select(-Notes) %>% select(-ends_with("code")) %>% type.convert(as.is=TRUE)
data2 <- data2 %>% mutate(age=as.integer(factor(agegroup))-1)

data2 <- data2 %>% filter(is.finite(as.numeric(Population))) %>% filter(is.finite(as.numeric(Deaths))) %>% filter(is.finite(as.numeric(`Crude Rate`))) %>% type.convert(as.is=TRUE)

excess_deaths <- data2 %>% group_by(Gender, agegroup, age, Year) %>% summarize(expected_deaths_black = Population[Race==1]*(Deaths[Race==3]/Population[Race==3]), age_specific_rate_ratio=`Crude Rate`[Race==1]/`Crude Rate`[Race==3], observed_rate_black=(Deaths[Race==1]/Population[Race==1])*100000, observed_rate_white=(Deaths[Race==3]/Population[Race==3])*100000, expected_rate_black=(expected_deaths_black/Population[Race==1])*100000, excess_rate_black = observed_rate_black - expected_rate_black, excess_deaths_black = (excess_rate_black * Population[Race==1])/100000, observed_vs_expected_rate_ratio = observed_rate_black / expected_rate_black) %>% ungroup()

excess_deaths %>% group_by(Gender) %>% summarize(excess_deaths_black=sum(excess_deaths_black))

ageadj_mortality_rate <- left_join(excess_deaths_year, women.excess.age.adj.deaths$gender_adj_rate_arima %>% select(-diff_black), by=c("gender"="gender", "Year"="Year", "Gender"="Gender")) %>% left_join(., men.excess.age.adj.deaths$gender_adj_rate_arima %>% select(-diff_black), by=c("gender"="gender", "Year"="Year", "Gender"="Gender")) %>% mutate(pred_diff_black = ifelse(is.na(pred_diff_black.x), pred_diff_black.y, pred_diff_black.x)) %>% select(-c(pred_diff_black.y, pred_diff_black.x))

saveRDS(object=list(excess_deaths_year = excess_deaths_year, excess_deaths = excess_deaths, ageadj_mortality_rate=ageadj_mortality_rate), file = outputfile)






#GRAPHICAL COMPONENT

sizing_theme <- theme(axis.text = element_text(size=12), axis.title=element_text(size=16), legend.text=element_text(size=12), legend.title=element_text(size=16), plot.title=element_text(size=18, hjust=0.5)) 

year_label <- scale_x_continuous(breaks=seq(1999, 2020, 1), labels= function(x) ifelse(x %% 2 == 1, x, ""))

panel_theme <- theme_bw() + theme(panel.grid.major.x = element_blank(), panel.grid.minor=element_blank())

excess_death_rate_fig <- ggplot() + geom_line(data=ageadj_mortality_rate, aes(x=Year, y=pred_diff_black, color=Gender), size=1) + geom_line(data=ageadj_mortality_rate %>% filter(Year>=2019) %>% mutate(diff_black=ifelse(Year==2019, pred_diff_black, diff_black)), aes(x=Year, y=diff_black, color=Gender), linetype="dashed", size=1) + geom_hline(yintercept=0, linetype="dotted")  + scale_y_continuous(limits = c(-100, 450), breaks=seq(-100, 450, 50)) + ylab("Excess Deaths per 100000 Individuals") + xlab("Year") + year_label + panel_theme + geom_point(data = ageadj_mortality_rate %>% filter(Year %in% c(2015) & Gender == "Female" | Year %in% c(2007, 2011) & Gender == "Male"), aes(x=Year, y=pred_diff_black, color=Gender, shape=Gender), size=3.75) + scale_color_manual(values=c("maroon", "navy")) + sizing_theme  + ggtitle("Estimated Excess Adjusted Mortality Rate Among the Black Population") 


mortality_rate_ratio_fig <- excess_deaths_year %>% ggplot(aes(x=Year, y=ratio_adj_excess_deaths_rate, color=Gender)) + geom_line(size=1.25) + ylab("Mortality Rate Ratio (Black / White)") + scale_y_continuous(limits = c(0.85, max(excess_deaths_year$ratio_adj_excess_deaths_rate + 1)), breaks=seq(0.85, max(excess_deaths_year$ratio_adj_excess_deaths_rate + 1), 0.1)) + geom_hline(yintercept=1, linetype="dashed") + sizing_theme + scale_color_manual(values=c("maroon", "navy")) + ggtitle("Black-White Age-Adjusted Mortality Rate Ratio") + year_label + panel_theme


age_adj_deaths_race_sex_fig <- excess_deaths_year %>% select(Gender, ageadjrate_white, ageadjrate_black, Year) %>% pivot_longer(-c(Gender, Year)) %>% mutate(Race=gsub(".*_", "", name) %>% str_to_title(.)) %>% select(-name) %>% ggplot(aes(x=Year, y=value, color=Race, shape=Gender, linetype=Gender)) + geom_line(size=0.5) + geom_point(size=2) + panel_theme + sizing_theme + year_label + scale_y_continuous(limits=c(0, 1600), breaks=seq(0, 1600, 200)) + ylab("Mortality Rate per 100K Individuals") + ggtitle("Estimated Age-Adjusted Mortality Rate") + scale_color_manual(values=cbb[1:2])


mortality_excess_numbers_fig <- excess_deaths %>% group_by(Year, Gender) %>% summarize(excess_deaths_black=sum(excess_deaths_black)) %>% ggplot(aes(x=Year, y=excess_deaths_black, color=Gender)) + geom_point() + geom_line() + panel_theme + sizing_theme + year_label + scale_color_manual(name="", values=c("maroon", "navy")) + scale_y_continuous(limits=c(0,100000), breaks=seq(0,100000, 10000)) + ggtitle("Estimated Excess Deaths Among the Black Population") + ylab("Number of Deaths")

cumulative_excess_deaths_fig <- excess_deaths %>% group_by(Year, Gender) %>% summarize(excess_deaths_black=sum(excess_deaths_black)) %>% ungroup() %>% group_by(Gender) %>% arrange(Year) %>% summarize(excess_deaths_black = cumsum(excess_deaths_black), Year=Year) %>% ungroup() %>% mutate(edb=excess_deaths_black/1000) %>% ggplot(aes(x=Year, y=edb, color=Gender)) + geom_point() + geom_line() + year_label + panel_theme + sizing_theme + ggtitle("Estimated Cumulative Number of Excess Deaths Among the Black Population") + ylab("Excess Number of Deaths (Thousands)") + scale_y_continuous(limits=c(0,1200), breaks=seq(0,1200,100)) + scale_color_manual(name="", values=c("maroon", "navy"))

mortality_excess_net_fig <- excess_deaths %>% group_by(Year) %>% summarize(excess_deaths_black=sum(excess_deaths_black)) %>% ggplot(aes(x=Year, y=excess_deaths_black)) + geom_point() + geom_line() + panel_theme + sizing_theme + year_label + scale_color_manual(name="", values=c("black")) + scale_y_continuous(limits=c(0,150000), breaks=seq(0,150000,10000)) + ggtitle("Estimated Excess Deaths Among the Black Population\nMales and Females Combined") + ylab("Number of Deaths")

cumulative_excess_net_fig <- excess_deaths %>% group_by(Year, Gender) %>% summarize(excess_deaths_black=sum(excess_deaths_black)) %>% ungroup() %>% group_by(Gender) %>% arrange(Year) %>% summarize(excess_deaths_black = cumsum(excess_deaths_black), Year=Year) %>% ungroup() %>% group_by(Year) %>% mutate(edb=sum(excess_deaths_black)) %>% mutate(edb=edb/1000) %>% ggplot(aes(x=Year, y=edb)) + geom_point() + geom_line() + year_label + panel_theme + sizing_theme + ggtitle("Estimated Cumulative Number of Excess Deaths Among the Black Population\nMales and Females Combined") + ylab("Excess Number of Deaths (Thousands)") + scale_y_continuous(limits=c(0,1800), breaks=seq(0,1800,100)) + scale_color_manual(name="", values=c("black"))


agevector <- excess_deaths %>% select(agegroup, age) %>% distinct()

gender_deaths <- excess_deaths %>% group_by(Gender) %>% summarize(totaldeath = sum(excess_deaths_black)) %>% ungroup()

excess_deaths_age_gender_fig <- excess_deaths %>% group_by(age,agegroup, Gender) %>% summarize(excess_deaths_black = sum(excess_deaths_black)) %>% ungroup() %>% ggplot(aes(x=age, y=excess_deaths_black, color=Gender, fill=Gender)) + geom_col(position="dodge", alpha=0.5) + panel_theme + sizing_theme + scale_color_manual(name="", values=c("maroon", "navy")) + scale_fill_manual(name="", values=c("maroon", "navy")) + ylab("Excess Deaths") + xlab("") + scale_y_continuous(limits=c(-100000, 300000), breaks=seq(-100000, 300000, 50000)) + scale_x_continuous(breaks=agevector$age, labels=agevector$agegroup) + theme(axis.text.x = element_text(angle=45, hjust=1))

percent_total_excess_deaths_age_gender_fig <- excess_deaths %>% group_by(age,agegroup, Gender) %>% summarize(excess_deaths_black = sum(excess_deaths_black)) %>% ungroup() %>% left_join(gender_deaths, by=c("Gender"="Gender")) %>% mutate(percent=(excess_deaths_black / totaldeath)*100) %>% ggplot(aes(x=age, y=percent, color=Gender, fill=Gender)) + geom_col(position="dodge", alpha=0.5) + panel_theme + sizing_theme + scale_color_manual(name="", values=c("maroon", "navy")) + scale_fill_manual(name="", values=c("maroon", "navy")) + ylab("Percent of Total Excess Deaths") + xlab("") + scale_y_continuous(limits=c(-15, 30), breaks=seq(-15, 30, 5)) + scale_x_continuous(breaks=agevector$age, labels=agevector$agegroup) + theme(axis.text.x = element_text(angle=45, hjust=1))


save_plot(excess_deaths_age_gender_fig, plotdir)
save_plot(percent_total_excess_deaths_age_gender_fig, plotdir)
save_plot(cumulative_excess_net_fig, plotdir)
save_plot(mortality_excess_net_fig, plotdir)
save_plot(excess_death_rate_fig, plotdir)
save_plot(mortality_rate_ratio_fig, plotdir)
save_plot(age_adj_deaths_race_sex_fig, plotdir)
save_plot(mortality_excess_numbers_fig, plotdir)
save_plot(cumulative_excess_deaths_fig, plotdir)
















































