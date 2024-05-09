library(tidyverse)
library(data.table)

library(optparse)

option_list <- list(
  make_option(c("-a", "--argfile"), type = "character", default = "", 
              help = "Argument file"),
  make_option(c("-p", "--project"), type = "character", default = "", 
              help = "Project Name")

)


opt_parser = OptionParser(option_list=option_list)
opt = parse_args(opt_parser)

source(opt$argfile)

pop <- popfilepath %>% read_csv()
pop <- pop %>% filter(Population != "Not Applicable") %>% type.convert(as.is = TRUE)

if (!dir.exists(outdir)){
	dir.create(outdir)
}

workdir <- file.path(outdir, project)

if (!dir.exists(workdir)){
	dir.create(workdir)
}

start_years <- c(2020, 2013, 2006, first_year)
end_years <- c(2022, 2019, 2012, 2005)

start_year <- first_year
end_year <- 2022

race1 <- "White"
race2 <- "Black or African American"

ethnicity1 <- "Not Hispanic or Latino"
ethnicity2 <- "Not Hispanic or Latino"

include_lowest <- TRUE
age_breaks <- c(0, 1, 4, 9, 14, 19, 24, 29, 34, 39, 44, 49, 54, 59, 64, 69, 74, 79, 84, Inf)
age_labels <- c("<1", "1-4", "5-9", "10-14", "15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49", "50-54", "55-59", "60-64", "65-69", "70-74", "75-79", "80-84", "85+")

if (age_first > 1) {
  age_first_i <- age_first - 1
  include_lowest <- FALSE
}

start_index <- which(age_breaks == age_first_i)

trimmed_age_breaks <- age_breaks[start_index:length(age_breaks)]
trimmed_age_labels <- age_labels[start_index:length(age_labels)]


pop <- pop %>% filter(`Five-Year Age Groups` != "Not Stated")

df <- df[(df$Race == race1 & df$`Hispanic Origin` == ethnicity1) | (df$Race == race2 & df$`Hispanic Origin` == ethnicity2), ]

df <- df %>% filter(!`Five-Year Age Groups` %in% c("85-59 years", "90-94 years", "95-99 years", "100+ years"))

#2000 standard
aa_df <- read.table("../raw-data/age_2000_standard_population_final.tsv", header = TRUE, sep = "\t") %>% as_tibble()

aa_df$age_groups <- cut(aa_df$Age,
                        breaks = trimmed_age_breaks,
                        labels = trimmed_age_labels,
                        include.lowest = include_lowest)

aa_df <- aa_df[complete.cases(aa_df$age_groups), ]

aa_df$Age_groups <- as.character(aa_df$age_groups)
aa_df$Age_groups <- ifelse(aa_df$Age == "1", "1-4", aa_df$Age_groups)

aa_df <- aggregate(Population ~ Age_groups, aa_df, sum)
aa_df <- aa_df %>% filter(Age_groups != "85+")
for (i in 1:nrow(aa_df)) {aa_df$AA_proportion[i] <- aa_df$Population[i] / sum(aa_df$Population) }

for (i in seq_along(start_years)) {
  start_year_i <- start_years[i]
  end_year_i <- end_years[i]
  filtered_data <- df[df$Year >= start_year_i & df$Year <= end_year_i, ]

  filtered_data <- filtered_data %>% group_by(Race, `Hispanic Origin`, `Five-Year Age Groups`, Gender) %>% summarize(Deaths = sum(Deaths)) %>% ungroup()

  pop_df <- pop[pop$Year >= start_year_i & pop$Year <= end_year_i,]
  pop_df <- pop_df %>% group_by(Race, `Hispanic Origin`, `Five-Year Age Groups`, Gender) %>% summarize(Population = sum(Population)) %>% ungroup()
  
  dat <- filtered_data %>% left_join(pop_df, by=c("Race"="Race", "Gender"="Gender", "Five-Year Age Groups"="Five-Year Age Groups", "Hispanic Origin" = "Hispanic Origin")) %>% mutate(`Crude Rate` = (Deaths/Population)*100000)

  dat <- dat %>% mutate(Gender = ifelse(Gender == "Female", "F", "M"))

  dat <- dat %>% filter(`Five-Year Age Groups` %in% paste0(trimmed_age_labels, " years")) 
  
  file_name <- paste0(workdir, "/deaths_crude_race_gender_age_", start_year_i, "-", end_year_i, ".tsv")
  write.table(dat, file = file_name, sep = "\t", quote = FALSE, row.names = FALSE)
}

 

for (i in seq_along(start_years)) {
  start_year_i <- start_years[i]
  end_year_i <- end_years[i]
  
  filtered_data <- df[df$Year >= start_year_i & df$Year <= end_year_i, ]
  
filtered_data <- filtered_data %>% group_by(Race, `Hispanic Origin`, `Five-Year Age Groups`, Gender, Year) %>% summarize(Deaths = sum(Deaths)) %>% ungroup()

  pop_df <- pop[pop$Year >= start_year_i & pop$Year <= end_year_i,]
  pop_df <- pop_df %>% group_by(Race, `Hispanic Origin`, `Five-Year Age Groups`, Gender, Year) %>% summarize(Population = sum(Population)) %>% ungroup()
  
  dat <- filtered_data %>% left_join(pop_df, by=c("Race"="Race", "Gender"="Gender", "Five-Year Age Groups"="Five-Year Age Groups", "Hispanic Origin" = "Hispanic Origin", "Year"="Year")) %>% mutate(`Crude Rate` = (Deaths/Population)*100000)
  
  dat <- dat %>% mutate(Gender = ifelse(Gender == "Female", "F", "M"))

  dat <- dat %>% filter(`Five-Year Age Groups` %in% paste0(trimmed_age_labels, " years")) 
  
  file_name <- paste0(workdir, "/deaths_crude_race_gender_age_year_", start_year_i, "-", end_year_i, ".tsv")
  write.table(dat, file = file_name, sep = "\t", quote = FALSE, row.names = FALSE)
}




filtered_data <- df[df$Year >= start_year & df$Year <= end_year, ]

filtered_data <- filtered_data %>% group_by(Race, `Hispanic Origin`, `Five-Year Age Groups`, Gender, Year) %>% summarize(Deaths = sum(Deaths)) %>% ungroup()

pop_df <- pop[pop$Year >= start_year & pop$Year <= end_year,]
  pop_df <- pop_df %>% group_by(Race, `Hispanic Origin`, `Five-Year Age Groups`, Gender, Year) %>% summarize(Population = sum(Population)) %>% ungroup()

dat <- filtered_data %>% left_join(pop_df, by=c("Race"="Race", "Gender"="Gender", "Five-Year Age Groups"="Five-Year Age Groups", "Hispanic Origin" = "Hispanic Origin", "Year"="Year")) %>% mutate(`Crude Rate` = (Deaths/Population)*100000)
  
  dat <- dat %>% mutate(Gender = ifelse(Gender == "Female", "F", "M"))

  dat <- dat %>% filter(`Five-Year Age Groups` %in% paste0(trimmed_age_labels, " years")) 

file_name <- paste0(workdir, "/export_deaths_crude_race_gender_age_year.tsv")
write.table(dat, file = file_name, sep = "\t", quote = FALSE, row.names = FALSE)

#
filtered_data <- df[df$Year >= start_year & df$Year <= end_year, ]
filtered_data <- filtered_data %>% filter(`Five-Year Age Groups` %in% paste0(trimmed_age_labels, " years")) 
filtered_data <- filtered_data[complete.cases(filtered_data$`Five-Year Age Groups`), ]
filtered_data$Age_groups <- as.character(filtered_data$`Five-Year Age Groups`)
filtered_data$Age_groups <- ifelse(filtered_data$`Five-Year Age Groups` == "1", "1-4", filtered_data$Age_groups)

filtered_data <- filtered_data %>% group_by(Race, `Hispanic Origin`, Age_groups, Gender, Year) %>% summarize(Deaths = sum(Deaths)) %>% ungroup()


pop_df <- pop[pop$Year >= start_year & pop$Year <= end_year,]
pop_df <- pop_df %>% filter(`Five-Year Age Groups` %in% paste0(trimmed_age_labels, " years")) 
pop_df <- pop_df[complete.cases(pop_df$`Five-Year Age Groups`), ]
pop_df$Age_groups <- as.character(pop_df$`Five-Year Age Groups`)
pop_df$Age_groups <- ifelse(pop_df$`Five-Year Age Groups` == "1", "1-4", pop_df$Age_groups)

 pop_df <- pop_df %>% group_by(Race, `Hispanic Origin`, Age_groups, Gender, Year) %>% summarize(Population = sum(Population)) %>% ungroup()

dat <- filtered_data %>% left_join(pop_df, by=c("Race"="Race", "Gender"="Gender", "Age_groups"="Age_groups", "Hispanic Origin" = "Hispanic Origin", "Year"="Year")) %>% mutate(`Crude Rate` = (Deaths/Population)*100000)

file_name <- paste0(workdir, "/export_deaths_race_gender_age_year.tsv")
write.table(dat, file = file_name, sep = "\t", quote = FALSE, row.names = FALSE)


#

filtered_data <- df[df$Year >= start_year & df$Year <= end_year, ]
filtered_data <- filtered_data %>% filter(`Five-Year Age Groups` %in% paste0(trimmed_age_labels, " years")) 
filtered_data <- filtered_data[complete.cases(filtered_data$`Five-Year Age Groups`), ]

filtered_data <- filtered_data %>% group_by(Race, `Hispanic Origin`, `Five-Year Age Groups`, Gender, Year) %>% summarize(Deaths = sum(Deaths)) %>% ungroup()

pop_df <- pop[pop$Year >= start_year & pop$Year <= end_year,]
pop_df <- pop_df %>% filter(`Five-Year Age Groups` %in% paste0(trimmed_age_labels, " years")) 
pop_df <- pop_df[complete.cases(pop_df$`Five-Year Age Groups`), ]

pop_df <- pop_df %>% group_by(Race, `Hispanic Origin`, `Five-Year Age Groups`, Gender, Year) %>% summarize(Population = sum(Population)) %>% ungroup()

dat <- filtered_data %>% left_join(pop_df, by=c("Race"="Race", "Gender"="Gender", "Five-Year Age Groups" = "Five-Year Age Groups", "Hispanic Origin" = "Hispanic Origin", "Year"="Year")) %>% mutate(`Crude Rate` = (Deaths/Population)*100000)


aa_df <- aa_df %>% mutate(`Five-Year Age Groups`= paste0(Age_groups, " years"))

dat <- dat %>% left_join(aa_df %>% select(3,4) , by = c("Five-Year Age Groups"="Five-Year Age Groups")) %>% mutate(AA_rate = `Crude Rate`*AA_proportion)

dat <- dat %>% group_by(Race, Gender, Year, `Hispanic Origin`) %>% summarize(AA_rate = sum(AA_rate), Deaths = sum(Deaths), Population = sum(Population)) %>% mutate(`Crude Rate`= (Deaths / Population) * 100000) %>% mutate(Gender = ifelse(Gender == "Female", "F", "M")) %>% ungroup()

file_name <- paste0(workdir, "/export_age_adjusted_deaths_race_gender_year_se.tsv")
write.table(dat, file = file_name, sep = "\t", quote = FALSE, row.names = FALSE)


#
filtered_data <- df[df$Year >= start_year & df$Year <= end_year, ]
filtered_data <- filtered_data %>% filter(`Five-Year Age Groups` %in% paste0(trimmed_age_labels, " years")) 
filtered_data <- filtered_data[complete.cases(filtered_data$`Five-Year Age Groups`), ]

filtered_data <- filtered_data %>% group_by(Race, `Hispanic Origin`, `Five-Year Age Groups`, Gender, Year) %>% summarize(Deaths = sum(Deaths)) %>% ungroup()

pop_df <- pop[pop$Year >= start_year & pop$Year <= end_year,]
pop_df <- pop_df %>% filter(`Five-Year Age Groups` %in% paste0(trimmed_age_labels, " years")) 
pop_df <- pop_df[complete.cases(pop_df$`Five-Year Age Groups`), ]

pop_df <- pop_df %>% group_by(Race, `Hispanic Origin`, `Five-Year Age Groups`, Gender, Year) %>% summarize(Population = sum(Population)) %>% ungroup()

dat <- filtered_data %>% left_join(pop_df, by=c("Race"="Race", "Gender"="Gender", "Five-Year Age Groups" = "Five-Year Age Groups", "Hispanic Origin" = "Hispanic Origin", "Year"="Year")) %>% mutate(`Crude Rate` = (Deaths/Population)*100000)

dat <- dat %>% left_join(aa_df %>% select(3,4) , by = c("Five-Year Age Groups"="Five-Year Age Groups")) %>% mutate(AA_rate = `Crude Rate`*AA_proportion)

dat <- dat %>% group_by(Race, Gender, Year, `Hispanic Origin`) %>% summarize(AA_rate = sum(AA_rate), Deaths = sum(Deaths), Population = sum(Population)) %>% mutate(`Crude Rate`= (Deaths / Population) * 100000) %>% mutate(Gender = ifelse(Gender == "Female", "F", "M")) %>% ungroup()

file_name <- paste0(workdir, "/export_icd_age_adjusted_deaths_race_gender_year_se.tsv")
write.table(dat, file = file_name, sep = "\t", quote = FALSE, row.names = FALSE)



#

filtered_data <- df[df$Year >= start_year & df$Year <= end_year, ]
filtered_data <- filtered_data %>% filter(`Five-Year Age Groups` %in% paste0(trimmed_age_labels, " years")) 
filtered_data <- filtered_data[complete.cases(filtered_data$`Five-Year Age Groups`), ]

filtered_data <- filtered_data %>% group_by(Race, `Hispanic Origin`, `Five-Year Age Groups`, Gender, Year) %>% summarize(Deaths = sum(Deaths)) %>% ungroup()

pop_df <- pop[pop$Year >= start_year & pop$Year <= end_year,]
pop_df <- pop_df %>% filter(`Five-Year Age Groups` %in% paste0(trimmed_age_labels, " years")) 
pop_df <- pop_df[complete.cases(pop_df$`Five-Year Age Groups`), ]

pop_df <- pop_df %>% group_by(Race, `Hispanic Origin`, `Five-Year Age Groups`, Gender, Year) %>% summarize(Population = sum(Population)) %>% ungroup()

dat <- filtered_data %>% left_join(pop_df, by=c("Race"="Race", "Gender"="Gender", "Five-Year Age Groups" = "Five-Year Age Groups", "Hispanic Origin" = "Hispanic Origin", "Year"="Year")) %>% mutate(`Crude Rate` = (Deaths/Population)*100000)

file_name <- paste0(workdir, "/export_icd_deaths_race_gender_age_year.tsv")
write.table(dat, file = file_name, sep = "\t", quote = FALSE, row.names = FALSE)













#