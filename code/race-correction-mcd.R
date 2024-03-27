library(tidyverse)

set.seed(123)


readTextFile <- function(filepath){
	lines <- readLines(filepath)
	index <- which(lines == '"---"')

	if (length(index) > 0) {
	  lines <- lines[1:(index[1] - 1)]
	}

	list_of_vectors <- lapply(lines, function(x) unlist(strsplit(x, "\t")))
	df <- do.call(rbind, lapply(list_of_vectors, function(x) as.data.frame(t(x))))

	df <- df %>% as_tibble()

	df[,1:11] <- lapply(df[,1:11], function(x) gsub('\\"', "", x))

	colnames(df) <- df[1,]

	df <- df[-1,]

	df <- df %>% select(-Notes)

	df
}


race6 <- readTextFile("../raw-data/2018-2020-race-6.txt")
race4 <- readTextFile("../raw-data/2018-2020-race-4.txt")

#race 6 to race 5

race6 <- race6 %>% mutate(Race = ifelse(`Single Race 6` %in% c("Asian", "Native Hawaiian or Other Pacific Islander"), "Asian or Pacific Islander", `Single Race 6`))

#supressed 1-9, unreliable 10-19

death_converter <- function(race6, race4){
	race6 <- race6 %>% select(`Five-Year Age Groups`, Year, Gender, `Hispanic Origin`, Race, Deaths) 
	race4 <- race4 %>% select(`Five-Year Age Groups`, Year, Gender,  `Hispanic Origin`, Race, Deaths) 


	race6 <- race6 %>% mutate(Deaths = ifelse(Deaths == "Suppressed", sample(c(1:9), size = 1), Deaths)) %>% type.convert(as.is=TRUE)
	race4 <- race4 %>% mutate(Deaths = ifelse(Deaths == "Suppressed", sample(c(1:9), size = 1), Deaths)) %>% type.convert(as.is=TRUE)

	#take out More than one race in race6
	other <- race6 %>% filter(Race == "More than one race")
	race6 <- race6 %>% filter(Race != "More than one race")

	df <- left_join(race6 %>% group_by(`Five-Year Age Groups`, Year, Gender, `Hispanic Origin`, Race) %>% summarize(deaths_6 = sum(Deaths)) %>% ungroup(Race) , 

	race4 %>% group_by(`Five-Year Age Groups`, Year, Gender, `Hispanic Origin`, Race) %>% summarize(deaths_4 = sum(Deaths)) %>% ungroup(Race) )

	df <- df %>% mutate(diff = deaths_6 - deaths_4) %>% mutate(diff = ifelse(diff > 0, 0, diff)) #cases where race6 deaths are greater than race4 would mean that we need to remove counts from race6 which does not make sense

	df <- df %>% mutate(total = sum(diff)) %>% mutate(diff = abs(diff), total = abs(total)) %>% mutate(frac = ifelse(total > 0, diff / total, 0)) %>% select(1,2,3,4,5,10) %>% group_by(`Five-Year Age Groups`, Gender, `Hispanic Origin`, Race) %>% summarize(mean_frac = mean(frac))

	df

}


pop_converter <- function(race6, race4){

	race6 <- race6 %>% select(-Deaths)
	race4 <- race4 %>% select(-Deaths)
	colnames(race4)[colnames(race4) == "Population"] <- "Deaths"
	colnames(race6)[colnames(race6) == "Population"] <- "Deaths"


	race6$Deaths[race6$Deaths == "Not Applicable"] <- 0
	race4$Deaths[race4$Deaths == "Not Applicable"] <- 0


	race6 <- race6 %>% select(`Five-Year Age Groups`, Year, Gender, `Hispanic Origin`, Race, Deaths) 
	race4 <- race4 %>% select(`Five-Year Age Groups`, Year, Gender,  `Hispanic Origin`, Race, Deaths) 


	race6 <- race6 %>% mutate(Deaths = ifelse(Deaths == "Suppressed", sample(c(1:9), size = 1), Deaths)) %>% type.convert(as.is=TRUE)
	race4 <- race4 %>% mutate(Deaths = ifelse(Deaths == "Suppressed", sample(c(1:9), size = 1), Deaths)) %>% type.convert(as.is=TRUE)

	race6 <- race6 %>% mutate(Deaths = ifelse(Deaths == "Unreliable", sample(c(11:19), size = 1), Deaths)) %>% type.convert(as.is=TRUE)
	race4 <- race4 %>% mutate(Deaths = ifelse(Deaths == "Unreliable", sample(c(11:19), size = 1), Deaths)) %>% type.convert(as.is=TRUE)

	#take out More than one race in race6
	other <- race6 %>% filter(Race == "More than one race")
	race6 <- race6 %>% filter(Race != "More than one race")

	df <- left_join(race6 %>% group_by(`Five-Year Age Groups`, Year, Gender, `Hispanic Origin`, Race) %>% summarize(deaths_6 = sum(Deaths)) %>% ungroup(Race) , 

	race4 %>% group_by(`Five-Year Age Groups`, Year, Gender, `Hispanic Origin`, Race) %>% summarize(deaths_4 = sum(Deaths)) %>% ungroup(Race) )

	df <- df %>% mutate(diff = deaths_6 - deaths_4) %>% mutate(diff = ifelse(diff > 0, 0, diff)) #cases where race6 deaths are greater than race4 would mean that we need to remove counts from race6 which does not make sense

	df <- df %>% mutate(total = sum(diff)) %>% mutate(diff = abs(diff), total = abs(total)) %>% mutate(frac = ifelse(total > 0, diff / total, 0)) %>% select(1,2,3,4,5,10) %>% group_by(`Five-Year Age Groups`, Gender, `Hispanic Origin`, Race) %>% summarize(mean_frac = mean(frac))

	df

}


death_conversion_table <- death_converter(race6, race4)
pop_conversion_table <- pop_converter(race6, race4)

### Perform Conversion

#Rates are marked as "not applicable" when the denominator population figure is unavailable, such as "not stated" or unknown age or ethnicity. Deaths of persons with "not stated" or unknown age are not included in the calculation of age-adjusted rates.

newdata <- lapply(list.files("../raw-data/mcd/2021", full.names = TRUE), function(x) readTextFile(x))
newdata <- do.call(rbind, newdata)

newdata <- newdata %>% mutate(Race = ifelse(`Single Race 6` %in% c("Asian", "Native Hawaiian or Other Pacific Islander"), "Asian or Pacific Islander", `Single Race 6`)) 

newdata <- newdata %>% mutate(Deaths = ifelse(Deaths == "Suppressed", sample(c(1:9), size = 1), Deaths)) %>% mutate(Population = ifelse(Population == "Suppressed", sample(c(1:9), size = 1), Population)) %>% mutate(Population = ifelse(Population == "Unreliable", sample(c(11:19), size = 1), Population)) %>% filter(Population != "Not Applicable") %>% type.convert(as.is = TRUE) %>% select(-c(`Single Race 6`, `Single Race 6 Code`)) %>% group_by_at(vars(-Deaths, -Population, -`Crude Rate`)) %>% summarize(Deaths = sum(Deaths), Population = sum(Population), `Crude Rate` = Deaths / Population) %>% ungroup()

other <- newdata %>% filter(Race== "More than one race")


list_fin <- list()

for (x in 1:nrow(other)){

	row <- other[x,]
	deaths <- row$Deaths
	pop <- row$Population

	a <- death_conversion_table %>% ungroup() %>% filter(`Five-Year Age Groups` == row$`Five-Year Age Groups`, Gender == row$Gender, `Hispanic Origin`==row$`Hispanic Origin`) %>% select(Race, mean_frac) %>% mutate(Deaths = round(mean_frac*deaths)) %>% select(-mean_frac)

	b <- pop_conversion_table %>% ungroup() %>% filter(`Five-Year Age Groups` == row$`Five-Year Age Groups`, Gender == row$Gender, `Hispanic Origin`==row$`Hispanic Origin`) %>% select(Race, mean_frac) %>% mutate(Population = round(mean_frac*pop)) %>% select(-mean_frac)


	fin <- cbind(left_join(a,b), row %>% select(-c(Deaths, Race, Population, `Crude Rate`)) %>% dplyr::slice(rep(row_number(), each = nrow(a)))) %>% as_tibble()

	list_fin[[x]] <- fin

}

distr_counts <- do.call(rbind, list_fin) %>% as_tibble()


data <- newdata %>% filter(Race != "More than one race" )

corrected_data <- rbind(distr_counts %>% mutate(`Crude Rate`=0), data) %>% group_by(`Five-Year Age Groups`, Gender, `Hispanic Origin`, `Multiple Cause of death Code`, Race) %>% summarize(Deaths = sum(Deaths), Population = sum(Population))  %>% ungroup() %>% relocate(Deaths, Population)


write_csv(x = corrected_data, file = "../raw-data/mcd/corrected-2021-data-htn.csv")

newdata <- lapply(list.files("../raw-data/mcd/2022", full.names = TRUE), function(x) readTextFile(x))
newdata <- do.call(rbind, newdata)

newdata <- newdata %>% mutate(Race = ifelse(`Single Race 6` %in% c("Asian", "Native Hawaiian or Other Pacific Islander"), "Asian or Pacific Islander", `Single Race 6`)) 

newdata <- newdata %>% mutate(Deaths = ifelse(Deaths == "Suppressed", sample(c(1:9), size = 1), Deaths)) %>% mutate(Population = ifelse(Population == "Suppressed", sample(c(1:9), size = 1), Population)) %>% mutate(Population = ifelse(Population == "Unreliable", sample(c(11:19), size = 1), Population)) %>% filter(Population != "Not Applicable") %>% type.convert(as.is = TRUE) %>% select(-c(`Single Race 6`, `Single Race 6 Code`)) %>% group_by_at(vars(-Deaths, -Population, -`Crude Rate`)) %>% summarize(Deaths = sum(Deaths), Population = sum(Population), `Crude Rate` = Deaths / Population) %>% ungroup()

other <- newdata %>% filter(Race== "More than one race")


list_fin <- list()

for (x in 1:nrow(other)){

	row <- other[x,]
	deaths <- row$Deaths
	pop <- row$Population

	a <- death_conversion_table %>% ungroup() %>% filter(`Five-Year Age Groups` == row$`Five-Year Age Groups`, Gender == row$Gender, `Hispanic Origin`==row$`Hispanic Origin`) %>% select(Race, mean_frac) %>% mutate(Deaths = round(mean_frac*deaths)) %>% select(-mean_frac)

	b <- pop_conversion_table %>% ungroup() %>% filter(`Five-Year Age Groups` == row$`Five-Year Age Groups`, Gender == row$Gender, `Hispanic Origin`==row$`Hispanic Origin`) %>% select(Race, mean_frac) %>% mutate(Population = round(mean_frac*pop)) %>% select(-mean_frac)


	fin <- cbind(left_join(a,b), row %>% select(-c(Deaths, Race, Population, `Crude Rate`)) %>% dplyr::slice(rep(row_number(), each = nrow(a)))) %>% as_tibble()

	list_fin[[x]] <- fin

}

distr_counts <- do.call(rbind, list_fin) %>% as_tibble()


data <- newdata %>% filter(Race != "More than one race" )

corrected_data <- rbind(distr_counts %>% mutate(`Crude Rate`=0), data) %>% group_by(`Five-Year Age Groups`, Gender, `Hispanic Origin`, `Multiple Cause of death Code`, Race) %>% summarize(Deaths = sum(Deaths), Population = sum(Population))  %>% ungroup() %>% relocate(Deaths, Population)


write_csv(x = corrected_data, file = "../raw-data/mcd/corrected-2022-data-htn.csv")


new_2021 <- "../raw-data/mcd/corrected-2021-data-htn.csv" %>% read_csv() %>% mutate(Year = 2021)
new_2022 <- "../raw-data/mcd/corrected-2022-data-htn.csv" %>% read_csv() %>% mutate(Year = 2022)
new <- rbind(new_2021, new_2022) %>% as_tibble()

htn_icdcodes <- c("I10", "I11", "I12", "I13", "I15")

parse_icdcode <- function(icdcodes){
	icd10_codes <- gsub("\\.", "", icdcodes)
	coderegex <- icd10_codes %>% paste0(., collapse = "|")
	return(coderegex)
}

htn_new <- new %>% filter(grepl(parse_icdcode(htn_icdcodes), `Multiple Cause of death Code`))

#STEP1: need to save population data for 2021 and 2022
htn_pop_2021_2022 <- htn_new %>% select(Population, `Five-Year Age Groups`, Gender, `Hispanic Origin`, Race, Year) %>% distinct()

pop_2021_2022 <- htn_pop_2021_2022 %>% distinct() %>% as_tibble()

pop_master_df <- "../raw-data/population-1999-2020.txt" %>% readTextFile(.) %>% select(Race, `Hispanic Origin`, `Five-Year Age Groups`, Year, Gender, Population) %>% distinct()

master_pop <- rbind(pop_2021_2022, pop_master_df) %>% as_tibble()

master_pop %>% write_csv(., file = "../raw-data/mcd/master-population.csv")

#STEP2: edit 2021/2022 to be in the shape of 1999-2020 form which must be in the form of below

htn_new <- htn_new %>% filter(Deaths > 0) %>% select(-Population)


#read in 1999-2020 data 

htn_1999_2020 <- lapply(paste0("../raw-data/mcd/", seq(2000, 2020, 1), "-htn.txt"), function(x) readTextFile(x) %>% select(Race, `Hispanic Origin`, `Five-Year Age Groups`, `Multiple Cause of death Code`, Gender, Deaths) %>% filter(grepl(parse_icdcode(htn_icdcodes), `Multiple Cause of death Code`)) %>% filter(Deaths > 0) %>% mutate(Year = str_sub(basename(x), 1, 4) %>% as.numeric()) )



htn_df <- rbind(do.call(rbind, htn_1999_2020) %>% as_tibble(), htn_new) %>% as_tibble()

htn_df <- htn_df %>% mutate(Deaths = ifelse(Deaths == "Suppressed", sample(seq(1, 9, 1), replace = TRUE, size = 1), Deaths)) 

htn_df <- htn_df %>% type.convert(as.is = TRUE)

colnames(htn_df)[colnames(htn_df) == "Multiple Cause of death Code"] <- "Cause of death Code"

htn_df %>% write_csv(., file = "../raw-data/mcd/htn-deaths.csv")
























#
