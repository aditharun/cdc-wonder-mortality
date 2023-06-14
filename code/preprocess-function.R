library(tidyverse)


create_output_dir <- function(outputdir){

	if (!dir.exists(outputdir)){

		dir.create(outputdir, recursive=TRUE)

	}
	
}

#turns tsv file output into dataframe we can analyze
preprocess_cdc_wonder <- function(filepath){

	data <- read_tsv(filepath)

	data <- data %>% 
		mutate(gender_lbl = Gender, gender = case_when(Gender == "Female" ~ 1, Gender == "Male" ~ 3)) %>% 
		mutate(Race = case_when(Race == "Black" ~ 1, Race == "White" ~ 3), race_lbl = case_when(Race == 1 ~ "Black", Race == 3 ~ "White"))

	return(data)

}

#turn txt file output into dataframe we can analyze
preprocess_cdc_wonder_file <- function(filepath){
	filedata <- readLines(filepath)

	metadata.start.line <- min(which(filedata=='\"---\"'))

	if (!identical(metadata.start.line, Inf)){

		filedata <- filedata[1:(metadata.start.line-1)]

	}

	txtdata <- str_split(filedata, "\t")

	data <- do.call(rbind, txtdata[-1]) %>% as_tibble() 

	colnames(data) <- txtdata[[1]]


	#character columns with value X have structure "\"X\"" and we need to get rid of the "\" \"" encloding the value of interest
	for (i in seq_along(data)) {
	  if (is.character(data[[i]])) {

	    data[[i]] <- gsub("^\\\"|\\\"$", "", data[[i]])

	  }
	}

	colnames(data) <- gsub("^\\\"|\\\"$", "", colnames(data))

	#do data cleaning
	data <- data %>% 
		mutate(gender_lbl = Gender, gender = case_when(Gender == "Female" ~ 1, Gender == "Male" ~ 3)) %>% 
		mutate(Race = case_when(Race == "Black or African American" ~ 1, Race == "White" ~ 3), race_lbl = case_when(Race == 1 ~ "Black", Race == 3 ~ "White"))

	return(data)
}

#rename columns in dataframe
rename_columns <- function(df, vector.of.cols, newnames){

	indices <- unlist(lapply(vector.of.cols, function(x) which(colnames(df)==x)))

	colnames(df)[indices] <- newnames

	df

}


#ARIMA for excess mortality age adj rate
gender_arima_age_adjusted_death <- function(data, excess_deaths_year, GENDER, year_knots, zcrit){

	year.excess.deaths <- excess_deaths_year %>% filter(Gender==GENDER) %>% filter(Year!=2020) 

	year_spline <- ns(x = year.excess.deaths$Year, knots = year_knots)


	model <- arima(year.excess.deaths$diff_black, order = c(1, 0, 0), xreg = year_spline)

	#in-sample estimates are the original - residuals
	year.excess.deaths$pred_diff_black <- year.excess.deaths$diff_black - residuals(model)

	year.excess.deaths <- year.excess.deaths %>% select(pred_diff_black, diff_black, Year, Gender, gender)

	#Excess age-adjusted rate change from 2019-2020

	covid.excess.age.adj <- data %>% filter(Gender==GENDER) %>% filter(Year >=2019) %>% group_by(Year) %>% summarize(diff=`Age Adjusted Rate`[Race==1] - `Age Adjusted Rate`[Race==3], se=sqrt(ageadjustedrate_se[Race==1]^2 + ageadjustedrate_se[Race==3]^2))

	did <- covid.excess.age.adj$diff[2] - covid.excess.age.adj$diff[1]; se_did <- sqrt(sum(covid.excess.age.adj$se^2))

	lb <- did - zcrit*se_did; ub <- did + zcrit*se_did; pval <- 2*(1-pnorm(abs(did/se_did)))

	if(pval < 0.001){
		pval <- "< 0.001"
	} else{
		pval <- as.character(format(pval, digits=3, scientific=TRUE))
	}

	covid.results <- paste0(did, " (", round(lb, 2), ",", round(ub, 2), "); p-value: ", pval)

	return(list(gender_adj_rate_arima=year.excess.deaths, covid.results=covid.results))

}


#ARIMA for Life Years Lost
gender_arima_years_lost <- function(data, excess, GENDER, year_knots, zcrit){

	year.excess.deaths <- excess %>% filter(Gender==GENDER) %>% filter(Year!=2020) 

	year_spline <- ns(x = year.excess.deaths$Year, knots = year_knots)

	model <- arima(year.excess.deaths$excess_yrs_lost, order = c(1, 0, 0), xreg = year_spline)

	#in-sample estimates are the original - residuals

	year.excess.deaths$pred_excess_yrs_lost <- year.excess.deaths$excess_yrs_lost - residuals(model)

	year.excess.deaths <- year.excess.deaths %>% select(pred_excess_yrs_lost, excess_yrs_lost, Year, Gender)

	#Excess age-adjusted rate change from 2019-2020

	covid.excess <- data %>% filter(Gender==GENDER) %>% filter(Year >=2019) %>% group_by(Year) %>% summarize(diff=yrs_lost[Race==1] - yrs_lost[Race==3], se=sqrt(yrs_lost_se[Race==1]^2 + yrs_lost_se[Race==3]^2))

	did <- covid.excess$diff[2] - covid.excess$diff[1]; se_did <- sqrt(sum(covid.excess$se^2))

	lb <- did - zcrit*se_did; ub <- did + zcrit*se_did; pval <- 2*(1-pnorm(abs(did/se_did)))

	if(pval < 0.001){
		pval <- "< 0.001"
	} else{
		pval <- as.character(format(pval, digits=3, scientific=TRUE))
	}

	covid.results <- paste0(did, " (", round(lb, 2), ",", round(ub, 2), "); p-value: ", pval)

	return(list(arima=year.excess.deaths, covid.results=covid.results))

}



#save plot
save_plot <- function(plt, folder, height.in=(10/1.5), width.in=10){

	if (!dir.exists(folder)){
		dir.create(folder, recursive=TRUE)
	}

	file <- paste0(deparse(substitute(plt)), ".pdf")

	ggsave(filename=file.path(folder, file), plot=plt, device=cairo_pdf, height=height.in, width=width.in, units="in")

}

















