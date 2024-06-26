library(optparse)

option_list <- list(
  make_option(c("-p", "--project"), type = "character", default = "HTN", 
              help = "File path"), 
   make_option(c("-y", "--year"), type = "character", default = "HTN", 
              help = "Year")
)


opt_parser = OptionParser(option_list=option_list)
opt = parse_args(opt_parser)

project <- opt$project
first_year <- opt$year

system(paste0("Rscript excess-mortality-by-age.R ", project))
system(paste0("Rscript life-years-lost-by-age.R ", project))
system(paste0("Rscript excess-mortality-by-year.R ", project, " ", first_year))
system(paste0("Rscript life-years-lost-by-year.R ", project, " ", first_year))

system(paste0("Rscript icd-ageadjusted.R ", project, " ", first_year))
system(paste0("Rscript icd-life-years-lost.R ", project, " ", first_year))

system(paste0("Rscript figure-stitch.R ", project))

system(paste0("Rscript ACC-fig.R ", project))