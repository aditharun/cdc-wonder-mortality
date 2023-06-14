library(optparse)

option_list <- list(
  make_option(c("-p", "--project"), type = "character", default = "HTN", 
              help = "File path")
)


opt_parser = OptionParser(option_list=option_list)
opt = parse_args(opt_parser)

project <- opt$project

system(paste0("Rscript excess-mortality-by-age.R ", project))
system(paste0("Rscript life-years-lost-by-age.R ", project))
system(paste0("Rscript excess-mortality-by-year.R ", project))
system(paste0("Rscript life-years-lost-by-year.R ", project))