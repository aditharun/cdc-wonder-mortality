library(tidyverse)

system("Rscript race-correction.R")
system("Rscript process-cdc-data.R -a args-file.R -p IHD")
system("Rscript process-cdc-data.R -a args-file.R -p HTN")