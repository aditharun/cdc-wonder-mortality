library(tidyverse)

system("Rscript race-correction.R")
system("Rscript process-cdc-data.R -a args-file.R -p IHD")
system("Rscript process-cdc-data.R -a args-file.R -p HTN")
system("Rscript run-figures.R -p HTN -y 2000")
system("Rscript run-figures.R -p IHD -y 1999")

system("Rscript race-correction-mcd.R")
system("Rscript process-cdc-data.R -a args-file-mcd.R -p MCD_HTN")
system("Rscript run-figures.R -p MCD_HTN -y 2000")