library(tidyverse)

#system("Rscript standard-pop-2000-parse.R")

#system("Rscript race-correction-generic.R -p cvd -t folder")
#system("Rscript process-cdc-data-generic.R -a args-file-cvd.R -p cvd")
system("Rscript run-figures-generic.R -p cvd -y 2000")

#system("Rscript race-correction-generic.R -p hfcvd -t file")
#system("Rscript process-cdc-data-generic.R -a args-file-hfcvd.R -p hfcvd")
system("Rscript run-figures-generic.R -p hfcvd -y 2000")

#system("Rscript race-correction-generic.R -p cvcvd -t file")
#system("Rscript process-cdc-data-generic.R -a args-file-cvcvd.R -p cvcvd")
system("Rscript run-figures-generic.R -p cvcvd -y 2000")

#system("Rscript race-correction-generic.R -p ihdcvd -t file")
#system("Rscript process-cdc-data-generic.R -a args-file-ihdcvd.R -p ihdcvd")
system("Rscript run-figures-generic.R -p ihdcvd -y 2000")

#system("Rscript race-correction-generic.R -p htncvd -t file")
#system("Rscript process-cdc-data-generic.R -a args-file-htncvd.R -p htncvd")
system("Rscript run-figures-generic.R -p htncvd -y 2000")


#system("Rscript assemble-tables-CVD.R")














#system("Rscript race-correction-generic.R -p diabetes")
#system("Rscript process-cdc-data-generic.R -a args-file-diabetes.R -p diabetes")
#system("Rscript run-figures-generic.R -p diabetes -y 1999")

#system("Rscript race-correction-generic.R -p cancer")
#system("Rscript process-cdc-data-generic.R -a args-file-cancer.R -p cancer")
#system("Rscript run-figures-generic.R -p cancer -y 1999")