library(tidyverse)
library(cowplot)


source("preprocess-function.R")

args = commandArgs(trailingOnly=TRUE)

project <- args[1]

plotdir <- file.path("../plots", project, "icd")

outputdir <- file.path("../results", project)

pllfig <- file.path(outputdir, "icd-pll.rds")
ageadjfig <- file.path(outputdir, "icd-ageadj.rds")

panela <- readRDS(pllfig)
panelb <- readRDS(ageadjfig)

icd_fig <- plot_grid(panela, panelb, labels = c("A", "B"), label_size = 25, nrow = 2)

save_plot_custom(plt = icd_fig, folder = plotdir, width.in = 14, height.in = 14)