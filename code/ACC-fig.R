#Create Figure for ACC Abstract

#For HTN specifically 

library(tidyverse)

args = commandArgs(trailingOnly=TRUE)

source("preprocess-function.R")

project <- args[1]

  basedir <- file.path("../results", project)

  #panel a
  aadr_indiv <- file.path(basedir, "excess-mortality-by-year", "age_adj_deaths_race_sex_fig.rds") %>% readRDS() 

  #panel b
  mrr <- file.path(basedir, "excess-mortality-by-year", "mortality_rate_ratio_fig.rds") %>% readRDS()

  #panel c (no longer uses ARIMA to smooth predictions)
  excess_aadr <- file.path(basedir, "excess-mortality-by-year", "excess_death_rate_fig.rds") %>% readRDS()

  #panel d
  pll <- file.path(basedir, "life-years-lost-by-year", "excess_pll_fig.rds") %>% readRDS()

if (project == "HTN"){
  aadr_indiv <- aadr_indiv + scale_y_continuous(breaks = scales::pretty_breaks(n = 10), limits = c(0, 95))

  pll <- pll + scale_y_continuous(breaks = scales::pretty_breaks(n = 10), limits = c(0, 170000))
  
}


cowplot::plot_grid(aadr_indiv, mrr, excess_aadr, pll, nrow = 2, ncol = 2, labels = c("A", "B", "C", "D"), label_size = 25) %>% ggsave(filename = file.path("../../cdc-wonder-output/figs", project, "ACC.pdf"), plot = ., units = "in", device = cairo_pdf, width = 20, height = 16)
