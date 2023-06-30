library(tidyverse)
library(cowplot)

source("preprocess-function.R")

args = commandArgs(trailingOnly=TRUE)

project <- args[1]

figdir <- file.path("../../cdc-wonder-output/figs", project)

create_output_dir(figdir)

emby <- file.path("../results", project, "excess-mortality-by-year")

emba <- file.path("../results", project, "excess-mortality-by-age")

lylba <- file.path("../results", project, "life-years-lost-by-age")

lylby <- file.path("../results", project, "life-years-lost-by-year")

icddir <- file.path("../results", project, "icd")

colorpal <- ggsci::pal_jama("default")(7)

colorcorrect <- function(bpal = colorpal[c(4,6)]){
	scale_color_manual(values = bpal[1:2])
}


#All age adjusted derivatives (raw, -, /)
fig1 <- plot_grid(readRDS(file.path(emby, "age_adj_deaths_race_sex_fig.rds")) + theme(axis.text.x = element_text(angle = 45, hjust = 1)), 

	readRDS(file.path(emby, "excess_death_rate_fig.rds")) + theme(axis.text.x = element_text(angle = 45, hjust = 1)) + colorcorrect(),

	readRDS(file.path(emby, "mortality_rate_ratio_fig.rds")) + theme(axis.text.x = element_text(angle = 45, hjust = 1)) + colorcorrect(),

	nrow = 1, labels = c("A", "B", "C"), label_size = 25)

save_plot_custom(plt = fig1, folder = figdir, width.in = 22, height.in = 8)


fig2 <- plot_grid(readRDS(file.path(lylby, "yrs_lost_race_sex_fig.rds")) + theme(axis.text.x = element_text(angle = 45, hjust = 1)), 


	readRDS(file.path(lylby, "excess_pll_fig.rds")) + theme(axis.text.x = element_text(angle = 45, hjust = 1)) + colorcorrect(), 

	nrow = 1, labels = c("A", "B"), label_size = 25

	)

save_plot_custom(plt = fig2, folder = figdir, width.in = 16, height.in = 8)

fig3 <- plot_grid(readRDS(file.path(emba, "excess_death_rate_age_fig.rds")) + colorcorrect(), 

	readRDS(file.path(lylba, "excess_pll_fig.rds")) + theme(plot.title = element_text(hjust = 0.5)) + colorcorrect(), 

	nrow = 1, labels = c("A", "B"), label_size = 25

	)

save_plot_custom(plt = fig3, folder = figdir, width.in = 14, height.in = 6.5)

fig4 <- plot_grid(readRDS(file.path(emby, "cumulative_excess_deaths_fig.rds")) + theme(axis.text.x = element_text(angle = 45, hjust = 1)) + colorcorrect(),

	readRDS(file.path(lylby, "cumulative_sex_pll_fig.rds")) + theme(axis.text.x = element_text(angle = 45, hjust = 1)) + colorcorrect(),

	nrow = 1, labels = c("A", "B"), label_size = 25


	)

save_plot_custom(plt = fig4, folder = figdir, width.in = 16, height.in = 7)


fig5 <- plot_grid(readRDS(file.path(icddir, "ageadj_icd_fig.rds")), 

	readRDS(file.path(icddir, "yrs_lost_icd_fig.rds")), 

	readRDS(file.path(icddir, "mortality_ratio_icd_fig.rds")), labels = c("A", "B", "C"), label_size = 25, nrow = 3)

save_plot_custom(plt = fig5, folder = figdir, width.in = 14, height.in = 20)







