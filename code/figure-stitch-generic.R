library(tidyverse)
library(cowplot)

source("preprocess-function.R")

args = commandArgs(trailingOnly=TRUE)

project <- args[1]

first_year <- args[2]

years <- seq(first_year, 2022, 1)

year_label <- scale_x_continuous(breaks=years, labels= function(x) ifelse(x %% 2 == 0, x, ""))


figdir <- file.path("../../cdc-wonder-output/figs", project)

create_output_dir(figdir)

emby <- file.path("../results", project, "excess-mortality-by-year")

emba <- file.path("../results", project, "excess-mortality-by-age")

lylba <- file.path("../results", project, "life-years-lost-by-age")

lylby <- file.path("../results", project, "life-years-lost-by-year")

colorpal <- ggsci::pal_jama("default")(7)

colorcorrect <- function(bpal = colorpal[c(4,6)]){
	scale_color_manual(values = bpal[1:2])
}


#All age adjusted derivatives (raw, -, /)
fig1 <- plot_grid(readRDS(file.path(emby, "age_adj_deaths_race_sex_fig.rds")) + theme(axis.text.x = element_text(angle = 45, hjust = 1)), 

	readRDS(file.path(emby, "excess_death_rate_fig.rds")) + theme(axis.text.x = element_text(angle = 45, hjust = 1)) + colorcorrect(),

	readRDS(file.path(emby, "mortality_rate_ratio_fig.rds")) + theme(axis.text.x = element_text(angle = 45, hjust = 1)) + colorcorrect() + scale_y_continuous(breaks = scales::pretty_breaks(n = 8)),

	readRDS(file.path(lylby, "excess_pll_fig.rds")) + theme(axis.text.x = element_text(angle = 45, hjust = 1)) + colorcorrect(),

	nrow = 2, labels = c("A", "B", "C", "D"), label_size = 25)

save_plot_custom(plt = fig1, folder = figdir, width.in = 14, height.in = 10)


if (project %in% c("cvd", "htncvd", "ihdcvd", "hfcvd", "cvcvd")){

	#fig_jacc <- plot_grid(readRDS(file.path(emby, "mortality_rate_ratio_fig.rds")) + theme(axis.text.x = element_text(angle = 45, hjust = 1)) + colorcorrect() + year_label,

		#readRDS(file.path(emby, "excess_death_rate_fig.rds")) + theme(axis.text.x = element_text(angle = 45, hjust = 1)) + colorcorrect(),

	#readRDS(file.path(lylby, "excess_pll_fig.rds")) + theme(axis.text.x = element_text(angle = 45, hjust = 1)) + colorcorrect() + year_label, nrow = 1, labels = c("A", "B", "C"), label_size = 25)

	fig_jacc <- plot_grid(readRDS(file.path(emby, "excess_death_rate_fig.rds")) + theme(axis.text.x = element_text(angle = 45, hjust = 1)) + colorcorrect(), readRDS(file.path(lylby, "excess_pll_fig.rds")) + theme(axis.text.x = element_text(angle = 45, hjust = 1)) + colorcorrect() + year_label, nrow = 1, labels = c("A", "B"), label_size = 25)

	#fig 1
	if (project == "cvd"){
		titletext <- "Overall Cardiovascular Disease"
	} 

	#fig 3
	if (project == "htncvd"){
		titletext <- "Hypertension"
	}

	#fig 2
	if (project == "ihdcvd"){
		titletext <- "Ischemic Heart Disease"
	}

	#fig 5
	if (project == "hfcvd"){
		titletext <- "Heart Failure"
	}

	#fig 4
	if (project == "cvcvd"){
		titletext <- "Cerebrovascular Disease"
	}



	#title <- ggdraw() + draw_label(titletext, fontface='bold', bg.color = "white")

	title <- ggplot() + theme_void() + theme(plot.background = element_rect(fill = "white")) + ggtitle(titletext) + theme(plot.title = element_text(size = 20, hjust = 0.5, face = "bold")) + theme(panel.border = element_blank())

	fig_jacc <- plot_grid(title, fig_jacc, ncol=1, rel_heights=c(0.08, 1)) # rel_heights values control title margins

	save_plot_custom(plt = fig_jacc, folder = figdir, width.in = 12, height.in = 5.2)

	save_plot_custom_tiff(plt = fig_jacc, folder = figdir, width.in = 12, height.in = 5.2)
}


fig2top <- plot_grid(readRDS(file.path(emba, "mortality_rate_ratio_fig.rds")) + theme(axis.text.x = element_text(angle = 45, hjust = 1)) + scale_y_continuous(breaks = scales::pretty_breaks(n = 6)), 

readRDS(file.path(emba, "excess_death_rate_age_fig.rds")) + theme(axis.text.x = element_text(angle = 45, hjust = 1)) + scale_y_continuous(breaks = scales::pretty_breaks(n = 6)), nrow = 1, labels = c("A", "B"), label_size = 25)



fig2 <- plot_grid(fig2top, readRDS(file.path(emba, "excess_death_rate_age_year_fig.rds")) + theme(axis.text.x = element_text(angle = 45, hjust = 1)) + scale_y_continuous(breaks = scales::pretty_breaks(n = 9)), nrow = 2, labels = c("", "C"), label_size = 25)


save_plot_custom(plt = fig2, folder = figdir, width.in = 10, height.in = 8.25)



