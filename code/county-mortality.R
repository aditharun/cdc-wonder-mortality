library(usmap)
library(ggplot2)
library(viridis)

args = commandArgs(trailingOnly=TRUE)
project <- args[1]
plotdir <- file.path("../plots", project, "map")

inputfile <- file.path(file.path("../data", project), "county_data.txt")
df <- read.table(inputfile, header = TRUE, sep = "\t", stringsAsFactors = FALSE)
df <- df[, c("fips", "values")]
df$values <- tryCatch(as.numeric(df$values), error = function(e) { 0 })
df$values[is.na(df$values)] <- 0
map <- plot_usmap(data = df, values = "values") + 
  scale_fill_viridis_c(
    option = "C", name = "Age Adjusted Death Rate", label = scales::comma
  ) + theme(legend.position = "right")
ggsave(file.path(plotdir, "map_plot.png"), plot = map, width = 10, height = 6, dpi = 300)