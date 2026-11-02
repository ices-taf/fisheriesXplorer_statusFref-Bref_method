# Produce plots and tables for report

# Before:
# After:

library(icesTAF)
source("utilities.R")
source("utilities_plotting.R")
mkdir("report")


plot_status_prop_pies_plotly_v2(catch_current_v3)


plot_GES_pies_plotly_v2(catch_current_v3)

p6 <- plot_kobe_app_plotly_v3(catch_current_v3, "benthic", size_by_catches = TRUE)
