# TAF REPORT: optional interactive stock-count status pies.
# Catch-weighted pies/Kobe sizing are deferred until regional RDBES data exist.
library(dplyr)
source("utilities.R")
source("utilities_plotting.R")
source("all_regions_config.R")

stocks <- readRDS("output/stock_status_by_ecoregion.rds")

# The existing plotting helper generates stock-count pies from ratio status.
# Producing HTML is optional; missing optional packages must not invalidate
# already validated reference-point outputs.
if (requireNamespace("plotly", quietly = TRUE) &&
    requireNamespace("htmlwidgets", quietly = TRUE) &&
    exists("plot_status_prop_pies_plotly_v2", mode = "function")) {
  dir.create("report/status_pies", recursive = TRUE, showWarnings = FALSE)
  for (region in ecoregions) {
    code <- get_ecoregion_acronym(region)
    regional <- stocks[stocks$Ecoregion == region, , drop = FALSE]
    if (!nrow(regional)) next
    tryCatch({
      p <- plot_status_prop_pies_plotly_v2(regional)
      htmlwidgets::saveWidget(p,
        file = file.path("report/status_pies", paste0("status_pies_", code, ".html")),
        selfcontained = TRUE)
    }, error = function(e) warning("Could not save pies for ", region, ": ",
                                   conditionMessage(e)))
  }
}