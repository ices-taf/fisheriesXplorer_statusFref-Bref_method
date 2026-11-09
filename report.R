# TAF REPORT: coverage and optional interactive stock-count status pies.
# Catch-weighted pies/Kobe sizing are deferred until regional RDBES data exist.
library(icesTAF)
library(dplyr)
source("utilities.R")
source("utilities_plotting.R")
source("all_regions_config.R")
mkdir("report")

coverage <- utils::read.csv("output/coverage_by_ecoregion.csv",
                            stringsAsFactors = FALSE)
stocks <- readRDS("output/stock_status_by_ecoregion.rds")
utils::write.csv(coverage, "report/stock_status_coverage.csv", row.names = FALSE)

summary_lines <- c(
  "# Reference-point coverage across ICES ecoregions",
  "",
  paste0("Assessment-year snapshot: ", assessment_year),
  paste0("Ecoregions processed: ", nrow(coverage)),
  paste0("Stock–ecoregion records: ", nrow(stocks)),
  paste0("Unique stock identifiers: ", dplyr::n_distinct(stocks$StockKeyLabel)),
  paste0("Records with both selected reference points: ",
         sum(coverage$n_both_refs)),
  paste0("Records with both valid plotted ratios: ",
         sum(coverage$n_both_ratios)),
  "",
  "The reference-point eligibility policy is provisional for automatically",
  "approved new candidates. This is a reference-point analysis, not an RDBES",
  "ecoregion-adjusted catches/landings analysis.",
  "",
  "See output/coverage_by_ecoregion.csv and output/reference_points/ for audits.",
  paste0("Named SAG settings resolved: ",
         nrow(utils::read.csv("output/reference_points/resolved_named_settings.csv"))),
  paste0("Settings remaining unresolved: ",
         nrow(utils::read.csv("output/reference_points/unresolved_settings.csv"))),
  paste0("Ambiguous guild assignments within ecoregions: ",
         nrow(utils::read.csv("output/guild_conflicts_within_ecoregion.csv"))),
  paste0("Stocks whose guild differs between ecoregions: ",
         nrow(utils::read.csv("output/guild_variations_across_ecoregions.csv")))
)
writeLines(summary_lines, "report/reference_point_summary.md")

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
