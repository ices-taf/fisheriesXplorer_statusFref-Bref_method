# TAF REPORT: reference-point coverage summary.
library(icesTAF)
library(dplyr)
source("utilities.R")
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