# TAF REPORT: prepare regional plot data and export stock-count status pies.
library(dplyr)
source("utilities.R")
source("utilities_plotting.R")
source("all_regions_config.R")

# Input exported by the RDBES shares workflow.
shares_file <- "data/stock_ecoregion_landings_shares.csv"
if (!file.exists(shares_file)) {
  stop("Place stock_ecoregion_landings_shares.csv at ", shares_file,
       " or update shares_file in report_2_plotting.R.")
}
share_lookup <- read.csv(shares_file, stringsAsFactors = FALSE) %>%
  transmute(
    StockKeyLabel = as.character(StockKeyLabel),
    Ecoregion = as.character(Ecoregion),
    RDBES_share_year = as.integer(as.character(CLyear)),
    share = as.numeric(as.character(ShareLandingsInEcoregion)) / 100,
    share_method, share_source, special_rule
  )
share_keys <- c("StockKeyLabel", "Ecoregion", "RDBES_share_year")
stopifnot(
  !anyDuplicated(share_lookup[share_keys]),
  !anyNA(share_lookup[share_keys]),
  all(is.na(share_lookup$share) | between(share_lookup$share, 0, 1))
)

# A zero share is usable. Only unavailable shares are excluded from selection.
available_shares <- share_lookup %>% filter(!is.na(share))
first_share_years <- available_shares %>%
  group_by(StockKeyLabel, Ecoregion) %>%
  summarise(first_share_year = min(RDBES_share_year), .groups = "drop")

add_ecoregion_shares <- function(x, year_column) {
  x %>%
    mutate(
      StockKeyLabel = as.character(StockKeyLabel),
      Ecoregion = as.character(Ecoregion),
      CatchYear = as.integer(as.character(.data[[year_column]]))
    ) %>%
    left_join(
      first_share_years,
      by = c("StockKeyLabel", "Ecoregion"),
      relationship = "many-to-one", na_matches = "never"
    ) %>%
    mutate(.share_search_year = pmax(CatchYear, first_share_year)) %>%
    left_join(
      available_shares,
      by = join_by(
        StockKeyLabel, Ecoregion,
        closest(.share_search_year >= RDBES_share_year)
      ),
      relationship = "many-to-one", na_matches = "never"
    ) %>%
    mutate(
      share_status = case_when(
        is.na(CatchYear) ~ "data year unavailable",
        is.na(share) ~ "share unavailable",
        RDBES_share_year == CatchYear ~ "same-year share",
        RDBES_share_year < CatchYear ~ "earlier share carried forward",
        TRUE ~ "later share applied to earlier year"
      ),
      Catches_in_ecoregion = Catches * share,
      Landings_in_ecoregion = Landings * share,
      Discards_in_ecoregion = Discards * share,
      Catches_out_ecoregion = Catches * (1 - share),
      Landings_out_ecoregion = Landings * (1 - share),
      Discards_out_ecoregion = Discards * (1 - share)
    ) %>%
    select(-first_share_year, -.share_search_year)
}

# Current catch quantities use F_Year. The existing Year column describes B.
stocks <- readRDS("output/stock_status_by_ecoregion.rds")
stopifnot(!anyDuplicated(stocks[c("StockKeyLabel", "Ecoregion")]))
stocks <- add_ecoregion_shares(stocks, "F_Year")

dir.create("report/plot_data", recursive = TRUE, showWarnings = FALSE)
can_plot <- requireNamespace("plotly", quietly = TRUE) &&
  requireNamespace("htmlwidgets", quietly = TRUE) &&
  exists("plot_status_prop_pies_plotly_v2", mode = "function")
if (can_plot) {
  dir.create("report/status_pies", recursive = TRUE, showWarnings = FALSE)
}

for (region in ecoregions) {
  code <- get_ecoregion_acronym(region)
  regional_current <- stocks[stocks$Ecoregion == region, , drop = FALSE]
  if (!nrow(regional_current)) stop("Missing current stocks for ", region)

  # Annual data retain every SAG year and the assessment/unit metadata.
  regional_annual <- readRDS(
    file.path("output/reference_points", paste0("v3_audit_", code, ".rds"))
  )$sag %>%
    rename(FisheriesGuild_reported = FisheriesGuild) %>%
    left_join(
      select(regional_current, StockKeyLabel, FisheriesGuild),
      by = "StockKeyLabel", relationship = "many-to-one", na_matches = "never"
    ) %>%
    mutate(
      Ecoregion = region,
      EcoregionCode = code,
      AssessmentSnapshotYear = regional_current$AssessmentSnapshotYear[[1L]],
      across(
        all_of(c("FishingPressure", "StockSize", "FMSY", "MSYBtrigger")),
        ~ suppressWarnings(as.numeric(as.character(.x)))
      ),
      F_FMSY = FishingPressure / FMSY,
      SSB_MSYBtrigger = StockSize / MSYBtrigger,
      FStatus = case_when(
        !is.finite(FMSY) | FMSY <= 0 ~ "GREY",
        !is.finite(F_FMSY) | F_FMSY < 0 ~ "GREY",
        F_FMSY > 1 ~ "RED",
        TRUE ~ "GREEN"
      ),
      BStatus = case_when(
        !is.finite(MSYBtrigger) | MSYBtrigger <= 0 ~ "GREY",
        !is.finite(SSB_MSYBtrigger) | SSB_MSYBtrigger < 0 ~ "GREY",
        SSB_MSYBtrigger < 1 ~ "RED",
        TRUE ~ "GREEN"
      )
    ) %>%
    add_ecoregion_shares("Year")

  saveRDS(regional_current,
          file.path("report/plot_data", paste0("current_", code, ".rds")))
  saveRDS(regional_annual,
          file.path("report/plot_data", paste0("annual_", code, ".rds")))
  message("Prepared ", code, ": ", nrow(regional_current),
          " current rows and ", nrow(regional_annual), " annual rows.")

  if (can_plot) {
    tryCatch({
      p <- plot_status_prop_pies_plotly_v2(regional_current)
      htmlwidgets::saveWidget(p,
        file = file.path("report/status_pies", paste0("status_pies_", code, ".html")),
        selfcontained = TRUE)
    }, error = function(e) warning("Could not save pies for ", region, ": ",
                                   conditionMessage(e)))
    tryCatch({
      p <- plot_GES_pies_plotly_v2(regional_current)
      htmlwidgets::saveWidget(p,
        file = file.path("report/status_pies", paste0("ges_pies_", code, ".html")),
        selfcontained = TRUE)
    }, error = function(e) warning("Could not save GES pies for ", region, ": ",
                                   conditionMessage(e)))
    tryCatch({
      regional_discards <- regional_annual %>%
        rename(
          Landings_ecoregion = Landings_in_ecoregion,
          Discards_ecoregion = Discards_in_ecoregion,
          Catches_ecoregion = Catches_in_ecoregion
        )
      p <- plot_discard_trends_app_plotly_v3(
        regional_discards,
        year = regional_current$AssessmentSnapshotYear[[1L]],
        ecoregion = region
      )
      dir.create("report/discard_trends", recursive = TRUE, showWarnings = FALSE)
      htmlwidgets::saveWidget(p,
        file = file.path("report/discard_trends", paste0("discard_trends_", code, ".html")),
        selfcontained = TRUE)
    }, error = function(e) warning("Could not save discard trends for ", region, ": ",
                                   conditionMessage(e)))
  }
}
