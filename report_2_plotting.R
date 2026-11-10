# TAF REPORT: prepare regional plot data and export pies, discards and Kobe plots.
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
export_dir <- "report/html_plots"
dir.create(export_dir, recursive = TRUE, showWarnings = FALSE)
kobe_guilds <- c("benthic", "demersal", "elasmobranch", "pelagic", "shellfish",
                 "all_stocks")
expected_plot_index <- expand.grid(
  plot_type = c("stock_summary", "ges", "discards"),
  ecoregion_id = vapply(ecoregions, get_ecoregion_acronym, character(1)),
  guild_id = "",
  stringsAsFactors = FALSE
)
expected_plot_index <- bind_rows(
  expected_plot_index,
  expand.grid(
    plot_type = "kobe",
    ecoregion_id = vapply(ecoregions, get_ecoregion_acronym, character(1)),
    guild_id = kobe_guilds,
    stringsAsFactors = FALSE
  )
)
expected_plot_index$file <- with(
  expected_plot_index,
  ifelse(nzchar(guild_id),
         paste0(plot_type, "__", ecoregion_id, "__", guild_id, ".html"),
         paste0(plot_type, "__", ecoregion_id, ".html"))
)
index_columns <- c("plot_type", "ecoregion_id", "guild_id", "file")
stopifnot(
  !anyDuplicated(expected_plot_index[c("plot_type", "ecoregion_id", "guild_id")]),
  !anyDuplicated(expected_plot_index$file)
)
export_report <- transform(
  expected_plot_index, status = "failed", reason = "Not attempted."
)

write_plot_export_report <- function() {
  utils::write.csv(
    export_report[export_report$status == "exported", index_columns, drop = FALSE],
    file.path(export_dir, "plot_index.csv"), row.names = FALSE
  )
  utils::write.csv(export_report,
                   file.path(export_dir, "plot_export_report.csv"), row.names = FALSE)
}
write_plot_export_report()

save_plot_export <- function(plot_type, ecoregion_id, build_plot, guild_id = "") {
  selection <- which(export_report$plot_type == plot_type &
                       export_report$ecoregion_id == ecoregion_id &
                       export_report$guild_id == guild_id)
  stopifnot(length(selection) == 1L)
  plot_file <- file.path(export_dir, export_report$file[[selection]])
  outcome <- tryCatch({
    if (file.exists(plot_file) && unlink(plot_file) != 0L) {
      stop("Could not remove previous export: ", plot_file)
    }
    if (!can_plot) stop("HTML export requires plotly, htmlwidgets and plotting helpers.")
    plot <- build_plot()
    htmlwidgets::saveWidget(plot, file = plot_file,
                            selfcontained = FALSE, libdir = "lib")
    if (!file.exists(plot_file) || file.info(plot_file)$size <= 0) {
      stop("HTML export was not written: ", plot_file)
    }
    list(status = "exported", reason = "")
  }, plot_no_data = function(error) {
    list(status = "no_data", reason = conditionMessage(error))
  }, error = function(error) {
    list(status = "failed", reason = conditionMessage(error))
  })
  if (outcome$status != "exported" && file.exists(plot_file)) unlink(plot_file)
  export_report$status[[selection]] <<- outcome$status
  export_report$reason[[selection]] <<- outcome$reason
  write_plot_export_report()
    message(plot_type, " / ", ecoregion_id,
      if (nzchar(guild_id)) paste0(" / ", guild_id) else "", ": ", outcome$status,
          if (nzchar(outcome$reason)) paste0(" - ", outcome$reason) else "")
  invisible(outcome)
}

tryCatch(for (region in ecoregions) {
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

  save_plot_export("stock_summary", code, function() {
    plot_status_prop_pies_plotly_v2(regional_current)
  })
  save_plot_export("ges", code, function() {
    plot_GES_pies_plotly_v2(regional_current)
  })
  save_plot_export("discards", code, function() {
    regional_discards <- regional_annual %>%
      rename(
        Landings_ecoregion = Landings_in_ecoregion,
        Discards_ecoregion = Discards_in_ecoregion,
        Catches_ecoregion = Catches_in_ecoregion
      )
    cutoff_year <- regional_current$AssessmentSnapshotYear[[1L]]
    discard_data <- plot_discard_trends_app_plotly_v3(
      regional_discards, year = cutoff_year, return_data = TRUE
    )
    if (!nrow(discard_data)) {
      stop_plot_no_data("No usable regional landings and discards in the reporting period.")
    }
    plot_discard_trends_app_plotly_v3(
      regional_discards, year = cutoff_year, ecoregion = region
    )
  })
  for (guild in kobe_guilds) {
    save_plot_export("kobe", code, function() {
      plot_guild <- if (identical(guild, "all_stocks")) "All" else guild
      guild_stocks <- if (identical(plot_guild, "All")) {
        regional_current
      } else {
        filter(regional_current, FisheriesGuild %in% plot_guild)
      }
      if (!nrow(guild_stocks)) {
        stop_plot_no_data(paste0("No stocks for fisheries guild: ", guild))
      }
      plot_kobe_app_plotly_v3(regional_current, guild = plot_guild)
    }, guild_id = guild)
  }
}, error = function(error) {
  unattempted <- export_report$reason == "Not attempted."
  export_report$reason[unattempted] <<- paste0(
    "Report preparation failed: ", conditionMessage(error)
  )
  write_plot_export_report()
  stop(error)
})

if (any(export_report$status == "failed")) {
  stop("HTML plot collection is incomplete: ", sum(export_report$status == "failed"),
       " failed exports. See ", file.path(export_dir, "plot_export_report.csv"))
}
