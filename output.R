# TAF OUTPUT: run approved v3, attach provenance, consolidate regional stocks.
# No network requests or policy editing occur here.
library(icesTAF)
library(dplyr)
source("utilities.R")
source("all_regions_config.R")
mkdir("output")
dir.create("output/regions", recursive = TRUE, showWarnings = FALSE)
dir.create("output/reference_points", recursive = TRUE, showWarnings = FALSE)

# Optional regional guild decisions. Only FisheriesGuild contains approved values.
guild_path <- "config/manual_fisheries_guilds.csv"
guild_overrides <- NULL
if (file.exists(guild_path)) {
  guild_overrides <- utils::read.csv(guild_path, colClasses = "character",
                                    stringsAsFactors = FALSE)
  keys <- c("EcoregionCode", "StockKeyLabel")
  required_cols <- c(keys, "FisheriesGuild")
  if (!all(required_cols %in% names(guild_overrides))) {
    stop("Missing columns in ", guild_path, ": ",
         paste(setdiff(required_cols, names(guild_overrides)), collapse = ", "))
  }
  guild_overrides <- guild_overrides |>
    dplyr::transmute(EcoregionCode = trimws(EcoregionCode),
                     StockKeyLabel = trimws(StockKeyLabel),
                     FisheriesGuild = tolower(trimws(FisheriesGuild)))
  codes <- vapply(ecoregions, get_ecoregion_acronym, character(1))
  if (any(!guild_overrides$EcoregionCode %in% codes) ||
      any(is.na(guild_overrides$StockKeyLabel) | !nzchar(guild_overrides$StockKeyLabel))) {
    stop("Unknown ecoregion code or empty stock key in ", guild_path)
  }
  if (anyDuplicated(guild_overrides[keys])) stop("Duplicate stock-ecoregion key in ", guild_path)
  guild_overrides <- dplyr::filter(guild_overrides, !is.na(FisheriesGuild), FisheriesGuild != "")
  allowed_guilds <- c("benthic", "demersal", "elasmobranch", "pelagic", "shellfish")
  invalid <- setdiff(guild_overrides$FisheriesGuild, allowed_guilds)
  if (length(invalid)) stop("Unrecognised fisheries guild in ", guild_path, ": ",
                            paste(invalid, collapse = ", "))
}

results <- vector("list", length(ecoregions))
failures <- list()
for (i in seq_along(ecoregions)) {
  region <- ecoregions[[i]]
  code <- get_ecoregion_acronym(region)
  message("OUTPUT ", i, "/", length(ecoregions), ": ", region)
  result_i <- tryCatch({
    snapshot <- readRDS(file.path("data/regions", paste0(code, ".rds")))
    policy_path <- file.path("model/policies", paste0("SAG_policy_", code, "_approved.csv"))
    res <- add_proxyRefPoints_v3(
      snapshot$original, snapshot$settings,
      review_policy = policy_path, return_audit = TRUE,
      strict_current_review = TRUE
    )
    # A fisheries guild is scoped to a stock AND ecoregion, not globally.
    # More than one reported guild WITHIN a region is a genuine ambiguity:
    # retain the source labels and assign Unknown provisionally. Never select
    # an arbitrary guild, and never multiply stock rows in Total/pie outputs.
    guild_membership <- snapshot$original |>
      dplyr::transmute(
        StockKeyLabel = as.character(StockKeyLabel),
        FisheriesGuild = trimws(as.character(FisheriesGuild))
      ) |>
      dplyr::mutate(FisheriesGuild = dplyr::if_else(
        is.na(FisheriesGuild) | !nzchar(FisheriesGuild),
        "Unknown", FisheriesGuild
      )) |>
      dplyr::distinct()

    universe <- guild_membership |>
      dplyr::group_by(StockKeyLabel) |>
      dplyr::summarise(
        FisheriesGuild_reported = paste(sort(unique(FisheriesGuild)),
                                         collapse = "; "),
        n_reported_guilds = dplyr::n_distinct(FisheriesGuild),
        GuildConflictInRegion = n_reported_guilds > 1L,
        FisheriesGuild = if (n_reported_guilds == 1L) {
          dplyr::first(FisheriesGuild)
        } else {
          "Unknown"
        },
        .groups = "drop"
      )

    if (!is.null(guild_overrides)) {
      regional <- dplyr::filter(guild_overrides, EcoregionCode == code)
      idx <- match(universe$StockKeyLabel, regional$StockKeyLabel)
      fill <- !universe$GuildConflictInRegion &
        tolower(universe$FisheriesGuild) == "unknown" & !is.na(idx)
      universe$FisheriesGuild[fill] <- regional$FisheriesGuild[idx[fill]]
      missing_stocks <- setdiff(regional$StockKeyLabel, universe$StockKeyLabel)
      if (length(missing_stocks)) warning("Guild override stocks absent in ", code, ": ",
                                          paste(missing_stocks, collapse = ", "))
    }

    region_guild_conflicts <- universe |>
      dplyr::filter(GuildConflictInRegion) |>
      dplyr::mutate(Ecoregion = region, EcoregionCode = code) |>
      dplyr::select(Ecoregion, EcoregionCode, StockKeyLabel,
                    FisheriesGuild_reported, n_reported_guilds,
                    FisheriesGuild, GuildConflictInRegion)
    if (nrow(region_guild_conflicts)) {
      warning("Conflicting guild assignments within ", region, ": ",
              paste(region_guild_conflicts$StockKeyLabel, collapse = ", "),
              ". Retaining original labels and using Unknown for plotting.")
    }

    # Normalise the guild before deriving stock status, otherwise the F and B
    # sides can receive different guilds and fail to join into one stock row.
    sag_for_status <- res$sag |>
      dplyr::select(-dplyr::any_of("FisheriesGuild")) |>
      dplyr::left_join(
        dplyr::select(universe, StockKeyLabel, FisheriesGuild),
        by = "StockKeyLabel"
      )
    if (nrow(sag_for_status) != nrow(res$sag)) {
      stop("Guild harmonisation changed the number of SAG rows for ", region)
    }
    # Guard against a real assessment disagreement hidden behind the two
    # guild labels. Identical repeated measurements are harmless; conflicting
    # values for the same stock and data year must not be picked arbitrarily.
    if (nrow(region_guild_conflicts)) {
      check_cols <- intersect(
        c("FishingPressure", "StockSize", "FMSY", "MSYBtrigger"),
        names(sag_for_status)
      )
      if (length(check_cols)) {
        within_stock_measure_conflicts <- sag_for_status |>
          dplyr::filter(StockKeyLabel %in% region_guild_conflicts$StockKeyLabel) |>
          dplyr::group_by(StockKeyLabel, Year) |>
          dplyr::summarise(
            dplyr::across(dplyr::all_of(check_cols),
                          ~ dplyr::n_distinct(.x, na.rm = FALSE)),
            .groups = "drop"
          ) |>
          dplyr::filter(dplyr::if_any(
            dplyr::all_of(check_cols), ~ .x > 1L
          ))
        if (nrow(within_stock_measure_conflicts)) {
          stop("Guild-ambiguous stocks also contain conflicting F/B values ",
               "for the same stock-year in ", region, ": ",
               paste(unique(within_stock_measure_conflicts$StockKeyLabel),
                     collapse = ", "), ". Resolve source data first.")
        }
      }
    }
    current <- stockstatus_CLD_current_proxy(sag_for_status)
    if (anyDuplicated(current$StockKeyLabel)) {
      stop("Multiple status records for a stock in ", region,
           "; investigate assessment/component duplication")
    }

    # Retain even stocks without usable F/B observations, once per region.
    # The region-level guild and the original labels both remain available.
    current <- universe |>
      dplyr::left_join(
        dplyr::select(current, -dplyr::any_of("FisheriesGuild")),
        by = "StockKeyLabel"
      )
    current <- current |>
      dplyr::mutate(
        FStatus = dplyr::coalesce(FStatus, "GREY"),
        BStatus = dplyr::coalesce(BStatus, "GREY")
      )
    current <- attach_refpoint_provenance_v3(current, res$current_v3)
    current$Ecoregion <- region
    current$EcoregionCode <- code
    current$AssessmentSnapshotYear <- assessment_year
    if (nrow(current) != nrow(universe)) stop("Stock universe changed during joins")

    saveRDS(current, file.path("output/regions", paste0("stocks_", code, ".rds")))
    saveRDS(res, file.path("output/reference_points", paste0("v3_audit_", code, ".rds")))
    list(stocks = current, audit = res,
         guild_conflicts = region_guild_conflicts)
  }, error = function(e) e)
  if (inherits(result_i, "error")) {
    failures[[length(failures) + 1L]] <- data.frame(
      Ecoregion = region, error = conditionMessage(result_i),
      stringsAsFactors = FALSE
    )
  } else {
    results[[i]] <- result_i
  }
}

if (length(failures)) {
  utils::write.csv(dplyr::bind_rows(failures), "output/failed_regions.csv", row.names = FALSE)
  stop(length(failures), " ecoregion(s) failed. See output/failed_regions.csv. ",
       "No final consolidated dataset was published.")
}

by_ecoregion <- dplyr::bind_rows(lapply(results, `[[`, "stocks"))
within_region_guild_conflicts <- dplyr::bind_rows(
  lapply(results, `[[`, "guild_conflicts")
)
utils::write.csv(within_region_guild_conflicts,
                 "output/guild_conflicts_within_ecoregion.csv",
                 row.names = FALSE, na = "")
if (anyDuplicated(by_ecoregion[c("StockKeyLabel", "Ecoregion")])) {
  stop("Duplicate stock-ecoregion pairs; investigate region-specific results")
}
if (!setequal(unique(by_ecoregion$Ecoregion), ecoregions)) {
  stop("Missing ecoregions in consolidated output")
}

# Write audit and diagnostics before validating the unique-stock snapshot.
all_candidates <- dplyr::bind_rows(lapply(seq_along(results), function(i) {
  dplyr::mutate(results[[i]]$audit$candidates, Ecoregion = ecoregions[[i]])
}))
all_unresolved <- dplyr::bind_rows(lapply(seq_along(results), function(i) {
  dplyr::mutate(results[[i]]$audit$unresolved_settings,
                Ecoregion = ecoregions[[i]])
}))
all_selections <- dplyr::bind_rows(lapply(seq_along(results), function(i) {
  dplyr::mutate(results[[i]]$audit$current_v3, Ecoregion = ecoregions[[i]])
}))
all_comparisons <- dplyr::bind_rows(lapply(seq_along(results), function(i) {
  dplyr::mutate(results[[i]]$audit$comparison, Ecoregion = ecoregions[[i]])
}))
utils::write.csv(all_selections, "output/reference_points/selected_v3_by_stock_region.csv",
                 row.names = FALSE, na = "")
utils::write.csv(all_comparisons, "output/reference_points/v2_vs_v3.csv",
                 row.names = FALSE, na = "")
utils::write.csv(all_candidates, "output/reference_points/all_v3_candidates.csv",
                 row.names = FALSE, na = "")
# Keep the original v2 diagnostic for traceability, then remove named
# settings that model.R demonstrably mapped to a valid numerical candidate.
utils::write.csv(all_unresolved,
                 "output/reference_points/unresolved_settings_raw_v2.csv",
                 row.names = FALSE, na = "")
resolved_file <- "model/named_settings_resolved.csv"
if (!file.exists(resolved_file)) {
  stop("Missing ", resolved_file, "; run model.R before output.R")
}
resolved_named <- utils::read.csv(resolved_file, stringsAsFactors = FALSE,
                                   colClasses = "character")
resolution_keys <- c("Ecoregion", "AssessmentKey", "SAGChartKey", "setting_token")
if (!all(resolution_keys %in% names(resolved_named))) {
  stop("Named settings resolution log lacks expected columns.")
}
for (nm in resolution_keys) {
  all_unresolved[[nm]] <- as.character(all_unresolved[[nm]])
  resolved_named[[nm]] <- as.character(resolved_named[[nm]])
}
remaining_unresolved <- dplyr::anti_join(
  all_unresolved,
  dplyr::distinct(resolved_named[, resolution_keys, drop = FALSE]),
  by = resolution_keys
)
utils::write.csv(resolved_named,
                 "output/reference_points/resolved_named_settings.csv",
                 row.names = FALSE, na = "")
utils::write.csv(remaining_unresolved,
                 "output/reference_points/unresolved_settings.csv",
                 row.names = FALSE, na = "")

coverage <- by_ecoregion |>
  dplyr::group_by(Ecoregion) |>
  dplyr::summarise(
    n_stocks = dplyr::n_distinct(StockKeyLabel),
    n_Fref = sum(is.finite(Fref) & Fref > 0),
    n_Bref = sum(is.finite(Bref) & Bref > 0),
    n_both_refs = sum(is.finite(Fref) & Fref > 0 & is.finite(Bref) & Bref > 0),
    n_both_ratios = sum(is.finite(F_FMSY) & F_FMSY >= 0 &
                          is.finite(SSB_MSYBtrigger) & SSB_MSYBtrigger >= 0),
    .groups = "drop"
  )
utils::write.csv(coverage, "output/coverage_by_ecoregion.csv", row.names = FALSE)

# A repeated stock should have the same assessment and reference-point values
# regardless of which ecoregion downloaded it. Guild may vary by region, so
# it is deliberately EXCLUDED from this validation.
compare_cols <- intersect(c(
  "F_AssessmentKey", "B_AssessmentKey", "F_Year", "B_Year",
  "Fref", "Bref", "Fref_name", "Bref_name",
  "F_FMSY", "SSB_MSYBtrigger",
  "FishingPressure", "StockSize", "Catches", "Landings"
), names(by_ecoregion))
conflicts <- by_ecoregion |>
  dplyr::group_by(StockKeyLabel) |>
  dplyr::summarise(
    dplyr::across(dplyr::all_of(compare_cols),
                  ~ dplyr::n_distinct(.x, na.rm = FALSE)),
    .groups = "drop"
  ) |>
  dplyr::filter(dplyr::if_any(dplyr::all_of(compare_cols), ~ .x > 1L))
utils::write.csv(conflicts, "output/conflicting_stocks.csv", row.names = FALSE)

if (nrow(conflicts)) {
  # The by-ecoregion file remains a useful diagnostic, but not an approved
  # final release. Never silently pick one of the conflicting assessments.
  saveRDS(by_ecoregion, "output/stock_status_by_ecoregion_REVIEW.rds", compress = "xz")
  stop("Cross-region inconsistencies for ", nrow(conflicts),
       " stocks. See output/conflicting_stocks.csv. ",
       "Resolve before publishing final snapshots.")
}

# Guild changes BETWEEN ecoregions are valid metadata, not assessment
# conflicts. Keep the complete regional mapping, and never publish a random
# single "global" guild in the unique-stock table.
regional_guild_variation <- by_ecoregion |>
  dplyr::group_by(StockKeyLabel) |>
  dplyr::summarise(
    n_guilds = dplyr::n_distinct(FisheriesGuild),
    guilds_by_ecoregion = paste(
      paste(Ecoregion, FisheriesGuild, sep = ": "),
      collapse = "; "
    ),
    guilds_reported_by_ecoregion = paste(
      paste(Ecoregion, FisheriesGuild_reported, sep = ": "),
      collapse = "; "
    ),
    .groups = "drop"
  )
utils::write.csv(
  dplyr::filter(regional_guild_variation, n_guilds > 1L),
  "output/guild_variations_across_ecoregions.csv",
  row.names = FALSE, na = ""
)

membership <- by_ecoregion |>
  dplyr::group_by(StockKeyLabel) |>
  dplyr::summarise(
    Ecoregions = paste(sort(unique(Ecoregion)), collapse = "; "),
    n_ecoregions = dplyr::n_distinct(Ecoregion),
    .groups = "drop"
  ) |>
  dplyr::left_join(regional_guild_variation, by = "StockKeyLabel")
unique_stocks <- by_ecoregion |>
  dplyr::arrange(StockKeyLabel, Ecoregion) |>
  dplyr::distinct(StockKeyLabel, .keep_all = TRUE) |>
  dplyr::select(-Ecoregion, -EcoregionCode) |>
  dplyr::left_join(membership, by = "StockKeyLabel") |>
  dplyr::mutate(
    GuildVariesByEcoregion = n_guilds > 1L,
    FisheriesGuild = dplyr::if_else(
      GuildVariesByEcoregion, "Varies by ecoregion", FisheriesGuild
    )
  )

saveRDS(by_ecoregion, "output/stock_status_by_ecoregion.rds", compress = "xz")
saveRDS(unique_stocks, "output/stock_status_unique.rds", compress = "xz")
utils::write.csv(by_ecoregion, "output/stock_status_by_ecoregion.csv",
                 row.names = FALSE, na = "")
utils::write.csv(unique_stocks, "output/stock_status_unique.csv",
                 row.names = FALSE, na = "")
message("Validated ", nrow(by_ecoregion), " stock-ecoregion records; ",
        nrow(unique_stocks), " unique stocks across ", length(ecoregions), " ecoregions.")
