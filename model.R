# TAF MODEL: approve/reference candidates and resolve named SAG settings.
# Reuses config/manual_reference_overrides.csv for BOTH existing custom
# reference overrides and named setting-to-field mappings (e.g. Fmgt).
# Data retrieval stays in boot/; this stage only reads prepared data/ files.
library(icesTAF)
library(dplyr)
library(tidyr)
source("utilities.R")
source("all_regions_config.R")
mkdir("model")
dir.create("model/policies", recursive = TRUE, showWarnings = FALSE)

# The existing six columns remain unchanged. Two OPTIONAL columns are added:
#   setting_token : named SAG settings token (blank for older custom overrides)
#   SAGChartKey   : 3 = Fref, 4 = Bref (derived from Indicator when blank)
manual <- if (file.exists(manual_overrides_path)) {
  utils::read.csv(manual_overrides_path, stringsAsFactors = FALSE,
                  colClasses = "character", check.names = FALSE)
} else {
  data.frame(StockKeyLabel = character(), AssessmentKey = character(),
             reference_field = character(), reference_name = character(),
             Indicator = character(), source_issue_note = character(),
             setting_token = character(), SAGChartKey = character())
}
required_manual <- c("StockKeyLabel", "AssessmentKey", "reference_field",
                     "reference_name", "Indicator", "source_issue_note")
if (!all(required_manual %in% names(manual))) {
  stop("manual_reference_overrides.csv is missing: ",
       paste(setdiff(required_manual, names(manual)), collapse = ", "))
}
for (nm in c("setting_token", "SAGChartKey")) {
  if (!nm %in% names(manual)) manual[[nm]] <- ""
}
clean <- function(z) {
  z <- trimws(as.character(z))
  z[is.na(z)] <- ""
  z
}
for (nm in names(manual)) manual[[nm]] <- clean(manual[[nm]])

if (any(!manual$Indicator %in% c("Fref", "Bref"))) {
  stop("Manual overrides require Indicator = Fref or Bref.")
}
expected_chart <- ifelse(manual$Indicator == "Fref", "3", "4")
manual$SAGChartKey[!nzchar(manual$SAGChartKey)] <-
  expected_chart[!nzchar(manual$SAGChartKey)]
if (any(manual$SAGChartKey != expected_chart)) {
  stop("Manual override has SAGChartKey inconsistent with Indicator.")
}
if (any(!nzchar(manual$AssessmentKey) | !nzchar(manual$reference_field))) {
  stop("Each manual override needs AssessmentKey and reference_field.")
}
if (any(!nzchar(manual$StockKeyLabel) & !nzchar(manual$setting_token))) {
  stop("Old-style custom overrides require a StockKeyLabel. ",
       "Only named-setting mappings may leave StockKeyLabel empty.")
}
manual_key <- manual |>
  dplyr::mutate(.key = paste(StockKeyLabel, AssessmentKey, setting_token,
                              SAGChartKey, reference_field, reference_name,
                              sep = "::"))
if (anyDuplicated(manual_key$.key)) stop("Duplicate override rows in manual_reference_overrides.csv")

# A structured log is used by output.R to remove ONLY successfully resolved
# named tokens from the old v2-oriented unresolved_settings diagnostic.
resolved_named <- data.frame(
  Ecoregion = character(), StockKeyLabel = character(),
  AssessmentKey = character(), SAGChartKey = integer(),
  setting_token = character(), Indicator = character(),
  reference_field = character(), reference_name = character(),
  mapping_type = character(), stringsAsFactors = FALSE
)

logs <- vector("list", length(ecoregions))
for (i in seq_along(ecoregions)) {
  region <- ecoregions[[i]]
  code <- get_ecoregion_acronym(region)
  message("MODEL ", i, "/", length(ecoregions), ": ", region)
  policy <- utils::read.csv(
    file.path("data/policies", paste0("SAG_policy_", code, ".csv")),
    stringsAsFactors = FALSE, colClasses = "character", check.names = FALSE
  )
  policy$manual_chart_override <- toupper(clean(policy$manual_chart_override)) %in%
    c("TRUE", "T", "1", "YES")
  policy$source_issue_note <- clean(policy$source_issue_note)
  policy$evidence_notes <- clean(policy$evidence_notes)

  # Existing default policy: approve chart-assigned candidates provisionally,
  # EXCEPT the configured exclusions. Do not approve unassigned candidates here.
  newly_reviewed <- policy$Indicator %in% c("Fref", "Bref") &
    policy$review_decision == "REVIEW"
  policy$review_decision[newly_reviewed] <- "INCLUDE"
  policy$matches_numerator[newly_reviewed] <- "YES"
  policy$evidence_notes[newly_reviewed] <-
    "Provisional bulk approval; verify numerator compatibility"
  excluded <- policy$Indicator %in% c("Fref", "Bref") &
    tolower(clean(policy$reference_field)) %in%
    tolower(excluded_standard_fields)
  policy$review_decision[excluded] <- "EXCLUDE"
  policy$matches_numerator[excluded] <- "NO"
  policy$evidence_notes[excluded] <- "Excluded by configured selection policy"

  snapshot <- readRDS(file.path("data/regions", paste0(code, ".rds")))
  original <- snapshot$original
  settings <- snapshot$settings
  keys_in_region <- unique(clean(original$AssessmentKey))

  setting_tokens <- settings |>
    dplyr::filter(as.character(settingKey) == "51",
                  as.character(SAGChartKey) %in% c("3", "4")) |>
    dplyr::transmute(
      AssessmentKey = clean(AssessmentKey),
      SAGChartKey = as.integer(as.character(SAGChartKey)),
      setting_token = as.character(settingValue)
    ) |>
    tidyr::separate_rows(setting_token, sep = ",") |>
    dplyr::mutate(setting_token = clean(setting_token)) |>
    dplyr::filter(nzchar(setting_token)) |>
    dplyr::distinct()

  applicable <- manual[manual$AssessmentKey %in% keys_in_region, , drop = FALSE]
  n_manual <- 0L
  n_named <- 0L
  for (j in seq_len(nrow(applicable))) {
    ov <- applicable[j, , drop = FALSE]
    chart <- as.integer(ov$SAGChartKey)
    named <- nzchar(ov$setting_token)
    assessment_rows <- original[
      clean(original$AssessmentKey) == ov$AssessmentKey, , drop = FALSE
    ]
    stock_codes <- unique(clean(assessment_rows$StockKeyLabel))
    stock_codes <- stock_codes[nzchar(stock_codes)]
    if (nzchar(ov$StockKeyLabel)) {
      if (!ov$StockKeyLabel %in% stock_codes) {
        stop("Assessment ", ov$AssessmentKey, " exists in ", region,
             " but the configured StockKeyLabel does not match: ", ov$StockKeyLabel)
      }
      stock <- ov$StockKeyLabel
    } else {
      if (length(stock_codes) != 1L) {
        stop("Named setting AssessmentKey ", ov$AssessmentKey,
             " matches ", length(stock_codes), " stocks in ", region,
             "; fill StockKeyLabel in manual_reference_overrides.csv.")
      }
      stock <- stock_codes[[1]]
    }

    if (named) {
      matching_token <- setting_tokens |>
        dplyr::filter(AssessmentKey == ov$AssessmentKey,
                      SAGChartKey == chart,
                      tolower(setting_token) == tolower(ov$setting_token))
      if (nrow(matching_token) != 1L) {
        stop("Named setting token '", ov$setting_token, "' not found uniquely for ",
             "AssessmentKey ", ov$AssessmentKey, " chart ", chart,
             " in ", region, ". Recheck the SAG settings or mapping.")
      }
    }

    # A named token is not itself a numeric value. The declared SAG field
    # MUST exist and contain a positive value for the same stock/assessment.
    actual_field <- names(original)[
      tolower(names(original)) == tolower(ov$reference_field)
    ]
    if (length(actual_field) != 1L) {
      stop("Mapped reference field '", ov$reference_field,
           "' absent/ambiguous in formatted SAG for ", region,
           ", assessment ", ov$AssessmentKey)
    }
    field <- actual_field[[1]]
    stock_rows <- assessment_rows[
      clean(assessment_rows$StockKeyLabel) == stock, , drop = FALSE
    ]
    numeric_values <- suppressWarnings(as.numeric(as.character(stock_rows[[field]])))
    if (!any(is.finite(numeric_values) & numeric_values > 0)) {
      stop("No positive numerical value in ", field, " for ", stock,
           " assessment ", ov$AssessmentKey,
           ". A named setting alone cannot provide Fref/Bref.")
    }

    # Match a candidate already discovered by the audit. For standard fields
    # there should be exactly one ordinary F/B policy candidate. For unassigned
    # CUSTOM points, add the manually approved F/B candidate as an override.
    matching_base <- policy$StockKeyLabel == stock &
      policy$AssessmentKey == ov$AssessmentKey &
      tolower(policy$reference_field) == tolower(field)
    if (nzchar(ov$reference_name)) {
      matching_base <- matching_base &
        clean(policy$reference_name) == ov$reference_name
    }
    ordinary <- which(matching_base & policy$Indicator == ov$Indicator &
                        policy$source %in% c("Standard", "Custom"))
    unassigned <- which(matching_base & policy$Indicator == "Unassigned" &
                          policy$source == "Custom")

    if (length(ordinary) == 1L) {
      pos <- ordinary[[1]]
      if (policy$review_decision[[pos]] == "EXCLUDE") {
        stop("Mapped setting '", ov$setting_token, "' points to excluded ",
             field, " for ", stock, ". Change the exclusion rule deliberately.")
      }
      if (policy$source[[pos]] == "Custom" &&
          !isTRUE(policy$manual_chart_override[[pos]]) &&
          !nzchar(ov$setting_token)) {
        stop("Custom override already has an F/B chart assignment: ", stock,
             " ", field, ". Remove redundant manual row.")
      }
      # Standard fields are already legitimate F/B candidates, so do not
      # convert them into 'manual_chart_override' (which v3 reserves for Custom).
      policy$review_decision[[pos]] <- "INCLUDE"
      policy$matches_numerator[[pos]] <- "YES"
      type <- if (policy$source[[pos]] == "Standard") {
        "Named token mapped to standard field"
      } else {
        "Existing approved custom F/B candidate"
      }
    } else if (length(ordinary) == 0L && length(unassigned) == 1L) {
      pos <- unassigned[[1]]
      new_entry <- policy[pos, , drop = FALSE]
      new_entry$Indicator <- ov$Indicator
      new_entry$review_decision <- "INCLUDE"
      new_entry$matches_numerator <- "YES"
      new_entry$manual_chart_override <- TRUE
      new_entry$policy_origin <- if (named) {
        "Named SAG setting: explicit F/B custom mapping"
      } else {
        "Stock-specific manual custom override"
      }
      policy <- dplyr::bind_rows(policy, new_entry)
      pos <- nrow(policy)
      n_manual <- n_manual + 1L
      type <- "Manual custom F/B override"
    } else {
      stop("Mapping is ambiguous/missing in policy for ", stock,
           ", assessment ", ov$AssessmentKey, ", field ", field,
           " (", length(ordinary), " assigned, ",
           length(unassigned), " unassigned candidates). Check name/indicator.")
    }

    policy$evidence_notes[[pos]] <- paste(
      unique(c(policy$evidence_notes[[pos]],
               if (named) paste0("Mapped SAG token '",
                                 matching_token$setting_token[[1]], "' (chart ", chart, ")")
               else "Approved stock-specific custom reference override")),
      collapse = " | "
    )
    if (nzchar(ov$source_issue_note)) {
      policy$source_issue_note[[pos]] <- paste(
        unique(c(policy$source_issue_note[[pos]], ov$source_issue_note))[
          nzchar(unique(c(policy$source_issue_note[[pos]], ov$source_issue_note)))
        ], collapse = " | "
      )
    }
    if (named) {
      resolved_named <- dplyr::bind_rows(resolved_named, data.frame(
        Ecoregion = region, StockKeyLabel = stock,
        AssessmentKey = ov$AssessmentKey, SAGChartKey = chart,
        setting_token = matching_token$setting_token[[1]],
        Indicator = ov$Indicator, reference_field = field,
        reference_name = policy$reference_name[[pos]],
        mapping_type = type, stringsAsFactors = FALSE
      ))
      n_named <- n_named + 1L
    }
  }

  # Explicit known source anomalies, approved provisionally.
  issue_nep <- policy$StockKeyLabel == "nep.fu.31" &
    policy$Indicator == "Bref" & policy$reference_name == "F MSY proxy"
  policy$source_issue_note[issue_nep] <-
    "Source SAG B-chart label looks F-related; approved pending DB correction"
  issue_gfb <- policy$StockKeyLabel == "gfb.27.nea" &
    policy$Indicator == "Fref" &
    policy$reference_field == "CustomRefPointValue2" &
    !nzchar(clean(policy$reference_name))
  policy$source_issue_note[issue_gfb] <-
    "Source SAG reference-point name missing; provisionally approved"

  approved_path <- file.path("model/policies",
                             paste0("SAG_policy_", code, "_approved.csv"))
  utils::write.csv(policy, approved_path, row.names = FALSE, na = "")
  read_sag_review_policy_v3(approved_path) # keys/decisions/overrides validated

  logs[[i]] <- data.frame(
    Ecoregion = region, Code = code, n_definitions = nrow(policy),
    n_included = sum(policy$review_decision == "INCLUDE"),
    n_excluded = sum(policy$review_decision == "EXCLUDE"),
    n_unassigned_pending = sum(policy$Indicator == "Unassigned" &
                                 policy$review_decision == "REVIEW"),
    n_manual_overrides = sum(policy$manual_chart_override),
    n_named_tokens_resolved = n_named,
    stringsAsFactors = FALSE
  )
}
utils::write.csv(dplyr::bind_rows(logs), "model/policy_summary.csv",
                 row.names = FALSE)
utils::write.csv(resolved_named, "model/named_settings_resolved.csv",
                 row.names = FALSE, na = "")
message("Named SAG settings mapped: ", nrow(resolved_named),
        "; see model/named_settings_resolved.csv")
