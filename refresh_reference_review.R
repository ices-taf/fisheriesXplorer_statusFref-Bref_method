# Save this file at the TAF repository root and add
# source("refresh_reference_review.R") at the end of output.R.
# Can also be run directly after a completed TAF run.
# In review/unassigned_reference_points.csv, filter review_status to "Needs decision".
# A draft Indicator and its notes are retained on the same reference definition.
# Confirm custom or named mappings in config/manual_reference_overrides.csv.
# Use issue_detail for assigned-policy or missing-source cases.
local({
  read_text <- function(path) {
    utils::read.csv(path, colClasses = "character", check.names = FALSE,
                    stringsAsFactors = FALSE, na.strings = character(),
                    fileEncoding = "UTF-8-BOM")
  }
  combine <- function(x) paste(sort(unique(x[!is.na(x) & nzchar(x)])), collapse = "; ")
  review_path <- "review/unassigned_reference_points.csv"
  config_cols <- c("StockKeyLabel", "AssessmentKey", "reference_field", "reference_name",
                   "Indicator", "source_issue_note", "setting_token", "SAGChartKey")
  field_keys <- config_cols[1:4]
  regional_keys <- c(field_keys, "ecoregion_code")

  regions <- read_text("data/coverage_input.csv")[c("Ecoregion", "Code")]
  stopifnot(nrow(regions) > 0L, !anyDuplicated(regions$Code), !anyDuplicated(regions$Ecoregion))
  policies <- dplyr::bind_rows(lapply(regions$Code, function(code) {
    path <- file.path("model/policies", paste0("SAG_policy_", code, "_approved.csv"))
    dplyr::mutate(read_text(path), ecoregion_code = code)
  }))

  # A manual F/B assignment appends a row alongside the original Unassigned row.
  approvals <- policies |>
    dplyr::filter(source == "Custom", toupper(manual_chart_override) == "TRUE",
                  Indicator %in% c("Fref", "Bref"), review_decision == "INCLUDE",
                  matches_numerator == "YES") |>
    dplyr::group_by(dplyr::across(dplyr::all_of(regional_keys))) |>
    dplyr::summarise(existing_approved_indicator = combine(Indicator), .groups = "drop")
  custom <- policies |>
    dplyr::filter(Indicator == "Unassigned", source == "Custom") |>
    dplyr::left_join(approvals, by = regional_keys) |>
    dplyr::mutate(
      issue_type = "Unassigned custom", setting_token = "", SAGChartKey = "",
      existing_approved_indicator = dplyr::coalesce(existing_approved_indicator, ""),
      review_status = dplyr::case_when(
        review_decision == "EXCLUDE" & existing_approved_indicator == "" ~ "Excluded",
        review_decision != "EXCLUDE" & existing_approved_indicator %in% c("Fref", "Bref") ~
          "Already approved",
        TRUE ~ "Needs decision"
      ),
      issue_detail = dplyr::if_else(
        review_status == "Needs decision" & existing_approved_indicator != "",
        "Conflicting decisions. Check the regional policy.", "")
    )

  # Assigned points with an outstanding eligibility decision need a policy review.
  assigned <- policies |>
    dplyr::filter(Indicator %in% c("Fref", "Bref"), review_decision != "EXCLUDE",
                  review_decision != "INCLUDE" | matches_numerator != "YES") |>
    dplyr::mutate(issue_type = "Assigned reference", setting_token = "",
                  SAGChartKey = dplyr::if_else(Indicator == "Fref", "3", "4"),
                  review_status = "Needs decision", existing_approved_indicator = "",
                  issue_detail = paste("Review the assigned reference in the master policy.",
                                       "Decision", review_decision, "/ numerator", matches_numerator))

  unresolved <- read_text("output/reference_points/unresolved_settings.csv")
  unresolved$ecoregion_code <- regions$Code[match(unresolved$Ecoregion, regions$Ecoregion)]
  if (anyNA(unresolved$ecoregion_code)) stop("Unrecognised ecoregion in unresolved settings.")
  assessment_stocks <- policies[FALSE, c("ecoregion_code", "AssessmentKey", "StockKeyLabel")]
  for (code in unique(unresolved$ecoregion_code)) {
    original <- readRDS(file.path("data/regions", paste0(code, ".rds")))$original
    stock_keys <- unique(original[c("AssessmentKey", "StockKeyLabel")])
    assessment_stocks <- dplyr::bind_rows(assessment_stocks, dplyr::transmute(
      stock_keys, ecoregion_code = code, AssessmentKey = as.character(AssessmentKey),
      StockKeyLabel = as.character(StockKeyLabel)))
  }

  # These diagnostics are keyed by assessment and SAG chart. A stock label can
  # be supplied only if the regional assessment identifies a single stock.
  setting_issues <- function(unresolved, policies, assessment_stocks) {
    stocks <- assessment_stocks |>
      dplyr::filter(!is.na(StockKeyLabel), nzchar(StockKeyLabel)) |>
      dplyr::distinct(ecoregion_code, AssessmentKey, StockKeyLabel) |>
      dplyr::group_by(ecoregion_code, AssessmentKey) |>
      dplyr::summarise(
        stock_labels = paste(sort(StockKeyLabel), collapse = ", "),
        StockKeyLabel = if (dplyr::n() == 1L) dplyr::first(StockKeyLabel) else "",
        .groups = "drop"
      )

    # A policy row describes a positive reference definition. The >4 warning
    # belongs to v2; v3 handles the slot on its actual stock and chart.
    has_positive <- function(x) {
      vapply(strsplit(as.character(x), ";", fixed = TRUE), function(values) {
        num <- suppressWarnings(as.numeric(values))
        any(is.finite(num) & num > 0)
      }, logical(1))
    }
    reviewed_slots <- policies |>
      dplyr::filter(
        source == "Custom", Indicator %in% c("Fref", "Bref"),
        review_decision == "EXCLUDE" |
          (review_decision == "INCLUDE" & matches_numerator == "YES"),
        has_positive(observed_values)
      ) |>
      dplyr::transmute(
        ecoregion_code, AssessmentKey, StockKeyLabel, reference_field,
        SAGChartKey = dplyr::if_else(Indicator == "Fref", "3", "4"),
        legacy = TRUE
      ) |>
      dplyr::distinct()

    unresolved |>
      dplyr::left_join(stocks, by = c("ecoregion_code", "AssessmentKey")) |>
      dplyr::mutate(
        StockKeyLabel = dplyr::coalesce(StockKeyLabel, ""),
        numeric_token = grepl("^[0-9]+$", setting_token),
        reference_field = dplyr::if_else(numeric_token,
          paste0("CustomRefPointValue", sub("^0+(?=[0-9])", "", setting_token, perl = TRUE)), ""),
        legacy = numeric_token & grepl("v2 does not consider custom indices above 4", issue, fixed = TRUE)
      ) |>
      dplyr::anti_join(reviewed_slots, by = c("ecoregion_code", "AssessmentKey",
        "StockKeyLabel", "reference_field", "SAGChartKey", "legacy")) |>
      dplyr::transmute(
        issue_type = dplyr::if_else(numeric_token, "Numeric setting", "Named setting"),
        StockKeyLabel, AssessmentKey, reference_field, reference_name = "",
        setting_token, SAGChartKey, ecoregion_code, review_status = "Needs decision",
        observed_values = "", existing_approved_indicator = "",
        issue_detail = paste0(dplyr::case_when(
          !numeric_token ~ "Identify the SAG field for this chart token and verify its values and units.",
          legacy ~ "Verify the slot and its positive value. A reviewed v3 candidate could not be matched to this stock and chart.",
          TRUE ~ paste0(issue, ". Check the source snapshot before mapping a value.")
        ), dplyr::case_when(
          nzchar(StockKeyLabel) ~ "",
          is.na(stock_labels) ~ " No regional stock found for this assessment. Verify the stock.",
          TRUE ~ paste0(" Multiple regional stock labels match. Verify the component among ", stock_labels, ".")
        ))
      ) |>
      dplyr::distinct()
  }

  settings <- setting_issues(unresolved, policies, assessment_stocks)
  group_keys <- c("issue_type", field_keys, "setting_token", "SAGChartKey")
  review <- dplyr::bind_rows(custom, assigned, settings) |>
    dplyr::group_by(dplyr::across(dplyr::all_of(group_keys))) |>
    dplyr::summarise(
      ecoregion_codes = combine(ecoregion_code), observed_values = combine(observed_values),
      issue_detail = if (dplyr::n_distinct(review_status) > 1L ||
                         dplyr::n_distinct(existing_approved_indicator) > 1L) {
        paste("Check regional decisions.", combine(paste(ecoregion_code, review_status,
                                                        existing_approved_indicator)))
      } else combine(issue_detail),
      review_status = if (dplyr::n_distinct(review_status) == 1L &&
                          dplyr::n_distinct(existing_approved_indicator) == 1L) {
        dplyr::first(review_status)
      } else "Needs decision",
      existing_approved_indicator = combine(existing_approved_indicator),
      .groups = "drop"
    ) |>
    dplyr::mutate(Indicator = "", source_issue_note = "", review_note = "",
                  indicator_to_investigate = dplyr::case_when(
                    SAGChartKey == "3" ~ "Fref", SAGChartKey == "4" ~ "Bref",
                    TRUE ~ ""))
  status_cols <- c("F_AssessmentKey", "F_Year", "FishingPressure", "Fref", "Fref_name",
                   "B_AssessmentKey", "B_Year", "StockSize", "Bref", "Bref_name")
  stocks <- read_text("output/stock_status_unique.csv")[c("StockKeyLabel", status_cols)]
  if (anyDuplicated(stocks$StockKeyLabel)) stop("Repeated StockKeyLabel in stock_status_unique.csv.")
  names(stocks)[-1L] <- paste0("current_", status_cols)
  review <- dplyr::left_join(review, stocks, by = "StockKeyLabel")

  # Stock/assessment/field/name identify custom slots. A named setting's target
  # field and name are reviewer inputs, so use its chart and token for identity.
  identify <- function(x) {
    setting <- x$issue_type %in% c("Named setting", "Numeric setting")
    x$.slot <- ifelse(setting, "", x$reference_field)
    x$.label <- ifelse(setting, "", x$reference_name)
    x
  }
  keys <- c("issue_type", "StockKeyLabel", "AssessmentKey", ".slot", ".label",
            "setting_token", "SAGChartKey")
  review <- identify(review)
  if (file.exists(review_path)) {
    old <- read_text(review_path)
    if (!all(config_cols %in% names(old))) stop("Review file is missing its configuration headers.")
    if (!"issue_type" %in% names(old)) old$issue_type <- rep("Unassigned custom", nrow(old))
    old <- identify(old)
    if (anyDuplicated(old[keys])) stop("Duplicate review keys. Resolve them before refreshing.")
    old$.old_row <- seq_len(nrow(old))
    review <- dplyr::left_join(review, old[c(keys, ".old_row")], by = keys)
    matched <- !is.na(review$.old_row)
    preserve <- intersect(c("Indicator", "source_issue_note", "indicator_to_investigate", "review_note"),
                          names(old))
    for (nm in preserve) {
      take <- matched & nzchar(old[[nm]][review$.old_row], keepNA = TRUE)
      review[[nm]][take] <- old[[nm]][review$.old_row[take]]
    }
    setting <- matched & review$issue_type %in% c("Named setting", "Numeric setting")
    for (nm in c("reference_field", "reference_name")) {
      review[[nm]][setting] <- old[[nm]][review$.old_row[setting]]
    }
    # Keep notes on cases that dropped out, without counting them as live work.
    archived <- dplyr::anti_join(old, review, by = keys)
    has_edits <- nzchar(archived$Indicator) | nzchar(archived$source_issue_note) |
      (archived$issue_type %in% c("Named setting", "Numeric setting") & nzchar(archived$reference_field))
    if ("review_note" %in% names(archived)) has_edits <- has_edits | nzchar(archived$review_note)
    archived <- archived[has_edits, , drop = FALSE]
    archived$review_status <- rep("No longer listed", nrow(archived))
    review <- dplyr::bind_rows(review, archived)
  }
  review <- review |>
    dplyr::select(dplyr::all_of(config_cols), review_status, issue_type, ecoregion_codes,
                  observed_values, existing_approved_indicator, indicator_to_investigate,
                  review_note, issue_detail, dplyr::all_of(names(stocks)[-1L])) |>
    dplyr::arrange(review_status != "Needs decision", review_status, StockKeyLabel,
                   AssessmentKey, issue_type, reference_field, reference_name, SAGChartKey, setting_token)
  dir.create(dirname(review_path), showWarnings = FALSE, recursive = TRUE)
  tmp <- tempfile("refpoint-review-", tmpdir = dirname(review_path))
  tryCatch({
    utils::write.csv(review, tmp, row.names = FALSE, na = "", fileEncoding = "UTF-8")
    if (!file.rename(tmp, review_path)) {
      stop("Could not update ", review_path, ". Close the worksheet and run the refresh again.")
    }
  }, finally = unlink(tmp))
  message(sum(review$review_status == "Needs decision"), " reference-point issues need a decision. See ",
          review_path)
})
