get_ecoregion_acronym <- function(ecoregion) {
  switch(ecoregion,
         "Baltic Sea" = "BtS",
         "Bay of Biscay and the Iberian Coast" = "BI",
         "Celtic Seas" = "CS",
         "Greater North Sea" = "NrS",
         "Norwegian Sea" = "NwS",
         "Icelandic Waters" = "IS",
         "Barents Sea" = "BrS",
         "Greenland Sea" = "GS",
         "Faroes" = "FO",
         "Oceanic Northeast Atlantic" = "ONA",
         "Azores" = "AZ",
         stop("Unknown ecoregion")
  )
}

add_keys <- function(df, stock_label, keys, key_col = "AssessmentKey") {
          template <- df %>%
            dplyr::filter(StockKeyLabel == stock_label) %>%
            dplyr::slice(1)
          if (nrow(template) == 0L) return(df)  # Stock absent from this ecoregion
          additions <- template[rep(1, length(keys)), ]
          additions[[key_col]] <- keys
          dplyr::bind_rows(df, additions)
        }

format_sag <- function(sag, sid){
        # sid <- load_sid(year)
        sid <- dplyr::filter(sid,!is.na(YearOfLastAssessment))
        # sid <- dplyr::select(sid,StockKeyLabel,FisheriesGuild)
        sid <- dplyr::select(sid,AssessmentKey, FisheriesGuild)
        
        df1 <- merge(sag, sid, all.x = T, all.y = F)
        
        df1 <-as.data.frame(df1)
        
        # df1 <- df1[, colSums(is.na(df1)) < nrow(df1)]
        
        df1$FisheriesGuild <- tolower(df1$FisheriesGuild)
        
        # replace the fisheries guild == crustacean with shellfish
        df1$FisheriesGuild[df1$FisheriesGuild == "crustacean"] <- "shellfish"
        
        check <-unique(df1[c("StockKeyLabel", "Purpose")])
        check <- check[duplicated(check$StockKeyLabel),]
        
        out <- dplyr::anti_join(df1, check)

        out$StockKeyLabel <- ifelse(is.na(out$AssessmentComponent) | out$AssessmentComponent == "", out$StockKeyLabel, paste0(out$StockKeyLabel, "_", out$AssessmentComponent))
        out$StockKeyLabel <- gsub("\\s*Substock\\b", "", out$StockKeyLabel, ignore.case = TRUE)
        
        return(out)
}





#' Audit candidate reference points used by add_proxyRefPoints_v2
#'
#' This is a READ-ONLY diagnostic function. It does not change the reference
#' points or the Kobe/status calculations currently used by the app.
#'
#' It retains all recognised standard SAG reference-point fields and all
#' numbered CustomRefPointName/Value fields that exist in the supplied data.
#' Custom point types are inferred ONLY from a numeric selection for SAG
#' chart 3 (Fref) or 4 (Bref); unselected custom points remain Unassigned.
#'
#' The column `considered_in_v2` reproduces the current selection *scope*:
#' original FMSY/MSYBtrigger and settings-selected custom indices 1-4.
#' Within this scope, the lowest positive F reference and highest positive B
#' reference are selected. Ties favour the standard reference point.
#'
#' @return A list with candidates, selected, unresolved_settings, settings,
#'   and discovered_columns data frames.
#' @noRd

audit_sag_reference_points <- function(sag_formatted, sag_settings) {

  need_sag <- c("AssessmentKey", "FMSY", "MSYBtrigger")
  need_settings <- c("AssessmentKey", "settingKey", "SAGChartKey", "settingValue")
  if (length(setdiff(need_sag, names(sag_formatted)))) {
    stop("Missing SAG columns: ",
         paste(setdiff(need_sag, names(sag_formatted)), collapse = ", "))
  }
  if (length(setdiff(need_settings, names(sag_settings)))) {
    stop("Missing settings columns: ",
         paste(setdiff(need_settings, names(sag_settings)), collapse = ", "))
  }

  to_num <- function(z) suppressWarnings(as.numeric(as.character(z)))
  optional_col <- function(name) {
    if (name %in% names(sag_formatted)) {
      as.character(sag_formatted[[name]])
    } else {
      rep(NA_character_, nrow(sag_formatted))
    }
  }

  # Preserve EVERY original assessment row. Ref. values may differ by row/year.
  base <- tibble::tibble(
    .audit_row_id = seq_len(nrow(sag_formatted)),
    AssessmentKey = as.character(sag_formatted$AssessmentKey),
    StockKeyLabel = optional_col("StockKeyLabel"),
    FisheriesGuild = optional_col("FisheriesGuild"),
    AssessmentYear = optional_col("AssessmentYear"),
    Year = optional_col("Year"),
    StockSize = to_num(optional_col("StockSize"))
  )

  settings <- sag_settings |>
    dplyr::filter(
      as.character(settingKey) == "51",
      as.character(SAGChartKey) %in% c("3", "4")
    ) |>
    dplyr::transmute(
      AssessmentKey = as.character(AssessmentKey),
      SAGChartKey = as.integer(as.character(SAGChartKey)),
      setting_token = as.character(settingValue)
    ) |>
    tidyr::separate_rows(setting_token, sep = ",") |>
    dplyr::mutate(setting_token = trimws(setting_token)) |>
    dplyr::filter(
      !is.na(AssessmentKey), nzchar(AssessmentKey),
      !is.na(setting_token), nzchar(setting_token)
    ) |>
    dplyr::distinct()

  # Column names are matched case-insensitively; keep their actual spelling.
  standard_specs <- tibble::tibble(
    canonical = c(
      "FMSY", "Fpa", "FLim", "Fmanagement",
      "MSYBtrigger", "Bpa", "Blim", "Bmanagement"
    ),
    Indicator = c(rep("Fref", 4), rep("Bref", 4))
  )
  standard_specs$field <- names(sag_formatted)[
    match(tolower(standard_specs$canonical), tolower(names(sag_formatted)))
  ]
  standard_specs <- standard_specs |>
    dplyr::filter(!is.na(field)) |>
    dplyr::distinct(field, .keep_all = TRUE)

  standard_rows <- dplyr::bind_rows(lapply(seq_len(nrow(standard_specs)), function(i) {
    s <- standard_specs[i, ]
    base |>
      dplyr::mutate(
        Indicator = s$Indicator[[1]],
        source = "Standard",
        reference_field = s$field[[1]],
        reference_name = s$canonical[[1]],
        reference_notes = NA_character_,
        custom_index = NA_integer_,
        SAGChartKey = NA_integer_,
        setting_selected = FALSE,
        value = to_num(sag_formatted[[s$field[[1]]]])
      )
  }))

  # Discover all numbered custom columns, not just indices 1-4.
  value_fields <- grep("^CustomRefPointValue[0-9]+$", names(sag_formatted), value = TRUE)
  name_fields  <- grep("^CustomRefPointName[0-9]+$", names(sag_formatted), value = TRUE)
  note_fields  <- grep("^CustomRefPointNotes?[0-9]+$", names(sag_formatted), value = TRUE)
  value_ids <- as.integer(sub("^CustomRefPointValue", "", value_fields))
  name_ids <- as.integer(sub("^CustomRefPointName", "", name_fields))
  note_ids <- as.integer(sub("^CustomRefPointNotes?", "", note_fields))
  custom_indices <- sort(unique(c(value_ids, name_ids, note_ids)))

  custom_raw <- dplyr::bind_rows(lapply(custom_indices, function(i) {
    value_field <- paste0("CustomRefPointValue", i)
    name_field <- paste0("CustomRefPointName", i)
    vals <- if (value_field %in% names(sag_formatted)) {
      to_num(sag_formatted[[value_field]])
    } else {
      rep(NA_real_, nrow(base))
    }
    nm <- if (name_field %in% names(sag_formatted)) {
      trimws(as.character(sag_formatted[[name_field]]))
    } else {
      rep(NA_character_, nrow(base))
    }
    nm[!is.na(nm) & !nzchar(nm)] <- NA_character_
    note_field <- intersect(
      paste0(c("CustomRefPointNote", "CustomRefPointNotes"), i),
      names(sag_formatted)
    )
    notes <- if (length(note_field)) {
      as.character(sag_formatted[[note_field[[1]]]])
    } else {
      rep(NA_character_, nrow(base))
    }

    base |>
      dplyr::mutate(
        source = "Custom",
        reference_field = value_field,
        reference_name = nm,
        reference_notes = notes,
        custom_index = as.integer(i),
        value = vals
      )
  }))

  numeric_settings <- settings |>
    dplyr::filter(grepl("^[0-9]+$", setting_token)) |>
    dplyr::mutate(custom_index = as.integer(setting_token)) |>
    dplyr::select(AssessmentKey, SAGChartKey, custom_index) |>
    dplyr::distinct()

  # Join chart 3 and chart 4 separately, so each join is many-to-one
  # (repeated annual assessment rows are preserved without join warnings).
  if (nrow(custom_raw)) {
    assigned <- dplyr::bind_rows(lapply(c(3L, 4L), function(chart) {
      chart_choices <- numeric_settings |>
        dplyr::filter(SAGChartKey == chart) |>
        dplyr::select(AssessmentKey, custom_index) |>
        dplyr::distinct()
      custom_raw |>
        dplyr::inner_join(
          chart_choices,
          by = c("AssessmentKey", "custom_index")
        ) |>
        dplyr::mutate(
          SAGChartKey = as.integer(chart),
          Indicator = if (chart == 3L) "Fref" else "Bref",
          setting_selected = TRUE
        )
    }))
    unassigned <- custom_raw |>
      dplyr::anti_join(
        numeric_settings |>
          dplyr::select(AssessmentKey, custom_index) |>
          dplyr::distinct(),
        by = c("AssessmentKey", "custom_index")
      ) |>
      dplyr::mutate(
        SAGChartKey = NA_integer_,
        Indicator = "Unassigned",
        setting_selected = FALSE
      )
    custom_rows <- dplyr::bind_rows(assigned, unassigned)
  } else {
    custom_rows <- custom_raw |>
      dplyr::mutate(
        SAGChartKey = integer(0),
        Indicator = character(0),
        setting_selected = logical(0)
      )
  }

  candidates <- dplyr::bind_rows(standard_rows, custom_rows) |>
    dplyr::mutate(
      valid_positive = is.finite(value) & value > 0,
      considered_in_v2 = dplyr::case_when(
        source == "Standard" &
          tolower(reference_field) %in% c("fmsy", "msybtrigger") ~ TRUE,
        source == "Custom" & setting_selected &
          custom_index %in% 1:4 ~ TRUE,
        TRUE ~ FALSE
      ),
      .candidate_id = paste(
        .audit_row_id, Indicator, reference_field,
        dplyr::coalesce(as.character(SAGChartKey), "none"), sep = ":"
      )
    )

  # Exactly reproduce the selection logic in add_proxyRefPoints_v2:
  # use only positive values; minimum F, maximum B; original wins ties.
  picked <- candidates |>
    dplyr::filter(
      considered_in_v2, valid_positive,
      Indicator %in% c("Fref", "Bref")
    ) |>
    dplyr::mutate(
      .rank = dplyr::if_else(Indicator == "Fref", value, -value),
      .tie_original = dplyr::if_else(source == "Standard", 0L, 1L)
    ) |>
    dplyr::arrange(
      .audit_row_id, Indicator, .rank, .tie_original, custom_index
    ) |>
    dplyr::group_by(.audit_row_id, Indicator) |>
    dplyr::slice_head(n = 1L) |>
    dplyr::ungroup()

  candidates <- candidates |>
    dplyr::mutate(
      selected_in_v2 = .candidate_id %in% picked$.candidate_id,
      audit_note = dplyr::case_when(
        selected_in_v2 ~ "Selected in current v2",
        !valid_positive ~ "Missing, non-finite, or nonpositive value",
        considered_in_v2 ~ "Compared in v2; another point selected",
        source == "Standard" ~ "Available standard point, not compared in v2",
        source == "Custom" & !setting_selected ~
          "No numeric chart 3/4 setting; type not established",
        source == "Custom" & custom_index > 4L ~
          "Selected in settings but index exceeds v2's 1-4 limit",
        TRUE ~ "Not compared in v2"
      )
    )

  picked_wide <- picked |>
    dplyr::transmute(
      .audit_row_id, Indicator,
      value, reference_name, reference_field, source
    ) |>
    tidyr::pivot_wider(
      id_cols = .audit_row_id,
      names_from = Indicator,
      values_from = c(value, reference_name, reference_field, source),
      names_glue = "{Indicator}_{.value}"
    )

  selected <- base |>
    dplyr::left_join(picked_wide, by = ".audit_row_id")

  # Always expose both types, including for assessments without valid points.
  for (nm in c("Fref_value", "Bref_value")) {
    if (!nm %in% names(selected)) selected[[nm]] <- NA_real_
  }
  for (nm in c(
    "Fref_reference_name", "Bref_reference_name",
    "Fref_reference_field", "Bref_reference_field",
    "Fref_source", "Bref_source"
  )) {
    if (!nm %in% names(selected)) selected[[nm]] <- NA_character_
  }

  # Keep a human-readable list of every POSITIVE candidate compared by v2.
  detail <- candidates |>
    dplyr::filter(
      considered_in_v2, valid_positive,
      Indicator %in% c("Fref", "Bref")
    ) |>
    dplyr::mutate(
      candidate_description = paste0(
        dplyr::coalesce(reference_name, reference_field),
        " [", reference_field, "] = ", signif(value, 8)
      )
    ) |>
    dplyr::group_by(.audit_row_id, Indicator) |>
    dplyr::summarise(
      candidate_count = dplyr::n(),
      candidate_list = paste(candidate_description, collapse = " | "),
      .groups = "drop"
    )

  for (indicator in c("Fref", "Bref")) {
    sub <- detail |>
      dplyr::filter(Indicator == indicator) |>
      dplyr::select(.audit_row_id, candidate_count, candidate_list)
    names(sub)[2:3] <- paste0(indicator, "_", names(sub)[2:3])
    selected <- dplyr::left_join(selected, sub, by = ".audit_row_id")
  }

  unresolved_settings <- settings |>
    dplyr::mutate(
      custom_index = suppressWarnings(as.integer(setting_token)),
      issue = dplyr::case_when(
        !grepl("^[0-9]+$", setting_token) ~
          "Named token: not mapped to a numeric reference in v2",
        !(custom_index %in% custom_indices) ~
          "Numeric index not present in downloaded custom fields",
        custom_index > 4L ~
          "Index present but v2 does not consider custom indices above 4",
        TRUE ~ NA_character_
      )
    ) |>
    dplyr::filter(!is.na(issue))

  if (nrow(selected) != nrow(sag_formatted)) {
    stop("Audit row count changed unexpectedly.")
  }

  list(
    candidates = candidates |>
      dplyr::select(
        .audit_row_id, AssessmentKey, StockKeyLabel, FisheriesGuild,
        AssessmentYear, Year, StockSize, Indicator, source, reference_field, reference_name,
        custom_index, SAGChartKey, setting_selected, value,
        valid_positive, considered_in_v2, selected_in_v2, audit_note
      ),
    selected = selected |>
      dplyr::relocate(
        .audit_row_id, AssessmentKey, StockKeyLabel, FisheriesGuild,
        AssessmentYear, Year, Fref_value, Fref_reference_name, Fref_reference_field, Fref_source,
        Bref_value, Bref_reference_name, Bref_reference_field, Bref_source
      ),
    unresolved_settings = unresolved_settings,
    settings = settings,
    discovered_columns = tibble::tibble(
      reference_field = c(standard_specs$field, value_fields, name_fields, note_fields),
      source = c(
        rep("Standard", nrow(standard_specs)),
        rep("Custom value", length(value_fields)),
        rep("Custom name", length(name_fields)),
        rep("Custom notes", length(note_fields))
      )
    )
  )
}





# Select the current B observation and its reference from the same assessment row.
# Prefer the latest usable pair in the latest assessment, up to its assessment year.
# If every biomass observation is unavailable/invalid, retain the latest known
# positive reference so its name and provenance can accompany a GREY status.
select_current_biomass_row <- function(x, reference_column) {
  to_num <- function(z) suppressWarnings(as.numeric(as.character(z)))
  original_columns <- names(x)

  x |>
    dplyr::mutate(
      .B_year = to_num(Year),
      .B_assessment_year = to_num(AssessmentYear),
      .B_stock_size = to_num(StockSize),
      .B_reference = to_num(.data[[reference_column]])
    ) |>
    dplyr::group_by(StockKeyLabel) |>
    dplyr::mutate(
      .B_latest_assessment = if (all(!is.finite(.B_assessment_year))) {
        NA_real_
      } else {
        max(.B_assessment_year[is.finite(.B_assessment_year)])
      }
    ) |>
    dplyr::filter(
      is.finite(.B_latest_assessment),
      .B_assessment_year == .B_latest_assessment,
      is.finite(.B_year), .B_year <= .B_latest_assessment,
      is.finite(.B_reference), .B_reference > 0
    ) |>
    dplyr::mutate(
      .B_usable = is.finite(.B_stock_size) & .B_stock_size >= 0 &
        is.finite(.B_stock_size / .B_reference)
    ) |>
    dplyr::arrange(StockKeyLabel, dplyr::desc(.B_usable), dplyr::desc(.B_year)) |>
    dplyr::slice_head(n = 1L) |>
    dplyr::ungroup() |>
    dplyr::select(dplyr::all_of(original_columns))
}


#' Reduce the audit to ONE ROW PER STOCK, following the intended year rules
#' in stockstatus_CLD_current_proxy(): F from assessment year - 1; B from
#' the latest usable biomass/reference pair in the latest assessment.
#' This is for provenance review, not a replacement for stockstatus.
#' @noRd
audit_sag_refpoints_by_stock <- function(audit) {
  d <- audit$selected
  needed <- c("StockKeyLabel", "FisheriesGuild", "AssessmentYear", "Year",
              "StockSize", "Fref_value", "Bref_value")
  missing <- setdiff(needed, names(d))
  if (length(missing)) stop("Audit selected table lacks: ", paste(missing, collapse = ", "))

  num <- function(z) suppressWarnings(as.numeric(as.character(z)))
  d <- d |>
    dplyr::mutate(
      .year_num = num(Year),
      .assessment_year_num = num(AssessmentYear)
    ) |>
    dplyr::group_by(StockKeyLabel) |>
    dplyr::mutate(
      .AY_latest = if (all(!is.finite(.assessment_year_num))) {
        NA_real_
      } else {
        max(.assessment_year_num[is.finite(.assessment_year_num)])
      }
    ) |>
    dplyr::ungroup()

  f <- d |>
    dplyr::filter(is.finite(.AY_latest), .year_num == .AY_latest - 1) |>
    dplyr::arrange(StockKeyLabel, dplyr::desc(.year_num)) |>
    dplyr::group_by(StockKeyLabel) |>
    dplyr::slice_head(n = 1L) |>
    dplyr::ungroup() |>
    dplyr::transmute(
      StockKeyLabel, FisheriesGuild,
      F_AssessmentKey = AssessmentKey,
      F_Year = .year_num,
      Fref = Fref_value,
      Fref_name = dplyr::coalesce(Fref_reference_name, Fref_reference_field),
      Fref_field = Fref_reference_field,
      Fref_source = Fref_source,
      Fref_candidates = Fref_candidate_list
    )

  b <- select_current_biomass_row(d, "Bref_value") |>
    dplyr::transmute(
      StockKeyLabel, FisheriesGuild,
      B_AssessmentKey = AssessmentKey,
      B_Year = .year_num,
      Bref = Bref_value,
      Bref_name = dplyr::coalesce(Bref_reference_name, Bref_reference_field),
      Bref_field = Bref_reference_field,
      Bref_source = Bref_source,
      Bref_candidates = Bref_candidate_list
    )

  dplyr::full_join(f, b, by = c("StockKeyLabel", "FisheriesGuild")) |>
    dplyr::arrange(StockKeyLabel)
}



# Prepare a NEW, ecoregion-specific review policy for add_proxyRefPoints_v3().
# Dependencies: dplyr, tidyr, audit_sag_reference_points.R, and (if reusing
# approvals) add_proxyRefPoints_v3_reviewed.R.
#
# A candidate is NOT approved simply because it appears in SAG. New candidates
# are REVIEW / UNKNOWN until examined. Prior decisions are reused ONLY when
# the exact stock, assessment, indicator, source field and name all match.
# Existing manual chart overrides are copied ONLY if the exact unassigned
# custom SAG candidate is present in this new snapshot.

make_sag_refpoint_policy <- function(
  sag_formatted,
  sag_settings,
  output_file,
  previous_policy = NULL
) {
  if (!exists("audit_sag_reference_points", mode = "function")) {
    stop("First source audit_sag_reference_points.R")
  }
  if (!is.null(previous_policy) &&
      !exists("read_sag_review_policy_v3", mode = "function")) {
    stop("To reuse an old policy, first source add_proxyRefPoints_v3_reviewed.R")
  }
  if (!is.character(output_file) || length(output_file) != 1L ||
      is.na(output_file) || !nzchar(output_file)) {
    stop("output_file must be a single nonempty file path")
  }
  if (!is.null(previous_policy) && is.character(previous_policy) &&
      length(previous_policy) == 1L &&
      identical(normalizePath(output_file, mustWork = FALSE),
                normalizePath(previous_policy, mustWork = FALSE))) {
    stop("Choose a NEW output path; do not overwrite the original approved policy.")
  }

  keys <- c("AssessmentKey", "StockKeyLabel", "Indicator", "source",
            "reference_field", "reference_name")
  policy_fields <- c("review_decision", "matches_numerator", "unit_scale",
                     "evidence_notes", "manual_chart_override",
                     "source_issue_note")

  tidy_text <- function(x) {
    x <- trimws(as.character(x))
    x[is.na(x)] <- ""
    x
  }
  values_text <- function(x) {
    v <- sort(unique(signif(x[is.finite(x)], 8)))
    if (!length(v)) return("")
    more <- if (length(v) > 8L) paste0("; ... (", length(v), " values)") else ""
    paste0(paste(utils::head(v, 8L), collapse = "; "), more)
  }
  years_text <- function(x) {
    y <- sort(unique(tidy_text(x)))
    y <- y[nzchar(y)]
    paste(y, collapse = "; ")
  }
  charts_text <- function(x) {
    z <- sort(unique(as.integer(x[!is.na(x)])))
    if (!length(z)) "" else paste(z, collapse = "; ")
  }

  audit <- audit_sag_reference_points(sag_formatted, sag_settings)
  positive <- audit$candidates |>
    dplyr::mutate(
      dplyr::across(dplyr::all_of(keys), tidy_text),
      value = suppressWarnings(as.numeric(as.character(value)))
    ) |>
    dplyr::filter(is.finite(value), value > 0)

  # One policy row PER reference definition, not one per data year.
  # The trailing audit columns are informative; v3 ignores them when reading.
  base <- positive |>
    dplyr::group_by(dplyr::across(dplyr::all_of(keys))) |>
    dplyr::summarise(
      SAG_chart = charts_text(SAGChartKey),
      selected_in_SAG = any(setting_selected %in% TRUE),
      compared_in_v2 = any(considered_in_v2 %in% TRUE),
      winner_in_v2 = any(selected_in_v2 %in% TRUE),
      years_in_SAG = years_text(Year),
      observed_values = values_text(value),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      review_decision = "REVIEW",
      matches_numerator = "UNKNOWN",
      unit_scale = "",
      evidence_notes = "",
      manual_chart_override = FALSE,
      source_issue_note = "",
      policy_origin = "New candidate — review required"
    )

  if (!is.null(previous_policy)) {
    old <- read_sag_review_policy_v3(previous_policy)

    # Only copy prior non-manual approvals for the EXACT same policy key.
    # An approval for a different assessment/stock is not transferable.
    old_ordinary <- old |>
      dplyr::filter(!manual_chart_override) |>
      dplyr::select(dplyr::all_of(c(keys, policy_fields)))

    base <- dplyr::left_join(base, old_ordinary, by = keys,
                             suffix = c("", ".previous"))
    prior_found <- !is.na(base$review_decision.previous)
    base$policy_origin[prior_found] <- "Exact match — prior review reused"
    for (col in policy_fields) {
      prev_col <- paste0(col, ".previous")
      base[[col]] <- dplyr::coalesce(base[[prev_col]], base[[col]])
      base[[prev_col]] <- NULL
    }

    # A prior manual F/B override is meaningful only if the SAME candidate
    # exists as an Unassigned custom point in the current SAG inventory.
    match_without_axis <- setdiff(keys, "Indicator")
    current_unassigned <- base |>
      dplyr::filter(Indicator == "Unassigned", source == "Custom") |>
      dplyr::select(dplyr::all_of(c(match_without_axis, "SAG_chart",
                            "selected_in_SAG", "compared_in_v2",
                            "winner_in_v2", "years_in_SAG", "observed_values")))
    approved_overrides <- old |>
      dplyr::filter(manual_chart_override, source == "Custom",
                    Indicator %in% c("Fref", "Bref"),
                    review_decision == "INCLUDE", matches_numerator == "YES") |>
      dplyr::semi_join(current_unassigned, by = match_without_axis)

    if (nrow(approved_overrides)) {
      manual_rows <- approved_overrides |>
        dplyr::left_join(current_unassigned, by = match_without_axis) |>
        dplyr::mutate(policy_origin =
                        "Exact unassigned candidate — prior manual override reused")
      base <- dplyr::bind_rows(base, manual_rows)
    }
  }

  # Put the 12 columns expected by v3 first; additional columns help review.
  output <- base |>
    dplyr::select(
      StockKeyLabel, AssessmentKey, Indicator,
      reference_field, reference_name, source,
      review_decision, matches_numerator,
      unit_scale, evidence_notes, manual_chart_override, source_issue_note,
      dplyr::everything()
    ) |>
    dplyr::arrange(StockKeyLabel, AssessmentKey, Indicator, reference_field,
                   reference_name, dplyr::desc(manual_chart_override))

  # The policy must never contain duplicate keys: v3 intentionally rejects them.
  if (anyDuplicated(output[keys])) {
    stop("Internal error: duplicate policy keys. Check manually transferred overrides.")
  }

  dir.create(dirname(output_file), recursive = TRUE, showWarnings = FALSE)
  utils::write.csv(output, output_file, row.names = FALSE, na = "")

  unresolved_path <- sub("\\.csv$", "_unresolved_settings.csv", output_file,
                         ignore.case = TRUE)
  if (identical(unresolved_path, output_file)) {
    unresolved_path <- paste0(output_file, "_unresolved_settings.csv")
  }
  utils::write.csv(audit$unresolved_settings, unresolved_path,
                   row.names = FALSE, na = "")

  message("Saved draft policy: ", output_file)
  message("Definitions: ", nrow(output),
          "; pending: ", sum(output$review_decision == "REVIEW"),
          "; prior manual overrides copied: ",
          sum(output$manual_chart_override %in% TRUE))
  message("Also saved unresolved settings: ", unresolved_path)
  invisible(output)
}



# Prepare a NEW, ecoregion-specific review policy for add_proxyRefPoints_v3().
# Dependencies: dplyr, tidyr, audit_sag_reference_points.R, and (if reusing
# approvals) add_proxyRefPoints_v3_reviewed.R.
#
# A candidate is NOT approved simply because it appears in SAG. New candidates
# are REVIEW / UNKNOWN until examined. Prior decisions are reused ONLY when
# the exact stock, assessment, indicator, source field and name all match.
# Existing manual chart overrides are copied ONLY if the exact unassigned
# custom SAG candidate is present in this new snapshot.

make_sag_refpoint_policy <- function(
  sag_formatted,
  sag_settings,
  output_file,
  previous_policy = NULL
) {
  if (!exists("audit_sag_reference_points", mode = "function")) {
    stop("First source audit_sag_reference_points.R")
  }
  if (!is.null(previous_policy) &&
      !exists("read_sag_review_policy_v3", mode = "function")) {
    stop("To reuse an old policy, first source add_proxyRefPoints_v3_reviewed.R")
  }
  if (!is.character(output_file) || length(output_file) != 1L ||
      is.na(output_file) || !nzchar(output_file)) {
    stop("output_file must be a single nonempty file path")
  }
  if (!is.null(previous_policy) && is.character(previous_policy) &&
      length(previous_policy) == 1L &&
      identical(normalizePath(output_file, mustWork = FALSE),
                normalizePath(previous_policy, mustWork = FALSE))) {
    stop("Choose a NEW output path; do not overwrite the original approved policy.")
  }

  keys <- c("AssessmentKey", "StockKeyLabel", "Indicator", "source",
            "reference_field", "reference_name")
  policy_fields <- c("review_decision", "matches_numerator", "unit_scale",
                     "evidence_notes", "manual_chart_override",
                     "source_issue_note")

  tidy_text <- function(x) {
    x <- trimws(as.character(x))
    x[is.na(x)] <- ""
    x
  }
  values_text <- function(x) {
    v <- sort(unique(signif(x[is.finite(x)], 8)))
    if (!length(v)) return("")
    more <- if (length(v) > 8L) paste0("; ... (", length(v), " values)") else ""
    paste0(paste(utils::head(v, 8L), collapse = "; "), more)
  }
  years_text <- function(x) {
    y <- sort(unique(tidy_text(x)))
    y <- y[nzchar(y)]
    paste(y, collapse = "; ")
  }
  charts_text <- function(x) {
    z <- sort(unique(as.integer(x[!is.na(x)])))
    if (!length(z)) "" else paste(z, collapse = "; ")
  }

  audit <- audit_sag_reference_points(sag_formatted, sag_settings)
  positive <- audit$candidates |>
    dplyr::mutate(
      dplyr::across(dplyr::all_of(keys), tidy_text),
      value = suppressWarnings(as.numeric(as.character(value)))
    ) |>
    dplyr::filter(is.finite(value), value > 0)

  # One policy row PER reference definition, not one per data year.
  # The trailing audit columns are informative; v3 ignores them when reading.
  base <- positive |>
    dplyr::group_by(dplyr::across(dplyr::all_of(keys))) |>
    dplyr::summarise(
      SAG_chart = charts_text(SAGChartKey),
      selected_in_SAG = any(setting_selected %in% TRUE),
      compared_in_v2 = any(considered_in_v2 %in% TRUE),
      winner_in_v2 = any(selected_in_v2 %in% TRUE),
      years_in_SAG = years_text(Year),
      observed_values = values_text(value),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      review_decision = "REVIEW",
      matches_numerator = "UNKNOWN",
      unit_scale = "",
      evidence_notes = "",
      manual_chart_override = FALSE,
      source_issue_note = "",
      policy_origin = "New candidate — review required"
    )

  if (!is.null(previous_policy)) {
    old <- read_sag_review_policy_v3(previous_policy)

    # Only copy prior non-manual approvals for the EXACT same policy key.
    # An approval for a different assessment/stock is not transferable.
    old_ordinary <- old |>
      dplyr::filter(!manual_chart_override) |>
      dplyr::select(dplyr::all_of(c(keys, policy_fields)))

    base <- dplyr::left_join(base, old_ordinary, by = keys,
                             suffix = c("", ".previous"))
    prior_found <- !is.na(base$review_decision.previous)
    base$policy_origin[prior_found] <- "Exact match — prior review reused"
    for (col in policy_fields) {
      prev_col <- paste0(col, ".previous")
      base[[col]] <- dplyr::coalesce(base[[prev_col]], base[[col]])
      base[[prev_col]] <- NULL
    }

    # A prior manual F/B override is meaningful only if the SAME candidate
    # exists as an Unassigned custom point in the current SAG inventory.
    match_without_axis <- setdiff(keys, "Indicator")
    current_unassigned <- base |>
      dplyr::filter(Indicator == "Unassigned", source == "Custom") |>
      dplyr::select(dplyr::all_of(c(match_without_axis, "SAG_chart",
                            "selected_in_SAG", "compared_in_v2",
                            "winner_in_v2", "years_in_SAG", "observed_values")))
    approved_overrides <- old |>
      dplyr::filter(manual_chart_override, source == "Custom",
                    Indicator %in% c("Fref", "Bref"),
                    review_decision == "INCLUDE", matches_numerator == "YES") |>
      dplyr::semi_join(current_unassigned, by = match_without_axis)

    if (nrow(approved_overrides)) {
      manual_rows <- approved_overrides |>
        dplyr::left_join(current_unassigned, by = match_without_axis) |>
        dplyr::mutate(policy_origin =
                        "Exact unassigned candidate — prior manual override reused")
      base <- dplyr::bind_rows(base, manual_rows)
    }
  }

  # Put the 12 columns expected by v3 first; additional columns help review.
  output <- base |>
    dplyr::select(
      StockKeyLabel, AssessmentKey, Indicator,
      reference_field, reference_name, source,
      review_decision, matches_numerator,
      unit_scale, evidence_notes, manual_chart_override, source_issue_note,
      dplyr::everything()
    ) |>
    dplyr::arrange(StockKeyLabel, AssessmentKey, Indicator, reference_field,
                   reference_name, dplyr::desc(manual_chart_override))

  # The policy must never contain duplicate keys: v3 intentionally rejects them.
  if (anyDuplicated(output[keys])) {
    stop("Internal error: duplicate policy keys. Check manually transferred overrides.")
  }

  dir.create(dirname(output_file), recursive = TRUE, showWarnings = FALSE)
  utils::write.csv(output, output_file, row.names = FALSE, na = "")

  unresolved_path <- sub("\\.csv$", "_unresolved_settings.csv", output_file,
                         ignore.case = TRUE)
  if (identical(unresolved_path, output_file)) {
    unresolved_path <- paste0(output_file, "_unresolved_settings.csv")
  }
  utils::write.csv(audit$unresolved_settings, unresolved_path,
                   row.names = FALSE, na = "")

  message("Saved draft policy: ", output_file)
  message("Definitions: ", nrow(output),
          "; pending: ", sum(output$review_decision == "REVIEW"),
          "; prior manual overrides copied: ",
          sum(output$manual_chart_override %in% TRUE))
  message("Also saved unresolved settings: ", unresolved_path)
  invisible(output)
}




# Reviewed precautionary reference-point selection for ICES SAG.
#
# REQUIREMENTS:
#   source("audit_sag_reference_points.R")   # provided separately
#   library(dplyr)
#   library(tidyr)
#
# The accompanying policy CSV was extracted from the user's completed Excel
# workbook on 2026-10-28. Approvals are user decisions, NOT independent
# verification of units or biological interpretation.
#
# IMPORTANT:
# - Original FMSY/MSYBtrigger are kept in *_original columns.
# - Standard F and B references are kept on their respective axes.
# - Settings-selected CUSTOM points use SAGChartKey 3 for F, 4 for B.
# - The two explicitly reviewer-approved, not-SAG-selected F custom points
#   (rjn.27.8c and sol.27.8c9a) are MANUAL CHART OVERRIDES, never inferred
#   automatically from their names.
# - nep.fu.31's chart-4 point named 'F MSY proxy' is provisionally approved;
#   source_issue_note retains the known anomaly without correcting its label.
# - Other named settings, e.g. BMGT_lower/BMGT_upper, are not silently mapped.
# - Nonpositive or non-finite values are ineligible.
# - Ties favour the original reference, then other standard points, then custom.
#
# This script does not replace add_proxyRefPoints_v2() or the plotting functions.

read_sag_review_policy_v3 <- function(path) {
  if (is.data.frame(path)) {
    policy <- path
  } else {
    policy <- utils::read.csv(
      path, stringsAsFactors = FALSE, check.names = FALSE,
      fileEncoding = "UTF-8", na.strings = c("NA")
    )
  }

  required <- c(
    "StockKeyLabel", "AssessmentKey", "Indicator", "reference_field",
    "reference_name", "source", "review_decision", "matches_numerator",
    "manual_chart_override", "unit_scale", "evidence_notes", "source_issue_note"
  )
  missing <- setdiff(required, names(policy))
  if (length(missing)) {
    stop("Policy is missing columns: ", paste(missing, collapse = ", "))
  }

  normalise_text <- function(z) {
    z <- trimws(as.character(z))
    z[is.na(z)] <- ""
    z
  }
  for (nm in c("StockKeyLabel", "AssessmentKey", "Indicator",
               "reference_field", "reference_name", "source",
               "review_decision", "matches_numerator", "unit_scale",
               "evidence_notes", "source_issue_note")) {
    policy[[nm]] <- normalise_text(policy[[nm]])
  }
  policy$review_decision <- toupper(policy$review_decision)
  policy$matches_numerator <- toupper(policy$matches_numerator)
  policy$manual_chart_override <- toupper(normalise_text(
    policy$manual_chart_override
  )) %in% c("TRUE", "T", "1", "YES")

  allowed_indicators <- c("Fref", "Bref", "Unassigned")
  if (any(!policy$Indicator %in% allowed_indicators)) {
    stop("Policy contains unexpected Indicator values.")
  }
  if (any(!policy$review_decision %in% c("INCLUDE", "EXCLUDE", "REVIEW"))) {
    stop("Policy review_decision must be INCLUDE, EXCLUDE, or REVIEW.")
  }
  if (any(!policy$matches_numerator %in% c("YES", "NO", "UNKNOWN"))) {
    stop("Policy matches_numerator must be YES, NO, or UNKNOWN.")
  }
  if (any(policy$manual_chart_override &
          (policy$source != "Custom" | !policy$Indicator %in% c("Fref", "Bref")))) {
    stop("Only approved F/B CUSTOM references can be manual chart overrides.")
  }

  join_keys <- c(
    "AssessmentKey", "StockKeyLabel", "Indicator", "source",
    "reference_field", "reference_name"
  )
  duplicates <- policy |>
    dplyr::group_by(dplyr::across(dplyr::all_of(join_keys))) |>
    dplyr::summarise(n = dplyr::n(), .groups = "drop") |>
    dplyr::filter(n > 1L)
  if (nrow(duplicates)) {
    stop("Policy has duplicate keys. Resolve before using it for selection.")
  }
  policy
}

add_proxyRefPoints_v3 <- function(
    sag_formatted,
    sag_settings,
    review_policy = "SAG_reference_point_policy_approved.csv",
    return_audit = FALSE,
    allow_manual_overrides = TRUE,
    strict_current_review = TRUE
) {
  if (!exists("audit_sag_reference_points", mode = "function") ||
      !exists("audit_sag_refpoints_by_stock", mode = "function")) {
    stop("First source audit_sag_reference_points.R")
  }
  if (!is.data.frame(sag_formatted)) stop("sag_formatted must be a data frame.")
  if (any(c("Fref_value", "Bref_value") %in% names(sag_formatted))) {
    stop("Pass the ORIGINAL format_sag(sag, sid), not output from v3.")
  }

  policy <- read_sag_review_policy_v3(review_policy)
  audit <- audit_sag_reference_points(sag_formatted, sag_settings)
  to_num <- function(z) suppressWarnings(as.numeric(as.character(z)))
  normalise_text <- function(z) {
    z <- trimws(as.character(z))
    z[is.na(z)] <- ""
    z
  }
  keys <- c(
    "AssessmentKey", "StockKeyLabel", "Indicator", "source",
    "reference_field", "reference_name"
  )

  base <- audit$candidates |>
    dplyr::mutate(
      AssessmentKey = as.character(AssessmentKey),
      StockKeyLabel = normalise_text(StockKeyLabel),
      reference_name = normalise_text(reference_name),
      source = as.character(source),
      reference_field = as.character(reference_field),
      Indicator = as.character(Indicator),
      value = to_num(value)
    )
  policy_cols <- c(keys, "review_decision", "matches_numerator",
                   "manual_chart_override", "unit_scale", "evidence_notes",
                   "source_issue_note")
  policy <- policy |> dplyr::select(dplyr::all_of(policy_cols))

  # Every standard point has an intrinsic F/B dimension. Every SAG-selected
  # custom point has its F/B dimension from chart 3/4 in the audit helper.
  assigned <- base |>
    dplyr::filter(Indicator %in% c("Fref", "Bref")) |>
    dplyr::left_join(policy, by = keys) |>
    dplyr::mutate(manual_chart_override = FALSE)

  # A custom point without a numeric chart-3/4 setting cannot be assigned
  # to F or B by its NAME alone. Include it only where the user explicitly
  # approved both its axis and an override in the review policy.
  manual_policy <- policy |>
    dplyr::filter(
      manual_chart_override, Indicator %in% c("Fref", "Bref"),
      review_decision == "INCLUDE", matches_numerator == "YES"
    )
  unassigned <- base |>
    dplyr::filter(Indicator == "Unassigned", source == "Custom")

  if (isTRUE(allow_manual_overrides)) {
    manual <- unassigned |>
      dplyr::select(-Indicator) |>
      dplyr::inner_join(manual_policy, by = setdiff(keys, "Indicator"))
    found_manual <- manual |>
      dplyr::select(dplyr::all_of(keys)) |>
      dplyr::distinct()
    missing_manual <- dplyr::anti_join(manual_policy, found_manual, by = keys)
    if (nrow(missing_manual)) {
      stop("A manual override in the approved review has no matching SAG ",
           "candidate. The snapshot may have changed; review before running v3.")
    }
  } else {
    manual <- assigned[FALSE, , drop = FALSE]
  }

  # Retain the rest of the unassigned custom inventory for inspection,
  # including the explicitly excluded historical eel reference points.
  unassigned_unmapped <- unassigned |>
    dplyr::left_join(
      policy |> dplyr::filter(Indicator == "Unassigned"),
      by = keys
    ) |>
    dplyr::mutate(manual_chart_override = FALSE)

  candidates <- dplyr::bind_rows(assigned, manual, unassigned_unmapped) |>
    dplyr::mutate(
      # Only manual rows explicitly approved for an axis can be used without
      # a SAG chart assignment. Otherwise the SAG chart is always honoured.
      chart_compatible = dplyr::case_when(
        Indicator == "Unassigned" ~ FALSE,
        source == "Standard" ~ TRUE,
        manual_chart_override %in% TRUE ~ TRUE,
        Indicator == "Fref" ~ SAGChartKey == 3L,
        Indicator == "Bref" ~ SAGChartKey == 4L,
        TRUE ~ FALSE
      ),
      chart_compatible = dplyr::coalesce(chart_compatible, FALSE),
      eligibility = dplyr::case_when(
        Indicator == "Unassigned" ~ "Unassigned to F/B",
        !is.finite(value) | value <= 0 ~ "Invalid/nonpositive value",
        !chart_compatible ~ "SAG chart incompatible",
        is.na(review_decision) ~ "Unreviewed",
        review_decision == "EXCLUDE" ~ "Excluded by reviewer",
        review_decision != "INCLUDE" ~ "Review pending",
        is.na(matches_numerator) | matches_numerator != "YES" ~
          "Numerator compatibility not approved",
        TRUE ~ "Eligible"
      ),
      eligible_v3 = eligibility == "Eligible",
      choice_id = paste(
        .audit_row_id, Indicator, source, reference_field, reference_name,
        dplyr::coalesce(as.character(SAGChartKey), "manual_or_none"),
        sep = "::"
      )
    )

  # Prevent silent use of an old review on a changed current SAG snapshot.
  # Historical rows remain in the audit; the approved field-level decisions
  # are carried across years for the same assessment and reference name.
  current_check <- candidates |>
    dplyr::filter(Indicator %in% c("Fref", "Bref")) |>
    dplyr::mutate(
      .AY = to_num(AssessmentYear), .Y = to_num(Year)
    ) |>
    dplyr::group_by(StockKeyLabel) |>
    dplyr::mutate(
      .AY_latest = if (all(!is.finite(.AY))) NA_real_ else max(.AY[is.finite(.AY)])
    ) |>
    dplyr::ungroup()

  # Review the potential current B year, including pending candidates.
  # This covers a fallback without bringing unrelated historical years into
  # the current-status review or silently skipping a newer unreviewed pair.
  b_review_years <- current_check |>
    dplyr::filter(
      Indicator == "Bref", .AY == .AY_latest, chart_compatible,
      eligibility %in% c("Eligible", "Unreviewed", "Review pending")
    ) |>
    select_current_biomass_row("value") |>
    dplyr::transmute(StockKeyLabel, .B_review_year = .Y)

  current_check <- current_check |>
    dplyr::left_join(b_review_years, by = "StockKeyLabel") |>
    dplyr::filter(
      is.finite(value), value > 0,
      is.finite(.AY_latest),
      (Indicator == "Fref" & .Y == .AY_latest - 1) |
        (Indicator == "Bref" & .AY == .AY_latest &
           (.Y == .AY_latest | .Y == .AY_latest - 1 | .Y == .B_review_year))
    )
  pending_current <- current_check |>
    dplyr::filter(eligibility %in% c("Unreviewed", "Review pending"))
  if (isTRUE(strict_current_review) && nrow(pending_current)) {
    stop(nrow(pending_current),
         " positive current-status F/B candidate record(s) are not approved ",
         "by the policy. See return_audit with strict_current_review=FALSE ",
         "to inspect them.")
  }

  picked <- candidates |>
    dplyr::filter(eligible_v3, Indicator %in% c("Fref", "Bref")) |>
    dplyr::mutate(
      .rank = dplyr::if_else(Indicator == "Fref", value, -value),
      .tie_original = dplyr::case_when(
        Indicator == "Fref" & reference_field == "FMSY" ~ 0L,
        Indicator == "Bref" & reference_field == "MSYBtrigger" ~ 0L,
        source == "Standard" ~ 1L,
        TRUE ~ 2L
      )
    ) |>
    dplyr::arrange(
      .audit_row_id, Indicator, .rank, .tie_original,
      reference_field, reference_name
    ) |>
    dplyr::group_by(.audit_row_id, Indicator) |>
    dplyr::slice_head(n = 1L) |>
    dplyr::ungroup()

  candidates <- candidates |>
    dplyr::mutate(winner_v3 = eligible_v3 & choice_id %in% picked$choice_id)

  # Audit-friendly lists retain ALL eligible candidates, not only winners.
  listings <- candidates |>
    dplyr::filter(eligible_v3, Indicator %in% c("Fref", "Bref")) |>
    dplyr::mutate(
      candidate_label = paste0(
        dplyr::if_else(nzchar(reference_name), reference_name, reference_field),
        " [", reference_field, "] = ", signif(value, 8),
        dplyr::if_else(manual_chart_override %in% TRUE,
                       " (manual chart override)", "")
      )
    ) |>
    dplyr::group_by(.audit_row_id, Indicator) |>
    dplyr::summarise(
      candidate_count = dplyr::n(),
      candidate_list = paste(candidate_label, collapse = " | "),
      .groups = "drop"
    ) |>
    tidyr::pivot_wider(
      id_cols = .audit_row_id,
      names_from = Indicator,
      values_from = c(candidate_count, candidate_list),
      names_glue = "{Indicator}_{.value}"
    )

  selections <- picked |>
    dplyr::transmute(
      .audit_row_id, Indicator,
      value = value,
      reference_name = dplyr::if_else(nzchar(reference_name), reference_name,
                                      reference_field),
      reference_field, source,
      manual_chart_override = manual_chart_override,
      SAGChartKey = SAGChartKey,
      review_note = dplyr::coalesce(source_issue_note, "")
    ) |>
    tidyr::pivot_wider(
      id_cols = .audit_row_id,
      names_from = Indicator,
      values_from = c(value, reference_name, reference_field, source,
                      manual_chart_override, SAGChartKey, review_note),
      names_glue = "{Indicator}_{.value}"
    )

  # The same column names as the original audit's selected data frame make
  # stock-level provenance extraction possible without a new summariser.
  base_rows <- audit$selected |>
    dplyr::select(.audit_row_id, AssessmentKey, StockKeyLabel,
                  FisheriesGuild, AssessmentYear, Year, StockSize)
  selected_v3 <- base_rows |>
    dplyr::left_join(selections, by = ".audit_row_id") |>
    dplyr::left_join(listings, by = ".audit_row_id")
  for (nm in c("Fref_value", "Bref_value")) {
    if (!nm %in% names(selected_v3)) selected_v3[[nm]] <- NA_real_
  }
  for (nm in c("Fref_reference_name", "Bref_reference_name",
               "Fref_reference_field", "Bref_reference_field",
               "Fref_source", "Bref_source",
               "Fref_candidate_list", "Bref_candidate_list",
               "Fref_review_note", "Bref_review_note")) {
    if (!nm %in% names(selected_v3)) selected_v3[[nm]] <- NA_character_
  }
  for (nm in c("Fref_manual_chart_override", "Bref_manual_chart_override")) {
    if (!nm %in% names(selected_v3)) selected_v3[[nm]] <- FALSE
  }

  out <- sag_formatted |>
    dplyr::mutate(
      .audit_row_id = dplyr::row_number(),
      FMSY_original = to_num(FMSY),
      MSYBtrigger_original = to_num(MSYBtrigger)
    ) |>
    dplyr::left_join(
      selected_v3 |>
        dplyr::select(
          .audit_row_id,
          Fref_value, Bref_value,
          Fref_reference_name, Bref_reference_name,
          Fref_reference_field, Bref_reference_field,
          Fref_source, Bref_source,
          Fref_candidate_list, Bref_candidate_list,
          Fref_manual_chart_override, Bref_manual_chart_override,
          Fref_review_note, Bref_review_note
        ),
      by = ".audit_row_id"
    ) |>
    dplyr::mutate(
      FMSY = to_num(Fref_value),
      MSYBtrigger = to_num(Bref_value),
      FMSY_is_proxy = is.finite(FMSY) &
        !is.na(Fref_reference_field) & Fref_reference_field != "FMSY",
      MSYB_is_proxy = is.finite(MSYBtrigger) &
        !is.na(Bref_reference_field) & Bref_reference_field != "MSYBtrigger",
      FMSY_proxy_name = dplyr::if_else(FMSY_is_proxy,
                                      Fref_reference_name, NA_character_),
      MSYB_proxy_name = dplyr::if_else(MSYB_is_proxy,
                                      Bref_reference_name, NA_character_),
      FMSY_refpoint_source = dplyr::case_when(
        !is.finite(FMSY) ~ "Missing",
        Fref_reference_field == "FMSY" ~ "Original",
        Fref_source == "Custom" ~ "Custom",
        TRUE ~ "Standard alternative"
      ),
      MSYB_refpoint_source = dplyr::case_when(
        !is.finite(MSYBtrigger) ~ "Missing",
        Bref_reference_field == "MSYBtrigger" ~ "Original",
        Bref_source == "Custom" ~ "Custom",
        TRUE ~ "Standard alternative"
      )
    ) |>
    dplyr::select(-.audit_row_id)

  if (nrow(out) != nrow(sag_formatted)) {
    stop("v3 changed the row count; review joins before using its output.")
  }
  if (!isTRUE(return_audit)) return(out)

  v2_current <- audit_sag_refpoints_by_stock(audit)
  v3_current <- audit_sag_refpoints_by_stock(list(selected = selected_v3))
  summarise_ref <- function(d, suffix) {
    out <- d |>
      dplyr::select(StockKeyLabel, Fref, Fref_name, Fref_field,
                    F_Year, Bref, Bref_name, Bref_field, B_Year)
    names(out)[-1] <- paste0(names(out)[-1], "_", suffix)
    out
  }
  difference <- function(a, b) {
    one_missing <- xor(is.na(a), is.na(b))
    both_present <- !is.na(a) & !is.na(b)
    different_number <- rep(FALSE, length(a))
    different_number[both_present] <- abs(a[both_present] - b[both_present]) >
      1e-9 * pmax(1, abs(a[both_present]), abs(b[both_present]))
    one_missing | different_number
  }
  comparison <- dplyr::full_join(
    summarise_ref(v2_current, "v2"),
    summarise_ref(v3_current, "v3"),
    by = "StockKeyLabel"
  ) |>
    dplyr::mutate(
      F_changed = difference(Fref_v2, Fref_v3),
      B_changed = difference(Bref_v2, Bref_v3),
      changed = F_changed | B_changed
    ) |>
    dplyr::arrange(StockKeyLabel)

  list(
    sag = out,
    candidates = candidates,
    selected = selected_v3,
    current_v2 = v2_current,
    current_v3 = v3_current,
    comparison = comparison,
    review_policy = policy,
    unresolved_settings = audit$unresolved_settings,
    unreviewed_current = pending_current
  )
}

# Attach both F/B reference-point provenance records to the one-row-per-stock
# output from stockstatus_CLD_current_proxy(). The original function drops
# assessment keys, names, candidates and source. This helper restores them
# without reclassifying a stock or changing catches/landings/discards.
attach_refpoint_provenance_v3 <- function(stock_current, current_v3) {
  if (!"StockKeyLabel" %in% names(stock_current) ||
      !"StockKeyLabel" %in% names(current_v3)) {
    stop("Both tables require StockKeyLabel.")
  }
  if (anyDuplicated(current_v3$StockKeyLabel)) {
    stop("current_v3 has duplicate StockKeyLabel values; resolve before joining.")
  }
  selected <- current_v3 |>
    dplyr::select(
      StockKeyLabel, F_AssessmentKey, F_Year,
      Fref, Fref_name, Fref_field, Fref_source, Fref_candidates,
      B_AssessmentKey, B_Year,
      Bref, Bref_name, Bref_field, Bref_source, Bref_candidates
    )
  out <- dplyr::left_join(stock_current, selected, by = "StockKeyLabel")
  if (nrow(out) != nrow(stock_current)) {
    stop("Joining the provenance table changed stock row counts.")
  }

  # Signal any disagreement between the status observation and its audit.
  differs <- function(a, b) {
    a <- suppressWarnings(as.numeric(as.character(a)))
    b <- suppressWarnings(as.numeric(as.character(b)))
    valid <- is.finite(a) & is.finite(b)
    out <- rep(FALSE, length(a))
    out[valid] <- abs(a[valid] - b[valid]) >
      1e-9 * pmax(1, abs(a[valid]), abs(b[valid]))
    out
  }
  if (all(c("FMSY", "Fref") %in% names(out)) &&
      any(differs(out$FMSY, out$Fref))) {
    warning("Some F references differ between stockstatus and v3 audit.")
  }
  if (all(c("Year", "B_Year") %in% names(out)) &&
      any(differs(out$Year, out$B_Year))) {
    warning("Some B observation years differ between stockstatus and v3 audit.")
  }
  if (all(c("MSYBtrigger", "Bref") %in% names(out)) &&
      any(differs(out$MSYBtrigger, out$Bref))) {
    warning("Some B references differ between stockstatus and v3 audit. ",
            "Check the per-stock AY_latest/Year filtering in stockstatus.")
  }
  out
}




# Current stock status uses F from assessment year - 1 and the latest usable
# biomass/reference pair within the stock's latest assessment for B.
# Status pies and Kobe plot compute their own indicator/quadrant statuses.

stockstatus_CLD_current_proxy <- function(x) {
  
  # --- Ensure proxy columns exist
  for (nm in c("FMSY_is_proxy","FMSY_proxy_name","MSYB_is_proxy","MSYB_proxy_name")) {
    if (!nm %in% names(x)) x[[nm]] <- NA
  }

  # --- Coerce numerics safely
  num_cols <- c("Year","FishingPressure","StockSize","FMSY","MSYBtrigger",
                "AssessmentYear","Catches","Landings","Discards")
  for (nm in intersect(num_cols, names(x))) {
    x[[nm]] <- suppressWarnings(as.numeric(x[[nm]]))
  }

  # --- Latest assessment year per stock
  x <- x %>%
    dplyr::group_by(StockKeyLabel) %>%
    dplyr::mutate(AY_latest = suppressWarnings(max(AssessmentYear, na.rm = TRUE))) %>%
    dplyr::ungroup()

  # ---------- F side: use Year == AY_latest - 1 ----------
  df_F <- x %>%
    dplyr::filter(Year == AY_latest - 1) %>%
    dplyr::mutate(
      F_FMSY = ifelse(!is.na(FMSY), FishingPressure / FMSY, NA_real_)
    ) %>%
    # if more than one row per stock remains, keep the last (most recent) one
    dplyr::arrange(StockKeyLabel, dplyr::desc(Year)) %>%
    dplyr::group_by(StockKeyLabel) %>%
    dplyr::slice_head(n = 1) %>%
    dplyr::ungroup() %>%
    dplyr::select(
      StockKeyLabel, FisheriesGuild, F_FMSY,
      Catches, Landings, Discards, FMSY, FishingPressure,
      F_proxy = FMSY_is_proxy, F_proxy_name = FMSY_proxy_name
    )

  # ---------- B side: latest usable pair in the latest assessment ----------
  df_B <- select_current_biomass_row(x, "MSYBtrigger") %>%
    dplyr::mutate(
      SSB_MSYBtrigger = ifelse(!is.na(MSYBtrigger), StockSize / MSYBtrigger, NA_real_)
    ) %>%
    dplyr::select(
      StockKeyLabel, Year, FisheriesGuild,
      SSB_MSYBtrigger, StockSize, MSYBtrigger,
      B_proxy = MSYB_is_proxy, B_proxy_name = MSYB_proxy_name
    )

  # ---------- Join F and B sides ----------
  df4 <- dplyr::full_join(df_F, df_B, by = c("StockKeyLabel","FisheriesGuild"))

  # Independent F/B statuses, using prepare_precautionary_status_pies() rules.
  df4 <- df4 %>%
    dplyr::mutate(
      FStatus = dplyr::case_when(
        !is.finite(FMSY) | FMSY <= 0 ~ "GREY",
        !is.finite(F_FMSY) | F_FMSY < 0 ~ "GREY",
        F_FMSY > 1 ~ "RED",
        TRUE ~ "GREEN"
      ),
      BStatus = dplyr::case_when(
        !is.finite(MSYBtrigger) | MSYBtrigger <= 0 ~ "GREY",
        !is.finite(SSB_MSYBtrigger) | SSB_MSYBtrigger < 0 ~ "GREY",
        SSB_MSYBtrigger < 1 ~ "RED",
        TRUE ~ "GREEN"
      )
    )

  df4
}