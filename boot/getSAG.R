# TAF BOOT: fetch latest SAG observations for every ecoregion, then refresh
# reference-point fields with the dedicated FishStockReferencePoints service.
#
# Executed by taf.boot() from boot/data/getSAG/.
# Outputs from this ONE boot step:
#   sag.csv                         -- enriched SAG observations for data.R
#   sag_reference_points.csv        -- one reference-point record per AssessmentKey
#   sag_reference_point_changes.csv -- differences from LatestStocks/Download
#   sag_reference_point_fetch_log.csv -- download outcomes / missing responses
#
# No API requests are needed in data.R, model.R, output.R or Shiny.

library(dplyr)
library(jsonlite)
library(icesSAG)

source("../../../utilities.R")
source("../../../all_regions_config.R")

# Fail the boot run if ANY reference-point service request fails (default).
# To investigate failures without stopping, set ICES_STRICT_REFPOINT_FETCH=false.
strict_fetch <- tolower(Sys.getenv("ICES_STRICT_REFPOINT_FETCH", "true")) %in%
  c("true", "t", "1", "yes")
max_attempts <- 3L

# Reference-point columns that are safe to replace. Never overwrite
# Year, AssessmentYear, FishingPressure, StockSize or catch observations.
standard_reference_fields <- c(
  "FMSY", "Fpa", "FLim", "Fmanagement",
  "MSYBtrigger", "Bpa", "Blim", "Bmanagement"
)
is_reference_field <- function(name) {
  tolower(name) %in% tolower(standard_reference_fields) |
    grepl("^CustomRefPoint(Name|Value|Note|Notes)[0-9]+$", name,
          ignore.case = TRUE)
}

key_string <- function(x) {
  out <- trimws(as.character(x))
  out[is.na(out)] <- ""
  out
}

read_ecoregion_sag <- function(region) {
  code <- get_ecoregion_acronym(region)
  endpoint <- sprintf(
    "https://sag.ices.dk/SAG_API/LatestStocks/Download?ecoregion=%s", code
  )
  response <- jsonlite::fromJSON(endpoint)
  x <- as.data.frame(response, stringsAsFactors = FALSE)
  if (!nrow(x) || !"AssessmentKey" %in% names(x)) {
    stop("Empty or incompatible SAG download for ecoregion: ", region)
  }
  x$TAF_Ecoregion <- region
  # Regional endpoint responses may use different types for optional fields.
  # Normalise before row binding; downstream read.csv/format_sag restores them.
  dplyr::mutate(x, dplyr::across(dplyr::everything(), as.character))
}

message("SAG: downloading all ", length(ecoregions), " ecoregions")
sag <- dplyr::bind_rows(lapply(ecoregions, function(region) {
  message("  SAG observations: ", region)
  read_ecoregion_sag(region)
}))
if (!setequal(unique(sag$TAF_Ecoregion), ecoregions)) {
  stop("The SAG download did not cover every configured ecoregion.")
}

keys <- sort(unique(key_string(sag$AssessmentKey)))
if (any(!nzchar(keys))) stop("SAG contains missing AssessmentKey values.")
if (any(!grepl("^[0-9]+$", keys))) {
  stop("SAG contains non-numeric AssessmentKey values.")
}
message("SAG reference-point API: ", length(keys),
        " distinct AssessmentKeys (retrieved ONCE each, across all regions)")

# Request one assessment at a time: explicitly avoids cross-assessment mixing,
# allows precise failure logging, and avoids requiring the API's multi-key
# response format to remain stable. This is a scheduled TAF job, not Shiny.
fetch_reference_points <- function(key) {
  last_error <- ""
  for (attempt in seq_len(max_attempts)) {
    response <- tryCatch(
      icesSAG::getFishStockReferencePoints(as.integer(key)),
      error = function(e) e
    )
    if (!inherits(response, "error")) {
      if (is.null(response)) {
        return(list(status = "no_record", data = NULL,
                    attempt = attempt, detail = "Service returned NULL"))
      }
      x <- tryCatch(as.data.frame(response, stringsAsFactors = FALSE),
                    error = function(e) e)
      if (inherits(x, "error")) {
        last_error <- conditionMessage(x)
      } else if (nrow(x) == 0L) {
        return(list(status = "no_record", data = NULL,
                    attempt = attempt, detail = "Service returned zero rows"))
      } else {
        key_col <- names(x)[tolower(names(x)) == "assessmentkey"]
        if (length(key_col) != 1L) {
          last_error <- "Reference-point response has no unique AssessmentKey column"
        } else {
          x <- x[key_string(x[[key_col]]) == key, , drop = FALSE]
          if (!nrow(x)) {
            last_error <- "Reference-point response does not match requested AssessmentKey"
          } else {
            fields <- names(x)[vapply(names(x), is_reference_field, logical(1))]
            # Multiple rows are permitted only if reference-point values agree.
            # Never silently choose one of several incompatible assessment rows.
            conflicts <- fields[vapply(fields, function(f) {
              values <- unique(key_string(x[[f]]))
              values <- values[nzchar(values)]
              length(values) > 1L
            }, logical(1))]
            if (length(conflicts)) {
              stop("Conflicting dedicated SAG reference points for AssessmentKey ",
                   key, ": ", paste(conflicts, collapse = ", "))
            }
            one <- x[1L, , drop = FALSE]
            # For repeated identical responses, retain the first non-missing
            # value in each field, rather than inheriting the first row's NA.
            for (f in fields) {
              nonmissing <- which(nzchar(key_string(x[[f]])))
              if (length(nonmissing)) {
                one[[f]] <- x[[f]][nonmissing[[1L]]]
              }
            }
            # Normalise key spelling across responses.
            names(one)[match(key_col, names(one))] <- "AssessmentKey"
            one$AssessmentKey <- key
            return(list(status = "ok", data = one,
                        attempt = attempt, detail = ""))
          }
        }
      }
    } else {
      last_error <- conditionMessage(response)
    }
    if (attempt < max_attempts) Sys.sleep(attempt)
  }
  list(status = "error", data = NULL, attempt = max_attempts,
       detail = last_error)
}

fetch_log <- vector("list", length(keys))
ref_list <- vector("list", length(keys))
for (i in seq_along(keys)) {
  key <- keys[[i]]
  if (i == 1L || i %% 25L == 0L || i == length(keys)) {
    message("  Reference-point downloads ", i, "/", length(keys))
  }
  response <- fetch_reference_points(key)
  fetch_log[[i]] <- data.frame(
    AssessmentKey = key,
    status = response$status,
    attempts = as.integer(response$attempt),
    detail = response$detail,
    stringsAsFactors = FALSE
  )
  if (identical(response$status, "ok")) {
    # Include only reference-point fields and AssessmentKey in this snapshot.
    x <- response$data
    keep <- names(x) == "AssessmentKey" |
      vapply(names(x), is_reference_field, logical(1))
    ref_list[[i]] <- dplyr::mutate(x[, keep, drop = FALSE],
                                  dplyr::across(dplyr::everything(), as.character))
  }
}
fetch_log <- dplyr::bind_rows(fetch_log)
utils::write.csv(fetch_log, "sag_reference_point_fetch_log.csv",
                 row.names = FALSE, na = "")

# Zero rows must still produce a valid, inspectable CSV.
nonempty_ref <- Filter(Negate(is.null), ref_list)
if (length(nonempty_ref)) {
  refs <- dplyr::bind_rows(nonempty_ref)
} else {
  refs <- data.frame(AssessmentKey = character(), stringsAsFactors = FALSE)
}
if (anyDuplicated(refs$AssessmentKey)) {
  stop("Duplicate AssessmentKeys in dedicated reference-point snapshot.")
}
utils::write.csv(refs, "sag_reference_points.csv", row.names = FALSE, na = "")

# Preserve the downloaded time series and all assessment/year relationships.
# Replace only explicitly returned reference-point columns by AssessmentKey.
# A present but NA dedicated value is authoritative; an absent column is not.
# Keep a full change log, including positive values overwritten by NA.
changes <- list()
change_id <- 0L
for (i in seq_len(nrow(refs))) {
  key <- refs$AssessmentKey[[i]]
  sag_rows <- which(key_string(sag$AssessmentKey) == key)
  if (!length(sag_rows)) stop("Unexpected reference point key outside bulk SAG: ", key)

  ref_fields <- names(ref_list[[match(key, keys)]])
  ref_fields <- ref_fields[vapply(ref_fields, is_reference_field, logical(1))]
  for (field in ref_fields) {
    existing_col <- which(tolower(names(sag)) == tolower(field))
    if (length(existing_col) > 1L) {
      stop("Ambiguous reference-point field in SAG: ", field)
    }
    if (!length(existing_col)) {
      sag[[field]] <- NA_character_
      actual_field <- field
    } else {
      actual_field <- names(sag)[existing_col]
    }
    old <- unique(key_string(sag[[actual_field]][sag_rows]))
    new <- key_string(refs[[field]][[i]])
    if (!identical(old, new)) {
      change_id <- change_id + 1L
      changes[[change_id]] <- data.frame(
        AssessmentKey = key, reference_field = actual_field,
        old_bulk_values = paste(ifelse(nzchar(old), old, "<NA>"), collapse = " | "),
        new_dedicated_value = if (nzchar(new)) new else "<NA>",
        n_rows_updated = length(sag_rows), stringsAsFactors = FALSE
      )
    }
    sag[[actual_field]][sag_rows] <- if (nzchar(new)) new else NA_character_
  }
}
changes_df <- if (length(changes)) dplyr::bind_rows(changes) else {
  data.frame(AssessmentKey = character(), reference_field = character(),
             old_bulk_values = character(), new_dedicated_value = character(),
             n_rows_updated = integer(), stringsAsFactors = FALSE)
}
utils::write.csv(changes_df, "sag_reference_point_changes.csv",
                 row.names = FALSE, na = "")
utils::write.csv(sag, "sag.csv", row.names = FALSE, na = "")

message("SAG reference-point integration: ", nrow(refs),
        " successful assessment records; ", nrow(changes_df),
        " reference field changes; ", sum(fetch_log$status == "no_record"),
        " missing reference-point records; ", sum(fetch_log$status == "error"),
        " failed requests")
if (strict_fetch && any(fetch_log$status == "error")) {
  failed <- fetch_log$AssessmentKey[fetch_log$status == "error"]
  stop("The dedicated reference-point service failed for ", length(failed),
       " assessment(s): ", paste(utils::head(failed, 20L), collapse = ", "),
       ". Inspect sag_reference_point_fetch_log.csv and rerun boot.")
}
