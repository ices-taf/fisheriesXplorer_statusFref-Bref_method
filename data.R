# TAF DATA: read the three boot snapshots; format SAG and prepare draft policies.
# No API calls occur here.
library(icesTAF)
library(dplyr)
source("utilities.R")
source("all_regions_config.R")
mkdir("data")
dir.create("data/regions", recursive = TRUE, showWarnings = FALSE)
dir.create("data/policies", recursive = TRUE, showWarnings = FALSE)
dir.create("data/reference_points", recursive = TRUE, showWarnings = FALSE)

sag_all <- read.csv("boot/data/getSAG/sag.csv", stringsAsFactors = FALSE)
settings_all <- read.csv("boot/data/getSAGsettings/SAG_Settings.csv", stringsAsFactors = FALSE)
sid_all <- read.csv("boot/data/getSID/sid.csv", stringsAsFactors = FALSE)

for (x in list(sag_all, settings_all, sid_all)) {
  if (!"TAF_Ecoregion" %in% names(x)) {
    stop("Boot dataset lacks TAF_Ecoregion; rerun taf.boot(force = TRUE).")
  }
}
if (!setequal(unique(sag_all$TAF_Ecoregion), ecoregions)) {
  stop("The SAG boot snapshot does not cover all configured ecoregions.")
}

log_rows <- vector("list", length(ecoregions))
for (i in seq_along(ecoregions)) {
  region <- ecoregions[[i]]
  code <- get_ecoregion_acronym(region)
  message("DATA ", i, "/", length(ecoregions), ": ", region)
  sag <- sag_all[sag_all$TAF_Ecoregion == region, , drop = FALSE]
  sid <- sid_all[sid_all$TAF_Ecoregion == region, , drop = FALSE]
  settings <- settings_all[settings_all$TAF_Ecoregion == region, , drop = FALSE]
  sag$TAF_Ecoregion <- NULL
  sid$TAF_Ecoregion <- NULL
  settings$TAF_Ecoregion <- NULL

  if (!nrow(sag) || !nrow(sid)) {
    stop("Missing SAG or SID stock records for ", region,
         "; inspect boot data instead of publishing a partial dataset.")
  }
  original <- format_sag(sag, sid)
  if (!nrow(original)) stop("format_sag returned no rows for ", region)
  if (any(is.na(original$StockKeyLabel) | !nzchar(original$StockKeyLabel))) {
    stop("Missing stock identifiers after formatting ", region)
  }

  # Persist input to the model/output stages, avoiding another format/join.
  saveRDS(list(original = original, settings = settings, ecoregion = region,
               code = code, assessment_year = assessment_year),
          file.path("data/regions", paste0(code, ".rds")))

  audit <- audit_sag_reference_points(original, settings)
  saveRDS(audit, file.path("data/reference_points", paste0(code, "_audit.rds")))
  utils::write.csv(audit$unresolved_settings,
                   file.path("data/reference_points", paste0(code, "_unresolved_settings.csv")),
                   row.names = FALSE, na = "")

  # This helper creates the DRAFT and uses precisely the same candidate schema
  # as v3; it does not make the approval decisions.
  policy_path <- file.path("data/policies", paste0("SAG_policy_", code, ".csv"))
  make_sag_refpoint_policy(
    sag_formatted = original, sag_settings = settings,
    output_file = policy_path, previous_policy = previous_policy
  )
  log_rows[[i]] <- data.frame(Ecoregion = region, Code = code,
                              n_formatted_rows = nrow(original),
                              n_stocks = dplyr::n_distinct(original$StockKeyLabel))
}
utils::write.csv(dplyr::bind_rows(log_rows), "data/coverage_input.csv",
                 row.names = FALSE)
