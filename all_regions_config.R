# Shared TAF settings; source from the repository root.
# Keep this catalogue aligned with get_ecoregion_acronym() in utilities.R.
ecoregions <- c(
  "Baltic Sea", "Bay of Biscay and the Iberian Coast", "Celtic Seas",
  "Greater North Sea", "Norwegian Sea", "Icelandic Waters",
  "Barents Sea", "Greenland Sea", "Faroes",
  "Oceanic Northeast Atlantic", "Azores"
)
assessment_year <- as.integer(Sys.getenv("ICES_ASSESSMENT_YEAR", "2026"))
if (length(assessment_year) != 1L || !is.finite(assessment_year)) {
  stop("Set ICES_ASSESSMENT_YEAR to a valid integer year.")
}

# Automatically include chart-assigned F/B references, with this exception.
# This is a provisional policy until independently verified for each stock.
excluded_standard_fields <- c("Blim")

# Optional approved master policy: prior decisions carry over on exact matches.
previous_policy <- "config/SAG_reference_point_policy_approved.csv"
if (!file.exists(previous_policy)) previous_policy <- NULL

# Explicit exceptions for custom points with no chart assignment. These are
# stock/assessment-specific, not generic rules based on reference-point names.
manual_overrides_path <- "config/manual_reference_overrides.csv"
