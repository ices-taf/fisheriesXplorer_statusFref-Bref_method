# TAF boot script. Executed in boot/data/getSAGsettings by taf.boot().
library(dplyr)
library(jsonlite)
source("../../../utilities.R")
source("../../../all_regions_config.R")

getSAG_SettingsEcoregion <- function(Ecoregion) {
  code <- get_ecoregion_acronym(Ecoregion)
  jsonlite::fromJSON(
    utils::URLencode(sprintf(
      "https://sag.ices.dk/SAG_API/LatestStocks/Settings?ecoregion=%s", code
    ))
  )
}

rows <- lapply(ecoregions, function(region) {
  message("SAG settings: ", region)
  x <- as.data.frame(getSAG_SettingsEcoregion(region), stringsAsFactors = FALSE)
  # A region may have no custom settings; retain an empty schema if so.
  for (nm in c("AssessmentKey", "settingKey", "SAGChartKey", "settingValue")) {
    if (!nm %in% names(x)) x[[nm]] <- rep(NA_character_, nrow(x))
  }
  x$TAF_Ecoregion <- rep(region, nrow(x))
  dplyr::mutate(x, dplyr::across(dplyr::everything(), as.character))
})
settings <- dplyr::bind_rows(rows)
# Preserve region catalogue even when a region has zero settings.
utils::write.csv(settings, "SAG_Settings.csv", row.names = FALSE, na = "")
