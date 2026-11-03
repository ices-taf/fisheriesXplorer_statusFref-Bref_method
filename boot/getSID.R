# TAF boot script. Executed in boot/data/getSID by taf.boot().
# Query the whole active-year SID list ONCE, then split by ecoregion.
library(dplyr)
library(tidyr)
library(jsonlite)
source("../../../utilities.R")
source("../../../all_regions_config.R")

url <- sprintf(paste0(
  "http://sd.ices.dk/services/odata4/StockListDWs4?",
  "$filter=ActiveYear eq %s&$select=",
  "StockKeyLabel,EcoRegion,YearOfLastAssessment,AssessmentKey,",
  "StockKeyDescription,SpeciesScientificName,SpeciesCommonName,",
  "AdviceCategory,DataCategory,FisheriesGuild"
), assessment_year)
stock_list <- jsonlite::fromJSON(utils::URLencode(url))$value
if (!is.data.frame(stock_list) || !nrow(stock_list)) {
  stop("SID returned no rows for assessment year ", assessment_year)
}
stock_list <- stock_list |>
  dplyr::mutate(EcoRegion = as.character(EcoRegion)) |>
  tidyr::separate_rows(EcoRegion, sep = ", ")

rows <- lapply(ecoregions, function(region) {
  message("SID: ", region)
  x <- stock_list |> dplyr::filter(EcoRegion == region)
  # Preserve the existing assessment-component mappings.
  if (nrow(x)) {
    x <- add_keys(x, "cod.27.46a7d20", c(19661, 19662))
    x <- add_keys(x, "cod.21.1.isc", c(19605))
  }
  x <- x[!is.na(x$AssessmentKey), , drop = FALSE]
  x$FisheriesGuild[x$FisheriesGuild %in% "crustacean"] <- "shellfish"
  x$TAF_Ecoregion <- rep(region, nrow(x))
  dplyr::mutate(x, dplyr::across(dplyr::everything(), as.character))
})
sid <- dplyr::bind_rows(rows)
if (!nrow(sid)) stop("No matched SID stocks for any configured ecoregion.")
utils::write.csv(sid, "sid.csv", row.names = FALSE, na = "")
