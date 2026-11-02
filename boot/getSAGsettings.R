library(dplyr)
library(tidyr)
library(jsonlite)
library(icesTAF)

# ICES SAG and SID accessors extracted from user's utilities files.
# Uses dplyr, tidyr and jsonlite. Requires live access to ICES endpoints.
# add_keys(): guard added so special-component stocks absent from another
# ecoregion do not create placeholder rows.
source(file.path("..", "..", "..", "utilities.R"))



getSAG_SettingsEcoregion <- function(Ecoregion) {
        
        EcoregionCode <- get_ecoregion_acronym(Ecoregion)
        
        sag_settings <- jsonlite::fromJSON(
                URLencode(
                        sprintf("https://sag.ices.dk/SAG_API/LatestStocks/Settings?ecoregion=%s", EcoregionCode)
                )
        )
        return(sag_settings)
}

SAG_Settings <- getSAG_SettingsEcoregion("Bay of Biscay and the Iberian Coast")




#### save csv
write.csv(SAG_Settings, "SAG_Settings.csv", row.names = FALSE)


