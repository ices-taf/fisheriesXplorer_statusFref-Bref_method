library(dplyr)
library(tidyr)
library(jsonlite)
library(icesTAF)

# ICES SAG and SID accessors extracted from user's utilities files.
# Uses dplyr, tidyr and jsonlite. Requires live access to ICES endpoints.
# add_keys(): guard added so special-component stocks absent from another
# ecoregion do not create placeholder rows.
source(file.path("..", "..", "..", "utilities.R"))

getSAG_ecoregion_new <- function(Ecoregion) {
       
        EcoregionCode <- get_ecoregion_acronym(Ecoregion)
        
        sag <- jsonlite::fromJSON(
                URLencode(
                        sprintf("https://sag.ices.dk/SAG_API/LatestStocks/Download?ecoregion=%s", EcoregionCode)
                )
        )
        return(sag)
}
sag <- getSAG_ecoregion_new("Bay of Biscay and the Iberian Coast")


write.csv(sag, "sag.csv", row.names = FALSE)