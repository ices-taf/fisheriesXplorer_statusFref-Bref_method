library(dplyr)
library(tidyr)
library(jsonlite)
library(icesTAF)

# ICES SAG and SID accessors extracted from user's utilities files.
# Uses dplyr, tidyr and jsonlite. Requires live access to ICES endpoints.
# add_keys(): guard added so special-component stocks absent from another
# ecoregion do not create placeholder rows.
source(file.path("..", "..", "..", "utilities.R"))



getSID <- function(year, EcoR) {
        
        stock_list_long <- jsonlite::fromJSON(
                URLencode(
                        sprintf("http://sd.ices.dk/services/odata4/StockListDWs4?$filter=ActiveYear eq %s&$select=StockKeyLabel,
                        EcoRegion,
                        YearOfLastAssessment,
                        AssessmentKey,
                        StockKeyDescription,
                        SpeciesScientificName,
                        SpeciesCommonName,
                        AdviceCategory,
                        DataCategory,
                        YearOfLastAssessment,
                        FisheriesGuild", year)
                )
        )$value

        stock_list_long <- stock_list_long %>%
                mutate(EcoRegion = as.character(EcoRegion)) %>%
                tidyr::separate_rows(EcoRegion, sep = ", ")

        stock_list_long <- stock_list_long %>%
                filter(EcoRegion == EcoR)

        ############ Hard coded for some stocks with assessmentComponents
        stock_list_long <- add_keys(stock_list_long, "cod.27.46a7d20", c(19661,19662))
        stock_list_long <- add_keys(stock_list_long, "cod.21.1.isc", c(19605))
        
        stock_list_long <- stock_list_long[!is.na(stock_list_long$AssessmentKey), ]
        
        stock_list_long$FisheriesGuild[stock_list_long$FisheriesGuild == "crustacean"] <- "shellfish"
        return(stock_list_long)
}


sid <- getSID(2026, "Bay of Biscay and the Iberian Coast")

write.csv(sid, "sid.csv", row.names = FALSE)