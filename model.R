# Run analysis, write model results

# Before:
# After:

library(icesTAF)
library(dplyr)
source("utilities.R")
mkdir("model")


policy <- read.csv(
  "./data/SAG_policy_BI_region.csv",
  stringsAsFactors = FALSE
)

policy <- dplyr::bind_rows(
  policy,
  policy %>%
    dplyr::filter(
      Indicator == "Unassigned",
      reference_name %in% c("F_{MSY proxy}", "Fmsy proxy")
    ) %>%
    dplyr::mutate(
      Indicator = "Fref",
      review_decision = "INCLUDE",
      matches_numerator = "YES",
      manual_chart_override = TRUE
    )
)



# policy %>%
#   count(Indicator, reference_field, reference_name, sort = TRUE) %>%
#   View()



# Define which reference points to exclude
exclude_fields <- c("Blim")

# Add more if necessary, e.g.
# exclude_fields <- c("Blim", "Bmanagement")

# Approve all candidates assigned to F or B
assigned <- policy$Indicator %in% c("Fref", "Bref")

policy$review_decision[assigned] <- "INCLUDE"
policy$matches_numerator[assigned] <- "YES"

# Exclude the specified reference points
exclude <- assigned &
  tolower(trimws(policy$reference_field)) %in%
  tolower(exclude_fields)

policy$review_decision[exclude] <- "EXCLUDE"
policy$matches_numerator[exclude] <- "UNKNOWN"

# Save under a new filename
write.csv(
  policy,
  "./model/SAG_policy_BI_approved.csv",
  row.names = FALSE,
  na = ""
)
