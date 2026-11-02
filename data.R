# Prepare data, write CSV data tables

# Before:
# After:

library(icesTAF)

source("utilities.R")

mkdir("data")

sag <- read.csv("boot/data/getSAG/sag.csv")
SAG_Settings <- read.csv("boot/data/getSAGsettings/SAG_Settings.csv")
sid <- read.csv("boot/data/getSID/sid.csv")

sag_formatted <- format_sag(sag, sid)
audit <- audit_sag_reference_points(sag_formatted, SAG_Settings)
View(audit$selected)             # chosen Fref/Bref and full v2 candidate lists
View(audit$candidates)           # all standard/custom fields and inclusion reasons
View(audit$unresolved_settings)  # settings such as Fpa or missing index 5



stock_audit <- audit_sag_refpoints_by_stock(audit)
View(stock_audit)                # ONE ROW per stock, F and B provenance



mkdir("data/reference_points")
saveRDS(audit, "./data/reference_points/SAG_reference_audit.rds")
utils::write.csv(stock_audit,
  "./data/reference_points/SAG_selected_refpoints_by_stock.csv", row.names = FALSE)
utils::write.csv(audit$selected,
  "./data/reference_points/SAG_selected_refpoints.csv", row.names = FALSE)
utils::write.csv(audit$candidates,
  "./data/reference_points/SAG_all_candidate_refpoints.csv", row.names = FALSE)
utils::write.csv(audit$unresolved_settings,
  "./data/reference_points/SAG_unresolved_settings.csv", row.names = FALSE)



new_policy <- make_sag_refpoint_policy(
  sag_formatted = sag_formatted,
  sag_settings = SAG_Settings,
  output_file = "./data/SAG_policy_BI_region.csv"
#   previous_policy = "./data/SAG_reference_point_policy_approved.csv"
)
