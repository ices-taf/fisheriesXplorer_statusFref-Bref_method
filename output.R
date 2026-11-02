# Extract results of interest, write CSV output tables

# Before:
# After:

library(icesTAF)
source("utilities.R")
mkdir("output")

sag <- read.csv("boot/data/getSAG/sag.csv")
SAG_Settings <- read.csv("boot/data/getSAGsettings/SAG_Settings.csv")
sid <- read.csv("boot/data/getSID/sid.csv")


original <- format_sag(sag, sid)
result <- add_proxyRefPoints_v3(
  original, SAG_Settings,
  review_policy = "./model/SAG_policy_BI_approved.csv",
  return_audit = TRUE
)
View(result$current_v3)
View(result$comparison |> dplyr::filter(changed))
View(result$candidates |> dplyr::filter(winner_v3))
View(result$unresolved_settings)

catch_current_v3 <- stockstatus_CLD_current_proxy(result$sag)
catch_current_v3 <- attach_refpoint_provenance_v3(
  catch_current_v3, result$current_v3
)

dir.create("./output/reference_points", recursive = TRUE, showWarnings = FALSE)
saveRDS(result, "./output/reference_points/SAG_reference_v3_audit.rds")
utils::write.csv(result$current_v3,
  "./output/reference_points/SAG_refpoints_v3_by_stock.csv", row.names = FALSE)
utils::write.csv(result$comparison,
  "./output/reference_points/SAG_v2_vs_v3.csv", row.names = FALSE)
utils::write.csv(result$candidates,
  "./output/reference_points/SAG_v3_all_candidates.csv", row.names = FALSE)


