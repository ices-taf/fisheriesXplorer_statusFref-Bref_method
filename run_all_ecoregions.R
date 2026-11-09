# Run from the TAF repository root. Requires live API access and icesTAF.
library(icesTAF)

# Refresh boot snapshots ONLY when a fresh SAG/SID dataset is required.
# This executes boot/getSAG.R, boot/getSAGsettings.R, boot/getSID.R.
taf.boot(force = TRUE)

# Each following stage reads files written by the previous stage.
source("data.R")
source("model.R")
source("output.R")
source("report.R")
source("refresh_reference_review.R")
