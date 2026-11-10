# All-ecoregion v3 reference-point patch — existing TAF structure

This patch was developed from the uploaded **fisheriesXplorer_statusFref-Bref_method** repository. It replaces only the listed boot scripts and the root TAF-stage scripts, and adds one configuration CSV. **Do not replace `utilities.R`, `utilities_plotting.R`, or your existing analysis data with an earlier generic R/ folder bundle.** Keep existing `.gitignore`, Git history and untracked data separately.

## The stage responsibilities follow your existing repo

```
boot/DATA.bib                    (same three script entries)
boot/getSAG.R                   -> boot/data/getSAG/sag.csv
boot/getSAGsettings.R           -> boot/data/getSAGsettings/SAG_Settings.csv
boot/getSID.R                   -> boot/data/getSID/sid.csv
                                  all 11 ecoregions; extra TAF_Ecoregion field

data.R                         -> data/regions/<code>.rds
                                  data/policies/SAG_policy_<code>.csv (DRAFT)
                                  data/reference_points/<code>_audit.rds

model.R                        -> model/policies/SAG_policy_<code>_approved.csv
                                  model/policy_summary.csv

output.R                       -> output/regions/stocks_<code>.rds
                                  output/reference_points/v3_audit_<code>.rds
                                  output/stock_status_by_ecoregion.rds
                                  output/stock_status_unique.rds
                                  output/coverage_by_ecoregion.csv
                                  output/conflicting_stocks.csv

report_1_summary.R             -> report/reference_point_summary.md
                                  report/stock_status_coverage.csv
report_2_plotting.R             -> report/html_plots/stock_summary__<code>.html (optional)
                                  report/html_plots/ges__<code>.html (optional)
                                  report/html_plots/discards__<code>.html (optional)
                                  report/html_plots/kobe__<code>__<guild>.html (optional)
                                  report/html_plots/lib/ (shared dependencies)
                                  report/html_plots/plot_index.csv
                                  report/html_plots/plot_export_report.csv
```

The **boot** scripts are executed by `taf.boot()` in their own `boot/data/<script_name>/` working directories, exactly as in your repo. Each script now retrieves all configured ecoregions (SAG and settings: one request per region; SID: one active-year download, then region splitting). They write the *same named files* your `DATA.bib` expects, but with a `TAF_Ecoregion` column to separate stock/assessment records. `data.R` reads those files and removes `TAF_Ecoregion` before calling your existing `format_sag()`. No subsequent stage performs API requests.

## Installation

Extract this patch over a copy/branch of the existing repo, retaining the relative paths. The patch includes:

- `boot/getSAG.R`, `boot/getSAGsettings.R`, `boot/getSID.R`, `boot/DATA.bib`
- `all_regions_config.R`, `data.R`, `model.R`, `output.R`, `report.R`, `report_1_summary.R`, `report_2_plotting.R`
- `config/manual_reference_overrides.csv` and `run_all_ecoregions.R`

Install dependencies already used by your repository (`icesTAF`, `dplyr`, `tidyr`, `tibble`, `jsonlite`); optionally `plotly` and `htmlwidgets` for the report HTML. `utilities.R` in your repository already provides `audit_sag_reference_points`, `make_sag_refpoint_policy`, `read_sag_review_policy_v3`, `add_proxyRefPoints_v3`, `stockstatus_CLD_current_proxy`, and the provenance helper. `utilities_plotting.R` is unchanged.

From the repository root, run:

```r
source("run_all_ecoregions.R")
```

Alternatively run `taf.boot(force = TRUE)` and then source `data.R`, `model.R`, `output.R`, and `report.R` in order. To reuse boot data without fetching anew, start at `source("data.R")`. The default SID active year is **2026**; set `ICES_ASSESSMENT_YEAR` in the environment before running boot to override. If you change the year, review the year metadata in `boot/DATA.bib` accordingly.

`report.R` runs both report stages. To run either stage independently from the repository root using existing output data:

```r
source("report_1_summary.R")
source("report_2_plotting.R")
```

Plot HTML files are exported with `selfcontained = FALSE` and share the sibling `lib/` directory. Keep the entire `report/html_plots/` directory together when copying or publishing plots; HTML files alone are not portable. Existing files in the former `report/status_pies/` and `report/discard_trends/` directories are not updated or removed.

`plot_index.csv` maps successfully exported selections to HTML basenames using `plot_type`, `ecoregion_id`, `guild_id`, and `file`. Ecoregion IDs use the existing acronyms; `guild_id` is empty for pies and discard trends. Kobe exports use the five guild IDs `benthic`, `demersal`, `elasmobranch`, `pelagic`, and `shellfish`, retaining the existing plotting function's defaults. `plot_export_report.csv` records all 88 expected selections (33 regional plots and 55 region/guild Kobe plots), with `status` (`exported`, `no_data`, or `failed`) and a `reason`. Absent Kobe guilds and other known no-data cases are recorded explicitly and omitted from the selection index. Both CSVs are updated after each export; a previous HTML for an attempted selection is removed before regeneration so a stale file cannot appear successful. Duplicate selection keys or filenames are rejected, and unexpected export errors cause the run to fail after recording the outcomes. Plotting packages remain optional for the summary, but missing dependencies now count as failures when running the plotting stage.

## Reference-point policy

- **Fref:** minimum positive approved F point. **Bref:** maximum positive approved B point.
- Chart 3 and chart 4 remain distinct via the original SAG `settingKey = 51` and `SAGChartKey` values; standard reference fields have their inherent indicator.
- As previously agreed, **new chart-assigned candidates** are provisionally `INCLUDE` / `YES`, except `Blim` (`EXCLUDE` / `NO`). They are *not independently checked for compatible numerator units.* Verify special custom reference points before deployment.
- Previously approved decisions can be reused on **exactly matching** stock/assessment/reference definitions by placing a master CSV in `config/SAG_reference_point_policy_approved.csv`. No old Bay-of-Biscay policy is required for the batch to run.
- Two previously reviewed unassigned custom F points are maintained as *stock- and assessment-specific* exceptions in `config/manual_reference_overrides.csv` (`rjn.27.8c`, `sol.27.8c9a`). Do not assign all unassigned points whose names resemble `Fmsy proxy` automatically.
- Known source anomalies `nep.fu.31` and `gfb.27.nea` are approved provisionally with explicit `source_issue_note` entries. Blank names must be retained in the policy; do not delete those rows.
- Other unassigned custom points remain `REVIEW`/`UNKNOWN` and are never automatically selected. A newly appearing assigned point must be present in the generated policy.

## Quality gates and duplicate stocks

The batch refuses to publish a final snapshot when a region has no SAG/SID data, a valid stock ID is missing, or one region's v3 calculation fails; it writes `output/failed_regions.csv` for model/output failures. The output preserves one record per **stock × ecoregion**, including stocks with unavailable F or B. Multiple ecoregion memberships are not discarded. It checks repeated stocks for contradictory assessment/reference values. If inconsistencies exist, it writes `output/conflicting_stocks.csv` and `output/stock_status_by_ecoregion_REVIEW.rds`, **not a final unique-stock result**.

The primary deployment-ready file after validation is `output/stock_status_by_ecoregion.rds`. `output/stock_status_unique.rds` adds a semicolon-separated list of member ecoregions. Catch-weighted pies and catch-scaled Kobe plots require the separate RDBES ecoregion allocation and are deliberately not computed here; the report can produce *stock-count* status pies without that allocation.

## Operational limitations

- The patch is a **static implementation, not a completed live run**. R is unavailable in this ChatGPT execution environment, so syntax and structural checks were performed but not a live `taf.boot()` / `source()` execution. Test on a new git branch first.
- Fetching is done during **TAF boot**, not on `shinyapps.io` and not during Shiny sessions. Publish the validated RDS snapshot to fisheriesXplorer using a separate deployment or external hosting workflow.
- `format_sag()` and existing reference-point functions are reused without modification. Their assessment-component exclusions and year-selection behavior therefore remain as in your repo, including F from `AssessmentYear - 1` and B from the most recent admissible `AssessmentYear` or `AssessmentYear - 1` row.
