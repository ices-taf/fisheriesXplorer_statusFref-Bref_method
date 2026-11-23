# fisheriesXplorer stock status using Fref and Bref

This repository implements the stock-status workflow for the fisheriesXplorer Status module using the ICES Transparent Assessment Framework (TAF). It combines stock assessment data from the ICES Stock Assessment Graphs database (SAG), reference-point settings and stock metadata from the Stock Information Database (SID). It produces traceable status summaries, annual time series and interactive plots for all configured ecoregions.

The method uses two generic reference points, **Fref for fishing pressure** and **Bref for stock size**. This allows stocks assessed against MSY, precautionary, management and proxy reference points to contribute to regional summaries. The selected reference name, value and source are retained so that each classification can be interpreted in its assessment context.

## Background and purpose

Stock status was an important area of discussion and methodological development during WKBoBFO. The workshop reviewed the presentation and interpretation of status pie charts and Kobe plots. It also developed the use of RDBES data to allocate catches from widely distributed stocks to ecoregions, helping regional summaries reflect the relative importance of those stocks in regional fisheries.

Following the workshop, the status method was extended to include a broader range of assessed stocks, including Category 3 stocks evaluated using management or proxy reference points. The earlier MSY/precautionary presentation could omit information from references such as a management harvest rate, an FMSY proxy or an index trigger. The Fref/Bref approach makes that information available while retaining the identity of the reference used.

## Stock-status method

### Reference-point definitions

Fref is a threshold for the fishing-pressure measure reported for a stock. Depending on the assessment, that measure can be fishing mortality, a harvest rate or a compatible pressure proxy. Bref is a threshold for the reported stock-size measure, which can be biomass or a stock-size index.

The principal reference families considered by the method are listed below. Names in SAG can contain mathematical formatting or stock-specific labels.

| Indicator | Reference families |
| --- | --- |
| Fishing pressure, Fref | FMSY, FPA, Fmanagement, FMSY proxy, HRMSY, HRPA, HRMSY proxy and HRmanagement |
| Stock size, Bref | MSY Btrigger, Management Btrigger, BPA, Bmanagement, Itrigger and Istat |

Assessments can also report fishing pressure on an already normalised **F/FMSY** scale. On that scale, FMSY corresponds to 1. The selected policy reference must be expressed on the same scale as the numerator, including where a proxy is reported relative to a reference of 1.

The effective candidate set is controlled by the reference-point policy files. The audit inventories a broader set of standard and custom fields. In the current provisional configuration, the standard SAG field `Blim` is excluded, while `FLim` remains an F candidate unless its policy excludes it. Custom fields are governed by their individual policy decisions. The table above therefore describes the main reference families used by the method, with individual eligibility determined by the reviewed policy.

### Selecting Fref and Bref

For each assessment row, the method selects the most precautionary value among the eligible, comparable reference points.

- **Fref is the lowest positive, finite approved F reference.**
- **Bref is the highest positive, finite approved B reference.**

An eligible reference must have an established F/B assignment, `review_decision = INCLUDE` and `matches_numerator = YES`. Standard SAG fields have an inherent indicator. Custom references use SAG chart settings, with chart 3 for F and chart 4 for B, or an explicit manual mapping. The settings audit uses `settingKey = 51` and retains the chart assignment.

Comparability must be checked against the numerator definition, units and scale. The current model stage provisionally approves newly assigned candidates, subject to configured exclusions. This provisional approval requires scientific review, particularly for custom references. `unit_scale` is retained as policy metadata, and the ratio calculations use the supplied numeric values directly. Any required conversion must already be reflected in those values.

Unassigned custom references remain pending until an explicit stock- and assessment-specific decision establishes how they should be used. A familiar-looking reference name alone is insufficient evidence for that assignment.

### Independent fishing-pressure and stock-size status

The calculations use the selected generic references.

```text
F ratio = FishingPressure / Fref
B ratio = StockSize / Bref
```

| Indicator | GREEN | RED | GREY |
| --- | --- | --- | --- |
| `FStatus` | Valid F ratio at or below 1 | Valid F ratio above 1 | Reference or observation unavailable or invalid |
| `BStatus` | Valid B ratio at or above 1 | Valid B ratio below 1 | Reference or observation unavailable or invalid |

A valid reference is finite and strictly positive. A valid observation and its calculated ratio are finite and non-negative. Equality with the reference is GREEN for both indicators. A zero observation remains valid, giving GREEN for F and RED for B.

A stock can have a named reference point and still be GREY when the observation needed for the comparison is missing. The two status columns are independent, so a stock can have a known status for one indicator and an unknown status for the other.

For compatibility with existing functions, several exported columns retain their original names.

| Exported column | Meaning under this method |
| --- | --- |
| `FMSY` | Selected generic Fref value |
| `MSYBtrigger` | Selected generic Bref value |
| `F_FMSY` | `FishingPressure / Fref` |
| `SSB_MSYBtrigger` | `StockSize / Bref` |
| `Fref_name`, `Bref_name` | Names of the references actually selected |
| `FStatus`, `BStatus` | Independent classifications using the rules above |

A GREEN result indicates that the observation is on the favourable side of the selected reference. Its biological and management interpretation follows that reference type. Regional summaries can therefore combine stocks evaluated against different kinds of thresholds.

### Observation years in the current snapshot

The current snapshot selects F and B separately within each stock's latest available assessment.

- **Fishing pressure and current catch quantities** use the observation year equal to `AssessmentYear - 1`.
- **Stock size** uses the latest usable observation/reference pair within the latest assessment, with `Year <= AssessmentYear`. The selected pair must have finite, non-negative `StockSize`, a finite positive Bref and a finite ratio. An earlier observation year is used when a later row lacks a usable pair.
- **If no usable B pair exists**, the latest positive-reference row is retained where available. The reference name can remain visible, while `BStatus` is GREY because the observation or calculated ratio is unavailable or invalid.

The biomass fallback keeps the observation and its selected reference on the same source row. It searches the time series of the latest assessment and excludes observation years beyond that assessment year. It does not impose a fixed historical lookback limit.

`F_Year` and `B_Year` identify the respective source years, while `F_AssessmentKey` and `B_AssessmentKey` retain assessment identity. In the current dataset, `Year` follows the B source row. When only a B reference is retained, `B_Year` identifies that reference row. `AssessmentSnapshotYear` identifies the configured extraction cycle. These fields can legitimately differ.

The fallback selects the current B value. Annual data retain the original yearly observations, including missing values.

## Worked examples from the 2026 snapshot

The following examples match the exported 2026 assessment snapshot. They illustrate the method using the values and reference names retained in that snapshot. Ratios are rounded below.

| Stock | Indicator | Observation | Selected reference and value | Ratio | Status |
| --- | --- | --- | --- | --- | --- |
| `cod.27.5a` | Fishing pressure | 0.198 | Management harvest rate, `HR_{mgt}` = 0.2 | 0.990 | GREEN |
| `cod.27.5a` | Stock size | 377,277 t | `MSYBtrigger` = 265,000 t | 1.424 | GREEN |
| `cap.27.2a514` | Fishing pressure | Unavailable | Unavailable | Unavailable | GREY |
| `cap.27.2a514` | Stock size | 260,947 t | `Bmanagement` = 114,000 t | 2.289 | GREEN |
| `whg.27.89a` | Fishing pressure | 0.95586929 | `F_{MSY proxy}` = 1 | 0.956 | GREEN |
| `whg.27.89a` | Stock-size index | 0.531505483 | `I_{trigger}` = 0.74 | 0.718 | RED |

### Cod in Icelandic waters, `cod.27.5a`

Several fishing-pressure references are available. The selected management harvest-rate reference is 0.2, and the observed pressure of 0.198 is below it. The selected biomass reference is MSY Btrigger at 265,000 t, and biomass is above it. Both indicators are GREEN. The F observation is from 2025 and the B observation from 2026, within assessment key `22640`.

This example shows how the broader method incorporates a management harvest-rate reference into the fishing-pressure classification. [ICES, 2026a](https://doi.org/10.17895/ices.advice.30932021)

### Capelin around Iceland, East Greenland and Jan Mayen, `cap.27.2a514`

The available management biomass reference is 114,000 t. Biomass of 260,947 t is above that threshold, giving GREEN stock-size status. Fishing pressure and its reference are unavailable in the snapshot, giving GREY fishing-pressure status. The B observation is from 2026, within assessment key `23744`.

This example shows how a stock with Bmanagement can contribute to stock-size summaries even when fishing-pressure status is unavailable. [ICES, 2026b](https://doi.org/10.17895/ices.advice.30931994)

### Category 3 whiting, `whg.27.89a`

This stock uses an FMSY proxy and Itrigger. Its pressure proxy is below its reference, giving GREEN fishing-pressure status, while its stock-size index is below Itrigger, giving RED stock-size status. Both observations are from 2025, within the 2026 assessment with key `23773`.

This example shows how the method represents stocks evaluated using pressure proxies and biomass indices. [ICES, 2026c](https://doi.org/10.17895/ices.advice.32261619)

## Regional catch allocation

The method for deriving RDBES ecoregion landings shares is documented in the dedicated [fisheriesXplorer ecoregion landings shares TAF repository](https://github.com/ices-taf/fisheriesXplorer_ecoregion_landings_shares_method).

This repository consumes the exported shares in `data/stock_ecoregion_landings_shares.csv`. The report expects `StockKeyLabel`, `Ecoregion`, `CLyear`, `ShareLandingsInEcoregion`, `share_method`, `share_source` and `special_rule`. Percentages are converted to proportions. The same landings-derived share is applied to catches, landings and discards as a regional allocation assumption.

The report uses the same-year usable share where available, otherwise the latest earlier usable share. Years preceding all available shares use the earliest usable share for that stock and ecoregion. With usable shares spanning 2021–2025, this applies the 2021 share to pre-2021 observations and the 2025 share to 2026 observations. Zero is a valid share, and missing shares remain unavailable.

Current catch quantities are matched using `F_Year`, and annual quantities using `Year`. The exports record `CatchYear`, `RDBES_share_year`, `share_status` and share provenance, together with `Catches_in_ecoregion`, `Landings_in_ecoregion` and `Discards_in_ecoregion`.

## TAF workflow

The ecoregion catalogue and assessment snapshot year are defined in `all_regions_config.R`. The current catalogue contains 11 ecoregions. In the paths below, `<code>` is the ecoregion acronym used by `get_ecoregion_acronym()`.

| Stage | Responsibility | Main outputs |
| --- | --- | --- |
| `boot/getSAG.R` | Retrieve assessment data | `boot/data/getSAG/sag.csv` |
| `boot/getSAGsettings.R` | Retrieve reference-point settings | `boot/data/getSAGsettings/SAG_Settings.csv` |
| `boot/getSID.R` | Retrieve stock metadata | `boot/data/getSID/sid.csv` |
| `data.R` | Prepare regional inputs and audit candidates | `data/regions/<code>.rds`, `data/policies/SAG_policy_<code>.csv`, `data/reference_points/<code>_audit.rds` |
| `model.R` | Apply reference policies and manual mappings | `model/policies/SAG_policy_<code>_approved.csv`, `model/policy_summary.csv`, `model/named_settings_resolved.csv` |
| `output.R` | Calculate status, attach provenance and validate regional results | `output/regions/stocks_<code>.rds`, `output/reference_points/v3_audit_<code>.rds`, consolidated snapshots and diagnostics |
| `report_1_summary.R` | Summarise reference coverage | `report/reference_point_summary.md`, `report/stock_status_coverage.csv` |
| `report_2_plotting.R` | Prepare regional current/annual data and export interactive plots | `report/plot_data/`, `report/html_plots/` |
| `refresh_reference_review.R` | Consolidate references requiring attention | `review/unassigned_reference_points.csv` |

TAF boot runs the retrieval scripts in their respective `boot/data/<script_name>/` directories, using the entries in `boot/DATA.bib`. The saved inputs carry `TAF_Ecoregion` so records can be separated by ecoregion. Later TAF stages work from the saved inputs. `data.R` removes the regional tag before passing each regional subset to `format_sag()`.

`utilities.R` contains the assessment, reference-selection, current-status and provenance helpers. `utilities_plotting.R` contains the plot preparation and rendering functions. `report.R` runs both report stages.

## Configuration and manual review

### Reference decisions

`review/unassigned_reference_points.csv` is the central review queue. It is regenerated by `refresh_reference_review.R`, preserving draft review entries where reference definitions still match. Filter `review_status` to `Needs decision` to find outstanding work. The queue includes unassigned custom references, incomplete decisions and unresolved SAG settings.

Review entries become active through the appropriate configuration file.

| File | Purpose |
| --- | --- |
| `config/manual_reference_overrides.csv` | Explicit custom-reference assignments and mappings from named SAG settings to numeric fields |
| `config/SAG_reference_point_policy_approved.csv` | Optional master policy for reusing reviewed decisions on exactly matching stock, assessment and reference definitions |
| `config/manual_fisheries_guilds.csv` | Regional fisheries-guild assignments for stocks reported with an unknown guild |

Manual reference overrides require `StockKeyLabel`, `AssessmentKey`, `reference_field`, `reference_name`, `Indicator` and `source_issue_note`. `Indicator` must be `Fref` or `Bref`. Optional `setting_token` and `SAGChartKey` fields support named settings. Keep the evidence for each mapping in `source_issue_note`.

Copy approved custom-reference and named-setting mappings into the manual override file. Store reviewed inclusion or exclusion decisions for already assigned references in the master policy. Investigate unresolved numeric slots against the source data. Editing the review queue alone does not activate a reference decision.

Preserve blank reference names and source-issue notes in the audit. Overrides are assessment specific and should be reviewed when a new assessment changes the reference definition.

### Fisheries guilds

`config/manual_fisheries_guilds.csv` uses `EcoregionCode`, `StockKeyLabel` and `FisheriesGuild`. The supported guilds are `benthic`, `demersal`, `elasmobranch`, `pelagic` and `shellfish`.

The output stage uses these assignments to fill unknown guilds where the regional mapping is unambiguous. It retains the reported guild for traceability. Conflicting assignments within an ecoregion remain available for review, and variation between ecoregions is reported separately.

## Running the pipeline

Run commands from the repository root with the package dependencies required by the repository and its boot scripts installed. Core stages use `icesTAF`, `dplyr`, `tidyr`, `tibble` and `jsonlite`. The plotting stage additionally requires its plotting dependencies, including `plotly` and `htmlwidgets`. The installed `dplyr` must support `join_by()`, rolling joins with `closest()` and the `relationship` argument.

The configured default assessment snapshot year is 2026. Set `ICES_ASSESSMENT_YEAR` before boot to select another extraction cycle, and keep the year metadata in `boot/DATA.bib` aligned.

```r
Sys.setenv(ICES_ASSESSMENT_YEAR = "2026")
source("run_all_ecoregions.R")
```

The full runner refreshes boot data with `taf.boot(force = TRUE)`, then runs data, model, output, report and the central review refresh in sequence. The regional shares CSV must be available before the plotting stage. Because review refresh follows `report.R`, a report failure stops the runner before that refresh. The queue can also be refreshed directly after output.

To reuse existing boot inputs, run the stages explicitly.

```r
source("data.R")
source("model.R")
source("output.R")
source("report.R")
source("refresh_reference_review.R")
```

Use the earliest affected stage when repeating work.

| Change | Stages to repeat |
| --- | --- |
| Assessment snapshot or source data | Full runner, including boot |
| Master approved policy | Data, model, output, report and review refresh |
| Manual reference overrides | Model, output, report and review refresh |
| Manual guild assignments or current-status selection | Output, report and review refresh |
| Regional shares or plotting code | `report_2_plotting.R` |
| Review queue only | `refresh_reference_review.R` |

The summary and plotting reports can also be run separately using `source("report_1_summary.R")` and `source("report_2_plotting.R")`.

## Data products and fisheriesXplorer integration

### Current and annual datasets

| Product | Contents | Main use |
| --- | --- | --- |
| `output/stock_status_by_ecoregion.rds` and `.csv` | Validated current status, one row per stock and ecoregion, before regional catch allocation | Consolidated status output and input to the report stage |
| `output/stock_status_unique.rds` and `.csv` | Deduplicated current stocks with a list of their member ecoregions | Cross-region inspection and audit |
| `report/plot_data/current_<code>.rds` | Current F/B status, selected references, provenance, guild and allocated catch quantities | Summary pies, catch-weighted summaries, Kobe plots and stock list |
| `report/plot_data/annual_<code>.rds` | Available annual observations, yearly F/B ratios and statuses, assessment metadata, guild and allocated catch quantities | Status trends, discard trends and assessment metadata for stock-list advice links |

The annual data retain missing observations and the available SAG time series. Plot preparation applies display-specific filtering and calculates arithmetic guild means from stock ratios. Means are calculated for plotting, while the exported annual data preserve the underlying stock records.

For the app, copy the RDS files from `report/plot_data/` into `inst/app/plot_data/`. The stock list uses current status, SID descriptions and annual assessment-year/component metadata for its advice links. Its fishing-pressure and stock-size cells display the selected reference names and use `FStatus` and `BStatus` for their colours.

The plot download handlers select the current or annual file by ecoregion and plot type. They export the complete selected regional dataset as CSV with a disclaimer in a ZIP file. Plotly controls provide figure downloads.

### Interactive HTML plots

The report writes the following selections to `report/html_plots/`.

| Plot | Filename |
| --- | --- |
| Stock-count summary | `stock_summary__<code>.html` |
| Catch-weighted status summary | `ges__<code>.html` |
| Discard trends | `discards__<code>.html` |
| Kobe | `kobe__<code>__<guild>.html` |
| Status trends | `status_trends__<code>__<guild>.html` |

Kobe selections include the five named guilds and `all_stocks`. Status-trend selections include the five named guilds and `all_guilds`. The combined trend view shows guild means, while individual guild views include stock trajectories.

For the current 11-region catalogue, the report attempts 165 selections. Known no-data cases are recorded explicitly. `plot_index.csv` maps successful exports using `plot_type`, `ecoregion_id`, `guild_id` and `file`. `plot_export_report.csv` records every attempted selection as `exported`, `no_data` or `failed`, with a reason.

HTML files use `selfcontained = FALSE` and share a sibling `lib/` directory. Copy the complete `report/html_plots/` contents, including both manifests and `lib/`, into the app's `inst/app/plots/` directory. Restart or redeploy the app to refresh its cached plot catalogue.

## Validation and provenance

The workflow preserves full stock/component identifiers and multiple ecoregion memberships. Stocks with unavailable F or B remain in the current stock universe with the corresponding status marked GREY.

The output stage checks regional coverage, stock identifiers, duplicate stock–ecoregion keys and contradictory assessment/reference values across ecoregions. Review the diagnostics from the current run before publishing its results.

| Diagnostic | Purpose |
| --- | --- |
| `output/coverage_by_ecoregion.csv` | Regional coverage and reference availability |
| `output/failed_regions.csv` | Regional output failures caught by the output stage |
| `output/conflicting_stocks.csv` | Contradictory values for stocks occurring in multiple ecoregions |
| `output/stock_status_by_ecoregion_REVIEW.rds` | Regional snapshot retained for investigation when cross-region conflicts block final consolidation |
| `output/guild_conflicts_within_ecoregion.csv` | Conflicting guild assignments within an ecoregion |
| `output/guild_variations_across_ecoregions.csv` | Guild variation between ecoregions |
| `output/reference_points/` | Candidate, selection, unresolved-setting and reference-provenance diagnostics |

Validation of the shares input checks unique stock–ecoregion–year keys and individual proportions between 0 and 1. Reference availability alone does not establish that the corresponding observation is available or scientifically comparable. Use the status, observation year, assessment key and selected-reference provenance together when interpreting results.
