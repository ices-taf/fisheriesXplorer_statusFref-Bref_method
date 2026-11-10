# Stock-count pies (six guild columns, two F/B rows).
# Extracted from the latest utilities_GES_pies.R.

prepare_precautionary_status_pies <- function(df) {

  required <- c(
    "StockKeyLabel", "FisheriesGuild",
    "FMSY", "MSYBtrigger", "F_FMSY", "SSB_MSYBtrigger"
  )
  missing_cols <- setdiff(required, names(df))
  if (length(missing_cols) > 0) {
    stop("Missing columns: ", paste(missing_cols, collapse = ", "))
  }

  stock_df <- df |>
    dplyr::transmute(
      StockKeyLabel = as.character(StockKeyLabel),
      FisheriesGuild = as.character(FisheriesGuild),
      FMSY = suppressWarnings(as.numeric(as.character(FMSY))),
      MSYBtrigger = suppressWarnings(as.numeric(as.character(MSYBtrigger))),
      F_FMSY = suppressWarnings(as.numeric(as.character(F_FMSY))),
      SSB_MSYBtrigger = suppressWarnings(
        as.numeric(as.character(SSB_MSYBtrigger))
      )
    ) |>
    dplyr::filter(!is.na(StockKeyLabel), nzchar(StockKeyLabel)) |>
    dplyr::mutate(
      FisheriesGuild = dplyr::if_else(
        is.na(FisheriesGuild) | !nzchar(FisheriesGuild),
        "Unknown", FisheriesGuild
      )
    ) |>
    dplyr::distinct()

  if (nrow(stock_df) == 0L) {
    stop("No stocks in the supplied data.")
  }

  # A stock should contribute once per indicator. Do not silently pick
  # between conflicting rows or count it twice.
  duplicated_stocks <- stock_df |>
    dplyr::count(StockKeyLabel, name = "n") |>
    dplyr::filter(n > 1L)

  if (nrow(duplicated_stocks) > 0L) {
    stop(
      "Multiple non-identical rows exist for StockKeyLabel: ",
      paste(utils::head(duplicated_stocks$StockKeyLabel, 8L),
            collapse = ", "),
      ". Supply one row per stock (e.g., the latest assessment)."
    )
  }

  # F/Fref > 1 is red. B/Bref < 1 is red.
  # Exact equality (1) is grouped with GREEN.
  # Missing/invalid references or ratios -> GREY.
  stock_df <- stock_df |>
    dplyr::mutate(
      FStatus = dplyr::case_when(
        !is.finite(FMSY) | FMSY <= 0 ~ "GREY",
        !is.finite(F_FMSY) | F_FMSY < 0 ~ "GREY",
        F_FMSY > 1 ~ "RED",
        TRUE ~ "GREEN"
      ),
      BStatus = dplyr::case_when(
        !is.finite(MSYBtrigger) | MSYBtrigger <= 0 ~ "GREY",
        !is.finite(SSB_MSYBtrigger) | SSB_MSYBtrigger < 0 ~ "GREY",
        SSB_MSYBtrigger < 1 ~ "RED",
        TRUE ~ "GREEN"
      ),
      FReason = dplyr::case_when(
        !is.finite(FMSY) | FMSY <= 0 ~ "F reference unavailable",
        !is.finite(F_FMSY) | F_FMSY < 0 ~ "F/Fref unavailable",
        F_FMSY > 1 ~ "Above F reference",
        TRUE ~ "At or below F reference"
      ),
      BReason = dplyr::case_when(
        !is.finite(MSYBtrigger) | MSYBtrigger <= 0 ~
          "Biomass reference unavailable",
        !is.finite(SSB_MSYBtrigger) | SSB_MSYBtrigger < 0 ~
          "B/Bref unavailable",
        SSB_MSYBtrigger < 1 ~ "Below biomass reference",
        TRUE ~ "At or above biomass reference"
      )
    )

  long_df <- dplyr::bind_rows(
    stock_df |>
      dplyr::transmute(
        StockKeyLabel, FisheriesGuild,
        Indicator = "Fref", Colour = FStatus,
        Ratio = F_FMSY, Reference = FMSY, Reason = FReason
      ),
    stock_df |>
      dplyr::transmute(
        StockKeyLabel, FisheriesGuild,
        Indicator = "Bref", Colour = BStatus,
        Ratio = SSB_MSYBtrigger, Reference = MSYBtrigger,
        Reason = BReason
      )
  )

  # Duplicate into Total AFTER deduplication: each stock counted once
  # in its guild and once in Total.
  long_df <- dplyr::bind_rows(
    long_df,
    dplyr::mutate(long_df, FisheriesGuild = "Total")
  )

  long_df |>
    dplyr::group_by(FisheriesGuild, Indicator, Colour) |>
    dplyr::summarise(
      COUNT = dplyr::n(),
      stock_ids = list(sort(StockKeyLabel)),
      .groups = "drop"
    ) |>
    dplyr::group_by(FisheriesGuild, Indicator) |>
    dplyr::mutate(
      total_stocks = sum(COUNT),
      pct = 100 * COUNT / total_stocks
    ) |>
    dplyr::ungroup()
}


plot_status_prop_pies_plotly_v2 <- function(
    df,
    return_data = FALSE,
    width_px = 800,
    max_hover_stocks = 25
) {

  counts <- prepare_precautionary_status_pies(df)
  if (isTRUE(return_data)) return(counts)

  if (!is.numeric(width_px) || length(width_px) != 1L ||
      !is.finite(width_px) || width_px <= 0) {
    width_px <- 800
  }

  if (!is.numeric(max_hover_stocks) || length(max_hover_stocks) != 1L ||
      !is.finite(max_hover_stocks) || max_hover_stocks < 1) {
    stop("max_hover_stocks must be a positive number.")
  }
  max_hover_stocks <- as.integer(max_hover_stocks)

  status_colors <- c(
    GREEN = "#00B26D",
    RED   = "#d93b1c",
    GREY  = "#d3d3d3"
  )

  indicators <- c("Fref", "Bref")
  headers <- c(
    Fref = "Fishing pressure (F/Fref)",
    Bref = "Stock size (B/Bref)"
  )

  # ---------------------------------------------------------
  # Helper labels for guilds
  # ---------------------------------------------------------

  guild_display_name <- function(g) {
    g <- tolower(trimws(g))
    if (g == "total") return("Total")
    tools::toTitleCase(g)
  }

  # Standardise guild names for plotting
  counts <- counts |>
    dplyr::mutate(
      GuildPlot = tolower(trimws(as.character(FisheriesGuild)))
    )

  # Remove Unknown from displayed guilds only
  counts_plot <- counts |>
    dplyr::filter(GuildPlot != "unknown")

  # Fixed desired order
  desired_guilds <- c(
    "benthic",
    "demersal",
    "elasmobranch",
    "pelagic",
    "shellfish",
    "total"
  )

  present_guilds <- unique(as.character(counts_plot$GuildPlot))
  guilds <- c(
    intersect(desired_guilds, present_guilds),
    sort(setdiff(present_guilds, desired_guilds))
  )

  format_stocks <- function(ids) {
    shown <- utils::head(ids, max_hover_stocks)
    out <- paste(shown, collapse = "<br>")
    n_more <- length(ids) - length(shown)
    if (n_more > 0L) {
      out <- paste0(out, "<br>... and ", n_more, " more")
    }
    out
  }

  counts_plot$hover_text <- vapply(seq_len(nrow(counts_plot)), function(i) {
    paste0(
      "<b>", guild_display_name(counts_plot$GuildPlot[[i]]), " — ",
      headers[[counts_plot$Indicator[[i]]]], "</b><br>",
      "Status: ", counts_plot$Colour[[i]], "<br>",
      "Stocks: ", counts_plot$COUNT[[i]], " / ", counts_plot$total_stocks[[i]],
      " (", sprintf("%.1f", counts_plot$pct[[i]]), "%)<br><br>",
      "<b>Stocks</b><br>", format_stocks(counts_plot$stock_ids[[i]])
    )
  }, character(1))

  # ---------------------------------------------------------
  # Layout:
  #   columns = guilds
  #   rows    = indicators
  # ---------------------------------------------------------

  n_rows <- length(indicators)
  n_cols <- length(guilds)

  top <- 0.88
  bottom <- 0.10
  left <- 0.12
  right <- 0.95

  # Height/spacing adjustments requested
  row_gap <- 0.16
  col_gap <- min(0.025, 0.12 / max(1, n_cols))

  row_height <- (top - bottom - (n_rows - 1L) * row_gap) / n_rows
  col_width  <- (right - left - (n_cols - 1L) * col_gap) / n_cols

  p <- plotly::plot_ly()
  ann <- list()

  # ---------------------------------------------------------
  # Stable legend
  # ---------------------------------------------------------

  for (status in names(status_colors)) {
    p <- plotly::add_trace(
      p,
      type = "scatter",
      mode = "markers",
      x = -1, y = -1,
      name = status,
      marker = list(color = status_colors[[status]], size = 11),
      legendgroup = status,
      showlegend = TRUE,
      hoverinfo = "skip",
      inherit = FALSE
    )
  }

  # ---------------------------------------------------------
  # Add pies
  # ---------------------------------------------------------

  for (i in seq_along(indicators)) {

    y1 <- top - (i - 1L) * (row_height + row_gap)
    y0 <- y1 - row_height

    for (j in seq_along(guilds)) {

      x0 <- left + (j - 1L) * (col_width + col_gap)
      x1 <- x0 + col_width

      sub <- counts_plot[
        counts_plot$GuildPlot == guilds[[j]] &
          counts_plot$Indicator == indicators[[i]],
        , drop = FALSE
      ]

      if (nrow(sub) == 0L) {
        ann[[length(ann) + 1L]] <- list(
          x = (x0 + x1) / 2,
          y = (y0 + y1) / 2,
          xref = "paper", yref = "paper",
          text = "No data",
          showarrow = FALSE,
          font = list(size = 10, color = "#666666")
        )
        next
      }

      sub <- sub[match(c("GREEN", "RED", "GREY"), sub$Colour), , drop = FALSE]
      sub <- sub[!is.na(sub$Colour), , drop = FALSE]

      p <- plotly::add_trace(
        p,
        type = "pie",
        labels = as.character(sub$Colour),
        values = as.numeric(sub$COUNT),
        text = as.character(sub$COUNT),
        customdata = as.character(sub$hover_text),
        hovertemplate = "%{customdata}<extra></extra>",
        textinfo = "text",
        textposition = "inside",
        insidetextorientation = "horizontal",
        sort = FALSE,
        direction = "clockwise",
        marker = list(
          colors = unname(status_colors[as.character(sub$Colour)]),
          line = list(color = "#FFFFFF", width = 1)
        ),
        domain = list(x = c(x0, x1), y = c(y0, y1)),
        showlegend = FALSE,
        inherit = FALSE
      )

      # total stocks under each pie
      pie_total <- unique(sub$total_stocks)
      pie_total <- pie_total[!is.na(pie_total)]

      if (length(pie_total) == 0L) {
        pie_total <- NA_integer_
      } else {
        pie_total <- pie_total[[1]]
      }

      ann[[length(ann) + 1L]] <- list(
        x = (x0 + x1) / 2,
        y = y0 - 0.025,
        xref = "paper", yref = "paper",
        text = paste0("total = ", pie_total, " stocks"),
        showarrow = FALSE,
        font = list(size = 10),
        yanchor = "top"
      )
    }
  }

  # ---------------------------------------------------------
  # Column headers = guilds
  # ---------------------------------------------------------

  for (j in seq_along(guilds)) {
    x0 <- left + (j - 1L) * (col_width + col_gap)
    x1 <- x0 + col_width

    ann[[length(ann) + 1L]] <- list(
      x = (x0 + x1) / 2,
      y = 0.955,
      xref = "paper", yref = "paper",
      xanchor = "center", yanchor = "middle",
      text = paste0("<b>", guild_display_name(guilds[[j]]), "</b>"),
      showarrow = FALSE,
      font = list(size = 12)
    )
  }

  # ---------------------------------------------------------
  # Row headers = indicators
  # ---------------------------------------------------------

  for (i in seq_along(indicators)) {
    y1 <- top - (i - 1L) * (row_height + row_gap)
    y0 <- y1 - row_height

    ann[[length(ann) + 1L]] <- list(
      x = 0.05,
      y = (y0 + y1) / 2,
      xref = "paper", yref = "paper",
      xanchor = "right", yanchor = "middle",
      text = paste0("<b>", headers[[indicators[[i]]]], "</b>"),
      showarrow = FALSE,
      font = list(size = 12),
      textangle = -90
    )
  }

  # footer
  ann[[length(ann) + 1L]] <- list(
    x = 1, y = -0.05,
    xref = "paper", yref = "paper",
    xanchor = "right", yanchor = "top",
    text = paste0(
      "ICES Stock Assessment Database, ",
      format(Sys.Date(), "%d-%b-%y"),
      ". ICES, Copenhagen"
    ),
    showarrow = FALSE,
    font = list(size = 10)
  )

  # Height adjustment requested
  plot_height <- max(700, min(1100, round(width_px * 0.75)))

  p <- plotly::layout(
    p,
    height = plot_height,
    showlegend = FALSE,
    paper_bgcolor = "white",
    plot_bgcolor = "white",
    # legend = list(
    #   title = list(text = "Status"),
    #   x = 1.01, y = 1,
    #   xanchor = "left", yanchor = "top",
    #   bgcolor = "rgba(255,255,255,0.85)"
    # ),
    margin = list(l = 95, r = 115, t = 65, b = 85),
    xaxis = list(visible = FALSE, range = c(0, 1), fixedrange = TRUE),
    yaxis = list(visible = FALSE, range = c(0, 1), fixedrange = TRUE),
    annotations = ann
  )

  plotly::config(
    p,
    toImageButtonOptions = list(
      filename = paste0("Status_pies_", format(Sys.Date(), "%d-%b-%y")),
      format = "png",
      scale = 3
    )
  )
}

# Catch/landing-weighted GES pies (six guild columns, two F/B rows).
# Extracted from the latest utilities_GES_pies.R.

plot_GES_pies_plotly_v2 <- function(
    x,
    y = NULL,
    return_data = FALSE,
    width_px = 800,
    max_hover_stocks = 25
) {

  status_colors <- c(
    GREEN = "#00B26D",
    RED   = "#d93b1c",
    GREY  = "#d3d3d3"
  )

  required_x <- c(
    "StockKeyLabel", "FisheriesGuild", "FMSY", "MSYBtrigger",
    "F_FMSY", "SSB_MSYBtrigger"
  )
  missing_x <- setdiff(required_x, names(x))
  if (length(missing_x) > 0L) {
    stop("Missing columns in x: ", paste(missing_x, collapse = ", "))
  }

  if (is.null(y)) y <- x
  if (!"StockKeyLabel" %in% names(y)) {
    stop("y must contain StockKeyLabel.")
  }
  if (!any(c("Catches_in_ecoregion", "Landings_in_ecoregion") %in% names(y))) {
    stop("y (or x if y is NULL) must contain Catches_in_ecoregion ",
         "and/or Landings_in_ecoregion.")
  }
  if (!"Catches_in_ecoregion" %in% names(y)) {
    y$Catches_in_ecoregion <- NA_real_
  }
  if (!"Landings_in_ecoregion" %in% names(y)) {
    y$Landings_in_ecoregion <- NA_real_
  }

  to_number <- function(z) suppressWarnings(as.numeric(as.character(z)))
  clean_nonnegative <- function(z) {
    z <- to_number(z)
    z[!is.finite(z) | z < 0] <- NA_real_
    z
  }
  format_tonnes <- function(z) {
    format(round(z, 0), big.mark = ",", scientific = FALSE, trim = TRUE)
  }
  escape_html <- function(z) {
    z <- gsub("&", "&amp;", z, fixed = TRUE)
    z <- gsub("<", "&lt;", z, fixed = TRUE)
    gsub(">", "&gt;", z, fixed = TRUE)
  }

  # ---------------------------------------------------------
  # 1. Calculate INDEPENDENT precautionary F and B status.
  #    Do not use the original SAG FishingPressure/StockSize status.
  # ---------------------------------------------------------
  stock_df <- x |>
    dplyr::transmute(
      StockKeyLabel = as.character(StockKeyLabel),
      FisheriesGuild = as.character(FisheriesGuild),
      FMSY = to_number(FMSY),
      MSYBtrigger = to_number(MSYBtrigger),
      F_FMSY = to_number(F_FMSY),
      SSB_MSYBtrigger = to_number(SSB_MSYBtrigger)
    ) |>
    dplyr::filter(!is.na(StockKeyLabel), nzchar(StockKeyLabel)) |>
    dplyr::mutate(
      FisheriesGuild = dplyr::if_else(
        is.na(FisheriesGuild) | !nzchar(FisheriesGuild),
        "Unknown", FisheriesGuild
      )
    ) |>
    dplyr::distinct()

  if (nrow(stock_df) == 0L) stop("No stocks in x.")

  # Exact duplicate rows are harmless, but differing records for the
  # same stock should not be joined/counted twice.
  repeated <- stock_df |>
    dplyr::count(StockKeyLabel, name = "n") |>
    dplyr::filter(n > 1L)
  if (nrow(repeated) > 0L) {
    stop("Conflicting rows for StockKeyLabel in x: ",
         paste(utils::head(repeated$StockKeyLabel, 8L), collapse = ", "),
         ". Supply one assessed row per stock.")
  }

  stock_df <- stock_df |>
    dplyr::mutate(
      F_Status = dplyr::case_when(
        !is.finite(FMSY) | FMSY <= 0 ~ "GREY",
        !is.finite(F_FMSY) | F_FMSY < 0 ~ "GREY",
        F_FMSY > 1 ~ "RED",
        TRUE ~ "GREEN"
      ),
      B_Status = dplyr::case_when(
        !is.finite(MSYBtrigger) | MSYBtrigger <= 0 ~ "GREY",
        !is.finite(SSB_MSYBtrigger) | SSB_MSYBtrigger < 0 ~ "GREY",
        SSB_MSYBtrigger < 1 ~ "RED",
        TRUE ~ "GREEN"
      )
    )

  # ---------------------------------------------------------
  # 2. Attach ecoregion catches, falling back to landings.
  #    Missing catch weights are not converted into status GREY.
  # ---------------------------------------------------------
  catch_df <- y |>
    dplyr::transmute(
      StockKeyLabel = as.character(StockKeyLabel),
      catch_value = clean_nonnegative(Catches_in_ecoregion),
      landing_value = clean_nonnegative(Landings_in_ecoregion)
    ) |>
    dplyr::filter(!is.na(StockKeyLabel), nzchar(StockKeyLabel)) |>
    dplyr::mutate(
      CATCH = dplyr::coalesce(catch_value, landing_value),
      CatchSource = dplyr::case_when(
        !is.na(catch_value) ~ "Catches",
        !is.na(landing_value) ~ "Landings",
        TRUE ~ "Missing"
      )
    ) |>
    dplyr::select(StockKeyLabel, CATCH, CatchSource) |>
    dplyr::distinct()

  repeated_catch <- catch_df |>
    dplyr::count(StockKeyLabel, name = "n") |>
    dplyr::filter(n > 1L)
  if (nrow(repeated_catch) > 0L) {
    stop("Conflicting catch/landing records in y for: ",
         paste(utils::head(repeated_catch$StockKeyLabel, 8L),
               collapse = ", "),
         ". Resolve these before plotting to avoid double counting.")
  }

  stock_df <- stock_df |>
    dplyr::left_join(catch_df, by = "StockKeyLabel")

  # ---------------------------------------------------------
  # 3. Long data: each stock contributes to BOTH indicators.
  # ---------------------------------------------------------
  long_df <- dplyr::bind_rows(
    stock_df |>
      dplyr::transmute(
        StockKeyLabel, FisheriesGuild, CATCH, CatchSource,
        Indicator = "Fref", Colour = F_Status
      ),
    stock_df |>
      dplyr::transmute(
        StockKeyLabel, FisheriesGuild, CATCH, CatchSource,
        Indicator = "Bref", Colour = B_Status
      )
  )

  # Total row is assembled from the same stocks, once per indicator.
  long_df <- dplyr::bind_rows(
    long_df,
    dplyr::mutate(long_df, FisheriesGuild = "Total")
  )

  # Note exclusions for transparency. Stocks with zero or unknown catch
  # cannot contribute to a catch-weighted pie, irrespective of status.
  coverage <- long_df |>
    dplyr::group_by(FisheriesGuild, Indicator) |>
    dplyr::summarise(
      n_stocks = dplyr::n(),
      excluded_stocks = sum(!is.finite(CATCH) | CATCH <= 0),
      .groups = "drop"
    )

  weighted <- long_df |>
    dplyr::filter(is.finite(CATCH), CATCH > 0)

  if (nrow(weighted) == 0L) {
    stop("No stocks have positive Catches_in_ecoregion or ",
         "Landings_in_ecoregion values.")
  }

  # Stock-level hover text within each status slice.
  if (!is.numeric(max_hover_stocks) || length(max_hover_stocks) != 1L ||
      !is.finite(max_hover_stocks) || max_hover_stocks < 1) {
    stop("max_hover_stocks must be a positive number.")
  }
  max_hover_stocks <- as.integer(max_hover_stocks)
  bar_width <- 20L

  build_breakdown <- function(stock_ids, catches, sources) {
    ix <- order(catches, decreasing = TRUE)
    stock_ids <- stock_ids[ix]
    catches <- catches[ix]
    sources <- sources[ix]
    total <- sum(catches)
    show <- seq_len(min(length(stock_ids), max_hover_stocks))
    lines <- vapply(show, function(i) {
      pct <- 100 * catches[[i]] / total
      paste0(
        escape_html(stock_ids[[i]]), ": ",
        strrep("\u2588", max(1L, round(bar_width * pct / 100))),
        " ", sprintf("%.1f%%", pct),
        " (", format_tonnes(catches[[i]]), " t; ",
        sources[[i]], ")"
      )
    }, character(1))
    additional <- length(stock_ids) - length(show)
    if (additional > 0L) {
      lines <- c(lines, paste0("... and ", additional, " more stocks"))
    }
    paste(lines, collapse = "<br>")
  }

  slices <- weighted |>
    dplyr::group_by(FisheriesGuild, Indicator, Colour) |>
    dplyr::summarise(
      Catch = sum(CATCH),
      COUNT = dplyr::n(),
      breakdown_html = build_breakdown(StockKeyLabel, CATCH, CatchSource),
      stock_ids = list(StockKeyLabel),
      .groups = "drop"
    ) |>
    dplyr::group_by(FisheriesGuild, Indicator) |>
    dplyr::mutate(
      total_catch = sum(Catch),
      pct = 100 * Catch / total_catch
    ) |>
    dplyr::ungroup() |>
    dplyr::left_join(coverage, by = c("FisheriesGuild", "Indicator"))

  headers <- c(
    Fref = "Fishing pressure (F/Fref)",
    Bref = "Stock size (B/Bref)"
  )

  slices$hover_text <- vapply(seq_len(nrow(slices)), function(i) {
    paste0(
      "<b>", escape_html(slices$FisheriesGuild[[i]]), " — ",
      headers[[slices$Indicator[[i]]]], "</b><br>",
      "Status: ", slices$Colour[[i]], "<br>",
      "Catch / landings: ", format_tonnes(slices$Catch[[i]]), " t",
      " (", sprintf("%.1f%%", slices$pct[[i]]), ")<br>",
      "Stocks in slice: ", slices$COUNT[[i]], "<br>",
      "Stocks excluded from this guild (no positive catch): ",
      slices$excluded_stocks[[i]], "<br><br>",
      "<b>Catch by stock</b><br>", slices$breakdown_html[[i]]
    )
  }, character(1))

  if (isTRUE(return_data)) return(slices)

  # ---------------------------------------------------------
  # 4. Plotly layout: guilds as COLUMNS, F/B as TWO ROWS.
  #    Matches the orientation of the original GES pies.
  # ---------------------------------------------------------
  if (!is.numeric(width_px) || length(width_px) != 1L ||
      !is.finite(width_px) || width_px <= 0) {
    width_px <- 800
  }

  # Display guilds left to right, with Total at the end.
  requested_guilds <- c(
    "benthic", "demersal", "elasmobranch",
    "pelagic", "shellfish", "Total"
  )
  actual_guilds <- unique(as.character(slices$FisheriesGuild))
  guilds <- c(
    intersect(requested_guilds, actual_guilds),
    sort(setdiff(actual_guilds, requested_guilds))
  )
  # Hide the Unknown guild column. Keep its stocks in the Total pies.
  guilds <- guilds[tolower(trimws(guilds)) != "unknown"]

  indicators <- c("Fref", "Bref")

  n_cols <- length(guilds)
  n_rows <- length(indicators)

  # Domains are expressed as fractions of the plotting area (0-1).
  # A little extra space between rows accommodates catch-total labels.
  left <- 0.065
  right <- 0.975
  col_gap <- if (n_cols <= 3L) 0.045 else 0.018
  col_width <- (right - left - (n_cols - 1L) * col_gap) / n_cols

  top <- 0.90
  bottom <- 0.12
  row_gap <- 0.16
  row_height <- (top - bottom - (n_rows - 1L) * row_gap) / n_rows

  p <- plotly::plot_ly()
  annotations <- list()

  # Explicit status legend (one entry for each possible colour).
  for (status in names(status_colors)) {
    p <- plotly::add_trace(
      p,
      x = -1, y = -1,
      type = "scatter", mode = "markers",
      name = status,
      marker = list(color = status_colors[[status]], size = 11),
      visible = "legendonly", showlegend = TRUE,
      legendgroup = status, hoverinfo = "skip", inherit = FALSE
    )
  }

  # Two ROWS: fishing pressure (top), biomass (bottom).
  for (i in seq_along(indicators)) {
    y1 <- top - (i - 1L) * (row_height + row_gap)
    y0 <- y1 - row_height

    # Indicator label on the left side of the row.
    annotations[[length(annotations) + 1L]] <- list(
      x = left - 0.030,
      y = mean(c(y0, y1)),
      xref = "paper", yref = "paper",
      xanchor = "right", yanchor = "middle",
      text = paste0("<b>", headers[[indicators[[i]]]], "</b>"),
      textangle = -90,
      showarrow = FALSE, font = list(size = 12)
    )

    # Guilds run across the COLUMNS.
    for (j in seq_along(guilds)) {
      x0 <- left + (j - 1L) * (col_width + col_gap)
      x1 <- x0 + col_width

      sub <- slices[
        slices$FisheriesGuild == guilds[[j]] &
          slices$Indicator == indicators[[i]],
        , drop = FALSE
      ]

      if (nrow(sub) == 0L) {
        annotations[[length(annotations) + 1L]] <- list(
          x = mean(c(x0, x1)), y = mean(c(y0, y1)),
          xref = "paper", yref = "paper",
          text = "No catch data", showarrow = FALSE,
          font = list(size = 10, color = "#666666")
        )
        next
      }

      # Keep a consistent clockwise status order in all pies.
      sub <- sub[match(c("GREEN", "RED", "GREY"), sub$Colour), , drop = FALSE]
      sub <- sub[!is.na(sub$Colour), , drop = FALSE]

      p <- plotly::add_trace(
        p,
        type = "pie",
        labels = as.character(sub$Colour),
        values = as.numeric(sub$Catch),
        customdata = as.character(sub$hover_text),
        hovertemplate = "%{customdata}<extra></extra>",
        textinfo = "label+percent",
        textposition = "auto",
        insidetextorientation = "horizontal",
        sort = FALSE,
        direction = "clockwise",
        marker = list(
          colors = unname(status_colors[as.character(sub$Colour)]),
          line = list(color = "#FFFFFF", width = 1)
        ),
        domain = list(x = c(x0, x1), y = c(y0, y1)),
        showlegend = FALSE,
        inherit = FALSE
      )

      # A total underneath each pie, as in the original guild-by-column plot.
      guild_catch <- unique(sub$total_catch)
      annotations[[length(annotations) + 1L]] <- list(
        x = mean(c(x0, x1)), y = y0 - 0.012,
        xref = "paper", yref = "paper",
        xanchor = "center", yanchor = "top",
        text = paste0(
          "total = ", format(round(guild_catch[[1]] / 1000, 0),
                              big.mark = ",", scientific = FALSE), "k t"
        ),
        showarrow = FALSE, font = list(size = 10)
      )
    }
  }

  # Guild names as COLUMN headers.
  for (j in seq_along(guilds)) {
    x0 <- left + (j - 1L) * (col_width + col_gap)
    annotations[[length(annotations) + 1L]] <- list(
      x = x0 + col_width / 2, y = 0.965,
      xref = "paper", yref = "paper",
      text = paste0(
        "<b>", escape_html(tools::toTitleCase(tolower(guilds[[j]]))), "</b>"
      ),
      showarrow = FALSE, font = list(size = 12)
    )
  }

  annotations[[length(annotations) + 1L]] <- list(
    x = 1, y = -0.04,
    xref = "paper", yref = "paper",
    xanchor = "right", yanchor = "top", showarrow = FALSE,
    text = paste0(
      "ICES Stock Assessment Database, ",
      format(Sys.Date(), "%d-%b-%y"), ". ICES, Copenhagen"
    ),
    font = list(size = 10)
  )

  # Only two rows now, so the chart can be much shorter than before.
  plot_height <- max(700, min(820, round(width_px * 0.50)))

  p <- plotly::layout(
    p,
    height = plot_height,
    paper_bgcolor = "white", plot_bgcolor = "white",
    # legend = list(
    #   title = list(text = "Status"),
    #   x = 1.01, y = 1, xanchor = "left", yanchor = "top",
    #   bgcolor = "rgba(255,255,255,0.90)"
    # ),
    margin = list(l = 100, r = 120, t = 65, b = 75),
    xaxis = list(visible = FALSE, range = c(0, 1), fixedrange = TRUE),
    yaxis = list(visible = FALSE, range = c(0, 1), fixedrange = TRUE),
    annotations = annotations
  )

  plotly::config(
    p,
    toImageButtonOptions = list(
      filename = paste0("GES_pies_precautionary_", format(Sys.Date(), "%d-%b-%y")),
      format = "png", scale = 3
    )
  )
}



# Kobe v3 with capped Unknown panel and responsive autosizing.
# Extracted from the latest utilities_GES_pies.R.

plot_kobe_app_plotly_v3 <- function(
    x,
    guild,
    return_data = FALSE,
    size_by_catches = FALSE,
    min_point_size = 8,
    max_point_size = 25,
    n_size_legend = 4,
    unknown_row_px = 26,
    max_unknown_stocks = 12
) {

  # 1. Validate input and filter stocks.
  if (!is.numeric(min_point_size) || length(min_point_size) != 1L ||
      !is.finite(min_point_size) || min_point_size <= 0 ||
      !is.numeric(max_point_size) || length(max_point_size) != 1L ||
      !is.finite(max_point_size) || max_point_size <= min_point_size) {
    stop("Require 0 < min_point_size < max_point_size.")
  }
  if (!is.numeric(unknown_row_px) || length(unknown_row_px) != 1L ||
      !is.finite(unknown_row_px) || unknown_row_px <= 0) {
    stop("unknown_row_px must be a positive number.")
  }

  if (!is.numeric(max_unknown_stocks) || length(max_unknown_stocks) != 1L ||
      is.na(max_unknown_stocks) || max_unknown_stocks < 1 ||
      (!is.infinite(max_unknown_stocks) &&
       max_unknown_stocks != floor(max_unknown_stocks))) {
    stop("max_unknown_stocks must be a positive integer or Inf.")
  }

  required_cols <- c(
    "StockKeyLabel", "FisheriesGuild", "F_FMSY", "SSB_MSYBtrigger",
    "Catches_in_ecoregion", "Landings_in_ecoregion"
  )
  missing_cols <- setdiff(required_cols, names(x))
  if (length(missing_cols) > 0L) {
    stop("Missing input columns: ", paste(missing_cols, collapse = ", "))
  }

  df <- if (identical(guild, "All")) {
    x
  } else {
    dplyr::filter(x, FisheriesGuild %in% guild)
  }
  if (nrow(df) == 0L) stop("No stocks for the selected fisheries guild(s).")

  to_number <- function(z) suppressWarnings(as.numeric(as.character(z)))
  for (nm in c("F_FMSY", "SSB_MSYBtrigger",
               "Catches_in_ecoregion", "Landings_in_ecoregion")) {
    df[[nm]] <- to_number(df[[nm]])
  }

  # 2. Standard/proxy point flags.
  if (!"F_proxy" %in% names(df)) df$F_proxy <- FALSE
  if (!"B_proxy" %in% names(df)) df$B_proxy <- FALSE
  df <- dplyr::mutate(
    df,
    ProxyFlag = dplyr::if_else(
      (F_proxy %in% TRUE) | (B_proxy %in% TRUE),
      "Proxy reference point", "Reference point"
    )
  )

  # 3. Classify using coordinates, not SAG's old Status column.
  # Negative ratios are invalid too: they belong in the unknown panel.
  valid_f <- is.finite(df$F_FMSY) & df$F_FMSY >= 0
  valid_b <- is.finite(df$SSB_MSYBtrigger) & df$SSB_MSYBtrigger >= 0
  known <- valid_f & valid_b

  df$KobeStatus <- dplyr::case_when(
    !known ~ "Unknown",
    df$F_FMSY <= 1 & df$SSB_MSYBtrigger >= 1 ~ "Green",
    df$F_FMSY > 1 & df$SSB_MSYBtrigger < 1 ~ "Red",
    TRUE ~ "Orange"
  )
  df$KobePanel <- ifelse(known, "Kobe", "Unknown")
  df$UnknownReason <- dplyr::case_when(
    !valid_f & !valid_b ~ "F/Fref and B/Bref unavailable or invalid",
    !valid_f ~ "F/Fref unavailable or invalid",
    !valid_b ~ "B/Bref unavailable or invalid",
    TRUE ~ ""
  )
  df$KobeDescription <- dplyr::case_when(
    df$KobeStatus == "Green" ~
      "Biomass above threshold; fishing pressure sustainable",
    df$KobeStatus == "Red" ~
      "Biomass below threshold; overfishing occurring",
    df$KobeStatus == "Orange" & df$F_FMSY > 1 ~
      "Biomass above threshold; overfishing occurring",
    df$KobeStatus == "Orange" & df$SSB_MSYBtrigger < 1 ~
      "Biomass below threshold; fishing pressure below threshold",
    TRUE ~ "Status unknown"
  )

  status_pal <- c(
    Green = "#4daf4a", Orange = "#ff8c00",
    Red = "#e41a1c", Unknown = "#d3d3d3"
  )
  df$status_hex <- unname(status_pal[df$KobeStatus])

  # 4. Catch/landings fallback (0 remains a valid reported quantity).
  catch_value <- df$Catches_in_ecoregion
  landing_value <- df$Landings_in_ecoregion
  catch_value[!is.finite(catch_value) | catch_value < 0] <- NA_real_
  landing_value[!is.finite(landing_value) | landing_value < 0] <- NA_real_
  df$CatchSizeValue <- dplyr::coalesce(catch_value, landing_value)
  df$CatchSizeSource <- dplyr::case_when(
    !is.na(catch_value) ~ "Catches in ecoregion",
    !is.na(landing_value) ~ "Landings in ecoregion",
    TRUE ~ "No catch/landing data"
  )

  # 5. ONE size scale for both panels (including unknown-status stocks).
  size_values <- df$CatchSizeValue[is.finite(df$CatchSizeValue)]
  c_min <- if (length(size_values)) min(size_values) else NA_real_
  c_max <- if (length(size_values)) max(size_values) else NA_real_

  scale_catches <- function(values) {
    out <- rep(min_point_size, length(values))
    good <- is.finite(values) & values >= 0
    if (!any(good) || !is.finite(c_min)) return(out)
    if (c_min == c_max) {
      out[good] <- (min_point_size + max_point_size) / 2
    } else {
      out[good] <- min_point_size +
        (sqrt(values[good]) - sqrt(c_min)) /
        (sqrt(c_max) - sqrt(c_min)) *
        (max_point_size - min_point_size)
    }
    pmax(min_point_size, pmin(max_point_size, out))
  }

  df$point_size <- if (isTRUE(size_by_catches)) {
    scale_catches(df$CatchSizeValue)
  } else {
    rep(12, nrow(df))
  }
  if (isTRUE(return_data)) return(df)

  # 6. Derive linear main-axis limits from stocks with both ratios.
  plot_df <- df[known, , drop = FALSE]
  unknown_df <- df[!known, , drop = FALSE]
  n_unknown_total <- nrow(unknown_df)
  has_unknown <- n_unknown_total > 0L
  # All unknown stocks remain in df; only the right-panel display is capped.
  n_unknown_shown <- as.integer(min(n_unknown_total, max_unknown_stocks))

  if (nrow(plot_df) > 0L) {
    xmin <- min(c(plot_df$F_FMSY, 1))
    xmax <- max(c(plot_df$F_FMSY, 1))
    ymin <- min(c(plot_df$SSB_MSYBtrigger, 1))
    ymax <- max(c(plot_df$SSB_MSYBtrigger, 1))
    xpad <- max(0.1, (xmax - xmin) * 0.10)
    ypad <- max(0.1, (ymax - ymin) * 0.10)
    x_range <- c(max(0, xmin - xpad), xmax + xpad)
    y_range <- c(max(0, ymin - ypad), ymax + ypad)
  } else {
    x_range <- c(0, 2)
    y_range <- c(0, 2)
  }

  # 7. Hover text, with original values retained.
  html_safe <- function(z) {
    z <- ifelse(is.na(z), "NA", as.character(z))
    z <- gsub("&", "&amp;", z, fixed = TRUE)
    z <- gsub("<", "&lt;", z, fixed = TRUE)
    gsub(">", "&gt;", z, fixed = TRUE)
  }
  format_quantity <- function(v) {
    ifelse(
      is.finite(v),
      format(round(v, 0), big.mark = ",", scientific = FALSE, trim = TRUE),
      "NA"
    )
  }
  format_ratio <- function(v) {
    ifelse(is.finite(v) & v >= 0, sprintf("%.2f", v), "NA")
  }

  df$hover_text <- paste0(
    "<b>", html_safe(df$StockKeyLabel), "</b><br>",
    "F/Fref: ", format_ratio(df$F_FMSY), "<br>",
    "B/Bref: ", format_ratio(df$SSB_MSYBtrigger), "<br>",
    "Kobe status: ", df$KobeStatus, "<br>",
    ifelse(df$KobeStatus == "Unknown", df$UnknownReason, df$KobeDescription),
    "<br>", df$CatchSizeSource, ": ", format_quantity(df$CatchSizeValue),
    " t"
  )
  plot_df <- df[known, , drop = FALSE]
  unknown_df <- df[!known, , drop = FALSE]

  p <- plotly::plot_ly()
  stock_annotations <- list()

  # 8. Main Kobe markers (same symbols, colours, and labels as v2).
  add_kobe_points <- function(plot, data, symbol, line_width, opacity) {
    if (nrow(data) == 0L) return(plot)

    for (status_name in unique(data$KobeStatus)) {
      sub <- data[data$KobeStatus == status_name, , drop = FALSE]
      col <- status_pal[[status_name]]
      marker_line <- if (identical(symbol, "circle-open")) {
        list(color = col, width = line_width)
      } else {
        list(color = "#333333", width = 0.1)
      }

      plot <- plotly::add_markers(
        plot,
        data = sub,
        x = ~F_FMSY, y = ~SSB_MSYBtrigger,
        marker = list(
          symbol = symbol, size = sub$point_size,
          color = col, line = marker_line
        ),
        customdata = ~hover_text,
        hovertemplate = "%{customdata}<extra></extra>",
        showlegend = FALSE, opacity = opacity, inherit = FALSE
      )

      for (k in seq_len(nrow(sub))) {
        # In the split view, place labels for right-edge stocks to their
        # left, reducing spillover into the unknown-status panel.
        at_right_edge <- has_unknown &&
          sub$F_FMSY[[k]] > (x_range[[2]] - 0.20 * diff(x_range))
        label_shift <- sub$point_size[[k]] / 2 + 8

        stock_annotations[[length(stock_annotations) + 1L]] <<- list(
          x = sub$F_FMSY[[k]], y = sub$SSB_MSYBtrigger[[k]],
          xref = "x", yref = "y",
          text = html_safe(sub$StockKeyLabel[[k]]),
          xanchor = if (at_right_edge) "right" else "left",
          yanchor = "middle",
          xshift = if (at_right_edge) -label_shift else label_shift,
          showarrow = FALSE,
          font = list(size = 10, color = "#333333")
        )
      }
    }
    plot
  }

  p <- add_kobe_points(
    p, plot_df[plot_df$ProxyFlag == "Reference point", , drop = FALSE],
    "circle", 0.1, 0.75
  )
  p <- add_kobe_points(
    p, plot_df[plot_df$ProxyFlag == "Proxy reference point", , drop = FALSE],
    "circle-open", 2, 1
  )

  # 9. Unknown panel: show up to max_unknown_stocks, ordered by catches.
  # Keep all unknown stocks for return_data and shared size scaling.
  # Separate x3/y3 axes are independent of the F/B axes.
  if (has_unknown) {
    unknown_df <- dplyr::arrange(
      unknown_df, dplyr::desc(CatchSizeValue), StockKeyLabel
    )
    unknown_df <- unknown_df[seq_len(n_unknown_shown), , drop = FALSE]
    unknown_df$PanelX <- rep(0.13, nrow(unknown_df))
    unknown_df$PanelY <- rev(seq_len(nrow(unknown_df)))
    unknown_df$PanelLabel <- substr(as.character(unknown_df$StockKeyLabel), 1, 24)

    for (proxy_type in c("Reference point", "Proxy reference point")) {
      sub <- unknown_df[
        unknown_df$ProxyFlag == proxy_type, , drop = FALSE
      ]
      if (nrow(sub) == 0L) next
      is_proxy <- identical(proxy_type, "Proxy reference point")

      p <- plotly::add_trace(
        p,
        data = sub,
        type = "scatter", mode = "markers+text",
        x = ~PanelX, y = ~PanelY,
        xaxis = "x3", yaxis = "y3",
        text = ~PanelLabel, textposition = "middle right",
        textfont = list(size = 10, color = "#333333"),
        marker = list(
          symbol = if (is_proxy) "circle-open" else "circle",
          size = sub$point_size,
          color = if (is_proxy) "#858585" else status_pal[["Unknown"]],
          line = list(
            color = if (is_proxy) "#858585" else "#333333",
            width = if (is_proxy) 2 else 0.1
          )
        ),
        customdata = ~hover_text,
        hovertemplate = "%{customdata}<extra></extra>",
        showlegend = FALSE, inherit = FALSE,
        cliponaxis = FALSE
      )
    }
  }

  # 10. Legend for standard / proxy markers (unchanged).
  legend_defs <- list(
    "Reference point" = list(symbol = "circle", width = 0.1),
    "Proxy reference point" = list(symbol = "circle-open", width = 1.8)
  )
  for (nm in names(legend_defs)) {
    def <- legend_defs[[nm]]
    p <- plotly::add_markers(
      p, x = -1, y = -1,
      xaxis = "x2", yaxis = "y2", name = nm,
      marker = list(
        symbol = def$symbol, size = 12, color = "#000000",
        line = list(color = "#000000", width = def$width)
      ),
      legendgroup = "reference_type", showlegend = TRUE,
      hoverinfo = "skip", inherit = FALSE
    )
  }

  # 11. ONE size legend covering stocks in BOTH panels.
  if (isTRUE(size_by_catches) && length(size_values) > 0L) {
    # Number of representative sizes displayed in the legend
    if (!is.numeric(n_size_legend) ||
        length(n_size_legend) != 1L ||
        !is.finite(n_size_legend) ||
        n_size_legend < 1 ||
        n_size_legend != as.integer(n_size_legend)) {
      stop("n_size_legend must be a positive integer.")
    }

    legend_probs <- if (n_size_legend == 1) {
      0.5
    } else {
      seq(0, 1, length.out = n_size_legend)
    }

    legend_values <- unique(as.numeric(stats::quantile(
      size_values,
      probs = legend_probs,
      na.rm = TRUE,
      names = FALSE
    )))
    legend_sizes <- scale_catches(legend_values)

    for (i in seq_along(legend_values)) {
      legend_label <- paste0(format_quantity(legend_values[[i]]), " tonnes")
      p <- plotly::add_trace(
        p,
        type = "scatter", mode = "markers", x = 0, y = 0,
        name = legend_label,
        marker = list(
          symbol = "circle", size = legend_sizes[[i]],
          color = "rgba(140,140,140,0.45)",
          line = list(color = "#666666", width = 1)
        ),
        legendgroup = "catch_size",
        legendgrouptitle = list(text = "Catches / landings in ecoregion"),
        visible = "legendonly", showlegend = TRUE,
        hoverinfo = "skip", inherit = FALSE
      )
    }
  }

  # 12. Layout: reserve a side domain only if unknown stocks exist.
  # No unknown stocks -> the regular Kobe plot occupies the full width.
  main_domain <- if (has_unknown) c(0, 0.72) else c(0, 1)
  legend_outside <- isTRUE(size_by_catches) || has_unknown
  layout_shapes <- list(
    list(
      type = "line", x0 = 1, x1 = 1,
      y0 = y_range[[1]], y1 = y_range[[2]],
      line = list(color = "#4D4D4D", dash = "dash")
    ),
    list(
      type = "line", x0 = x_range[[1]], x1 = x_range[[2]],
      y0 = 1, y1 = 1,
      line = list(color = "#4D4D4D", dash = "dash")
    )
  )
  layout_annotations <- stock_annotations

  if (has_unknown) {
    layout_shapes <- c(
      list(
        list(
          type = "rect", xref = "paper", yref = "paper",
          x0 = 0.78, x1 = 1, y0 = 0, y1 = 1,
          fillcolor = "rgba(245,245,245,0.55)",
          line = list(width = 0), layer = "below"
        ),
        list(
          type = "line", xref = "paper", yref = "paper",
          x0 = 0.765, x1 = 0.765, y0 = 0, y1 = 1,
          line = list(color = "#CCCCCC", width = 1)
        )
      ),
      layout_shapes
    )
    layout_annotations[[length(layout_annotations) + 1L]] <- list(
      x = 0.89, y = 1.035, xref = "paper", yref = "paper",
      text = paste0("<b>Unknown status (", n_unknown_total, ")</b>"),
      xanchor = "center", yanchor = "bottom", showarrow = FALSE,
      font = list(size = 12, color = "#333333")
    )
    if (n_unknown_shown < n_unknown_total) {
      layout_annotations[[length(layout_annotations) + 1L]] <- list(
        x = 0.89, y = 1.005, xref = "paper", yref = "paper",
        text = paste0("Showing ", n_unknown_shown, " of ", n_unknown_total),
        xanchor = "center", yanchor = "bottom", showarrow = FALSE,
        font = list(size = 10, color = "#666666")
      )
    }
  }

  layout_annotations[[length(layout_annotations) + 1L]] <- list(
    x = 1, y = -0.14, xref = "paper", yref = "paper",
    xanchor = "right", yanchor = "top", showarrow = FALSE,
    text = paste0(
      "ICES Stock Assessment Database, ",
      format(Sys.Date(), "%d-%b-%y"), ". ICES, Copenhagen"
    ),
    font = list(size = 10)
  )

  p <- plotly::layout(
    p,
    height = NULL,
    autosize = TRUE,
    paper_bgcolor = "white", plot_bgcolor = "white",
    xaxis = list(
      title = "F/Fref", domain = main_domain,
      range = x_range, zeroline = FALSE
    ),
    yaxis = list(
      title = "B/Bref", domain = c(0, 1),
      range = y_range, zeroline = FALSE
    ),
    xaxis2 = list(
      overlaying = "x", visible = FALSE, range = c(0, 1),
      fixedrange = TRUE, autorange = FALSE
    ),
    yaxis2 = list(
      overlaying = "y", visible = FALSE, range = c(0, 1),
      fixedrange = TRUE, autorange = FALSE
    ),
    xaxis3 = if (has_unknown) list(
      domain = c(0.79, 1), anchor = "y3", range = c(0, 1),
      visible = FALSE, fixedrange = TRUE
    ) else NULL,
    yaxis3 = if (has_unknown) list(
      domain = c(0, 1), anchor = "x3",
      range = c(0.5, n_unknown_shown + 0.5),
      visible = FALSE, fixedrange = TRUE
    ) else NULL,
    shapes = layout_shapes,
    legend = list(
      title = list(text = "ICES reference point"),
      x = if (legend_outside) 1.02 else 0.98,
      y = 0.98,
      xanchor = if (legend_outside) "left" else "right",
      yanchor = "top",
      bgcolor = "rgba(255,255,255,0.90)",
      bordercolor = "#D9D9D9", borderwidth = 1,
      tracegroupgap = 18
    ),
    hovermode = "closest",
    margin = list(
      l = 90, r = if (legend_outside) 280 else 30,
      t = if (has_unknown) 55 else 30, b = 70
    ),
    annotations = layout_annotations
  )

  plotly::config(
    p,
    responsive = TRUE,
    toImageButtonOptions = list(
      filename = paste0("Kobe_v3_", format(Sys.Date(), "%d-%b-%y")),
      format = "png", scale = 3
    )
  )
}

