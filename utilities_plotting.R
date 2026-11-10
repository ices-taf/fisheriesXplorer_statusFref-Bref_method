# Stock-count pies (six guild columns, two F/B rows).
# Extracted from the latest utilities_GES_pies.R.

add_html_hover <- function(plot, renderer) {
  js <- r"---(
  const {tooltip, build} = view;
  const isNumber = value => typeof value === 'number' && Number.isFinite(value);
  if (typeof el.__htmlHoverCleanup === 'function') el.__htmlHoverCleanup();
  document.body.appendChild(tooltip);
  let lastPointer = null;
  let hideTimer = null;
  let observer = null;
  function cancelHide() {
    if (hideTimer !== null) { clearTimeout(hideTimer); hideTimer = null; }
  }
  function hideSoon() {
    cancelHide();
    hideTimer = setTimeout(function() { tooltip.style.display = 'none'; }, 180);
  }
  function position(xPos, yPos) {
    if (!isNumber(xPos) || !isNumber(yPos)) return;
    const pad = 10, offset = 18;
    const tw = tooltip.offsetWidth, th = tooltip.offsetHeight;
    let left = xPos + offset;
    if (left + tw + pad > window.innerWidth) left = xPos - tw - offset;
    left = Math.max(pad, Math.min(left, window.innerWidth - tw - pad));
    let top = yPos + 12;
    if (top + th + pad > window.innerHeight) top = window.innerHeight - th - pad;
    top = Math.max(pad, top);
    tooltip.style.left = left + 'px';
    tooltip.style.top = top + 'px';
  }
  function onMove(event) {
    lastPointer = {x:event.clientX, y:event.clientY};
    if (tooltip.style.display !== 'none') position(event.clientX, event.clientY);
  }
  function onHover(event) {
    const point = event && event.points && event.points[0];
    if (!point) return;
    let payload = point.customdata;
    if (payload == null && point.data && point.data.customdata) {
      payload = point.data.customdata[point.pointNumber];
    }
    if (Array.isArray(payload)) payload = payload[0];
    if (typeof payload !== 'string') { hideSoon(); return; }
    let data;
    try { data = JSON.parse(payload); } catch(error) { hideSoon(); return; }
    if (!data || typeof data !== 'object') { hideSoon(); return; }
    cancelHide();
    tooltip.innerHTML = tooltip.querySelector('style').outerHTML + build(data);
    tooltip.style.display = 'block';
    const mouse = event.event && isNumber(event.event.clientX) ?
      {x:event.event.clientX, y:event.event.clientY} : lastPointer;
    if (mouse) position(mouse.x, mouse.y);
  }
  function onUnhover() { hideSoon(); }
  function onTipEnter() { cancelHide(); }
  function onTipLeave() { hideSoon(); }
  tooltip.addEventListener('mouseenter', onTipEnter);
  tooltip.addEventListener('mouseleave', onTipLeave);
  el.addEventListener('mousemove', onMove);
  el.on('plotly_hover', onHover);
  el.on('plotly_unhover', onUnhover);
  const cleanup = function() {
    cancelHide();
    el.removeEventListener('mousemove', onMove);
    if (typeof el.removeListener === 'function') {
      el.removeListener('plotly_hover', onHover);
      el.removeListener('plotly_unhover', onUnhover);
    }
    tooltip.removeEventListener('mouseenter', onTipEnter);
    tooltip.removeEventListener('mouseleave', onTipLeave);
    if (tooltip.parentNode) tooltip.parentNode.removeChild(tooltip);
    if (observer) observer.disconnect();
    if (el.__htmlHoverCleanup === cleanup) delete el.__htmlHoverCleanup;
  };
  el.__htmlHoverCleanup = cleanup;
  observer = new MutationObserver(function() {
    if (!document.body.contains(el)) cleanup();
  });
  observer.observe(document.body, {childList:true, subtree:true});
)---"
  htmlwidgets::onRender(plot, paste0(
    "function(el, x) { const view = (", renderer, ")(el, x);\n", js, "\n}"
  ))
}

ecoregion_exclusion_reason <- function(usable, share_unavailable = FALSE) {
  stopifnot(
    is.logical(usable), is.logical(share_unavailable),
    length(share_unavailable) %in% c(1L, length(usable))
  )
  excluded <- !usable %in% TRUE
  reason <- rep(NA_character_, length(usable))
  reason[excluded] <- "quantity unavailable or non-positive"
  reason[excluded & share_unavailable %in% TRUE] <- "share unavailable"
  reason
}

stop_plot_no_data <- function(message) {
  stop(structure(
    list(message = message, call = NULL),
    class = c("plot_no_data", "error", "condition")
  ))
}

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
    stop_plot_no_data("No stocks in the supplied data.")
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

  counts_plot$hover_payload <- vapply(seq_len(nrow(counts_plot)), function(index) {
    stock_ids <- counts_plot$stock_ids[[index]]
    shown <- utils::head(stock_ids, max_hover_stocks)
    jsonlite::toJSON(
      list(
        guild = guild_display_name(counts_plot$GuildPlot[[index]]),
        indicator = headers[[counts_plot$Indicator[[index]]]],
        status = counts_plot$Colour[[index]],
        n_stocks = counts_plot$COUNT[[index]],
        total_stocks = counts_plot$total_stocks[[index]],
        pct = counts_plot$pct[[index]],
        stocks = as.list(shown),
        additional_stocks = length(stock_ids) - length(shown)
      ),
      auto_unbox = TRUE, na = "null", null = "null", digits = 12
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
        customdata = as.character(sub$hover_payload),
        hoverinfo = "none",
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
        x = mean(c(x0, x1)),
        y = y0 - 0.012,
        xref = "paper", yref = "paper",
        xanchor = "center",
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

  p <- plotly::config(
    p,
    toImageButtonOptions = list(
      filename = paste0("Status_pies_", format(Sys.Date(), "%d-%b-%y")),
      format = "png",
      scale = 3
    )
  )

  renderer <- r"---(
function(el, x) {
  const tooltip = document.createElement('div');
  tooltip.className = 'status-pies-tooltip';
  tooltip.setAttribute('role', 'tooltip');
  tooltip.style.display = 'none';
  tooltip.innerHTML = '<style>' +
    '.status-pies-tooltip{position:fixed;z-index:2147483000;width:400px;max-width:calc(100vw - 20px);max-height:calc(100vh - 20px);overflow-y:auto;box-sizing:border-box;padding:15px 16px;background:#fff;color:#243746;border:1px solid #d8e2ea;border-radius:8px;box-shadow:0 10px 32px rgba(15,33,45,.22);font:13px/1.4 -apple-system,BlinkMacSystemFont,Segoe UI,Arial,sans-serif;pointer-events:auto;}' +
    '.status-pies-tooltip *{box-sizing:border-box;}' +
    '.status-pies-tooltip .sp-title{font-size:16px;font-weight:700;overflow-wrap:anywhere;}' +
    '.status-pies-tooltip .sp-muted{font-size:11px;color:#697b87;}' +
    '.status-pies-tooltip .sp-summary{display:flex;justify-content:space-between;align-items:baseline;gap:12px;padding:10px 0;border-bottom:1px solid #e4eaf0;}' +
    '.status-pies-tooltip .sp-percent{font-size:21px;font-weight:750;white-space:nowrap;}' +
    '.status-pies-tooltip .sp-section{font-size:12px;font-weight:700;margin-top:10px;}' +
    '.status-pies-tooltip .sp-stock{font-size:11px;font-weight:650;overflow-wrap:anywhere;padding:7px 0;border-bottom:1px solid #eef2f5;}' +
    '</style>';
  const safe = value => String(value == null ? '' : value).replace(/[&<>"']/g,
    character => ({'&':'&amp;','<':'&lt;','>':'&gt;','"':'&quot;',"'":'&#39;'}[character]));
  const pct = value => typeof value === 'number' && Number.isFinite(value) ?
    value.toFixed(1) + '%' : 'Not available';
  const build = function(data) {
    let html = '<div class="sp-title">' + safe(data.guild) + '</div>' +
      '<div class="sp-muted">' + safe(data.indicator) + ' | ' + safe(data.status) + '</div>' +
      '<div class="sp-summary"><div><b>' + safe(data.n_stocks) + ' / ' + safe(data.total_stocks) +
      ' stocks</b><div class="sp-muted">Stocks in slice / guild total</div></div>' +
      '<div class="sp-percent">' + pct(data.pct) + '</div></div>' +
      '<div class="sp-section">Stocks in slice</div>';
    (Array.isArray(data.stocks) ? data.stocks : []).forEach(function(stock) {
      html += '<div class="sp-stock">' + safe(stock) + '</div>';
    });
    if (data.additional_stocks > 0) html += '<div class="sp-muted" style="margin-top:7px">... and ' +
      safe(data.additional_stocks) + ' more stocks</div>';
    return html;
  };
  return {tooltip, build};
}
)---"
  add_html_hover(p, renderer)
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
  if (!"Catches_out_ecoregion" %in% names(y)) {
    y$Catches_out_ecoregion <- NA_real_
  }
  if (!"Landings_out_ecoregion" %in% names(y)) {
    y$Landings_out_ecoregion <- NA_real_
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

  if (nrow(stock_df) == 0L) stop_plot_no_data("No stocks in x.")

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
  share_unavailable <- if ("share" %in% names(y)) {
    share_values <- to_number(y$share)
    !is.finite(share_values) | share_values < 0 | share_values > 1
  } else {
    rep(FALSE, nrow(y))
  }
  catch_df <- y |>
    dplyr::transmute(
      StockKeyLabel = as.character(StockKeyLabel),
      ShareUnavailable = share_unavailable,
      catch_value = clean_nonnegative(Catches_in_ecoregion),
      landing_value = clean_nonnegative(Landings_in_ecoregion),
      outside_catch = clean_nonnegative(Catches_out_ecoregion),
      outside_landings = clean_nonnegative(Landings_out_ecoregion)
    ) |>
    dplyr::filter(!is.na(StockKeyLabel), nzchar(StockKeyLabel)) |>
    dplyr::mutate(
      CATCH = dplyr::coalesce(catch_value, landing_value),
      OUT_CATCH = dplyr::case_when(
        !is.na(catch_value) ~ outside_catch,
        !is.na(landing_value) ~ outside_landings,
        TRUE ~ NA_real_
      ),
      CatchSource = dplyr::case_when(
        !is.na(catch_value) ~ "Catches",
        !is.na(landing_value) ~ "Landings",
        TRUE ~ "Missing"
      )
    ) |>
    dplyr::select(StockKeyLabel, CATCH, OUT_CATCH, CatchSource,
            ShareUnavailable) |>
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
    dplyr::left_join(catch_df, by = "StockKeyLabel") |>
    dplyr::mutate(
      ExclusionReason = ecoregion_exclusion_reason(
        is.finite(CATCH) & CATCH > 0, ShareUnavailable %in% TRUE
      )
    )

  # ---------------------------------------------------------
  # 3. Long data: each stock contributes to BOTH indicators.
  # ---------------------------------------------------------
  long_df <- dplyr::bind_rows(
    stock_df |>
      dplyr::transmute(
        StockKeyLabel, FisheriesGuild, CATCH, OUT_CATCH, CatchSource,
        ExclusionReason,
        Indicator = "Fref", Colour = F_Status
      ),
    stock_df |>
      dplyr::transmute(
        StockKeyLabel, FisheriesGuild, CATCH, OUT_CATCH, CatchSource,
        ExclusionReason,
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
      excluded_share_unavailable = sum(
        ExclusionReason == "share unavailable", na.rm = TRUE
      ),
      excluded_no_positive_quantity = sum(
        ExclusionReason == "quantity unavailable or non-positive", na.rm = TRUE
      ),
      .groups = "drop"
    )

  weighted <- long_df |>
    dplyr::filter(is.finite(CATCH), CATCH > 0)

  if (nrow(weighted) == 0L) {
    stop_plot_no_data(paste0(
      "No stocks have positive Catches_in_ecoregion or ",
      "Landings_in_ecoregion values."
    ))
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

  build_stock_breakdown <- function(stock_ids, catches, outside, sources) {
    shown <- utils::head(order(catches, decreasing = TRUE), max_hover_stocks)
    list(
      stocks = lapply(shown, function(index) {
        list(
          stock = stock_ids[[index]],
          inside_t = catches[[index]],
          outside_t = outside[[index]],
          source = sources[[index]],
          slice_pct = 100 * catches[[index]] / sum(catches)
        )
      }),
      n_more = length(stock_ids) - length(shown)
    )
  }

  slices <- weighted |>
    dplyr::group_by(FisheriesGuild, Indicator, Colour) |>
    dplyr::summarise(
      Catch = sum(CATCH),
      COUNT = dplyr::n(),
      breakdown_html = build_breakdown(StockKeyLabel, CATCH, CatchSource),
      stock_breakdown = list(build_stock_breakdown(
        StockKeyLabel, CATCH, OUT_CATCH, CatchSource
      )),
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
      "Stocks excluded from this guild: ", slices$excluded_stocks[[i]], "<br>",
      "No ecoregion landings share available: ", slices$excluded_share_unavailable[[i]], "<br>",
      "No positive catch/landings (zero, missing or invalid quantity): ",
      slices$excluded_no_positive_quantity[[i]], "<br><br>",
      "<b>Catch by stock</b><br>", slices$breakdown_html[[i]]
    )
  }, character(1))

  slices$hover_payload <- vapply(seq_len(nrow(slices)), function(index) {
    jsonlite::toJSON(
      list(
        guild = slices$FisheriesGuild[[index]],
        indicator = headers[[slices$Indicator[[index]]]],
        status = slices$Colour[[index]],
        inside_t = slices$Catch[[index]],
        pct = slices$pct[[index]],
        n_stocks = slices$COUNT[[index]],
        excluded_stocks = slices$excluded_stocks[[index]],
        excluded_share_unavailable = slices$excluded_share_unavailable[[index]],
        excluded_no_positive_quantity = slices$excluded_no_positive_quantity[[index]],
        stocks = slices$stock_breakdown[[index]]$stocks,
        additional_stocks = slices$stock_breakdown[[index]]$n_more
      ),
      auto_unbox = TRUE, na = "null", null = "null", digits = 12
    ) |> as.character()
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
        customdata = as.character(sub$hover_payload),
        hoverinfo = "none",
        textinfo = "percent",
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
    showlegend = FALSE,
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

  p <- plotly::config(
    p,
    toImageButtonOptions = list(
      filename = paste0("GES_pies_precautionary_", format(Sys.Date(), "%d-%b-%y")),
      format = "png", scale = 3
    )
  )

  renderer <- r"---(
function(el, x) {
  const tooltip = document.createElement('div');
  tooltip.className = 'ges-pies-tooltip';
  tooltip.setAttribute('role', 'tooltip');
  tooltip.style.display = 'none';
  tooltip.innerHTML = '<style>' +
    '.ges-pies-tooltip{position:fixed;z-index:2147483000;width:400px;max-width:calc(100vw - 20px);max-height:calc(100vh - 20px);overflow-y:auto;box-sizing:border-box;padding:15px 16px;background:#fff;color:#243746;border:1px solid #d8e2ea;border-radius:8px;box-shadow:0 10px 32px rgba(15,33,45,.22);font:13px/1.4 -apple-system,BlinkMacSystemFont,Segoe UI,Arial,sans-serif;pointer-events:auto;}' +
    '.ges-pies-tooltip *{box-sizing:border-box;}' +
    '.ges-pies-tooltip .gp-title{font-size:16px;font-weight:700;overflow-wrap:anywhere;}' +
    '.ges-pies-tooltip .gp-muted{font-size:11px;color:#697b87;}' +
    '.ges-pies-tooltip .gp-summary{padding:10px 0;border-bottom:1px solid #e4eaf0;}' +
    '.ges-pies-tooltip .gp-stock{padding:9px 0;border-bottom:1px solid #eef2f5;}' +
    '.ges-pies-tooltip .gp-row{display:flex;justify-content:space-between;gap:8px;align-items:baseline;}' +
    '.ges-pies-tooltip .gp-code{font-size:11px;font-weight:650;overflow-wrap:anywhere;min-width:0;}' +
    '.ges-pies-tooltip .gp-track{display:flex;height:10px;background:#e9eef2;border-radius:4px;overflow:hidden;margin:5px 0;}' +
    '.ges-pies-tooltip .gp-in{background:#6895af;flex:none;}' +
    '.ges-pies-tooltip .gp-out{background:#d77a52;flex:none;}' +
    '.ges-pies-tooltip .gp-key{display:inline-block;width:9px;height:9px;margin-right:4px;}' +
    '</style>';
  const safe = value => String(value == null ? '' : value).replace(/[&<>"']/g,
    character => ({'&':'&amp;','<':'&lt;','>':'&gt;','"':'&quot;',"'":'&#39;'}[character]));
  const isNumber = value => typeof value === 'number' && Number.isFinite(value);
  const fmt = value => isNumber(value) ? value.toLocaleString('en-GB', {
    maximumFractionDigits:0
  }) : 'Not available';
  const pct = value => isNumber(value) ? value.toFixed(1) + '%' : 'Not available';
  const build = function(data) {
    let html = '<div class="gp-title">' + safe(data.guild) + '</div>' +
      '<div class="gp-muted">' + safe(data.indicator) + ' | ' + safe(data.status) + '</div>' +
      '<div class="gp-summary"><b>' + fmt(data.inside_t) + ' t in ecoregion</b> (' + pct(data.pct) + ' of guild)' +
      '<div class="gp-muted">' + safe(data.n_stocks) + ' stocks in slice; ' +
      safe(data.excluded_stocks) + ' excluded from guild' +
      '<br>No ecoregion landings share available: ' + safe(data.excluded_share_unavailable) +
      '<br>No positive catch/landings (zero, missing or invalid quantity): ' +
      safe(data.excluded_no_positive_quantity) + '</div></div>' +
      '<div class="gp-muted" style="margin-top:9px"><i class="gp-key gp-in"></i>Inside ecoregion ' +
      '<i class="gp-key gp-out"></i>Outside ecoregion</div>';
    (Array.isArray(data.stocks) ? data.stocks : []).forEach(function(stock) {
      const knownOutside = isNumber(stock.outside_t) && stock.outside_t >= 0;
      const total = knownOutside ? stock.inside_t + stock.outside_t : null;
      const insidePct = isNumber(total) && total > 0 ? 100 * stock.inside_t / total : null;
      const outsidePct = insidePct === null ? null : 100 - insidePct;
      const bar = insidePct === null ? '<div class="gp-track"></div>' :
        '<div class="gp-track" role="img" aria-label="' + pct(insidePct) + ' inside; ' + pct(outsidePct) + ' outside">' +
        '<div class="gp-in" style="width:' + insidePct + '%"></div>' +
        '<div class="gp-out" style="width:' + outsidePct + '%"></div></div>';
      html += '<div class="gp-stock"><div class="gp-row"><span class="gp-code">' + safe(stock.stock) +
        '</span><span class="gp-muted" style="white-space:nowrap">' + pct(stock.slice_pct) + ' of slice</span></div>' +
        bar + '<div class="gp-muted">' + safe(stock.source) + ' inside: ' + fmt(stock.inside_t) + ' t (' + pct(insidePct) + ')' +
        '<br>Outside: ' + (knownOutside ? fmt(stock.outside_t) + ' t (' + pct(outsidePct) + ')' : 'Not available') + '</div></div>';
    });
    if (data.additional_stocks > 0) html += '<div class="gp-muted">... and ' + safe(data.additional_stocks) + ' more stocks</div>';
    return html;
  };
  return {tooltip, build};
}
)---"
  add_html_hover(p, renderer)
}



# Kobe v3 with responsive autosizing.
# Extracted from the latest utilities_GES_pies.R.

plot_kobe_app_plotly_v3 <- function(
    x,
    guild,
    return_data = FALSE,
    size_by_catches = TRUE,
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
        at_right_edge <- sub$F_FMSY[[k]] >
          (x_range[[2]] - 0.20 * diff(x_range))
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

  # 11. ONE size legend.
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

  legend_outside <- isTRUE(size_by_catches)
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
      title = "F/Fref", domain = c(0, 1),
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
      t = 30, b = 70
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






##### Discards plot



plot_discard_trends_app_plotly_v3 <- function(
    x, year, return_data = FALSE, ecoregion = NULL,
    complete_pairs = TRUE,
    width_px = 800, max_hover_stocks = 20
) {
  # Expects ecoregion-adjusted quantities in x, e.g. values multiplied by
  # a stock-specific ShareLandingsInEcoregion. Regional landings/discards
  # are required. Original Landings/Catches and regional catches are
  # optional; if present they quantify the extent of excluded data.
  #
  # complete_pairs = TRUE: only stocks with BOTH landings and discards
  # contribute to the guild-year numerator/denominator.
  # complete_pairs = FALSE: independently sum available landings/discards,
  # matching the original function's na.rm = TRUE treatment. A stock is
  # excluded when BOTH values are missing or their known sum is zero.
  #
  # The returned data (return_data = TRUE) has one row per plotted guild-year,
  # including all coverage/exclusion metrics and the structured stock breakdown.
  # The HTML tooltip is drawn in the browser by htmlwidgets::onRender().

  required <- c("Year", "StockKeyLabel", "FisheriesGuild",
                "Landings_ecoregion", "Discards_ecoregion")
  missing <- setdiff(required, names(x))
  if (length(missing)) {
    stop("Missing columns in x: ", paste(missing, collapse = ", "))
  }
  if (!is.logical(complete_pairs) || length(complete_pairs) != 1L ||
      is.na(complete_pairs)) {
    stop("complete_pairs must be TRUE or FALSE.")
  }
  if (!is.logical(return_data) || length(return_data) != 1L ||
      is.na(return_data)) {
    stop("return_data must be TRUE or FALSE.")
  }
  if (!is.numeric(year) || length(year) != 1L || !is.finite(year) ||
      year <= 2011 || year != floor(year)) {
    stop("year must be one integer-valued year greater than 2011.")
  }
  if (!is.numeric(max_hover_stocks) || length(max_hover_stocks) != 1L ||
      !is.finite(max_hover_stocks) || max_hover_stocks < 1) {
    stop("max_hover_stocks must be a positive number.")
  }
  if (!is.numeric(width_px) || length(width_px) != 1L ||
      !is.finite(width_px) || width_px <= 0) width_px <- 800

  max_hover_stocks <- as.integer(max_hover_stocks)

  to_number <- function(z) suppressWarnings(as.numeric(as.character(z)))
  clean_nonnegative <- function(z) {
    z <- to_number(z)
    z[!is.finite(z) | z < 0] <- NA_real_
    z
  }
  escape_html <- function(z) {
    z <- gsub("&", "&amp;", z, fixed = TRUE)
    z <- gsub("<", "&lt;", z, fixed = TRUE)
    gsub(">", "&gt;", z, fixed = TRUE)
  }
  format_tonnes <- function(z) {
    out <- format(round(z, 1), big.mark = ",",
                  scientific = FALSE, trim = TRUE)
    out[is.na(z)] <- "not available"
    out
  }
  # Do not print '0' for an excluded group in which every value is missing.
  # With no excluded stocks the known excluded total is genuinely zero,
  # provided the corresponding column exists.
  known_total <- function(z) {
    if (length(z) == 0L) return(0)
    if (all(is.na(z))) return(NA_real_)
    sum(z, na.rm = TRUE)
  }
  display_thousand <- function(z) {
    if (is.na(z)) "not available" else
      paste0(format_tonnes(z / 1000), " thousand t")
  }
  display_percent <- function(z) {
    ifelse(is.na(z), "not available", sprintf("%.1f%%", z))
  }

  # Stock-level quantities for the genuine HTML/CSS stacked bars.
  # Each segment's width is a percentage of included guild (L + D),
  # not a percentage of the individual stock. Missing values are kept
  # missing in labels rather than silently represented as observed zeros.
  build_stock_breakdown <- function(stock_ids, landings, discards) {
    known_L <- dplyr::coalesce(landings, 0)
    known_D <- dplyr::coalesce(discards, 0)
    amount <- known_L + known_D
    positive <- which(amount > 0)
    if (!length(positive)) {
      return(list(stocks = list(), n_more = 0L))
    }
    ord <- positive[order(amount[positive], decreasing = TRUE)]
    shown <- utils::head(ord, max_hover_stocks)
    total <- sum(amount[positive])

    stocks <- lapply(shown, function(i) {
      list(
        stock = as.character(stock_ids[i]),
        landings_t = as.numeric(landings[i]),
        discards_t = as.numeric(discards[i]),
        # All widths have the same guild-year denominator.
        landings_share_pct = 100 * known_L[i] / total,
        discards_share_pct = 100 * known_D[i] / total,
        share_pct = 100 * amount[i] / total,
        rate_pct = if (is.na(landings[i]) || is.na(discards[i])) {
          NA_real_
        } else {
          100 * discards[i] / amount[i]
        }
      )
    })
    list(stocks = stocks, n_more = length(ord) - length(shown))
  }

  # Catch and original quantity columns are optional because earlier
  # versions of this function did not require them. Report each separately
  # to avoid conflating full-stock and regional quantities.
  catch_col <- if ("Catches_ecoregion" %in% names(x)) {
    "Catches_ecoregion"
  } else if ("Catches_ecoregion_est" %in% names(x)) {
    "Catches_ecoregion_est"
  } else {
    NULL
  }
  catch_values <- if (is.null(catch_col)) {
    rep(NA_real_, nrow(x))
  } else {
    clean_nonnegative(x[[catch_col]])
  }
  original_landings <- if ("Landings" %in% names(x)) {
    clean_nonnegative(x[["Landings"]])
  } else {
    rep(NA_real_, nrow(x))
  }
  original_catches <- if ("Catches" %in% names(x)) {
    clean_nonnegative(x[["Catches"]])
  } else {
    rep(NA_real_, nrow(x))
  }

  share_unavailable <- if ("share" %in% names(x)) {
    share_values <- to_number(x$share)
    !is.finite(share_values) | share_values < 0 | share_values > 1
  } else {
    rep(FALSE, nrow(x))
  }

  # One observation per stock-year; no rowid_to_column/spread/gather joins.
  df_all <- x |>
    dplyr::transmute(
      Year = to_number(Year),
      StockKeyLabel = as.character(StockKeyLabel),
      FisheriesGuild = as.character(FisheriesGuild),
      ShareUnavailable = share_unavailable,
      Landings_ecoregion = clean_nonnegative(Landings_ecoregion),
      Discards_ecoregion = clean_nonnegative(Discards_ecoregion),
      Catches_ecoregion = catch_values,
      Landings_original = original_landings,
      Catches_original = original_catches
    ) |>
    dplyr::filter(
      is.finite(Year), Year == floor(Year),
      Year >= 2011, Year < year,
      !is.na(StockKeyLabel), nzchar(StockKeyLabel),
      !is.na(FisheriesGuild), nzchar(FisheriesGuild)
    ) |>
    dplyr::distinct()

  repeated <- df_all |>
    dplyr::count(Year, StockKeyLabel, name = "n") |>
    dplyr::filter(n > 1L)
  if (nrow(repeated)) {
    stop(
      "Multiple distinct records for the same stock and year: ",
      paste(utils::head(repeated$StockKeyLabel, 8L), collapse = ", "),
      ". Resolve these before plotting to avoid double counting."
    )
  }

  df_all <- df_all |>
    dplyr::mutate(
      usable = if (complete_pairs) {
        !is.na(Landings_ecoregion) & !is.na(Discards_ecoregion)
      } else {
        !is.na(Landings_ecoregion) | !is.na(Discards_ecoregion)
      },
      usable = usable & (
        dplyr::coalesce(Landings_ecoregion, 0) +
          dplyr::coalesce(Discards_ecoregion, 0) > 0
      ),
      ExclusionReason = ecoregion_exclusion_reason(usable, ShareUnavailable)
    )

  # Denominator/data coverage and excluded known regional quantities.
  # Missing excluded values stay missing: excluded totals are *known* totals,
  # not estimates of the unknown quantities.
  coverage <- df_all |>
    dplyr::group_by(Year, FisheriesGuild) |>
    dplyr::summarise(
      n_available = dplyr::n(),
      n_included = sum(usable),
      n_excluded = sum(!usable),
      excluded_share_unavailable = sum(
        ExclusionReason == "share unavailable", na.rm = TRUE
      ),
      excluded_no_positive_quantity = sum(
        ExclusionReason == "quantity unavailable or non-positive", na.rm = TRUE
      ),
      n_included_missing_landings = sum(usable & is.na(Landings_ecoregion)),
      n_included_missing_discards = sum(usable & is.na(Discards_ecoregion)),
      n_excluded_missing_landings = sum(!usable & is.na(Landings_ecoregion)),
      n_excluded_missing_catches = sum(!usable & is.na(Catches_ecoregion)),
      n_excluded_missing_original_landings =
        sum(!usable & is.na(Landings_original)),
      n_excluded_missing_original_catches =
        sum(!usable & is.na(Catches_original)),
      excluded_landings_original_t = known_total(Landings_original[!usable]),
      excluded_catches_original_t = known_total(Catches_original[!usable]),
      excluded_landings_ecoregion_t =
        known_total(Landings_ecoregion[!usable]),
      excluded_catches_ecoregion_t =
        known_total(Catches_ecoregion[!usable]),
      # Denominator includes all KNOWN regional landings in this guild/year,
      # whether included in the discard-rate calculation or excluded.
      # Unknown regional landings are not implicitly counted as zero.
      known_landings_ecoregion_t =
        sum(Landings_ecoregion, na.rm = TRUE),
      n_missing_landings_ecoregion = sum(is.na(Landings_ecoregion)),
      excluded_share_known_landings_pct = {
        known <- Landings_ecoregion[!is.na(Landings_ecoregion)]
        if (!length(known) || sum(known) <= 0) {
          NA_real_
        } else {
          100 * sum(Landings_ecoregion[!usable], na.rm = TRUE) / sum(known)
        }
      },
      .groups = "drop"
    )

  # In non-paired mode, sums are calculated independently, as in v1.
  included <- df_all |>
    dplyr::filter(usable)

  df5 <- included |>
    dplyr::group_by(Year, FisheriesGuild) |>
    dplyr::summarise(
      guildLandings = sum(Landings_ecoregion, na.rm = TRUE) / 1000,
      guildDiscards = sum(Discards_ecoregion, na.rm = TRUE) / 1000,
      n_stocks = sum(dplyr::coalesce(Landings_ecoregion, 0) +
                       dplyr::coalesce(Discards_ecoregion, 0) > 0),
      stock_breakdown = list(build_stock_breakdown(
        StockKeyLabel, Landings_ecoregion, Discards_ecoregion
      )),
      .groups = "drop"
    ) |>
    dplyr::mutate(
      guildRate = dplyr::if_else(
        guildLandings + guildDiscards > 0,
        guildDiscards / (guildLandings + guildDiscards),
        NA_real_
      )
    ) |>
    dplyr::filter(!is.na(guildRate)) |>
    dplyr::left_join(
      coverage, by = c("Year", "FisheriesGuild"),
      relationship = "one-to-one"
    ) |>
    dplyr::arrange(FisheriesGuild, Year)

  if (nrow(df5)) {
    method_label <- if (complete_pairs) {
      "Paired stocks only"
    } else {
      "Independent available values (v1 method)"
    }
    df5 <- df5 |>
      dplyr::mutate(
        hover_text = paste0(
          "<b>", escape_html(FisheriesGuild), " — ", Year, "</b>",
          "<br>Estimated ecoregion discard rate: ",
          scales::percent(guildRate, accuracy = 0.01),
          "<br>Landings: ", format_tonnes(guildLandings), " thousand t",
          " | Discards: ", format_tonnes(guildDiscards), " thousand t",
          "<br>Method: ", method_label,
          "<br>Included stocks: ", n_included, " of ", n_available,
          " (", n_excluded, " excluded)",
          "<br>No ecoregion landings share available: ", excluded_share_unavailable,
          "<br>No usable landings/discards (zero, missing, invalid or incomplete pair): ",
          excluded_no_positive_quantity,
          "<br>Excluded share of known ecoregion landings: ",
          display_percent(excluded_share_known_landings_pct)
        )
      )

    # Each plotted point carries its data as JSON, not pre-rendered HTML.
    # This is also part of the return_data output for reproducibility.
    df5$hover_payload <- vapply(seq_len(nrow(df5)), function(i) {
      d <- df5[i, , drop = FALSE]
      jsonlite::toJSON(
        list(
          guild = d$FisheriesGuild[[1]],
          year = as.integer(d$Year[[1]]),
          discard_rate_pct = 100 * d$guildRate[[1]],
          landings_kt = d$guildLandings[[1]],
          discards_kt = d$guildDiscards[[1]],
          method = method_label,
          n_available = as.integer(d$n_available[[1]]),
          n_included = as.integer(d$n_included[[1]]),
          n_excluded = as.integer(d$n_excluded[[1]]),
          excluded_share_unavailable =
            as.integer(d$excluded_share_unavailable[[1]]),
          excluded_no_positive_quantity =
            as.integer(d$excluded_no_positive_quantity[[1]]),
          excluded_share_known_landings_pct =
            d$excluded_share_known_landings_pct[[1]],
          n_missing_landings_ecoregion =
            as.integer(d$n_missing_landings_ecoregion[[1]]),
          stocks = d$stock_breakdown[[1]]$stocks,
          additional_stocks = as.integer(d$stock_breakdown[[1]]$n_more)
        ),
        auto_unbox = TRUE,
        na = "null",
        null = "null",
        digits = 12
      ) |> as.character()
    }, character(1))
  } else {
    df5$hover_text <- character(0)
    df5$hover_payload <- character(0)
  }

  # Absolutely no Plotly calls if the caller only wants the plotted data.
  if (isTRUE(return_data)) return(df5)

  if (!nrow(df5)) {
    return(plotly::plot_ly(
      x = numeric(0), y = numeric(0), type = "scatter", mode = "markers",
      showlegend = FALSE, hoverinfo = "skip"
    ) |>
      plotly::layout(
        xaxis = list(visible = FALSE),
        yaxis = list(visible = FALSE),
        annotations = list(list(
          text = "No usable regional landings and discards available",
          x = 0.5, y = 0.5, xref = "paper", yref = "paper",
          showarrow = FALSE
        ))
      ))
  }

  # Stable guild-specific colours.
  guild_colours <- c(
    benthic = "#0072B2",
    demersal = "#E69F00",
    elasmobranch = "#009E73",
    pelagic = "#CC79A7",
    shellfish = "#D55E00"
  )
  guilds <- sort(unique(df5$FisheriesGuild))
  other_guilds <- setdiff(guilds, names(guild_colours))
  if (length(other_guilds)) {
    guild_colours <- c(
      guild_colours,
      stats::setNames(
        grDevices::hcl.colors(length(other_guilds), palette = "Dark 3"),
        other_guilds
      )
    )
  }

  tick_size <- max(9, min(16, round(width_px / 55)))
  axis_title_size <- max(10, min(20, round(width_px / 50)))
  title_size <- max(12, min(22, round(width_px / 40)))
  caption_size <- max(8, min(14, round(width_px / 70)))

  p <- plotly::plot_ly()
  for (guild in guilds) {
    sub <- df5 |>
      dplyr::filter(FisheriesGuild == guild) |>
      tidyr::complete(Year = seq.int(2011, year - 1)) |>
      dplyr::arrange(Year)

    p <- plotly::add_trace(
      p, data = sub,
      x = ~Year, y = ~guildRate,
      type = "scatter", mode = "lines+markers",
      name = guild, showlegend = TRUE,
      line = list(color = unname(guild_colours[guild]), width = 3),
      marker = list(color = unname(guild_colours[guild]), size = 5),
      customdata = ~hover_payload,
      # 'none' hides native Plotly hover labels but preserves hover events.
      hoverinfo = "none",
      connectgaps = FALSE,
      inherit = FALSE
    )
  }

  region_title <- if (is.null(ecoregion) || !nzchar(as.character(ecoregion))) {
    "Discard trends"
  } else {
    paste0("Discard trends (", ecoregion, ")")
  }

  p <- plotly::layout(
    p,
    hovermode = "closest",
    xaxis = list(
      title = list(text = "Year", font = list(size = axis_title_size)),
      dtick = 1, tickfont = list(size = tick_size)
    ),
    yaxis = list(
      title = list(text = "Discard rate", font = list(size = axis_title_size)),
      tickformat = ".1%", rangemode = "tozero",
      tickfont = list(size = tick_size)
    ),
    legend = list(title = list(text = "<b>Fisheries guild:</b>")),
    margin = list(l = 75, r = 30, t = 65, b = 115),
    annotations = list(
      list(
        text = region_title, x = 0.01, y = 1.08,
        xref = "paper", yref = "paper", showarrow = FALSE,
        xanchor = "left", yanchor = "top",
        font = list(size = title_size, color = "black")
      ),
      list(
        text = paste0("ICES Stock Assessment Database, ",
                      format(Sys.Date(), "%d-%b-%y"), ". ICES, Copenhagen<br>",
                      "Ecoregion estimates calculated from recent spatial shares derived from RDBES"),
        x = 1, y = -0.24, xref = "paper", yref = "paper",
        xanchor = "right", yanchor = "top", showarrow = FALSE,
        font = list(size = caption_size)
      )
    )
  )

  p <- plotly::config(
    p, responsive = TRUE,
    toImageButtonOptions = list(
      filename = paste0(
        if (is.null(ecoregion)) "Ecoregion" else ecoregion,
        "_DiscardTrends_", format(Sys.Date(), "%d-%b-%y")
      ),
      format = "png", scale = 3
    )
   )

  # Real HTML tooltip: safely render escaped labels and coloured CSS bars.
  # htmlwidgets::onRender runs in the browser after Plotly is drawn.
  js <- r"---(
function(el, x) {
  const tooltip = document.createElement('div');
  tooltip.className = 'discard-v3-tooltip';
  tooltip.setAttribute('role', 'tooltip');
  tooltip.style.display = 'none';
  tooltip.innerHTML = '<style>' +
    '.discard-v3-tooltip{position:fixed;z-index:2147483000;width:400px;max-width:calc(100vw - 20px);max-height:calc(100vh - 20px);overflow-y:auto;box-sizing:border-box;padding:15px 16px;background:#fff;color:#243746;border:1px solid #d8e2ea;border-radius:12px;box-shadow:0 10px 32px rgba(15,33,45,.22);font:13px/1.4 -apple-system,BlinkMacSystemFont,Segoe UI,Arial,sans-serif;pointer-events:auto;}' +
    '.discard-v3-tooltip *{box-sizing:border-box;}' +
    '.discard-v3-tooltip .dv3-head{display:flex;justify-content:space-between;align-items:flex-start;gap:12px;}' +
    '.discard-v3-tooltip .dv3-guild{font-size:16px;font-weight:700;line-height:1.2;}' +
    '.discard-v3-tooltip .dv3-muted{color:#697b87;font-size:11px;}' +
    '.discard-v3-tooltip .dv3-rate{text-align:right;white-space:nowrap;font-size:21px;font-weight:750;color:#17354c;line-height:1.15;}' +
    '.discard-v3-tooltip .dv3-grid{display:grid;grid-template-columns:1fr 1fr;gap:10px;margin:12px 0;}' +
    '.discard-v3-tooltip .dv3-number{font-size:16px;font-weight:650;color:#203d51;}' +
    '.discard-v3-tooltip .dv3-rule{height:1px;background:#e4eaf0;margin:12px 0;}' +
    '.discard-v3-tooltip .dv3-coverage{background:#f2f6f9;border-radius:8px;padding:11px;}' +
    '.discard-v3-tooltip .dv3-row{display:flex;justify-content:space-between;align-items:baseline;gap:8px;}' +
    '.discard-v3-tooltip .dv3-section{font-size:12px;font-weight:700;margin-bottom:6px;}' +
    '.discard-v3-tooltip .dv3-track{display:flex;width:100%;height:10px;background:#e9eef2;border-radius:4px;overflow:hidden;}' +
    '.discard-v3-tooltip .dv3-track-coverage{margin-top:8px;height:11px;background:#e9eef2;}' +
    '.discard-v3-tooltip .dv3-coverage-included{height:100%;background:#398b71;flex:none;}' +
    '.discard-v3-tooltip .dv3-coverage-excluded{height:100%;background:#ce805a;flex:none;}' +
    '.discard-v3-tooltip .dv3-coverage-labels{display:flex;justify-content:space-between;gap:8px;margin-top:5px;font-size:11px;color:#697b87;}' +
    '.discard-v3-tooltip .dv3-L{height:100%;background:#6895af;}' +
    '.discard-v3-tooltip .dv3-D{height:100%;background:#d77a52;}' +
    '.discard-v3-tooltip .dv3-stock{padding:7px 0;border-bottom:1px solid #eef2f5;}' +
    '.discard-v3-tooltip .dv3-stock:last-child{border-bottom:0;}' +
    '.discard-v3-tooltip .dv3-code{font-size:11px;font-weight:650;overflow-wrap:anywhere;}' +
    '.discard-v3-tooltip .dv3-details{font-size:10.5px;color:#687b86;margin-top:3px;}' +
    '.discard-v3-tooltip .dv3-legend{display:flex;gap:10px;align-items:center;font-size:10.5px;color:#6b7b85;}' +
    '.discard-v3-tooltip .dv3-key{display:inline-block;width:9px;height:9px;border-radius:2px;margin-right:4px;}' +
    '</style>';
  const safe = function(value) {
    return String(value == null ? '' : value).replace(/[&<>"']/g, function(c) {
      return {'&':'&amp;','<':'&lt;','>':'&gt;','"':'&quot;',"'":'&#39;'}[c];
    });
  };
  const isNumber = function(z) {
    return typeof z === 'number' && Number.isFinite(z);
  };
  const fmt = function(z, digits) {
    return isNumber(z) ? z.toLocaleString('en-GB', {
      minimumFractionDigits: digits,
      maximumFractionDigits: digits
    }) : 'NA';
  };
  const pct = function(z, digits) {
    return isNumber(z) ? fmt(z, digits) + '%' : 'Not available';
  };
  const clamp = function(z) {
    return isNumber(z) ? Math.max(0, Math.min(100, z)) : 0;
  };

  const build = function(d) {
    const method = d.method === 'Paired stocks only' ?
      'Paired stocks only' : 'Independent available values';

    // Percentages refer only to regional landings with known values.
    // A missing percentage is NOT equivalent to 0% excluded / 100% included.
    const excludedKnownPct = isNumber(d.excluded_share_known_landings_pct) ?
      clamp(d.excluded_share_known_landings_pct) : null;
    const includedKnownPct = excludedKnownPct === null ?
      null : 100 - excludedKnownPct;
    const coverageBar = excludedKnownPct === null ?
      '<div class="dv3-track dv3-track-coverage"></div>' +
      '<div class="dv3-coverage-labels"><span>Share unavailable: no positive known regional landings</span></div>' :
      '<div class="dv3-track dv3-track-coverage" role="img" aria-label="' +
      pct(includedKnownPct, 1) + ' of known regional landings included; ' +
      pct(excludedKnownPct, 1) + ' excluded">' +
      '<div class="dv3-coverage-included" style="width:' + includedKnownPct + '%"></div>' +
      '<div class="dv3-coverage-excluded" style="width:' + excludedKnownPct + '%"></div></div>' +
      '<div class="dv3-coverage-labels"><span>' + pct(includedKnownPct, 1) +
      ' included</span><span>' + pct(excludedKnownPct, 1) + ' excluded</span></div>';

    let html = '<div class="dv3-head">' +
      '<div><div class="dv3-guild">' + safe(d.guild) + '</div>' +
      '<div class="dv3-muted">' + safe(d.year) + ' · Estimated regional quantities</div></div>' +
      '<div><div class="dv3-rate">' + pct(d.discard_rate_pct, 2) + '</div>' +
      '<div class="dv3-muted" style="text-align:right">Discard rate</div></div></div>' +
      '<div class="dv3-grid">' +
      '<div><div class="dv3-muted">Landings</div><div class="dv3-number">' + fmt(d.landings_kt, 2) + ' kt</div></div>' +
      '<div><div class="dv3-muted">Discards</div><div class="dv3-number">' + fmt(d.discards_kt, 2) + ' kt</div></div></div>' +
      '<div class="dv3-muted">Method: ' + safe(method) + '</div>' +
      '<div class="dv3-rule"></div>' +
      '<div class="dv3-coverage"><div class="dv3-row"><span class="dv3-section">Data coverage</span>' +
      '<b>' + safe(d.n_included) + ' / ' + safe(d.n_available) + ' stocks</b></div>' +
      '<div class="dv3-row dv3-muted"><span>Excluded share of known regional landings</span>' +
      '<b>' + pct(d.excluded_share_known_landings_pct, 1) + '</b></div>' +
      coverageBar +
      '<div class="dv3-muted" style="margin-top:7px">' + safe(d.n_excluded) + ' excluded stock(s)' +
      '<br>No ecoregion landings share available: ' + safe(d.excluded_share_unavailable) +
      '<br>No usable landings/discards (zero, missing, invalid or incomplete pair): ' +
      safe(d.excluded_no_positive_quantity) + '</div></div>' +
      '<div class="dv3-rule"></div>' +
      '<div class="dv3-row" style="align-items:center"><span class="dv3-section">Included stock composition</span>' +
      '<span class="dv3-legend"><span><i class="dv3-key dv3-L"></i>Landings</span>' +
      '<span><i class="dv3-key dv3-D"></i>Discards</span></span></div>' +
      '<div class="dv3-muted" style="margin-bottom:4px">Bar length = share of included guild landings + discards</div>';

    if (!Array.isArray(d.stocks) || d.stocks.length === 0) {
      html += '<div class="dv3-muted">No positive stock quantities</div>';
    } else {
      d.stocks.forEach(function(s) {
        html += '<div class="dv3-stock"><div class="dv3-row">' +
          '<span class="dv3-code">' + safe(s.stock) + '</span>' +
          '<span class="dv3-muted" style="white-space:nowrap">' + pct(s.share_pct, 1) + ' of guild</span></div>' +
          '<div class="dv3-track" style="margin-top:5px">' +
          '<div class="dv3-L" style="width:' + clamp(s.landings_share_pct) + '%"></div>' +
          '<div class="dv3-D" style="width:' + clamp(s.discards_share_pct) + '%"></div></div>' +
          '<div class="dv3-details">L: ' + fmt(s.landings_t, 0) + ' t  ·  D: ' +
          fmt(s.discards_t, 0) + ' t  ·  Stock discard rate: ' +
          (isNumber(s.rate_pct) ? pct(s.rate_pct, 1) : 'NA (incomplete)') + '</div></div>';
      });
    }
    if (d.additional_stocks > 0) {
      html += '<div class="dv3-muted" style="margin-top:7px">... and ' +
        safe(d.additional_stocks) + ' more stocks</div>';
    }
    return html;
  };

  return {tooltip, build};
}
)---"

  add_html_hover(p, js)
}
