#' Plot projected fire-regime multipliers against a baseline
#'
#' One panel per quantity (fire count, mean fire size and area burned, say),
#' each on a log scale of multipliers of a baseline regime, which is the dashed
#' rule at 1. The x axis is a free grouping column:
#'
#' * numeric, with `x_end` given (climate periods stepped through one run):
#'   each value holds from `x` to `x_end`, drawn as a step, with `lower` to
#'   `upper` shaded, and the last value labelled at the end of its step;
#' * anything else (stationary climate envelopes, say): a point per series at
#'   each category, with `lower` to `upper` as a bar.
#'
#' With `panel`, the facets are `quantity ~ panel` (a fire zone, say). The
#' facet is an ordinary [ggplot2::facet_grid()] or [ggplot2::facet_wrap()]:
#' adding another facet to the returned plot replaces it.
#'
#' The plot draws what it is given. Whether the multipliers are a design's
#' projected means or simulated values, and how they were derived, is for the
#' caller's title and caption to say.
#'
#' @param factors Data frame, one row per value: the columns named by `x`,
#'   `series` and (optionally) `x_end` and `panel`, plus `quantity`,
#'   `estimate` and, optionally, `lower` and `upper`. `estimate` must be
#'   positive (log scale). Quantities are drawn in the order of `quantity`'s
#'   factor levels, or of first appearance.
#' @param x,series Names of the x and colour columns.
#' @param x_end Optional name of the column where each value's step ends.
#' @param panel Optional name of a second facet column.
#' @param colours Optional named colours, one per series. Without them up to
#'   three series take the calibration palette's `simulated`, `observed` and
#'   `alternate` roles; more need `colours`.
#' @param baseline The multiplier of the baseline regime, drawn as the dashed
#'   rule.
#'
#' @returns A ggplot object with no title or axis titles.
#'
#' @family calibration plots
#' @export
#' @examples
#' if (requireNamespace("ggplot2", quietly = TRUE)) {
#'   f <- expand.grid(
#'     start = c(2026, 2041, 2071),
#'     series = c("A", "B"),
#'     quantity = c("count", "size", "area"),
#'     stringsAsFactors = FALSE
#'   )
#'   f$end <- c(2041, 2071, 2101)[match(f$start, c(2026, 2041, 2071))]
#'   f$estimate <- 1 + 0.2 * match(f$start, unique(f$start)) * match(f$series, c("A", "B"))
#'   plot_fire_regime_multipliers(f, x = "start", series = "series", x_end = "end")
#' }
plot_fire_regime_multipliers <- function(
  factors,
  x,
  series,
  x_end = NULL,
  panel = NULL,
  colours = NULL,
  baseline = 1
) {
  .need("ggplot2", "plot_fire_regime_multipliers()")
  need <- c(x, series, x_end, panel, "quantity", "estimate")
  if (!is.data.frame(factors) || !all(need %in% names(factors))) {
    stop(
      "`factors` must be a data frame with the columns ",
      paste(need, collapse = ", "),
      ".",
      call. = FALSE
    )
  }
  if (any(!is.finite(factors$estimate) | factors$estimate <= 0)) {
    stop("`estimate` must be finite and positive: the scale is logarithmic.", call. = FALSE)
  }
  pal <- calibration_plot_palette()
  df <- data.frame(
    x = factors[[x]],
    series = as.character(factors[[series]]),
    quantity = factors$quantity,
    estimate = factors$estimate,
    lower = if ("lower" %in% names(factors)) factors$lower else NA_real_,
    upper = if ("upper" %in% names(factors)) factors$upper else NA_real_
  )
  if (!is.factor(df$quantity)) {
    df$quantity <- factor(df$quantity, levels = unique(df$quantity))
  }
  df$panel <- if (is.null(panel)) "all" else factors[[panel]]
  series_levels <- if (is.factor(factors[[series]])) {
    levels(droplevels(factors[[series]]))
  } else {
    unique(df$series)
  }
  df$series <- factor(df$series, levels = series_levels)
  if (is.null(colours)) {
    if (length(series_levels) > 3L) {
      stop(
        "more than three series need `colours`, one per series: ",
        paste(series_levels, collapse = ", "),
        call. = FALSE
      )
    }
    colours <- stats::setNames(
      unname(pal[c("simulated", "observed", "alternate")][seq_along(series_levels)]),
      series_levels
    )
  }
  if (!all(series_levels %in% names(colours))) {
    stop(
      "`colours` names no colour for: ",
      paste(setdiff(series_levels, names(colours)), collapse = ", "),
      call. = FALSE
    )
  }
  band <- df[is.finite(df$lower) & is.finite(df$upper) & df$lower != df$upper, , drop = FALSE]
  steps <- !is.null(x_end) && is.numeric(df$x)
  gg <- ggplot2::ggplot() +
    ggplot2::geom_hline(
      yintercept = baseline,
      linetype = "22",
      colour = pal[["muted"]],
      linewidth = 0.8
    )
  if (steps) {
    df$x_end <- factors[[x_end]]
    band$x_end <- df$x_end[is.finite(df$lower) & is.finite(df$upper) & df$lower != df$upper]
    ## Each value is held from its start to its end: two points per value, joined in order.
    path <- do.call(
      rbind,
      lapply(split(df, list(df$series, df$quantity, df$panel), drop = TRUE), function(d) {
        d <- d[order(d$x), , drop = FALSE]
        data.frame(
          series = d$series[[1L]],
          quantity = d$quantity[[1L]],
          panel = d$panel[[1L]],
          px = as.vector(rbind(d$x, d$x_end)),
          py = rep(d$estimate, each = 2L)
        )
      })
    )
    last <- df[df$x_end == max(df$x_end), , drop = FALSE]
    last$text <- paste0(format(round(last$estimate, 2), nsmall = 2, trim = TRUE), "x")
    span <- diff(range(c(df$x, df$x_end)))
    if (nrow(band)) {
      gg <- gg +
        ggplot2::geom_rect(
          data = band,
          ggplot2::aes(
            xmin = .data$x,
            xmax = .data$x_end,
            ymin = .data$lower,
            ymax = .data$upper,
            fill = .data$series
          ),
          alpha = 0.18,
          colour = NA,
          show.legend = FALSE
        )
    }
    gg <- gg +
      ggplot2::geom_path(
        data = path,
        ggplot2::aes(.data$px, .data$py, colour = .data$series),
        linewidth = 1
      ) +
      ggplot2::geom_text(
        data = last,
        ggplot2::aes(.data$x_end + 0.015 * span, .data$estimate, label = .data$text),
        hjust = 0,
        size = 3.4,
        colour = pal[["muted"]]
      ) +
      ggplot2::scale_x_continuous(expand = ggplot2::expansion(mult = c(0.02, 0.2)))
  } else {
    dodge <- ggplot2::position_dodge(width = 0.5)
    if (nrow(band)) {
      gg <- gg +
        ggplot2::geom_errorbar(
          data = band,
          ggplot2::aes(x = .data$x, ymin = .data$lower, ymax = .data$upper, colour = .data$series),
          width = 0.2,
          position = dodge
        )
    }
    gg <- gg +
      ggplot2::geom_point(
        data = df,
        ggplot2::aes(.data$x, .data$estimate, colour = .data$series),
        size = 3,
        position = dodge
      )
  }
  breaks <- c(0.25, 0.5, 0.75, 1, 1.5, 2, 3, 4, 6, 8)
  lim <- range(baseline, df$estimate, band$lower, band$upper, na.rm = TRUE) * c(0.95, 1.06)
  gg <- gg +
    ggplot2::scale_colour_manual(values = colours[series_levels], name = NULL) +
    ggplot2::scale_fill_manual(values = colours[series_levels], guide = "none") +
    ggplot2::scale_y_log10(
      breaks = breaks,
      labels = paste0(format(breaks, drop0trailing = TRUE, trim = TRUE), "x"),
      limits = lim
    ) +
    ggplot2::labs(x = NULL, y = NULL) +
    .calibration_theme()
  if (is.null(panel)) {
    gg + ggplot2::facet_wrap(ggplot2::vars(.data$quantity), nrow = 1L)
  } else {
    gg + ggplot2::facet_grid(ggplot2::vars(.data$quantity), ggplot2::vars(.data$panel))
  }
}
