## Diagnostic plots for a Dynamic Fire calibration.
##
## Every function here consumes the objects this package already produces -- the
## `run_calibration_validation()` summary and the DEoptim trace written by
## `calibrate_dynamic_fire()` -- and returns a ggplot object for the caller to
## theme, annotate or save. Titles are deliberately descriptive rather than
## conclusory: what a given gap MEANS is a property of the landscape being
## calibrated, not of this package, so the interpretation belongs in the
## caller's `labs()`.

.calibration_palette_default <- c(
  observed = "#2a78d6",
  simulated = "#eb6834",
  alternate = "#1baf7a",
  ink = "#0b0b0b",
  muted = "#52514e",
  grid = "#e1e0d9"
)

#' Colour roles for the calibration diagnostic plots
#'
#' Returns the named colours the `plot_calibration_*()` functions use, with
#' optional per-role overrides. The defaults are checked for colour-vision
#' separation: `observed`/`simulated`/`alternate` clear the usual deuteranopia
#' and tritanopia thresholds against each other and against the plot surface.
#' Replacing them is supported but unchecked.
#'
#' @param ... Named overrides, e.g. `simulated = "red"`. Unknown role names are
#'   an error, so a typo cannot silently leave the default in place.
#'
#' @returns A named character vector of colours, with roles `observed`,
#'   `simulated`, `alternate`, `ink`, `muted` and `grid`.
#'
#' @family calibration plots
#' @export
calibration_plot_palette <- function(...) {
  out <- .calibration_palette_default
  over <- list(...)
  if (length(over) == 0L) {
    return(out)
  }
  if (is.null(names(over)) || !all(nzchar(names(over)))) {
    stop(
      "overrides must be named, e.g. calibration_plot_palette(simulated = 'red').",
      call. = FALSE
    )
  }
  unknown <- setdiff(names(over), names(out))
  if (length(unknown)) {
    stop(
      "unknown palette role(s): ",
      paste(unknown, collapse = ", "),
      ". Known roles: ",
      paste(names(out), collapse = ", "),
      ".",
      call. = FALSE
    )
  }
  out[names(over)] <- vapply(over, as.character, character(1))
  out
}

## Shared look. Deliberately light: the caller is expected to add its own theme
## on top for a report or a slide.
.calibration_theme <- function(base_size = 12) {
  .need("ggplot2", "the calibration plots")
  pal <- calibration_plot_palette()
  ggplot2::theme_bw(base_size = base_size) +
    ggplot2::theme(
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major = ggplot2::element_line(colour = pal[["grid"]], linewidth = 0.3),
      plot.title.position = "plot",
      legend.position = "top",
      legend.title = ggplot2::element_blank()
    )
}

## Validate the shape of a run_calibration_validation() summary before plotting,
## so a malformed payload fails with a useful message rather than deep inside a
## ggplot build.
.check_calibration_stats <- function(stats, need = character()) {
  if (!is.list(stats) || is.null(stats$reps) || is.null(stats$observed)) {
    stop(
      "`stats` must be a run_calibration_validation() summary ",
      "(a list with `reps` and `observed`).",
      call. = FALSE
    )
  }
  primary <- stats$observed$primary
  missing <- need[vapply(need, function(n) is.null(primary[[n]]), logical(1))]
  if (length(missing)) {
    stop(
      "`stats$observed$primary` is missing: ",
      paste(missing, collapse = ", "),
      ".",
      call. = FALSE
    )
  }
  invisible(TRUE)
}

#' Pool the per-replicate fire-event logs
#'
#' Binds every replicate's event table into one data frame and adds the burned
#' area of each event in hectares. One row per simulated fire event.
#'
#' `sites` is the event's burned cells (the log's `SitesChecked`) and
#' `mean_severity` its mean severity over those cells, as
#' [parse_dynamic_fire_logs()] recovers them. In the published extension the
#' log's own `MeanSeverity` is divided by `DamagedSites`, one more than the
#' burned cells, which is why raw values fall below 1.
#'
#' @param stats A `run_calibration_validation()` summary.
#'
#' @returns A data frame with `year`, `eco`, `init_fuel`, `sites`,
#'   `mean_severity` and `area_ha`.
#'
#' @family calibration plots
#' @export
calibration_events <- function(stats) {
  .check_calibration_stats(stats)
  ev <- do.call(rbind, lapply(stats$reps, function(r) r$events))
  ev <- as.data.frame(ev)
  cell <- if (is.null(stats$pixel_area_ha)) 1 else stats$pixel_area_ha
  ev$area_ha <- ev$sites * cell
  ev
}

## Bin a continuous Dynamic Fire severity onto integer classes 1..5, using the
## same half-integer breaks the objective's severity term uses.
.calibration_severity_bin <- function(x) {
  cut(x, breaks = c(-Inf, 1.5, 2.5, 3.5, 4.5, Inf), labels = as.character(1:5), right = TRUE)
}

## Collapse a length-5 severity vector to the three classes an observed
## reference typically distinguishes.
.calibration_collapse3 <- function(p) {
  c(Low = sum(p[1:2]), Medium = p[[3]], High = sum(p[4:5]))
}

#' Plot the DEoptim search trace
#'
#' Draws the best objective value reached by each generation. When `stats` is
#' supplied, the loss it records is added as a point at the last generation:
#' the search reports the minimum of many noisy evaluations and so is optimistic
#' about its own winner, and re-simulating that winner at a higher replicate
#' count is the honest measurement. The gap between line and point is that
#' optimism.
#'
#' @param trace The DEoptim trace: either a path to the CSV
#'   `calibrate_dynamic_fire()` writes (columns generation and best value) or a
#'   data frame with those two columns in that order.
#' @param stats Optional `run_calibration_validation()` summary, used for the
#'   re-checked loss point and the replicate count in its label.
#'
#' @returns A ggplot object.
#'
#' @family calibration plots
#' @export
plot_calibration_convergence <- function(trace, stats = NULL) {
  .need("ggplot2", "plot_calibration_convergence()")
  if (is.character(trace)) {
    if (!file.exists(trace)) {
      stop("trace file not found: ", trace, call. = FALSE)
    }
    trace <- utils::read.csv(trace)
  }
  trace <- as.data.frame(trace)
  if (ncol(trace) < 2L || nrow(trace) == 0L) {
    stop("`trace` needs at least two columns and one row.", call. = FALSE)
  }
  names(trace)[1:2] <- c("generation", "best_loss")
  pal <- calibration_plot_palette()

  gg <- ggplot2::ggplot(trace, ggplot2::aes(.data$generation, .data$best_loss)) +
    ggplot2::geom_step(colour = pal[["observed"]], linewidth = 0.9) +
    ggplot2::labs(title = "Search trace", x = "generation", y = "best objective value") +
    .calibration_theme()

  if (!is.null(stats) && !is.null(stats$loss$total)) {
    pt <- data.frame(generation = max(trace$generation), best_loss = stats$loss$total)
    lab <- if (is.null(stats$n_reps)) {
      "re-checked"
    } else {
      sprintf("re-checked at %d reps", stats$n_reps)
    }
    ## label precomputed as a column: keeping sprintf() out of aes() means the
    ## layer maps one plain variable instead of an expression over a mask
    pt$label <- sprintf("%s: %.3f", lab, pt$best_loss)
    gg <- gg +
      ggplot2::geom_point(data = pt, colour = pal[["simulated"]], size = 3) +
      ggplot2::geom_text(
        data = pt,
        ggplot2::aes(label = .data$label),
        hjust = 1.1,
        vjust = 0.5,
        colour = pal[["simulated"]],
        size = 3.6
      )
  }
  gg
}

#' Plot the weighted contribution of each objective component
#'
#' Bar lengths are `component x weight`, so they sum to the total loss and can
#' be read as a budget: which part of the fire regime the remaining
#' disagreement is in.
#'
#' @param stats A `run_calibration_validation()` summary carrying `$loss`.
#' @param labels Optional named character vector renaming components for
#'   display, e.g. `c(count = "Fires per year")`. Names are component names;
#'   unmatched components keep their own name.
#'
#' @returns A ggplot object.
#'
#' @family calibration plots
#' @export
plot_calibration_loss <- function(stats, labels = NULL) {
  .need("ggplot2", "plot_calibration_loss()")
  if (!is.list(stats) || is.null(stats$loss)) {
    stop("`stats` must carry a `$loss` element.", call. = FALSE)
  }
  lv <- unlist(stats$loss)
  comp <- lv[grep("^components\\.", names(lv))]
  wt <- lv[grep("^weights\\.", names(lv))]
  if (length(comp) == 0L) {
    stop("`stats$loss` has no `components` element to plot.", call. = FALSE)
  }
  names(comp) <- sub("^components\\.", "", names(comp))
  names(wt) <- sub("^weights\\.", "", names(wt))
  w <- wt[names(comp)]
  w[is.na(w)] <- 1

  df <- data.frame(
    component = names(comp),
    weighted = as.numeric(comp) * as.numeric(w),
    stringsAsFactors = FALSE
  )
  df$label <- df$component
  if (!is.null(labels)) {
    hit <- df$component %in% names(labels)
    df$label[hit] <- unname(labels[df$component[hit]])
  }
  df <- df[order(df$weighted), ]
  df$label <- factor(df$label, levels = df$label)
  pal <- calibration_plot_palette()

  ggplot2::ggplot(df, ggplot2::aes(.data$weighted, .data$label)) +
    ggplot2::geom_col(fill = pal[["simulated"]], width = 0.6) +
    ggplot2::geom_text(
      ggplot2::aes(label = sprintf("%.3f", .data$weighted)),
      hjust = -0.15,
      colour = pal[["muted"]],
      size = 3.6
    ) +
    ggplot2::scale_x_continuous(expand = ggplot2::expansion(mult = c(0, 0.18))) +
    ggplot2::labs(
      title = "Weighted contribution to the objective",
      x = "component value x weight",
      y = NULL
    ) +
    .calibration_theme()
}

#' Plot observed against simulated fire sizes
#'
#' Empirical cumulative distributions on a log size axis. Both sides are
#' truncated at the observed `min_size_ha` floor when the summary records one,
#' matching the truncation the objective's size term applies.
#'
#' @param stats A `run_calibration_validation()` summary.
#'
#' @returns A ggplot object.
#'
#' @family calibration plots
#' @export
plot_calibration_fire_sizes <- function(stats) {
  .need("ggplot2", "plot_calibration_fire_sizes()")
  .check_calibration_stats(stats, need = "fire_sizes_ha")
  obs <- stats$observed$primary$fire_sizes_ha
  sim <- unlist(lapply(stats$reps, function(r) r$fire_sizes_ha))
  floor_ha <- stats$observed$min_size_ha
  if (!is.null(floor_ha) && floor_ha > 0) {
    obs <- obs[obs >= floor_ha]
    sim <- sim[sim >= floor_ha]
  }
  df <- rbind(
    data.frame(source = "observed", size = obs),
    data.frame(source = "simulated", size = sim)
  )
  df <- df[df$size > 0, ]
  pal <- calibration_plot_palette()

  ggplot2::ggplot(df, ggplot2::aes(.data$size, colour = .data$source)) +
    ggplot2::stat_ecdf(linewidth = 0.9) +
    ggplot2::scale_x_log10() +
    ggplot2::scale_colour_manual(
      values = c(observed = pal[["observed"]], simulated = pal[["simulated"]])
    ) +
    ggplot2::labs(
      title = "Fire-size distribution",
      x = "fire size (ha, log scale)",
      y = "cumulative share of fires"
    ) +
    .calibration_theme()
}

#' Plot observed against simulated annual fire counts
#'
#' Overlaid densities of fires per year, with each side's mean marked. The
#' spread matters as much as the mean: a model can match the average year and
#' still never produce the extreme years that carry most of the area burned.
#'
#' @param stats A `run_calibration_validation()` summary.
#' @param bins Number of histogram bins.
#'
#' @returns A ggplot object.
#'
#' @family calibration plots
#' @export
plot_calibration_fire_counts <- function(stats, bins = 24L) {
  .need("ggplot2", "plot_calibration_fire_counts()")
  .check_calibration_stats(stats, need = "n_fires_by_year")
  obs <- stats$observed$primary$n_fires_by_year$n
  ## drop the summary log's Time 0 row: it is the initial state, always zero fires, and one spurious
  ## zero per replicate both shifts the mean down and (where counts are large) collapses the spread
  sim <- unlist(lapply(stats$reps, function(r) .drop_initial_timestep(r$n_fires_by_year)$n_fires))
  df <- rbind(data.frame(source = "observed", n = obs), data.frame(source = "simulated", n = sim))
  means <- data.frame(source = c("observed", "simulated"), n = c(mean(obs), mean(sim)))
  pal <- calibration_plot_palette()
  cols <- c(observed = pal[["observed"]], simulated = pal[["simulated"]])

  ggplot2::ggplot(df, ggplot2::aes(.data$n, fill = .data$source)) +
    ggplot2::geom_histogram(
      ggplot2::aes(y = ggplot2::after_stat(.data$density)),
      bins = bins,
      position = "identity",
      alpha = 0.6,
      colour = NA
    ) +
    ggplot2::geom_vline(
      data = means,
      ggplot2::aes(xintercept = .data$n, colour = .data$source),
      linewidth = 0.8,
      linetype = "22",
      show.legend = FALSE
    ) +
    ggplot2::scale_fill_manual(values = cols) +
    ggplot2::scale_colour_manual(values = cols) +
    ggplot2::labs(title = "Fires per year", x = "fires per year", y = "density") +
    .calibration_theme()
}

#' Plot observed against simulated area burned by base fuel type
#'
#' Plotted as shares rather than totals, because the objective's area-by-fuel
#' term renormalises both sides over the base fuels shared between them.
#'
#' Simulated base fuel types are the `base` column of each replicate's
#' `area_by_fuel_ha`, which [parse_dynamic_fire_logs()] takes from the run's own
#' `FuelTypeTable`. A replicate parsed before landisutils 0.0.168 has no `base`
#' column and is decoded through [defaultFuelTypeTable()], with a warning, as in
#' [loss_from_stats()].
#'
#' @param stats A `run_calibration_validation()` summary carrying
#'   `$observed$primary$area_by_fuel_ha`.
#'
#' @returns A ggplot object.
#'
#' @family calibration plots
#' @export
plot_calibration_area_by_fuel <- function(stats) {
  .need("ggplot2", "plot_calibration_area_by_fuel()")
  .check_calibration_stats(stats, need = "area_by_fuel_ha")
  obs <- stats$observed$primary$area_by_fuel_ha
  rep_dfs <- Filter(Negate(is.null), lapply(stats$reps, function(r) r$area_by_fuel_ha))
  if (length(rep_dfs) == 0L) {
    stop(
      "no per-replicate `area_by_fuel_ha`; the replicates predate cell-based ",
      "fuel attribution.",
      call. = FALSE
    )
  }
  ## Never through `stats$observed$fuel_code_to_base`: that decodes the OBSERVED raster's codes,
  ## and read through it a FuelType map value (fuel-type index + 1) names the wrong fuel type.
  ca <- do.call(rbind, lapply(rep_dfs, function(d) .area_by_fuel_base(d)[, c("base", "area_ha")]))
  ca <- ca[!is.na(ca$base), , drop = FALSE]
  sim <- tapply(ca$area_ha, ca$base, sum)

  df <- rbind(
    data.frame(source = "observed", base = obs$base, area = obs$area_ha),
    data.frame(source = "simulated", base = names(sim), area = as.numeric(sim))
  )
  df$share <- stats::ave(df$area, df$source, FUN = function(x) x / sum(x))
  pal <- calibration_plot_palette()

  ggplot2::ggplot(df, ggplot2::aes(.data$base, .data$share, fill = .data$source)) +
    ggplot2::geom_col(position = ggplot2::position_dodge(width = 0.72), width = 0.6) +
    ggplot2::scale_fill_manual(
      values = c(observed = pal[["observed"]], simulated = pal[["simulated"]])
    ) +
    ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0, 0.1))) +
    ggplot2::labs(
      title = "Area burned by base fuel type",
      x = "base fuel type",
      y = "share of burned area"
    ) +
    .calibration_theme()
}

#' Plot observed against simulated burn-severity classes
#'
#' @section Weighting:
#' `weight_by` is the important argument. The objective's severity term counts
#' fire EVENTS: it bins each event's mean severity and gives every fire one
#' vote, whatever its size (`weight_by = "events"`, the default, so the figure
#' matches what was scored). An observed severity reference is almost always a
#' share of burned AREA -- so many pixels or hectares in each class -- and where
#' severity rises with fire size the two summaries of the same simulation can
#' differ sharply, and can even disagree in sign about which way the model is
#' off. `weight_by = "area"` weights each event by the area it burned, which is
#' the like-for-like comparison against an area-weighted reference.
#'
#' Both options still summarise each fire by its mean severity, so neither
#' recovers the within-fire variation that a per-pixel reference retains.
#'
#' @param stats A `run_calibration_validation()` summary carrying
#'   `$observed$primary$severity_dist`.
#' @param weight_by `"events"` (one vote per fire) or `"area"` (weight each
#'   fire by the area it burned). See Weighting.
#' @param classes `3` to collapse to low/medium/high before plotting -- the
#'   three categories a thresholded observed reference usually distinguishes --
#'   or `5` for the raw Dynamic Fire classes.
#'
#' @returns A ggplot object.
#'
#' @family calibration plots
#' @export
plot_calibration_severity <- function(stats, weight_by = c("events", "area"), classes = c(3, 5)) {
  .need("ggplot2", "plot_calibration_severity()")
  .check_calibration_stats(stats, need = "severity_dist")
  weight_by <- match.arg(weight_by)
  classes <- as.integer(classes[[1]])
  if (!classes %in% c(3L, 5L)) {
    stop("`classes` must be 3 or 5.", call. = FALSE)
  }
  ev <- calibration_events(stats)
  ev$cls <- .calibration_severity_bin(ev$mean_severity)

  sim <- if (identical(weight_by, "area")) {
    a <- tapply(ev$area_ha, ev$cls, sum)
    a[is.na(a)] <- 0
    as.numeric(a / sum(a))
  } else {
    as.numeric(prop.table(table(ev$cls)))
  }
  obs <- as.numeric(stats$observed$primary$severity_dist)

  if (classes == 3L) {
    sim <- .calibration_collapse3(sim)
    obs <- .calibration_collapse3(obs)
    lev <- c("Low", "Medium", "High")
  } else {
    lev <- as.character(1:5)
  }
  df <- rbind(
    data.frame(source = "observed", class = lev, prop = as.numeric(obs)),
    data.frame(source = "simulated", class = lev, prop = as.numeric(sim))
  )
  df$class <- factor(df$class, levels = lev)
  pal <- calibration_plot_palette()

  ggplot2::ggplot(df, ggplot2::aes(.data$class, .data$prop, fill = .data$source)) +
    ggplot2::geom_col(position = ggplot2::position_dodge(width = 0.72), width = 0.6) +
    ggplot2::scale_fill_manual(
      values = c(observed = pal[["observed"]], simulated = pal[["simulated"]])
    ) +
    ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0, 0.1))) +
    ggplot2::labs(
      title = "Burn-severity distribution",
      subtitle = sprintf(
        "simulated side weighted by %s",
        if (identical(weight_by, "area")) "area burned" else "fire count"
      ),
      x = "severity class",
      y = "share of burning"
    ) +
    .calibration_theme()
}

#' Plot simulated fire size against fire severity
#'
#' Each simulated fire as a point, with the mean severity of each size class
#' drawn over it. Where the line rises, the severity distribution depends on how
#' the fires are weighted, and `plot_calibration_severity()`'s `weight_by`
#' argument stops being cosmetic: a size class holding a small share of the
#' fires can hold most of the area.
#'
#' @param stats A `run_calibration_validation()` summary.
#' @param breaks Size-class boundaries in hectares, passed to [base::cut()]. The
#'   default spans four orders of magnitude.
#'
#' @returns A ggplot object. Its `size_summary` attribute holds the per-class
#'   share of fires, share of area, and mean severity.
#'
#' @family calibration plots
#' @export
plot_calibration_severity_by_size <- function(stats, breaks = c(0, 10, 100, 1000, 10000, Inf)) {
  .need("ggplot2", "plot_calibration_severity_by_size()")
  .check_calibration_stats(stats)
  ev <- calibration_events(stats)
  if (nrow(ev) == 0L) {
    stop("no simulated fire events to plot.", call. = FALSE)
  }
  ev$size_class <- cut(ev$area_ha, breaks = breaks)
  summ <- data.frame(
    size_class = levels(ev$size_class),
    n = as.numeric(table(ev$size_class)),
    area = as.numeric(tapply(ev$area_ha, ev$size_class, sum)),
    severity = as.numeric(tapply(ev$mean_severity, ev$size_class, mean)),
    stringsAsFactors = FALSE
  )
  summ$area[is.na(summ$area)] <- 0
  summ$share_n <- summ$n / sum(summ$n)
  summ$share_area <- summ$area / sum(summ$area)
  ## geometric centre of each class, clamped to the data, so the summary line
  ## sits over the points it summarises on a log axis
  lo <- pmax(utils::head(breaks, -1L), min(ev$area_ha))
  hi <- pmin(breaks[-1L], max(ev$area_ha))
  summ$x <- 10^((log10(lo) + log10(hi)) / 2)
  pal <- calibration_plot_palette()

  gg <- ggplot2::ggplot(ev, ggplot2::aes(.data$area_ha, .data$mean_severity)) +
    ggplot2::geom_point(colour = pal[["simulated"]], alpha = 0.15, size = 1.4) +
    ggplot2::geom_line(
      data = summ,
      ggplot2::aes(.data$x, .data$severity),
      colour = pal[["ink"]],
      linewidth = 0.9
    ) +
    ggplot2::geom_point(
      data = summ,
      ggplot2::aes(.data$x, .data$severity),
      colour = pal[["ink"]],
      size = 2.4
    ) +
    ggplot2::scale_x_log10() +
    ggplot2::labs(
      title = "Fire size against fire severity",
      x = "fire size (ha, log scale)",
      y = "mean severity of the fire"
    ) +
    .calibration_theme()
  attr(gg, "size_summary") <- summ
  gg
}

## "1st", "22nd", "69th": a percentile as the plots label it.
.calibration_ordinal <- function(x) {
  x <- round(x)
  suffix <- ifelse(x %% 100 %in% 11:13, "th", c("th", "st", "nd", "rd", rep("th", 6))[x %% 10 + 1])
  paste0(x, suffix)
}

.check_whiskers <- function(whiskers) {
  ok <- is.numeric(whiskers) &&
    length(whiskers) == 2L &&
    all(is.finite(whiskers)) &&
    whiskers[[1]] >= 0 &&
    whiskers[[1]] <= 0.25 &&
    whiskers[[2]] >= 0.75 &&
    whiskers[[2]] <= 1
  if (!ok) {
    stop(
      "`whiskers` must be two probabilities, the lower at most 0.25 and the upper at least 0.75.",
      call. = FALSE
    )
  }
  invisible(whiskers)
}

#' Box-and-whisker statistics with whiskers at chosen quantiles
#'
#' The quartiles and median for the box, and whiskers at the `whiskers`
#' quantiles rather than at 1.5 times the interquartile range, so that a
#' whisker can mark the range a calibration check uses (its 5th and 95th
#' percentiles, say).
#'
#' All five values are [stats::quantile()] type 7: INTERPOLATED quantiles,
#' not order statistics. With 40 values the 5th percentile falls between the
#' second and third smallest, so a check defined as "the second lowest
#' replicate" will not match the lower whisker exactly.
#'
#' @param x Numeric. Non-finite values are dropped.
#' @param whiskers The quantiles the whiskers reach: two probabilities, the
#'   lower at most 0.25 and the upper at least 0.75.
#'
#' @returns A list: `stats`, a one-row data frame with `ymin`, `lower`,
#'   `middle`, `upper` and `ymax` (the names `ggplot2::geom_boxplot(stat =
#'   "identity")` takes), and `out`, the values beyond the whiskers.
#'
#' @family calibration plots
#' @export
#' @examples
#' calibration_box_stats(1:20, whiskers = c(0.1, 0.9))
calibration_box_stats <- function(x, whiskers = c(0.05, 0.95)) {
  .check_whiskers(whiskers)
  x <- x[is.finite(x)]
  if (length(x) == 0L) {
    stop("`x` holds no finite value.", call. = FALSE)
  }
  q <- stats::quantile(x, c(whiskers[[1]], 0.25, 0.5, 0.75, whiskers[[2]]), names = FALSE, type = 7)
  list(
    stats = data.frame(
      ymin = q[[1]],
      lower = q[[2]],
      middle = q[[3]],
      upper = q[[4]],
      ymax = q[[5]]
    ),
    out = x[x < q[[1]] | x > q[[5]]]
  )
}

#' Mid-rank percentile of a value among simulated replicates
#'
#' `100 * (mean(sim < x) + 0.5 * mean(sim == x))`: the share of replicates
#' below `x`, counting ties as half. Pass both sides on the scale a check uses;
#' a value compared with rounded totals should be rounded the same way first,
#' so a tie is a tie.
#'
#' @param sim Numeric, one value per replicate. Non-finite values are dropped.
#' @param x Numeric, the value(s) to place.
#'
#' @returns Numeric, one percentile per element of `x`, or `NA` where `x` is
#'   not finite or `sim` holds no finite value.
#'
#' @family calibration plots
#' @export
#' @examples
#' replicate_percentile(1:10, c(0.5, 5, 5.5, 11))
replicate_percentile <- function(sim, x) {
  sim <- sim[is.finite(sim)]
  vapply(
    x,
    function(v) {
      if (length(sim) == 0L || !is.finite(v)) {
        return(NA_real_)
      }
      100 * (mean(sim < v) + 0.5 * mean(sim == v))
    },
    numeric(1)
  )
}

#' Plot simulated replicates against reference values
#'
#' One box of per-replicate values ([calibration_box_stats()]: quartiles and
#' median, whiskers at `whiskers`, the replicates beyond them as dots), with
#' each reference beside it in a column of its own, labelled with its value and
#' its [replicate_percentile()] among the replicates. Dashed lines at the
#' whiskers' ends run across the panel, so each reference reads against them.
#'
#' Whether the whiskers are an acceptance range or only a spread is the
#' caller's to say: a calibration that scores pooled statistics, not
#' replicates, should say the box is a spread.
#'
#' @param values Numeric, one value per replicate.
#' @param references Optional data frame with `name` and `value`, one row per
#'   reference value (a recorded value, a target), in the order to draw them.
#' @param whiskers See [calibration_box_stats()].
#' @param label Optional function turning a value into its label text; the
#'   default prints three significant figures. Units are the caller's.
#' @param name The label of the replicates' column.
#'
#' @returns A ggplot object with no title or axis titles.
#'
#' @family calibration plots
#' @export
#' @examples
#' if (requireNamespace("ggplot2", quietly = TRUE)) {
#'   plot_calibration_replicates(
#'     c(3, 5, 6, 8, 9, 10, 11, 12, 14, 16, 20, 26, 39),
#'     data.frame(name = c("Record", "Target"), value = c(15.3, 13.5))
#'   )
#' }
plot_calibration_replicates <- function(
  values,
  references = NULL,
  whiskers = c(0.05, 0.95),
  label = NULL,
  name = "Simulated"
) {
  .need("ggplot2", "plot_calibration_replicates()")
  bad_ref <- !is.null(references) &&
    (!is.data.frame(references) || !all(c("name", "value") %in% names(references)))
  if (bad_ref) {
    stop("`references` must be a data frame with `name` and `value`.", call. = FALSE)
  }
  label <- label %||% function(v) format(signif(v, 3), trim = TRUE, drop0trailing = TRUE)
  pal <- calibration_plot_palette()
  box <- calibration_box_stats(values, whiskers)
  n_ref <- if (is.null(references)) 0L else nrow(references)
  ref <- if (n_ref > 0L) {
    data.frame(
      x = 1L + seq_len(n_ref),
      y = references$value,
      text = sprintf(
        "%s\n%s percentile\nof the replicates",
        label(references$value),
        .calibration_ordinal(replicate_percentile(values, references$value))
      )
    )
  }
  ends <- data.frame(
    y = c(box$stats$ymin, box$stats$ymax),
    text = sprintf(
      "%s percentile, %s",
      .calibration_ordinal(100 * whiskers),
      label(c(box$stats$ymin, box$stats$ymax))
    )
  )
  right <- n_ref + 1.6
  gg <- ggplot2::ggplot() +
    ggplot2::geom_hline(
      data = ends,
      ggplot2::aes(yintercept = .data$y),
      colour = pal[["simulated"]],
      linewidth = 0.4,
      linetype = "22"
    ) +
    ggplot2::geom_text(
      data = ends,
      ggplot2::aes(x = right - 0.05, y = .data$y, label = .data$text),
      hjust = 1,
      vjust = -0.4,
      size = 3.4,
      colour = pal[["muted"]]
    ) +
    ggplot2::geom_boxplot(
      data = cbind(x = 1, box$stats),
      ggplot2::aes(
        x = .data$x,
        ymin = .data$ymin,
        lower = .data$lower,
        middle = .data$middle,
        upper = .data$upper,
        ymax = .data$ymax,
        group = 1
      ),
      stat = "identity",
      width = 0.45,
      fill = pal[["simulated"]],
      alpha = 0.35,
      colour = pal[["simulated"]],
      linewidth = 0.8
    ) +
    ggplot2::geom_point(
      data = data.frame(x = rep(1, length(box$out)), y = box$out),
      ggplot2::aes(.data$x, .data$y),
      colour = pal[["simulated"]],
      size = 2.4
    )
  if (n_ref > 0L) {
    gg <- gg +
      ggplot2::geom_point(
        data = ref,
        ggplot2::aes(.data$x, .data$y),
        colour = pal[["observed"]],
        size = 4.2
      ) +
      ggplot2::geom_text(
        data = ref,
        ggplot2::aes(.data$x, .data$y, label = .data$text),
        hjust = -0.18,
        size = 3.5,
        lineheight = 0.9,
        colour = pal[["muted"]]
      )
  }
  gg +
    ggplot2::scale_x_continuous(
      breaks = seq_len(n_ref + 1L),
      labels = c(name, if (n_ref > 0L) references$name),
      limits = c(0.5, right),
      expand = ggplot2::expansion(0)
    ) +
    ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0.02, 0.06))) +
    ggplot2::labs(x = NULL, y = NULL) +
    .calibration_theme() +
    ggplot2::theme(panel.grid.major.x = ggplot2::element_blank())
}

#' Plot observed against simulated spreads, group by group
#'
#' For each group, an observed and a simulated box side by side
#' ([calibration_box_stats()]), with diamonds at the values the caller
#' compares. Groups are drawn in the order of `spread$group`'s factor levels,
#' or of first appearance.
#'
#' A group with no observed values in `spread` (a record with one value and no
#' resampling) has its observed side drawn from `reference`: a point, and a
#' whisker-only bar where `lower` and `upper` are given. Observed values beyond
#' the whiskers are not dotted, since resampling draws are not observations;
#' simulated replicates beyond them are. A diamond marks a compared value only
#' on a side drawn as a box. Whether a group passes any check is the caller's
#' to say, through `note`.
#'
#' @param spread Data frame with `group`, `source` (`"observed"` or
#'   `"simulated"`) and `value`, one row per draw or replicate.
#' @param reference Optional data frame, one row per group: `group`, and any of
#'   `observed` (the observed value compared), `simulated` (the simulated value
#'   compared, e.g. pooled over replicates), `lower` and `upper` (an interval
#'   around the observed value) and `note` (text printed above the group).
#' @param whiskers See [calibration_box_stats()].
#' @param labels Legend labels for the two sources.
#'
#' @returns A ggplot object with no title or axis titles.
#'
#' @family calibration plots
#' @export
#' @examples
#' if (requireNamespace("ggplot2", quietly = TRUE)) {
#'   spread <- data.frame(
#'     group = rep(c("A", "B"), each = 60),
#'     source = rep(rep(c("observed", "simulated"), each = 30), 2),
#'     value = c(seq(0.2, 0.5, length.out = 30), seq(0.25, 0.45, length.out = 30),
#'               seq(0.1, 0.4, length.out = 30), seq(0, 0.6, length.out = 30))
#'   )
#'   plot_calibration_groups(spread, data.frame(group = c("A", "B"), note = c("inside", "inside")))
#' }
plot_calibration_groups <- function(
  spread,
  reference = NULL,
  whiskers = c(0.05, 0.95),
  labels = c(observed = "Observed", simulated = "Simulated")
) {
  .need("ggplot2", "plot_calibration_groups()")
  if (!is.data.frame(spread) || !all(c("group", "source", "value") %in% names(spread))) {
    stop("`spread` must be a data frame with `group`, `source` and `value`.", call. = FALSE)
  }
  bad <- setdiff(unique(spread$source), c("observed", "simulated"))
  if (length(bad)) {
    stop(
      "`spread$source` must be \"observed\" or \"simulated\", not: ",
      paste(bad, collapse = ", "),
      call. = FALSE
    )
  }
  if (!is.null(reference) && (!is.data.frame(reference) || !"group" %in% names(reference))) {
    stop("`reference` must be a data frame with a `group` column.", call. = FALSE)
  }
  .check_whiskers(whiskers)
  pal <- calibration_plot_palette()
  groups <- if (is.factor(spread$group)) {
    levels(droplevels(spread$group))
  } else {
    unique(as.character(spread$group))
  }
  groups <- c(groups, setdiff(as.character(reference$group), groups))
  ref_of <- function(g, col) {
    if (is.null(reference) || !col %in% names(reference)) {
      return(NA)
    }
    v <- reference[[col]][as.character(reference$group) == g]
    if (length(v)) v[[1]] else NA
  }
  offset <- c(observed = -0.2, simulated = 0.2)
  boxes <- list()
  outs <- list()
  points <- list()
  bars <- list()
  for (i in seq_along(groups)) {
    g <- groups[[i]]
    for (src in names(offset)) {
      v <- spread$value[as.character(spread$group) == g & spread$source == src]
      v <- v[is.finite(v)]
      x <- i + offset[[src]]
      if (length(v)) {
        b <- calibration_box_stats(v, whiskers)
        boxes[[length(boxes) + 1L]] <- data.frame(x = x, source = src, b$stats)
        if (src == "simulated" && length(b$out)) {
          outs[[length(outs) + 1L]] <- data.frame(x = x, y = b$out)
        }
      } else {
        p <- ref_of(g, src)
        if (is.finite(p)) {
          points[[length(points) + 1L]] <- data.frame(x = x, y = p, source = src)
        }
        lo <- ref_of(g, "lower")
        hi <- ref_of(g, "upper")
        if (src == "observed" && is.finite(lo) && is.finite(hi)) {
          bars[[length(bars) + 1L]] <- data.frame(x = x, lo = lo, hi = hi, source = src)
        }
      }
    }
  }
  rbind_or_null <- function(l) if (length(l)) do.call(rbind, l) else NULL
  boxes <- rbind_or_null(boxes)
  outs <- rbind_or_null(outs)
  points <- rbind_or_null(points)
  bars <- rbind_or_null(bars)
  ## A diamond marks a compared value on a box; a side drawn as a point is that value already.
  marks <- if (!is.null(reference)) {
    do.call(
      rbind,
      lapply(seq_along(groups), function(i) {
        y <- c(ref_of(groups[[i]], "observed"), ref_of(groups[[i]], "simulated"))
        data.frame(x = i + offset, y = as.numeric(y))
      })
    )
  }
  if (!is.null(marks)) {
    marks <- marks[is.finite(marks$y) & marks$x %in% boxes$x, , drop = FALSE]
  }
  notes <- if (!is.null(reference) && "note" %in% names(reference)) {
    n <- vapply(groups, function(g) as.character(ref_of(g, "note")), character(1))
    data.frame(x = seq_along(groups), note = n)[!is.na(n) & nzchar(n), , drop = FALSE]
  }
  top <- max(c(boxes$ymax, outs$y, points$y, bars$hi, marks$y), na.rm = TRUE)
  bottom <- min(c(boxes$ymin, outs$y, points$y, bars$lo, marks$y), na.rm = TRUE)
  if (!is.finite(top)) {
    stop("nothing to draw: `spread` and `reference` hold no finite value.", call. = FALSE)
  }
  span <- if (top > bottom) top - bottom else abs(top) + 1
  if (!is.null(notes)) {
    notes$y <- rep(top + 0.16 * span, nrow(notes))
  }
  gg <- ggplot2::ggplot()
  if (!is.null(boxes)) {
    gg <- gg +
      ggplot2::geom_boxplot(
        data = boxes,
        ggplot2::aes(
          x = .data$x,
          ymin = .data$ymin,
          lower = .data$lower,
          middle = .data$middle,
          upper = .data$upper,
          ymax = .data$ymax,
          group = .data$x,
          colour = .data$source,
          fill = .data$source
        ),
        stat = "identity",
        width = 0.32,
        alpha = 0.35,
        linewidth = 0.8
      )
  }
  if (!is.null(bars)) {
    gg <- gg +
      ggplot2::geom_errorbar(
        data = bars,
        ggplot2::aes(x = .data$x, ymin = .data$lo, ymax = .data$hi, colour = .data$source),
        width = 0.12,
        linewidth = 0.8
      )
  }
  if (!is.null(points)) {
    gg <- gg +
      ggplot2::geom_point(
        data = points,
        ggplot2::aes(.data$x, .data$y, colour = .data$source),
        size = 4
      )
  }
  if (!is.null(outs)) {
    gg <- gg +
      ggplot2::geom_point(
        data = outs,
        ggplot2::aes(.data$x, .data$y),
        colour = pal[["simulated"]],
        size = 2.2
      )
  }
  if (!is.null(marks) && nrow(marks)) {
    gg <- gg +
      ggplot2::geom_point(
        data = marks,
        ggplot2::aes(.data$x, .data$y),
        shape = 23,
        size = 3.4,
        fill = "white",
        colour = pal[["ink"]],
        stroke = 1
      )
  }
  if (!is.null(notes) && nrow(notes)) {
    gg <- gg +
      ggplot2::geom_text(
        data = notes,
        ggplot2::aes(.data$x, .data$y, label = .data$note),
        vjust = 1,
        size = 3.5,
        lineheight = 0.95,
        colour = pal[["muted"]]
      )
  }
  gg +
    ggplot2::scale_colour_manual(
      values = c(observed = pal[["observed"]], simulated = pal[["simulated"]]),
      labels = labels,
      breaks = c("observed", "simulated"),
      name = NULL,
      aesthetics = c("colour", "fill")
    ) +
    ggplot2::scale_x_continuous(
      breaks = seq_along(groups),
      labels = groups,
      limits = c(0.5, length(groups) + 0.5),
      expand = ggplot2::expansion(0)
    ) +
    ggplot2::scale_y_continuous(
      limits = c(
        bottom - 0.04 * span,
        top + (if (!is.null(notes) && nrow(notes)) 0.18 else 0.06) * span
      )
    ) +
    ggplot2::labs(x = NULL, y = NULL) +
    .calibration_theme() +
    ggplot2::theme(panel.grid.major.x = ggplot2::element_blank())
}
