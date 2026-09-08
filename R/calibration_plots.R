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
#' `mean_severity` is the value Dynamic Fire writes to its event log: the mean
#' severity over all of the event's sites, which is why it can fall below 1.
#' Note that it and `sites` (the log's `DamagedSites`) are not on the same
#' denominator.
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
#' @param stats A `run_calibration_validation()` summary carrying
#'   `$observed$primary$area_by_fuel_ha` and `$observed$fuel_code_to_base`.
#'
#' @returns A ggplot object.
#'
#' @family calibration plots
#' @export
plot_calibration_area_by_fuel <- function(stats) {
  .need("ggplot2", "plot_calibration_area_by_fuel()")
  .check_calibration_stats(stats, need = "area_by_fuel_ha")
  ftb <- stats$observed$fuel_code_to_base
  if (is.null(ftb)) {
    stop("`stats$observed$fuel_code_to_base` is required.", call. = FALSE)
  }
  obs <- stats$observed$primary$area_by_fuel_ha
  ca <- do.call(rbind, lapply(stats$reps, function(r) r$area_by_fuel_ha))
  if (is.null(ca)) {
    stop(
      "no per-replicate `area_by_fuel_ha`; the replicates predate cell-based ",
      "fuel attribution.",
      call. = FALSE
    )
  }
  ca <- as.data.frame(ca)
  ca$base <- unname(ftb[as.character(ca$fuel_code)])
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
