#' Consistent linetype scale for reference curves
#'
#' @return A ggplot2 scale.
#' @family growth calibration helpers
#' @export
scale_linetype_growth_reference <- function() {
  ggplot2::scale_linetype_manual(
    values = .growth_reference_linetypes,
    limits = names(.growth_reference_linetypes),
    drop = FALSE,
    na.translate = FALSE
  )
}

#' Plot a species' growth curve against its references
#'
#' Reproduces the layout of the source parameterization figures: the fitted
#' LANDIS-II trajectory in black, the ICH-SORTIE reference in dark grey, the
#' TIPSY yield curve in purple, and ground-plot observations coloured by BEC
#' subzone and shaped by sample-establishment type.
#'
#' Deciduous plots are shown by their RAW leading species, because cottonwood
#' and birch are modelled as trembling aspen but observed separately.
#'
#' The fitting window is shaded, so a reviewer can see at a glance which part of
#' the curve the fit statistic is actually responding to.
#'
#' @param species Character. Modelled species code.
#' @param curve A tibble with `age` and `aboveground_c_mg_ha`.
#' @param reference A data frame of reference observations, with columns
#'   `source` (`"SORTIE"`, `"TIPSY"` or `"Ground plots"`), `age` and
#'   `aboveground_c_mg_ha`.
#' @param x_max Numeric. Upper age limit for the panel.
#' @param mature_window Numeric length-2. Fitting window to shade;
#'   `NULL` to omit.
#'
#' @return A `ggplot`.
#' @family growth calibration helpers
#' @export
plot_growth_calibration <- function(
  species,
  curve,
  reference,
  x_max = 400,
  mature_window = c(100L, Inf)
) {
  obs <- dplyr::filter(reference, .data$source == "Ground plots")
  model <- dplyr::filter(reference, .data$source %in% c("SORTIE", "TIPSY"))

  shade <- NULL
  if (!is.null(mature_window)) {
    shade <- ggplot2::annotate(
      "rect",
      xmin = mature_window[[1L]],
      xmax = min(x_max, mature_window[[2L]]),
      ymin = -Inf,
      ymax = Inf,
      fill = "goldenrod2",
      alpha = 0.13
    )
  }

  ggplot2::ggplot() +
    shade +
    ggplot2::geom_point(
      data = obs,
      ggplot2::aes(
        x = .data$age,
        y = .data$aboveground_c_mg_ha,
        colour = .data$bec_label,
        shape = .data$leading_raw
      ),
      alpha = 0.7,
      size = 1.6
    ) +
    ggplot2::geom_line(
      data = model,
      ggplot2::aes(x = .data$age, y = .data$aboveground_c_mg_ha, linetype = .data$source),
      colour = "grey35",
      linewidth = 0.6,
      ## Round caps turn the short "on" segment into a round dot. Without them a
      ## dot pattern renders as a short dash, because R scales dash lengths by
      ## line width -- which is exactly what makes it unreadable in a legend key.
      lineend = "round"
    ) +
    ggplot2::geom_line(
      data = curve,
      ggplot2::aes(x = .data$age, y = .data$aboveground_c_mg_ha),
      colour = "black",
      linewidth = 1
    ) +
    ggplot2::scale_shape_manual(values = c(16, 17, 15, 3, 7, 8, 4, 10, 12)) +
    scale_linetype_growth_reference() +
    ggplot2::coord_cartesian(xlim = c(0, x_max)) +
    ggplot2::labs(
      x = "Stand age (years)",
      y = expression("Aboveground live carbon" ~ (Mg ~ C ~ ha^-1)),
      colour = "BEC subzone",
      shape = "Leading species",
      linetype = "Reference",
      title = species,
      subtitle = if (is.null(mature_window)) {
        NULL
      } else {
        sprintf("shaded: fitting window (%g to %g yr)", mature_window[[1L]], mature_window[[2L]])
      }
    ) +
    ggplot2::theme_bw(base_size = 10) +
    ggplot2::theme(legend.key.size = ggplot2::unit(0.4, "cm"))
}

#' Plot the current parameter set against a candidate, for review
#'
#' Both trajectories are drawn over the same references, with the fitting window
#' shaded. This is the figure to look at when deciding whether a sweep result is
#' worth promoting: it shows what actually changes, over the part of the curve
#' the objective responds to.
#'
#' The age-binned plot series is drawn as well, in orange. That series -- not the
#' scatter behind it -- is what the ground-plot term of the score is computed
#' against, so a candidate that looks wrong against the cloud but right against
#' the binned points is behaving exactly as scored.
#'
#' @param species Character. Modelled species code.
#' @param current_curve,candidate_curve Tibbles with `age` and
#'   `aboveground_c_mg_ha`. `candidate_curve` may be `NULL`.
#' @param reference A data frame of reference observations, with columns
#'   `source` (`"SORTIE"`, `"TIPSY"` or `"Ground plots"`), `age` and
#'   `aboveground_c_mg_ha`.
#' @param binned Optional tibble from [growth_bin_observations()].
#' @param current_label,candidate_label Character legend labels.
#' @param x_max Numeric. Upper age limit.
#' @param mature_window Numeric length-2. Fitting window to shade.
#' @param subtitle Character. Overrides the default subtitle.
#'
#' @return A `ggplot`.
#' @family growth calibration helpers
#' @export
plot_growth_candidate <- function(
  species,
  current_curve,
  candidate_curve,
  reference,
  binned = NULL,
  current_label = "current parameters",
  candidate_label = "best candidate",
  x_max = 400,
  mature_window = c(100L, Inf),
  subtitle = NULL
) {
  obs <- dplyr::filter(reference, .data$source == "Ground plots")
  model <- dplyr::filter(reference, .data$source %in% c("SORTIE", "TIPSY"))

  curves <- dplyr::bind_rows(
    dplyr::mutate(current_curve, series = current_label),
    if (is.null(candidate_curve)) NULL else dplyr::mutate(candidate_curve, series = candidate_label)
  )

  ## Every drawn series is MAPPED, never given a bare colour, so each one earns
  ## a legend key. The binned series in particular is what the score is actually
  ## computed against, and an unlabelled orange line invites the reader to
  ## guess.
  plots_label <- "ground plots"
  binned_label <- "ground plots, age-binned (scored)"
  pal <- stats::setNames(
    c("black", "firebrick", "grey60", "darkorange3"),
    c(current_label, candidate_label, plots_label, binned_label)
  )

  p <- ggplot2::ggplot() +
    ggplot2::annotate(
      "rect",
      xmin = mature_window[[1L]],
      xmax = min(x_max, mature_window[[2L]]),
      ymin = -Inf,
      ymax = Inf,
      fill = "goldenrod2",
      alpha = 0.13
    ) +
    ggplot2::geom_point(
      data = obs,
      ggplot2::aes(x = .data$age, y = .data$aboveground_c_mg_ha, colour = plots_label),
      alpha = 0.6,
      size = 1.3
    ) +
    ggplot2::geom_line(
      data = model,
      ggplot2::aes(x = .data$age, y = .data$aboveground_c_mg_ha, linetype = .data$source),
      colour = "grey35",
      linewidth = 0.5,
      lineend = "round"
    )

  if (!is.null(binned) && nrow(binned) > 0L) {
    p <- p +
      ggplot2::geom_line(
        data = binned,
        ggplot2::aes(x = .data$age, y = .data$value, colour = binned_label),
        linewidth = 0.6,
        alpha = 0.9
      ) +
      ggplot2::geom_point(
        data = binned,
        ggplot2::aes(x = .data$age, y = .data$value, colour = binned_label),
        size = 2.1,
        shape = 18
      )
  }

  p +
    ggplot2::geom_line(
      data = curves,
      ggplot2::aes(x = .data$age, y = .data$aboveground_c_mg_ha, colour = .data$series),
      linewidth = 1
    ) +
    ggplot2::scale_colour_manual(values = pal, breaks = names(pal), limits = names(pal)) +
    scale_linetype_growth_reference() +
    ## Without this the observation keys inherit the line geom and read as
    ## curves, which is exactly the confusion the legend is meant to remove.
    ggplot2::guides(
      colour = ggplot2::guide_legend(
        nrow = 2L,
        override.aes = list(
          linetype = c("solid", "solid", "blank", "solid"),
          shape = c(NA, NA, 16, 18),
          linewidth = c(1, 1, 0, 0.6),
          size = c(0, 0, 1.3, 2.1)
        )
      )
    ) +
    ggplot2::coord_cartesian(xlim = c(0, x_max)) +
    ggplot2::labs(
      x = "Stand age (years)",
      y = expression("Aboveground live carbon" ~ (Mg ~ C ~ ha^-1)),
      colour = NULL,
      linetype = "Reference curve",
      title = species,
      subtitle = subtitle %||%
        sprintf("shaded: fitting window (%g to %g yr)", mature_window[[1L]], mature_window[[2L]])
    ) +
    ggplot2::theme_bw(base_size = 10) +
    ggplot2::theme(legend.position = "bottom", legend.box = "vertical")
}

#' Plot factorial sensitivity of fit to each growth parameter
#'
#' For every species and parameter, shows how the shape error varies as that
#' parameter moves below or above its calibrated value, marginalizing over the
#' others. A parameter whose boxes separate cleanly is one the fit is sensitive
#' to; a parameter whose boxes overlap is one the reference data cannot
#' constrain.
#'
#' Only the SWEPT parameters appear. `biomass_max` is pinned across the factorial
#' and recovered arithmetically, so it has no sensitivity to show.
#'
#' Candidates are shown by their position RELATIVE to the calibrated value
#' rather than by their absolute value: the candidates differ per species, so an
#' absolute axis would have to carry every species' values in every panel.
#'
#' @param scores A tibble from the `growth_factorial_scores` target.
#' @param current A tibble of the parameters currently in use, with `species`
#'   and the parameter columns.
#'
#' @return A `ggplot`.
#' @family growth calibration helpers
#' @export
plot_growth_factorial_sensitivity <- function(scores, current) {
  params <- c("growth_shp", "mort_shp", "anpp_prop")
  labels <- c(
    growth_shp = "Growth shape",
    mort_shp = "Mortality shape",
    anpp_prop = "Max. ANPP (% of max. biomass)"
  )

  cur_long <- current |>
    dplyr::mutate(anpp_prop = 100 * .data$anpp_max / .data$biomass_max) |>
    dplyr::select(dplyr::all_of(c("species", params))) |>
    tidyr::pivot_longer(dplyr::all_of(params), names_to = "parameter", values_to = "calibrated")

  scores |>
    ## Species with no scorable reference have an all-NA error and would
    ## contribute empty panels plus a "removed rows" warning.
    dplyr::filter(!all(is.na(.data$nrmse_shape)), .by = "species") |>
    dplyr::select(dplyr::all_of(c("species", "nrmse_shape", params))) |>
    tidyr::pivot_longer(dplyr::all_of(params), names_to = "parameter", values_to = "value") |>
    dplyr::left_join(cur_long, by = c("species", "parameter")) |>
    dplyr::mutate(
      position = dplyr::case_when(
        .data$value < .data$calibrated - 1e-6 ~ "lower",
        .data$value > .data$calibrated + 1e-6 ~ "higher",
        .default = "calibrated"
      ) |>
        factor(levels = c("lower", "calibrated", "higher")),
      parameter = factor(labels[.data$parameter], levels = unname(labels))
    ) |>
    ggplot2::ggplot(ggplot2::aes(x = .data$position, y = .data$nrmse_shape)) +
    ggplot2::geom_boxplot(
      ggplot2::aes(fill = .data$position),
      outlier.size = 0.5,
      linewidth = 0.3,
      show.legend = FALSE
    ) +
    ggplot2::scale_fill_manual(
      values = c(lower = "grey88", calibrated = "steelblue3", higher = "grey88")
    ) +
    ggplot2::facet_grid(
      rows = ggplot2::vars(.data$species),
      cols = ggplot2::vars(.data$parameter),
      scales = "free_y"
    ) +
    ggplot2::labs(
      x = "Candidate value, relative to the calibrated value",
      y = "Shape error (level-normalised RMSE)"
    ) +
    ggplot2::theme_bw(base_size = 9) +
    ggplot2::theme(
      axis.text.x = ggplot2::element_text(angle = 30, hjust = 1),
      panel.grid.minor = ggplot2::element_blank()
    )
}

## ---- landscape cohort structures -----------------------------------------------------------------
##
## The calibration landscape is most useful when its cells resemble the
## communities the simulation actually contains. Extracting those structures
## needs the VRI chain, which the growth project deliberately cannot reach, so
## the extraction runs once in the MAIN project and is committed as a snapshot.

#' Write a standalone calibration review bundle
#'
#' Per-species review figures plus a one-page summary table, written to a plain
#' directory that needs no Quarto render and no pipeline knowledge to inspect.
#'
#' @param dir Character. Output directory; created if absent.
#' @param species Character vector of species to write.
#' @param curves Simulated curves keyed by species (the verification run).
#' @param candidate_curves Optional simulated curves for the best candidate.
#' @param references Named list of reference tables, one per species.
#' @param reference_curves Named list from [growth_reference_curves()], one per
#'   species, supplying the age-binned ground-plot series.
#' @param best A tibble of best-candidate parameters and fit statistics.
#' @param windows A tibble from [growth_fitting_windows()].
#' @param scoring_file,params_file Character. Paths named in the bundle's
#'   `README.txt`, so a reviewer is pointed at the files this project actually
#'   keeps them in.
#'
#' @return Character vector of the written file paths.
#' @family growth calibration helpers
#' @export
write_growth_review_bundle <- function(
  dir,
  species,
  curves,
  candidate_curves = NULL,
  references,
  reference_curves = NULL,
  best,
  windows,
  scoring_file = "growth_scoring.csv",
  params_file = "the growth-parameter table"
) {
  dir <- fs::dir_create(dir)
  win_for <- function(sp) growth_window_for(windows, sp)
  written <- character(0)

  for (sp in species) {
    cur <- dplyr::filter(curves, .data$species == sp)
    ## NOT `row`: the simulated curves carry a `row` column (the landscape row
    ## index), which shadows a local of that name inside dplyr data masking.
    best_row <- dplyr::filter(best, .data$species == sp)
    scale_to <- if (nrow(best_row)) best_row$level_used[[1L]] else NA_real_

    ## The candidate ran at the PINNED biomass_max, so its plateau carries no
    ## information. Rescale it to the level its shape implies, which is what
    ## promoting it would actually produce.
    cand <- if (is.null(candidate_curves)) {
      NULL
    } else {
      x <- dplyr::filter(candidate_curves, .data$species == sp)
      peak <- if (nrow(x)) max(x$aboveground_c_mg_ha, na.rm = TRUE) else NA_real_
      if (nrow(x) == 0L) {
        NULL
      } else if (!is.na(scale_to) && !is.na(peak) && peak > 0) {
        dplyr::mutate(x, aboveground_c_mg_ha = .data$aboveground_c_mg_ha * scale_to / peak)
      } else {
        x
      }
    }

    win <- win_for(sp)
    sub <- if (nrow(best_row)) {
      if (isFALSE(best_row$fitted[[1L]])) {
        sprintf(
          "window %g-%g yr; NOT FITTED: no reference series at all (%d plots)",
          win[[1L]],
          win[[2L]],
          best_row$n_plots[[1L]]
        )
      } else {
        used <- best_row$level_source_used[[1L]]
        lvl <- if (is.na(used)) {
          sprintf("no level (%s requested, unavailable)", best_row$level_source_requested[[1L]])
        } else {
          sprintf("level from %s", used)
        }
        sprintf(
          "window %g-%g yr (%s); %s; %d plots in %d bins%s",
          win[[1L]],
          win[[2L]],
          best_row$window_source[[1L]],
          lvl,
          best_row$n_plots[[1L]],
          best_row$n_bins[[1L]],
          if (isTRUE(best_row$plots_sparse[[1L]])) " -- SPARSE, weigh the fit accordingly" else ""
        )
      }
    } else {
      NULL
    }

    p <- plot_growth_candidate(
      species = sp,
      current_curve = cur,
      candidate_curve = cand,
      reference = references[[sp]],
      binned = if (is.null(reference_curves)) NULL else reference_curves[[sp]]$binned,
      mature_window = win,
      subtitle = sub
    )
    f <- file.path(dir, paste0("review-", sp, ".png"))
    ggplot2::ggsave(f, p, width = 7, height = 5, dpi = 200)
    written <- c(written, f)
  }

  summ <- file.path(dir, "review-summary.csv")
  utils::write.csv(best, summ, row.names = FALSE)

  readme <- file.path(dir, "README.txt")
  writeLines(
    c(
      "ForCS growth calibration -- review bundle",
      paste0("written ", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
      "",
      "review-<species>.png   current parameters (black) vs best candidate (red),",
      "                       over the SORTIE / TIPSY / ground-plot references.",
      "                       Grey points are individual plots; the ORANGE series",
      "                       is those plots binned by age, and it -- not the",
      "                       scatter -- is what the score is computed against,",
      "                       so every age band counts once however many plots",
      "                       landed in it.",
      "                       The shaded band is the fitting window. It is DERIVED:",
      "                       it opens at age 20, below which the plot programmes",
      "                       do not sample, and closes at the earliest of the 95th",
      "                       percentile of plot ages, the end of the reference",
      "                       curve, and 0.45 x longevity, before LANDIS-II's",
      "                       senescence collapse.",
      "                       The candidate is drawn at the level its shape implies,",
      "                       not at the level it was simulated with.",
      "review-summary.csv     best candidate per species beside the values in use.",
      "                       biomass_max_est / anpp_max_est are the recommendation;",
      "                       current_* are the values in use.",
      "",
      "To change what is fitted, or over what ages, edit",
      paste0("  ", scoring_file, " -- no R changes needed. Leave age_min and"),
      "age_max blank to keep the derived window.",
      "",
      "Nothing here is applied automatically. To promote a value, edit",
      paste0("  ", params_file, " by hand.")
    ),
    readme
  )

  c(written, summ, readme)
}
