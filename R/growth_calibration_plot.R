#' Consistent linetype scale for reference curves
#'
#' @return A ggplot2 scale.
#' @family growth calibration helpers
#' @export
scale_linetype_growth_reference <- function() {
  .need("ggplot2", "Building a growth-reference scale")
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
#'   `source` (`"SORTIE"`, `"TIPSY"`, `"VDYP"` or `"Ground plots"`), `age` and
#'   `aboveground_c_mg_ha`.
#' @param x_max Numeric or `NULL`. Upper age limit for the panel. `NULL` (the
#'   default) extends to the last age present in the data, so a longer run is
#'   never silently clipped.
#' @param density Logical. Draw the ground-plot cloud as a WEIGHTED hexagonal
#'   density instead of one point per plot, with individual points kept only for
#'   the best-matched plots. Off by default, because it is worth it only where
#'   the cloud is dense enough to be unreadable as points: a species with a
#'   hundred plots gets a sparse, blocky panel that says less than the points
#'   did. Requires the 'hexbin' package.
#' @param density_bins Integer. Number of bins across the x range of the
#'   hexagonal grid when `density` is `TRUE`.
#' @param density_points_max Integer. When `density` is `TRUE`, how many of the
#'   best-matched plots stay drawn individually over the density. A COUNT rather
#'   than a fraction of the maximum weight, because a fraction does not control
#'   the number drawn: the weight distribution differs by species, and at 60% of
#'   maximum one species keeps 989 plots where another keeps 65. Requires a
#'   `plot_weight` column; without one every plot counts equally.
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
  x_max = NULL,
  mature_window = c(100L, Inf),
  density = FALSE,
  density_bins = 34L,
  density_points_max = 150L
) {
  .need("ggplot2", "Plotting a growth calibration")
  obs <- dplyr::filter(reference, .data$source == "Ground plots")
  model <- dplyr::filter(reference, .data$source %in% c("SORTIE", "TIPSY", "VDYP"))
  x_max <- .growth_x_max(x_max, curve$age, reference$age)

  shade <- NULL
  if (!is.null(mature_window)) {
    shade <- ggplot2::annotate(
      "rect",
      xmin = mature_window[[1L]],
      xmax = min(x_max, mature_window[[2L]]),
      ymin = -Inf,
      ymax = Inf,
      fill = growth_plot_palette()[["window"]],
      alpha = 0.13
    )
  }

  ## Either one point per plot, or a weighted density with the best-matched
  ## plots still drawn over it. The density carries EVERY plot, weighted, so
  ## nothing is dropped from view; what changes is that a thousand plots stop
  ## competing for the same ink and for a legend key each.
  cloud <- if (isTRUE(density)) {
    .need("hexbin", "Drawing a weighted ground-plot density")
    has_w <- "plot_weight" %in% names(obs)
    rel <- if (has_w) {
      w <- obs$plot_weight
      w[is.na(w)] <- 0
      if (max(w) > 0) w / max(w) else w
    } else {
      rep(1, nrow(obs))
    }
    hex_aes <- if (has_w) {
      ggplot2::aes(x = .data$age, y = .data$aboveground_c_mg_ha, weight = .data$plot_weight)
    } else {
      ggplot2::aes(x = .data$age, y = .data$aboveground_c_mg_ha)
    }
    list(
      ggplot2::stat_binhex(data = obs, mapping = hex_aes, bins = density_bins),
      ggplot2::scale_fill_gradient(
        low = "grey88",
        high = "grey25",
        name = "Weighted\nplot density"
      ),
      ggplot2::geom_point(
        data = obs[
          rank(-rel, ties.method = "first") <= min(density_points_max, length(rel)),
          ,
          drop = FALSE
        ],
        ggplot2::aes(x = .data$age, y = .data$aboveground_c_mg_ha),
        colour = growth_plot_palette()[["summary"]],
        alpha = 0.65,
        size = 0.9
      )
    )
  } else {
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
    )
  }

  ggplot2::ggplot() +
    shade +
    cloud +
    ggplot2::geom_line(
      data = model,
      ggplot2::aes(x = .data$age, y = .data$aboveground_c_mg_ha, linetype = .data$source),
      colour = growth_plot_palette()[["reference"]],
      linewidth = 0.6,
      ## Round caps turn the short "on" segment into a round dot. Without them a
      ## dot pattern renders as a short dash, because R scales dash lengths by
      ## line width -- which is exactly what makes it unreadable in a legend key.
      lineend = "round"
    ) +
    ggplot2::geom_line(
      data = curve,
      ggplot2::aes(x = .data$age, y = .data$aboveground_c_mg_ha),
      colour = growth_plot_palette()[["current"]],
      linewidth = 1
    ) +
    ## Both are dropped in density mode: nothing maps shape or colour there, and
    ## a manual scale with no layer behind it warns about levels it cannot find
    ## while contributing a legend title for a key that never appears.
    (if (isTRUE(density)) {
      NULL
    } else {
      ggplot2::scale_shape_manual(values = c(16, 17, 15, 3, 7, 8, 4, 10, 12))
    }) +
    scale_linetype_growth_reference() +
    ggplot2::coord_cartesian(xlim = c(0, x_max)) +
    ggplot2::labs(
      x = "Stand age (years)",
      y = expression("Aboveground live carbon" ~ (Mg ~ C ~ ha^-1)),
      colour = if (isTRUE(density)) NULL else "BEC subzone",
      shape = if (isTRUE(density)) NULL else "Leading species",
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
#' The age-binned plot series is drawn as well, in blue. That series -- not the
#' scatter behind it -- is what the ground-plot term of the score is computed
#' against, so a candidate that looks wrong against the cloud but right against
#' the binned points is behaving exactly as scored.
#'
#' Each binned point is sized by the number of plots behind it, because they
#' routinely differ by more than an order of magnitude and an equal-sized point
#' hides that completely. A bin holding a single plot is not a median of
#' anything, and the sharp reversals in the series are usually those bins.
#'
#' The points are NOT joined by line segments. Connecting them asserts a
#' trajectory across ages where nothing was measured, and most of the movement
#' that line described came from the one-plot bins.
#'
#' Passing `smooth` overlays a fit through the whole cloud with a confidence
#' band, for comparison only -- see [growth_smooth_observations()]. It is not
#' scored, and the legend says so. It shares the binned points' colour because
#' it summarizes the same observations; glyph, not hue, is what tells them apart.
#'
#' @param species Character. Modelled species code.
#' @param current_curve,candidate_curve Tibbles with `age` and
#'   `aboveground_c_mg_ha`. `candidate_curve` may be `NULL`.
#' @param reference A data frame of reference observations, with columns
#'   `source` (`"SORTIE"`, `"TIPSY"`, `"VDYP"` or `"Ground plots"`), `age` and
#'   `aboveground_c_mg_ha`.
#' @param binned Optional tibble from [growth_bin_observations()]. An `n` column,
#'   when present, sizes the points.
#' @param smooth Optional tibble from [growth_smooth_observations()], drawn as a
#'   fitted line and confidence band. Display only; nothing is scored against it.
#' @param current_label,candidate_label Character legend labels.
#' @param x_max Numeric or `NULL`. Upper age limit. `NULL` (the default)
#'   extends to the last age present in the data, so a longer run is never
#'   silently clipped.
#' @param mature_window Numeric length-2. Fitting window to shade.
#' @param density Logical. Draw the ground-plot cloud as a WEIGHTED hexagonal
#'   density instead of one point per plot, keeping the best-matched plots drawn
#'   over it. Weighted via the `plot_weight` column if present, so the shading
#'   reads as evidence rather than as sampling effort; without that column it
#'   falls back to counts.
#' @param density_bins Integer. Bins across the x range of the hex grid.
#' @param density_points_max Integer. How many of the best-matched plots stay
#'   drawn individually over the density. A COUNT rather than a fraction of the
#'   maximum weight, because a fraction does not control the number drawn: the
#'   weight distribution differs by species, and at 60% of maximum one species
#'   keeps 989 plots where another keeps 65.
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
  smooth = NULL,
  current_label = "current parameters",
  candidate_label = "best candidate",
  x_max = NULL,
  mature_window = c(100L, Inf),
  subtitle = NULL,
  density = FALSE,
  density_bins = 34L,
  density_points_max = 150L
) {
  .need("ggplot2", "Plotting a growth candidate")
  obs <- dplyr::filter(reference, .data$source == "Ground plots")
  model <- dplyr::filter(reference, .data$source %in% c("SORTIE", "TIPSY", "VDYP"))
  x_max <- .growth_x_max(x_max, current_curve$age, candidate_curve$age, reference$age)

  curves <- dplyr::bind_rows(
    dplyr::mutate(current_curve, series = current_label),
    if (is.null(candidate_curve)) NULL else dplyr::mutate(candidate_curve, series = candidate_label)
  )

  ## Every drawn series is MAPPED, never given a bare colour, so each one earns
  ## a legend key. The binned series in particular is what the score is actually
  ## computed against, and an unlabelled series invites the reader to guess.
  plots_label <- "ground plots"
  binned_label <- "ground plots, age-binned (scored)"
  smooth_label <- "ground plots, GAM fit (not scored)"
  has_smooth <- !is.null(smooth) && nrow(smooth) > 0L
  ## The binned points and the fit summarize the SAME observations, so they
  ## share a colour and are told apart by glyph. Giving them different colours
  ## implied two independent series.
  pal <- stats::setNames(
    unname(growth_plot_palette()[c("current", "candidate", "plots", "summary")]),
    c(current_label, candidate_label, plots_label, binned_label)
  )
  if (has_smooth) {
    pal <- c(pal, stats::setNames(growth_plot_palette()[["summary"]], smooth_label))
  }

  ## Behind everything, so the band reads as context rather than as a series.
  ribbon <- if (has_smooth) {
    ggplot2::geom_ribbon(
      data = smooth,
      ggplot2::aes(x = .data$age, ymin = .data$lo, ymax = .data$hi),
      fill = growth_plot_palette()[["summary"]],
      alpha = 0.16
    )
  }

  ## EITHER one point per plot, OR a weighted hex density with the best-matched plots over it.
  ## Which is right depends on how many plots there are: the panels stop being readable well before
  ## the data run out, and a species carrying thousands of plots is solid ink with a shape key per
  ## leading species. The density carries EVERY plot at its weight, so nothing leaves the figure --
  ## what changes is that a thousand plots stop competing for the same ink.
  ##
  ## Not automatic: a species with a hundred plots gets a sparse, blocky grid that says less than the
  ## points did, so the switch is the caller's.
  cloud <- if (isTRUE(density)) {
    .need("hexbin", "Drawing a weighted ground-plot density")
    has_w <- "plot_weight" %in% names(obs)
    w <- if (has_w) obs$plot_weight else rep(1, nrow(obs))
    w[is.na(w)] <- 0
    ## A COUNT cap, not a fraction of the maximum weight. A fraction does not control how many points
    ## are drawn, because the weight distribution differs by species: at 60% of maximum one species
    ## keeps 989 of its plots and another keeps 65, and the first buries the density it annotates.
    keep <- rank(-w, ties.method = "first") <= min(density_points_max, length(w))
    hex_aes <- if (has_w) {
      ggplot2::aes(x = .data$age, y = .data$aboveground_c_mg_ha, weight = .data$plot_weight)
    } else {
      ggplot2::aes(x = .data$age, y = .data$aboveground_c_mg_ha)
    }
    list(
      ggplot2::stat_binhex(data = obs, mapping = hex_aes, bins = density_bins),
      ## Ramps to the SUMMARY colour rather than through greys: the density and the binned points are
      ## the same observations summarized two ways, which is the argument that already governs the
      ## binned series and the smooth.
      ggplot2::scale_fill_gradient(
        low = "grey93",
        high = growth_plot_palette()[["summary"]],
        name = "Weighted\nplot density"
      ),
      ggplot2::geom_point(
        data = obs[keep, , drop = FALSE],
        ggplot2::aes(x = .data$age, y = .data$aboveground_c_mg_ha),
        colour = growth_plot_palette()[["key_outline"]],
        fill = growth_plot_palette()[["faint"]],
        shape = 21,
        stroke = 0.3,
        size = 1.1,
        alpha = 0.9
      )
    )
  } else {
    list(
      ggplot2::geom_point(
        data = obs,
        ggplot2::aes(
          x = .data$age,
          y = .data$aboveground_c_mg_ha,
          colour = plots_label,
          shape = .data$leading_raw
        ),
        alpha = 0.6,
        size = 1.5
      ),
      ggplot2::scale_shape_manual(values = c(16, 17, 15, 3, 7, 8, 4, 10, 12))
    )
  }

  p <- ggplot2::ggplot() +
    ggplot2::annotate(
      "rect",
      xmin = mature_window[[1L]],
      xmax = min(x_max, mature_window[[2L]]),
      ymin = -Inf,
      ymax = Inf,
      fill = growth_plot_palette()[["window"]],
      alpha = 0.13
    ) +
    ribbon +
    ## Shape carries the RAW species code, not the modelled one. Several codes
    ## commonly lump into one modelled species, and the members are not
    ## interchangeable -- in the network this was built against, black cottonwood
    ## carries a median 180 Mg C/ha against trembling aspen's 53. Without this a
    ## reviewer cannot see which member is setting the curve.
    cloud +
    ggplot2::geom_line(
      data = model,
      ggplot2::aes(x = .data$age, y = .data$aboveground_c_mg_ha, linetype = .data$source),
      colour = growth_plot_palette()[["reference"]],
      linewidth = 0.5,
      lineend = "round"
    )

  if (!is.null(binned) && nrow(binned) > 0L) {
    ## Deliberately NOT joined by line segments. A straight line between two bin
    ## medians asserts a trajectory through ages where nothing was measured, and
    ## most of the apparent movement it drew was bins holding a single plot.
    ## Area proportional to the plot count, with a visible floor: a one-plot bin
    ## must still be findable on the page, since knowing where those bins ARE is
    ## the point. Integer breaks because a fractional plot is not a thing.
    ## The SMOOTH goes down first and the scored points on top of it, never the
    ## other way round: the fit is an aid, the points are what the ground-plot
    ## term is computed against, and a line drawn over them hides the very values
    ## it summarizes. A white outline separates a point from the line where the
    ## two coincide, which is exactly where the reader is checking agreement.
    if (has_smooth) {
      p <- p +
        ggplot2::geom_line(
          data = smooth,
          ggplot2::aes(x = .data$age, y = .data$value, colour = smooth_label),
          linewidth = 0.7
        )
    }
    p <- if ("n" %in% names(binned)) {
      p +
        ggplot2::geom_point(
          data = binned,
          ggplot2::aes(x = .data$age, y = .data$value, fill = binned_label, size = .data$n),
          shape = 23,
          colour = growth_plot_palette()[["key_outline"]],
          stroke = 0.4
        ) +
        ggplot2::scale_size(
          range = c(1.4, 5),
          transform = "sqrt",
          breaks = .growth_bin_size_breaks(binned$n)
        )
    } else {
      p +
        ggplot2::geom_point(
          data = binned,
          ggplot2::aes(x = .data$age, y = .data$value, fill = binned_label),
          size = 2.4,
          shape = 23,
          colour = growth_plot_palette()[["key_outline"]],
          stroke = 0.4
        )
    }
    ## Shape 23 takes its interior from `fill`, so this series is mapped on fill
    ## while every other series is mapped on colour. The fill scale is given the
    ## SAME name, breaks and limits as the colour scale so ggplot2 merges the two
    ## into one legend. Silencing it instead leaves the key label with no glyph:
    ## `override.aes` can only restyle a key some layer contributes, and once
    ## nothing maps colour to this series there is no key to restyle.
    p <- p +
      ggplot2::scale_fill_manual(
        values = pal,
        breaks = names(pal),
        limits = names(pal),
        name = NULL
      )
  } else if (has_smooth) {
    p <- p +
      ggplot2::geom_line(
        data = smooth,
        ggplot2::aes(x = .data$age, y = .data$value, colour = smooth_label),
        linewidth = 0.7
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
    ## Explicit order: ggplot2 otherwise sequences the guides by how the scales
    ## happen to be built, which differs between species and leaves a six-panel
    ## set with its legends in six different arrangements.
    ggplot2::guides(
      ## The binned points map `fill` (shape 23 takes its interior from fill, and
      ## its outline is the fixed white), so NOTHING maps colour to that series
      ## and its key would otherwise be drawn empty -- the label appears, the
      ## glyph does not. Both guides therefore state the marker explicitly.
      size = ggplot2::guide_legend(
        order = 2L,
        override.aes = list(
          shape = 23,
          fill = growth_plot_palette()[["summary"]],
          colour = growth_plot_palette()[["key_outline"]],
          stroke = 0.4
        )
      ),
      linetype = ggplot2::guide_legend(order = 3L),
      shape = ggplot2::guide_legend(order = 4L),
      ## Both scales need the same order and layout or ggplot2 declines to merge
      ## them and draws two legends for one set of series. `override.aes` goes on
      ## the colour guide ONLY: supplying it twice merges fine but warns
      ## "Duplicated `override.aes` is ignored".
      fill = ggplot2::guide_legend(order = 1L, nrow = 2L),
      colour = ggplot2::guide_legend(
        order = 1L,
        nrow = 2L,
        override.aes = .growth_series_key(has_smooth)
      )
    ) +
    ggplot2::coord_cartesian(xlim = c(0, x_max)) +
    ggplot2::labs(
      x = "Stand age (years)",
      y = expression("Aboveground live carbon" ~ (Mg ~ C ~ ha^-1)),
      colour = NULL,
      size = "plots in bin",
      shape = "Leading species",
      linetype = "Reference curve",
      title = species,
      subtitle = subtitle %||%
        sprintf("shaded: fitting window (%g to %g yr)", mature_window[[1L]], mature_window[[2L]])
    ) +
    ggplot2::theme_bw(base_size = 10) +
    ## Four stacked guides otherwise take half the panel. Tight spacing and small
    ## keys keep the data area dominant; the size guide in particular needs no
    ## more room than its largest key.
    ggplot2::theme(
      legend.position = "bottom",
      legend.box = "vertical",
      legend.box.spacing = ggplot2::unit(0.2, "lines"),
      legend.spacing.y = ggplot2::unit(0.1, "lines"),
      legend.margin = ggplot2::margin(0, 0, 0, 0),
      legend.key.height = ggplot2::unit(0.8, "lines"),
      legend.text = ggplot2::element_text(size = ggplot2::rel(0.8)),
      legend.title = ggplot2::element_text(size = ggplot2::rel(0.8))
    )
}

#' Plot factorial sensitivity of fit to each growth parameter
#'
#' For every species and parameter, shows how the shape error varies as that
#' parameter moves below or above its calibrated value, marginalizing over the
#' others. A parameter whose boxes separate cleanly is one the fit is sensitive
#' to; a parameter whose boxes overlap is one the reference data cannot
#' constrain.
#'
#' Only the SWEPT parameters appear, and which those are is read from `scores`
#' rather than assumed. `biomass_max` is pinned across the factorial and
#' recovered arithmetically, so it has no sensitivity to show; a shape parameter
#' the design fixes to one value per species has none either, and drawing it
#' would be actively misleading. Such a parameter has no "calibrated" box at all
#' -- every cell sits on one side of the in-use value -- so the panel would show
#' a lone box under "lower" or "higher" and invite the reader to interpret the
#' side as a result, when it only restates which value was assigned.
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
  .need("ggplot2", "Plotting factorial sensitivity")
  labels <- c(
    growth_shp = "Growth shape",
    mort_shp = "Mortality shape",
    anpp_prop = "Max. ANPP (% of max. biomass)"
  )
  ## Swept means "varies WITHIN a species", not "takes more than one value in the table". The design
  ## assigns a fixed parameter per species, so a column holding 10 for one species and 25 for another
  ## is globally varied and locally constant -- and it is the local structure the figure plots, since
  ## every box is a comparison against that species' own calibrated value.
  swept <- vapply(
    names(labels),
    function(p) {
      max(tapply(scores[[p]], scores$species, \(x) length(unique(stats::na.omit(x))))) > 1L
    },
    logical(1)
  )
  if (!any(swept)) {
    stop("no swept parameter in `scores`: every candidate column holds a single value.")
  }
  params <- names(labels)[swept]
  labels <- labels[swept]

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
      ## RELATIVE tolerance. An absolute one fails here: the swept candidates are
      ## reconstructed from a rounded ratio, so a candidate meant to BE the
      ## calibrated value can differ from it by ~1 part in 3000 -- enough to be
      ## binned as "lower" or "higher", which silently removes the reference box
      ## the whole figure is read against. Grid spacing is ~25%, so 0.5% is
      ## comfortably inside the gap and comfortably outside the noise.
      position = dplyr::case_when(
        .data$value < .data$calibrated * (1 - 5e-3) ~ "lower",
        .data$value > .data$calibrated * (1 + 5e-3) ~ "higher",
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
#' @param smooth_plots Logical. Overlay a spline through the ground-plot cloud
#'   on each panel, for comparison against the binned series. Display only;
#'   nothing is scored against it. See [growth_smooth_observations()].
#' @param x_max Numeric or `NULL`. Upper age limit for every panel. `NULL` lets
#'   each panel extend to the last age in its own data.
#' @param smooth_bin,smooth_site Passed to [growth_smooth_observations()].
#'   `smooth_bin` may be a single width or a vector named by species. Set
#'   `smooth_site` to the location column wherever the plots are a permanent
#'   network, so the fit and the binned series rest on the same evidence.
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
  params_file = "the growth-parameter table",
  smooth_plots = TRUE,
  smooth_bin = 20L,
  smooth_site = NULL,
  x_max = NULL,
  density_min_plots = 500L,
  density_bins = 34L,
  density_points_max = 150L
) {
  .need("ggplot2", "Writing a growth review bundle")
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

    ## Display only. Failing to fit is not an error here -- several species have
    ## too few plots to smooth, which is itself worth seeing on the panel.
    smooth <- if (isTRUE(smooth_plots)) {
      ## Named vector -> per-species bin, so a species whose `age_bin` differs
      ## from the rest is not smoothed at somebody else's width.
      bw <- if (!is.null(names(smooth_bin)) && sp %in% names(smooth_bin)) {
        smooth_bin[[sp]]
      } else {
        smooth_bin[[1L]]
      }
      out <- try(
        growth_smooth_observations(
          dplyr::filter(references[[sp]], .data$source == "Ground plots"),
          bin = bw,
          site = smooth_site
        ),
        silent = TRUE
      )
      if (inherits(out, "try-error")) NULL else out
    }

    p <- plot_growth_candidate(
      species = sp,
      current_curve = cur,
      candidate_curve = cand,
      reference = references[[sp]],
      binned = if (is.null(reference_curves)) NULL else reference_curves[[sp]]$binned,
      smooth = smooth,
      mature_window = win,
      x_max = x_max,
      subtitle = sub,
      ## Per species, not per bundle: the threshold is where the panel stops reading, and a bundle
      ## routinely spans two orders of magnitude in plot count.
      density = nrow(dplyr::filter(references[[sp]], .data$source == "Ground plots")) >=
        density_min_plots,
      density_bins = density_bins,
      density_points_max = density_points_max
    )
    f <- file.path(dir, paste0("review-", sp, ".png"))
    ## Taller than the panel needs, to absorb the legend box rather than let it
    ## squeeze the data area.
    ggplot2::ggsave(f, p, width = 7, height = 5.8, dpi = 200)
    written <- c(written, f)
  }

  summ <- file.path(dir, "review-summary.csv")
  utils::write.csv(best, summ, row.names = FALSE)

  readme <- file.path(dir, "README.txt")
  writeLines(
    c(
      "Biomass Succession growth calibration -- review bundle",
      paste0("written ", format(Sys.time(), "%Y-%m-%d %H:%M:%S")),
      "",
      "review-<species>.png   current parameters (black) vs best candidate (red),",
      "                       over the SORTIE / TIPSY / ground-plot references.",
      "                       Grey points are individual plots; the BLUE DIAMONDS",
      "                       are those plots binned by age, and they -- not the",
      "                       scatter -- are what the score is computed against,",
      "                       so every age band counts once however many plots",
      "                       landed in it.",
      "                       Each diamond is SIZED by how many plots are behind",
      "                       it. The counts differ by more than an order of",
      "                       magnitude, and the sharp reversals in the series",
      "                       are usually its smallest points: a bin holding one",
      "                       plot is not a median of anything. They are NOT",
      "                       joined up, because a line between two bins asserts",
      "                       a trajectory through ages nobody measured.",
      "                       The BLUE LINE and band are a spline through the",
      "                       whole cloud, drawn for comparison ONLY -- nothing",
      "                       is scored against it. Same colour as the diamonds",
      "                       because it summarizes the same observations. Where",
      "                       the band is wide, the binned points nearby are not",
      "                       evidence of much.",
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
