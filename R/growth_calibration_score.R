#' Read the per-species scoring controls
#'
#' The hand-editable knobs that decide what the calibration is fitting to, and
#' over what part of the curve. This is the file to change when a fit looks
#' wrong; nothing here requires touching R.
#'
#' @param path Character. Path to `growth_scoring.csv`.
#'
#' @return A tibble, one row per species.
#' @family growth calibration helpers
#' @export
read_growth_scoring <- function(path) {
  utils::read.csv(path, stringsAsFactors = FALSE) |>
    tibble::as_tibble() |>
    dplyr::mutate(
      dplyr::across(dplyr::any_of(c("age_bin", "plots_warn_below")), as.integer),
      dplyr::across(
        dplyr::any_of(c(
          "age_min",
          "age_max",
          "plot_quantile",
          "weight_sortie",
          "weight_tipsy",
          "weight_vdyp",
          "weight_plots"
        )),
        as.numeric
      )
    ) |>
    ## `weight_vdyp` postdates the other three, so a scoring file written before it
    ## must still read rather than error on a missing column
    (\(x) if ("weight_vdyp" %in% names(x)) x else dplyr::mutate(x, weight_vdyp = NA_real_))() |>
    dplyr::select(
      species,
      age_min,
      age_max,
      age_bin,
      plot_quantile,
      plots_warn_below,
      weight_sortie,
      weight_tipsy,
      weight_vdyp,
      weight_plots,
      level_source,
      note
    )
}

#' Look up one species' scoring controls, with defaults
#'
#' @param scoring A tibble from [read_growth_scoring()], or `NULL`.
#' @param species Character. Species code.
#'
#' @return A one-row list of controls.
#' @family growth calibration helpers
#' @export
growth_scoring_for <- function(scoring, species) {
  out <- list(
    age_bin = 20L,
    plot_quantile = 0.5,
    plots_warn_below = 50L,
    weight_sortie = 1,
    ## TIPSY defaults OUT of the ranking: the workbook does not record how its
    ## native volume output was converted, so the series cannot carry a
    ## quantitative score until that is confirmed. A species may still take its
    ## LEVEL from TIPSY, which back-calculation does support.
    weight_tipsy = 0,
    ## VDYP defaults OUT of the ranking for the same reason as TIPSY: a project
    ## that supplies no VDYP curve must not have its scoring changed by the
    ## series existing. Switch it on per species in `growth_scoring.csv`.
    weight_vdyp = 0,
    weight_plots = 1,
    level_source = NA_character_
  )
  if (is.null(scoring)) {
    return(out)
  }
  ctl <- dplyr::filter(scoring, .data$species == !!species)
  if (nrow(ctl) == 0L) {
    return(out)
  }
  keep <- function(x, default) if (length(x) && !is.na(x[[1L]])) x[[1L]] else default
  out$age_bin <- keep(ctl$age_bin, out$age_bin)
  out$plot_quantile <- keep(ctl$plot_quantile, out$plot_quantile)
  out$plots_warn_below <- keep(ctl$plots_warn_below, out$plots_warn_below)
  out$weight_sortie <- keep(ctl$weight_sortie, out$weight_sortie)
  out$weight_tipsy <- keep(ctl$weight_tipsy, out$weight_tipsy)
  out$weight_vdyp <- keep(ctl$weight_vdyp, out$weight_vdyp)
  out$weight_plots <- keep(ctl$weight_plots, out$weight_plots)
  lvl <- keep(ctl$level_source, NA_character_)
  out$level_source <- if (is.character(lvl) && nzchar(trimws(lvl))) trimws(lvl) else NA_character_
  out
}

#' Score each observation by climatic distance from a target
#'
#' Ground plots from a wider area can inform a calibration, but only in
#' proportion to how much their climate resembles the landscape being modelled.
#' This scores each observation individually, which is the point: aggregating
#' plots into map units first and comparing unit means makes the comparison only
#' as reliable as the thinnest unit, and map units carrying a handful of plots
#' get estimates too noisy to rank.
#'
#' Distance is the root-mean-square deviation across variables after
#' standardizing each by its spread, so that a variable measured in millimetres
#' does not swamp one measured in degrees.
#'
#' @param climate A data frame with one row per observation and one column per
#'   climate variable.
#' @param target Named numeric vector giving the target climate.
#' @param vars Character. Variables to compare on; defaults to the names of
#'   `target`.
#' @param scale Optional named numeric vector of per-variable spreads. Defaults
#'   to each variable's standard deviation across `climate`.
#'
#' @return A numeric vector of distances, one per row of `climate`.
#' @family growth calibration helpers
#' @export
growth_climatic_distance <- function(climate, target, vars = names(target), scale = NULL) {
  missing <- setdiff(vars, names(climate))
  if (length(missing)) {
    stop("`climate` is missing variable(s): ", paste(missing, collapse = ", "), call. = FALSE)
  }
  if (!all(vars %in% names(target))) {
    stop(
      "`target` is missing variable(s): ",
      paste(setdiff(vars, names(target)), collapse = ", "),
      call. = FALSE
    )
  }
  m <- as.matrix(climate[, vars, drop = FALSE])
  if (is.null(scale)) {
    scale <- apply(m, 2L, stats::sd, na.rm = TRUE)
  }
  scale <- scale[vars]
  if (any(!is.finite(scale)) || any(scale == 0)) {
    stop("`scale` must be finite and non-zero for every variable.", call. = FALSE)
  }
  z <- sweep(sweep(m, 2L, target[vars], "-"), 2L, scale, "/")
  sqrt(rowMeans(z^2))
}

#' Turn climatic distance into a calibration weight
#'
#' A weight rather than a cut-off, because any threshold is arbitrary and a plot
#' just past it is not meaningfully different from one just inside. `bandwidth`
#' is the distance at which a Gaussian weight falls to about 0.61; it is the one
#' number to tune, and it can be chosen by cross-validation.
#'
#' @param distance Numeric, from [growth_climatic_distance()].
#' @param bandwidth Numeric. Scale over which similarity decays.
#' @param kernel One of `"gaussian"` (smooth, never exactly zero),
#'   `"tricube"` (smooth, zero beyond `bandwidth`) or `"uniform"` (a hard
#'   cut-off at `bandwidth`).
#'
#' @return A numeric vector of weights in `[0, 1]`.
#' @family growth calibration helpers
#' @export
growth_climatic_weight <- function(
  distance,
  bandwidth = 0.5,
  kernel = c("gaussian", "tricube", "uniform")
) {
  kernel <- match.arg(kernel)
  if (
    !is.numeric(bandwidth) || length(bandwidth) != 1L || !is.finite(bandwidth) || bandwidth <= 0
  ) {
    stop("`bandwidth` must be a single positive number.", call. = FALSE)
  }
  u <- distance / bandwidth
  switch(
    kernel,
    gaussian = exp(-0.5 * u^2),
    tricube = ifelse(u < 1, (1 - u^3)^3, 0),
    uniform = as.numeric(u <= 1)
  )
}

## Weighted quantile, "lower" convention: the smallest observed value at which
## the cumulative weight reaches `probs`. Deliberately not interpolated -- with
## uneven weights there is no defensible way to interpolate between two
## observations carrying very different influence.
.weighted_quantile <- function(x, w, probs) {
  ok <- !is.na(x) & !is.na(w) & w > 0
  if (!any(ok)) {
    return(NA_real_)
  }
  x <- x[ok]
  w <- w[ok]
  o <- order(x)
  x <- x[o]
  cw <- cumsum(w[o]) / sum(w)
  x[[which(cw >= probs)[[1L]]]]
}

#' Condense a ground-plot cloud into an age-binned series
#'
#' Bins the observations on age and takes one quantile per bin, so every age
#' band contributes once no matter how many plots landed in it. This is the
#' non-parametric alternative to fitting a growth equation through the cloud: it
#' assumes nothing about curve shape, which matters because the shape being
#' tested is ForCS's own.
#'
#' `probs = 0.5` (the median) tracks the central tendency of realized stands.
#' Raising it moves the series toward the upper envelope, which is arguably
#' where a fully stocked, single-cohort simulation belongs: the plots span every
#' stocking level, site quality and partial-disturbance history, and whole-plot
#' volume is attributed to the leading species, which holds a median of 69% of
#' the stand here.
#'
#' Where observations come from a permanent-plot network, pass `site` so that
#' each location contributes one value per bin. Permanent plots are remeasured
#' on a schedule that reflects program history rather than anything ecological
#' -- in the network this was built against, 78% of locations carry more than one
#' visit and some carry thirteen -- so treating every visit as an independent
#' observation silently weights each bin toward whichever locations happen to
#' have been revisited most. That is pseudo-replication, and it biases the
#' quantile rather than merely tightening it.
#'
#' @param obs A tibble with `age` and `aboveground_c_mg_ha`.
#' @param bin Numeric. Bin width in years.
#' @param probs Numeric. Quantile to take within each bin.
#' @param site Optional column name identifying the sampling location. When
#'   given, repeated visits to one location are averaged within a bin before the
#'   quantile is taken, so `n` counts locations rather than visits. Errors if the
#'   named column is absent, rather than silently skipping the correction.
#' @param weight Optional column name holding a per-observation weight, typically
#'   from [growth_climatic_weight()]. When given, the within-bin quantile is
#'   weighted, so plots resembling the modelled landscape carry more of it. `n`
#'   still counts observations; `weight` reports the weight behind each bin, so a
#'   bin resting on many barely-relevant plots is visible as such.
#'
#' @return A tibble with `age` (bin mean), `value`, `n`, and `weight`.
#' @family growth calibration helpers
#' @export
growth_bin_observations <- function(obs, bin = 20L, probs = 0.5, site = NULL, weight = NULL) {
  check_col <- function(col, arg) {
    if (!is.null(col) && !col %in% names(obs)) {
      stop("`", arg, "` column '", col, "' not found in `obs`.", call. = FALSE)
    }
  }
  check_col(site, "site")
  check_col(weight, "weight")
  d <- dplyr::filter(obs, !is.na(.data$age), !is.na(.data$aboveground_c_mg_ha))
  if (nrow(d) == 0L) {
    return(tibble::tibble(
      age = numeric(0),
      value = numeric(0),
      n = integer(0),
      weight = numeric(0)
    ))
  }
  d <- dplyr::mutate(d, .bin = floor(.data$age / bin))
  ## A weight is a property of the location, so it survives the collapse; an
  ## absent weight is 1, which reduces the weighted quantile to the plain one.
  d$.w <- if (is.null(weight)) 1 else d[[weight]]
  if (!is.null(site)) {
    ## One value per location per bin, so a plot visited five times counts once.
    d <- dplyr::summarise(
      d,
      age = mean(.data$age),
      aboveground_c_mg_ha = mean(.data$aboveground_c_mg_ha),
      .w = mean(.data$.w),
      .by = dplyr::all_of(c(".bin", site))
    )
  }
  d |>
    dplyr::summarise(
      age = mean(.data$age),
      value = if (is.null(weight)) {
        stats::quantile(.data$aboveground_c_mg_ha, probs, names = FALSE)
      } else {
        .weighted_quantile(.data$aboveground_c_mg_ha, .data$.w, probs)
      },
      n = dplyr::n(),
      weight = sum(.data$.w),
      .by = ".bin"
    ) |>
    dplyr::arrange(.data$age) |>
    dplyr::select(age, value, n, weight)
}

#' Smooth a ground-plot cloud for display
#'
#' Fits a thin-plate spline through the observations and returns it on a dense
#' age grid with a pointwise confidence band. This is a VISUAL AID ONLY: nothing
#' in the scoring path consumes it, and [growth_reference_curves()] continues to
#' build the scored ground-plot reference by binning. Keeping the two separate is
#' deliberate -- swapping the scored reference changes every `biomass_max_est`
#' that rests on plots, which is a calibration decision rather than a plotting
#' one.
#'
#' What it is for is judging the binned series. A bin holding one plot is drawn
#' at the same visual weight as a bin holding thirty, and the straight lines
#' between bins imply a trajectory the plots may not support; a fit over the
#' whole cloud shows how much of that movement is real. Where the band is wide,
#' the binned points nearby are not evidence of anything.
#'
#' Observations are collapsed by location and bin first, exactly as
#' [growth_bin_observations()] does, so the fit and the binned series rest on the
#' same evidence and any difference between them is the summarizing method rather
#' than the sample.
#'
#' The fit is on the IDENTITY scale. A log link is the obvious response to
#' right-skewed biomass, but with a handful of plots at the old end it
#' extrapolates violently -- in the network this was built against it lifted one
#' species' curve to 347 Mg C/ha against a binned maximum of 238 -- so the
#' skew is left to the confidence band to express. The band is clamped at zero,
#' since negative aboveground carbon is not a state a stand can be in.
#'
#' No prediction is returned outside the observed age range: a spline given no
#' data has nothing to say, and a curve drawn past the last plot invites the
#' reader to believe otherwise.
#'
#' @param obs A tibble with `age` and `aboveground_c_mg_ha`.
#' @param bin,site As in [growth_bin_observations()]; used only to collapse
#'   repeated visits, not to summarize.
#' @param k Integer. Spline basis dimension. `NULL` derives one from the number
#'   of occupied bins, capped at 5, which keeps the fit from chasing individual
#'   plots.
#' @param n_grid Integer. Number of ages at which to evaluate the fit.
#' @param level Numeric. Confidence level for the band.
#'
#' @return A tibble with `age`, `value`, `lo`, `hi`, and a `k` attribute; zero
#'   rows when there are too few distinct observations to fit.
#' @family growth calibration helpers
#' @export
growth_smooth_observations <- function(
  obs,
  bin = 20L,
  site = NULL,
  k = NULL,
  n_grid = 200L,
  level = 0.95
) {
  .need("mgcv", "Smoothing a ground-plot cloud")
  if (!is.null(site) && !site %in% names(obs)) {
    stop("`site` column '", site, "' not found in `obs`.", call. = FALSE)
  }
  empty <- tibble::tibble(age = numeric(0), value = numeric(0), lo = numeric(0), hi = numeric(0))
  d <- dplyr::filter(obs, !is.na(.data$age), !is.na(.data$aboveground_c_mg_ha))
  if (nrow(d) == 0L) {
    return(empty)
  }
  d <- dplyr::mutate(d, .bin = floor(.data$age / bin))
  if (!is.null(site)) {
    d <- dplyr::summarise(
      d,
      age = mean(.data$age),
      aboveground_c_mg_ha = mean(.data$aboveground_c_mg_ha),
      .by = dplyr::all_of(c(".bin", site))
    )
  }
  ## A spline needs more distinct ages than basis functions; below that there is
  ## nothing to smooth and the honest answer is to draw nothing.
  n_bins <- dplyr::n_distinct(d$.bin)
  kk <- k %||% max(3L, min(5L, as.integer(floor(n_bins / 2))))
  if (dplyr::n_distinct(d$age) <= kk) {
    return(empty)
  }
  fit <- try(
    mgcv::gam(aboveground_c_mg_ha ~ s(age, k = kk), data = d, method = "REML"),
    silent = TRUE
  )
  if (inherits(fit, "try-error")) {
    return(empty)
  }
  grid <- data.frame(age = seq(min(d$age), max(d$age), length.out = n_grid))
  pred <- stats::predict(fit, grid, se.fit = TRUE)
  z <- stats::qnorm(1 - (1 - level) / 2)
  ## Clamp the FIT as well as the band, not just the band: an unconstrained
  ## spline runs slightly negative at the young end where the plots start above
  ## zero, and clamping only `lo` would leave `lo > value` there -- a band that
  ## does not contain its own curve.
  out <- tibble::tibble(
    age = grid$age,
    value = pmax(0, as.numeric(pred$fit)),
    lo = pmax(0, as.numeric(pred$fit) - z * as.numeric(pred$se.fit)),
    hi = pmax(0, as.numeric(pred$fit) + z * as.numeric(pred$se.fit))
  )
  attr(out, "k") <- kk
  out
}

#' Build one species' reference curves on a common age grid
#'
#' Everything the scorer needs, computed once per species rather than once per
#' parameter combination: each reference series evaluated at the same ages, and
#' each series' plateau level.
#'
#' Interpolation uses `rule = 1`, so a series is `NA` outside its own age range
#' and nothing is ever scored against an extrapolation of a reference that
#' simply stops.
#'
#' @param reference A data frame of reference observations, with columns
#'   `source` (`"SORTIE"`, `"TIPSY"`, `"VDYP"` or `"Ground plots"`), `age` and
#'   `aboveground_c_mg_ha`.
#' @param window Numeric length-2. The fitting window.
#' @param bin,plot_quantile,min_plots Ground-plot controls; `min_plots` is
#'   advisory and only sets `plots_sparse`. See
#'   [growth_bin_observations()] and [read_growth_scoring()].
#' @param n_grid Integer. Number of ages in the common grid.
#' @param use_tipsy Logical. Score against TIPSY as well.
#' @param use_vdyp Logical. Score against VDYP as well. VDYP is the British
#'   Columbia Variable Density Yield Projection model, whose curves are natural
#'   (unmanaged) stand yields; it is a separate series from TIPSY, which
#'   projects MANAGED stands, because a natural-disturbance model wants the
#'   former and the distinction must not be lost in the outputs.
#' @param site Optional column name identifying the sampling location of a
#'   ground-plot observation. Passed to [growth_bin_observations()]; when given,
#'   `n_plots` counts distinct locations rather than visits.
#' @param weight Optional column name holding a per-observation climatic weight.
#'   Passed to [growth_bin_observations()]; see [growth_climatic_weight()].
#'
#' @return A list with `ages`, `series`, `levels`, `n_plots`, `n_bins`,
#'   `plots_sparse`.
#' @family growth calibration helpers
#' @export
growth_reference_curves <- function(
  reference,
  window,
  bin = 20L,
  plot_quantile = 0.5,
  min_plots = 50L,
  n_grid = 60L,
  use_tipsy = FALSE,
  use_vdyp = FALSE,
  site = NULL,
  weight = NULL
) {
  ages <- seq(window[[1L]], window[[2L]], length.out = n_grid)

  as_series <- function(src, extra = character(0)) {
    dplyr::filter(reference, .data$source == src) |>
      dplyr::select(
        age = "age",
        value = "aboveground_c_mg_ha",
        dplyr::all_of(intersect(extra, names(reference)))
      ) |>
      dplyr::filter(!is.na(.data$age), !is.na(.data$value)) |>
      dplyr::arrange(.data$age)
  }

  at_grid <- function(d) {
    if (nrow(d) < 2L) {
      return(rep(NA_real_, length(ages)))
    }
    stats::approx(d$age, d$value, xout = ages, rule = 1)$y
  }

  obs <- as_series("Ground plots", extra = c(site, weight))
  ## Count locations, not visits, whenever the caller has identified them: a
  ## permanent plot measured five times is one plot's worth of evidence.
  n_plots <- if (!is.null(site) && site %in% names(obs)) {
    dplyr::n_distinct(obs[[site]])
  } else {
    nrow(obs)
  }
  ## ALWAYS bin and always score. `plots_warn_below` is advisory only: where
  ## observations are thin they are also the only evidence there is, and
  ## declining to fit leaves the parameter with no support at all rather than
  ## weak support. The plot and bin counts travel with the result so a reviewer
  ## can weigh the fit accordingly.
  plots_sparse <- n_plots < min_plots
  binned <- growth_bin_observations(
    dplyr::rename(obs, aboveground_c_mg_ha = "value"),
    bin = bin,
    probs = plot_quantile,
    site = site,
    weight = weight
  )

  raw <- list(
    sortie = as_series("SORTIE"),
    tipsy = as_series("TIPSY"),
    vdyp = as_series("VDYP"),
    plots = binned
  )

  ## The plateau each series implies. For the modelled curves that is the
  ## maximum of the WHOLE curve, which is the potential the stand is heading
  ## for; for the plots it is the top of the binned series inside the window,
  ## since the cloud has no asymptote of its own.
  levels <- c(
    sortie = if (nrow(raw$sortie)) max(raw$sortie$value) else NA_real_,
    tipsy = if (nrow(raw$tipsy)) max(raw$tipsy$value) else NA_real_,
    vdyp = if (nrow(raw$vdyp)) max(raw$vdyp$value) else NA_real_,
    plots = if (nrow(binned)) {
      inside <- binned$age >= window[[1L]] & binned$age <= window[[2L]]
      if (any(inside)) max(binned$value[inside]) else max(binned$value)
    } else {
      NA_real_
    }
  )

  scored <- c("sortie", "plots", if (isTRUE(use_tipsy)) "tipsy", if (isTRUE(use_vdyp)) "vdyp")
  series <- lapply(stats::setNames(scored, scored), function(nm) at_grid(raw[[nm]]))

  list(
    ages = ages,
    window = window,
    series = series,
    levels = levels,
    binned = binned,
    n_plots = n_plots,
    n_bins = nrow(binned),
    plots_sparse = plots_sparse
  )
}

#' Recover the `biomass_max` a candidate implies
#'
#' A ForCS cohort never quite reaches its `biomass_max`: it approaches it
#' asymptotically while mortality is already removing biomass, so the plateau it
#' actually holds is some fraction of the parameter. The current parameter set
#' was built as `biomass_max = reference curve maximum x 200`, which assumes that
#' fraction is 1. It is not: it runs from 0.90 (trembling aspen) to 1.00
#' (lodgepole pine) across these species, so that rule undershoots the intended
#' plateau by up to 11%.
#'
#' The fraction depends only on the growth and mortality shapes and on the ratio
#' of `anpp_max` to `biomass_max`, not on the absolute level -- verified across
#' a ForC Succession sweep, where combinations sharing a ratio but differing in
#' absolute `biomass_max` agree on the achieved fraction to within 0.04%. That
#' invariance is what lets the level be recovered arithmetically instead of
#' searched, and it is why `biomass_max` is held fixed across the factorial.
#'
#' @param achieved Numeric. Plateau the simulated curve actually reaches, in
#'   whatever units the curves are expressed in.
#' @param biomass_max Numeric. The maximum-biomass parameter that simulation
#'   ran at.
#' @param level Numeric. Plateau the curve should reach; same units as
#'   `achieved`.
#' @param biomass_max_scale Numeric. Divide `biomass_max` by this to express it
#'   in the curve's own units. The default 200 is the ForC Succession
#'   convention: `biomass_max` is g m^-2 of biomass while the summary log
#'   reports g C m^-2, and 1 Mg C ha^-1 corresponds to 200 g m^-2 of biomass.
#'   For an extension whose parameter and output share units -- Biomass
#'   Succession reports g m^-2 of biomass against a `maxBiomass` in g m^-2 --
#'   pass `1`.
#'
#' @return A list with `achieved_frac`, `inflation`, and `biomass_max_est`.
#' @family growth calibration helpers
#' @export
growth_inflation_factor <- function(achieved, biomass_max, level, biomass_max_scale = 200) {
  plateau <- biomass_max / biomass_max_scale
  frac <- achieved / plateau
  list(
    achieved_frac = frac,
    inflation = 1 / frac,
    biomass_max_est = biomass_max_scale * level / frac
  )
}

#' Score a simulated growth curve against its references
#'
#' Ranks on SHAPE alone. Each reference series is compared against the simulated
#' curve rescaled to that series' own plateau, so a candidate is never rewarded
#' for landing at the right level with the wrong trajectory, nor penalized for
#' the reverse -- the level is recovered separately and exactly.
#'
#' Errors are normalized by each series' level (`nrmse_*`) before being averaged
#' across series, since a hemlock curve plateauing near 240 Mg C ha^-1 and a pine
#' curve near 90 would otherwise contribute incomparable residuals.
#'
#' The two reference kinds answer different questions, so their relative weight
#' is a judgement the calibration must not make silently. SORTIE and TIPSY are
#' potential yield curves for fully stocked, pure, undisturbed stands, which is
#' exactly what a single-cohort calibration cell is. Ground plots are realized
#' stands, spanning every stocking level and disturbance history, with whole-plot
#' volume attributed to a leading species that holds a median of 69% of the
#' stand. Weight them with `weight_sortie` / `weight_plots` in
#' `growth_scoring.csv`; setting one to 0 drops it from the ranking while leaving
#' it on the review figures.
#'
#' @param curve A tibble with `age`, `aboveground_c_mg_ha`, `anpp_max` and
#'   `biomass_max` for one combination.
#' @param ref A list from [growth_reference_curves()].
#' @param level_source Character. Which series' plateau to report a
#'   `biomass_max` recommendation against; `NA` picks the first available of
#'   SORTIE, TIPSY, plots.
#' @param weights Named numeric. Relative weight per reference series in the
#'   ranking.
#' @param biomass_max_scale Numeric. Passed to [growth_inflation_factor()].
#'
#' @return A one-row tibble of fit statistics.
#' @family growth calibration helpers
#' @export
growth_score_fit <- function(
  curve,
  ref,
  level_source = NA_character_,
  weights = c(sortie = 1, tipsy = 1, vdyp = 1, plots = 1),
  biomass_max_scale = 200
) {
  empty <- tibble::tibble(
    n_series = 0L,
    n_plots = NA_integer_,
    n_bins = NA_integer_,
    plots_sparse = NA,
    rmse_sortie = NA_real_,
    rmse_tipsy = NA_real_,
    rmse_vdyp = NA_real_,
    rmse_plots = NA_real_,
    nrmse_shape = NA_real_,
    achieved = NA_real_,
    achieved_frac = NA_real_,
    anpp_prop = NA_real_,
    level_used = NA_real_,
    level_source_requested = NA_character_,
    level_source_used = NA_character_,
    biomass_max_est = NA_real_,
    anpp_max_est = NA_real_,
    biomass_at_end = NA_real_
  )
  if (nrow(curve) == 0L) {
    return(empty)
  }

  achieved <- max(curve$aboveground_c_mg_ha, na.rm = TRUE)
  sim <- stats::approx(curve$age, curve$aboveground_c_mg_ha, xout = ref$ages, rule = 1)$y

  ## Shape error per series: the simulated curve rescaled to that series'
  ## plateau, then compared point for point on the shared grid.
  per_series <- vapply(
    names(ref$series),
    function(nm) {
      lvl <- ref$levels[[nm]]
      obs <- ref$series[[nm]]
      if (is.na(lvl) || achieved <= 0 || all(is.na(obs))) {
        return(c(rmse = NA_real_, nrmse = NA_real_))
      }
      scaled <- sim * lvl / achieved
      ok <- !is.na(scaled) & !is.na(obs)
      if (!any(ok)) {
        return(c(rmse = NA_real_, nrmse = NA_real_))
      }
      rmse <- sqrt(mean((scaled[ok] - obs[ok])^2))
      c(rmse = rmse, nrmse = rmse / lvl)
    },
    numeric(2)
  )

  rmse_of <- function(nm) {
    if (nm %in% colnames(per_series)) unname(per_series["rmse", nm]) else NA_real_
  }
  nrmse <- per_series["nrmse", ]

  ## Weighted mean over the series that produced an error, so a zero weight
  ## drops a series from the ranking without dropping it from the figures.
  w <- weights[names(nrmse)]
  w[is.na(w)] <- 1
  usable_w <- !is.na(nrmse) & w > 0
  shape <- if (any(usable_w)) {
    sum(nrmse[usable_w] * w[usable_w]) / sum(w[usable_w])
  } else {
    NA_real_
  }

  ## Which plateau to report a level recommendation against.
  order_pref <- c("sortie", "tipsy", "vdyp", "plots")
  usable <- order_pref[!is.na(ref$levels[order_pref])]
  chosen <- if (!is.na(level_source)) {
    ## A NOMINATED level source is a constraint, not a preference. Quietly
    ## substituting another reference reports a level nobody asked for: amabilis
    ## and subalpine fir nominate SORTIE, whose curves are `available_unused`,
    ## and falling through to TIPSY inflated their `biomass_max` by 63% and 74%
    ## over the values in use. Returning nothing points at the actual fix.
    if (level_source %in% usable) level_source else NA_character_
  } else if (length(usable)) {
    usable[[1L]]
  } else {
    NA_character_
  }

  bmax <- curve$biomass_max[[1L]]
  anpp <- curve$anpp_max[[1L]]
  level <- if (is.na(chosen)) NA_real_ else unname(ref$levels[[chosen]])
  inf <- growth_inflation_factor(achieved, bmax, level, biomass_max_scale)
  prop <- 100 * anpp / bmax

  tibble::tibble(
    n_series = sum(usable_w),
    n_plots = as.integer(ref$n_plots),
    n_bins = as.integer(ref$n_bins),
    plots_sparse = ref$plots_sparse,
    rmse_sortie = rmse_of("sortie"),
    rmse_tipsy = rmse_of("tipsy"),
    rmse_vdyp = rmse_of("vdyp"),
    rmse_plots = rmse_of("plots"),
    nrmse_shape = shape,
    achieved = achieved,
    achieved_frac = inf$achieved_frac,
    anpp_prop = prop,
    level_used = level,
    ## Recorded separately so a silent fallback is visible: a species whose
    ## nominated reference has no level (no SORTIE curve, too few plots) is
    ## scored against a different one, which the reviewer must see.
    level_source_requested = level_source,
    level_source_used = chosen,
    biomass_max_est = inf$biomass_max_est,
    anpp_max_est = inf$biomass_max_est * prop / 100,
    biomass_at_end = curve$aboveground_c_mg_ha[which.max(curve$age)]
  )
}

#' Rank fit statistics
#'
#' Ranking is on `nrmse_shape`: mean level-normalized shape error across the
#' reference series that exist for that species.
#'
#' @param scores A tibble of per-combination fit statistics.
#'
#' @return `scores` with `objective_rmse` and `objective`.
#' @family growth calibration helpers
#' @export
growth_add_objective <- function(scores) {
  dplyr::mutate(scores, objective_rmse = .data$nrmse_shape, objective = "shape")
}

## Candidates within the top `frac` of a species' ranking, at least two so a
## band is always defined. Ties are kept: dropping them would understate the
## very ambiguity this is measuring.
.growth_top_candidates <- function(scores, frac) {
  stopifnot(frac > 0, frac <= 1)
  scorable <- dplyr::filter(scores, !is.na(.data$objective_rmse))
  scorable |>
    dplyr::group_by(.data$species) |>
    dplyr::filter(dplyr::min_rank(.data$objective_rmse) <= max(2L, ceiling(frac * dplyr::n()))) |>
    dplyr::ungroup()
}

#' Is a swept parameter actually determined by the references?
#'
#' Taking an argmin over a factorial presumes the objective surface has a
#' well-defined minimum. Often it does not, and the reported best combination
#' is then whichever cell happened to sort first rather than a fitted value.
#' Nothing in a ranked table distinguishes the two cases, so this reports the
#' distinction directly: for each swept parameter, the range of values spanned
#' by the best-scoring candidates and the error spread across them.
#'
#' A parameter whose top candidates span most of the swept grid while their
#' errors differ by a few percent is not being estimated. Two patterns recur
#' and both are worth naming in a calibration's own output:
#'
#' * Mortality shape is routinely unidentified. Once a curve has reached its
#'   level, the shape of the approach barely moves the residual, so the
#'   objective is nearly flat along that axis. This appears to be inherent to
#'   fitting a plateau rather than a property of any one data set.
#' * An argmin on the edge of the swept grid means the optimum may lie outside
#'   it, and `boundary` flags this. It also makes any weighted average of the
#'   candidates biased inward by construction, which is the main reason to
#'   check identifiability before reaching for model averaging as the remedy.
#'
#' Species with no scorable candidate are absent from the result; see
#' [growth_best_candidates()], which reports them as an explicit refusal.
#'
#' @param scores A tibble from [growth_score_fit()] with an `objective_rmse`,
#'   as returned by [growth_add_objective()].
#' @param params Character vector of swept parameter columns to assess.
#' @param top_frac Numeric. Fraction of each species' ranking treated as the
#'   set of candidates that cannot be told apart.
#' @param identified_below Numeric. A parameter is reported as identified when
#'   its top candidates span no more than this fraction of the swept grid.
#'
#' @return One row per species and parameter, with the argmin value, the range
#'   spanned by the top candidates, the fraction of the grid that range covers,
#'   whether the argmin sits on a grid boundary, and the relative spread in
#'   objective across the top candidates.
#' @family growth calibration helpers
#' @export
growth_identifiability <- function(
  scores,
  params = c("growth_shp", "mort_shp", "anpp_prop"),
  top_frac = 0.10,
  identified_below = 0.5
) {
  missing_cols <- setdiff(c("species", "objective_rmse", params), names(scores))
  if (length(missing_cols)) {
    stop("`scores` has no column(s): ", paste(missing_cols, collapse = ", "), call. = FALSE)
  }

  top <- .growth_top_candidates(scores, top_frac)
  if (nrow(top) == 0L) {
    return(tibble::tibble(
      species = character(),
      parameter = character(),
      n_candidates = integer(),
      n_top = integer(),
      error_spread = numeric(),
      best = numeric(),
      top_min = numeric(),
      top_max = numeric(),
      grid_min = numeric(),
      grid_max = numeric(),
      grid_frac = numeric(),
      boundary = character(),
      identified = logical()
    ))
  }

  scorable <- dplyr::filter(scores, !is.na(.data$objective_rmse))
  out <- lapply(split(scorable, scorable$species), function(d) {
    if (!nrow(d)) {
      return(NULL)
    }
    t <- top[top$species == d$species[[1L]], , drop = FALSE]
    argmin <- d[which.min(d$objective_rmse), , drop = FALSE]
    rows <- lapply(params, function(p) {
      grid <- sort(unique(d[[p]]))
      inside <- unique(t[[p]])
      ## Compared on the grid, not on the raw value: a sweep is a set of
      ## nominated values, so "how much of what was asked for is still in play"
      ## is the question, and it is unaffected by uneven spacing.
      at <- which.min(abs(grid - argmin[[p]]))
      tibble::tibble(
        species = argmin$species,
        parameter = p,
        n_candidates = nrow(d),
        n_top = nrow(t),
        error_spread = max(t$objective_rmse) / min(t$objective_rmse) - 1,
        best = argmin[[p]],
        top_min = min(inside),
        top_max = max(inside),
        grid_min = min(grid),
        grid_max = max(grid),
        grid_frac = length(inside) / length(grid),
        boundary = if (length(grid) < 2L) {
          NA_character_
        } else if (at == 1L) {
          "min"
        } else if (at == length(grid)) {
          "max"
        } else {
          NA_character_
        }
      )
    })
    dplyr::bind_rows(rows)
  })

  dplyr::bind_rows(out) |>
    dplyr::mutate(identified = .data$grid_frac <= identified_below) |>
    dplyr::arrange(.data$species, .data$parameter)
}

#' Best candidate per species, or an explicit refusal
#'
#' A species with no scorable reference series MUST NOT produce a
#' recommendation. Three of the six species here are in that position: amabilis
#' fir, subalpine fir and hybrid spruce have too few ground plots to bin, and
#' their SORTIE curves are marked `available_unused` in
#' the project's curve-selection table because that model is not well
#' parameterized for them
#' in the ICH. Ranking hundreds of indistinguishable all-`NA` rows would return
#' whichever combination happened to sort first, dressed up as a result.
#'
#' Those species come back with `fitted = FALSE`, no parameters, and the values
#' currently in use carried through untouched. That is the honest answer, and it
#' names what would change it: more plots, or promoting a SORTIE curve to `used`.
#'
#' # Reading the level band
#'
#' Ranking is on shape alone and the level is recovered afterwards, by dividing
#' the reference plateau by the fraction of its own asymptote the simulated
#' curve reached (see [growth_inflation_factor()]). That division is the whole
#' reason `biomass_max_est` is not simply read off the winner and trusted: its
#' leverage on any error in the simulated curve is `1 / achieved_frac`. A
#' candidate that plateaued cleanly recovers its level almost exactly, while one
#' still climbing when the run ended is extrapolating, and two candidates a
#' fraction of a percent apart in shape error can then imply levels differing by
#' a factor of two or more.
#'
#' So `biomass_max_lo` / `biomass_max_hi` report the range of `biomass_max_est`
#' across the candidates that cannot be told apart from the winner, and
#' `level_extrapolated` flags a winner that never approached its asymptote.
#' A wide band is not noise to be averaged away: it says the references do not
#' determine the level, and the fix is a longer run, a better-constrained
#' reference, or a wider sweep -- not a different summary of the same surface.
#' Use [growth_identifiability()] to see which swept parameter is responsible.
#'
#' @param scores A tibble from [growth_score_fit()] with an `objective_rmse`.
#' @param growth_params The parameters currently in use.
#' @param windows A tibble from [growth_fitting_windows()].
#' @param scoring A tibble from [read_growth_scoring()], or `NULL`.
#' @param top_frac Numeric. Fraction of each species' ranking treated as
#'   indistinguishable from the winner, over which the level band is reported.
#' @param level_frac_warn Numeric. Warn when the selected candidate reached less
#'   than this fraction of its own asymptote, so its level is an extrapolation.
#'   `NA` disables the warning.
#'
#' @return One row per species.
#' @family growth calibration helpers
#' @export
growth_best_candidates <- function(
  scores,
  growth_params,
  windows,
  scoring = NULL,
  top_frac = 0.10,
  level_frac_warn = 0.9
) {
  ## The first four columns after `species` are the promotable set: exactly the
  ## four ForCS growth parameters, in the units `forcs_growth_params.csv` uses,
  ## ready to copy across. Everything after them is diagnostic. `anpp_prop` is
  ## how the sweep was specified, not what gets promoted -- `anpp_max_est` is the
  ## same quantity converted back to the g m^-2 yr^-1 ForCS expects.
  keep <- c(
    "species",
    "growth_shp",
    "mort_shp",
    "anpp_max_est",
    "biomass_max_est",
    "map_code",
    "anpp_prop",
    "objective",
    "objective_rmse",
    "rmse_sortie",
    "rmse_tipsy",
    ## `rmse_vdyp` postdates the other three: a scores table built by an earlier
    ## release will not carry it, so the per-series residuals are selected with
    ## any_of() below rather than required outright
    "rmse_vdyp",
    "rmse_plots",
    "n_series",
    "n_plots",
    "n_bins",
    "plots_sparse",
    "achieved",
    "achieved_frac",
    "level_used",
    "level_source_requested",
    "level_source_used",
    "biomass_at_end"
  )

  scorable <- dplyr::filter(scores, !is.na(.data$objective_rmse))

  ## What the level would have been had the ranking picked any of the other
  ## candidates it cannot distinguish from the winner. Reported beside the point
  ## estimate rather than instead of it: the argmin is still the recommendation,
  ## the band is how far it can be trusted.
  ## A candidate can rank on shape and still recover no level, when the series
  ## it was told to take its level from has none. `min(NA, na.rm = TRUE)` would
  ## answer `Inf` and a warning about no non-missing arguments; the band is
  ## simply undefined.
  rng <- function(x, f) if (all(is.na(x))) NA_real_ else f(x, na.rm = TRUE)
  band <- .growth_top_candidates(scorable, top_frac) |>
    dplyr::summarise(
      biomass_max_lo = rng(.data$biomass_max_est, min),
      biomass_max_hi = rng(.data$biomass_max_est, max),
      anpp_max_lo = rng(.data$anpp_max_est, min),
      anpp_max_hi = rng(.data$anpp_max_est, max),
      n_indistinct = dplyr::n(),
      .by = "species"
    )

  best <- scorable |>
    dplyr::slice_min(.data$objective_rmse, n = 1L, by = "species", with_ties = FALSE) |>
    dplyr::select(dplyr::any_of(keep)) |>
    dplyr::mutate(fitted = TRUE) |>
    dplyr::left_join(band, by = "species") |>
    dplyr::mutate(
      level_leverage = 1 / .data$achieved_frac,
      level_extrapolated = !is.na(level_frac_warn) &
        !is.na(.data$achieved_frac) &
        .data$achieved_frac < level_frac_warn
    )

  flagged <- best$species[best$level_extrapolated]
  if (length(flagged)) {
    warning(
      "level recovered by extrapolation for ",
      paste(flagged, collapse = ", "),
      ": the best candidate reached under ",
      round(100 * level_frac_warn),
      "% of its own asymptote, so `biomass_max_est` is scaled up by ",
      paste0(round(best$level_leverage[best$level_extrapolated], 2), "x", collapse = ", "),
      ". Check `biomass_max_lo`/`biomass_max_hi` before promoting it.",
      call. = FALSE
    )
  }

  ## Species that produced no scorable combination at all. Keep their
  ## diagnostics -- plot count, requested level source -- so the reason is on
  ## the face of the table.
  missing <- setdiff(unique(scores$species), best$species)
  if (length(missing)) {
    stub <- scores |>
      dplyr::filter(.data$species %in% missing) |>
      dplyr::slice_head(n = 1L, by = "species") |>
      dplyr::select(dplyr::any_of(keep)) |>
      dplyr::mutate(
        dplyr::across(
          dplyr::any_of(c(
            "map_code",
            "growth_shp",
            "mort_shp",
            "anpp_prop",
            "objective_rmse",
            "rmse_sortie",
            "rmse_tipsy",
            "rmse_vdyp",
            "rmse_plots",
            "achieved",
            "achieved_frac",
            "level_used",
            "biomass_max_est",
            "anpp_max_est",
            "biomass_at_end"
          )),
          \(x) x[NA_integer_]
        ),
        level_source_used = NA_character_,
        fitted = FALSE
      )
    best <- dplyr::bind_rows(best, stub)
  }

  best |>
    dplyr::mutate(dplyr::across(
      dplyr::any_of(c(
        "biomass_max_est",
        "anpp_max_est",
        "biomass_max_lo",
        "biomass_max_hi",
        "anpp_max_lo",
        "anpp_max_hi"
      )),
      round
    )) |>
    ## Band beside the estimate it qualifies, so a reader copying a value across
    ## cannot reach it without passing the range it could equally have taken.
    dplyr::relocate(
      "anpp_max_lo",
      "anpp_max_hi",
      "biomass_max_lo",
      "biomass_max_hi",
      "n_indistinct",
      "level_leverage",
      "level_extrapolated",
      .after = "biomass_max_est"
    ) |>
    dplyr::left_join(
      dplyr::select(
        growth_params,
        species,
        current_growth_shp = growth_shp,
        current_mort_shp = mort_shp,
        current_anpp_max = anpp_max,
        current_biomass_max = biomass_max
      ),
      by = "species"
    ) |>
    dplyr::left_join(
      dplyr::select(windows, species, mature_from, mature_to, window_source),
      by = "species"
    ) |>
    dplyr::arrange(.data$species)
}


## Reference-series appearance, defined once so a linetype means the same thing
## on every figure. Without `drop = FALSE` ggplot assigns linetypes from the
## levels PRESENT, so a panel with only TIPSY would draw it solid -- exactly the
## key that means SORTIE everywhere else.
## Every modelled source needs an entry here, not just a linetype anyone likes: the scale is built
## with `limits = names(...)` and `na.translate = FALSE`, so a source missing from this vector is
## DROPPED FROM THE PLOT rather than drawn in some default style. That is how the VDYP series came
## to be scored, filtered in, and still invisible -- the only symptom was a
## "Removed N rows containing missing values (geom_line)" warning.
.growth_reference_linetypes <- c(SORTIE = "solid", TIPSY = "14", VDYP = "22")

.growth_palette_default <- c(
  ## The parameter set in use, and the sweep result being weighed against it.
  current = "black",
  candidate = "firebrick",
  ## Individual ground plots, and those carrying so little weight that drawing
  ## them at full strength would misstate what the curve was fitted to.
  plots = "grey60",
  faint = "grey72",
  ## Every summary of the ground-plot cloud -- the age-binned points and the
  ## fitted curve alike. ONE colour, because they are one set of observations
  ## summarized two ways; the glyph is what separates them.
  summary = "steelblue4",
  ## Model reference curves (SORTIE / TIPSY / VDYP). They share a colour and are
  ## told apart by linetype; see [scale_linetype_growth_reference()].
  reference = "grey35",
  ## The shaded fitting window, and the outline that keeps a point legible where
  ## it sits on top of a line.
  window = "goldenrod2",
  key_outline = "white"
)

#' Colours for the growth-calibration figures
#'
#' The figure families in this package and in the projects that use it are read
#' side by side, so a colour has to mean the same thing in all of them. This is
#' that vocabulary, keyed by ROLE rather than by figure, so a project drawing its
#' own variant can match without copying hex values -- and so changing one is a
#' single edit rather than a search.
#'
#' Colours only. Linetypes have their own scale
#' ([scale_linetype_growth_reference()]), and the review panel's per-series key
#' spec is positional and specific to that one figure, so folding all three into
#' a single table would couple things that change for different reasons.
#'
#' Returns a NAMED CHARACTER VECTOR, which is what
#' `ggplot2::scale_colour_manual(values = )` and friends take directly.
#'
#' Roles: `current`, `candidate`, `plots`, `faint`, `summary`, `reference`,
#' `window`, `key_outline`.
#'
#' @param ... Optional named overrides, e.g. `candidate = "darkorange"`. Names
#'   must be existing roles; anything else is an error rather than a silently
#'   ignored typo.
#'
#' @return A named character vector of colours.
#' @family growth calibration helpers
#' @export
#' @examples
#' growth_plot_palette()[["summary"]]
#' growth_plot_palette(candidate = "darkorange")[["candidate"]]
growth_plot_palette <- function(...) {
  out <- .growth_palette_default
  over <- list(...)
  if (length(over) == 0L) {
    return(out)
  }
  if (is.null(names(over)) || !all(nzchar(names(over)))) {
    stop("overrides must be named, e.g. growth_plot_palette(candidate = 'red').", call. = FALSE)
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


## Upper age limit for a growth panel. `NULL` means "as far as the data goes",
## which is the only default that cannot silently clip a longer run: a hard 400
## hid the entire senescence half of a 600-year western hemlock curve, whose
## longevity is 650, and the figure gave no sign the data continued past the
## axis. Rounded up to a multiple of 50 so the axis stays tidy.
.growth_x_max <- function(x_max, ...) {
  if (!is.null(x_max)) {
    return(x_max)
  }
  ages <- unlist(list(...), use.names = FALSE)
  ages <- ages[is.finite(ages)]
  if (!length(ages)) {
    return(400)
  }
  ceiling(max(ages) / 50) * 50
}

## Key glyphs for the review panel's series legend, in palette order: current
## parameters, best candidate, ground plots, age-binned points, GAM fit. The
## colour and fill guides must be given IDENTICAL specs or ggplot2 refuses to
## merge them and draws the same series twice.
.growth_series_key <- function(has_smooth) {
  pal <- growth_plot_palette()
  list(
    linetype = c("solid", "solid", "blank", "blank", if (has_smooth) "solid"),
    shape = c(NA, NA, 16, 23, if (has_smooth) NA),
    fill = c(NA, NA, NA, pal[["summary"]], if (has_smooth) NA),
    colour = unname(pal[c(
      "current",
      "candidate",
      "plots",
      "key_outline",
      if (has_smooth) "summary"
    )]),
    linewidth = c(1, 1, 0, 0, if (has_smooth) 0.7),
    size = c(0, 0, 1.3, 2.6, if (has_smooth) 0)
  )
}

## Legend breaks for the plots-per-bin size scale. Always show 1, because
## "this point is a single plot" is the thing the scale exists to communicate,
## and always show the maximum, so the reader can judge the span.
.growth_bin_size_breaks <- function(n) {
  n <- n[!is.na(n)]
  if (length(n) == 0L) {
    return(1L)
  }
  hi <- max(n)
  if (hi <= 1) {
    return(1L)
  }
  ## At most four keys: this guide shares a crowded legend box with three
  ## others, and 1 and the maximum carry almost all of its meaning.
  breaks <- unique(c(1L, pretty(c(1L, hi), n = 2L), hi))
  breaks <- breaks[breaks >= 1 & breaks <= hi]
  breaks <- as.integer(sort(unique(round(breaks))))
  if (length(breaks) > 4L) {
    breaks <- unique(c(breaks[[1L]], breaks[c(2L, length(breaks) - 1L)], breaks[[length(breaks)]]))
  }
  breaks
}

#' Age at which a reference growth curve's increment peaks
#'
#' The inflection point of a sigmoid growth curve: the age of maximum annual
#' increment, after which growth slows and the curve approaches its asymptote.
#'
#' This is what defines "mature" for scoring purposes. A fixed age threshold
#' cannot: the modelled species differ several-fold in longevity and in how fast
#' they get there, so age 100 is past the inflection for a fast, short-lived
#' species and well before it for a slow, long-lived one. Deriving the threshold
#' from each species' own reference curve keeps the prioritized window in the
#' same place on every curve -- the part where the trajectory is settling toward
#' the level it will hold.
#'
#' The increment is smoothed before the maximum is taken, because SORTIE curves
#' are individual-tree simulations and their year-to-year increments are noisy
#' enough for the raw argmax to land almost anywhere.
#'
#' Two definitions are available. `"inflection"` is the age of peak increment
#' itself, the point at which growth stops accelerating. `"asymptote"` is the
#' age at which the curve first reaches `frac` of its maximum, which sits later
#' and isolates the landing region more tightly -- an inflection-based window
#' can start very early for a fast species (age 18 for lodgepole pine) and so
#' still carries much of the rapid-growth phase.
#'
#' @param reference A data frame of reference observations, with columns
#'   `source` (`"SORTIE"`, `"TIPSY"`, `"VDYP"` or `"Ground plots"`), `age` and
#'   `aboveground_c_mg_ha`.
#' @param method `"inflection"` or `"asymptote"`.
#' @param frac Numeric. Fraction of the curve maximum, for `"asymptote"`.
#' @param sources Character vector of reference sources to use, in order of
#'   preference; the first one present is used.
#' @param smooth_window Integer. Width of the moving average over increments.
#' @param default Numeric. Returned when no usable model reference exists.
#'
#' @return A single age.
#' @family growth calibration helpers
#' @export
growth_reference_inflection <- function(
  reference,
  method = c("inflection", "asymptote"),
  frac = 0.9,
  sources = c("SORTIE", "TIPSY", "VDYP"),
  smooth_window = 21L,
  default = 100
) {
  method <- match.arg(method)
  for (src in sources) {
    d <- reference |>
      dplyr::filter(.data$source == src, !is.na(.data$aboveground_c_mg_ha)) |>
      dplyr::arrange(.data$age)

    if (nrow(d) < smooth_window + 2L) {
      next
    }

    if (method == "asymptote") {
      hit <- which(d$aboveground_c_mg_ha >= frac * max(d$aboveground_c_mg_ha))
      if (length(hit)) {
        return(d$age[hit[[1L]]])
      }
      next
    }

    inc <- diff(d$aboveground_c_mg_ha) / diff(d$age)
    ages <- d$age[-1L]
    ## centred moving average; stats::filter, not dplyr::filter
    sm <- as.numeric(stats::filter(inc, rep(1 / smooth_window, smooth_window), sides = 2))
    ok <- !is.na(sm)
    if (!any(ok)) {
      next
    }
    return(ages[ok][which.max(sm[ok])])
  }
  default
}

#' Fraction of longevity at which age-related mortality begins
#'
#' Biomass Succession expresses `MortalityCurve` as a position in the lifespan,
#' not as a rate: v7 User Guide 2.12.4 states that 5 puts the onset of
#' age-related mortality at 10% of life span and 25 puts it at 85%. This inverts
#' that definition, linearly between the two documented endpoints.
#'
#' The parameter therefore carries no absolute meaning without the `longevity`
#' it sits beside, which is why a value cannot be lifted from one
#' parameterisation into another with a different lifespan.
#'
#' @param mort_shp Numeric `MortalityCurve`, 5 to 25.
#'
#' @return Numeric fraction of longevity, 0.10 to 0.85.
#' @family growth calibration helpers
#' @export
growth_mortality_onset_frac <- function(mort_shp) {
  0.10 + (as.numeric(mort_shp) - 5) / 20 * 0.75
}

#' Derive one species' fitting window from where its references have support
#'
#' Nobody should have to nominate an age range by hand. Both bounds are dictated
#' by the data and by a species attribute that is not being fitted, so both can
#' be read off directly.
#'
#' The window OPENS at `age_floor`. Stands younger than that are essentially
#' unmeasured -- the ground-plot programs do not sample them -- and it is also
#' the range where LANDIS-II is known to overestimate biomass for reasons that
#' have nothing to do with these four parameters, so scoring there would import a
#' bias the sweep cannot fix.
#'
#' The window CLOSES at the earliest of three limits: the `age_quantile` of the
#' observed plot ages, beyond which the cloud thins to a handful of stands; the
#' end of the modelled reference curve; and `senescence_frac` x `longevity`.
#' LANDIS-II ramps mortality up as a cohort approaches `longevity` and the curve
#' then falls to exactly zero and stays there, so an open-ended window scores the
#' modelled die-off rather than the level the stand holds.
#'
#' WHERE THAT CAP BELONGS IS A PROPERTY OF `MortalityCurve`. The extension
#' defines it as a position in the lifespan (2.12.4: 5 puts onset at 10% of life
#' span, 25 at 85%), so the age at which a species leaves its plateau varies by
#' nearly twofold across the documented range. Supply `mort_shp` and the cap is
#' that species' own onset, via [growth_mortality_onset_frac()]. Measured on one
#' calibration, the departure from 95% of peak biomass ran 0.43-0.48 x longevity
#' at `MortalityCurve` 10, 0.63-0.70 at 15 and 0.82-0.84 at 25 -- so a single
#' fraction cannot separate a species that breaks up early from one that holds
#' its stand almost to the end, which is the distinction the parameter exists to
#' make.
#'
#' The cap is at the ONSET of mortality, not at peak biomass, and is therefore
#' conservative: biomass keeps rising for a period after onset while growth still
#' exceeds mortality. That is deliberate. Peak location depends on `GrowthCurve`
#' as well, and `GrowthCurve` may still be swept -- a peak-based cap would then
#' score different candidates over different ranges and could not rank them
#' fairly. Onset depends on `MortalityCurve` alone, so as long as that is
#' assigned rather than swept, every candidate for a species sees one window.
#'
#' `senescence_frac` remains as the fallback when `mort_shp` is not supplied. Its
#' default of 0.45 was calibrated against a parameterisation that gave every
#' species a `MortalityCurve` near 23; it does not generalise, and on a set
#' carrying 10s the earliest 95%-of-peak departure falls to 0.433, below the cap
#' itself.
#'
#' @param reference A data frame of reference observations, with columns
#'   `source` (`"SORTIE"`, `"TIPSY"`, `"VDYP"` or `"Ground plots"`), `age` and
#'   `aboveground_c_mg_ha`.
#' @param longevity Numeric. The species' `longevity`.
#' @param age_floor Numeric. Youngest age to score.
#' @param age_quantile Numeric. Quantile of observed plot ages to close at.
#' @param senescence_frac Numeric. Fraction of longevity at which to close, used
#'   only when `mort_shp` is `NULL`.
#' @param mort_shp Numeric `MortalityCurve` for this species, or `NULL`. When
#'   supplied it sets the cap and `senescence_frac` is ignored.
#' @param sources Character. Modelled reference series to consider.
#'
#' @return Numeric length-2.
#' @family growth calibration helpers
#' @export
growth_auto_window <- function(
  reference,
  longevity,
  age_floor = 20,
  age_quantile = 0.95,
  senescence_frac = 0.45,
  mort_shp = NULL,
  sources = c("SORTIE", "TIPSY", "VDYP")
) {
  ages_of <- function(srcs) {
    a <- reference$age[reference$source %in% srcs & !is.na(reference$aboveground_c_mg_ha)]
    a[!is.na(a)]
  }

  plot_ages <- ages_of("Ground plots")
  model_ages <- ages_of(sources)

  frac <- if (!is.null(mort_shp) && length(mort_shp) && !is.na(mort_shp[[1L]])) {
    growth_mortality_onset_frac(mort_shp[[1L]])
  } else {
    senescence_frac
  }
  upper <- frac * longevity
  if (length(plot_ages) >= 5L) {
    upper <- min(upper, stats::quantile(plot_ages, age_quantile, names = FALSE))
  }
  if (length(model_ages)) {
    upper <- min(upper, max(model_ages))
  }

  ## Whole years: LANDIS-II steps annually, so a bound of 83.93 names no
  ## timestep the simulation ever reports. Rounded INWARD so neither bound is
  ## pushed past the support it was derived from.
  lower <- ceiling(age_floor)
  ## The tolerance is not cosmetic. `growth_mortality_onset_frac(10)` is
  ## 0.28749999999999998 rather than 0.2875, because 0.10 has no exact binary
  ## representation, so an onset that lands mathematically on a whole year comes
  ## out a hair below it and `floor()` takes the year before: 0.2875 x 400 gives
  ## 114.99999999999999 and a cap of 114 instead of 115. Rounding inward is
  ## deliberate everywhere else here, and this keeps that while not letting it be
  ## triggered by representation error at an exact integer.
  upper <- floor(upper + 1e-8)
  ## A degenerate window would silently score nothing.
  c(lower, max(upper, lower + 1))
}

#' Per-species fitting windows
#'
#' Derives each species' window with [growth_auto_window()], then applies any
#' explicit `age_min` / `age_max` from `growth_scoring.csv`. Blank cells keep
#' the derived value, so the hand-maintained file only ever has to record
#' DEPARTURES from what the data implies.
#'
#' @param references Named list of reference tables, one per species.
#' @param species_core A tibble with `species` and `longevity`.
#' @param scoring A tibble from [read_growth_scoring()], or `NULL`.
#' @param mort_shp Per-species `MortalityCurve`: a named numeric vector, or a
#'   data frame with `species` and `mort_shp`. Supplying it makes each window
#'   close at that species' own onset of age-related mortality rather than at a
#'   single fraction shared by every species; see [growth_auto_window()]. A
#'   species missing from it falls back to `senescence_frac`.
#' @param ... Passed to [growth_auto_window()].
#'
#' @return A tibble with `species`, `mature_from`, `mature_to`, `longevity`,
#'   `inflection` and `window_source`.
#' @family growth calibration helpers
#' @export
growth_fitting_windows <- function(references, species_core, scoring = NULL, mort_shp = NULL, ...) {
  longevity_of <- function(sp) {
    i <- match(sp, species_core$species)
    if (is.na(i)) NA_real_ else as.numeric(species_core$longevity[[i]])
  }

  ## Accept either shape rather than making the caller reshape: the assigned
  ## values live in a per-species table in one project and a named vector in
  ## another, and neither is more natural than the other.
  if (is.data.frame(mort_shp)) {
    mort_shp <- stats::setNames(as.numeric(mort_shp$mort_shp), as.character(mort_shp$species))
  }
  mort_of <- function(sp) {
    if (is.null(mort_shp) || !sp %in% names(mort_shp)) NULL else unname(mort_shp[[sp]])
  }

  auto <- purrr::map2(references, names(references), function(r, sp) {
    w <- growth_auto_window(r, longevity_of(sp), mort_shp = mort_of(sp), ...)
    tibble::tibble(
      species = sp,
      mature_from = w[[1L]],
      mature_to = w[[2L]],
      longevity = longevity_of(sp),
      ## Reported for context only: the age of peak increment on the species'
      ## own reference curve, i.e. where growth stops accelerating.
      inflection = as.numeric(growth_reference_inflection(r))
    )
  }) |>
    dplyr::bind_rows()

  if (is.null(scoring) || nrow(scoring) == 0L) {
    return(dplyr::mutate(auto, window_source = "derived"))
  }

  auto |>
    dplyr::left_join(dplyr::select(scoring, species, age_min, age_max), by = "species") |>
    dplyr::mutate(
      window_source = dplyr::if_else(
        is.na(.data$age_min) & is.na(.data$age_max),
        "derived",
        "growth_scoring.csv"
      ),
      mature_from = ceiling(dplyr::coalesce(.data$age_min, .data$mature_from)),
      mature_to = pmax(
        floor(dplyr::coalesce(.data$age_max, .data$mature_to)),
        ceiling(dplyr::coalesce(.data$age_min, .data$mature_from)) + 1
      )
    ) |>
    dplyr::select(species, mature_from, mature_to, longevity, inflection, window_source)
}

#' Look up one species' fitting window
#'
#' @param windows A tibble from [growth_fitting_windows()].
#' @param species Character. Species code.
#'
#' @return Numeric length-2, suitable for `mature_window`.
#' @family growth calibration helpers
#' @export
growth_window_for <- function(windows, species) {
  i <- match(species, windows$species)
  if (is.na(i)) {
    return(c(100, Inf))
  }
  c(windows$mature_from[[i]], windows$mature_to[[i]])
}
