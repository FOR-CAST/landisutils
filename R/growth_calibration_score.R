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
          "weight_plots"
        )),
        as.numeric
      )
    ) |>
    dplyr::select(
      species,
      age_min,
      age_max,
      age_bin,
      plot_quantile,
      plots_warn_below,
      weight_sortie,
      weight_tipsy,
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
  out$weight_plots <- keep(ctl$weight_plots, out$weight_plots)
  lvl <- keep(ctl$level_source, NA_character_)
  out$level_source <- if (is.character(lvl) && nzchar(trimws(lvl))) trimws(lvl) else NA_character_
  out
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
#' @param obs A tibble with `age` and `aboveground_c_mg_ha`.
#' @param bin Numeric. Bin width in years.
#' @param probs Numeric. Quantile to take within each bin.
#'
#' @return A tibble with `age` (bin mean), `value`, and `n`.
#' @family growth calibration helpers
#' @export
growth_bin_observations <- function(obs, bin = 20L, probs = 0.5) {
  d <- dplyr::filter(obs, !is.na(.data$age), !is.na(.data$aboveground_c_mg_ha))
  if (nrow(d) == 0L) {
    return(tibble::tibble(age = numeric(0), value = numeric(0), n = integer(0)))
  }
  d |>
    dplyr::mutate(.bin = floor(.data$age / bin)) |>
    dplyr::summarise(
      age = mean(.data$age),
      value = stats::quantile(.data$aboveground_c_mg_ha, probs, names = FALSE),
      n = dplyr::n(),
      .by = ".bin"
    ) |>
    dplyr::arrange(.data$age) |>
    dplyr::select(age, value, n)
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
#'   `source` (`"SORTIE"`, `"TIPSY"` or `"Ground plots"`), `age` and
#'   `aboveground_c_mg_ha`.
#' @param window Numeric length-2. The fitting window.
#' @param bin,plot_quantile,min_plots Ground-plot controls; `min_plots` is
#'   advisory and only sets `plots_sparse`. See
#'   [growth_bin_observations()] and [read_growth_scoring()].
#' @param n_grid Integer. Number of ages in the common grid.
#' @param use_tipsy Logical. Score against TIPSY as well.
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
  use_tipsy = FALSE
) {
  ages <- seq(window[[1L]], window[[2L]], length.out = n_grid)

  as_series <- function(src) {
    dplyr::transmute(
      dplyr::filter(reference, .data$source == src),
      age = .data$age,
      value = .data$aboveground_c_mg_ha
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

  obs <- as_series("Ground plots")
  n_plots <- nrow(obs)
  ## ALWAYS bin and always score. `plots_warn_below` is advisory only: where
  ## observations are thin they are also the only evidence there is, and
  ## declining to fit leaves the parameter with no support at all rather than
  ## weak support. The plot and bin counts travel with the result so a reviewer
  ## can weigh the fit accordingly.
  plots_sparse <- n_plots < min_plots
  binned <- growth_bin_observations(
    dplyr::rename(obs, aboveground_c_mg_ha = value),
    bin = bin,
    probs = plot_quantile
  )

  raw <- list(sortie = as_series("SORTIE"), tipsy = as_series("TIPSY"), plots = binned)

  ## The plateau each series implies. For the modelled curves that is the
  ## maximum of the WHOLE curve, which is the potential the stand is heading
  ## for; for the plots it is the top of the binned series inside the window,
  ## since the cloud has no asymptote of its own.
  levels <- c(
    sortie = if (nrow(raw$sortie)) max(raw$sortie$value) else NA_real_,
    tipsy = if (nrow(raw$tipsy)) max(raw$tipsy$value) else NA_real_,
    plots = if (nrow(binned)) {
      inside <- binned$age >= window[[1L]] & binned$age <= window[[2L]]
      if (any(inside)) max(binned$value[inside]) else max(binned$value)
    } else {
      NA_real_
    }
  )

  scored <- c("sortie", "plots", if (isTRUE(use_tipsy)) "tipsy")
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
  weights = c(sortie = 1, tipsy = 1, plots = 1),
  biomass_max_scale = 200
) {
  empty <- tibble::tibble(
    n_series = 0L,
    n_plots = NA_integer_,
    n_bins = NA_integer_,
    plots_sparse = NA,
    rmse_sortie = NA_real_,
    rmse_tipsy = NA_real_,
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
  order_pref <- c("sortie", "tipsy", "plots")
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
#' @param scores A tibble from [growth_score_fit()] with an `objective_rmse`.
#' @param growth_params The parameters currently in use.
#' @param windows A tibble from [growth_fitting_windows()].
#' @param scoring A tibble from [read_growth_scoring()], or `NULL`.
#'
#' @return One row per species.
#' @family growth calibration helpers
#' @export
growth_best_candidates <- function(scores, growth_params, windows, scoring = NULL) {
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
  best <- scorable |>
    dplyr::slice_min(.data$objective_rmse, n = 1L, by = "species", with_ties = FALSE) |>
    dplyr::select(dplyr::all_of(keep)) |>
    dplyr::mutate(fitted = TRUE)

  ## Species that produced no scorable combination at all. Keep their
  ## diagnostics -- plot count, requested level source -- so the reason is on
  ## the face of the table.
  missing <- setdiff(unique(scores$species), best$species)
  if (length(missing)) {
    stub <- scores |>
      dplyr::filter(.data$species %in% missing) |>
      dplyr::slice_head(n = 1L, by = "species") |>
      dplyr::select(dplyr::all_of(keep)) |>
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
    dplyr::mutate(
      biomass_max_est = round(.data$biomass_max_est),
      anpp_max_est = round(.data$anpp_max_est)
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
.growth_reference_linetypes <- c(SORTIE = "solid", TIPSY = "dashed")

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
#'   `source` (`"SORTIE"`, `"TIPSY"` or `"Ground plots"`), `age` and
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
  sources = c("SORTIE", "TIPSY"),
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
#' The last of those is the one that binds in practice. LANDIS-II ramps mortality
#' up as a cohort approaches `longevity` and the curve then falls to exactly zero
#' and stays there, so an open-ended window scores the modelled die-off rather
#' than the level the stand holds. The cap is a fraction of `longevity` rather
#' than something read off the simulated curve, because the decline timing
#' depends on the mortality shape, which is itself being swept -- a
#' candidate-dependent window would score different candidates over different
#' ranges and could not rank them fairly.
#'
#' `senescence_frac = 0.45` is conservative: across the calibrated species the
#' earliest departure from 95% of peak biomass is at 0.47 x longevity, so every
#' species is still at its plateau throughout its window.
#'
#' @param reference A data frame of reference observations, with columns
#'   `source` (`"SORTIE"`, `"TIPSY"` or `"Ground plots"`), `age` and
#'   `aboveground_c_mg_ha`.
#' @param longevity Numeric. The species' `longevity`.
#' @param age_floor Numeric. Youngest age to score.
#' @param age_quantile Numeric. Quantile of observed plot ages to close at.
#' @param senescence_frac Numeric. Fraction of longevity at which to close.
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
  sources = c("SORTIE", "TIPSY")
) {
  ages_of <- function(srcs) {
    a <- reference$age[reference$source %in% srcs & !is.na(reference$aboveground_c_mg_ha)]
    a[!is.na(a)]
  }

  plot_ages <- ages_of("Ground plots")
  model_ages <- ages_of(sources)

  upper <- senescence_frac * longevity
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
  upper <- floor(upper)
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
#' @param ... Passed to [growth_auto_window()].
#'
#' @return A tibble with `species`, `mature_from`, `mature_to`, `longevity`,
#'   `inflection` and `window_source`.
#' @family growth calibration helpers
#' @export
growth_fitting_windows <- function(references, species_core, scoring = NULL, ...) {
  longevity_of <- function(sp) {
    i <- match(sp, species_core$species)
    if (is.na(i)) NA_real_ else as.numeric(species_core$longevity[[i]])
  }

  auto <- purrr::map2(references, names(references), function(r, sp) {
    w <- growth_auto_window(r, longevity_of(sp), ...)
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
