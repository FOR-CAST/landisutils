## A reference set with a smooth model curve and a deliberately noisy plot cloud,
## so binning has something to do.
reference <- function() {
  ages <- 1:300
  dplyr::bind_rows(
    tibble::tibble(
      source = "SORTIE",
      age = ages,
      aboveground_c_mg_ha = 200 * (1 - exp(-0.02 * ages))
    ),
    tibble::tibble(
      source = "Ground plots",
      age = c(25, 30, 35, 45, 50, 55, 105, 110, 115, 205, 210, 215),
      aboveground_c_mg_ha = c(60, 20, 90, 100, 40, 130, 150, 90, 180, 190, 140, 200)
    )
  )
}

sim_curve <- function(k = 0.02, plateau = 150, biomass_max = 30000) {
  ages <- 1:300
  tibble::tibble(
    age = ages,
    aboveground_c_mg_ha = plateau * (1 - exp(-k * ages)),
    anpp_max = 2000,
    biomass_max = biomass_max
  )
}

test_that("binning gives one point per age band regardless of plot count", {
  obs <- tibble::tibble(age = c(1, 2, 3, 4, 5, 25), aboveground_c_mg_ha = c(10, 20, 30, 40, 50, 99))
  out <- growth_bin_observations(obs, bin = 20, probs = 0.5)

  expect_equal(nrow(out), 2L)
  expect_equal(out$n, c(5L, 1L))
  expect_equal(out$value, c(30, 99))
})

test_that("repeat visits to one location count once per bin", {
  ## Site A visited four times, sites B and C once each, all inside one bin.
  ## Visit-weighted the median is 10 (A dominates); location-weighted it is 30.
  obs <- tibble::tibble(
    site = c("A", "A", "A", "A", "B", "C"),
    age = c(1, 2, 3, 4, 5, 6),
    aboveground_c_mg_ha = c(10, 10, 10, 10, 30, 50)
  )

  expect_equal(growth_bin_observations(obs, bin = 20)$value, 10)
  expect_equal(growth_bin_observations(obs, bin = 20, site = "site")$value, 30)
})

test_that("site weighting reports locations rather than visits", {
  obs <- tibble::tibble(
    site = c("A", "A", "A", "B"),
    age = c(1, 2, 3, 4),
    aboveground_c_mg_ha = c(10, 20, 30, 40)
  )

  expect_equal(growth_bin_observations(obs, bin = 20)$n, 4L)
  expect_equal(growth_bin_observations(obs, bin = 20, site = "site")$n, 2L)
})

test_that("a location visited across two bins contributes to each", {
  obs <- tibble::tibble(site = c("A", "A"), age = c(5, 25), aboveground_c_mg_ha = c(10, 80))
  out <- growth_bin_observations(obs, bin = 20, site = "site")

  expect_equal(nrow(out), 2L)
  expect_equal(out$n, c(1L, 1L))
  expect_equal(out$value, c(10, 80))
})

test_that("a missing site column is an error, not a silent skip", {
  obs <- tibble::tibble(age = 1:3, aboveground_c_mg_ha = c(10, 20, 30))

  expect_snapshot(error = TRUE, growth_bin_observations(obs, site = "site"))
})

test_that("growth_reference_curves counts distinct locations when told about them", {
  ref <- dplyr::mutate(
    reference(),
    site = ifelse(.data$source == "Ground plots", rep_len(c("A", "A", "B"), dplyr::n()), NA)
  )

  plain <- growth_reference_curves(ref, window = c(20, 180))
  keyed <- growth_reference_curves(ref, window = c(20, 180), site = "site")

  expect_equal(plain$n_plots, 12L)
  expect_equal(keyed$n_plots, 2L)
})

test_that("the quantile selects where in the cloud the binned series sits", {
  obs <- tibble::tibble(age = rep(5, 5L), aboveground_c_mg_ha = c(10, 20, 30, 40, 50))

  expect_equal(growth_bin_observations(obs, probs = 0.5)$value, 30)
  expect_equal(growth_bin_observations(obs, probs = 0.9)$value, 46)
})

test_that("the derived window opens at the floor and closes before senescence", {
  w <- growth_auto_window(reference(), longevity = 400, age_floor = 20, senescence_frac = 0.45)

  expect_equal(w[[1L]], 20)
  ## 0.45 * 400 = 180, but the 95th percentile of plot ages is lower
  expect_lte(w[[2L]], 180)
  expect_true(all(w == round(w)))
})

test_that("longevity binds the window when observations run past it", {
  w <- growth_auto_window(reference(), longevity = 100, senescence_frac = 0.45)
  expect_equal(w[[2L]], 45)
})

test_that("a degenerate window is widened rather than silently scoring nothing", {
  w <- growth_auto_window(reference(), longevity = 10, age_floor = 20)
  expect_gt(w[[2L]], w[[1L]])
})

test_that("scoring.csv bounds override the derived window, blanks do not", {
  refs <- list(Aa = reference(), Bb = reference())
  core <- tibble::tibble(species = c("Aa", "Bb"), longevity = c(400, 400))
  scoring <- tibble::tibble(
    species = c("Aa", "Bb"),
    age_min = c(50, NA_real_),
    age_max = c(120, NA_real_)
  )
  w <- growth_fitting_windows(refs, core, scoring)

  expect_equal(w$mature_from[w$species == "Aa"], 50)
  expect_equal(w$mature_to[w$species == "Aa"], 120)
  expect_equal(w$window_source, c("growth_scoring.csv", "derived"))
  expect_equal(w$mature_from[w$species == "Bb"], 20)
})

test_that("the inflation factor recovers the biomass_max that lands a given plateau", {
  ## a curve reaching 135 when it ran at biomass_max 30000 has achieved 0.9
  inf <- growth_inflation_factor(achieved = 135, biomass_max = 30000, level = 200)

  expect_equal(inf$achieved_frac, 0.9)
  expect_equal(inf$inflation, 1 / 0.9)
  ## to plateau at 200 instead: 200 * 200 / 0.9
  expect_equal(inf$biomass_max_est, 200 * 200 / 0.9)
})

test_that("biomass_max_scale = 1 supports an extension whose output shares its parameter units", {
  inf <- growth_inflation_factor(270, biomass_max = 300, level = 300, biomass_max_scale = 1)

  expect_equal(inf$achieved_frac, 0.9)
  expect_equal(inf$biomass_max_est, 300 / 0.9)
})

test_that("shape scoring is invariant to the level the simulation ran at", {
  ref <- growth_reference_curves(reference(), window = c(20, 180))
  a <- growth_score_fit(sim_curve(plateau = 150, biomass_max = 30000), ref)
  b <- growth_score_fit(sim_curve(plateau = 300, biomass_max = 60000), ref)

  ## same shape, different level: identical shape error
  expect_equal(a$nrmse_shape, b$nrmse_shape)
  ## and the same recovered biomass_max
  expect_equal(a$biomass_max_est, b$biomass_max_est)
})

test_that("a closer shape scores lower than a further one", {
  ref <- growth_reference_curves(reference(), window = c(20, 180))
  near <- growth_score_fit(sim_curve(k = 0.02), ref)
  far <- growth_score_fit(sim_curve(k = 0.2), ref)

  expect_lt(near$nrmse_shape, far$nrmse_shape)
})

test_that("a zero weight drops a series from the ranking", {
  ref <- growth_reference_curves(reference(), window = c(20, 180))
  both <- growth_score_fit(sim_curve(), ref)
  sortie_only <- growth_score_fit(sim_curve(), ref, weights = c(sortie = 1, plots = 0))

  expect_equal(both$n_series, 2L)
  expect_equal(sortie_only$n_series, 1L)
  ## the dropped series is still REPORTED, just not ranked on
  expect_false(is.na(sortie_only$rmse_plots))
  expect_equal(sortie_only$nrmse_shape, both$rmse_sortie / both$level_used)
})

test_that("a nominated level source is a constraint, not a preference", {
  ref <- growth_reference_curves(reference(), window = c(20, 180))
  ## there is no TIPSY series here at all
  out <- growth_score_fit(sim_curve(), ref, level_source = "tipsy")

  expect_true(is.na(out$level_source_used))
  expect_equal(out$level_source_requested, "tipsy")
  expect_true(is.na(out$biomass_max_est))
})

test_that("plots below the advisory threshold are flagged but still scored", {
  ref <- growth_reference_curves(reference(), window = c(20, 180), min_plots = 50L)
  out <- growth_score_fit(sim_curve(), ref)

  expect_true(out$plots_sparse)
  expect_false(is.na(out$rmse_plots))
  expect_equal(out$n_plots, 12L)
  expect_gt(out$n_bins, 0L)
})

test_that("best candidates carry the current values and refuse to invent a fit", {
  ref <- growth_reference_curves(reference(), window = c(20, 180))
  scores <- dplyr::bind_rows(
    dplyr::bind_cols(
      tibble::tibble(species = "Aa", map_code = 1L, growth_shp = 0.9, mort_shp = 10),
      growth_score_fit(sim_curve(k = 0.02), ref)
    ),
    dplyr::bind_cols(
      tibble::tibble(species = "Aa", map_code = 2L, growth_shp = 0.6, mort_shp = 12),
      growth_score_fit(sim_curve(k = 0.2), ref)
    )
  ) |>
    growth_add_objective()

  cur <- tibble::tibble(
    species = "Aa",
    growth_shp = 0.8,
    mort_shp = 11,
    anpp_max = 2000,
    biomass_max = 30000
  )
  windows <- tibble::tibble(
    species = "Aa",
    mature_from = 20,
    mature_to = 180,
    window_source = "derived"
  )
  best <- growth_best_candidates(scores, cur, windows)

  expect_equal(nrow(best), 1L)
  expect_true(best$fitted)
  expect_equal(best$map_code, 1L)
  expect_equal(best$current_growth_shp, 0.8)
})

test_that("a species with no scorable series reports fitted = FALSE, not an argmin", {
  empty <- tibble::tibble(source = character(0), age = numeric(0), aboveground_c_mg_ha = numeric(0))
  ref <- growth_reference_curves(empty, window = c(20, 180))
  scores <- dplyr::bind_rows(
    dplyr::bind_cols(
      tibble::tibble(species = "Zz", map_code = 1L, growth_shp = 0.9, mort_shp = 10),
      growth_score_fit(sim_curve(), ref)
    ),
    dplyr::bind_cols(
      tibble::tibble(species = "Zz", map_code = 2L, growth_shp = 0.6, mort_shp = 12),
      growth_score_fit(sim_curve(k = 0.2), ref)
    )
  ) |>
    growth_add_objective()

  cur <- tibble::tibble(
    species = "Zz",
    growth_shp = 0.8,
    mort_shp = 11,
    anpp_max = 2000,
    biomass_max = 30000
  )
  windows <- tibble::tibble(
    species = "Zz",
    mature_from = 20,
    mature_to = 180,
    window_source = "derived"
  )
  best <- growth_best_candidates(scores, cur, windows)

  expect_equal(nrow(best), 1L)
  expect_false(best$fitted)
  expect_true(is.na(best$growth_shp))
  expect_true(is.na(best$biomass_max_est))
  ## the values in use still travel through untouched
  expect_equal(best$current_biomass_max, 30000)
})

test_that("climatic distance standardises so no variable dominates by unit", {
  climate <- tibble::tibble(MAT = c(2, 3, 4), MAP = c(700, 1000, 1300))
  ## MAT sd is 1, MAP sd is 300: without standardising, MAP would swamp MAT
  d <- growth_climatic_distance(climate, target = c(MAT = 3, MAP = 1000))

  expect_equal(d[[2L]], 0)
  expect_equal(d[[1L]], d[[3L]])
})

test_that("a supplied scale overrides the observed spread", {
  climate <- tibble::tibble(MAT = c(2, 4))
  d <- growth_climatic_distance(climate, c(MAT = 3), scale = c(MAT = 2))

  expect_equal(d, c(0.5, 0.5))
})

test_that("climatic weight decays with distance and respects the kernel", {
  d <- c(0, 0.5, 1, 2)

  expect_equal(growth_climatic_weight(d, bandwidth = 1)[[1L]], 1)
  expect_true(all(diff(growth_climatic_weight(d, bandwidth = 1)) < 0))
  ## tricube and uniform are zero past the bandwidth, gaussian never is
  expect_equal(growth_climatic_weight(d, bandwidth = 1, kernel = "tricube")[[4L]], 0)
  expect_equal(growth_climatic_weight(d, bandwidth = 1, kernel = "uniform"), c(1, 1, 1, 0))
  expect_gt(growth_climatic_weight(d, bandwidth = 1)[[4L]], 0)
})

test_that("weighting the bin moves the quantile toward the similar plots", {
  ## three plots in one bin; the low value is the climatically closest
  obs <- tibble::tibble(age = c(1, 2, 3), aboveground_c_mg_ha = c(10, 50, 90), w = c(1, 0.05, 0.05))

  expect_equal(growth_bin_observations(obs, bin = 20)$value, 50)
  expect_equal(growth_bin_observations(obs, bin = 20, weight = "w")$value, 10)
})

test_that("equal weights reproduce the unweighted ordering", {
  obs <- tibble::tibble(age = c(1, 2, 3), aboveground_c_mg_ha = c(10, 50, 90), w = c(1, 1, 1))

  expect_equal(growth_bin_observations(obs, bin = 20, weight = "w")$value, 50)
  expect_equal(growth_bin_observations(obs, bin = 20, weight = "w")$weight, 3)
})

test_that("site collapsing and climatic weighting compose", {
  ## site A visited three times and climatically distant; site B close
  obs <- tibble::tibble(
    site = c("A", "A", "A", "B"),
    age = c(1, 2, 3, 4),
    aboveground_c_mg_ha = c(90, 90, 90, 10),
    w = c(0.01, 0.01, 0.01, 1)
  )
  out <- growth_bin_observations(obs, bin = 20, site = "site", weight = "w")

  expect_equal(out$n, 2L)
  expect_equal(out$value, 10)
})

## A ranked factorial with every column growth_best_candidates() carries, so a
## surface can be given whatever shape a test needs without routing it through
## a simulated curve.
ranked <- function(n) {
  tibble::tibble(
    species = "Aa",
    map_code = seq_len(n),
    growth_shp = 0.9,
    mort_shp = 10,
    anpp_prop = 6.7,
    objective = "shape",
    objective_rmse = 0.1,
    rmse_sortie = 1,
    rmse_tipsy = NA_real_,
    rmse_plots = NA_real_,
    n_series = 1L,
    n_plots = 10L,
    n_bins = 5L,
    plots_sparse = FALSE,
    achieved = 150,
    achieved_frac = 1,
    level_used = 150,
    level_source_requested = NA_character_,
    level_source_used = "sortie",
    biomass_max_est = 30000,
    anpp_max_est = 2000,
    biomass_at_end = 150
  )
}

current <- tibble::tibble(
  species = "Aa",
  growth_shp = 0.8,
  mort_shp = 11,
  anpp_max = 2000,
  biomass_max = 30000
)

window_aa <- tibble::tibble(
  species = "Aa",
  mature_from = 20,
  mature_to = 180,
  window_source = "derived"
)

test_that("identifiability tells a determined parameter from a free one", {
  grid <- expand.grid(growth_shp = seq(0.5, 0.9, by = 0.1), mort_shp = c(5, 10, 15, 20, 25))
  ## Error depends on growth shape alone, so mortality shape is unconstrained:
  ## every mortality value appears among the best candidates at the same error.
  scores <- tibble::tibble(
    species = "Aa",
    growth_shp = grid$growth_shp,
    mort_shp = grid$mort_shp,
    objective_rmse = (grid$growth_shp - 0.7)^2 + 1e-3
  )

  out <- growth_identifiability(scores, params = c("growth_shp", "mort_shp"), top_frac = 0.2)

  expect_equal(out$parameter, c("growth_shp", "mort_shp"))
  expect_equal(out$identified, c(TRUE, FALSE))
  expect_equal(out$grid_frac, c(0.2, 1))
  expect_equal(out$best[[1L]], 0.7)
  expect_equal(out$top_min[[2L]], 5)
  expect_equal(out$top_max[[2L]], 25)
})

test_that("identifiability flags an argmin sitting on the edge of the sweep", {
  scores <- tibble::tibble(
    species = "Aa",
    growth_shp = c(0.5, 0.6, 0.7, 0.8, 0.9),
    objective_rmse = c(0.9, 0.7, 0.5, 0.3, 0.1)
  )

  out <- growth_identifiability(scores, params = "growth_shp", top_frac = 0.2)

  expect_equal(out$best, 0.9)
  expect_equal(out$boundary, "max")
})

test_that("the level band spans the candidates that cannot be told apart", {
  scores <- ranked(3) |>
    dplyr::mutate(
      objective_rmse = c(0.100, 0.101, 0.500),
      biomass_max_est = c(30000, 45000, 99000),
      anpp_max_est = c(2000, 3000, 6600)
    )

  best <- growth_best_candidates(scores, current, window_aa, top_frac = 0.5)

  ## Two candidates are within the band, the third is not.
  expect_equal(best$n_indistinct, 2L)
  expect_equal(best$biomass_max_est, 30000)
  expect_equal(best$biomass_max_lo, 30000)
  expect_equal(best$biomass_max_hi, 45000)
  expect_equal(best$anpp_max_hi, 3000)
  expect_false(best$level_extrapolated)
})

test_that("a level recovered from a curve that never plateaued warns", {
  scores <- ranked(2) |>
    dplyr::mutate(
      objective_rmse = c(0.1, 0.2),
      ## Winner stopped at half its own asymptote, so its level is doubled.
      achieved_frac = c(0.5, 1)
    )

  expect_snapshot(best <- growth_best_candidates(scores, current, window_aa))
  expect_true(best$level_extrapolated)
  expect_equal(best$level_leverage, 2)
})

test_that("the promotable parameters stay the first four columns after species", {
  best <- growth_best_candidates(ranked(2), current, window_aa)

  expect_equal(
    names(best)[1:5],
    c("species", "growth_shp", "mort_shp", "anpp_max_est", "biomass_max_est")
  )
})

## ---- VDYP as a series in its own right -----------------------------------------------------------
## VDYP used to have to travel in the TIPSY slot, which made `rmse_tipsy` a VDYP residual and left a
## reader unable to tell from the outputs which model produced the number. TIPSY projects MANAGED
## stands and VDYP unmanaged natural ones, so they must not share a slot.

vdyp_reference <- function() {
  ages <- 1:300
  dplyr::bind_rows(
    reference(),
    tibble::tibble(
      source = "VDYP",
      age = ages,
      ## deliberately a different curve from the SORTIE one, so a series mix-up shows up
      aboveground_c_mg_ha = 240 * (1 - exp(-0.01 * ages))
    )
  )
}

test_that("a VDYP series is carried and its level read from the whole curve", {
  ref <- growth_reference_curves(vdyp_reference(), window = c(20, 180), use_vdyp = TRUE)

  expect_true("vdyp" %in% names(ref$series))
  expect_false(all(is.na(ref$series$vdyp)))
  ## the modelled level is the maximum of the WHOLE curve, not just inside the window
  expect_equal(unname(ref$levels[["vdyp"]]), 240 * (1 - exp(-0.01 * 300)))
})

test_that("VDYP is scored separately from TIPSY rather than sharing its slot", {
  ref <- growth_reference_curves(vdyp_reference(), window = c(20, 180), use_vdyp = TRUE)
  fit <- growth_score_fit(sim_curve(), ref)

  expect_false(is.na(fit$rmse_vdyp))
  ## no TIPSY curve was supplied, so its residual must stay empty
  expect_true(is.na(fit$rmse_tipsy))
})

test_that("VDYP stays out of the ranking unless it is switched on", {
  off <- growth_reference_curves(vdyp_reference(), window = c(20, 180))
  expect_false("vdyp" %in% names(off$series))
})

test_that("a scoring file written before weight_vdyp still reads", {
  path <- withr::local_tempfile(fileext = ".csv")
  utils::write.csv(
    data.frame(
      species = "Pinu_con",
      age_min = NA,
      age_max = NA,
      age_bin = 20L,
      plot_quantile = 0.5,
      plots_warn_below = 50L,
      weight_sortie = 1,
      weight_tipsy = 0,
      weight_plots = 1,
      level_source = "plots",
      note = ""
    ),
    path,
    row.names = FALSE
  )

  scoring <- read_growth_scoring(path)
  expect_true("weight_vdyp" %in% names(scoring))
  ## absent means absent, and the default keeps it out of the ranking
  expect_true(is.na(scoring$weight_vdyp))
  expect_equal(growth_scoring_for(scoring, "Pinu_con")$weight_vdyp, 0)
})
