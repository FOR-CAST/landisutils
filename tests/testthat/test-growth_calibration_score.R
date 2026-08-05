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
