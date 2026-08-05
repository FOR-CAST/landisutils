params <- function() {
  tibble::tibble(
    species = c("Aa", "Bb"),
    growth_shp = c(0.9, 0.6),
    mort_shp = c(10, 19),
    anpp_max = c(2000, 1000),
    biomass_max = c(30000, 48000)
  )
}

test_that("a design with no grid is one cell per species at its current values", {
  d <- growth_calibration_design(params())

  expect_equal(nrow(d), 2L)
  expect_equal(d$landis_species, c("Aa", "Bb"))
  expect_equal(d$biomass_max, c(30000, 48000))
  expect_equal(d$cohort_age, c(1L, 1L))
})

test_that("shape candidates become pseudo-species and level candidates become ecoregions", {
  grid <- tibble::tibble(
    species = rep(c("Aa", "Bb"), each = 2L),
    growth_shp = c(0.8, 0.9, 0.5, 0.6),
    mort_shp = c(10, 12, 19, 21),
    anpp_max = c(2000, 2400, 1000, 1200),
    biomass_max = c(30000, 30000, 48000, 48000)
  )
  d <- growth_calibration_design(params(), grid = grid)

  ## 2 species x 2 growth x 2 mortality = 8 pseudo-species, 2 ecoregions
  expect_equal(dplyr::n_distinct(d$landis_species), 8L)
  expect_equal(dplyr::n_distinct(d$ecoregion), 2L)
  expect_equal(nrow(d), 16L)
  expect_equal(dplyr::n_distinct(d$map_code), 16L)
  ## a pseudo-species maps back to exactly one real species
  expect_equal(d |> dplyr::distinct(landis_species, species) |> nrow(), 8L)
})

test_that("an oversized design errors rather than building an enormous landscape", {
  grid <- tibble::tibble(
    species = "Aa",
    growth_shp = seq(0.1, 0.99, length.out = 80),
    mort_shp = seq(5, 25, length.out = 80)
  )
  expect_snapshot(growth_calibration_design(params(), grid = grid, max_cells = 100L), error = TRUE)
})

test_that("growth_pseudo_species_name collapses a single combination to the plain code", {
  expect_equal(growth_pseudo_species_name(c("Aa", "Aa"), c(1L, 1L)), c("Aa", "Aa"))
  expect_equal(growth_pseudo_species_name(c("Aa", "Aa"), c(1L, 2L)), c("Aa_c01", "Aa_c02"))
})

test_that("the ratio grid pins biomass_max and converts anpp_prop to absolute ANPP", {
  grid <- tibble::tibble(
    species = c("Aa", "Aa", "Bb"),
    growth_shp = c(0.8, 0.9, 0.5),
    mort_shp = c(10, 12, 19),
    anpp_prop = c(5, 10, 2)
  )
  out <- growth_factorial_ratio_grid(grid, params())

  ## biomass_max only ever takes each species' current value
  bmax <- out |> dplyr::filter(!is.na(biomass_max)) |> dplyr::distinct(species, biomass_max)
  expect_equal(bmax$biomass_max[bmax$species == "Aa"], 30000)
  expect_equal(bmax$biomass_max[bmax$species == "Bb"], 48000)

  ## 10% of 30000 is 3000
  anpp <- out |> dplyr::filter(species == "Aa", !is.na(anpp_max)) |> dplyr::pull(anpp_max)
  expect_setequal(anpp, c(1500, 3000))

  ## ForCS parses both as integers
  expect_true(all(out$anpp_max[!is.na(out$anpp_max)] %% 1 == 0))
})

test_that("a species absent from the ratio grid keeps the ratio it already uses", {
  grid <- tibble::tibble(species = "Aa", growth_shp = 0.8, mort_shp = 10, anpp_prop = 5)
  out <- growth_factorial_ratio_grid(grid, params())

  ## Bb currently runs 1000 / 48000
  expect_equal(out$anpp_max[out$species == "Bb" & !is.na(out$anpp_max)], 1000)
})

test_that("partitioning cuts on cell boundaries and renumbers within each batch", {
  d <- tibble::tibble(
    map_code = rep(1:6, each = 2L),
    ecoregion = rep(1L, 12L),
    species = rep(c("Aa", "Bb"), 6L),
    cohort_age = rep(c(1L, 50L), 6L)
  )
  out <- growth_calibration_partition(d, max_cells_per_batch = 2L)

  expect_equal(dplyr::n_distinct(out$batch), 3L)
  ## every cohort survives the partition
  expect_equal(nrow(out), nrow(d))
  ## a cell's cohorts always stay together: each (batch, map_code) keeps both
  expect_equal(
    out |>
      dplyr::summarise(n = dplyr::n(), .by = c("batch", "map_code")) |>
      dplyr::pull(n) |>
      unique(),
    2L
  )
  ## map_code is renumbered WITHIN each batch, so it restarts at 1 every time
  expect_equal(
    out |> dplyr::summarise(m = min(map_code), .by = "batch") |> dplyr::pull(m),
    c(1L, 1L, 1L)
  )
  expect_equal(dplyr::n_distinct(out$map_code), 2L)
})

test_that("the work root honours the environment variable ahead of any default", {
  withr::with_envvar(c(LANDIS_GROWTH_SCRATCH = "/tmp/somewhere-explicit"), {
    expect_equal(growth_calibration_work_root(), "/tmp/somewhere-explicit")
  })
})
