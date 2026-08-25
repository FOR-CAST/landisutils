## A pixel group's biomass is its stand total per species. simplifyCohorts() merges many pixel groups
## into one community, and what has to survive that merge is the total -- not the mean of the cohorts
## that make it up. Until 0.0.128 each age class received its own copy of the mean cohort biomass, so
## a community's biomass grew with the number of age classes it happened to pool.

make_cohorts <- function(ages, B, pixelGroup = 1L, species = "Pinu_con", ecoregion = "e1") {
  data.table::data.table(
    pixelGroup = as.integer(pixelGroup),
    speciesCode = species,
    ecoregionGroup = ecoregion,
    age = as.integer(ages),
    B = as.numeric(B)
  )
}

## a 1-cell map is enough: the map is reclassified, not used in the biomass arithmetic
dummy_map <- function(pixelGroups) {
  r <- terra::rast(nrows = 1, ncols = length(pixelGroups), vals = as.integer(pixelGroups))
  terra::crs(r) <- "EPSG:3005"
  r
}

test_that("simplifyCohorts() conserves each species' stand biomass", {
  cd <- make_cohorts(ages = c(10, 30, 50, 70), B = c(20, 30, 40, 50))
  out <- simplifyCohorts(cd, dummy_map(1L), ageBin = 20)[[1]]

  expect_equal(sum(unique(out)$B), sum(cd$B), tolerance = 0.01)
})

test_that("conserved biomass does not scale with the number of age classes", {
  ## same stand total, split over two age classes and then over six
  few <- make_cohorts(ages = c(10, 90), B = c(60, 60))
  many <- make_cohorts(ages = seq(10, 210, by = 40), B = rep(20, 6))

  b_few <- sum(unique(simplifyCohorts(few, dummy_map(1L), ageBin = 20)[[1]])$B)
  b_many <- sum(unique(simplifyCohorts(many, dummy_map(1L), ageBin = 20)[[1]])$B)

  expect_equal(b_few, 120, tolerance = 0.01)
  expect_equal(b_many, 120, tolerance = 0.01)
})

test_that("merged pixel groups take the mean of their stand totals", {
  ## two pixel groups with the same species community, so they merge; totals 100 and 300
  cd <- rbind(
    make_cohorts(ages = c(10, 50), B = c(40, 60), pixelGroup = 1L),
    make_cohorts(ages = c(10, 50), B = c(120, 180), pixelGroup = 2L)
  )
  out <- simplifyCohorts(cd, dummy_map(1:2), ageBin = 20)[[1]]

  expect_equal(length(unique(out$pixelGroup)), 1L)
  expect_equal(sum(unique(out)$B), 200, tolerance = 0.01)
})

test_that("older age classes still carry more biomass than younger ones", {
  cd <- make_cohorts(ages = c(10, 50, 90), B = c(30, 30, 30))
  out <- unique(simplifyCohorts(cd, dummy_map(1L), ageBin = 20)[[1]])
  data.table::setorder(out, age)

  expect_equal(order(out$B), seq_len(nrow(out)))
})

test_that("a species with one age class keeps its whole total", {
  cd <- make_cohorts(ages = 50, B = 175)
  out <- unique(simplifyCohorts(cd, dummy_map(1L), ageBin = 20)[[1]])

  expect_equal(sum(out$B), 175, tolerance = 0.01)
})
