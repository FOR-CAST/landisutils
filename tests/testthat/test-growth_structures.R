structure_curves <- function() {
  ## Two cells that BOTH carry map_code 1, in different batches -- the shape
  ## `growth_calibration_partition()` produces.
  tibble::tibble(
    batch = c(1L, 1L, 1L, 1L, 2L, 2L),
    map_code = 1L,
    species = c("Aa", "Bb", "Aa", "Bb", "Aa", "Aa"),
    cohort_age = c(10L, 30L, 10L, 30L, 50L, 70L),
    age = c(1L, 1L, 2L, 2L, 1L, 1L),
    aboveground_c_mg_ha = c(5, 5, 6, 6, 9, 9)
  )
}

test_that("growth_structure_cell_curves() reduces a repeated trajectory to one row per cell", {
  cells <- growth_structure_cell_curves(structure_curves())

  expect_equal(nrow(cells), 3L)
  expect_equal(cells$aboveground_c_mg_ha, c(5, 6, 9))
  expect_equal(cells$n_cohorts, c(2L, 2L, 2L))
})

test_that("growth_structure_cell_curves() keys on (batch, map_code), not map_code alone", {
  cells <- growth_structure_cell_curves(structure_curves())

  ## Merging the two batches would give one cell of 4 cohorts labelled Aa+Aa+Aa+Bb.
  expect_equal(sort(unique(cells$composition)), c("Aa+Aa", "Aa+Bb"))
  expect_equal(max(cells$n_cohorts), 2L)
})

test_that("growth_structure_cell_curves() keeps a repeated species in the composition", {
  cells <- growth_structure_cell_curves(structure_curves())

  ## Two cohorts of one species is an age structure, not a monoculture.
  expect_equal(cells$composition[cells$batch == 2L], "Aa+Aa")
})

test_that("growth_structure_cell_curves() requires the batch column", {
  expect_snapshot(
    error = TRUE,
    growth_structure_cell_curves(dplyr::select(structure_curves(), -"batch"))
  )
})

test_that("growth_structure_summary() drops compositions with too few cells", {
  cells <- growth_structure_cell_curves(structure_curves())

  expect_equal(nrow(growth_structure_summary(cells, min_cells = 25L)), 0L)
  expect_equal(
    sort(unique(growth_structure_summary(cells, min_cells = 1L)$composition)),
    c("Aa+Aa", "Aa+Bb")
  )
})

test_that("growth_structure_summary() reports the band across cells", {
  cells <- growth_structure_cell_curves(structure_curves())
  s <- growth_structure_summary(cells, min_cells = 1L)

  expect_named(
    s,
    c("composition", "n_cohorts", "age", "n_cells", "lower", "median", "upper"),
    ignore.order = TRUE
  )
  expect_equal(s$median[s$composition == "Aa+Aa"], 9)
})

test_that("growth_structure_cohort_table() reports cohort ages only when given the curves", {
  curves <- structure_curves()
  cells <- growth_structure_cell_curves(curves)

  without <- growth_structure_cohort_table(cells)
  with <- growth_structure_cohort_table(cells, curves)

  expect_false("cohort_age_min" %in% names(without))
  expect_equal(with$cohort_age_min[with$composition == "Aa+Bb"], 10L)
  expect_equal(with$cohort_age_max[with$composition == "Aa+Aa"], 70L)
})

test_that("growth_structure_cohort_table() counts cells across batches", {
  cells <- growth_structure_cell_curves(structure_curves())
  tbl <- growth_structure_cohort_table(cells)

  expect_equal(sum(tbl$n_cells), 2L)
  expect_equal(tbl$age_start, c(1L, 1L))
})

test_that("read_landscape_cohort_structures() returns the documented schema", {
  p <- withr::local_tempfile(fileext = ".csv")
  utils::write.csv(
    data.frame(
      structure_id = "S00001",
      n_cohorts = 2,
      species = c("Aa", "Bb"),
      cohort_age = c(10, 30),
      n_communities = 7
    ),
    p,
    row.names = FALSE
  )
  s <- read_landscape_cohort_structures(p)

  expect_named(s, c("structure_id", "n_cohorts", "species", "cohort_age", "n_communities"))
  expect_type(s$cohort_age, "integer")
  expect_type(s$structure_id, "character")
})

test_that("plot_growth_structures() builds, and is NULL for an absent species", {
  skip_if_not_installed("ggplot2")
  cells <- growth_structure_cell_curves(structure_curves())
  s <- growth_structure_summary(cells, min_cells = 1L)

  p <- plot_growth_structures(s, "Aa", x_max = 10)
  expect_no_error(ggplot2::ggplot_build(p))
  expect_null(plot_growth_structures(s, "Zz"))
})

test_that("plot_growth_structures() separates an age structure from a mixture", {
  skip_if_not_installed("ggplot2")
  cells <- growth_structure_cell_curves(structure_curves())
  s <- growth_structure_summary(cells, min_cells = 1L)

  p <- plot_growth_structures(s, "Aa", x_max = 10)
  ## The legend must distinguish the three kinds, not collapse them.
  expect_setequal(
    unique(ggplot2::ggplot_build(p)$plot$data$kind),
    c("one species, multiple cohorts", "multiple species")
  )
})


test_that("growth_structure_cell_curves() sums per-cohort biomass instead of de-duplicating it", {
  ## Same cell and age, three cohorts, two of which happen to carry equal biomass.
  curves <- tibble::tibble(
    batch = 1L,
    map_code = 1L,
    species = c("Aa", "Aa", "Aa"),
    cohort_age = c(10L, 30L, 50L),
    age = 1L,
    aboveground_c_mg_ha = c(2, 2, 5)
  )

  summed <- growth_structure_cell_curves(curves, biomass = "cohort")
  expect_equal(nrow(summed), 1L)
  expect_equal(summed$aboveground_c_mg_ha, 9)
  expect_equal(summed$n_cohorts, 3L)

  ## The default treats the column as a whole-cell total already, so it keeps one
  ## row per DISTINCT value -- 2 and 5, which is neither a total nor a trajectory.
  deduped <- growth_structure_cell_curves(curves)
  expect_equal(nrow(deduped), 2L)
})

test_that("plot_growth_structures() does not call a many-cohort cell two cohorts", {
  skip_if_not_installed("ggplot2")
  curves <- tibble::tibble(
    batch = 1L,
    map_code = 1L,
    species = "Aa",
    cohort_age = seq(10L, 160L, by = 10L),
    age = 1L,
    aboveground_c_mg_ha = 1
  )
  cells <- growth_structure_cell_curves(curves, biomass = "cohort")
  s <- growth_structure_summary(cells, min_cells = 1L)
  p <- plot_growth_structures(s, "Aa", x_max = 10)

  expect_equal(unique(ggplot2::ggplot_build(p)$plot$data$kind), "one species, multiple cohorts")
  expect_equal(unique(cells$n_cohorts), 16L)
})
