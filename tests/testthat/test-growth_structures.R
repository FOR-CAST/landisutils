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
  expect_equal(sort(unique(cells$composition)), c("Aa x2", "Bb+Aa"))
  expect_equal(max(cells$n_cohorts), 2L)
})

test_that("growth_structure_cell_curves() keeps a repeated species in the composition", {
  cells <- growth_structure_cell_curves(structure_curves())

  ## Two cohorts of one species is an age structure, not a monoculture.
  expect_equal(cells$composition[cells$batch == 2L], "Aa x2")
  expect_equal(cells$species_set[cells$batch == 2L], "Aa")
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
    c("Aa x2", "Bb+Aa")
  )
})

test_that("growth_structure_summary() reports the band across cells", {
  cells <- growth_structure_cell_curves(structure_curves())
  s <- growth_structure_summary(cells, min_cells = 1L)

  expect_named(
    s,
    c(
      "composition",
      "species_set",
      "oldest_species",
      "n_cohorts",
      "age",
      "n_cells",
      "lower",
      "median",
      "upper"
    ),
    ignore.order = TRUE
  )
  expect_equal(s$median[s$composition == "Aa x2"], 9)
})

test_that("growth_structure_cohort_table() reports cohort ages only when given the curves", {
  curves <- structure_curves()
  cells <- growth_structure_cell_curves(curves)

  without <- growth_structure_cohort_table(cells)
  with <- growth_structure_cohort_table(cells, curves)

  expect_false("cohort_age_min" %in% names(without))
  expect_equal(with$cohort_age_min[with$composition == "Bb+Aa"], 10L)
  expect_equal(with$cohort_age_max[with$composition == "Aa x2"], 70L)
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

  ## Each structure now appears in exactly one species' figure, so the two
  ## kinds are split across them: `Aa x2` under Aa, `Bb+Aa` under Bb.
  kinds <- unlist(lapply(c("Aa", "Bb"), function(sp) {
    unique(ggplot2::ggplot_build(plot_growth_structures(s, sp, x_max = 10))$plot$data$kind)
  }))
  ## The legend must distinguish the kinds, not collapse them.
  expect_setequal(kinds, c("one species, multiple cohorts", "multiple species"))
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


test_that("a many-cohort composition gets a counted label, not a repeated one", {
  curves <- tibble::tibble(
    batch = 1L,
    map_code = 1L,
    species = rep(c("Aa", "Bb"), each = 8L),
    cohort_age = rep(seq(10L, 80L, by = 10L), times = 2L),
    age = 1L,
    aboveground_c_mg_ha = 1
  )
  cells <- growth_structure_cell_curves(curves, biomass = "cohort")

  expect_equal(cells$composition, "Aa x8+Bb x8")
  expect_equal(cells$species_set, "Aa+Bb")
  ## The repeated form would be 71 characters for a cell of 16.
  expect_lt(nchar(cells$composition), 20L)
})

test_that("plot_growth_structures() caps panels at the best-evidenced compositions", {
  skip_if_not_installed("ggplot2")
  ## Four compositions with distinct cell counts.
  s <- tibble::tibble(
    composition = rep(c("Aa x2", "Aa+Bb", "Aa+Cc", "Aa+Dd"), each = 2L),
    species_set = rep(c("Aa", "Aa+Bb", "Aa+Cc", "Aa+Dd"), each = 2L),
    n_cohorts = 2L,
    age = rep(c(1L, 2L), times = 4L),
    n_cells = rep(c(40L, 30L, 20L, 10L), each = 2L),
    lower = 1,
    median = 2,
    upper = 3
  )

  all_p <- plot_growth_structures(s, "Aa", x_max = NULL)
  expect_equal(dplyr::n_distinct(ggplot2::ggplot_build(all_p)$plot$data$composition), 4L)

  capped <- plot_growth_structures(s, "Aa", x_max = NULL, max_panels = 2L)
  kept <- ggplot2::ggplot_build(capped)$plot$data
  expect_setequal(unique(kept$composition), c("Aa x2", "Aa+Bb"))
  expect_match(capped$labels$subtitle, "2 compositions with the most cells, of 4")
})

test_that("plot_growth_structures() says nothing about dropping when nothing is dropped", {
  skip_if_not_installed("ggplot2")
  cells <- growth_structure_cell_curves(structure_curves())
  s <- growth_structure_summary(cells, min_cells = 1L)

  p <- plot_growth_structures(s, "Aa", x_max = NULL, max_panels = 99L)
  expect_no_match(p$labels$subtitle, "compositions with the most cells")
})

test_that("growth_structure_cell_curves() carries the cell's oldest starting cohort", {
  cells <- growth_structure_cell_curves(structure_curves())

  ## batch 1 holds cohorts aged 10 and 30; batch 2 holds 50 and 70.
  expect_equal(cells$start_age[cells$batch == 1L], c(30L, 30L))
  expect_equal(cells$start_age[cells$batch == 2L], 70L)
})

test_that("growth_structure_summary() summarises within starting-age classes when asked", {
  cells <- growth_structure_cell_curves(structure_curves())
  s <- growth_structure_summary(cells, min_cells = 1L, start_age_breaks = c(0, 40, Inf))

  expect_true("start_class" %in% names(s))
  ## The two batches fall either side of the break, so they never share a row.
  expect_equal(dplyr::n_distinct(s$start_class), 2L)
  expect_false(any(duplicated(s[, c("composition", "start_class", "age")])))
})

test_that("growth_structure_summary() pools over starting age by default", {
  cells <- growth_structure_cell_curves(structure_curves())

  ## The historical shape: no extra column, and no extra grouping.
  expect_false("start_class" %in% names(growth_structure_summary(cells, min_cells = 1L)))
})

test_that("growth_structure_summary() rejects breaks when start_age is absent", {
  cells <- dplyr::select(growth_structure_cell_curves(structure_curves()), -"start_age")

  expect_error(
    growth_structure_summary(cells, min_cells = 1L, start_age_breaks = c(0, 40, Inf)),
    "needs a `start_age` column"
  )
})

test_that("plot_growth_structures() colours by starting-age class when the summary carries one", {
  skip_if_not_installed("ggplot2")
  cells <- growth_structure_cell_curves(structure_curves())
  s <- growth_structure_summary(cells, min_cells = 1L, start_age_breaks = c(0, 40, Inf))

  p <- plot_growth_structures(s, "Aa", x_max = NULL)
  mapped <- rlang::as_label(p$layers[[1]]$mapping$colour)
  expect_match(mapped, "start_class")
  ## Within a panel the composition is constant, so `kind` would say nothing;
  ## and the band is suppressed rather than drawn once per class.
  expect_false(any(vapply(p$layers, function(l) inherits(l$geom, "GeomRibbon"), logical(1))))
})

test_that("growth_structure_cell_curves() orders the composition oldest cohort first", {
  cells <- growth_structure_cell_curves(structure_curves())

  ## The mixed cell is Aa at 10 and Bb at 30, so Bb leads and `Aa+Bb` would be
  ## a different stand: the alphabetical label collapsed the two.
  expect_equal(unique(cells$composition[cells$batch == 1L]), "Bb+Aa")
  expect_equal(unique(cells$oldest_species[cells$batch == 1L]), "Bb")
  expect_equal(unique(cells$oldest_species[cells$batch == 2L]), "Aa")
})

test_that("growth_structure_cell_curves() breaks ties in the composition alphabetically", {
  curves <- structure_curves()
  ## Equal starting ages: order must not depend on row order.
  curves$cohort_age[curves$batch == 1L] <- 30L
  forward <- growth_structure_cell_curves(curves)
  reversed <- growth_structure_cell_curves(curves[rev(seq_len(nrow(curves))), ])

  expect_equal(unique(forward$composition[forward$batch == 1L]), "Aa+Bb")
  expect_equal(
    unique(reversed$composition[reversed$batch == 1L]),
    unique(forward$composition[forward$batch == 1L])
  )
})

test_that("plot_growth_structures() draws a structure under its oldest species only", {
  skip_if_not_installed("ggplot2")
  cells <- growth_structure_cell_curves(structure_curves())
  s <- growth_structure_summary(cells, min_cells = 1L)

  ## `Bb+Aa` contains Aa, but Bb is older, so it belongs to Bb's figure alone.
  ## Selecting on presence drew it in both, as the same panel.
  expect_equal(
    unique(
      ggplot2::ggplot_build(plot_growth_structures(s, "Aa", x_max = 10))$plot$data$composition
    ),
    "Aa x2"
  )
  expect_equal(
    unique(
      ggplot2::ggplot_build(plot_growth_structures(s, "Bb", x_max = 10))$plot$data$composition
    ),
    "Bb+Aa"
  )
})

test_that("plot_growth_structures() falls back to presence without an oldest_species column", {
  skip_if_not_installed("ggplot2")
  cells <- growth_structure_cell_curves(structure_curves())
  s <- growth_structure_summary(cells, min_cells = 1L)
  s$oldest_species <- NULL

  ## A summary stored before the column existed still plots, the old way: Aa is
  ## present in both compositions, so both are drawn.
  drawn <- unique(
    ggplot2::ggplot_build(plot_growth_structures(s, "Aa", x_max = 10))$plot$data$composition
  )
  expect_setequal(drawn, c("Aa x2", "Bb+Aa"))
})
