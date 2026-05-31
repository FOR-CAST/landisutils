## helpers ------------------------------------------------------------------------------------

## Build a minimal log_BiomassC-style data.table with known values.
## Mimics ForCS output: per-cohort rows, header has spaces after commas.
make_biomass_c_csv <- function(path, times = c(0L, 100L)) {
  ## two cells (r1c1, r1c2), two species (Hw, Sx), one cohort each, two timesteps
  rows <- data.frame(
    Time = rep(times, each = 4L),
    row = rep(c(1L, 1L, 2L, 2L), length(times)),
    column = rep(c(1L, 2L, 1L, 2L), length(times)),
    ecoregion = 1L,
    species = rep(c("Hw", "Sx", "Hw", "Sx"), length(times)),
    Age = 100L,
    Wood = c(100, 50, 80, 120, 200, 60, 160, 100)[seq_along(rep(times, each = 4L))],
    Leaf = 10,
    CrsRoot = 5,
    FineRoot = 2
  )
  ## write with spaces after commas in header to match real ForCS output
  con <- file(path, open = "w")
  writeLines("Time, row, column, ecoregion, species, Age, Wood, Leaf, CrsRoot, FineRoot", con)
  close(con)
  write.table(rows, file = path, sep = ",", row.names = FALSE, col.names = FALSE, append = TRUE)
  invisible(path)
}

## Build a minimal per-cell biomass data.table (output of read_biomass_c_snapshots).
make_biomass_dt <- function(times = c(0L, 100L), scenario = "s1", n_reps = 2L) {
  data.table::rbindlist(lapply(seq_len(n_reps), function(r) {
    data.table::rbindlist(lapply(times, function(t) {
      data.table::data.table(
        scenario = scenario,
        replicate = paste0("rep0", r),
        Time = t,
        row = c(1L, 1L, 2L, 2L),
        column = c(1L, 2L, 1L, 2L),
        ecoregion = 1L,
        species = c("Hw", "Sx", "Hw", "Sx"),
        ## Hw dominates cells (1,1) and (2,2); Sx dominates (1,2) and (2,1) at t=0;
        ## all cells switch to Hw-dominant by t=100
        biomass = if (t == 0L) c(5, 2, 3, 4) else c(8, 1, 7, 2)
      )
    }))
  }))
}

## ---- read_biomass_c_snapshots ---------------------------------------------------------------

testthat::test_that("read_biomass_c_snapshots reads snapshot times from CSV", {
  tmp <- withr::local_tempdir("test_biomass_c_")

  csv_path <- file.path(tmp, "log_BiomassC.csv")
  make_biomass_c_csv(csv_path, times = c(0L, 50L, 100L))

  result <- read_biomass_c_snapshots(
    paths = csv_path,
    times = c(0L, 100L),
    run_name = "test_scenario"
  )

  ## only requested times are returned
  testthat::expect_equal(sort(unique(result$Time)), c(0L, 100L))

  ## correct columns
  testthat::expect_named(
    result,
    c("scenario", "replicate", "Time", "row", "column", "ecoregion", "species", "biomass"),
    ignore.order = TRUE
  )

  ## units: g C/m² → Mg C/ha (× 0.01); Wood+Leaf+CrsRoot+FineRoot summed per cohort
  ## first row at t=0: Wood=100, Leaf=10, CrsRoot=5, FineRoot=2 → (100+10+5+2)*0.01 = 1.17
  r0_hw_r1c1 <- result[
    result$Time == 0 & result$row == 1 & result$column == 1 & result$species == "Hw",
    "biomass"
  ][[1]]
  testthat::expect_equal(r0_hw_r1c1, (100 + 10 + 5 + 2) * 0.01)

  ## scenario and replicate labels are attached
  testthat::expect_true(all(result$scenario == "test_scenario"))
  testthat::expect_true(all(result$replicate == basename(tmp)))
})

testthat::test_that("read_biomass_c_snapshots handles multiple replicate paths", {
  tmp <- withr::local_tempdir("test_biomass_c_multi_")

  rep1 <- file.path(tmp, "rep01", "log_BiomassC.csv")
  rep2 <- file.path(tmp, "rep02", "log_BiomassC.csv")
  dir.create(dirname(rep1))
  dir.create(dirname(rep2))
  make_biomass_c_csv(rep1)
  make_biomass_c_csv(rep2)

  result <- read_biomass_c_snapshots(paths = c(rep1, rep2), times = 0L, run_name = "s1")

  testthat::expect_true(all(c("rep01", "rep02") %in% result$replicate))
})

## ---- read_biomass_output_rasters ------------------------------------------------------------

testthat::test_that("read_biomass_output_rasters reads per-species tifs at snapshot times", {
  tmp <- withr::local_tempdir("test_biomass_rast_")

  out_dir <- file.path(tmp, "rep01", "outputs", "biomass")
  dir.create(out_dir, recursive = TRUE)
  rep_dir <- file.path(tmp, "rep01")

  ## write tiny 2x2 rasters for two species and two times
  r <- terra::rast(nrows = 2L, ncols = 2L, vals = c(100, 50, 80, 120))
  for (spp in c("Hw", "Sx")) {
    for (t in c(0L, 100L)) {
      terra::writeRaster(
        r,
        file.path(out_dir, paste0("biomass-", spp, "-", t, ".tif")),
        overwrite = TRUE
      )
    }
  }

  result <- read_biomass_output_rasters(
    dirs = rep_dir,
    times = c(0L, 100L),
    species = c("Hw", "Sx"),
    run_name = "s1"
  )

  testthat::expect_named(
    result,
    c("scenario", "replicate", "Time", "row", "column", "species", "biomass"),
    ignore.order = TRUE
  )
  testthat::expect_equal(sort(unique(result$Time)), c(0L, 100L))
  testthat::expect_equal(sort(unique(result$species)), c("Hw", "Sx"))

  ## units: raw raster values × 0.01
  testthat::expect_equal(sort(unique(result$biomass)), sort(c(100, 50, 80, 120) * 0.01))
})

testthat::test_that("read_biomass_output_rasters skips missing rasters silently", {
  tmp <- withr::local_tempdir("test_biomass_rast_missing_")
  out_dir <- file.path(tmp, "rep01", "outputs", "biomass")
  dir.create(out_dir, recursive = TRUE)
  rep_dir <- file.path(tmp, "rep01")

  r <- terra::rast(nrows = 2L, ncols = 2L, vals = 1:4)
  terra::writeRaster(r, file.path(out_dir, "biomass-Hw-0.tif"), overwrite = TRUE)
  ## Sx raster intentionally absent

  result <- read_biomass_output_rasters(
    dirs = rep_dir,
    times = 0L,
    species = c("Hw", "Sx"),
    run_name = "s1"
  )

  testthat::expect_equal(unique(result$species), "Hw")
})

## ---- biomass_landscape_summary --------------------------------------------------------------

testthat::test_that("biomass_landscape_summary returns mean and sd per species per time", {
  df <- make_biomass_dt(times = c(0L, 100L), n_reps = 3L)

  result <- biomass_landscape_summary(df)

  testthat::expect_s3_class(result, "tbl_df")
  testthat::expect_named(
    result,
    c("Time", "species", "mean_biomass", "sd_biomass"),
    ignore.order = TRUE
  )
  testthat::expect_equal(sort(unique(result$Time)), c(0L, 100L))
  testthat::expect_equal(sort(unique(result$species)), c("Hw", "Sx"))
  testthat::expect_true(all(result$sd_biomass >= 0))
})

testthat::test_that("biomass_landscape_summary returns sd = 0 for single replicate", {
  df <- make_biomass_dt(times = 0L, n_reps = 1L)
  result <- biomass_landscape_summary(df)
  testthat::expect_true(all(result$sd_biomass == 0))
})

## ---- leading_species ------------------------------------------------------------------------

testthat::test_that("leading_species returns species with highest biomass per cell", {
  df <- make_biomass_dt(times = c(0L, 100L), n_reps = 1L)

  result <- leading_species(df)

  testthat::expect_s3_class(result, "tbl_df")
  testthat::expect_named(
    result,
    c("scenario", "replicate", "Time", "row", "column", "label"),
    ignore.order = TRUE
  )

  ## at t=0: cell (1,1) has Hw=5 > Sx=2 → Hw; cell (1,2) has Sx=2 < Hw? check make_biomass_dt
  ## biomass at t=0: c(5, 2, 3, 4) for (Hw_r1c1, Sx_r1c1?, ...)
  ## make_biomass_dt: rows are (1,1,Hw), (1,1,Sx?), (1,2,Hw?), (1,2,Sx?)... wait let me re-check.
  ## Actually the dt has row=c(1,1,2,2), col=c(1,2,1,2), species=c("Hw","Sx","Hw","Sx")
  ## biomass t=0: Hw(1,1)=5, Sx(1,2)=2, Hw(2,1)=3, Sx(2,2)=4
  ## So leading: (1,1)→Hw, (1,2)→Sx, (2,1)→Hw, (2,2)→Sx at t=0
  r0 <- result[result$Time == 0L, ]
  testthat::expect_equal(r0[r0$row == 1L & r0$column == 1L, "label"][[1]], "Hw")
  testthat::expect_equal(r0[r0$row == 1L & r0$column == 2L, "label"][[1]], "Sx")
})

testthat::test_that("leading_species breaks ties alphabetically", {
  df <- data.table::data.table(
    scenario = "s",
    replicate = "rep01",
    Time = 0L,
    row = 1L,
    column = 1L,
    ecoregion = 1L,
    species = c("Hw", "Sx"),
    biomass = c(5, 5) ## exact tie
  )
  result <- leading_species(df)
  ## alphabetically first: "Hw" < "Sx"
  testthat::expect_equal(result$label, "Hw")
})

testthat::test_that("leading_species labels zero-biomass cells as Non-vegetated", {
  ## All-zero cells must not get an arbitrary species via alphabetical tiebreaker;
  ## they should be labelled "Non-vegetated" (matching community_label() semantics).
  df <- data.table::data.table(
    scenario = "s",
    replicate = "rep01",
    Time = 0L,
    row = 1L,
    column = 1L,
    ecoregion = 1L,
    species = c("Hw", "Sx"),
    biomass = c(0, 0)
  )
  result <- leading_species(df)
  testthat::expect_equal(result$label, "Non-vegetated")
})

## ---- community_label ------------------------------------------------------------------------

testthat::test_that("community_label returns top-2 species joined with '-'", {
  df <- data.table::data.table(
    scenario = "s",
    replicate = "rep01",
    Time = 0L,
    row = 1L,
    column = 1L,
    ecoregion = 1L,
    species = c("Hw", "Sx", "Bl"),
    biomass = c(5, 3, 1)
  )
  result <- community_label(df, n_spp = 2L)
  testthat::expect_equal(result$label, "Hw-Sx")
})

testthat::test_that("community_label drops species below min_pct", {
  df <- data.table::data.table(
    scenario = "s",
    replicate = "rep01",
    Time = 0L,
    row = 1L,
    column = 1L,
    ecoregion = 1L,
    species = c("Hw", "Sx"),
    ## Sx is only 5% of total — below default 10% threshold
    biomass = c(19, 1)
  )
  result <- community_label(df, n_spp = 2L, min_pct = 0.1)
  testthat::expect_equal(result$label, "Hw")
})

testthat::test_that("community_label labels zero-biomass cells as Non-vegetated", {
  df <- data.table::data.table(
    scenario = "s",
    replicate = "rep01",
    Time = 0L,
    row = 1L,
    column = 1L,
    ecoregion = 1L,
    species = c("Hw", "Sx"),
    biomass = c(0, 0)
  )
  result <- community_label(df)
  testthat::expect_equal(result$label, "Non-vegetated")
})

testthat::test_that("community_label respects n_spp = 1", {
  df <- data.table::data.table(
    scenario = "s",
    replicate = "rep01",
    Time = 0L,
    row = 1L,
    column = 1L,
    ecoregion = 1L,
    species = c("Hw", "Sx"),
    biomass = c(7, 3)
  )
  result <- community_label(df, n_spp = 1L)
  testthat::expect_equal(result$label, "Hw")
})

## ---- transition_data ------------------------------------------------------------------------

testthat::test_that("transition_data returns lodes-form tibble with correct columns", {
  df <- make_biomass_dt(times = c(0L, 100L), n_reps = 2L)
  labels <- leading_species(df)

  result <- transition_data(labels, times = c(0L, 100L))

  testthat::expect_s3_class(result, "tbl_df")
  testthat::expect_named(result, c("alluvium", "x", "stratum", "y"), ignore.order = TRUE)
  testthat::expect_true(all(result$x %in% c(0L, 100L)))
  testthat::expect_true(all(result$y > 0))
})

testthat::test_that("transition_data total y equals n_cells across any x value", {
  df <- make_biomass_dt(times = c(0L, 100L), n_reps = 2L)
  labels <- leading_species(df)
  result <- transition_data(labels, times = c(0L, 100L))

  n_cells <- 4L ## make_biomass_dt creates 4 cells
  totals <- tapply(result$y, result$x, sum)
  testthat::expect_equal(as.vector(totals), rep(n_cells, 2L))
})

testthat::test_that("transition_data rejects times not in label_df", {
  df <- make_biomass_dt(times = c(0L, 100L), n_reps = 1L)
  labels <- leading_species(df)
  testthat::expect_error(transition_data(labels, times = c(0L, 200L)))
})

## ---- plot_species_biomass -------------------------------------------------------------------

testthat::test_that("plot_species_biomass returns a ggplot", {
  df <- make_biomass_dt(times = c(0L, 100L), n_reps = 2L)
  summary <- biomass_landscape_summary(df)
  p <- plot_species_biomass(summary)
  testthat::expect_s3_class(p, "ggplot")
})

testthat::test_that("plot_species_biomass accepts custom colours", {
  df <- make_biomass_dt(times = 0L, n_reps = 1L)
  summary <- biomass_landscape_summary(df)
  p <- plot_species_biomass(summary, colours = c(Hw = "#ff0000", Sx = "#0000ff"))
  testthat::expect_s3_class(p, "ggplot")
})

## ---- plot_transitions -----------------------------------------------------------------------

testthat::test_that("plot_transitions returns a ggplot", {
  df <- make_biomass_dt(times = c(0L, 100L), n_reps = 2L)
  labels <- leading_species(df)
  lodes <- transition_data(labels, times = c(0L, 100L))
  p <- plot_transitions(lodes)
  testthat::expect_s3_class(p, "ggplot")
})

testthat::test_that("plot_transitions accepts custom colours and title", {
  df <- make_biomass_dt(times = c(0L, 100L), n_reps = 1L)
  labels <- leading_species(df)
  lodes <- transition_data(labels, times = c(0L, 100L))
  p <- plot_transitions(
    lodes,
    colours = c(Hw = "#2c7bb6", Sx = "#d7191c"),
    title = "Test transitions"
  )
  testthat::expect_s3_class(p, "ggplot")
})

testthat::test_that("community_label transitions also produce a valid ggplot", {
  df <- make_biomass_dt(times = c(0L, 100L), n_reps = 2L)
  comm <- community_label(df, n_spp = 2L)
  lodes <- transition_data(comm, times = c(0L, 100L))
  p <- plot_transitions(lodes)
  testthat::expect_s3_class(p, "ggplot")
})
