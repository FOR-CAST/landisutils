## NOTE: the scenario fixture is built by local_landis_scenario() in helper-landis-maps.R, which
## writes REAL rasters and CSVs (not the zero-byte stubs the config-generation tests use), because
## these checks read pixel types, dimensions and map codes.

test_that("a clean scenario validates without problems", {
  skip_if_not_installed("terra")
  dir <- local_landis_scenario()

  expect_equal(validate_landis_scenario(dir, error = FALSE), character(0))
  expect_no_error(validate_landis_scenario(dir))
})

test_that("landis_directive() reads a directive, unquoted, ignoring comments", {
  tmp <- withr::local_tempfile(fileext = ".txt")
  writeLines(
    c(
      ">> InitialCommunitiesMap  commented-out.tif",
      'LandisData  "Biomass Succession"',
      "InitialCommunitiesMap   initial-communities.tif  >> trailing comment",
      'Species                 "species file.txt"'
    ),
    tmp
  )

  expect_equal(landis_directive(tmp, "InitialCommunitiesMap"), "initial-communities.tif")
  expect_equal(landis_directive(tmp, "Species"), "species file.txt")
  expect_equal(landis_directive(tmp, "Absent"), NA_character_)
  expect_equal(landis_directive(tmp, "Absent", default = "fallback.tif"), "fallback.tif")
  expect_equal(landis_directive("no-such-file.txt", "Species", default = "d.txt"), "d.txt")
})

## --- the mirrored-map check -------------------------------------------------
##
## This is the check the whole exercise exists for. A vertically mirrored map has the right
## dimensions, the right values and the right totals, so nothing rejects it: LANDIS-II runs to
## completion with the vegetation displaced relative to the ecoregion, fire-region and topography
## maps. It cost a 25-generation Dynamic Fire calibration before landisutils 0.0.95 stopped
## producing one. Only per-cell content agreement finds it -- the file is north-up either way.

test_that("a vertically mirrored initial-communities map is caught", {
  skip_if_not_installed("terra")
  dir <- local_landis_scenario()
  ic <- fs::path(dir, "initial-communities.tif")

  r <- terra::rast(ic)
  terra::writeRaster(
    terra::flip(r, direction = "vertical"),
    ic,
    datatype = terra::datatype(r),
    overwrite = TRUE
  )

  problems <- validate_landis_scenario(dir, error = FALSE)
  expect_length(problems, 1L)
  expect_match(problems, "VERTICALLY MIRRORED")
  expect_match(problems, "initial-communities.tif", fixed = TRUE)
  expect_match(problems, "read_landis_raster", fixed = TRUE)
})

test_that("the orientation check exempts continuous maps", {
  skip_if_not_installed("terra")
  dir <- local_landis_scenario()
  slope <- fs::path(dir, "ground_slope.tif")

  ## Slope is legitimately 0 on active cells, so its non-zero mask is not a footprint and comparing
  ## it against the ecoregion mask is meaningless in EITHER orientation. Mirroring it must not be
  ## reported -- a false positive here would block every valid scenario.
  r <- terra::rast(slope)
  terra::writeRaster(
    terra::flip(r, direction = "vertical"),
    slope,
    datatype = terra::datatype(r),
    overwrite = TRUE
  )

  expect_equal(validate_landis_scenario(dir, error = FALSE), character(0))
})

## --- pixel type, existence, geometry ----------------------------------------

test_that("a pixel type LANDIS-II cannot read is caught", {
  skip_if_not_installed("terra")
  dir <- local_landis_scenario()
  eco <- fs::path(dir, "ecoregions.tif")

  ## INT4U is the tempting choice for positive map codes and is exactly the one that aborts the run
  ## at extension-init time, with empty stderr on the R side.
  r <- terra::rast(eco)
  .landis_write_map(
    eco,
    terra::values(r, mat = FALSE),
    nr = terra::nrow(r),
    nc = terra::ncol(r),
    datatype = "INT4U"
  )

  problems <- validate_landis_scenario(dir, error = FALSE)
  expect_match(problems, "INT4U", all = FALSE)
  expect_match(problems, "landis_datatype", all = FALSE, fixed = TRUE)
})

test_that("missing and empty input files are caught", {
  skip_if_not_installed("terra")
  dir <- local_landis_scenario()
  fs::file_delete(fs::path(dir, "species.txt"))
  writeLines(character(0), fs::path(dir, "ecoregions.txt"))

  problems <- validate_landis_scenario(dir, error = FALSE)
  expect_match(problems, "file not found: species.txt", all = FALSE, fixed = TRUE)
  expect_match(problems, "file is empty: ecoregions.txt", all = FALSE, fixed = TRUE)
})

test_that("a map whose dimensions differ from the ecoregions map is caught", {
  skip_if_not_installed("terra")
  dir <- local_landis_scenario()
  ic <- fs::path(dir, "initial-communities.tif")
  .landis_write_map(ic, rep(c(1L, 2L, 3L), length.out = 12L), nr = 4L, nc = 3L)

  problems <- validate_landis_scenario(dir, error = FALSE)
  expect_match(problems, "map is 4x3 but the ecoregions map is 6x5", all = FALSE, fixed = TRUE)
})

test_that("files LANDIS-II writes are not reported as missing inputs", {
  skip_if_not_installed("terra")
  dir <- local_landis_scenario()

  ## fire/dynamic-fire-event-log.csv is in output_manifest.txt and does not exist yet; a
  ## `{timestep}` template names a file per output step. Neither is an input.
  writeLines(
    c(
      readLines(fs::path(dir, "biomass-succession.txt")),
      "MapNames    biomass/{species}-{timestep}.tif",
      "LogFile     biomass-succession-log.csv"
    ),
    fs::path(dir, "biomass-succession.txt")
  )

  expect_equal(validate_landis_scenario(dir, error = FALSE), character(0))
})

## --- initial communities ----------------------------------------------------

test_that("unresolved initial-communities map codes are caught, but one empty community is not", {
  skip_if_not_installed("terra")
  dir <- local_landis_scenario()
  csv <- fs::path(dir, "initial-communities.csv")
  d <- data.table::fread(csv)

  ## dedup_community_snapshot() assigns ONE shared code to active cells that have no cohorts, and
  ## that code deliberately has no CSV rows. Dropping a single code must therefore be tolerated.
  data.table::fwrite(d[d$MapCode != 3L, ], csv)
  expect_equal(validate_landis_scenario(dir, error = FALSE), character(0))

  ## two unresolved codes is a genuinely incomplete CSV: LANDIS-II aborts with "Unknown map code"
  data.table::fwrite(d[d$MapCode == 1L, ], csv)
  problems <- validate_landis_scenario(dir, error = FALSE)
  expect_match(problems, "Unknown map code", all = FALSE, fixed = TRUE)
})

test_that("an oversized initial-communities CSV is caught", {
  skip_if_not_installed("terra")
  dir <- local_landis_scenario()

  ## the real case was a 1.5 GB per-pixel snapshot that exhausted the container's --memory inside
  ## the parser; the threshold is an argument so the test needn't write anything large
  problems <- validate_landis_scenario(dir, error = FALSE, max_ic_csv_mb = 0)
  expect_match(problems, "dedup_community_snapshot", all = FALSE, fixed = TRUE)
})

## --- per-extension contracts ------------------------------------------------
##
## These read the written configuration rather than the R6 object that produced it, because the
## Dynamic Fire calibration patches dynamic-fire.txt after the writer has run.

test_that("non-dyadic season proportions are caught", {
  skip_if_not_installed("terra")
  dir <- local_landis_scenario()
  fire <- fs::path(dir, "dynamic-fire.txt")

  ## 0.1 + 0.2 + 0.7 sums to 1 in double but not in the single precision the parser uses, so the
  ## run aborts with "Season Probabilities don't add to 1.0"
  writeLines(
    sub(
      "^Spring(.*)0\\.25",
      "Spring\\10.10",
      sub(
        "^Summer(.*)0\\.50",
        "Summer\\10.20",
        sub("^Fall(.*)0\\.25", "Fall\\10.70", readLines(fire))
      )
    ),
    fire
  )

  problems <- validate_landis_scenario(dir, error = FALSE)
  expect_match(problems, "not dyadic fractions", all = FALSE)
  expect_match(problems, "insertSeasonTable", all = FALSE, fixed = TRUE)
})

test_that("a fire-regions map that does not cover the core active mask is caught", {
  skip_if_not_installed("terra")
  dir <- local_landis_scenario()
  f <- fs::path(dir, "fire-ecoregions.tif")

  ## one active cell left without a fire region: LANDIS-II reads the raw 0, finds no matching row in
  ## the fire-size table and aborts with "Unknown map code"
  r <- terra::rast(f)
  v <- terra::values(r, mat = FALSE)
  v[which(v > 0)[[1L]]] <- 0L
  .landis_write_map(f, v, nr = terra::nrow(r), nc = terra::ncol(r))

  problems <- validate_landis_scenario(dir, error = FALSE)
  expect_match(problems, "1 cell\\(s\\) are active in the ecoregions map", all = FALSE)
  expect_match(problems, "Unknown map code", all = FALSE, fixed = TRUE)
})

test_that("a modelled species with no fuel type is caught", {
  skip_if_not_installed("terra")
  dir <- local_landis_scenario()
  fuels <- fs::path(dir, "dynamic-fuels.txt")

  ## dropping a species from FuelTypes removes it from the fuels model silently -- nothing in the
  ## LANDIS-II logs says a species was left unparameterised
  writeLines(grep("pinubank", readLines(fuels), invert = TRUE, value = TRUE), fuels)

  problems <- validate_landis_scenario(dir, error = FALSE)
  expect_match(problems, "no fuel type", all = FALSE, fixed = TRUE)
  expect_match(problems, "pinubank", all = FALSE, fixed = TRUE)
})

## --- reporting --------------------------------------------------------------

test_that("validation reports every problem at once", {
  skip_if_not_installed("terra")
  dir <- local_landis_scenario()
  fs::file_delete(fs::path(dir, "species.txt"))
  writeLines(character(0), fs::path(dir, "ecoregions.txt"))

  ## a scenario with three defects should not cost three build cycles
  expect_gte(length(validate_landis_scenario(dir, error = FALSE)), 2L)
})

test_that("a missing scenario directory or scenario file is reported, not crashed on", {
  expect_match(
    validate_landis_scenario(fs::path(tempdir(), "no-such-dir"), error = FALSE),
    "scenario directory not found"
  )
  empty <- withr::local_tempdir()
  expect_match(validate_landis_scenario(empty, error = FALSE), "no scenario.txt", fixed = TRUE)
})

test_that("validate_landis_scenario() failure message lists the problems", {
  skip_if_not_installed("terra")
  dir <- local_landis_scenario()
  fs::file_delete(fs::path(dir, "species.txt"))

  expect_error(validate_landis_scenario(dir), "failed pre-flight validation")
  expect_error(validate_landis_scenario(dir), "species.txt", fixed = TRUE)
})

test_that("scenario builders validate by default and honour the opt-out", {
  skip_if_not_installed("terra")
  dir <- local_landis_scenario()
  fs::file_delete(fs::path(dir, "species.txt"))

  rebuild <- function(...) {
    write_landis_scenario_file(
      path = dir,
      duration = 10L,
      cell_length = 100L,
      species_file = fs::path(dir, "species.txt"),
      ecoregions_files = c(fs::path(dir, "ecoregions.txt"), fs::path(dir, "ecoregions.tif")),
      succession_ext_files = c("Biomass Succession" = fs::path(dir, "biomass-succession.txt")),
      ...
    )
  }
  ## species_file is gone: path_real() inside the writer fails before validation would run, so
  ## assert the opt-out plumbing on a scenario that is merely invalid, not unwritable
  fs::file_create(fs::path(dir, "species.txt"))
  writeLines(character(0), fs::path(dir, "ecoregions.txt"))

  expect_error(rebuild(), "failed pre-flight validation")
  expect_no_error(rebuild(validate = FALSE))

  withr::local_options(landisutils.validate_scenario = FALSE)
  expect_no_error(rebuild())
})
