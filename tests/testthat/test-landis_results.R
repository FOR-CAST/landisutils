## read_landis_raster() reads a LANDIS-II output back in the row order
## LANDIS-II wrote it, and georef_landis_raster() copies a template raster's
## CRS + extent onto a spatially-reference-less LANDIS-II output of identical
## dimensions (and errors when the dimensions do not match).
##
## Two fixtures stand in for the two ways a file can be stored south-up; see
## fixtures/make-landis-output-fixtures.R. Everything else uses tiny in-memory
## terra rasters so the tests stay hermetic.

## a real LANDIS-II Output Biomass Community map: 15 rows x 1 col, NO
## geotransform, map codes 3..17 down the file
fixture_no_gt <- function() {
  testthat::test_path("fixtures", "landis-output-no-geotransform.tif")
}

## 4 rows x 3 cols carrying an explicit south-up geotransform, values 1..12 in
## file order
fixture_south_up <- function() {
  testthat::test_path("fixtures", "landis-output-south-up.tif")
}

testthat::test_that("read_landis_raster() restores the row order of a LANDIS-II output", {
  ## terra reverses the rows of the south-up file on read; the map codes are
  ## written 3, 4, ... 17 from the first row down
  testthat::expect_equal(
    as.vector(terra::values(suppressWarnings(terra::rast(fixture_no_gt())))),
    17:3
  )
  testthat::expect_equal(
    as.vector(terra::values(landisutils::read_landis_raster(fixture_no_gt()))),
    3:17
  )
})

testthat::test_that("read_landis_raster() flips rows only, keeping column order", {
  testthat::expect_equal(
    as.vector(terra::values(landisutils::read_landis_raster(fixture_south_up()))),
    1:12
  )
})

testthat::test_that("read_landis_raster() leaves a north-up raster alone", {
  path <- withr::local_tempfile(fileext = ".tif")
  r <- terra::rast(nrows = 4, ncols = 3, xmin = 0, xmax = 3, ymin = 0, ymax = 4, crs = "")
  terra::values(r) <- 1:12
  terra::writeRaster(r, path)

  testthat::expect_equal(as.vector(terra::values(landisutils::read_landis_raster(path))), 1:12)
})

testthat::test_that("read_landis_raster() georeferences against a template", {
  template <- terra::rast(
    nrows = 15,
    ncols = 1,
    xmin = 1000,
    xmax = 1100,
    ymin = 2000,
    ymax = 3500,
    crs = "EPSG:3005"
  )
  out <- landisutils::read_landis_raster(fixture_no_gt(), template)
  testthat::expect_equal(as.vector(terra::ext(out)), as.vector(terra::ext(template)))
  testthat::expect_equal(terra::crs(out), terra::crs(template))
  testthat::expect_equal(as.vector(terra::values(out)), 3:17)
})

testthat::test_that("georef_landis_raster() reorients a LANDIS-II output given its path", {
  template <- terra::rast(
    nrows = 4,
    ncols = 3,
    xmin = 1000,
    xmax = 1300,
    ymin = 2000,
    ymax = 2400,
    crs = "EPSG:3005"
  )
  out <- landisutils::georef_landis_raster(fixture_south_up(), template)
  testthat::expect_equal(as.vector(terra::values(out)), 1:12)
  testthat::expect_equal(as.vector(terra::ext(out)), as.vector(terra::ext(template)))
})

testthat::test_that("georef_landis_raster() reorients a raster still backed by the file", {
  template <- terra::rast(
    nrows = 4,
    ncols = 3,
    xmin = 1000,
    xmax = 1300,
    ymin = 2000,
    ymax = 2400,
    crs = "EPSG:3005"
  )
  r <- terra::rast(fixture_south_up()) ## the wrong way to open one: rows reversed
  out <- landisutils::georef_landis_raster(r, template)
  testthat::expect_equal(as.vector(terra::values(out)), 1:12)
})

testthat::test_that("georef_landis_raster() copies extent and CRS from the template", {
  template <- terra::rast(
    nrows = 4,
    ncols = 5,
    xmin = 1000,
    xmax = 1500,
    ymin = 2000,
    ymax = 2400,
    crs = "EPSG:3005"
  )
  ## a same-dimension raster with the default (lon/lat) georeferencing
  r <- terra::rast(nrows = 4, ncols = 5)
  terra::values(r) <- seq_len(terra::ncell(r))

  out <- landisutils::georef_landis_raster(r, template)
  testthat::expect_s4_class(out, "SpatRaster")
  testthat::expect_equal(as.vector(terra::ext(out)), as.vector(terra::ext(template)))
  testthat::expect_equal(terra::crs(out), terra::crs(template))
  ## values are untouched
  testthat::expect_equal(terra::values(out)[, 1], seq_len(terra::ncell(r)))
})

testthat::test_that("georef_landis_raster() errors on a dimension mismatch", {
  template <- terra::rast(nrows = 4, ncols = 5, crs = "EPSG:3005")
  r <- terra::rast(nrows = 3, ncols = 5)
  testthat::expect_snapshot(landisutils::georef_landis_raster(r, template), error = TRUE)
})
