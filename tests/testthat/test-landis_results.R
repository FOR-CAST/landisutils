## georef_landis_raster() copies a template raster's CRS + extent onto a
## spatially-reference-less LANDIS-II output of identical dimensions, and
## errors when the dimensions do not match. Tests use tiny in-memory terra
## rasters so they are hermetic.

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
