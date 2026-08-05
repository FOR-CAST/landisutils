## LANDIS-II output reading ---------------------------------------------------------------------
##
## LANDIS-II writes its output GeoTIFFs through `Landis.RasterIO.Gdal`, which
## streams pixels in landscape order -- the same order it read the input maps
## in, first row first -- but never calls SetGeoTransform() or SetProjection().
## The files therefore carry NO spatial reference at all, and GDAL falls back to
## its identity geotransform: origin (0, 0) with a POSITIVE pixel height, i.e.
## the first row at y = 0 and the last at y = nrow. That is a south-up raster,
## and `terra::rast()` normalises south-up rasters to north-up by REVERSING the
## rows. So a LANDIS-II output opened with `terra::rast()` is a vertical mirror
## of the grid LANDIS-II wrote, and of the input maps it is supposed to align
## with -- silently, because the only complaint is a "[rast] unknown extent"
## warning about the missing georeferencing.
##
## The helpers here read a LANDIS-II output back in its written row order and
## attach the CRS + extent of a study area's rasterToMatch, so the raw outputs
## become analysis-ready layers. De-duplicated from the BC_HRV /
## gitanyow-partial-harvest Phase-6 output-reading templates.

#' Does GDAL see this file as south-up (first row at the BOTTOM)?
#'
#' TRUE for every LANDIS-II output written by `Landis.RasterIO.Gdal`, which
#' writes no geotransform at all; TRUE also for a file that carries an explicit
#' south-up geotransform (positive pixel height). FALSE for an ordinary
#' north-up raster. Deciding this from the file rather than assuming it means a
#' LANDIS-II release that starts writing a proper geotransform needs no change
#' here.
#' @noRd
.landis_raster_south_up <- function(path) {
  info <- terra::describe(path)
  px <- grep("^Pixel Size = ", info, value = TRUE)
  if (length(px) == 0L) {
    ## no geotransform: GDAL reports the identity transform, whose pixel height is +1
    return(TRUE)
  }
  dy <- suppressWarnings(as.numeric(sub("^Pixel Size = \\([^,]*,([^)]*)\\).*$", "\\1", px[[1L]])))
  if (is.na(dy)) {
    stop(
      "cannot determine the row order of '",
      path,
      "': GDAL reported an unparseable pixel size (",
      px[[1L]],
      ")",
      call. = FALSE
    )
  }
  dy > 0
}

#' Read a LANDIS-II output raster in the row order LANDIS-II wrote it
#'
#' **Always open a LANDIS-II output map with this rather than with
#' [terra::rast()].** LANDIS-II writes its GeoTIFFs with no geotransform, which
#' GDAL reports as a south-up raster; `terra::rast()` normalises that by
#' reversing the rows, so the raster it returns is a vertical mirror of the
#' landscape LANDIS-II simulated. Nothing about the mirrored raster looks wrong
#' -- it has the right dimensions and the right values -- so the error surfaces
#' only as maps and per-region summaries that disagree with the inputs.
#'
#' This reads the file, restores the written row order when the file is stored
#' south-up (which is checked per file, not assumed), and optionally
#' georeferences the result against a template via [georef_landis_raster()].
#'
#' @param path A character path to a LANDIS-II output GeoTIFF.
#' @param template Optional `terra::SpatRaster`, or a character path to the
#'   study area rasterToMatch. When supplied, its CRS and extent are copied
#'   onto the result.
#'
#' @return A `terra::SpatRaster` whose row 1 is the first row LANDIS-II wrote,
#'   i.e. the first row of the input maps.
#'
#' @family output-reading helpers
#'
#' @export
#'
#' @examples
#' \dontrun{
#' sev <- read_landis_raster("fire/severity-10.tif", "rasterToMatch_Chine.tif")
#' }
read_landis_raster <- function(path, template = NULL) {
  stopifnot(is.character(path), length(path) == 1L, file.exists(path))
  ## "[rast] unknown extent" -- the missing georeferencing is the norm here
  r <- suppressWarnings(terra::rast(path))
  if (.landis_raster_south_up(path)) {
    r <- terra::flip(r, direction = "vertical")
  }
  if (!is.null(template)) {
    r <- georef_landis_raster(r, template)
  }
  r
}

#' Attach CRS and extent from a template to a LANDIS-II raster
#'
#' LANDIS-II GeoTIFFs are written without a spatial reference (a raw row/col
#' grid). This copies the coordinate reference system and extent from a
#' template raster (typically the study area's rasterToMatch) onto a LANDIS-II
#' output of identical dimensions, making it analysis-ready.
#'
#' This stamps georeferencing on; it does not resample or reproject. It DOES
#' reorient, but only when it can see the file the raster came from: given a
#' path, or a `SpatRaster` still backed by one, the row order is restored via
#' [read_landis_raster()]. A `SpatRaster` already loaded into memory carries no
#' record of how it was read, so orientation cannot be checked -- read LANDIS-II
#' outputs with [read_landis_raster()] and derive from there.
#'
#' @param r A `terra::SpatRaster`, or a character path to a LANDIS-II GeoTIFF.
#' @param template A `terra::SpatRaster`, or a character path to the study
#'   area rasterToMatch (defines the CRS and extent to copy).
#'
#' @return The georeferenced `terra::SpatRaster`.
#'
#' @family output-reading helpers
#'
#' @export
georef_landis_raster <- function(r, template) {
  if (is.character(r)) {
    return(read_landis_raster(r, template))
  }
  if (is.character(template)) {
    template <- terra::rast(template)
  }
  src <- terra::sources(r)
  if (length(src) == 1L && nzchar(src) && file.exists(src) && .landis_raster_south_up(src)) {
    r <- terra::flip(r, direction = "vertical")
  }
  stopifnot(terra::ncell(r) == terra::ncell(template), all(dim(r)[1:2] == dim(template)[1:2]))
  terra::ext(r) <- terra::ext(template)
  terra::crs(r) <- terra::crs(template)
  r
}
