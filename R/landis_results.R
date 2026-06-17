## LANDIS-II output georeferencing -------------------------------------------------------------
##
## LANDIS-II writes GeoTIFFs with NO spatial reference (a raw row/col grid).
## The helper here attaches CRS + extent from a study area's rasterToMatch so
## the raw outputs become analysis-ready layers. De-duplicated from the
## BC_HRV / gitanyow-partial-harvest Phase-6 output-reading templates.

#' Attach CRS and extent from a template to a LANDIS-II raster
#'
#' LANDIS-II GeoTIFFs are written without a spatial reference (a raw row/col
#' grid). This copies the coordinate reference system and extent from a
#' template raster (typically the study area's rasterToMatch) onto a LANDIS-II
#' output of identical dimensions, making it analysis-ready.
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
    r <- terra::rast(r)
  }
  if (is.character(template)) {
    template <- terra::rast(template)
  }
  stopifnot(terra::ncell(r) == terra::ncell(template), all(dim(r)[1:2] == dim(template)[1:2]))
  terra::ext(r) <- terra::ext(template)
  terra::crs(r) <- terra::crs(template)
  r
}
