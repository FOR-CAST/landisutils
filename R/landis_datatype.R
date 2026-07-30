## The GDAL pixel types LANDIS-II will open, as terra datatype strings.
##
## LANDIS-II reads every map through `Landis.RasterIO.Gdal.GdalInputRaster.NewInputBand`, which
## switches on the GDAL band type and throws for anything outside this set:
##
##   "Raster band is not byte, short, int, float, double"
##
## That is the complete list -- Byte, Int16, Int32, Float32, Float64. Everything else GDAL can write
## is rejected, and the failure comes at extension-initialisation time (a few seconds into a run) with
## the message above and a stack trace, NOT at write time, so a bad map is only caught by running the
## model. The rejected types that R code actually reaches for are the UNSIGNED integers -- and they
## are the tempting ones, because map codes, ecoregion codes and fire-region codes are all positive by
## construction, so `INT2U` / `INT4U` look like the natural fit and encode the same range in the same
## number of bytes. `INT1S` is rejected too: GDAL 3.7+ writes it as Int8, which is not `Byte`.
.LANDIS_DATATYPES <- c("INT1U", "INT2S", "INT4S", "FLT4S", "FLT8S")

#' Pick a raster pixel type LANDIS-II can read
#'
#' Returns the smallest terra `datatype` that LANDIS-II will open and that can
#' hold `max_value`. Use it for any integer-coded map written for LANDIS-II
#' (initial communities, ecoregions, fire regions), rather than choosing a
#' datatype by hand.
#'
#' LANDIS-II opens maps through
#' `Landis.RasterIO.Gdal.GdalInputRaster.NewInputBand`, which accepts only GDAL
#' `Byte`, `Int16`, `Int32`, `Float32` and `Float64`, and aborts the run with
#' `Raster band is not byte, short, int, float, double` on anything else. The
#' unsigned integer types are therefore unusable even though map codes are
#' always positive -- as is `INT1S`, which GDAL 3.7+ writes as `Int8`. Because
#' the check happens when the extension initialises rather than when the file is
#' written, an unreadable map is only discovered by running the model.
#'
#' @param max_value Largest value the map has to represent. Must be finite and
#'   non-negative.
#'
#' @return A length-1 character `datatype` suitable for [terra::writeRaster()]:
#'   `"INT1U"`, `"INT2S"` or `"INT4S"`.
#'
#' @examples
#' landis_datatype(200) ## "INT1U"
#' landis_datatype(5000) ## "INT2S" -- NOT "INT2U", which LANDIS-II rejects
#' landis_datatype(1e6) ## "INT4S"
#'
#' @export
landis_datatype <- function(max_value) {
  if (!is.numeric(max_value) || length(max_value) != 1L || !is.finite(max_value)) {
    stop("`max_value` must be a single finite number.", call. = FALSE)
  }
  if (max_value < 0) {
    stop("`max_value` must be non-negative; LANDIS-II map codes are positive.", call. = FALSE)
  }
  if (max_value > .Machine$integer.max) {
    stop(
      "`max_value` (",
      format(max_value, scientific = FALSE),
      ") exceeds Int32; LANDIS-II has no wider integer type it can read.",
      call. = FALSE
    )
  }
  if (max_value <= 255) {
    "INT1U" ## GDAL Byte (unsigned, but "byte" is on the accepted list)
  } else if (max_value <= 32767) {
    "INT2S" ## GDAL Int16 -- NOT INT2U
  } else {
    "INT4S" ## GDAL Int32 -- NOT INT4U
  }
}
