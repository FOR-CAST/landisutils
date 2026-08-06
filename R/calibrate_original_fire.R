## Original Fire calibration ---------------------------------------------------------------
##
## Not yet implemented. A calibration for the LANDIS-II Original Fire extension would mirror the
## structure of `calibrate_dynamic_fire.R`. Tracked in
## <https://github.com/FOR-CAST/landisutils/issues/2>.
##
## Deliberately NOT exported until it does something: an exported no-op is indistinguishable
## from a calibration that ran and found nothing.

calibrate_original_fire <- function(...) {
  stop(
    "`calibrate_original_fire()` is not yet implemented. ",
    "Only the Dynamic Fire calibration exists; see `calibrate_dynamic_fire()`."
  )
}
