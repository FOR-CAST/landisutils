#' Create Base Fire Input File
#'
#' Follows the Base Fire User Guide.
#'
#' @param path
#'
#' @param ... arguments passed to other functions:
#'   - `DynamicFireRegionsTable`  (optional);
#'   - `FireRegionParametersTable`;
#'   - `FireDamageTable`;
#'   - `FuelCurveTable`;
#'   - `InitialFireRegionsMap`;
#'   - `LogFile`;
#'   - `MapNames`;
#'   - `SummaryLogFile`;
#'   - `Timestep`;
#'   - `WindCurveTable`;
#'
#' @export
BaseFireInput <- function(path, ...) {
  stopifnot(
    !is.null(path)
  )
browser()
  dots <- list(...)
  stopifnot(
    !is.null(dots$FireRegionParametersTable)
  )

  file <- file.path(path, "base_fire.txt")
  writeLines(c(
    LandisData("Base Fire"),
    insertTimestep(dots$Timestep),
    insertFireRegionParametersTable(dots$FireRegionParametersTable),
    insertInitialFireRegionsMap(dots$InitialFireRegionsMap),
    insertDynamicFireRegionsTable(dots$DynamicFireRegionsTable),
    insertFuelCurveTable(dots$FuelCurveTable),
    insertWindCurveTable(dots$WindCurveTable),
    insertFireDamageTable(dots$FireDamageTable),
    insertMapNames(dots$MapNames),
    insertLogFile(dots$LogFile),
    insertSummaryLogFile(dots$SummaryLogFile)
  ), file)

  return(file)
}
