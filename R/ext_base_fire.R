#' Create Base Fire Input File
#'
#' Follows the Base Fire User Guide.
#'
#' @template param_path
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

#' Specify Fire Region Parameters Table
#'
#' @param df data.frame corresponding to Fire Region Parameters Table
#'
#' @template return_insert
#'
#' @export
insertFireRegionParametersTable <- function(df) {
  browser()
  c(
    glue::glue(">> Fire Region Parameters"),
    glue::glue(">> "),
    glue::glue(">> Region  Map    Mean  Min   Max   Ignition  Fire"),
    glue::glue(">> Name    Code   Size  Size  Size  Prob      k"),
    glue::glue(">> -----------------------------------------------"),
    glue::glue(""), ## TODO
    glue::glue("") ## add blank line after each item group
  )
}

#' Create Base Fire `InitialFireRegionsMap`
#'
#' @param r `SpatRaster` corresponding to initial fire regions map
#'
#' @template return_file
#'
#' @export
prepInitialFireRegionsMap <- function(r, file = "fire-regions-map.tif") {
  terra::writeRaster(r, file, overwrite = TRUE)

  return(file)
}

#' Specify `InitialFireRegionsMap` file
#'
#' @param file
#'
#' @template return_insert
#'
#' @export
insertInitialFireRegionsMap <- function(file) {
  c(
    glue::glue("InitialFireRegionsMap    \"{file}\""),
    glue::glue("") ## add blank line after each item group
  )
}

#' Specify Base Fire `DynamicFireRegionTable`
#'
#' @param df data.frame corresponding to `DynamicFireRegionTable` (optional)
#'
#' @template return_insert
#'
#' @export
insertDynamicFireRegionTable <- function(df = NULL) {
  browser() ## TODO
  if (is.null(df)) {
    c(
      glue::glue(">> DynamicFireRegionTable"),
      glue::glue(">> Year   Filename"),
      glue::glue(">> -------------------------------"),
      glue::glue(">> "),
      glue::glue("") ## add blank line after each item group
    )
  } else {
    c(
      glue::glue("DynamicFireRegionTable"),
      glue::glue(">> Year   Filename"),
      glue::glue(">> -------------------------------"),
      glue::glue(""), ## TODO
      glue::glue("") ## add blank line after each item group
    )
  }
}

#' Specify Base Fire `FuelCurveTable`
#'
#' @param df data.frame
#'
#' @template return_insert
#'
#' @export
insertFuelCurveTable <- function(df) {
  browser() ## TODO
  c(
    glue::glue("FuelCurveTable"),
    glue::glue(">> Ecoregion    S5  S4  S3  S2  S1"),
    glue::glue(">> -------------------------------"),
    glue::glue(""), ## TODO
    glue::glue("") ## add blank line after each item group
  )
}

#' Specify Base Fire `WindCurveTable`
#'
#' @param df data.frame
#'
#' @template return_insert
#'
#' @export
insertWindCurveTable <- function(df) {
  browser() ## TODO
  c(
    glue::glue("WindCurveTable"),
    glue::glue(">> Ecoregion    S5  S4  S3  S2  S1"),
    glue::glue(">> -------------------------------"),
    glue::glue(""), ## TODO
    glue::glue("") ## add blank line after each item group
  )
}

#' Specify Base Fire `FireDamageTable`
#'
#' @param df data.frame
#'
#' @template return_insert
#'
#' @export
insertFireDamageTable <- function(df) {
  browser() ## TODO
  c(
    glue::glue("FireDamageTable"),
    glue::glue(">> Cohort Age      FireSeverity - "),
    glue::glue(">> % of longevity  FireTolerance"),
    glue::glue(">> --------------  ---------------"),
    glue::glue(""), ## TODO
    glue::glue("") ## add blank line after each item group
  )
}

#' Specify Base Fire `MapNames`
#'
#' @template param_path
#'
#' @template return_insert
#'
#' @export
insertMapNames <- function(path = "fire") {
  ## NOTE: careful using glue() here; need literal {timestep}, so use {{timestep}}
  glue::glue("MapNames    \"{path}/severity-{{timestep}}.tif\"")
}

#' Specify Base Fire `LogFile`
#'
#' @param file
#'
#' @template return_insert
#'
#' @export
insertLogFile <- function(file = "fire/log.csv") {
  glue::glue("LogFile    \"{file}\"")
}

#' Specify Base Fire `SummaryLogFile`
#'
#' @param file
#'
#' @template return_insert
#'
#' @export
insertSummaryLogFile <- function(file = "fire/summary-log.csv") {
  glue::glue("SummaryLogFile    \"{file}\"")
}
