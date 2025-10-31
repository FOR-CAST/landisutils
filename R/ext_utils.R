## fire extensions --------------------------------------------------------------------------------

#' Specify Dynamic Tables
#'
#'
#' @param df data.frame corresponding to the dynamic table (optional).
#'   Must have two columns: `Year` and `FileName`.
#'
#' @param name Character specifying the name of the table
#'   (e.g., `DynamicEcoregionTable`, `DynamicFireRegionTable`, `DynamicWeatherTable`).
#'
#' @template return_insert
#'
#' @keywords internal
insertDynamicTable <- function(name = NULL, df = NULL) {
  if (is.null(df)) {
    df <- data.frame(Year = integer(0), FileName = character(0))
  }

  stopifnot(ncol(df) == 2)

  c(
    glue::glue("{name}"),
    glue::glue(">> Year    FileName"),
    glue::glue(">> ----    ------------------------"),
    apply(df, 1, function(x) {
      glue::glue_collapse(x, sep = "    ")
    }),
    glue::glue("") ## add blank line after each item group
  )
}

#' Specify `FireDamageTable` for fire extensions
#'
#' @param df data.frame
#'
#' @template return_insert
#'
#' @keywords internal
insertFireDamageTable <- function(df = NULL) {
  if (is.null(df)) {
    df <- data.frame(
      CohortAgePercentLongevity = c(20, 50, 85, 100),
      FireSeverityMinusFireTolerance = c(-2, -1, 0, 1)
    )
  }

  c(
    glue::glue("FireDamageTable"),
    glue::glue(">> Cohort Age      FireSeverity - "),
    glue::glue(">> % of longevity  FireTolerance"),
    glue::glue(">> --------------  ---------------"),
    apply(df, 1, function(x) {
      glue::glue_collapse(x, sep = "%      ")
    }),
    glue::glue("") ## add blank line after each item group
  )
}

#' Create Map Names pattern for file outputs
#'
#' @param type Character, specifying the output file type (e.g., 'severity').
#'
#' @param ext_type Character, specifying the output extension type (e.g., 'fire').
#'
#' @template param_path
#'
#' @returns Character, specifying filename pattern for map outputs.
#'
#' @export
#' @examples
#' SeverityMaps <- MapNames("severity", "fire")
#' PctConiferMaps <- MapNames("PctConifer", "fire")
#' PctDeadFirMaps <- MapNames("PctDeadFir", "fire")
#'
MapNames <- function(type, ext_type, path = ".") {
  path <- fs::path_rel(file.path(path, ext_type), path)

  ## NOTE: careful using glue() here; need literal {timestep}, so use {{timestep}}
  glue::glue("{path}/{type}-{{timestep}}.tif")
}
