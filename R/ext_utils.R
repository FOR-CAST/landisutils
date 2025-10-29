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
#' @export
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

#' @export
#' @rdname insertDynamicTable
insertDynamicEcoregionTable <- function(df = NULL) {
  insertDynamicTable("DynamicEcoregionTable", df)
}

#' @export
#' @rdname insertDynamicTable
insertDynamicFireRegionsTable <- function(df = NULL) {
  insertDynamicTable("DynamicFireRegionTable", df)
}

#' Specify `FireDamageTable` for fire extensions
#'
#' @param df data.frame
#'
#' @template return_insert
#'
#' @export
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

#' Specify Fire Severity Map Names for fire extensions
#'
#' @template param_path
#'
#' @template return_insert
#'
#' @export
insertMapNames <- function(path) {
  path <- fs::path_rel(file.path(path, "fire"), path)

  ## NOTE: careful using glue() here; need literal {timestep}, so use {{timestep}}
  file <- glue::glue("{path}/severity-{{timestep}}.tif")

  insertFile("MapNames", file)
}

#' Specify `LogFile` for fire extensions
#'
#' @template param_file
#'
#' @template return_insert
#'
#' @export
insertLogFile <- function(file = "fire/log.csv") {
  insertFile("LogFile", file)
}

#' Specify `SummaryLogFile` for fire extensions
#'
#' @template param_file
#'
#' @template return_insert
#'
#' @export
insertSummaryLogFile <- function(file = "fire/summary-log.csv") {
  insertFile("SummaryLogFile", file)
}
