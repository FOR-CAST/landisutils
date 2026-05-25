#' Ensure percent symbol appended to table values
#'
#' @param x Character
#'
#' @export
appendPercent <- function(x) {
  ifelse(stringr::str_ends(x, stringr::fixed("%")), x, stringr::str_c(x, "%"))
}

#' Allowed `SeedingAlgorithm` values
#'
#' Used by every LANDIS-II succession extension that exposes a
#' `SeedingAlgorithm` field.
#'
#' @keywords internal
.seedingAlgorithms <- c("WardSeedDispersal", "NoDispersal", "UniversalDispersal")

#' Validate a `SeedingAlgorithm` value
#'
#' Internal helper used by the active bindings of every succession extension
#' that exposes a `SeedingAlgorithm` field. Raises an informative error if
#' `value` is not one of `.seedingAlgorithms`.
#'
#' @param value Character. Candidate value.
#'
#' @return `value`, invisibly, when valid.
#'
#' @keywords internal
.checkSeedingAlgorithm <- function(value) {
  if (!value %in% .seedingAlgorithms) {
    stop(
      "SeedingAlgorithm must be one of: ",
      paste(.seedingAlgorithms, collapse = ", "),
      "; got '",
      value,
      "'"
    )
  }
  invisible(value)
}

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

#' Create sample Fire Damage Table
#'
#' @returns `data.frame` with 2 columns:
#'   `CohortAgePercentLongevity` and `FireSeverityMinusFireTolerance`.
#'
#' @export
defaultFireDamageTable <- function() {
  data.frame(
    CohortAgePercentLongevity = c(20, 50, 85, 100),
    FireSeverityMinusFireTolerance = c(-2, -1, 0, 1)
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
    df <- defaultFireDamageTable()
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

#' Collapse one row of a labelled data frame into a separator-joined string
#'
#' Drops the first (label) column of `df`, applies `fmt` to each remaining
#' cell, and collapses the result with `sep`. Used by succession-extension
#' table writers (e.g. `MinRelativeBiomass`, `SufficientLight`).
#'
#' @param df  data.frame whose first column is a row label and whose
#'   remaining columns hold the values to emit.
#' @param i   integer row index.
#' @param fmt formatter applied to each cell; must return a length-1 character.
#'   Defaults to [as.character()].
#' @param sep separator between cells. Defaults to two spaces.
#'
#' @keywords internal
.collapseRow <- function(df, i, fmt = as.character, sep = "  ") {
  glue::glue_collapse(vapply(df[i, -1, drop = FALSE], fmt, character(1L)), sep = sep)
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
