#' Find an installed version of LANDIS-II
#'
#' @returns Character vector. File path(s), if found, to any LANDIS-II executables.
#'
#' @export
landis_find <- function() {
  browser()
  ## TODO: how to find LANDIS on Windows and on Linux? would it be on the PATH?
  ## TODO: should we run this check on load/attach, and store the path for subsequent retrieval?
}

#' Run a LANDIS-II simulation from the R session
#'
#' @param scenario_file
#'
#' @param landis_console Character. Path to LANDIS-II console executable.
#'
#' @template LANDIS_version
#'
#' @returns List of output files produced by the LANDIS-II simulation run.
#'
#' @export
landis_run <- function(scenario_file = NULL, landis_console = NULL) {
  stopifnot(
    !is.null(scenario_file), file.exists(scenario_file)
  )

  scenario_dir <- dirname(scenario_file)

  if (is.null(landis_console)) {
    landis_console <- landis_find() ## TODO: if multiple found, select the first one? or error?
  }
browser() ## TODO: setwd for landis console execution
  landis_result <- system2(landis_console, scenario_file, wait = TRUE)
  ## TODO: check execution status, though presumably landis errors will propagate to R session

  output_files <- fs::dir_ls(scenario_dir) ## TODO: only look in the output directories? omit inputs

  return(output_files)
}
