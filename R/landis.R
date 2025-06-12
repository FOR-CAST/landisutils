#' Find an installed version of LANDIS-II
#'
#' @returns Character vector. File path(s), if found, to any LANDIS-II executables.
#'
#' To ensure this function can find your LANDIS-II installation, please ensure that the
#' `LANDIS_CONSOLE` environment variable is set and accessible by in R
#'  (e.g., `Sys.getenv("LANDIS_CONSOLE")` should return a non-empty value).
#'
#' @export
landis_find <- function() {
  ## just search the typical places
  landis_console <- Sys.getenv("LANDIS_CONSOLE")
  if (nzchar(landis_console)) {
    landis_console <- list.files("/opt", "Landis[.]Console[.]dll$", full.names = TRUE, recursive = TRUE) |>
      grep(x = _, pattern = "/build/Release/", value = TRUE)
  }

  return(landis_console)
}

#' Run a LANDIS-II simulation from the R session
#'
#' @param scenario_file character, specifying path to scenario file.
#'
#' @param landis_console character, specifying path to LANDIS-II console executable.
#'
#' @returns List of output files produced by the LANDIS-II simulation run.
#'
#' @export
landis_run <- function(scenario_file = NULL, landis_console = NULL) {
  stopifnot(
    !is.null(scenario_file), file.exists(scenario_file)
  )

  scenario_dir <- dirname(scenario_file)

  landis_console %||% landis_find() ## TODO: if multiple found, select the first one? or error?

  message(glue::glue("Starting LANDIS-II run ({Sys.time()})"))

  landis_result <- withr::local_dir(
    system2(
      Sys.which("dotnet"),
      glue::glue("{landis_console} {scenario_file}"),
      wait = TRUE
    )
  )

  output_files <- fs::dir_ls(scenario_dir) ## TODO: only look in the output directories? omit inputs

  return(output_files)
}
