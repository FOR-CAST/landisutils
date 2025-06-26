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

  return(landis_console[1])
}

#' Run a LANDIS-II simulation from the R session
#'
#' Run
#'
#' @param scenario `LandisScenario` object
#'
#' @param landis_console character, specifying path to LANDIS-II console executable.
#'
#' @returns \pkg{callr} background R process
#'
#' @seealso
#' - <https://callr.r-lib.org/index.html#background-r-processes>
#'
#' @export
landis_run <- function(scenario = NULL, landis_console = NULL) {
  landis_console %||% landis_find()

  stopifnot(
    !is(scenario, "LandisScenario")
  )

  scenario_path <- scenario$path
  scenario_file <- scenario$files[1]

  message(glue::glue("Starting LANDIS-II run ({Sys.time()})"))

  landis_process <- callr::r_bg(
    func = function(scenario_file, scenario_path, landis_console) {
      withr::with_dir(
        scenario_path,
        system2(
          Sys.which("dotnet"),
          glue::glue("{landis_console} {scenario_file}"),
          wait = TRUE
        )
      )
    },
    args = list(scenario_file, scenario_path, landis_console)
  )

  return(landis_process)
}
