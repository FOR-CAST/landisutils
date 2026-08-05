#' Create replicate directories for a LANDIS-II scenario
#'
#' Creates numbered replicate sub-directories inside `scenario_dir`
#' (`rep01/`, `rep02/`, ...) and copies input files into each.
#'
#' Two modes:
#' - **All-at-once** (`n_reps`): creates `rep01` through `repNN`. Used by
#'   legacy code and interactive workflows.
#' - **Single-rep** (`rep_index`): creates exactly one directory for the
#'   given 1-based index. Used by [tar_landis()] when each replicate is a
#'   separate targets branch; branches run concurrently and each sets up only
#'   its own directory, avoiding the O(N_REPS) re-setup cost of all-at-once.
#'
#' If `files` is supplied it is used as the explicit copy list; GDAL sidecar
#' files (`.tfw`, `.aux.xml`) that accompany any `.tif` in the list are
#' included automatically. When `files` is `NULL` the fallback is every
#' top-level file in `scenario_dir` (sub-directories are never copied).
#' The function is idempotent: existing replicate directories are left
#' untouched, so adding more replicates later never alters previously-created
#' ones.
#'
#' When `base_seed` is provided the `RandomNumberSeed` line in each rep's
#' `scenario.txt` is rewritten to `base_seed + (rep_index - 1)`, giving every
#' replicate a distinct but deterministic seed. Because seeds are derived from
#' the rep index (not the order of creation), adding replicates later never
#' changes existing seeds.
#'
#' @param scenario_dir Character. Path to the base scenario directory.
#' @param n_reps Integer or `NULL`. Number of replicates to create (all-at-once
#'   mode). Exactly one of `n_reps` and `rep_index` must be provided.
#' @param rep_index Integer or `NULL`. 1-based index of the single replicate to
#'   create (single-rep mode). Exactly one of `n_reps` and `rep_index` must be
#'   provided.
#' @param files Character vector or `NULL`. Explicit set of file paths to copy
#'   into each replicate directory. Typically the values of the `deps` targets
#'   passed to [tar_landis()] (already absolute paths). When `NULL`, all
#'   top-level files in `scenario_dir` are copied.
#' @param base_seed Integer or `NULL`. When non-`NULL`, the `RandomNumberSeed`
#'   in each copied `scenario.txt` is set to `base_seed + (rep_index - 1)`.
#'   Rep indices are 1-based (`rep01` -> `base_seed`, `rep02` -> `base_seed + 1`,
#'   ...).
#'
#' @returns Character vector of absolute paths to the created replicate
#'   directories (length 1 in single-rep mode, length `n_reps` in all-at-once
#'   mode), in replicate order.
#'
#' @family LANDIS-II execution helpers
#' @seealso [tar_landis()], [landis_run_docker()], [landis_run_local()]
#' @export
landis_replicate <- function(
  scenario_dir,
  n_reps = NULL,
  rep_index = NULL,
  files = NULL,
  base_seed = NULL
) {
  if (is.null(n_reps) == is.null(rep_index)) {
    stop("Exactly one of 'n_reps' or 'rep_index' must be provided.", call. = FALSE)
  }

  scenario_dir <- fs::path_real(scenario_dir)
  indices <- if (!is.null(rep_index)) as.integer(rep_index) else seq_len(as.integer(n_reps))
  rep_dirs <- fs::path(scenario_dir, sprintf("rep%02d", indices))

  if (is.null(files)) {
    ## fallback: every top-level file in the scenario dir (skip sub-directories)
    src_files <- fs::dir_ls(scenario_dir, type = "file")
  } else {
    src_files <- fs::path_abs(as.character(files))
    ## auto-include GDAL sidecar files that accompany any tracked .tif
    tif_files <- src_files[grepl("\\.tif$", src_files, ignore.case = TRUE)]
    sidecars <- fs::path(c(
      paste0(tif_files, ".aux.xml"),
      sub("\\.tif$", ".tfw", tif_files, ignore.case = TRUE)
    ))
    src_files <- unique(c(src_files, sidecars[fs::file_exists(sidecars)]))
    src_files <- src_files[fs::file_exists(src_files)]
  }

  for (i in seq_along(indices)) {
    rep_dir <- rep_dirs[[i]]
    ## Always re-stage: create the rep dir if missing AND overwrite any
    ## existing input files copied from a prior run. Previously the entire
    ## block was guarded by `if (!fs::dir_exists(rep_dir))`, which meant a
    ## rep dir left behind by a failed run would silently keep its stale
    ## input files on the next `landis_replicate()` call -- LANDIS-II would
    ## then run against an old `dynamic-fire.txt` / `scenario.txt` even
    ## though `tar_make` had rebuilt those files in the scenario template.
    ## Only LANDIS-II output files (`Landis-log.txt`, `log_*.csv`, the
    ## `log/`, `fire/`, etc. subdirs) survive across re-stages; LANDIS-II
    ## overwrites those itself during the run.
    fs::dir_create(rep_dir)
    fs::file_copy(src_files, rep_dir, overwrite = TRUE)

    ## per-replicate RandomNumberSeed: deterministic by rep index so that
    ## adding more reps later never shifts seeds for existing replicates
    if (!is.null(base_seed)) {
      scenario_txt <- fs::path(rep_dir, "scenario.txt")
      if (fs::file_exists(scenario_txt)) {
        seed_i <- as.integer(base_seed) + (indices[[i]] - 1L)
        lines <- readLines(scenario_txt)
        ## matches both the commented form (>> RandomNumberSeed ...) written
        ## by insertRandomNumberSeed(NULL) and any active seed line
        lines <- sub(
          "^(>>\\s+)?RandomNumberSeed\\s.*",
          sprintf("RandomNumberSeed    %d  << optional parameter", seed_i),
          lines
        )
        writeLines(lines, scenario_txt)
      }
    }
  }
  rep_dirs
}

#' Find the LANDIS-II console for a local installation
#'
#' Returns the path to `Landis.Console.dll` for a locally-installed LANDIS-II.
#' Resolution order: the `LANDIS_CONSOLE` environment variable, then a
#' filesystem search under `/opt` for a `build/Release/` path.
#'
#' The `/opt` search is a Linux convention and finds nothing on Windows, where
#' `LANDIS_CONSOLE` is the only route. `method = "local"` is the default there
#' (see [tar_landis()]), so a Windows user must set that variable.
#'
#' @param check_version Logical. When `TRUE` (default), verify the console's
#'   major version with [landis_version()] and stop if it is not
#'   `required_major`. A version that cannot be determined only warns.
#' @param required_major Integer. Required LANDIS-II major version.
#'
#' @returns Character. Path to `Landis.Console.dll`, or `NA_character_` if not
#'   found.
#'
#' @family LANDIS-II execution helpers
#' @seealso [landis_version()], [landis_find_docker()], [landis_run_local()],
#'   [tar_landis()]
#' @export
landis_find <- function(check_version = TRUE, required_major = 8L) {
  ## An explicit LANDIS_CONSOLE wins. The condition used to be inverted: when
  ## the variable WAS set its value was discarded and replaced by the /opt
  ## search, and when it was unset the function returned "". On Windows, where
  ## /opt does not exist, that meant local runs could never find the console.
  landis_console <- Sys.getenv("LANDIS_CONSOLE")

  if (!nzchar(landis_console)) {
    hits <- list.files("/opt", "Landis[.]Console[.]dll$", full.names = TRUE, recursive = TRUE) |>
      grep(x = _, pattern = "/build/Release/", value = TRUE)

    landis_console <- if (length(hits) == 0L) NA_character_ else hits[[1L]]
  }

  if (isTRUE(check_version) && !is.na(landis_console)) {
    .assert_landis_major(landis_console, required_major)
  }

  landis_console
}

## Stop when the console is a major version this package's input writers do not
## target. v7 and v8 differ in the input-file formats (initial communities moved
## to a CSV + raster pair, ForCS gained the SpinUp BiomassSpinUpFlag column and
## the map-control block, core species.txt dropped shade/fire tolerance), so a v7
## console silently mis-parses v8 inputs rather than failing cleanly.
.assert_landis_major <- function(console, required_major) {
  version <- landis_version(console, check_version = FALSE)

  if (is.na(version)) {
    warning(
      sprintf(
        paste0(
          "could not determine the LANDIS-II version of '%s'; proceeding, but the input ",
          "writers in this package target v%d and v7 input formats differ"
        ),
        console,
        as.integer(required_major)
      ),
      call. = FALSE
    )
    return(invisible(NULL))
  }

  major <- as.integer(unclass(version)[[1L]][1L])
  if (!identical(major, as.integer(required_major))) {
    stop(
      sprintf(
        paste0(
          "LANDIS-II v%s found at '%s', but this package writes v%d input files. ",
          "v7 and v8 input formats differ (initial communities, ForCS SpinUp and map ",
          "control, core species.txt), so a mismatched console mis-parses the inputs ",
          "rather than failing cleanly. Point LANDIS_CONSOLE at a v%d install."
        ),
        as.character(version),
        console,
        as.integer(required_major),
        as.integer(required_major)
      ),
      call. = FALSE
    )
  }
  invisible(NULL)
}

#' Report the version of a local LANDIS-II console
#'
#' Runs the console with no scenario file. It prints its version banner --
#' `LANDIS-II 8.0 (8)` -- and then exits with an error about the missing
#' scenario, so a non-zero status is expected and is not treated as failure.
#'
#' @param console Character or `NULL`. Path to `Landis.Console.dll`; resolved
#'   via [landis_find()] when `NULL`.
#' @param timeout Numeric. Seconds to wait for the console to print its banner.
#' @param check_version Logical. Passed to [landis_find()] when `console` is
#'   `NULL`; `FALSE` here avoids infinite recursion.
#'
#' @returns A [numeric_version], or `NA` when the console, `dotnet`, or the
#'   banner cannot be found.
#'
#' @family LANDIS-II execution helpers
#' @seealso [landis_find()]
#' @export
landis_version <- function(console = NULL, timeout = 60, check_version = FALSE) {
  console <- console %||% landis_find(check_version = check_version)

  if (length(console) != 1L || is.na(console) || !nzchar(console) || !file.exists(console)) {
    return(NA)
  }

  dotnet <- Sys.which("dotnet")
  if (!nzchar(dotnet)) {
    return(NA)
  }

  out <- tryCatch(
    processx::run(
      dotnet,
      args = console,
      wd = dirname(console),
      error_on_status = FALSE, ## "No scenario file specified." is expected
      timeout = timeout
    ),
    error = function(e) NULL
  )
  if (is.null(out)) {
    return(NA)
  }

  txt <- paste(out$stdout, out$stderr)
  hit <- regmatches(txt, regexpr("LANDIS-II[[:space:]]+[0-9]+(\\.[0-9]+)*", txt))
  if (length(hit) == 0L) {
    return(NA)
  }

  tryCatch(numeric_version(sub("^LANDIS-II[[:space:]]+", "", hit[[1L]])), error = function(e) NA)
}

#' Find the LANDIS-II console path inside a Docker container
#'
#' Returns the path to `Landis.Console.dll` **inside the container**.
#' Reads `getOption("landisutils.docker.console")`, which `.onLoad()` initializes
#' to the standard path used by the official LANDIS-II v8 Docker images.
#' Override the option in `_local.R` when using a non-standard image layout.
#'
#' @returns Character. Path to `Landis.Console.dll` inside the container.
#'
#' @family LANDIS-II execution helpers
#' @seealso [landis_find()], [landis_run_docker()], [tar_landis()]
#' @export
landis_find_docker <- function() {
  getOption(
    "landisutils.docker.console",
    default = "/opt/landis-ii/Core-Model-v8-LINUX/build/Release/Landis.Console.dll"
  )
}

#' Run a LANDIS-II simulation from the R session
#'
#' @param scenario_file character specifying the filename of a scenario file
#'
#' @param scenario_path character specifying the path to the directory containing the scenario file
#'
#' @param landis_console character, specifying path to LANDIS-II console executable.
#'
#' @returns \pkg{callr} background R process
#'
#' @note Users should call `landis_run()` (which wraps `landis_process`) rather than calling
#' `landis_process` directly.
#'
#' @seealso
#' - <https://callr.r-lib.org/index.html#background-r-processes>
#'
#' @export
#' @rdname landis_run
landis_process <- function(scenario_file, scenario_path, landis_console) {
  message(glue::glue("Starting LANDIS-II run ({Sys.time()})"))

  log_path <- file.path(scenario_path, "log") |> fs::dir_create()

  callr::r_bg(
    func = function(scenario_file, scenario_path, landis_console) {
      withr::with_dir(
        scenario_path,
        system2(Sys.which("dotnet"), glue::glue("{landis_console} {scenario_file}"), wait = TRUE)
      )
    },
    args = list(scenario_file, scenario_path, landis_console),
    stdout = file.path(log_path, "callr_stdout.log") |> fs::path_rel(),
    stderr = file.path(log_path, "callr_stderr.log") |> fs::path_rel(),
    supervise = TRUE
  )
}

#' @param scenario `LandisScenario` object
#'
#' @param rep integer, replicate id
#'
#' @export
#' @rdname landis_run
landis_run <- function(scenario = NULL, rep = NULL, landis_console = NULL) {
  landis_console %||% landis_find()

  stopifnot(is(scenario, "LandisScenario"))

  if (is.null(rep)) {
    scenario_path <- scenario$path
    scenario_file <- scenario$files[1]
  } else {
    scenario_path <- file.path(scenario$path, sprintf("rep%02d", rep))
    scenario_file <- scenario$files[1]
  }

  landis_process(scenario_file, scenario_path, landis_console)
}
