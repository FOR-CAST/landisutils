## LANDIS-II execution helpers (local and Docker) --------------------------------------------------

#' Run a LANDIS-II simulation locally (synchronous)
#'
#' Runs LANDIS-II directly via `dotnet`, blocking until the simulation
#' completes. Stdout and stderr are written to
#' `<scenario_dir>/log/local_stdout.log` and `local_stderr.log`.
#'
#' @param scenario_dir Character. Path to the scenario directory (resolved to
#'   absolute before use).
#' @param scenario_file Character. Scenario filename relative to `scenario_dir`.
#' @param console Character or `NULL`. Path to `Landis.Console.dll`. Defaults
#'   to `NULL`, which calls [landis_find()] at run time.
#'
#' @returns Integer exit code, invisibly.
#'
#' @family LANDIS-II execution helpers
#' @seealso [landis_find()], [landis_find_docker()], [landis_run()], [landis_run_docker()], [tar_landis()]
#' @export
landis_run_local <- function(scenario_dir, scenario_file = "scenario.txt", console = NULL) {
  console <- console %||% landis_find()

  scenario_dir <- fs::path_real(scenario_dir)

  if (!fs::file_exists(fs::path(scenario_dir, scenario_file))) {
    stop(
      sprintf("scenario file not found: %s", fs::path(scenario_dir, scenario_file)),
      call. = FALSE
    )
  }

  log_dir <- fs::dir_create(fs::path(scenario_dir, "log"))

  message(glue::glue("Starting LANDIS-II local run ({Sys.time()})"))
  message(glue::glue("  scenario_dir:  {scenario_dir}"))
  message(glue::glue("  console:       {console}"))

  old_wd <- setwd(scenario_dir)
  on.exit(setwd(old_wd), add = TRUE)

  rc <- system2(
    "dotnet",
    args = c(console, scenario_file),
    stdout = fs::path(log_dir, "local_stdout.log"),
    stderr = fs::path(log_dir, "local_stderr.log"),
    wait = TRUE
  )

  if (rc != 0L) {
    stop(
      sprintf("LANDIS-II local run failed (exit code %d). Check logs:\n  %s", rc, log_dir),
      call. = FALSE
    )
  }

  message(glue::glue("LANDIS-II local run completed ({Sys.time()})"))
  invisible(rc)
}

#' Run a LANDIS-II simulation inside a Docker container
#'
#' Runs LANDIS-II in an ephemeral Docker container, blocking until the
#' simulation completes. The scenario directory is bind-mounted to `/sim`
#' inside the container. Stdout and stderr are written to
#' `<scenario_dir>/log/docker_stdout.log` and `docker_stderr.log`.
#'
#' @param scenario_dir Character. Path to the scenario directory (resolved to
#'   absolute before mounting).
#' @param scenario_file Character. Scenario filename relative to `scenario_dir`.
#' @param image Character or `NULL`. Docker image reference. Defaults to
#'   `getOption("landisutils.docker.image")` (set by `.onLoad()` to
#'   `"ghcr.io/landis-ii-foundation/landis-ii-v8-release:main"`).
#' @param console Character or `NULL`. Path to `Landis.Console.dll` **inside
#'   the container**. Defaults to `NULL`, which calls [landis_find_docker()] at
#'   run time (reads `getOption("landisutils.docker.console")`).
#'
#' @returns Integer exit code, invisibly.
#'
#' @family LANDIS-II execution helpers
#' @seealso [landis_find_docker()], [landis_find()], [landis_run()], [landis_run_local()], [tar_landis()]
#' @export
landis_run_docker <- function(
  scenario_dir,
  scenario_file = "scenario.txt",
  image = NULL,
  console = NULL
) {
  image <- image %||% getOption("landisutils.docker.image")
  console <- console %||% landis_find_docker()

  scenario_dir <- fs::path_real(scenario_dir)

  if (!fs::file_exists(fs::path(scenario_dir, scenario_file))) {
    stop(
      sprintf("scenario file not found: %s", fs::path(scenario_dir, scenario_file)),
      call. = FALSE
    )
  }

  log_dir <- fs::dir_create(fs::path(scenario_dir, "log"))

  uid <- trimws(system("id -u", intern = TRUE))
  gid <- trimws(system("id -g", intern = TRUE))

  message(glue::glue("Starting LANDIS-II Docker run ({Sys.time()})"))
  message(glue::glue("  scenario_dir:  {scenario_dir}"))
  message(glue::glue("  scenario_file: {scenario_file}"))
  message(glue::glue("  image:         {image}"))

  rc <- system2(
    command = "docker",
    args = c(
      "run",
      "--rm",
      "--entrypoint",
      "dotnet",
      "--user",
      paste0(uid, ":", gid),
      "-v",
      paste0(scenario_dir, ":/sim"),
      "-w",
      "/sim",
      image,
      console,
      scenario_file
    ),
    stdout = fs::path(log_dir, "docker_stdout.log"),
    stderr = fs::path(log_dir, "docker_stderr.log"),
    wait = TRUE
  )

  if (rc != 0L) {
    stop(
      sprintf("LANDIS-II Docker run failed (exit code %d). Check logs:\n  %s", rc, log_dir),
      call. = FALSE
    )
  }

  message(glue::glue("LANDIS-II Docker run completed ({Sys.time()})"))
  invisible(rc)
}

#' Create a `targets` target that runs LANDIS-II
#'
#' A `{targets}` factory that creates one `format = "file"` target per
#' scenario (via dynamic branching). The target runs LANDIS-II — locally or
#' inside a Docker container depending on `method` — and returns a character
#' vector of tracked output and log files.
#'
#' The `method` is resolved at *factory-call time* (i.e., when `_targets.R` is
#' evaluated) and baked into the target command. This ensures that
#' `{crew}` worker processes, which do not inherit R session options, receive
#' the correct values. Set `options(landisutils.run.method = ...)` in
#' `_local.R` before the pipeline list is constructed to control the method
#' per machine.
#'
#' @param name Symbol (unquoted). Target name.
#' @param scenario_dir Symbol or expression (unquoted). Upstream target that
#'   provides the scenario directory path(s) at run time.
#' @param deps List (unquoted, optional). A `list()` of upstream target
#'   symbols that must complete before the simulation runs, e.g.
#'   `list(landis_scenario_file, landis_ext_forcs_file)`. Values are not used
#'   directly — they are embedded in the command so `{targets}` detects them
#'   as upstream dependencies.
#' @param scenario_file Character. Scenario filename inside `scenario_dir`.
#' @param output_dir Character. Output subdirectory inside `scenario_dir`;
#'   all files found there (recursively) are returned as tracked outputs.
#' @param method Character or `NULL`. `"docker"` to run in Docker,
#'   `"local"` to run via a local `dotnet` installation. `NULL` (default)
#'   reads `getOption("landisutils.run.method")`, which itself defaults to
#'   `"local"` on Windows and `"docker"` on Linux/macOS.
#' @param image Character or `NULL`. Docker image to use
#'   (`method = "docker"` only). Resolved from
#'   `getOption("landisutils.docker.image")` when `NULL`.
#' @param console Character or `NULL`. Path to `Landis.Console.dll`.
#'   For `method = "docker"` this is the path *inside* the container;
#'   for `method = "local"` it is the local filesystem path (defaults to
#'   [landis_find()]).
#' @param n_reps Integer. Number of replicate runs per scenario. Each replicate
#'   is placed in `<scenario_dir>/rep01`, `/rep02`, … (subdirectories of the
#'   base scenario directory) so input files can be shared. LANDIS runs
#'   independently in each replicate directory. Output files from all
#'   replicates are returned as a single character vector. Defaults to `1L`.
#' @param base_seed Integer or `NULL`. Passed to [landis_replicate()]: when
#'   non-`NULL`, the `RandomNumberSeed` in each replicate's `scenario.txt` is
#'   set to `base_seed + (rep_index - 1)`, giving each run a distinct but
#'   deterministic seed. Adding more replicates later never changes existing
#'   seeds because seeds are derived from the rep index, not the order of
#'   creation.
#' @param pattern Expression (unquoted, optional). Dynamic-branching pattern,
#'   e.g. `map(landis_run_name)`. Passed to [targets::tar_target_raw()].
#' @param packages,library,error,memory,resources,storage,retrieval,cue,description
#'   Standard `{targets}` options; all default to [targets::tar_option_get()].
#'
#' @returns A `tar_target` object (from [targets::tar_target_raw()]).
#'
#' @family LANDIS-II execution helpers
#' @seealso [landis_run_local()], [landis_run_docker()]
#' @export
tar_landis <- function(
  name,
  scenario_dir,
  deps = NULL,
  scenario_file = "scenario.txt",
  output_dir = "output",
  method = NULL,
  image = NULL,
  console = NULL,
  n_reps = 1L,
  base_seed = NULL,
  pattern = NULL,
  packages = targets::tar_option_get("packages"),
  library = targets::tar_option_get("library"),
  error = targets::tar_option_get("error"),
  memory = targets::tar_option_get("memory"),
  resources = targets::tar_option_get("resources"),
  storage = targets::tar_option_get("storage"),
  retrieval = targets::tar_option_get("retrieval"),
  cue = targets::tar_option_get("cue"),
  description = targets::tar_option_get("description")
) {
  if (!requireNamespace("targets", quietly = TRUE)) {
    stop(
      "Package 'targets' is required for tar_landis(). Install it with: install.packages('targets')",
      call. = FALSE
    )
  }

  ## Capture unevaluated caller expressions before any evaluation occurs.
  name_str <- deparse(substitute(name))
  scenario_dir_expr <- substitute(scenario_dir)
  deps_expr <- substitute(deps)
  pattern_expr <- substitute(pattern)

  ## Resolve n_reps and base_seed at factory-call time so the values are baked
  ## into the command. crew workers don't inherit R session state.
  n_reps_val <- as.integer(n_reps)
  base_seed_val <- if (is.null(base_seed)) NULL else as.integer(base_seed)

  ## Resolve method, image at factory-call time so the values are baked into
  ## the command expression. crew workers don't inherit R session options, so
  ## resolution must happen here (in the main process, after _local.R is sourced).
  method <- method %||%
    getOption(
      "landisutils.run.method",
      default = if (.Platform$OS.type == "windows") "local" else "docker"
    )
  image <- image %||%
    getOption(
      "landisutils.docker.image",
      default = "ghcr.io/landis-ii-foundation/landis-ii-v8-release:main"
    )

  ## Build the command expression.
  ##
  ## .deps forces {targets} dependency detection: the package scans the
  ## command for symbol references, so embedding the list() of upstream
  ## target names here is all that is needed. The assignment itself is a
  ## no-op at execution time.
  ##
  ## landis_replicate() creates n_reps subdirectories inside the base scenario
  ## directory (rep01/, rep02/, …), copies input files into each, and returns
  ## their absolute paths. LANDIS runs in each replicate directory; output and
  ## log files from all replicates are returned as a combined character vector.
  cmd <- if (identical(method, "docker")) {
    bquote({
      .deps <- .(deps_expr)
      .sd <- as.character(.(scenario_dir_expr))
      ## pass the dep file values as the explicit copy list so only tracked
      ## input files (+ their GDAL sidecars) are placed in each replicate dir
      .dep_files <- Filter(is.character, .deps) |> unlist()
      .rep_dirs <- landisutils::landis_replicate(
        .sd,
        .(n_reps_val),
        files = .dep_files,
        base_seed = .(base_seed_val)
      )
      for (.rd in .rep_dirs) {
        landisutils::landis_run_docker(
          scenario_dir = .rd,
          scenario_file = .(scenario_file),
          image = .(image),
          console = .(console)
        )
      }
      unlist(lapply(.rep_dirs, function(.rd) {
        c(
          list.files(file.path(.rd, "log"), full.names = TRUE),
          list.files(file.path(.rd, .(output_dir)), full.names = TRUE, recursive = TRUE)
        )
      }))
    })
  } else {
    bquote({
      .deps <- .(deps_expr)
      .sd <- as.character(.(scenario_dir_expr))
      .dep_files <- Filter(is.character, .deps) |> unlist()
      .rep_dirs <- landisutils::landis_replicate(
        .sd,
        .(n_reps_val),
        files = .dep_files,
        base_seed = .(base_seed_val)
      )
      for (.rd in .rep_dirs) {
        landisutils::landis_run_local(
          scenario_dir = .rd,
          scenario_file = .(scenario_file),
          console = .(console)
        )
      }
      unlist(lapply(.rep_dirs, function(.rd) {
        c(
          list.files(file.path(.rd, "log"), full.names = TRUE),
          list.files(file.path(.rd, .(output_dir)), full.names = TRUE, recursive = TRUE)
        )
      }))
    })
  }

  targets::tar_target_raw(
    name = name_str,
    command = cmd,
    pattern = pattern_expr,
    packages = packages,
    library = library,
    format = "file",
    error = error,
    memory = memory,
    resources = resources,
    storage = storage,
    retrieval = retrieval,
    cue = cue,
    description = description
  )
}
