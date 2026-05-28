## LANDIS-II execution helpers (local and Docker) --------------------------------------------------

## Format an integer number of seconds as "Xh Ym Zs" / "Ym Zs" / "Zs".
.fmt_duration <- function(seconds) {
  s <- as.integer(round(seconds))
  h <- s %/% 3600L
  m <- (s %% 3600L) %/% 60L
  sc <- s %% 60L
  if (h > 0L) {
    sprintf("%dh %02dm %02ds", h, m, sc)
  } else if (m > 0L) {
    sprintf("%dm %02ds", m, sc)
  } else {
    sprintf("%ds", sc)
  }
}

## Format a byte count as "X.X GiB" / "X.X MiB" / etc.
.fmt_bytes <- function(bytes) {
  if (bytes >= 1024^3) {
    sprintf("%.1f GiB", bytes / 1024^3)
  } else if (bytes >= 1024^2) {
    sprintf("%.1f MiB", bytes / 1024^2)
  } else if (bytes >= 1024) {
    sprintf("%.1f KiB", bytes / 1024)
  } else {
    sprintf("%d B", as.integer(bytes))
  }
}

## Parse the current-usage half of a docker-stats MemUsage string (e.g. "1.23GiB / 15.4GiB")
## into bytes. Returns 0 on parse failure.
.parse_docker_mem_bytes <- function(x) {
  usage <- trimws(strsplit(x, "/", fixed = TRUE)[[1L]][1L])
  m <- regmatches(usage, regexpr("[0-9.]+[A-Za-z]+", usage, perl = TRUE))
  if (!length(m)) {
    return(0)
  }
  val <- as.numeric(sub("([0-9.]+).*", "\\1", m[1L]))
  unit <- sub("[0-9.]+([A-Za-z]+)", "\\1", m[1L])
  mult <- c(
    B = 1,
    kB = 1e3,
    MB = 1e6,
    GB = 1e9,
    TB = 1e12,
    KiB = 1024,
    MiB = 1024^2,
    GiB = 1024^3,
    TiB = 1024^4
  )
  val * (mult[[unit]] %||% 1)
}

#' Run a LANDIS-II simulation locally (synchronous)
#'
#' Runs LANDIS-II directly via `dotnet`, blocking until the simulation
#' completes. Stdout and stderr are written to
#' `<scenario_dir>/log/local_stdout.log` and `local_stderr.log`.
#' Wall-clock elapsed time and peak memory use (polled every 2 s via the `ps` package)
#' are reported on completion and written to `<scenario_dir>/log/local_resources.log`.
#' Works on Linux, macOS, and Windows.
#'
#' @param scenario_dir Character. Path to the scenario directory (resolved to
#'   absolute before use).
#' @param scenario_file Character. Scenario filename relative to `scenario_dir`.
#' @param console Character or `NULL`. Path to `Landis.Console.dll`. Defaults
#'   to `NULL`, which calls [landis_find()] at run time.
#'
#' @returns Named list with `exit_code` (integer), `elapsed_sec` (numeric), and
#'   `peak_mem_bytes` (numeric), returned invisibly.
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

  ## processx gives us a PID for memory polling; wd = sets the working directory
  ## so LANDIS-II resolves relative file paths correctly (replaces setwd()).
  t_start <- proc.time()
  landis_proc <- processx::process$new(
    command = "dotnet",
    args = c(console, scenario_file),
    wd = scenario_dir,
    stdout = fs::path(log_dir, "local_stdout.log"),
    stderr = fs::path(log_dir, "local_stderr.log"),
    cleanup = TRUE
  )

  ## Poll RSS memory every 2 s using ps (cross-platform: Linux, macOS, Windows).
  peak_mem_bytes <- 0
  while (landis_proc$is_alive()) {
    mem <- tryCatch(
      ps::ps_memory_info(ps::ps_handle(landis_proc$get_pid()))[["rss"]],
      error = function(e) 0
    )
    peak_mem_bytes <- max(peak_mem_bytes, mem)
    Sys.sleep(2)
  }

  landis_proc$wait()
  rc <- landis_proc$get_exit_status()
  elapsed_sec <- (proc.time() - t_start)[["elapsed"]]

  writeLines(
    c(sprintf("elapsed_sec: %.1f", elapsed_sec), sprintf("peak_mem_bytes: %.0f", peak_mem_bytes)),
    fs::path(log_dir, "local_resources.log")
  )

  if (rc != 0L) {
    stop(
      sprintf("LANDIS-II local run failed (exit code %d). Check logs:\n  %s", rc, log_dir),
      call. = FALSE
    )
  }

  mem_str <- if (peak_mem_bytes > 0) {
    .fmt_bytes(peak_mem_bytes)
  } else {
    "(not sampled -- run too short)"
  }
  message(glue::glue("LANDIS-II local run completed ({Sys.time()})"))
  message(glue::glue("  Elapsed:     {.fmt_duration(elapsed_sec)}"))
  message(glue::glue("  Peak memory: {mem_str}"))

  invisible(list(exit_code = rc, elapsed_sec = elapsed_sec, peak_mem_bytes = peak_mem_bytes))
}

#' Run a LANDIS-II simulation inside a Docker container
#'
#' Runs LANDIS-II in an ephemeral Docker container, blocking until the
#' simulation completes. The scenario directory is bind-mounted to `/sim`
#' inside the container. Stdout and stderr are written to
#' `<scenario_dir>/log/docker_stdout.log` and `docker_stderr.log`. Wall-clock
#' elapsed time and peak memory use (polled every 2 s from `docker stats`) are
#' reported on completion and written to
#' `<scenario_dir>/log/docker_resources.log`.
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
#' @returns Named list with `exit_code` (integer), `elapsed_sec` (numeric), and
#'   `peak_mem_bytes` (numeric), returned invisibly.
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

  ## --user uid:gid is a POSIX-only concept; omit on Windows (Docker Desktop runs
  ## Linux containers via WSL2 and does not need the flag for file ownership).
  user_args <- if (.Platform$OS.type != "windows") {
    c(
      "--user",
      paste0(trimws(system("id -u", intern = TRUE)), ":", trimws(system("id -g", intern = TRUE)))
    )
  } else {
    character(0)
  }

  ## Unique name so docker stats can identify this specific container during the run.
  ## Include PID and a random suffix to prevent collisions when multiple reps run simultaneously.
  container_name <- paste0(
    "landis-run-",
    format(Sys.time(), "%Y%m%d%H%M%S"),
    "-",
    Sys.getpid(),
    "-",
    sample.int(.Machine$integer.max, 1L)
  )

  message(glue::glue("Starting LANDIS-II Docker run ({Sys.time()})"))
  message(glue::glue("  scenario_dir:  {scenario_dir}"))
  message(glue::glue("  scenario_file: {scenario_file}"))
  message(glue::glue("  image:         {image}"))

  docker_args <- c(
    "run",
    "--rm",
    "--name",
    container_name,
    "--entrypoint",
    "dotnet",
    user_args,
    "-v",
    paste0(scenario_dir, ":/sim"),
    "-w",
    "/sim",
    image,
    console,
    scenario_file
  )

  ## Run docker in a background R process so we can poll docker stats from the main thread.
  t_start <- proc.time()
  stdout_log <- fs::path(log_dir, "docker_stdout.log")
  stderr_log <- fs::path(log_dir, "docker_stderr.log")

  docker_proc <- callr::r_bg(
    func = function(docker_args, stdout_log, stderr_log) {
      system2("docker", docker_args, stdout = stdout_log, stderr = stderr_log, wait = TRUE)
    },
    args = list(docker_args = docker_args, stdout_log = stdout_log, stderr_log = stderr_log)
  )

  ## Poll docker stats every 2 s, tracking peak memory across samples.
  peak_mem_bytes <- 0
  while (docker_proc$is_alive()) {
    stats_raw <- tryCatch(
      system2(
        "docker",
        c("stats", "--no-stream", "--format", "{{.MemUsage}}", container_name),
        stdout = TRUE,
        stderr = FALSE
      ),
      error = function(e) character(0)
    )
    if (length(stats_raw) && nzchar(trimws(stats_raw[1L]))) {
      peak_mem_bytes <- max(peak_mem_bytes, .parse_docker_mem_bytes(stats_raw[1L]))
    }
    Sys.sleep(2)
  }

  rc <- docker_proc$get_result()
  elapsed_sec <- (proc.time() - t_start)[["elapsed"]]

  writeLines(
    c(sprintf("elapsed_sec: %.1f", elapsed_sec), sprintf("peak_mem_bytes: %.0f", peak_mem_bytes)),
    fs::path(log_dir, "docker_resources.log")
  )

  if (rc != 0L) {
    stop(
      sprintf("LANDIS-II Docker run failed (exit code %d). Check logs:\n  %s", rc, log_dir),
      call. = FALSE
    )
  }

  mem_str <- if (peak_mem_bytes > 0) {
    .fmt_bytes(peak_mem_bytes)
  } else {
    "(not sampled -- run too short)"
  }
  message(glue::glue("LANDIS-II Docker run completed ({Sys.time()})"))
  message(glue::glue("  Elapsed:     {.fmt_duration(elapsed_sec)}"))
  message(glue::glue("  Peak memory: {mem_str}"))

  invisible(list(exit_code = rc, elapsed_sec = elapsed_sec, peak_mem_bytes = peak_mem_bytes))
}

#' Create a `targets` target that runs LANDIS-II
#'
#' A `{targets}` factory that creates one `format = "file"` target per
#' scenario (via dynamic branching). The target runs LANDIS-II -- locally or
#' inside a Docker container depending on `method` -- and returns a character
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
#'   directly -- they are embedded in the command so `{targets}` detects them
#'   as upstream dependencies.
#' @param scenario_file Character. Scenario filename inside `scenario_dir`.
#' @param output_dir Character vector. Output subdirectory (or subdirectories)
#'   inside `scenario_dir`; all files found there (recursively) are returned as
#'   tracked outputs. Defaults to `"output"`. Pass `c("output", "fire")` when
#'   using the Dynamic Fire extension, which writes its maps and logs to a
#'   `fire/` subdirectory.
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
#'   is placed in `<scenario_dir>/rep01`, `/rep02`, ... (subdirectories of the
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
  output_dirs_val <- as.character(output_dir)

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
  ## directory (rep01/, rep02/, ...), copies input files into each, and returns
  ## their absolute paths. LANDIS runs in each replicate directory; output and
  ## log files from all replicates are returned as a combined character vector.
  cmd <- if (identical(method, "docker")) {
    bquote({
      .deps <- .(deps_expr)
      .sd <- as.character(.(scenario_dir_expr))
      ## pass the dep file values as the explicit copy list so only tracked
      ## input files (+ their GDAL sidecars) are placed in each replicate dir
      .dep_files <- Filter(is.character, .deps) |> unlist()
      ## targets may pass aggregate (all-branch) values for format="file" targets
      ## when branch hashes diverge after a dependency is recomputed.  Normalize
      ## to absolute paths first so that relative + absolute representations of
      ## the same file are caught by unique(), then deduplicate by basename,
      ## preferring files under .sd so the correct scenario-specific file always
      ## wins over a cross-scenario duplicate.
      .dep_files <- .dep_files |> fs::path_abs() |> unique()
      .dep_files <- .dep_files[fs::file_exists(.dep_files)]
      ## Append "/" so startsWith() matches only files *under* .sd, not files
      ## under a sibling directory whose name begins with the same string
      ## (e.g. phase_2_ICH_fire/ must not match prefix phase_2_ICH/).
      .sd_real <- paste0(fs::path_real(.sd), "/")
      .dep_files <- c(
        .dep_files[startsWith(.dep_files, .sd_real)],
        .dep_files[!startsWith(.dep_files, .sd_real)]
      )
      .dep_files <- .dep_files[!duplicated(basename(.dep_files))]
      .rep_dirs <- landisutils::landis_replicate(
        .sd,
        .(n_reps_val),
        files = .dep_files,
        base_seed = .(base_seed_val)
      )
      for (.rd in .rep_dirs) {
        ## Skip reps where LANDIS already completed (idempotency).  Check
        ## Landis-log.txt (written by every run mode) for the terminal message
        ## rather than docker_resources.log (written even on failure).
        .landis_log <- file.path(.rd, "Landis-log.txt")
        .already_done <- file.exists(.landis_log) &&
          any(grepl("Model run is complete", readLines(.landis_log, warn = FALSE)))
        if (!.already_done) {
          landisutils::landis_run_docker(
            scenario_dir = .rd,
            scenario_file = .(scenario_file),
            image = .(image),
            console = .(console)
          )
        }
      }
      .manifest <- file.path(.sd, "output_manifest.txt")
      .manifest_entries <- if (file.exists(.manifest)) readLines(.manifest) else character(0)
      unlist(lapply(.rep_dirs, function(.rd) {
        c(
          list.files(file.path(.rd, "log"), full.names = TRUE),
          list.files(file.path(.rd, .(output_dirs_val)), full.names = TRUE, recursive = TRUE),
          file.path(.rd, .manifest_entries)
        )
      }))
    })
  } else {
    bquote({
      .deps <- .(deps_expr)
      .sd <- as.character(.(scenario_dir_expr))
      .dep_files <- Filter(is.character, .deps) |> unlist()
      .dep_files <- .dep_files |> fs::path_abs() |> unique()
      .dep_files <- .dep_files[fs::file_exists(.dep_files)]
      .sd_real <- paste0(fs::path_real(.sd), "/")
      .dep_files <- c(
        .dep_files[startsWith(.dep_files, .sd_real)],
        .dep_files[!startsWith(.dep_files, .sd_real)]
      )
      .dep_files <- .dep_files[!duplicated(basename(.dep_files))]
      .rep_dirs <- landisutils::landis_replicate(
        .sd,
        .(n_reps_val),
        files = .dep_files,
        base_seed = .(base_seed_val)
      )
      for (.rd in .rep_dirs) {
        .landis_log <- file.path(.rd, "Landis-log.txt")
        .already_done <- file.exists(.landis_log) &&
          any(grepl("Model run is complete", readLines(.landis_log, warn = FALSE)))
        if (!.already_done) {
          landisutils::landis_run_local(
            scenario_dir = .rd,
            scenario_file = .(scenario_file),
            console = .(console)
          )
        }
      }
      .manifest <- file.path(.sd, "output_manifest.txt")
      .manifest_entries <- if (file.exists(.manifest)) readLines(.manifest) else character(0)
      unlist(lapply(.rep_dirs, function(.rd) {
        c(
          list.files(file.path(.rd, "log"), full.names = TRUE),
          list.files(file.path(.rd, .(output_dirs_val)), full.names = TRUE, recursive = TRUE),
          file.path(.rd, .manifest_entries)
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
