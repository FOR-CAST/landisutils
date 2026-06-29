## LANDIS-II execution helpers (local and Docker) --------------------------------------------------

#' Host CPU and RAM identification (cross-platform)
#'
#' Identifies the CPU model, logical core count, and total RAM of the machine
#' running the current R process. Each field falls back to `NA` if it can't be
#' determined.
#'
#' Implementation by platform:
#'
#' * **Linux**: reads `/proc/cpuinfo` (`model name`) and `/proc/meminfo`
#'   (`MemTotal`).
#' * **macOS** (`Darwin`): `sysctl -n machdep.cpu.brand_string` for the CPU
#'   model, `sysctl -n hw.memsize` for RAM.
#' * **Windows**: `PROCESSOR_IDENTIFIER` environment variable for the CPU
#'   model, `wmic ComputerSystem get TotalPhysicalMemory` for RAM.
#' * Logical core count uses [parallel::detectCores()] on every platform.
#'
#' Used by [landis_run_docker()] and [landis_run_local()] to append host
#' context to each rep's resource log so downstream provenance tooling can
#' identify what host produced any given replicate's outputs.
#'
#' @returns A list with elements `model` (character), `n_logical` (integer),
#'   and `ram_bytes` (numeric).
#'
#' @family LANDIS-II execution helpers
#' @export
host_cpu_info <- function() {
  os <- Sys.info()[["sysname"]]
  ## Logical cores: parallel::detectCores() is cross-platform (base R).
  n_logical <- tryCatch(as.integer(parallel::detectCores(logical = TRUE)), error = function(e) {
    NA_integer_
  })
  model <- NA_character_
  ram_bytes <- NA_real_

  if (identical(os, "Linux")) {
    if (file.exists("/proc/cpuinfo")) {
      cpu_lines <- readLines("/proc/cpuinfo", warn = FALSE)
      ml <- grep("^model name\\s*:", cpu_lines, value = TRUE)[1L]
      if (!is.na(ml)) {
        model <- trimws(sub("^model name\\s*:\\s*", "", ml))
      }
    }
    if (file.exists("/proc/meminfo")) {
      mem_lines <- readLines("/proc/meminfo", warn = FALSE)
      mt <- grep("^MemTotal:", mem_lines, value = TRUE)[1L]
      if (!is.na(mt)) {
        ## /proc/meminfo reports kB but the unit is actually KiB (Linux convention).
        ram_bytes <- as.numeric(sub(".*?([0-9]+).*", "\\1", mt)) * 1024
      }
    }
  } else if (identical(os, "Darwin")) {
    model <- tryCatch(
      trimws(system2("sysctl", c("-n", "machdep.cpu.brand_string"), stdout = TRUE, stderr = FALSE)[
        1L
      ]),
      error = function(e) NA_character_
    )
    ram_bytes <- tryCatch(
      as.numeric(system2("sysctl", c("-n", "hw.memsize"), stdout = TRUE, stderr = FALSE)[1L]),
      error = function(e) NA_real_
    )
  } else if (identical(os, "Windows")) {
    ## PROCESSOR_IDENTIFIER is always set on Windows; e.g. "AMD64 Family 23 Model 113 Stepping 0, AuthenticAMD".
    ## It's not a friendly brand name but is reliable. WMIC's "cpu get name"
    ## is the friendly form but `wmic` is deprecated/removed on Windows 11/Server 2025.
    pi <- Sys.getenv("PROCESSOR_IDENTIFIER", unset = "")
    if (nzchar(pi)) {
      model <- pi
    }
    ram_bytes <- tryCatch(
      {
        out <- system2(
          "wmic",
          c("ComputerSystem", "get", "TotalPhysicalMemory", "/value"),
          stdout = TRUE,
          stderr = FALSE
        )
        m <- regmatches(out, regexpr("[0-9]+", out, perl = TRUE))
        if (length(m)) as.numeric(m[1L]) else NA_real_
      },
      error = function(e) NA_real_
    )
  }

  list(model = model, n_logical = n_logical, ram_bytes = ram_bytes)
}

#' Read per-rep resource logs written by `landis_run_docker()` / `landis_run_local()`
#'
#' Discovers all `docker_resources.log` and `local_resources.log` files under
#' `run_dir` (typically a scenario directory containing per-rep subdirectories)
#' and returns one row per rep with every key-value pair the run helpers wrote.
#' Always includes `elapsed_sec` and `peak_mem_bytes`; rows from runs produced
#' by landisutils >= 0.0.22 additionally include `host_cpu_model`,
#' `host_cpu_cores`, and `host_ram_bytes`. Older logs return `NA` for missing
#' fields.
#'
#' Used by per-scenario report templates to surface run-time / memory / host
#' statistics in build-provenance appendices.
#'
#' @param run_dir Character. Scenario directory to search (recursively) for
#'   resource logs. Returns an empty data.frame when `run_dir` does not exist
#'   or contains no logs.
#'
#' @returns A `data.frame` with columns `replicate, source` (one of `"docker"`
#'   or `"local"`), and one numeric or character column per recorded key.
#'
#' @family LANDIS-II execution helpers
#' @export
read_landis_resource_logs <- function(run_dir) {
  empty <- data.frame(
    replicate = character(0),
    source = character(0),
    elapsed_sec = numeric(0),
    peak_mem_bytes = numeric(0),
    host_cpu_model = character(0),
    host_cpu_cores = integer(0),
    host_ram_bytes = numeric(0)
  )
  if (!dir.exists(run_dir)) {
    return(empty)
  }
  logs <- list.files(
    run_dir,
    pattern = "^(docker|local)_resources\\.log$",
    recursive = TRUE,
    full.names = TRUE
  )
  if (length(logs) == 0L) {
    return(empty)
  }
  parse_one <- function(path) {
    lines <- readLines(path, warn = FALSE)
    ## Numeric "key: value" lines.
    num_m <- regmatches(lines, regexec("^([A-Za-z_]+):\\s*([0-9.eE+-]+)\\s*$", lines))
    num_kv <- Filter(function(m) length(m) == 3L, num_m)
    num_vals <- as.numeric(vapply(num_kv, `[`, character(1L), 3L))
    names(num_vals) <- vapply(num_kv, `[`, character(1L), 2L)
    ## Free-form "key: rest of line" entries (e.g. host_cpu_model).
    str_m <- regmatches(lines, regexec("^([A-Za-z_]+):\\s*(.+?)\\s*$", lines))
    str_kv <- Filter(function(m) length(m) == 3L, str_m)
    str_vals <- vapply(str_kv, `[`, character(1L), 3L)
    names(str_vals) <- vapply(str_kv, `[`, character(1L), 2L)
    ## Numeric wins when a key parses both ways (e.g. host_cpu_cores).
    out <- as.list(str_vals)
    out[names(num_vals)] <- as.list(num_vals)
    out$replicate <- basename(dirname(dirname(path)))
    out$source <- sub("_resources\\.log$", "", basename(path))
    out
  }
  rows <- lapply(logs, parse_one)
  ## Union of keys across rows -> one column per key.
  all_keys <- unique(unlist(lapply(rows, names)))
  fill <- function(row) {
    miss <- setdiff(all_keys, names(row))
    row[miss] <- NA
    row[all_keys]
  }
  do.call(rbind, lapply(rows, function(r) data.frame(fill(r), stringsAsFactors = FALSE)))
}

## Parse a memory-limit input (numeric bytes, NULL/Inf for "no limit", or
## a string like "8g" / "512m" / "1024k") into either a numeric byte count
## (positive) or Inf. Returns Inf when the input means "no limit".
.parse_mem_limit <- function(x) {
  if (is.null(x) || (is.numeric(x) && length(x) == 1L && (is.infinite(x) || is.na(x)))) {
    return(Inf)
  }
  if (is.numeric(x)) {
    return(as.numeric(x))
  }
  ## String form: number + optional suffix (b/k/m/g/t, case-insensitive).
  s <- trimws(tolower(as.character(x)))
  m <- regmatches(s, regexec("^([0-9.]+)\\s*([kmgt]?)b?$", s))[[1L]]
  if (length(m) != 3L) {
    stop(
      sprintf("Could not parse mem_limit '%s' (expected e.g. '8g', '512m', or numeric bytes).", x),
      call. = FALSE
    )
  }
  val <- as.numeric(m[2L])
  suffix <- m[3L]
  mult <- if (!nzchar(suffix)) {
    1
  } else {
    ## switch() can't use "" as a label (zero-length variable name) so we
    ## special-case the suffix-less form above.
    switch(suffix, k = 1024, m = 1024^2, g = 1024^3, t = 1024^4)
  }
  val * mult
}

## Resolve memory cap for a LANDIS run.
## - `mem_limit` is the user-supplied baseline (any form accepted by .parse_mem_limit).
## - If a prior resource log for this rep records peak_mem_bytes, scale by
##   `mem_margin` and raise the cap to that value (so we never kill a run
##   that previously fit). If NO prior log exists, drop the cap to Inf so
##   the first run can discover its own requirements.
## Returns Inf (no limit) or a numeric byte count.
.resolve_mem_limit <- function(rep_dir, mem_limit, mem_margin) {
  baseline <- .parse_mem_limit(mem_limit)
  log_dir <- file.path(rep_dir, "log")
  prior_logs <- if (dir.exists(log_dir)) {
    list.files(log_dir, pattern = "^(docker|local)_resources\\.log$", full.names = TRUE)
  } else {
    character(0)
  }
  if (length(prior_logs) == 0L) {
    ## No history: relax the constraint so the first run can discover its peak.
    return(Inf)
  }
  prior_peak <- tryCatch(
    {
      ## Pick the most recently modified log if multiple exist.
      lines <- readLines(
        prior_logs[order(file.info(prior_logs)$mtime, decreasing = TRUE)[1L]],
        warn = FALSE
      )
      m <- regmatches(lines, regexec("^peak_mem_bytes:\\s*([0-9.eE+-]+)\\s*$", lines))
      m <- Filter(function(x) length(x) == 2L, m)
      if (length(m) == 0L) NA_real_ else as.numeric(m[[1L]][2L])
    },
    error = function(e) NA_real_
  )
  if (is.na(prior_peak) || prior_peak <= 0) {
    return(baseline)
  }
  ## Honour the user baseline, but raise it if the prior peak (with margin)
  ## would exceed it -- never set the cap below what we've seen the rep need.
  max(baseline, prior_peak * mem_margin)
}

## Resolve the startup-jitter upper bound (seconds) for staggering container
## launches. `startup_jitter` NULL falls back to the LANDIS_STARTUP_JITTER env
## var so it can be set once in a project .Rprofile that crew workers inherit;
## an unset/blank/non-numeric/negative value (or NA) means 0 (no stagger).
## Returns a single non-negative numeric.
.resolve_startup_jitter <- function(startup_jitter = NULL) {
  if (is.null(startup_jitter)) {
    startup_jitter <- suppressWarnings(as.numeric(Sys.getenv("LANDIS_STARTUP_JITTER", "0")))
  } else {
    startup_jitter <- suppressWarnings(as.numeric(startup_jitter)[1L])
  }
  if (length(startup_jitter) != 1L || is.na(startup_jitter) || startup_jitter < 0) {
    return(0)
  }
  startup_jitter
}

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

## Post-completion watchdog decision (pure; see landis_run_docker()).
## Returns TRUE when the grace period after the "Model run is complete." marker
## has elapsed and the container should be SIGTERMed. A non-finite `timeout`
## (e.g. Inf) disables the watchdog, so this always returns FALSE. Safe against
## an NA `seen_at` (the marker not yet seen) via the `completion_seen` guard.
.watchdog_should_stop <- function(completion_seen, seen_at, now, timeout) {
  isTRUE(completion_seen) && is.finite(timeout) && !is.na(seen_at) && (now - seen_at) > timeout
}

## Is a container with exactly this name currently running? The name filter is
## anchored (^name$) so we only ever match this replicate's container, never a
## sibling rep whose name shares a prefix. Returns FALSE on any docker error
## (treated as "no longer running").
.docker_container_running <- function(name) {
  out <- tryCatch(
    system2(
      "docker",
      c("ps", "-q", "--filter", sprintf("name=^%s$", name)),
      stdout = TRUE,
      stderr = FALSE
    ),
    error = function(e) character(0)
  )
  length(out) > 0L && any(nzchar(trimws(out)))
}

## Has this replicate's LANDIS-II run completed? Mirrors tar_landis()'s skip
## check: Landis-log.txt exists and records the completion marker.
.landis_run_complete <- function(scenario_dir) {
  log <- fs::path(scenario_dir, "Landis-log.txt")
  fs::file_exists(log) && any(grepl("Model run is complete", readLines(log, warn = FALSE)))
}

## Adopt an in-progress container started by another worker (or orphaned by a
## worker that was declared crashed): wait for it to exit, applying the same
## post-completion watchdog as landis_run_docker() so that an orphan whose
## owning worker is no longer polling cannot hang at 100% CPU in the .NET
## shutdown path forever. `stdout_log` is the shared per-rep docker_stdout.log
## that the owning run is writing to.
.adopt_and_wait <- function(name, stdout_log, post_completion_timeout_sec) {
  completion_marker <- "Model run is complete."
  completion_seen <- FALSE
  completion_seen_at <- NA_real_
  stopped_for_timeout <- FALSE
  while (.docker_container_running(name)) {
    if (!completion_seen && fs::file_exists(stdout_log)) {
      tail_lines <- tryCatch(
        utils::tail(readLines(stdout_log, warn = FALSE), n = 50L),
        error = function(e) character(0)
      )
      if (any(grepl(completion_marker, tail_lines, fixed = TRUE))) {
        completion_seen <- TRUE
        completion_seen_at <- proc.time()[["elapsed"]]
      }
    }
    if (
      !stopped_for_timeout &&
        .watchdog_should_stop(
          completion_seen,
          completion_seen_at,
          proc.time()[["elapsed"]],
          post_completion_timeout_sec
        )
    ) {
      stopped_for_timeout <- TRUE
      tryCatch(
        system2("docker", c("stop", "-t", "30", name), stdout = FALSE, stderr = FALSE),
        error = function(e) NULL
      )
    }
    Sys.sleep(2)
  }
  invisible(stopped_for_timeout)
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

  ## Serialize replicate runs: a duplicate dispatch (e.g. {targets} re-running a
  ## branch after a false-positive worker crash) must never start a second
  ## dotnet against this directory concurrently and O_TRUNC its half-written
  ## outputs. An exclusive advisory lock blocks the duplicate until the first
  ## run frees it. Unlike a sentinel file, the lock is released automatically if
  ## the holder dies, so a crashed worker cannot deadlock the replicate.
  run_lock <- filelock::lock(fs::path(log_dir, "run.lock"))
  on.exit(if (!is.null(run_lock)) filelock::unlock(run_lock), add = TRUE)

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

  host <- host_cpu_info()
  writeLines(
    c(
      sprintf("elapsed_sec: %.1f", elapsed_sec),
      sprintf("peak_mem_bytes: %.0f", peak_mem_bytes),
      sprintf("host_cpu_model: %s", host$model %||% "NA"),
      sprintf("host_cpu_cores: %s", host$n_logical %||% "NA"),
      sprintf(
        "host_ram_bytes: %s",
        if (is.na(host$ram_bytes)) "NA" else sprintf("%.0f", host$ram_bytes)
      )
    ),
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
#' The image's immutable `sha256` digest is captured into
#' `<scenario_dir>/log/docker_image.log` so downstream provenance tools can
#' pin runs to a specific image regardless of subsequent tag movement.
#'
#' **Single container per replicate.** The container name is derived
#' deterministically from the (real) scenario directory, so docker's own name
#' uniqueness acts as a cross-worker mutex: if the same replicate is dispatched
#' to two workers at once (for example when `targets` re-runs a branch after a
#' false-positive worker crash while the original container is still running),
#' the second call cannot start a parallel container and instead *adopts* the
#' running one: it waits for that container to finish (applying the same
#' post-completion watchdog) and returns its result. A duplicate dispatch thus
#' becomes a harmless wait rather than two `dotnet` processes truncating each
#' other's outputs.
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
#' @param pull Logical. When `TRUE`, run `docker pull <image>` before the
#'   simulation so the recorded digest reflects the current registry rather
#'   than a stale local copy. Defaults to `FALSE` to keep runs reproducible
#'   across iterations of an already-cached image.
#' @param cpu_limit Numeric or `NULL`. Hard CPU cap for the container
#'   (`docker run --cpus`). Default `2`: LANDIS-II compute is effectively
#'   single-threaded -- empirical measurement across 90 concurrent ForCS +
#'   Dynamic Fire + Dynamic Fuels containers shows a median of 1.00 cores
#'   used per container, p99 = 1.11 cores, max = 1.11 cores. The .NET
#'   runtime hosts 9-11 OS threads but only the simulator thread is
#'   compute-bound; the GC / threadpool helpers occasionally peek above
#'   1.0 cores in brief bursts. `2` covers that 99th-percentile burst with
#'   ~80% headroom; `1` is tight enough that the .NET GC helper would
#'   contend with the simulator thread for cycles. Pass `NULL` for no
#'   CPU limit.
#' @param mem_limit Numeric byte count, character (e.g. `"8g"`, `"512m"`),
#'   `NULL`, or `Inf`. Baseline RAM cap (`docker run --memory`). Default
#'   `"8g"`. When a prior `<rep_dir>/log/*_resources.log` exists with a
#'   recorded `peak_mem_bytes`, the cap is raised to
#'   `peak_mem_bytes * mem_margin` if that exceeds the baseline -- a rep
#'   that fit last time will never be killed by this cap on a rerun. When
#'   **no** prior log exists for the rep (first run, or rep dir freshly
#'   deleted), the cap is dropped entirely so the first run can discover
#'   what it needs.
#' @param mem_margin Numeric. Headroom factor applied to a previously
#'   observed peak when auto-raising `mem_limit`. Default `1.5`.
#' @param post_completion_timeout_sec Numeric. Grace period (seconds) after
#'   the string `"Model run is complete."` first appears in the container's
#'   stdout before the watchdog SIGTERMs the container. Some long ForCS +
#'   Dynamic Fire scenarios with many output extensions log this completion
#'   marker but then spin in the .NET runtime shutdown path indefinitely
#'   (outputs are already on disk, so the sim itself completed cleanly). On
#'   timeout the container is stopped and exit codes 137/143 are remapped to
#'   `0`. Set to `Inf` to disable the watchdog. Default `300` (5 min).
#' @param startup_jitter Numeric seconds or `NULL`. Upper bound on a random
#'   start delay applied **before** the function touches Docker, to stagger
#'   container launches and avoid a thundering-herd surge on the Docker daemon
#'   (and on the disk backing the image layers / renv library) when many
#'   replicates start at once under `crew`. Each call sleeps
#'   `runif(1, 0, startup_jitter)` seconds. The delay cannot affect simulation
#'   results (identical seed and inputs), so when run from `tar_landis()` it is
#'   deliberately **not** baked into the `{targets}` command -- changing it
#'   never invalidates completed replicates. `NULL` (the default) reads the
#'   `LANDIS_STARTUP_JITTER` environment variable (so it can be set once in a
#'   project `.Rprofile` that `crew` workers inherit); an unset/invalid value
#'   means `0` (no stagger).
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
  console = NULL,
  pull = FALSE,
  cpu_limit = 2,
  mem_limit = "8g",
  mem_margin = 1.5,
  post_completion_timeout_sec = 300,
  startup_jitter = NULL
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

  ## Stagger container startup: when many replicates launch at once under crew,
  ## the simultaneous `docker run` storm overwhelms the daemon (it stops
  ## answering `docker stats`, returning exit 1) and the simultaneous reads of
  ## the image layers / renv library hammer the backing disk -- both observed to
  ## churn crew workers at >=50-way concurrency. A small random pre-Docker delay
  ## spreads the herd. It cannot change results (same seed and inputs), so it is
  ## read from an env var at run time rather than baked into the {targets}
  ## command (changing it never invalidates a completed rep). Default 0 (off).
  startup_jitter <- .resolve_startup_jitter(startup_jitter)
  if (startup_jitter > 0) {
    jitter_sec <- stats::runif(1L, 0, startup_jitter)
    message(glue::glue(
      "  staggering start by {format(round(jitter_sec, 1L), nsmall = 1L)}s ",
      "(jitter <= {startup_jitter}s) to ease the Docker/disk startup surge"
    ))
    Sys.sleep(jitter_sec)
  }

  ## Optionally pull the image so the captured digest reflects the current
  ## registry rather than a possibly-stale local copy.
  if (isTRUE(pull)) {
    message(glue::glue("  pulling:       {image}"))
    pull_rc <- system2("docker", c("pull", image), stdout = FALSE, stderr = FALSE)
    if (pull_rc != 0L) {
      warning(
        sprintf(
          "`docker pull %s` failed (exit code %d); continuing with local image.",
          image,
          pull_rc
        ),
        call. = FALSE
      )
    }
  }

  ## Capture the immutable image digest into a sidecar. Image tags are mutable;
  ## the digest is the canonical identifier of the bytes that ran. RepoDigests
  ## carries `<repo>@sha256:<64hex>` for any image with a known registry origin;
  ## locally-built images fall back to the content-addressable Id (`sha256:...`).
  digest_line <- tryCatch(
    {
      rd <- system2(
        "docker",
        c(
          "image",
          "inspect",
          image,
          "--format",
          "{{if .RepoDigests}}{{index .RepoDigests 0}}{{else}}{{.Id}}{{end}}"
        ),
        stdout = TRUE,
        stderr = FALSE
      )
      if (length(rd) && nzchar(trimws(rd[1L]))) trimws(rd[1L]) else NA_character_
    },
    error = function(e) NA_character_,
    warning = function(w) NA_character_
  )
  if (is.na(digest_line)) {
    digest_line <- sprintf("# %s (digest unavailable: not present in local image cache?)", image)
  }
  writeLines(digest_line, fs::path(log_dir, "docker_image.log"))

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

  ## Container name is derived deterministically from the (already real-path'd)
  ## scenario directory, so every worker handed this same replicate maps to the
  ## SAME name. Docker enforces name uniqueness, which makes the name the
  ## cross-worker mutex: a duplicate dispatch cannot start a second container
  ## against this rep dir and O_TRUNC its outputs. Different reps hash to
  ## different names and still run fully in parallel. A duplicate instead adopts
  ## the already-running container (see the run loop below). docker stats and the
  ## watchdog identify this rep's container by the same name.
  container_name <- paste0(
    "landis-run-",
    substr(digest::digest(as.character(scenario_dir)), 1L, 16L)
  )

  ## Resolve resource caps (auto-raises memory based on prior peak; drops
  ## memory cap entirely on first runs so they can discover what they need).
  effective_mem_bytes <- .resolve_mem_limit(scenario_dir, mem_limit, mem_margin)
  cpu_args <- if (is.null(cpu_limit) || is.infinite(cpu_limit)) {
    character(0)
  } else {
    c("--cpus", as.character(cpu_limit))
  }
  mem_args <- if (is.infinite(effective_mem_bytes)) {
    character(0)
  } else {
    c("--memory", sprintf("%.0fb", effective_mem_bytes))
  }

  message(glue::glue("Starting LANDIS-II Docker run ({Sys.time()})"))
  message(glue::glue("  scenario_dir:  {scenario_dir}"))
  message(glue::glue("  scenario_file: {scenario_file}"))
  message(glue::glue("  image:         {image}"))
  message(glue::glue(
    "  resources:     cpus={if (length(cpu_args)) cpu_args[2] else 'unlimited'}, ",
    "memory={if (length(mem_args)) .fmt_bytes(effective_mem_bytes) else 'unlimited (no prior log)'}"
  ))

  docker_args <- c(
    "run",
    "--rm",
    "--name",
    container_name,
    "--entrypoint",
    "dotnet",
    user_args,
    cpu_args,
    mem_args,
    "-v",
    paste0(scenario_dir, ":/sim"),
    "-w",
    "/sim",
    image,
    console,
    scenario_file
  )

  ## Run docker as a direct child process so we can poll docker stats from the
  ## main thread. We use `processx::process` (which exec()s the docker CLI)
  ## rather than `callr::r_bg` (which forks a *full R session*). A nested R
  ## session launched from inside a `{crew}`/`{mirai}` worker can be orphaned or
  ## crash the worker when crew recycles/duplicates it ("could not start R");
  ## the docker CLI child is the same lightweight kind of process as the
  ## `docker stats` calls below and avoids that interaction entirely. `docker
  ## run` stays in the foreground (no `-d`), so the CLI's exit status is the
  ## container's exit status and `--rm` still auto-removes the container.
  t_start <- proc.time()
  stdout_log <- fs::path(log_dir, "docker_stdout.log")
  stderr_log <- fs::path(log_dir, "docker_stderr.log")

  ## Run loop with cross-worker mutual exclusion (see container_name above).
  ##
  ## Normal (owner) path: start the container, poll it, break with its exit code.
  ##
  ## Contended path: `docker run --name` fails with a name conflict because
  ## another worker -- or a container orphaned by a worker that was declared
  ## crashed -- already owns this rep. We then *adopt* that run: wait for it to
  ## finish (with the post-completion watchdog so an orphan whose owning worker
  ## is no longer polling cannot hang forever), and once it is gone accept its
  ## result if the rep completed, otherwise clear any leftover container and
  ## retry as the owner. `adopt_max` bounds pathological ping-pong.
  completion_marker <- "Model run is complete."
  rc <- NA_integer_
  peak_mem_bytes <- 0
  adopted <- FALSE
  adopt_attempts <- 0L
  adopt_max <- 10L
  repeat {
    peak_mem_bytes <- 0
    docker_proc <- processx::process$new(
      command = "docker",
      args = docker_args,
      stdout = stdout_log,
      stderr = stderr_log,
      cleanup = TRUE
    )

    ## Poll docker stats every 2 s, tracking peak memory across samples.
    ##
    ## Post-completion watchdog: some LANDIS-II runs (particularly long ForCS +
    ## Dynamic Fire scenarios with many output extensions) print
    ## "Model run is complete." but the dotnet process then hangs spinning at
    ## 100% CPU in the .NET runtime shutdown phase. Outputs are already on disk
    ## at that point, so we watch the stdout log for the completion marker and
    ## SIGTERM the container `post_completion_timeout_sec` later if the process
    ## hasn't exited on its own. The post-completion stop is treated as success
    ## (the sim itself finished cleanly).
    completion_seen <- FALSE
    completion_seen_at <- NA_real_
    stopped_for_timeout <- FALSE
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
      if (!completion_seen && fs::file_exists(stdout_log)) {
        ## Cheap tail-grep: read the file as raw lines and look for the marker.
        ## `readLines()` with `warn = FALSE` to suppress the no-newline-at-eof
        ## complaint that LANDIS-II's stdout occasionally triggers.
        tail_lines <- tryCatch(
          utils::tail(readLines(stdout_log, warn = FALSE), n = 50L),
          error = function(e) character(0)
        )
        if (any(grepl(completion_marker, tail_lines, fixed = TRUE))) {
          completion_seen <- TRUE
          completion_seen_at <- proc.time()[["elapsed"]]
          message(glue::glue(
            "  [{Sys.time()}] '{completion_marker}' detected; ",
            "post-completion watchdog armed ({post_completion_timeout_sec} s grace period)."
          ))
        }
      }
      if (
        !stopped_for_timeout &&
          .watchdog_should_stop(
            completion_seen,
            completion_seen_at,
            proc.time()[["elapsed"]],
            post_completion_timeout_sec
          )
      ) {
        message(glue::glue(
          "  [{Sys.time()}] post-completion timeout exceeded; sending docker stop to {container_name}."
        ))
        stopped_for_timeout <- TRUE
        tryCatch(
          system2("docker", c("stop", "-t", "30", container_name), stdout = FALSE, stderr = FALSE),
          error = function(e) NULL
        )
      }
      Sys.sleep(2)
    }

    docker_proc$wait()
    rc <- docker_proc$get_exit_status()

    ## A name conflict means another worker, or a crash-orphaned container,
    ## already owns this rep. Docker (and podman) exit 125 and print
    ## "is already in use" to stderr. Match on that string rather than the bare
    ## exit code, so a genuine 125 from another cause (image not found, bad flag)
    ## still surfaces as a real failure instead of looping to the adopt cap.
    stderr_txt <- tryCatch(
      paste(readLines(stderr_log, warn = FALSE), collapse = "\n"),
      error = function(e) ""
    )
    name_conflict <- grepl("is already in use", stderr_txt, fixed = TRUE)

    if (!name_conflict) {
      ## We owned this run. If we forced a stop after completion, treat as
      ## success (the sim itself finished cleanly; exit 143 from SIGTERM or 137
      ## from SIGKILL is the watchdog firing, not a real failure).
      if (stopped_for_timeout && rc %in% c(137L, 143L)) {
        message(glue::glue(
          "  [{Sys.time()}] docker exit {rc} treated as success ",
          "(post-completion watchdog; '{completion_marker}' was logged)."
        ))
        rc <- 0L
      }
      break
    }

    adopt_attempts <- adopt_attempts + 1L
    if (adopt_attempts > adopt_max) {
      stop(
        sprintf(
          paste0(
            "LANDIS-II Docker run for '%s' could not acquire its replicate after %d attempts ",
            "(container name in use but the run never completes). Check logs:\n  %s"
          ),
          container_name,
          adopt_max,
          log_dir
        ),
        call. = FALSE
      )
    }
    message(glue::glue(
      "  [{Sys.time()}] container '{container_name}' already running for this replicate; ",
      "adopting the in-progress run (no parallel container started)."
    ))
    if (.docker_container_running(container_name)) {
      .adopt_and_wait(container_name, stdout_log, post_completion_timeout_sec)
    }
    if (.landis_run_complete(scenario_dir)) {
      adopted <- TRUE
      rc <- 0L
      break
    }
    ## The adopted run vanished without completing (its owner died mid-run), or a
    ## stopped container is squatting on the name. Clear it and retry as owner.
    tryCatch(
      system2("docker", c("rm", "-f", container_name), stdout = FALSE, stderr = FALSE),
      error = function(e) NULL
    )
    message(glue::glue(
      "  [{Sys.time()}] adopted run for '{container_name}' did not complete; retrying as owner."
    ))
  }

  elapsed_sec <- (proc.time() - t_start)[["elapsed"]]

  ## Only the worker that actually ran the container writes the resource log; an
  ## adopting worker must not clobber the owner's measurements with its own
  ## (zero) peak memory and wait-only elapsed time.
  if (!adopted) {
    host <- host_cpu_info()
    writeLines(
      c(
        sprintf("elapsed_sec: %.1f", elapsed_sec),
        sprintf("peak_mem_bytes: %.0f", peak_mem_bytes),
        sprintf("host_cpu_model: %s", host$model %||% "NA"),
        sprintf("host_cpu_cores: %s", host$n_logical %||% "NA"),
        sprintf(
          "host_ram_bytes: %s",
          if (is.na(host$ram_bytes)) "NA" else sprintf("%.0f", host$ram_bytes)
        )
      ),
      fs::path(log_dir, "docker_resources.log")
    )
  }

  if (rc != 0L) {
    stop(
      sprintf("LANDIS-II Docker run failed (exit code %d). Check logs:\n  %s", rc, log_dir),
      call. = FALSE
    )
  }

  if (adopted) {
    message(glue::glue("LANDIS-II Docker run adopted from a concurrent worker ({Sys.time()})"))
    message(glue::glue(
      "  Waited:      {.fmt_duration(elapsed_sec)} for the in-progress run to finish"
    ))
  } else {
    mem_str <- if (peak_mem_bytes > 0) {
      .fmt_bytes(peak_mem_bytes)
    } else {
      "(not sampled -- run too short)"
    }
    message(glue::glue("LANDIS-II Docker run completed ({Sys.time()})"))
    message(glue::glue("  Elapsed:     {.fmt_duration(elapsed_sec)}"))
    message(glue::glue("  Peak memory: {mem_str}"))
  }

  invisible(list(exit_code = rc, elapsed_sec = elapsed_sec, peak_mem_bytes = peak_mem_bytes))
}

#' Move a completed LANDIS-II replicate from scratch to its final location
#'
#' Moves a finished replicate directory `run_dir` (typically on fast, local,
#' Docker-bind-mountable scratch) to `final_dir` (typically a slower, networked
#' final/archive location such as an NFS project share) in a way that is
#' fault-tolerant *and* all-or-nothing at the destination:
#'
#' 1. `rsync -a --partial` copies `run_dir` into a sibling staging directory
#'    `paste0(final_dir, ".partial")` on the *destination* filesystem, with
#'    retry + linear backoff so a transient network blip does not abort the run
#'    (`--partial` keeps partially transferred files so a retry resumes rather
#'    than restarts). Crucially this is a COPY -- the scratch source stays the
#'    complete, authoritative replicate until the destination is verified, so a
#'    total transfer failure loses nothing.
#' 2. After a verified `rsync` exit status of `0`, the staging directory is
#'    published with an atomic `rename` into `final_dir`. Because the rename is
#'    atomic, `final_dir` only ever appears *complete* -- a partial transfer is
#'    never visible to a downstream skip-check that reads `final_dir`. (This is
#'    why `--remove-source-files` is deliberately NOT used: it would delete
#'    scratch incrementally and could leave a half-emptied source on total
#'    failure, and a partial destination could be mistaken for a complete run.)
#' 3. Only once `final_dir` is in place is the scratch source deleted.
#'
#' If `run_dir` and `final_dir` already resolve to the same path (no scratch in
#' use), this is a no-op that returns `final_dir` without copying or deleting.
#'
#' Requires the `rsync` executable on `PATH` (standard on Linux/macOS). A
#' missing `rsync`, or repeated failures, raises an error after `max_tries`
#' attempts so the run target fails loudly and the scratch copy is retained for
#' inspection rather than silently lost.
#'
#' @param run_dir Character. The completed replicate directory to move (source).
#' @param final_dir Character. The destination directory (created if needed).
#' @param max_tries Integer. Maximum `rsync` attempts before giving up
#'   (default `5`).
#' @param backoff_sec Numeric. Base seconds for linear backoff between attempts
#'   (attempt `i` waits `backoff_sec * i`; default `5`).
#'
#' @returns Character. `final_dir`, on success.
#'
#' @family LANDIS-II execution helpers
#' @seealso [tar_landis()], [landis_run_docker()], [landis_replicate()]
#' @export
landis_archive_rep <- function(run_dir, final_dir, max_tries = 5L, backoff_sec = 5) {
  run_real <- fs::path_real(run_dir)
  final_real <- tryCatch(fs::path_real(final_dir), error = function(e) NA_character_)
  ## same location (no scratch in use): nothing to move, never delete
  if (!is.na(final_real) && identical(as.character(run_real), as.character(final_real))) {
    return(final_dir)
  }
  fs::dir_create(fs::path_dir(final_dir))
  ## Stage into a sibling ".partial" directory on the SAME (destination)
  ## filesystem so the publish step below is an atomic rename. Trailing "/" on
  ## the source copies its CONTENTS into the staging dir.
  staging <- paste0(final_dir, ".partial")
  fs::dir_create(staging)
  args <- c("-a", "--partial", paste0(run_real, "/"), paste0(staging, "/"))
  status <- NA_integer_
  last_err <- ""
  for (i in seq_len(max_tries)) {
    res <- tryCatch(
      processx::run("rsync", args, error_on_status = FALSE, echo = FALSE),
      error = function(e) list(status = 127L, stderr = conditionMessage(e))
    )
    status <- as.integer(res$status)
    last_err <- res$stderr %||% ""
    if (identical(status, 0L)) {
      break
    }
    if (i < max_tries) {
      Sys.sleep(backoff_sec * i)
    }
  }
  if (!identical(status, 0L)) {
    stop(
      sprintf(
        "rsync of completed replicate failed after %d attempt(s) (exit %s): %s -> %s\n%s",
        max_tries,
        status,
        run_real,
        staging,
        last_err
      ),
      call. = FALSE
    )
  }
  ## atomic publish: a verified, complete copy replaces any existing final_dir
  if (fs::dir_exists(final_dir)) {
    fs::dir_delete(final_dir)
  }
  fs::file_move(staging, final_dir)
  ## move semantics: drop the scratch copy only after final_dir is in place
  fs::dir_delete(run_real)
  final_dir
}

#' Create a `targets` target that runs one LANDIS-II replicate
#'
#' A `{targets}` factory that creates **one** `format = "file"` target.
#' Each branch runs exactly one LANDIS-II simulation and returns only that
#' replicate's output files.
#'
#' **Per-replicate parallel branching:** the caller creates a rep-index target
#' with `iteration = "vector"` and combines it with the scenario dimension via
#' `cross()`. Keeping the rep-index target explicit in the project's `list()`
#' makes it visible to static analysis tools (tarborist) and makes the
#' `iteration = "vector"` annotation clear in the project code:
#'
#' ```r
#' list(
#'   ## iteration = "vector" is critical: cross() iterates over each ELEMENT,
#'   ## giving n_scenarios x n_reps independent branches dispatched in parallel.
#'   tar_target(
#'     name      = landis_run_output_rep_index,
#'     command   = seq_len(5L),
#'     iteration = "vector"
#'   ),
#'   landisutils::tar_landis(
#'     name      = landis_run_output,
#'     rep_index = landis_run_output_rep_index,
#'     ...
#'     pattern   = cross(landis_run_name, landis_run_output_rep_index)
#'   )
#' )
#' ```
#'
#' **Caching:** each (scenario, replicate) branch is cached independently.
#' Adding replicates (increasing the `seq_len()` value) only computes new
#' branches; existing ones remain untouched.
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
#' @param rep_index Symbol (unquoted). Upstream target that provides the
#'   1-based integer index for the current branch. Must be defined with
#'   `iteration = "vector"` so that `cross()` iterates over individual
#'   elements. Typically created as
#'   `tar_target(name = ..._rep_index, command = seq_len(n_reps), iteration = "vector")`.
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
#' @param base_seed Integer or `NULL`. Passed to [landis_replicate()]: when
#'   non-`NULL`, the `RandomNumberSeed` in each replicate's `scenario.txt` is
#'   set to `base_seed + (rep_index - 1)`, giving each run a distinct but
#'   deterministic seed. Adding more replicates later never changes existing
#'   seeds because seeds are derived from the rep index, not the order of
#'   creation.
#' @param pull Logical. Passed to [landis_run_docker()] when
#'   `method = "docker"`: when `TRUE`, the image is `docker pull`ed before
#'   running so the digest captured in `log/docker_image.log` reflects the
#'   current registry. Defaults to `FALSE`. No effect for `method = "local"`.
#' @param pattern Expression (unquoted). Dynamic-branching pattern covering
#'   both the scenario and replicate dimensions, e.g.
#'   `cross(landis_run_name, landis_run_output_rep_index)`.
#'   Passed directly to [targets::tar_target_raw()].
#' @param force Logical (default `FALSE`). When `FALSE`, `tar_landis()` skips
#'   the actual `landis_run_*()` call if the rep dir already contains a
#'   completed `Landis-log.txt` *and* a `log/input_hash.json` sidecar whose
#'   recorded hash matches the current inputs (per-input-file MD5 + `base_seed`
#'   + `rep_index` + `scenario_file`). When `TRUE`, the skip check is bypassed
#'   and LANDIS-II is invoked unconditionally.
#' @param cpu_limit,mem_limit,mem_margin Passed to [landis_run_docker()] when
#'   `method = "docker"`. See that function's documentation for semantics;
#'   defaults are `2`, `"8g"`, and `1.5` respectively. No effect for
#'   `method = "local"`.
#' @param post_completion_timeout_sec Numeric. Passed to [landis_run_docker()]
#'   when `method = "docker"`: grace period (seconds) after the LANDIS-II
#'   console logs `"Model run is complete."` before the container is SIGTERMed
#'   if `dotnet` has not exited on its own. Guards against the known v8
#'   post-completion hang (long ForCS + Dynamic Fire sims spin at 100% CPU in
#'   the .NET shutdown path after outputs are already on disk). Default `300`
#'   (5 min); pass `Inf` to disable the watchdog. No effect for
#'   `method = "local"`.
#' @param work_root Character or `NULL`. Optional fast, local, Docker-bind-
#'   mountable scratch root used to RUN each replicate, separate from the final
#'   (tracked) `scenario_dir`. Needed when `scenario_dir` lives on storage the
#'   Docker daemon cannot bind-mount (e.g. a root-squashed NFS share): the rep
#'   is staged + run under `work_root/<scenario>/<studyArea>/repNN`, then its
#'   completed outputs are moved to `scenario_dir/repNN` via
#'   [landis_archive_rep()] (fault-tolerant `rsync` + retry) so the value the
#'   target returns -- and everything `{targets}` tracks -- is the FINAL
#'   location, with scratch holding only transient run files. The skip-check
#'   and output collection both read the final `scenario_dir/repNN`. When `NULL`
#'   (default), the per-run env var `LANDIS_SCRATCH` is consulted at run time
#'   (so `{crew}` workers can set it via `.Rprofile`); when that is also empty
#'   the rep runs in place under `scenario_dir` (original behaviour). No effect
#'   for `method = "local"`.
#' @param packages,library,error,memory,resources,storage,retrieval,cue,description
#'   Standard `{targets}` options; all default to [targets::tar_option_get()].
#'
#' @returns A single `tar_target` object (from [targets::tar_target_raw()]).
#'
#' @family LANDIS-II execution helpers
#' @seealso [landis_replicate()], [landis_run_local()], [landis_run_docker()]
#' @export
tar_landis <- function(
  name,
  scenario_dir,
  rep_index,
  deps = NULL,
  scenario_file = "scenario.txt",
  output_dir = "output",
  method = NULL,
  image = NULL,
  console = NULL,
  base_seed = NULL,
  pull = FALSE,
  force = FALSE,
  cpu_limit = 2,
  mem_limit = "8g",
  mem_margin = 1.5,
  post_completion_timeout_sec = 300,
  work_root = NULL,
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
  rep_index_expr <- substitute(rep_index)
  deps_expr <- substitute(deps)
  pattern_expr <- substitute(pattern)

  ## Resolve scalar args at factory-call time so values are baked into the
  ## command. crew workers don't inherit R session state.
  base_seed_val <- if (is.null(base_seed)) NULL else as.integer(base_seed)
  output_dirs_val <- as.character(output_dir)
  pull_val <- isTRUE(pull)
  force_val <- isTRUE(force)
  cpu_limit_val <- cpu_limit
  mem_limit_val <- mem_limit
  mem_margin_val <- as.numeric(mem_margin)
  post_completion_timeout_sec_val <- post_completion_timeout_sec
  work_root_val <- if (is.null(work_root)) NULL else as.character(work_root)

  ## Resolve method and image at factory-call time.
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

  ## ---- shared input-file prep expression -------------------------------------------------------
  ## .deps forces {targets} dependency detection: symbol references embedded in
  ## the list() are scanned at pipeline parse time; the assignment is a no-op
  ## at execution time.
  prep_expr <- bquote({
    .deps <- .(deps_expr)
    .sd <- as.character(.(scenario_dir_expr))
    .rep_idx <- as.integer(.(rep_index_expr))

    .dep_files <- Filter(is.character, .deps) |> unlist()
    .dep_files <- .dep_files |> fs::path_abs() |> unique()
    .dep_files <- .dep_files[fs::file_exists(.dep_files)]
    ## Append "/" so startsWith() matches only files *under* .sd, not files
    ## under a sibling whose name shares the same prefix
    ## (e.g. phase_2_ICH_fire/ must not match prefix phase_2_ICH/).
    .sd_real <- paste0(fs::path_real(.sd), "/")
    .dep_files <- c(
      .dep_files[startsWith(.dep_files, .sd_real)],
      .dep_files[!startsWith(.dep_files, .sd_real)]
    )
    .dep_files <- .dep_files[!duplicated(basename(.dep_files))]

    ## Final (tracked) replicate location: the value this target returns -- and
    ## everything {targets} tracks -- always points HERE, under scenario_dir.
    .final_rep_dir <- fs::path(.sd, sprintf("rep%02d", .rep_idx))
    ## Scratch run-root (transient): explicit work_root arg, else the per-run
    ## env var (so {crew} workers can set it via .Rprofile). When set, the rep
    ## is staged + run under <work_root>/<scenario>/<studyArea> and the finished
    ## rep is moved to .final_rep_dir; when empty it runs in place under .sd.
    .work_root <- if (!is.null(.(work_root_val))) {
      .(work_root_val)
    } else {
      Sys.getenv("LANDIS_SCRATCH", unset = "")
    }
    .stage_sd <- if (nzchar(.work_root)) {
      fs::path(.work_root, fs::path_file(fs::path_dir(.sd)), fs::path_file(.sd))
    } else {
      .sd
    }
  })

  ## ---- output-collection expression ------------------------------------------------------------
  ## Always collect from the FINAL (tracked) location, never scratch.
  collect_expr <- bquote({
    .manifest <- file.path(.sd, "output_manifest.txt")
    .manifest_entries <- if (file.exists(.manifest)) readLines(.manifest) else character(0)
    c(
      list.files(file.path(.final_rep_dir, "log"), full.names = TRUE),
      list.files(
        file.path(.final_rep_dir, .(output_dirs_val)),
        full.names = TRUE,
        recursive = TRUE
      ),
      file.path(.final_rep_dir, .manifest_entries)
    )
  })

  ## ---- input-hash + skip check -----------------------------------------------------------------
  ## Idempotency is hash-based: a successful LANDIS-II run writes
  ## <rep_dir>/log/input_hash.json containing a digest of (per-input-file MD5,
  ## base_seed, rep_index, scenario_file). On subsequent runs the same digest
  ## is recomputed from current inputs and compared. If `Landis-log.txt`
  ## reports completion AND the hash matches, the rep is treated as up-to-date.
  ## If the hash mismatches (e.g. upstream inputs changed) the rep is rerun
  ## even though outputs nominally exist on disk -- which fixes the case where
  ## an output-existence-only skip check let stale outputs through after
  ## upstream invalidation.
  hash_expr <- bquote({
    .hash_file <- file.path(.final_rep_dir, "log", "input_hash.json")
    .input_hash <- digest::digest(
      list(
        files = vapply(sort(.dep_files), tools::md5sum, character(1L)),
        base_seed = .(base_seed_val),
        rep_index = .rep_idx,
        scenario_file = .(scenario_file)
      ),
      algo = "sha1"
    )
  })
  skip_check_expr <- bquote({
    .landis_log <- file.path(.final_rep_dir, "Landis-log.txt")
    .saved_hash <- if (file.exists(.hash_file)) {
      tryCatch(jsonlite::fromJSON(.hash_file)$input_hash, error = function(e) NA_character_)
    } else {
      NA_character_
    }
    .already_done <- !isTRUE(.(force_val)) &&
      file.exists(.landis_log) &&
      any(grepl("Model run is complete", readLines(.landis_log, warn = FALSE))) &&
      identical(.saved_hash, .input_hash)
  })
  ## Stage + run one replicate, then persist its input hash and move the
  ## completed rep from scratch to its final (tracked) home. Shared by the
  ## docker and local command branches; `.run_call` is the run expression.
  ##
  ## Cleanup contract:
  ##   - Success: `landis_archive_rep()` rsyncs scratch -> NFS, then deletes
  ##     the scratch rep dir (the standard path).
  ##   - Failure: the `tryCatch()` around `.(run_call)` ALSO archives scratch
  ##     -> NFS so the partial run (configs, `Landis-log.txt`, `docker_*.log`,
  ##     any partial outputs) is preserved on NFS for inspection, then deletes
  ##     the scratch rep dir. The original LANDIS-II error is re-thrown so
  ##     `tar_make` still marks the target as errored. If the failure-side
  ##     archive itself errors (e.g. rsync unavailable, NFS unmounted), the
  ##     scratch dir is left untouched and a warning is emitted -- never
  ##     swallow the run error or trap the scratch contents silently.
  run_and_archive_expr <- function(run_call) {
    bquote({
      ## stage this replicate under .stage_sd (scratch when work_root is set)
      fs::dir_create(.stage_sd)
      .run_rep_dir <- landisutils::landis_replicate(
        .stage_sd,
        rep_index = .rep_idx,
        files = .dep_files,
        base_seed = .(base_seed_val)
      )[[1L]]
      tryCatch(.(run_call), error = function(e) {
        tryCatch(
          landisutils::landis_archive_rep(.run_rep_dir, .final_rep_dir),
          error = function(e2) {
            warning(
              "failed-run archive to NFS failed; scratch rep dir retained at ",
              .run_rep_dir,
              "\n  archive error: ",
              conditionMessage(e2),
              call. = FALSE
            )
          }
        )
        stop(e)
      })
      ## persist the input hash next to the run, then move the completed rep to
      ## its final home (a no-op when .run_rep_dir resolves to .final_rep_dir).
      .run_hash_file <- file.path(.run_rep_dir, "log", "input_hash.json")
      dir.create(dirname(.run_hash_file), recursive = TRUE, showWarnings = FALSE)
      jsonlite::write_json(
        list(input_hash = .input_hash, written = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z")),
        .run_hash_file,
        auto_unbox = TRUE,
        pretty = TRUE
      )
      landisutils::landis_archive_rep(.run_rep_dir, .final_rep_dir)
    })
  }

  run_docker_call <- bquote(landisutils::landis_run_docker(
    scenario_dir = .run_rep_dir,
    scenario_file = .(scenario_file),
    image = .(image),
    console = .(console),
    pull = .(pull_val),
    cpu_limit = .(cpu_limit_val),
    mem_limit = .(mem_limit_val),
    mem_margin = .(mem_margin_val),
    post_completion_timeout_sec = .(post_completion_timeout_sec_val)
  ))
  run_local_call <- bquote(landisutils::landis_run_local(
    scenario_dir = .run_rep_dir,
    scenario_file = .(scenario_file),
    console = .(console)
  ))

  cmd <- if (identical(method, "docker")) {
    bquote({
      .(prep_expr)
      .(hash_expr)
      .(skip_check_expr)
      if (!.already_done) {
        .(run_and_archive_expr(run_docker_call))
      }
      .(collect_expr)
    })
  } else {
    bquote({
      .(prep_expr)
      .(hash_expr)
      .(skip_check_expr)
      if (!.already_done) {
        .(run_and_archive_expr(run_local_call))
      }
      .(collect_expr)
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
