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
#'   (`docker run --cpus`). Default `4`: LANDIS-II compute is single-threaded
#'   but the .NET runtime spins up 9-11 OS threads, so 4 is a comfortable
#'   headroom default. Pass `NULL` for no CPU limit.
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
  cpu_limit = 4,
  mem_limit = "8g",
  mem_margin = 1.5,
  post_completion_timeout_sec = 300
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
  ##
  ## Post-completion watchdog: some LANDIS-II runs (particularly long ForCS +
  ## Dynamic Fire scenarios with many output extensions) print
  ## "Model run is complete." but the dotnet process then hangs spinning at
  ## 100% CPU in the .NET runtime shutdown phase. Outputs are already on disk
  ## at that point, so we watch the stdout log for the completion marker and
  ## SIGTERM the container `post_completion_timeout_sec` later if the process
  ## hasn't exited on its own. The post-completion stop is treated as success
  ## (the sim itself finished cleanly).
  peak_mem_bytes <- 0
  completion_marker <- "Model run is complete."
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

  rc <- docker_proc$get_result()
  ## If we forced a stop after completion, treat as success (the sim itself
  ## finished cleanly; exit code 143 from SIGTERM or 137 from SIGKILL is the
  ## watchdog firing, not a real failure).
  if (stopped_for_timeout && rc %in% c(137L, 143L)) {
    message(glue::glue(
      "  [{Sys.time()}] docker exit {rc} treated as success ",
      "(post-completion watchdog; '{completion_marker}' was logged)."
    ))
    rc <- 0L
  }
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
#'   defaults are `4`, `"8g"`, and `1.5` respectively. No effect for
#'   `method = "local"`.
#' @param post_completion_timeout_sec Numeric. Passed to [landis_run_docker()]
#'   when `method = "docker"`: grace period (seconds) after the LANDIS-II
#'   console logs `"Model run is complete."` before the container is SIGTERMed
#'   if `dotnet` has not exited on its own. Guards against the known v8
#'   post-completion hang (long ForCS + Dynamic Fire sims spin at 100% CPU in
#'   the .NET shutdown path after outputs are already on disk). Default `300`
#'   (5 min); pass `Inf` to disable the watchdog. No effect for
#'   `method = "local"`.
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
  cpu_limit = 4,
  mem_limit = "8g",
  mem_margin = 1.5,
  post_completion_timeout_sec = 300,
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

    ## Create/verify exactly this replicate's directory (idempotent).
    .rep_dir <- landisutils::landis_replicate(
      .sd,
      rep_index = .rep_idx,
      files = .dep_files,
      base_seed = .(base_seed_val)
    )[[1L]]
  })

  ## ---- output-collection expression ------------------------------------------------------------
  collect_expr <- bquote({
    .manifest <- file.path(.sd, "output_manifest.txt")
    .manifest_entries <- if (file.exists(.manifest)) readLines(.manifest) else character(0)
    c(
      list.files(file.path(.rep_dir, "log"), full.names = TRUE),
      list.files(file.path(.rep_dir, .(output_dirs_val)), full.names = TRUE, recursive = TRUE),
      file.path(.rep_dir, .manifest_entries)
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
    .hash_file <- file.path(.rep_dir, "log", "input_hash.json")
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
    .landis_log <- file.path(.rep_dir, "Landis-log.txt")
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
  ## After a successful run, persist the hash so the next tar_make can detect
  ## "no-op" reruns (deps unchanged) and skip cleanly.
  write_hash_expr <- bquote({
    dir.create(dirname(.hash_file), recursive = TRUE, showWarnings = FALSE)
    jsonlite::write_json(
      list(input_hash = .input_hash, written = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z")),
      .hash_file,
      auto_unbox = TRUE,
      pretty = TRUE
    )
  })

  cmd <- if (identical(method, "docker")) {
    bquote({
      .(prep_expr)
      .(hash_expr)
      .(skip_check_expr)
      if (!.already_done) {
        landisutils::landis_run_docker(
          scenario_dir = .rep_dir,
          scenario_file = .(scenario_file),
          image = .(image),
          console = .(console),
          pull = .(pull_val),
          cpu_limit = .(cpu_limit_val),
          mem_limit = .(mem_limit_val),
          mem_margin = .(mem_margin_val),
          post_completion_timeout_sec = .(post_completion_timeout_sec_val)
        )
        .(write_hash_expr)
      }
      .(collect_expr)
    })
  } else {
    bquote({
      .(prep_expr)
      .(hash_expr)
      .(skip_check_expr)
      if (!.already_done) {
        landisutils::landis_run_local(
          scenario_dir = .rep_dir,
          scenario_file = .(scenario_file),
          console = .(console)
        )
        .(write_hash_expr)
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
