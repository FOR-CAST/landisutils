## LANDIS-II runtime provenance ---------------------------------------------------------------
##
## Pure base-R parsers for the artifacts a LANDIS-II run leaves on disk
## (`Landis-log.txt`, the `docker_image.log` sidecar, the per-rep resource
## logs) plus the `prov_*()` collectors that turn those parsers' output into
## the small `data.frame`s a build-provenance report appendix renders.
##
## These were de-duplicated from the BC_HRV and gitanyow-partial-harvest
## report-pipeline templates (`R/helpers.R`, `R/build_provenance.R`); they
## are study-area-agnostic and depend only on other landisutils functions.

#' Parse a LANDIS-II `Landis-log.txt` for version metadata
#'
#' Extracts the console banner, user-supplied random number seed, and the
#' succession / disturbance / other extension version blocks from the header
#' of a LANDIS-II log file. The log format is identical across scenarios (the
#' LANDIS-II console writes the same banner on every run), so only the first
#' ~30 lines are read.
#'
#' @param path Character path to a `Landis-log.txt`.
#'
#' @return A named list with elements:
#'   \describe{
#'     \item{`console`}{Console version string (e.g. `"LANDIS-II 8.0 (8)"`),
#'       or `NA_character_`.}
#'     \item{`seed`}{Numeric user-supplied seed, or `NA_real_`.}
#'     \item{`succession`}{Character vector of `"Name vX.Y.Z"` for the
#'       succession extension (length 0 when absent).}
#'     \item{`disturbance`}{As `succession`, for disturbance extensions.}
#'     \item{`other`}{As `succession`, for other extensions.}
#'   }
#'   When `path` does not exist, every element takes its empty default.
#'
#' @family provenance helpers
#'
#' @export
parse_landis_log_versions <- function(path) {
  empty <- list(
    console = NA_character_,
    seed = NA_real_,
    succession = character(0),
    disturbance = character(0),
    other = character(0)
  )
  if (!file.exists(path)) {
    return(empty)
  }

  ## Read only the header (first ~30 lines); the rest is per-timestep noise.
  lines <- readLines(path, n = 30L, warn = FALSE)

  ## console version: first line of the form "LANDIS-II X.Y (Z)"
  console_line <- grep("LANDIS-II\\s+\\S+\\s+\\(\\d+\\)", lines, value = TRUE)[1]
  console <- if (is.na(console_line)) {
    NA_character_
  } else {
    sub(".*-\\s+(LANDIS-II.*)$", "\\1", console_line)
  }

  ## random number seed
  seed_line <- grep("user-supplied seed\\s*=\\s*[0-9,]+", lines, value = TRUE)[1]
  seed <- if (is.na(seed_line)) {
    NA_real_
  } else {
    as.numeric(gsub(",", "", sub(".*seed\\s*=\\s*([0-9,]+).*", "\\1", seed_line)))
  }

  ## Section helper: lines following a header until the next header / blank line.
  ## Headers are "Succession:", "Disturbance Extensions:", "Other Extensions:".
  ## Each section's body lines have the form "<Ext Name> Version: X.Y, Input: foo.txt".
  extract_section <- function(header) {
    idx <- grep(paste0("^", header, ":$"), lines)
    if (length(idx) == 0L) {
      return(character(0))
    }
    headers <- c("^Succession:$", "^Disturbance Extensions:$", "^Other Extensions:$")
    end_pattern <- paste(c(headers, "^\\s*$"), collapse = "|")
    body <- character(0)
    for (j in seq(idx[1L] + 1L, min(length(lines), idx[1L] + 20L))) {
      if (grepl(end_pattern, lines[j])) {
        break
      }
      body <- c(body, lines[j])
    }
    ## Convert "Foo Bar Version: 1.2, Input: x.txt" -> "Foo Bar v1.2"
    sub("\\s*Version:\\s*([0-9.]+).*$", " v\\1", body)
  }

  list(
    console = console,
    seed = seed,
    succession = extract_section("Succession"),
    disturbance = extract_section("Disturbance Extensions"),
    other = extract_section("Other Extensions")
  )
}

#' Read the LANDIS-II Docker image reference used for a run
#'
#' Looks for `<run_dir>/rep*/log/docker_image.log`, a one-line sidecar of the
#' form `ghcr.io/.../landis-ii-v8-release@sha256:<64hex>` written by
#' [landis_run_docker()] after `docker pull`. The immutable `sha256` digest
#' pins the exact runtime image; an image tag alone is mutable.
#'
#' @param run_dir Character path to a scenario run directory (containing
#'   `rep*` subdirectories).
#'
#' @return A named list with `image_name`, `image_digest`, and `source`:
#'   \describe{
#'     \item{`"captured"`}{Sidecar present (authoritative; the digest pins
#'       the runtime).}
#'     \item{`"option"`}{Sidecar absent; image name read from the
#'       `landisutils.docker.image` option on the current host (best-effort;
#'       the tag is mutable).}
#'     \item{`"local"`}{No sidecar and no Docker option set (a local install).}
#'   }
#'
#' @family provenance helpers
#'
#' @export
landis_image_info <- function(run_dir) {
  na_result <- list(image_name = NA_character_, image_digest = NA_character_, source = "local")
  if (!dir.exists(run_dir)) {
    return(na_result)
  }
  logs <- list.files(run_dir, pattern = "^docker_image\\.log$", recursive = TRUE, full.names = TRUE)
  if (length(logs) > 0L) {
    ref <- trimws(readLines(logs[1L], warn = FALSE)[1L])
    parts <- strsplit(ref, "@", fixed = TRUE)[[1L]]
    return(list(
      image_name = parts[1L],
      image_digest = if (length(parts) > 1L) parts[2L] else NA_character_,
      source = "captured"
    ))
  }
  opt_image <- getOption("landisutils.docker.image")
  if (!is.null(opt_image) && nzchar(opt_image)) {
    ## Mirror the captured-log parsing: a digest-pinned option
    ## (`repo:tag@sha256:...`) splits into name and digest.
    parts <- strsplit(opt_image, "@", fixed = TRUE)[[1L]]
    return(list(
      image_name = parts[1L],
      image_digest = if (length(parts) > 1L) parts[2L] else NA_character_,
      source = "option"
    ))
  }
  na_result
}

## ---- provenance collectors ----------------------------------------------------------------------

## Truncate a string to `max` characters, appending an ellipsis when cut.
## Keeps long unbreakable tokens (sha256 digests) from overflowing PDF cells.
.prov_truncate <- function(s, max = 24L) {
  if (is.null(s) || is.na(s)) {
    return(s)
  }
  if (nchar(s) <= max) {
    return(s)
  }
  paste0(substr(s, 1L, max - 3L), "...")
}

#' LANDIS-II console and extension versions (provenance)
#'
#' Parses the first replicate's `Landis-log.txt` under `run_dir` via
#' [parse_landis_log_versions()] and formats a compact `data.frame` for a
#' build-provenance report appendix.
#'
#' @param run_dir Character path to a scenario run directory.
#'
#' @return A two-column `data.frame` (`Component`, `Version(s)`), or `NULL`
#'   when no `Landis-log.txt` is found under `run_dir`.
#'
#' @family provenance helpers
#'
#' @export
prov_landis_versions <- function(run_dir) {
  log <- list.files(run_dir, pattern = "^Landis-log\\.txt$", recursive = TRUE, full.names = TRUE)[
    1L
  ]
  if (is.na(log)) {
    return(NULL)
  }
  v <- parse_landis_log_versions(log)
  fmt_exts <- function(xs) {
    if (length(xs)) paste0("`", xs, "`", collapse = "; ") else "none"
  }
  data.frame(
    Component = c("Console", "Succession", "Disturbance", "Other"),
    `Version(s)` = c(
      sprintf("`%s`", v$console %||% "n/a"),
      fmt_exts(v$succession),
      fmt_exts(v$disturbance),
      fmt_exts(v$other)
    ),
    check.names = FALSE,
    stringsAsFactors = FALSE,
    row.names = NULL
  )
}

#' LANDIS-II container image and digest (provenance)
#'
#' Resolves the runtime image for `run_dir` via [landis_image_info()] and
#' formats it as a `data.frame` for a build-provenance report appendix.
#'
#' @param run_dir Character path to a scenario run directory.
#'
#' @return A two-column `data.frame` (`Field`, `Value`).
#'
#' @family provenance helpers
#'
#' @export
prov_landis_container <- function(run_dir) {
  img <- landis_image_info(run_dir)
  source_str <- switch(
    img$source,
    captured = "captured at run time",
    option = "from current `landisutils.docker.image` option (not captured at run time)",
    local = "local install (no container)"
  )
  digest_cell <- if (is.na(img$image_digest)) {
    "*not captured*"
  } else {
    ## sha256 digests are 71 chars (`sha256:` + 64 hex) -- show a short form
    ## here so the cell doesn't overflow; the full digest lives in
    ## `<run_dir>/log/docker_image.log` and in INFO.md.
    sprintf("`%s`", .prov_truncate(img$image_digest, max = 23L))
  }
  data.frame(
    Field = c("Image", "Digest"),
    Value = c(sprintf("`%s` (%s)", img$image_name %||% "n/a", source_str), digest_cell),
    stringsAsFactors = FALSE,
    row.names = NULL
  )
}

#' LANDIS-II stochasticity summary (provenance)
#'
#' Reports the base random number seed (from the first replicate's
#' `Landis-log.txt`) and the replicate count (`rep*` subdirectories) under
#' `run_dir`, formatted for a build-provenance report appendix.
#'
#' @param run_dir Character path to a scenario run directory.
#'
#' @return A two-column `data.frame` (`Field`, `Value`).
#'
#' @family provenance helpers
#'
#' @export
prov_stochasticity <- function(run_dir) {
  log <- list.files(run_dir, pattern = "^Landis-log\\.txt$", recursive = TRUE, full.names = TRUE)[
    1L
  ]
  v <- if (is.na(log)) {
    list(seed = NA_real_)
  } else {
    parse_landis_log_versions(log)
  }
  seed_str <- if (is.na(v$seed)) "n/a" else format(v$seed)
  n_reps <- length(list.files(run_dir, pattern = "^rep\\d+$", include.dirs = TRUE))
  rep_label <- if (n_reps == 0L) {
    "n/a"
  } else {
    paste0("rep01..rep", sprintf("%02d", n_reps), " (n = ", n_reps, ")")
  }
  data.frame(
    Field = c("Base seed (rep01)", "Replicates", "Seed derivation"),
    Value = c(
      sprintf("`%s`", seed_str),
      rep_label,
      "deterministic function of `base_seed` and the replicate index"
    ),
    stringsAsFactors = FALSE,
    row.names = NULL
  )
}

#' LANDIS-II per-replicate resource use (provenance)
#'
#' Summarises per-replicate elapsed time and peak memory (mean +/- SD across
#' the replicates of a single scenario) from the resource logs read by
#' [read_landis_resource_logs()], formatted for a build-provenance report
#' appendix.
#'
#' @param run_dir Character path to a scenario run directory.
#'
#' @return A three-column `data.frame` (`Metric`, `Mean`, `SD`) with an
#'   `n_reps` attribute, or `NULL` when no resource logs are present (e.g.
#'   the run is still in flight).
#'
#' @family provenance helpers
#'
#' @export
prov_run_resources <- function(run_dir) {
  res <- tryCatch(read_landis_resource_logs(run_dir), error = function(e) NULL)
  if (is.null(res) || nrow(res) == 0L) {
    return(NULL)
  }
  fmt_duration <- function(sec) {
    if (!length(sec)) {
      return("n/a")
    }
    s <- as.integer(round(sec))
    if (s >= 3600L) {
      sprintf("%dh %02dm", s %/% 3600L, (s %% 3600L) %/% 60L)
    } else if (s >= 60L) {
      sprintf("%dm %02ds", s %/% 60L, s %% 60L)
    } else {
      sprintf("%ds", s)
    }
  }
  fmt_bytes <- function(b) {
    if (!length(b)) {
      return("n/a")
    }
    if (b >= 1024^3) sprintf("%.2f GiB", b / 1024^3) else sprintf("%.1f MiB", b / 1024^2)
  }
  el <- res$elapsed_sec[!is.na(res$elapsed_sec)]
  mem <- res$peak_mem_bytes[!is.na(res$peak_mem_bytes)]
  out <- data.frame(
    Metric = c("Wall-clock elapsed", "Peak memory (RSS)"),
    Mean = c(fmt_duration(mean(el)), fmt_bytes(mean(mem))),
    SD = c(
      if (length(el) > 1L) fmt_duration(stats::sd(el)) else "n/a",
      if (length(mem) > 1L) fmt_bytes(stats::sd(mem)) else "n/a"
    ),
    stringsAsFactors = FALSE,
    row.names = NULL
  )
  attr(out, "n_reps") <- nrow(res)
  out
}
