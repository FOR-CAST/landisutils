#!/usr/bin/env Rscript
##
## Install the LANDIS-II v8 console and every extension shipped in the
## landis-ii-v8-release Docker image, natively on a Windows machine. Used
## by `.github/workflows/landis-integration-native.yaml` so the
## integration-test scenarios produced by `build_scenarios.R` can be
## executed via `dotnet Landis.Console.dll` (see `run_native.R`).
##
## Strategy:
##
## * Core console: WiX MSI from Core-Model-v8 (pinned in `_pins.R`).
##   Installed silently via `msiexec /i ... /quiet /norestart` with a
##   verbose log captured next to the MSI for failure triage. Installs
##   to `Program Files\LANDIS-II-v8\v8\` and seeds `extensions.xml`.
## * Extensions: Inno Setup 6 installers from each extension repo's
##   `deploy/` (or GitHub releases for Klemet). Same flag set works for
##   all of them: `/VERYSILENT /SUPPRESSMSGBOXES /NORESTART /SP-`. Each
##   installer registers its DLL in `extensions.xml`.
##
## The extension list is fetched from
## `Tool-Docker-Apptainer/extensions-v8-release.yaml` at the same SHA
## `build_scenarios.R` uses, so the (release) docker workflow and the
## native Windows workflow exercise the same set of extensions.
##
## Usage:
##   Rscript install_landis_windows.R [--dry-run] [--download-dir <path>]
##
## Environment:
##   GITHUB_PAT (or GITHUB_TOKEN) raises the GitHub API rate limit.
##   LANDIS_DOWNLOAD_DIR          override download cache dir
##                                (default: %RUNNER_TEMP%/landis-installers
##                                or tempdir() outside CI).
##
## Exit code: 0 on success. Non-zero if Core MSI install fails (fatal).
## Extension install failures emit a `::warning::` and continue; the
## per-scenario `run_native.R` invocation surfaces the real failure as a
## `::error::` so the workflow stays red without masking which scenarios
## are broken.

if (.Platform$OS.type != "windows") {
  stop("install_landis_windows.R is for Windows runners only", call. = FALSE)
}

suppressPackageStartupMessages({
  library(gh)
  library(cli)
})

args <- commandArgs(trailingOnly = TRUE)
dry_run <- "--dry-run" %in% args

## Resolve script dir so we can source `_pins.R` whether the script is
## invoked as `Rscript install_landis_windows.R` or via `R --file=...`.
.this_script_dir <- (function() {
  cargs <- commandArgs(trailingOnly = FALSE)
  m <- regmatches(cargs, regexpr("(?<=--file=).+", cargs, perl = TRUE))
  if (length(m) > 0L) {
    dirname(normalizePath(m[1]))
  } else {
    getwd()
  }
})()
source(file.path(.this_script_dir, "_pins.R"))

YAML_URL <- sprintf(
  "https://raw.githubusercontent.com/%s/%s/extensions-v8-release.yaml",
  TDA_REPO,
  TDA_REF
)

download_dir <- {
  i <- match("--download-dir", args)
  if (!is.na(i) && i < length(args)) {
    args[i + 1L]
  } else {
    Sys.getenv("LANDIS_DOWNLOAD_DIR", unset = "")
  }
}
if (!nzchar(download_dir)) {
  runner_temp <- Sys.getenv("RUNNER_TEMP", unset = "")
  download_dir <- if (nzchar(runner_temp)) {
    file.path(runner_temp, "landis-installers")
  } else {
    file.path(tempdir(), "landis-installers")
  }
}
dir.create(download_dir, recursive = TRUE, showWarnings = FALSE)
download_dir <- normalizePath(download_dir, mustWork = TRUE)

## ---------------------------------------------------------------------------
## Helpers (adapted from scripts/update_landis_extensions.R)
## ---------------------------------------------------------------------------

parse_extensions_yaml <- function(text) {
  lines <- strsplit(text, "\n")[[1]]
  repo <- org <- NULL
  out <- list()
  for (line in lines) {
    stripped <- trimws(line)
    if (nchar(stripped) == 0 || startsWith(stripped, "#")) {
      next
    }
    if (grepl("^-\\s*repo:\\s*(\\S+)", stripped)) {
      repo <- sub("^-\\s*repo:\\s*(\\S+).*", "\\1", stripped)
      org <- NULL
    } else if (grepl("^org:\\s*(\\S+)", stripped) && !is.null(repo)) {
      org <- sub("^org:\\s*(\\S+).*", "\\1", stripped)
      out <- c(out, list(list(repo = repo, org = org)))
      repo <- org <- NULL
    }
  }
  do.call(rbind, lapply(out, as.data.frame, stringsAsFactors = FALSE))
}

ver_key <- function(name) {
  m <- regmatches(name, regexpr("(\\d+(?:\\.\\d+)*)-setup\\.exe$", name, perl = TRUE))
  if (length(m) == 0) {
    return(numeric(0))
  }
  as.numeric(strsplit(sub("-setup\\.exe$", "", m), "\\.")[[1]])
}

compare_ver <- function(a, b) {
  av <- ver_key(a$name)
  bv <- ver_key(b$name)
  len <- max(length(av), length(bv))
  av <- c(av, rep(0, len - length(av)))
  bv <- c(bv, rep(0, len - length(bv)))
  for (i in seq_along(av)) {
    if (av[i] != bv[i]) return(av[i] > bv[i])
  }
  FALSE
}

latest_v8_installer <- function(items) {
  if (!is.list(items) || length(items) == 0) {
    return(NULL)
  }
  names_urls <- lapply(items, function(item) {
    list(name = item$name, url = item$download_url, size = item$size)
  })
  v8 <- Filter(
    function(x) {
      startsWith(x$name, "LANDIS-II-V8") && endsWith(x$name, "-setup.exe")
    },
    names_urls
  )
  if (length(v8) == 0) {
    return(NULL)
  }
  sorted <- v8
  for (i in seq_along(sorted)) {
    for (j in seq_len(i - 1)) {
      if (compare_ver(sorted[[j]], sorted[[i]])) {
        tmp <- sorted[[i]]
        sorted[[i]] <- sorted[[j]]
        sorted[[j]] <- tmp
      }
    }
  }
  tail(sorted, 1)[[1]]
}

find_deploy_installer <- function(org, repo) {
  for (path in c("deploy/installer", "deploy/current", "deploy")) {
    items <- tryCatch(
      gh("GET /repos/{owner}/{repo}/contents/{path}", owner = org, repo = repo, path = path),
      error = function(e) NULL
    )
    result <- latest_v8_installer(items)
    if (!is.null(result)) return(result)
  }
  NULL
}

find_release_installer <- function(org, repo) {
  releases <- tryCatch(
    gh("GET /repos/{owner}/{repo}/releases", owner = org, repo = repo),
    error = function(e) NULL
  )
  if (is.null(releases) || length(releases) == 0) {
    return(NULL)
  }
  for (release in releases) {
    for (asset in release$assets) {
      if (startsWith(asset$name, "LANDIS-II-V8") && endsWith(asset$name, "-setup.exe")) {
        return(list(name = asset$name, url = asset$browser_download_url, size = asset$size))
      }
    }
  }
  NULL
}

download_with_check <- function(url, dest, expected_size = NULL) {
  rc <- tryCatch(
    {
      utils::download.file(url, destfile = dest, mode = "wb", quiet = TRUE)
      0L
    },
    error = function(e) {
      cli_alert_danger("download failed: {conditionMessage(e)}")
      1L
    }
  )
  if (rc != 0L || !file.exists(dest)) {
    return(FALSE)
  }
  if (!is.null(expected_size) && expected_size > 0L) {
    actual <- file.size(dest)
    if (!is.na(actual) && actual != expected_size) {
      cli_alert_danger("size mismatch for {basename(dest)}: expected {expected_size}, got {actual}")
      file.remove(dest)
      return(FALSE)
    }
  }
  TRUE
}

## Inno Setup 6 silent flags (confirmed by inspecting strings of multiple
## extension installers under /mnt/software/LANDIS-II/v8/extensions/).
INNO_SILENT_FLAGS <- c("/VERYSILENT", "/SUPPRESSMSGBOXES", "/NORESTART", "/SP-")

run_inno_installer <- function(exe_path) {
  cli_alert_info("inno-setup: {basename(exe_path)}")
  if (dry_run) {
    cli_alert_info("  (dry-run; skipping)")
    return(0L)
  }
  tryCatch(
    {
      out <- system2(exe_path, args = INNO_SILENT_FLAGS, wait = TRUE, stdout = TRUE, stderr = TRUE)
      st <- attr(out, "status")
      if (is.null(st)) 0L else as.integer(st)
    },
    error = function(e) {
      cli_alert_danger("install error: {conditionMessage(e)}")
      127L
    }
  )
}

## WiX MSI silent install. `/qn` is the standard "no UI" UILevel; combined
## with `/norestart` to keep the runner alive. Verbose log lands next to
## the MSI for triage.
run_msi_installer <- function(msi_path) {
  cli_alert_info("msi: {basename(msi_path)}")
  if (dry_run) {
    cli_alert_info("  (dry-run; skipping)")
    return(0L)
  }
  log_path <- paste0(msi_path, ".install.log")
  tryCatch(
    {
      out <- system2(
        "msiexec.exe",
        args = c("/i", shQuote(msi_path), "/qn", "/norestart", "/l*v", shQuote(log_path)),
        wait = TRUE,
        stdout = TRUE,
        stderr = TRUE
      )
      st <- attr(out, "status")
      st <- if (is.null(st)) 0L else as.integer(st)
      if (st != 0L) {
        cli_alert_warning("msiexec exit {st}; see log: {log_path}")
      }
      st
    },
    error = function(e) {
      cli_alert_danger("msiexec error: {conditionMessage(e)}")
      127L
    }
  )
}

## Search for `Landis.Console.dll` under the standard Windows install
## roots. Restricts the recursion to LANDIS-II-named subtrees so we don't
## walk the entire Program Files tree (which is slow on GHA runners).
find_console_dll <- function() {
  roots <- c(
    Sys.getenv("ProgramFiles"),
    Sys.getenv("ProgramFiles(x86)"),
    file.path(Sys.getenv("LOCALAPPDATA"), "Programs"),
    Sys.getenv("LOCALAPPDATA"),
    Sys.getenv("APPDATA")
  )
  roots <- unique(roots[nzchar(roots) & dir.exists(roots)])
  for (root in roots) {
    landis_dirs <- list.dirs(root, recursive = FALSE, full.names = TRUE)
    landis_dirs <- landis_dirs[grepl("LANDIS-II", basename(landis_dirs), ignore.case = TRUE)]
    for (d in landis_dirs) {
      hits <- list.files(
        d,
        pattern = "^Landis\\.Console\\.dll$",
        recursive = TRUE,
        full.names = TRUE,
        ignore.case = TRUE
      )
      if (length(hits) > 0L) return(hits[1])
    }
  }
  NULL
}

## ---------------------------------------------------------------------------
## Rate limit + Core install (MSI)
## ---------------------------------------------------------------------------

rate_info <- tryCatch(gh("GET /rate_limit"), error = function(e) NULL)
if (!is.null(rate_info)) {
  cli_alert_info("GitHub API: {rate_info$rate$remaining}/{rate_info$rate$limit} requests remaining")
}

cli_h1("Installing LANDIS-II Core console (MSI)")
core_msi <- file.path(download_dir, basename(CORE_MSI_PATH))
if (!file.exists(core_msi)) {
  cli_alert_info("downloading {basename(CORE_MSI_PATH)}")
  if (!download_with_check(CORE_MSI_URL, core_msi)) {
    cli_abort("Core MSI download failed: {CORE_MSI_URL}")
  }
}
core_status <- run_msi_installer(core_msi)
if (core_status != 0L) {
  cli_abort("Core MSI install failed (msiexec exit {core_status}); see {core_msi}.install.log")
}
cli_alert_success("Core console installed")

## ---------------------------------------------------------------------------
## Extensions (Inno Setup)
## ---------------------------------------------------------------------------

cli_h1("Fetching extension list from Tool-Docker-Apptainer")
yaml_text <- paste(readLines(YAML_URL, warn = FALSE), collapse = "\n")
extensions <- parse_extensions_yaml(yaml_text)
cli_alert_info("found {nrow(extensions)} extensions")

n_ok <- 0L
n_fail <- 0L
n_skip <- 0L

for (i in seq_len(nrow(extensions))) {
  repo <- extensions$repo[i]
  org <- extensions$org[i]

  result <- if (org == "Klemet") {
    find_release_installer(org, repo)
  } else {
    find_deploy_installer(org, repo)
  }

  if (is.null(result)) {
    cli_alert_warning("SKIP {repo} (no V8 installer found)")
    n_skip <- n_skip + 1L
    next
  }

  dest <- file.path(download_dir, result$name)
  if (!file.exists(dest)) {
    cli_alert_info("downloading {result$name}")
    if (!download_with_check(result$url, dest, result$size)) {
      cat(sprintf("::warning::failed to download %s\n", result$name), file = stderr())
      n_fail <- n_fail + 1L
      next
    }
  }

  status <- run_inno_installer(dest)
  if (status == 0L) {
    n_ok <- n_ok + 1L
  } else {
    cat(sprintf("::warning::installer %s exited %d\n", result$name, status), file = stderr())
    n_fail <- n_fail + 1L
  }
}

cli_rule()
cli_alert_info("Done: {n_ok} installed, {n_fail} failed, {n_skip} no-installer")

## ---------------------------------------------------------------------------
## Locate and report Landis.Console.dll for the run step.
## ---------------------------------------------------------------------------

console_dll <- find_console_dll()
if (is.null(console_dll)) {
  cli_abort("Landis.Console.dll not found after Core install; check {core_msi}.install.log")
}

cli_alert_success("Landis.Console.dll: {console_dll}")

github_env <- Sys.getenv("GITHUB_ENV", unset = "")
if (nzchar(github_env)) {
  cat(sprintf("LANDIS_CONSOLE_DLL=%s\n", console_dll), file = github_env, append = TRUE)
  cli_alert_info("exported LANDIS_CONSOLE_DLL to GITHUB_ENV")
}
