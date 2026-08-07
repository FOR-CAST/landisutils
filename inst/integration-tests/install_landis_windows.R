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
  ## msiexec will not open a package whose path mixes separators, and answers 1619
  ## (ERROR_INSTALL_PACKAGE_OPEN_FAILED) rather than saying so. The path arrives that way by
  ## construction: `download_dir` is normalizePath()'d (backslashes) and the filename is joined
  ## with file.path() (forward slash), giving `D:\a\_temp\landis-installers/LANDIS-II-8.0-setup64.msi`.
  ## Quoting does not help -- shQuote() already emits the double quotes msiexec wants.
  ##
  ## chartr() rather than normalizePath(winslash = "\\"): that only rewrites separators on Windows,
  ## and only for a path that already exists, so it cannot be exercised anywhere else and would
  ## silently do nothing if the file were missing. This is unconditional and testable.
  msi_path <- chartr("/", "\\", msi_path)
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
## Extensions whose installer is re-run after every other, so that the shared cohort library in the
## install tree is the build THEY were compiled against. See the re-install block below for why.
##
## Only Biomass Succession for now. The upstream list carries four succession backends (Biomass,
## ForCS, NECN, PnET) and they cannot all win: each re-install overwrites the last. If the scenarios
## for the other three start failing with the same type mismatch, that is the signal that their
## installers ship *different* library builds, and the fix becomes re-installing the relevant
## backend immediately before its own scenarios rather than once at the end.
REINSTALL_LAST <- c("Extension-Biomass-Succession")

## Report which copy of each shared library is actually present after install. Version plus
## timestamp: the timestamp is what identifies which installer wrote it, and that is the thing a
## type-identity mismatch turns on.
report_shared_libraries <- function() {
  console <- find_console_dll()
  if (is.null(console)) {
    return(invisible(NULL))
  }
  ## Search the whole LANDIS-II install tree, not dirname(console). Extensions install into sibling
  ## directories and register themselves in extensions.xml, so scanning only the console's own
  ## directory finds essentially nothing -- the first version of this reported a single
  ## Landis.Library.Metadata dll and no UniversalCohorts at all, which is what made the assembly
  ## mismatch impossible to attribute.
  root <- dirname(dirname(console))
  libs <- list.files(
    root,
    pattern = "^Landis\\.Library\\..*\\.dll$",
    recursive = TRUE,
    full.names = TRUE,
    ignore.case = TRUE
  )
  if (length(libs) == 0L) {
    cli_alert_warning("no Landis.Library.*.dll found under {root}")
    return(invisible(NULL))
  }

  ## Where LANDIS-II actually loads assemblies from. Two scans of the filesystem have now failed to
  ## locate Landis.Library.UniversalCohorts -- the library the "data type of site variable X is T,
  ## not T" abort is about -- so stop guessing at install layout and read the registry LANDIS-II
  ## itself consults.
  ext_xml <- list.files(
    dirname(console),
    pattern = "^extensions\\.xml$",
    recursive = TRUE,
    full.names = TRUE,
    ignore.case = TRUE
  )
  if (length(ext_xml) > 0L) {
    cli_h1("extensions.xml: {ext_xml[[1]]}")
    xml <- readLines(ext_xml[[1]], warn = FALSE)
    asm <- unique(trimws(regmatches(xml, regexpr("(?<=<Assembly>)[^<]+", xml, perl = TRUE))))
    cli_alert_info("{length(asm)} assemblies registered")
    ## Resolve each registered assembly to a file and report duplicates by size.
    found <- unlist(lapply(unique(asm), function(a) {
      list.files(
        dirname(console),
        pattern = paste0("^", gsub("([.\\\\+*?\\[^\\]$(){}=!<>|:-])", "\\\\\\1", a), "\\.dll$"),
        recursive = TRUE,
        full.names = TRUE,
        ignore.case = TRUE
      )
    }))
    if (length(found) > 0L) {
      cli_alert_info("resolved {length(found)} assembly file(s) under {dirname(console)}")
    }
  } else {
    cli_alert_warning("no extensions.xml found under {dirname(console)}")
  }

  ## Broad sweep: the shared cohort libraries wherever they live on this machine.
  sweep_roots <- unique(Filter(
    function(x) nzchar(x) && dir.exists(x),
    c(Sys.getenv("ProgramFiles"), Sys.getenv("ProgramFiles(x86)"), Sys.getenv("LOCALAPPDATA"))
  ))
  cohort <- unlist(lapply(sweep_roots, function(r) {
    list.files(
      r,
      pattern = "^Landis\\.Library\\.(UniversalCohorts|Succession).*\\.dll$",
      recursive = TRUE,
      full.names = TRUE,
      ignore.case = TRUE
    )
  }))
  if (length(cohort) > 0L) {
    cli_h1("Cohort/succession libraries on this machine")
    ci <- file.info(cohort)
    for (i in order(basename(cohort))) {
      cli_alert_info(
        "{basename(cohort[i])}  {format(ci$size[i], big.mark = ',')} bytes  {cohort[i]}"
      )
    }
  } else {
    cli_alert_warning(
      "no UniversalCohorts/Succession libraries found in: {paste(sweep_roots, collapse = ', ')}"
    )
  }

  cli_h1("Shared libraries present after install")
  info <- file.info(libs)
  ## Group by library name. More than one copy of a name means more than one assembly identity is
  ## loadable, and differing SIZES mean they are genuinely different builds -- which is the
  ## precondition for LANDIS-II aborting with "the data type of site variable X is T, not T".
  for (nm in sort(unique(basename(libs)))) {
    idx <- which(basename(libs) == nm)
    sizes <- unique(info$size[idx])
    if (length(idx) == 1L) {
      cli_alert_info("{nm}  {format(info$size[idx], big.mark = ',')} bytes")
    } else if (length(sizes) == 1L) {
      cli_alert_info("{nm}  {length(idx)} identical copies  {format(sizes, big.mark = ',')} bytes")
    } else {
      cli_alert_danger(
        "{nm}  {length(idx)} copies with {length(sizes)} DIFFERENT builds -- assembly conflict"
      )
      for (i in idx) {
        cli_alert_warning(
          "    {format(info$size[i], big.mark = ',')} bytes  {format(info$mtime[i], '%H:%M:%S')}  {sub(root, '', libs[i], fixed = TRUE)}"
        )
      }
    }
  }
  invisible(libs)
}

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

## The Core MSI was the one download with no integrity check: the extension assets pass an
## `expected_size` from the contents API, this did not. A truncated or partial download therefore
## surfaced as msiexec 1619 at install time rather than as a failed download, which is both later
## and far less legible. Ask the API for the size the same way the extensions do.
core_owner <- sub("/.*$", "", CORE_MSI_REPO)
core_repo <- sub("^.*/", "", CORE_MSI_REPO)
core_size <- tryCatch(
  gh(
    "GET /repos/{owner}/{repo}/contents/{path}",
    owner = core_owner,
    repo = core_repo,
    path = CORE_MSI_PATH,
    ref = CORE_MSI_REF
  )$size,
  error = function(e) {
    cli_alert_warning("could not resolve Core MSI size from the API: {conditionMessage(e)}")
    NULL
  }
)

## A cached MSI is only reusable if it is the right size. `landis-installers` is restored from the
## Actions cache, so a bad download would otherwise be cached and replayed on every later run.
if (file.exists(core_msi) && !is.null(core_size)) {
  cached <- file.size(core_msi)
  if (!is.na(cached) && cached != core_size) {
    cli_alert_warning("cached MSI is {cached} bytes, expected {core_size}; re-downloading")
    file.remove(core_msi)
  }
}
if (!file.exists(core_msi)) {
  cli_alert_info(
    "downloading {basename(CORE_MSI_PATH)}{if (is.null(core_size)) '' else paste0(' (', core_size, ' bytes)')}"
  )
  if (!download_with_check(CORE_MSI_URL, core_msi, expected_size = core_size)) {
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

reinstall_paths <- list()
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

  if (repo %in% REINSTALL_LAST) {
    reinstall_paths[[repo]] <- dest
  }
}

cli_rule()
cli_alert_info("Done: {n_ok} installed, {n_fail} failed, {n_skip} no-installer")

## ---------------------------------------------------------------------------
## Re-install the succession extension LAST
## ---------------------------------------------------------------------------
##
## The extension installers each carry their own copy of the shared cohort library
## (`Landis.Library.UniversalCohorts.dll` and friends) and write it into the same install tree, so
## whichever runs last decides which build every extension binds against. When a later installer
## overwrites the copy the succession extension was built against, LANDIS-II aborts at run time with
##
##   The data type of site variable "Succession.UniversalCohorts" is
##   Landis.Library.UniversalCohorts.SiteCohorts, not Landis.Library.UniversalCohorts.SiteCohorts
##
## -- the same type name on both sides, because the two are the same type from *different*
## assemblies. Re-running the succession installer at the end restores its copy and the types match.
##
## Deliberately a re-install rather than a reorder: `parse_extensions_yaml()` reflects the upstream
## extension list, and its order is left exactly as published. This appends one step instead of
## perturbing that, so the log says plainly what happened and why.
for (repo in REINSTALL_LAST) {
  dest <- reinstall_paths[[repo]]
  if (is.null(dest) || !file.exists(dest)) {
    cli_alert_warning("cannot re-install {repo} last: installer not found")
    next
  }
  cli_h1("Re-installing {repo} last (shared cohort library must be its build)")
  status <- run_inno_installer(dest)
  if (status != 0L) {
    cat(sprintf("::warning::re-install of %s exited %d\n", repo, status), file = stderr())
  }
}

## Which copy of each shared library actually won, so a future mismatch is evidence rather than
## inference. Version + timestamp; the timestamp is what identifies the installer that wrote it.
report_shared_libraries()

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
