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
## IMPORTANT -- what this does and does NOT reproduce.
##
## A Tool-Docker-Apptainer YAML supplies the list of extension REPOS, and this
## script installs each repo's newest PUBLISHED Windows installer. It does not
## read the `commit:` each entry pins. The docker images build that pinned
## source; this installs whatever binary the maintainer last uploaded. The two
## are routinely different generations, so the Windows legs reproduce NEITHER
## docker image -- they test the third thing, which is what a Windows user
## actually gets from the LANDIS-II site today.
##
## That distinction is load-bearing, and mistaking it is what produced a wrong
## exclusion once already. `extensions-v8-release.yaml` is the UCLv1 set: its
## pinned succession sources build against UniversalCohorts-v1/Succession-v9,
## and it deliberately sources Output-Biomass-By-Age from a fork because that
## fork holds the only v1 source whose `.csproj` has a working HintPath. But the
## newest published installer for most of those same repos is now v2. Installing
## "the release list" on Windows therefore yields a MIX, and the v1 stragglers in
## it have to be found by measurement rather than assumed from the list's name.
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

## Which extension generation to install. The v8 collection ships two: extensions built against
## UniversalCohorts v1 / Succession v9, and those rebuilt against v2 / v10. Both install side by
## side under different filenames, so nothing overwrites anything -- but an extension compiled
## against one cannot agree with an extension compiled against the other on the type of the
## `Succession.UniversalCohorts` site variable, and LANDIS-II aborts printing the same type name on
## both sides of "is not". Install order has no bearing on it, which is why the workflow runs one
## leg per generation rather than trying to install both.
##
## `extensions-v8-UCL2-release.yaml` is upstream's own curation of the v2 subset (16 extensions
## against the release list's 25) and is the default; the workflow sets LANDIS_EXTENSIONS_YAML per
## leg. Taking upstream's list means the set stays correct as extensions are rebuilt, rather than
## becoming a list this repo maintains.
EXTENSIONS_YAML <- Sys.getenv("LANDIS_EXTENSIONS_YAML", unset = "extensions-v8-UCL2-release.yaml")

YAML_URL <- sprintf(
  "https://raw.githubusercontent.com/%s/%s/%s",
  TDA_REPO,
  TDA_REF,
  EXTENSIONS_YAML
)

## Which cohort-library generation this leg is assembling a runnable set for. Extensions binding
## anything else are excluded from the SCENARIOS after installation -- see the measured exclusion
## below. `2` is the only sensible default: every succession extension with a current build targets
## UniversalCohorts-v2, so a v1 binder is by definition the one that cannot join the landscape.
TARGET_COHORT_GENERATION <- as.integer(Sys.getenv("LANDIS_TARGET_COHORT_GENERATION", unset = "2"))

## NOTE: there is deliberately no hand-maintained exclusion list here any more.
##
## There used to be one, keyed by YAML, holding `Extension-Social-Climate-Fire` on the grounds that
## SCRAPPLE binds the older library. That was wrong, and wrong in a way a list of names cannot
## catch: SCRAPPLE binds UniversalCohorts-v2 in both the jameslamping and LANDIS-II-Foundation
## repos, loads cleanly, and writes its output rasters. The abort blamed on it actually fired three
## extensions later, when Output Biomass-by-Age loaded -- `extensions-v8-release.yaml` sources that
## one from a fork whose `deploy/` only ever carried the v1 4.0 binary, while upstream has published
## a v2 4.1. Excluding SCRAPPLE removed a scenario from the matrix and left the real defect in it.
##
## So the exclusion is now MEASURED rather than declared: install everything the YAML lists, read
## the generation each extension binary actually binds (`assembly_refs()`), and exclude the ones
## that disagree with TARGET_COHORT_GENERATION. That is self-correcting -- an extension rejoins the
## matrix the moment upstream publishes a rebuilt installer, with no edit here.
EXTENSIONS_EXCLUDED <- character(0)

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

## Also captures each entry's `commit:`. The installer lookup needs it because upstream publishes a
## rebuilt installer on the branch the YAML pins BEFORE merging it to the default branch -- the
## UCLv2 PnET-Succession and Output-PnET 6.1 installers sat on `UCL_update` while `master` still
## offered the UCLv1 6.0.3, so a default-branch-only lookup silently installed the older generation.
parse_extensions_yaml <- function(text) {
  lines <- strsplit(text, "\n")[[1]]
  cur <- NULL
  out <- list()
  flush <- function() {
    if (!is.null(cur) && !is.null(cur$repo) && !is.null(cur$org)) {
      out[[length(out) + 1L]] <<- list(
        repo = cur$repo,
        org = cur$org,
        commit = if (is.null(cur$commit)) NA_character_ else cur$commit
      )
    }
  }
  for (line in lines) {
    stripped <- trimws(line)
    if (nchar(stripped) == 0 || startsWith(stripped, "#")) {
      next
    }
    if (grepl("^-\\s*repo:\\s*(\\S+)", stripped)) {
      flush()
      cur <- list(repo = sub("^-\\s*repo:\\s*(\\S+).*", "\\1", stripped))
    } else if (grepl("^org:\\s*(\\S+)", stripped) && !is.null(cur)) {
      cur$org <- sub("^org:\\s*(\\S+).*", "\\1", stripped)
    } else if (grepl("^commit:\\s*(\\S+)", stripped) && !is.null(cur)) {
      cur$commit <- sub("^commit:\\s*(\\S+).*", "\\1", stripped)
    }
  }
  flush()
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

## Search the default branch AND the ref the YAML pins, then take the highest version across both.
##
## Neither alone is right. Default-branch-only misses an installer published on the branch the
## image is built from (PnET-Succession 6.1, the UCLv2 rebuild, lives on `UCL_update` while `master`
## still offers the UCLv1 6.0.3). Pinned-ref-only silently downgrades extensions whose maintainer
## published a newer installer after the pinned commit (Biomass Succession 7.2.1 -> 7.2, NECN 8.2.1
## -> 8.2). The union gets the newest of each, and the measured generation check downstream is what
## guarantees the resulting set is coherent -- so preferring "newest" here is safe.
find_deploy_installer <- function(org, repo, ref = NULL) {
  gather <- function(r) {
    for (path in c("deploy/installer", "deploy/current", "deploy")) {
      args <- list(
        "GET /repos/{owner}/{repo}/contents/{path}",
        owner = org,
        repo = repo,
        path = path
      )
      if (!is.null(r) && !is.na(r) && nzchar(r)) {
        args$ref <- r
      }
      items <- tryCatch(do.call(gh, args), error = function(e) NULL)
      if (!is.null(latest_v8_installer(items))) {
        return(items)
      }
    }
    list()
  }
  latest_v8_installer(c(gather(NULL), gather(ref)))
}

find_release_installer <- function(org, repo) {
  releases <- tryCatch(
    gh("GET /repos/{owner}/{repo}/releases", owner = org, repo = repo),
    error = function(e) NULL
  )
  if (is.null(releases) || length(releases) == 0) {
    return(NULL)
  }
  ## Collect across ALL releases and version-sort, rather than taking the first V8 asset of the
  ## newest release. Those coincide today only because each maintainer's newest release happens to
  ## carry their highest version; a hotfix re-released against an older line, or a prerelease at the
  ## top of the list, would otherwise silently downgrade the installed extension. Same ordering as
  ## the `deploy/` path, so both sources answer "latest" the same way.
  assets <- list()
  for (release in releases) {
    for (asset in release$assets) {
      if (startsWith(asset$name, "LANDIS-II-V8") && endsWith(asset$name, "-setup.exe")) {
        assets <- c(
          assets,
          list(list(
            name = asset$name,
            download_url = asset$browser_download_url,
            size = asset$size
          ))
        )
      }
    }
  }
  latest_v8_installer(assets)
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

## The library versions an extension is built against decide whether it can share a landscape with
## the others. Two major versions of the cohort library ship in the v8 extension set --
## UniversalCohorts v1 and v2 (and Succession v9 and v10) -- installed side by side under different
## filenames, so nothing overwrites anything and install order is irrelevant. An extension compiled
## against v1 and one compiled against v2 cannot agree on the type of the `Succession.UniversalCohorts`
## site variable, and LANDIS-II aborts with the same type name printed on both sides of "is not".
##
## These two functions record which installer drops which library, and which library each extension
## binary actually references, so the whitelist is built from evidence rather than assumption.
.LANDIS_SPLIT_LIBS <- c(
  "Landis.Library.UniversalCohorts-v1",
  "Landis.Library.UniversalCohorts-v2",
  "Landis.Library.Succession-v9",
  "Landis.Library.Succession-v10"
)

## Files (name -> size) currently in the extensions directory.
snapshot_ext_dir <- function(ext_dir) {
  if (!dir.exists(ext_dir)) {
    return(setNames(numeric(0), character(0)))
  }
  f <- list.files(ext_dir, full.names = TRUE)
  stats::setNames(file.info(f)$size, basename(f))
}

## Which of .LANDIS_SPLIT_LIBS a .NET assembly references. Assembly references are stored as plain
## strings in the metadata, so a byte scan is enough and needs no .NET tooling on the runner.
assembly_refs <- function(dll) {
  raw <- tryCatch(readBin(dll, "raw", file.size(dll)), error = function(e) raw(0))
  if (length(raw) == 0L) {
    return(character(0))
  }
  txt <- rawToChar(raw[raw != as.raw(0)])
  Encoding(txt) <- "bytes"
  .LANDIS_SPLIT_LIBS[vapply(
    .LANDIS_SPLIT_LIBS,
    function(p) grepl(p, txt, fixed = TRUE, useBytes = TRUE),
    logical(1)
  )]
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

cli_alert_info("installing {nrow(extensions)} extension(s) from {EXTENSIONS_YAML}")

## Everything the YAML lists gets INSTALLED, including binaries from the wrong generation. That is
## safe: an extension the console never loads is inert on disk, and installing it is what lets us
## read which library it binds. The exclusion is applied to the SCENARIOS afterwards, once measured.

installer_libs <- list()
## Extension DLLs each installer drops, so a measured generation can be attributed back to the repo
## that supplied it -- which is what `LANDIS_EXCLUDE_EXTENSIONS` (a list of repo names) needs.
installer_exts <- list()
## The Core MSI has been installed by this point, so the console (and thus the extensions dir
## beside it) can be located. NULL-safe: the diff below simply records nothing if it is missing.
.console_now <- find_console_dll()
ext_dir <- if (is.null(.console_now)) {
  ""
} else {
  file.path(dirname(dirname(.console_now)), "extensions")
}
n_ok <- 0L
n_fail <- 0L
n_skip <- 0L

for (i in seq_len(nrow(extensions))) {
  repo <- extensions$repo[i]
  org <- extensions$org[i]

  result <- if (org == "Klemet") {
    find_release_installer(org, repo)
  } else {
    find_deploy_installer(org, repo, extensions$commit[i])
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

  before <- snapshot_ext_dir(ext_dir)
  status <- run_inno_installer(dest)
  if (status == 0L) {
    n_ok <- n_ok + 1L
  } else {
    cat(sprintf("::warning::installer %s exited %d\n", result$name, status), file = stderr())
    n_fail <- n_fail + 1L
  }
  after <- snapshot_ext_dir(ext_dir)
  changed <- names(after)[
    !(names(after) %in% names(before)) |
      (names(after) %in% names(before) & after[names(after)] != before[names(after)])
  ]
  libs_dropped <- grep("^Landis\\.Library\\.", changed, value = TRUE)
  if (length(libs_dropped) > 0L) {
    installer_libs[[repo]] <- libs_dropped
  }
  exts_dropped <- grep("^Landis\\.Extension\\..*\\.dll$", changed, value = TRUE)
  if (length(exts_dropped) > 0L) {
    installer_exts[[repo]] <- exts_dropped
  }
}

cli_rule()
cli_alert_info("Done: {n_ok} installed, {n_fail} failed, {n_skip} no-installer")

## Which installer dropped which cohort/succession library, and which version each extension
## binary binds to. Together these say exactly which extensions belong in a UCLv2 + Succession-v10
## whitelist and which have not been rebuilt against them.
report_library_split <- function() {
  if (length(installer_libs) > 0L) {
    cli_h1("Cohort/succession libraries by installer")
    for (repo in names(installer_libs)) {
      split_libs <- grep(
        paste(.LANDIS_SPLIT_LIBS, collapse = "|"),
        installer_libs[[repo]],
        value = TRUE
      )
      if (length(split_libs) > 0L) {
        ## Precompute: cli PARSES the contents of `{}`, so a regex with escapes inside an
        ## interpolation is a parse error at runtime rather than a string.
        libs_txt <- paste(sub("[.]dll$", "", split_libs), collapse = ", ")
        cli_alert_info("{repo}: {libs_txt}")
      }
    }
  }

  if (!nzchar(ext_dir) || !dir.exists(ext_dir)) {
    return(invisible(NULL))
  }
  dlls <- list.files(ext_dir, pattern = "\\.dll$", full.names = TRUE)
  dlls <- dlls[!grepl("^Landis\\.Library\\.", basename(dlls))]
  if (length(dlls) == 0L) {
    return(invisible(NULL))
  }
  cli_h1("Which cohort library each extension binds to")
  v2 <- character(0)
  v1 <- character(0)
  for (d in sort(dlls)) {
    refs <- assembly_refs(d)
    ## No reference to either generation: the extension does not touch the cohort library at all
    ## (several outputs are like this). It cannot conflict, so it is deliberately in NEITHER bucket
    ## and is never excluded.
    if (length(refs) == 0L) {
      next
    }
    tag <- paste(sub("Landis\\.Library\\.", "", refs), collapse = ", ")
    if (any(grepl("UniversalCohorts-v2|Succession-v10", refs))) {
      v2 <- c(v2, basename(d))
      cli_alert_success("{basename(d)}  -> {tag}")
    } else {
      v1 <- c(v1, basename(d))
      cli_alert_danger("{basename(d)}  -> {tag}")
    }
  }
  cli_alert_info("UCLv2/Succession-v10: {length(v2)} extension(s); older: {length(v1)}")
  invisible(list(v2 = v2, v1 = v1))
}
measured <- report_library_split()

## ---------------------------------------------------------------------------
## Measured exclusion: which repos supplied an extension of the wrong generation
## ---------------------------------------------------------------------------

## Translate the measured per-DLL generation back into the repo names build_scenarios.R filters on.
## Attribution comes from the before/after diff of the extensions directory taken around each
## installer, so it reflects what this run actually installed rather than a static table.
wrong_gen_dlls <- if (is.null(measured)) {
  character(0)
} else if (TARGET_COHORT_GENERATION == 2L) {
  measured$v1
} else {
  measured$v2
}

## Precomputed, not inlined into the cli string below: cli PARSES the contents of `{}`, so keeping
## expressions out of interpolations avoids a class of runtime parse error that only fires on the
## Windows runner.
wrong_gen <- if (TARGET_COHORT_GENERATION == 2L) 1L else 2L

## An installer can also drop a wrong-generation LIBRARY beside a correct extension, and that
## poisons a run just as surely: Forest Roads binds UniversalCohorts-v2 itself but ships
## `Landis.Library.HarvestManagement-v4.dll`, which binds v1. Scanning only `Landis.Extension.*`
## therefore passes it. Attribute libraries the same way, from the same per-installer diff.
libs_wrong_gen_repos <- character(0)
for (repo in names(installer_libs)) {
  for (lib in installer_libs[[repo]]) {
    p <- file.path(ext_dir, lib)
    if (!nzchar(ext_dir) || !file.exists(p)) {
      next
    }
    refs <- assembly_refs(p)
    if (length(refs) == 0L) {
      next
    }
    gen <- if (any(grepl("UniversalCohorts-v2|Succession-v10", refs))) 2L else 1L
    if (gen != TARGET_COHORT_GENERATION) {
      libs_wrong_gen_repos <- c(libs_wrong_gen_repos, repo)
      lib_txt <- sub("[.]dll$", "", lib)
      cli_alert_danger("{repo} ships {lib_txt}, which binds UniversalCohorts-v{gen}")
    }
  }
}
libs_wrong_gen_repos <- unique(libs_wrong_gen_repos)

if (length(wrong_gen_dlls) > 0L && length(installer_exts) > 0L) {
  hit <- vapply(installer_exts, function(dlls) any(dlls %in% wrong_gen_dlls), logical(1))
  EXTENSIONS_EXCLUDED <- names(installer_exts)[hit]

  ## A wrong-generation DLL nobody claims would be silently kept in every scenario, which is the
  ## failure mode this whole mechanism exists to prevent. Say so rather than under-reporting.
  attributed <- unlist(installer_exts[hit], use.names = FALSE)
  orphaned <- setdiff(wrong_gen_dlls, attributed)
  if (length(orphaned) > 0L) {
    orphan_txt <- paste(orphaned, collapse = ", ")
    cat(
      sprintf(
        "::warning::%d wrong-generation extension DLL(s) could not be attributed to an installer and are NOT excluded: %s\n",
        length(orphaned),
        orphan_txt
      ),
      file = stderr()
    )
  }
}

EXTENSIONS_EXCLUDED <- sort(unique(c(EXTENSIONS_EXCLUDED, libs_wrong_gen_repos)))

cli_h1("Extensions excluded from scenarios (measured)")
if (length(EXTENSIONS_EXCLUDED) > 0L) {
  for (d in EXTENSIONS_EXCLUDED) {
    cli_alert_warning("{d}: binds UniversalCohorts-v{wrong_gen}")
  }
} else {
  cli_alert_success("none -- every installed extension binds v{TARGET_COHORT_GENERATION}")
}

## Export so build_scenarios.R (a later workflow step, hence a separate process) builds scenarios
## against the set that can actually share a landscape. Without it the two disagree and LANDIS-II
## aborts at run time -- either with the cohort type mismatch, or with `No extension with the name
## ...` if a scenario references something that was never installed.
github_env_excl <- Sys.getenv("GITHUB_ENV", unset = "")
if (nzchar(github_env_excl) && length(EXTENSIONS_EXCLUDED) > 0L) {
  cat(
    sprintf("LANDIS_EXCLUDE_EXTENSIONS=%s\n", paste(EXTENSIONS_EXCLUDED, collapse = ",")),
    file = github_env_excl,
    append = TRUE
  )
  cli_alert_info("exported LANDIS_EXCLUDE_EXTENSIONS to GITHUB_ENV")
}

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
