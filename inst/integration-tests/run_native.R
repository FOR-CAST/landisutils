#!/usr/bin/env Rscript
##
## Run a LANDIS-II scenario natively (no Docker), via the installed
## `dotnet` CLI and a discovered `Landis.Console.dll`. Cross-platform
## counterpart to `run_in_docker.sh`; used by the Windows integration-test
## workflow but works anywhere LANDIS-II is installed natively.
##
## Usage:
##   Rscript run_native.R <scenario_dir> [<scenario_file>]
##
## Console DLL resolution order (first match wins):
##   1. LANDIS_CONSOLE_DLL environment variable (set by install_landis_
##      windows.R on Windows runners).
##   2. LANDIS_CONSOLE environment variable (legacy / hand-set).
##   3. First `Landis.Console.dll` found under a small set of standard
##      Windows install roots.
##
## Exit code propagates the LANDIS-II process exit. Stdout/stderr stream
## to the caller (no log capture; the workflow uses ::group:: markers to
## fold output per scenario).

args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 1L || length(args) > 2L) {
  message("usage: Rscript run_native.R <scenario_dir> [<scenario_file>]")
  quit(status = 64L)
}

scen_dir <- normalizePath(args[1], mustWork = TRUE)
scen_file <- if (length(args) == 2L) args[2] else paste0(basename(scen_dir), ".txt")

if (!file.exists(file.path(scen_dir, scen_file))) {
  message(sprintf("error: %s does not exist", file.path(scen_dir, scen_file)))
  quit(status = 66L)
}

resolve_console_dll <- function() {
  for (envvar in c("LANDIS_CONSOLE_DLL", "LANDIS_CONSOLE")) {
    v <- Sys.getenv(envvar, unset = "")
    if (nzchar(v) && file.exists(v)) return(v)
  }
  if (.Platform$OS.type != "windows") {
    return(NULL)
  }
  roots <- c(
    Sys.getenv("ProgramFiles"),
    Sys.getenv("ProgramFiles(x86)"),
    file.path(Sys.getenv("LOCALAPPDATA"), "Programs"),
    Sys.getenv("LOCALAPPDATA"),
    Sys.getenv("APPDATA")
  )
  roots <- unique(roots[nzchar(roots) & dir.exists(roots)])
  for (root in roots) {
    ## Only scan LANDIS-II* subtrees -- recursive scans of full Program
    ## Files are slow.
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

console_dll <- resolve_console_dll()
if (is.null(console_dll)) {
  message(
    "error: could not locate Landis.Console.dll. ",
    "Set LANDIS_CONSOLE_DLL or run install_landis_windows.R first."
  )
  quit(status = 70L)
}

if (Sys.which("dotnet") == "") {
  message("error: `dotnet` not on PATH. The .NET runtime is required to run LANDIS-II.")
  quit(status = 71L)
}

message(sprintf("LANDIS-II: %s", console_dll))
message(sprintf("scenario:  %s/%s", scen_dir, scen_file))

old_wd <- getwd()
on.exit(setwd(old_wd), add = TRUE)
setwd(scen_dir)

status <- system2("dotnet", args = c(shQuote(console_dll), shQuote(scen_file)))

quit(status = as.integer(status), save = "no")
