## Forest Product Sector Module (FPSM) execution ---------------------------------------------------
##
## FPSM is NOT a running LANDIS-II extension: its `PlugIn.Run()` is an empty stub
## and all the work happens in a plain console entry point taking one argument,
## the path to its configuration file. It is a post-processor over two ForCS flux
## logs, so it runs after a replicate finishes rather than inside the simulation.
##
## This file therefore deliberately does NOT reuse the `LandisExtension` R6
## contract, and the runner below is much simpler than `landis_run_docker()`: no
## core-version assertion, no startup jitter, no output streaming and no
## post-completion watchdog, because a whole FPSM run is about a second and peaks
## under 64 MB. What it does keep from that function is the part that matters for
## reproducibility -- capturing the immutable image digest of the bytes that ran.

#' Files FPSM writes into its working directory
#'
#' FPSM creates these three under fixed names in the current working directory,
#' which is why each run needs a directory of its own.
#'
#' @returns Character vector of file names.
#'
#' @family FPSM helpers
#'
#' @export
fps_output_files <- function() {
  c("FPS_log.txt", "FPS_raw_out.csv", "FPS_test_out.csv")
}

## Columns FPSM reads out of the ForCS flux logs, BY POSITION. FPSM splits each
## line on commas and indexes the result directly, with no header validation
## whatsoever, so a future ForCS release that inserts or reorders a column would
## be read silently as the wrong quantity. Positions here are 1-based (R); the
## corresponding 0-based indices in `ForestProps.cs::ReadHarvestFile()` are one
## less. `Time` and `species` are read for every row; the remaining entries are
## the carbon actually transferred to the product sector.
.fps_flux_columns <- list(
  live = c("1" = "Time", "5" = "species", "17" = "BioToFPS"),
  dom = c("1" = "Time", "5" = "species", "19" = "SnagsToFPS", "20" = "DOMtoFPS")
)

## Read the two input filenames FPSM will look for out of its configuration file.
.fps_declared_inputs <- function(config_path) {
  txt <- readLines(config_path, warn = FALSE)
  ## Strip `>>` comments before matching so a commented-out keyword is not picked up.
  txt <- sub(">>.*$", "", txt)
  grab <- function(key) {
    hit <- grep(paste0("^\\s*", key, "\\s"), txt, value = TRUE)
    if (length(hit) == 0L) {
      return(NA_character_)
    }
    val <- trimws(sub(paste0("^\\s*", key, "\\s+"), "", hit[1L]))
    gsub('^"|"$', "", val)
  }
  c(live = grab("HarvestFileLive"), dom = grab("HarvestFileDOM"))
}

## Assert the positional contract above still holds for one flux log.
.fps_check_flux_header <- function(path, which) {
  expected <- .fps_flux_columns[[which]]
  hdr <- trimws(strsplit(readLines(path, n = 1L, warn = FALSE), ",", fixed = TRUE)[[1L]])
  bad <- character()
  for (i in names(expected)) {
    idx <- as.integer(i)
    got <- if (idx <= length(hdr)) hdr[idx] else "<absent>"
    if (!identical(got, unname(expected[i]))) {
      bad <- c(bad, sprintf("column %d: expected '%s', found '%s'", idx, expected[i], got))
    }
  }
  if (length(bad) > 0L) {
    stop(
      "ForCS flux log does not match the column layout FPSM reads by position:\n  ",
      path,
      "\n  ",
      paste(bad, collapse = "\n  "),
      "\nFPSM indexes these columns directly and does not check the header, so ",
      "running anyway would silently read the wrong quantity.",
      call. = FALSE
    )
  }
  invisible(TRUE)
}

#' Run the Forest Product Sector Module in a Docker container
#'
#' Runs FPSM over one directory containing a configuration file and the two ForCS
#' flux logs it names, with that directory bind-mounted as the container's working
#' directory. FPSM writes [fps_output_files()] there under fixed names, so each
#' run needs its own directory.
#'
#' Three pre-flight checks run before the container starts, each guarding a
#' failure mode that is otherwise silent or obscure:
#'
#' * the input files named in the configuration must exist **with exactly that
#'   case**. The shipped FPSM examples name `log_fluxDOM.csv` beside a file
#'   called `log_FluxDOM.csv`, which works on Windows and aborts on Linux.
#' * each flux log's header must still match the positions FPSM indexes (see
#'   `.fps_flux_columns`), because FPSM performs no header validation and would
#'   otherwise read a reordered column as the wrong quantity.
#' * on completion, a non-empty `FPS_log.txt` is treated as an error by default.
#'   That file collects the *non-fatal* problems FPSM detects, which include carbon that
#'   was never allocated to any pool, so a silent run is the only acceptable one.
#'
#' @param run_dir Character. Directory holding the configuration and flux logs;
#'   bind-mounted as the container working directory.
#' @param config_file Character. Configuration file name, relative to `run_dir`.
#' @param image Character. Container image. Defaults to the
#'   `landisutils.fps.image` option.
#' @param assembly Character. Path to the FPSM assembly inside the image. Passed
#'   explicitly with `--entrypoint dotnet` rather than relying on the image's own
#'   `ENTRYPOINT`, so the function works with any image carrying dotnet and the
#'   assembly.
#' @param console Character. Path to the `docker` executable; defaults to
#'   [landis_find_docker()].
#' @param pull Logical. `docker pull` before running, so the captured digest
#'   reflects the registry rather than a possibly stale local copy.
#' @param cpu_limit,mem_limit Resource caps. FPSM is single-threaded and peaks
#'   well under 64 MB even on a 400-year replicate, so the defaults are generous.
#'   `NULL` or `Inf` omits the corresponding flag.
#' @param error_on_log Logical. Fail when `FPS_log.txt` is non-empty.
#' @param check_headers Logical. Perform the flux-log header assertion.
#'
#' @returns Character vector of paths to the files in [fps_output_files()], in
#'   that order, suitable for a `targets::tar_target(format = "file")`.
#'
#' @family FPSM helpers
#'
#' @seealso [landis_run_docker()], [landis_find_docker()]
#'
#' @export
fps_run_docker <- function(
  run_dir,
  config_file = "fps.txt",
  image = NULL,
  assembly = "/opt/fps/Landis.Extension.FPS-v1.dll",
  console = NULL,
  pull = FALSE,
  cpu_limit = 1,
  mem_limit = "1g",
  error_on_log = TRUE,
  check_headers = TRUE
) {
  image <- image %||% getOption("landisutils.fps.image")
  if (is.null(image)) {
    stop(
      "no FPSM image: pass `image` or set `options(landisutils.fps.image = ...)`.",
      call. = FALSE
    )
  }
  console <- console %||% landis_find_docker()
  run_dir <- fs::path_real(run_dir)

  cfg <- fs::path(run_dir, config_file)
  if (!fs::file_exists(cfg)) {
    stop("FPSM configuration file not found: ", cfg, call. = FALSE)
  }

  ## -- pre-flight 1: declared inputs exist, case-exactly.
  declared <- .fps_declared_inputs(cfg)
  present <- fs::path_file(fs::dir_ls(run_dir, type = "file"))
  for (which in names(declared)) {
    nm <- declared[[which]]
    if (is.na(nm)) {
      stop("configuration does not declare HarvestFile", toupper(which), ": ", cfg, call. = FALSE)
    }
    if (!nm %in% present) {
      ci <- present[tolower(present) == tolower(nm)]
      hint <- if (length(ci) > 0L) {
        sprintf(
          " A file differing only in case is present (%s). FPSM's own examples ship this mismatch; it is harmless on Windows and fatal here.",
          paste(ci, collapse = ", ")
        )
      } else {
        ""
      }
      stop(
        "input file named by the configuration is missing from ",
        run_dir,
        ": ",
        nm,
        hint,
        call. = FALSE
      )
    }
  }

  ## -- pre-flight 2: the positional column contract still holds.
  if (isTRUE(check_headers)) {
    for (which in names(declared)) {
      .fps_check_flux_header(fs::path(run_dir, declared[[which]]), which)
    }
  }

  if (isTRUE(pull)) {
    pull_rc <- system2(console, c("pull", image), stdout = FALSE, stderr = FALSE)
    if (pull_rc != 0L) {
      warning(
        sprintf("`docker pull %s` failed (exit %d); continuing with local image.", image, pull_rc),
        call. = FALSE
      )
    }
  }

  ## Image tags are mutable; the digest identifies the bytes that actually ran.
  ## Same sidecar convention as `landis_run_docker()`.
  digest_line <- tryCatch(
    {
      rd <- system2(
        console,
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
    digest_line <- sprintf("# %s (digest unavailable: not in the local image cache?)", image)
  }
  writeLines(digest_line, fs::path(run_dir, "docker_image.log"))

  ## `--user` is POSIX-only; without it the container writes root-owned output.
  user_args <- if (.Platform$OS.type != "windows") {
    c(
      "--user",
      paste0(trimws(system("id -u", intern = TRUE)), ":", trimws(system("id -g", intern = TRUE)))
    )
  } else {
    character(0)
  }
  cpu_args <- if (is.null(cpu_limit) || is.infinite(cpu_limit)) {
    character(0)
  } else {
    c("--cpus", as.character(cpu_limit))
  }
  mem_args <- if (is.null(mem_limit) || identical(mem_limit, Inf)) {
    character(0)
  } else {
    c("--memory", as.character(mem_limit))
  }

  ## Stale outputs would otherwise be indistinguishable from fresh ones if the
  ## container failed part way through.
  fs::file_delete(Filter(fs::file_exists, fs::path(run_dir, fps_output_files())))

  args <- c(
    "run",
    "--rm",
    user_args,
    cpu_args,
    mem_args,
    "--entrypoint",
    "dotnet",
    "-v",
    paste0(run_dir, ":/work"),
    "-w",
    "/work",
    image,
    assembly,
    config_file
  )
  out <- system2(console, args, stdout = TRUE, stderr = TRUE)
  rc <- attr(out, "status") %||% 0L
  if (rc != 0L) {
    stop(
      "FPSM run failed (exit ",
      rc,
      ") in ",
      run_dir,
      ":\n",
      paste(out, collapse = "\n"),
      call. = FALSE
    )
  }

  produced <- fs::path(run_dir, fps_output_files())
  missing <- produced[!fs::file_exists(produced)]
  if (length(missing) > 0L) {
    stop(
      "FPSM exited 0 but did not write: ",
      paste(fs::path_file(missing), collapse = ", "),
      call. = FALSE
    )
  }

  ## -- post-flight: FPSM's own non-fatal problem log must be empty.
  log_path <- fs::path(run_dir, "FPS_log.txt")
  log_lines <- readLines(log_path, warn = FALSE)
  log_lines <- log_lines[nzchar(trimws(log_lines))]
  if (length(log_lines) > 0L) {
    msg <- paste0(
      "FPSM reported ",
      length(log_lines),
      " non-fatal problem(s) in ",
      log_path,
      ". These include carbon that was not allocated to any pool, so the run is ",
      "not trustworthy:\n  ",
      paste(utils::head(log_lines, 10L), collapse = "\n  ")
    )
    if (isTRUE(error_on_log)) stop(msg, call. = FALSE) else warning(msg, call. = FALSE)
  }

  as.character(produced)
}
