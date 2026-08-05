## LANDIS-II core version detection + enforcement.
##
## Every config file this package writes targets ONE core generation's grammar. A
## console from a different generation does not reject those files cleanly -- it
## mis-parses them, and the run either dies deep inside extension loading with an
## unrelated message or, worse, completes against a misread scenario. Nothing
## downstream distinguishes that from a good run, so the version is checked BEFORE
## any LANDIS-II process is asked to do work.

## THE core generation this package is built for. This is the switch to flip when v9
## lands and supersedes v8: it seeds the `landisutils.landis.version` option in
## .onLoad(), and the runtime guard, its messages, and the default image tag and
## console path are all derived from it rather than repeating the number.
##
## Flipping it is necessary but NOT sufficient -- the config writers in `ext_*.R`
## encode the grammar itself, and a new core generation is exactly the kind of change
## that alters it. Treat a bump here as the start of that work, not the end.
.landis_version_default <- 8L

#' The LANDIS-II core generation this package targets
#'
#' Reads the `landisutils.landis.version` option, which `.onLoad()` seeds from the
#' package's built-for generation. Set the option to override it for a session.
#'
#' @returns Integer major version, e.g. `8L`.
#'
#' @family LANDIS-II execution helpers
#' @seealso [landis_assert_version()]
#'
#' @export
landis_target_version <- function() {
  as.integer(getOption("landisutils.landis.version", .landis_version_default))
}

## Parse the core version out of console output. The console announces itself on its
## first line, both in `Landis-log.txt` and on stdout when invoked with no scenario:
##
##   LANDIS-II 8.0 (8)
##
## Returns a list(version = "8.0", major = 8L), or NULL when no banner is present --
## the caller decides what an unreadable probe means.
.parse_landis_version <- function(txt) {
  if (!length(txt)) {
    return(NULL)
  }
  m <- regmatches(txt, regexpr("LANDIS-II[[:space:]]+[0-9]+(\\.[0-9]+)*", txt))
  m <- m[nzchar(m)]
  if (!length(m)) {
    return(NULL)
  }
  ver <- sub("^LANDIS-II[[:space:]]+", "", m[[1L]])
  major <- suppressWarnings(as.integer(strsplit(ver, ".", fixed = TRUE)[[1L]][[1L]]))
  if (is.na(major)) {
    return(NULL)
  }
  list(version = ver, major = major)
}

## Memo cache, keyed by the thing being probed (image tag, container name, or console
## path). Per-process: a calibration worker probes its own pool once and no more.
.landis_version_cache <- new.env(parent = emptyenv())

## Probe a console, image or container for its major version. Delegates to
## [landis_version()] so there is ONE implementation of "run the console and read its
## banner"; this wrapper only reduces the result to an integer major or NULL.
.probe_landis_version <- function(image = NULL, container = NULL, console = NULL) {
  v <- tryCatch(
    landis_version(console = console, image = image, container = container),
    error = function(e) NA
  )
  if (length(v) != 1L || identical(v, NA) || is.na(v)) {
    return(NULL)
  }
  list(version = as.character(v), major = as.integer(unclass(v)[[1L]][1L]))
}

#' Require a specific LANDIS-II core generation before running anything
#'
#' Probes the LANDIS-II console that a run is about to use and throws unless its
#' major version is `version`. Exactly one of `image`, `container` or `console`
#' identifies what to probe; the result is memoized per process against that key,
#' so a pool probes once rather than once per replicate.
#'
#' An **undetectable** version is treated as a failure, not as permission to
#' proceed: a probe that returns nothing is indistinguishable from a console of the
#' wrong generation that never announced itself, and proceeding is the exact
#' failure this guards against. Set
#' `options(landisutils.skip_version_check = TRUE)` to bypass the check where that
#' is genuinely wanted -- an explicit, visible opt-out rather than a silent one.
#'
#' @param version Integer. Required major version. Defaults to
#'   [landis_target_version()], i.e. the generation this package is built for.
#' @param image Character. Docker image to probe with a throwaway container.
#' @param container Character. Name of a running container to probe via `docker exec`.
#' @param console Character. Path to `Landis.Console.dll` for a local (non-Docker) run.
#'
#' @returns The detected version string (invisibly), e.g. `"8.0"`.
#'
#' @family LANDIS-II execution helpers
#' @seealso [landis_target_version()], [landis_pool_start()], [landis_run_docker()]
#'
#' @export
landis_assert_version <- function(
  version = landis_target_version(),
  image = NULL,
  container = NULL,
  console = NULL
) {
  version <- as.integer(version)
  stopifnot(length(version) == 1L, !is.na(version))
  if (isTRUE(getOption("landisutils.skip_version_check", FALSE))) {
    return(invisible(NA_character_))
  }
  ## `c()` rather than `paste0()`: paste0() of all-NULL yields character(0), and the
  ## length-zero `if` that follows is an error rather than the intended message.
  key <- c(
    if (!is.null(container)) paste0("container:", container),
    if (!is.null(image)) paste0("image:", image),
    if (!is.null(console)) paste0("console:", console)
  )
  if (length(key) != 1L) {
    stop(
      "landis_assert_version(): supply exactly one of `image`, `container` or `console`.",
      call. = FALSE
    )
  }
  what <- sub("^[a-z]+:", "", key)
  ## The required version is part of the cache key: a cached PASS for v8 says nothing
  ## about whether the same image satisfies v9.
  key <- paste0("v", version, "|", key)
  if (!is.null(hit <- .landis_version_cache[[key]])) {
    return(invisible(hit))
  }

  got <- .probe_landis_version(image = image, container = container, console = console)

  if (is.null(got)) {
    stop(
      "could not determine the LANDIS-II version of ",
      what,
      ".\n",
      "Refusing to run: an unreadable version is not evidence of a v",
      version,
      " core, and every config file this package writes targets the v",
      version,
      " grammar.\n",
      "Set options(landisutils.skip_version_check = TRUE) to override.",
      call. = FALSE
    )
  }
  if (!identical(got$major, version)) {
    stop(
      "LANDIS-II v",
      version,
      " is required, but ",
      what,
      " reports version ",
      got$version,
      ".\n",
      "The config files this package writes use the v",
      version,
      " grammar; a v",
      got$major,
      " core mis-parses them rather than rejecting them.",
      call. = FALSE
    )
  }

  assign(key, got$version, envir = .landis_version_cache)
  invisible(got$version)
}
