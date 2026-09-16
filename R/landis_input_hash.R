#' Input hash of one LANDIS-II replicate
#'
#' A digest of what a replicate was run from: the MD5 of every dependency file (keyed by path), the
#' base seed, the replicate index and the scenario file name. [tar_landis()] writes it to
#' `<rep>/log/input_hash.json` after a run and compares it before the next, via
#' [landis_rep_is_current()].
#'
#' Paths are ordered by bytes (`sort(method = "radix")`), so the hash does not depend on the
#' collation locale of the R process computing it. R collates with ICU in UTF-8 locales and by bytes
#' under C/POSIX; a locale-sorted hash differs between a worker started with and without `LANG`.
#'
#' R also collates by bytes, whatever [Sys.setlocale()] says, while the environment variable
#' `LC_ALL` (or, when that is unset, `LC_COLLATE`) is `C`. For a named `collation` the sort therefore
#' runs with `LC_ALL` unset and `LC_COLLATE` set to `collation`, as in a process started in that
#' locale, and both are restored afterwards.
#'
#' @param dep_files Character. The dependency files staged into the replicate.
#' @param base_seed Integer. The base random seed.
#' @param rep_index Integer. The replicate index.
#' @param scenario_file Character(1). The scenario file name.
#' @param collation `NULL` (the default) for byte order. Otherwise an `LC_COLLATE` locale to sort in,
#'   which reproduces the hash landisutils 0.0.149 and earlier wrote in that locale; `""` means the
#'   locale the process's environment selects, as when R started.
#'
#' @return Character(1), a SHA-1 digest; `NA_character_` if `collation` cannot be set.
#' @export
landis_input_hash <- function(dep_files, base_seed, rep_index, scenario_file, collation = NULL) {
  files <- as.character(dep_files)
  if (is.null(collation)) {
    files <- sort(files, method = "radix")
  } else {
    old_collate <- Sys.getlocale("LC_COLLATE")
    old_env <- Sys.getenv(c("LC_ALL", "LC_COLLATE"), unset = NA)
    on.exit(
      {
        restore <- old_env[!is.na(old_env)]
        if (length(restore) > 0L) {
          do.call(Sys.setenv, as.list(restore))
        }
        Sys.unsetenv(names(old_env)[is.na(old_env)])
        Sys.setlocale("LC_COLLATE", old_collate)
      },
      add = TRUE
    )
    if (nzchar(collation)) {
      Sys.unsetenv("LC_ALL")
      Sys.setenv(LC_COLLATE = collation)
    }
    if (!nzchar(suppressWarnings(Sys.setlocale("LC_COLLATE", collation)))) {
      return(NA_character_)
    }
    files <- sort(files)
  }
  digest::digest(
    list(
      files = vapply(files, tools::md5sum, character(1L)),
      base_seed = base_seed,
      rep_index = rep_index,
      scenario_file = scenario_file
    ),
    algo = "sha1"
  )
}

#' Is a LANDIS-II replicate already complete for these inputs?
#'
#' `TRUE` when `Landis-log.txt` reports a completed run and `log/input_hash.json` records the input
#' hash of `dep_files` et al. Besides the current [landis_input_hash()], the hash landisutils 0.0.149
#' and earlier would have written is accepted, sorted in this process's locale or under `C.UTF-8`
#' (ICU). A replicate finished by an earlier version is therefore not re-simulated after an upgrade,
#' whichever of those locales wrote it.
#'
#' @param rep_dir Character(1). The replicate directory.
#' @inheritParams landis_input_hash
#' @param force Logical(1). `TRUE` never counts a replicate as current.
#'
#' @return Logical(1).
#' @export
landis_rep_is_current <- function(
  rep_dir,
  dep_files,
  base_seed,
  rep_index,
  scenario_file,
  force = FALSE
) {
  if (isTRUE(force)) {
    return(FALSE)
  }
  log_file <- file.path(rep_dir, "Landis-log.txt")
  if (
    !file.exists(log_file) ||
      !any(grepl("Model run is complete", readLines(log_file, warn = FALSE), fixed = TRUE))
  ) {
    return(FALSE)
  }
  hash_file <- file.path(rep_dir, "log", "input_hash.json")
  saved <- if (file.exists(hash_file)) {
    tryCatch(jsonlite::fromJSON(hash_file)$input_hash, error = function(e) NA_character_)
  } else {
    NA_character_
  }
  if (!is.character(saved) || length(saved) != 1L || is.na(saved)) {
    return(FALSE)
  }
  candidates <- c(
    landis_input_hash(dep_files, base_seed, rep_index, scenario_file),
    landis_input_hash(dep_files, base_seed, rep_index, scenario_file, collation = ""),
    landis_input_hash(dep_files, base_seed, rep_index, scenario_file, collation = "C.UTF-8")
  )
  saved %in% candidates[!is.na(candidates)]
}
