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
#' Up to 0.0.168, [landis_dep_files()] also staged other scenarios' files, and the hash covered
#' them. When `dep_files` carries those files as its `"foreign"` attribute, as [landis_dep_files()]
#' returns it, that hash is accepted too. It is rebuilt from `dep_files` and the copies in `rep_dir`
#' of files named in the attribute, with the MD5 of the copy rather than of the source, each keyed
#' by a path it could have been staged from: one in the attribute, or the same name in another
#' directory beside the replicate's scenario directory. A change to `dep_files` still makes the
#' replicate stale; a change to another scenario's file, which this replicate does not read, does
#' not. A copy whose name no file in the attribute has is not considered.
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
  if (saved %in% candidates[!is.na(candidates)]) {
    return(TRUE)
  }
  saved %in% .foreign_staged_hashes(rep_dir, dep_files, base_seed, rep_index, scenario_file)
}

## The hashes landisutils 0.0.168 and earlier could have written for a replicate into which
## landis_dep_files() staged other scenarios' files: the replicate's own files plus each such file
## found in `rep_dir`, keyed by the path it was staged from, with the MD5 of the staged copy. That
## path is not recorded, and which scenario's copy was staged depended on `deps` when the replicate
## ran, so each path with the file's name is tried: those in `attr(dep_files, "foreign")`, current
## order first, then those in other directories beside the scenario directory, for a scenario that
## has since left `deps`. Beyond `max_candidates` combinations, only the first path of each name.
.foreign_staged_hashes <- function(
  rep_dir,
  dep_files,
  base_seed,
  rep_index,
  scenario_file,
  max_candidates = 4096L
) {
  files <- as.character(dep_files)
  ## a replicate that ran another scenario's scenario file is that scenario's run
  if (!basename(scenario_file) %in% basename(files)) {
    return(character(0))
  }
  foreign <- as.character(attr(dep_files, "foreign", exact = TRUE))
  foreign <- foreign[!basename(foreign) %in% basename(files)]
  foreign <- foreign[utils::file_test("-f", file.path(rep_dir, basename(foreign)))]
  if (!length(foreign)) {
    return(character(0))
  }
  sd <- as.character(fs::path_real(dirname(rep_dir)))
  siblings <- tryCatch(
    setdiff(as.character(fs::dir_ls(dirname(sd), type = "directory")), sd),
    error = function(e) character(0)
  )
  sources <- lapply(unique(basename(foreign)), function(b) {
    beside <- file.path(siblings, b)
    beside <- as.character(fs::path_real(beside[utils::file_test("-f", beside)]))
    unique(c(foreign[basename(foreign) == b], beside))
  })
  names(sources) <- unique(basename(foreign))
  if (prod(lengths(sources)) > max_candidates) {
    sources <- lapply(sources, `[`, 1L)
  }
  keys <- as.matrix(expand.grid(sources, stringsAsFactors = FALSE))
  staged <- unname(tools::md5sum(file.path(rep_dir, names(sources))))
  own <- vapply(files, tools::md5sum, character(1L))
  apply(keys, 1L, function(k) {
    md5 <- c(own, stats::setNames(staged, k))
    ## the digest landis_input_hash() writes, over these keys
    digest::digest(
      list(
        files = md5[order(names(md5), method = "radix")],
        base_seed = base_seed,
        rep_index = rep_index,
        scenario_file = scenario_file
      ),
      algo = "sha1"
    )
  })
}
