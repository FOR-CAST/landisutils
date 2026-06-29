#' `LandisScenario` class
#'
#' Keeps track of a scenario's files and all its extensions' input files.
#'
#' @export
LandisScenario <- R6Class(
  "LandisScenario",

  public = list(
    #' @field path character specifying the (directory) path to the scenario file
    path = NULL,

    #' @field files character specifying the filenames of the scenario files
    #' (relative to `path`)
    files = character(0),

    #' @field extensions list of `LandisExtension` objects
    extensions = list(),

    #' @param path character specifying the (directory) path to the scenario file
    #'
    #' @param file character specifying the filename of the scenario file
    #' (relative to `path`)
    #'
    #' @param extensions list of `LandisExtension` objects
    initialize = function(path = NA_character_, extensions = list()) {
      self$path <- path
      self$extensions <- extensions
    },

    #' @param value if specified, the new value to append to `files`
    add_file = function(value) {
      stopifnot(file.exists(file.path(self$path, value)))
      self$files <- c(self$files, fs::path_tidy(value)) |> unique()
    },

    #' @param type character specifying the extension type
    #' (must be one of: `r paste(.extTypes, collapse = ", ")`)
    list_extensions = function(type) {
      lapply(self$extensions, function(ext) {
        if (ext$type %in% type) ext$LandisData else NULL
      })
    },

    #' @param full.names logical indicating whether full absolute file paths should be returned
    list_files = function(full.names = TRUE) {
      all_files <- lapply(self$extensions, function(ext) ext$files) |> unlist()
      if (isTRUE(full.names)) {
        fs::path_abs(all_files, self$path)
      } else {
        fs::path_tidy(all_files)
      }
    },

    #' @param n integer specifying the number of replicates to generate
    replicate = function(n) {
      new_reps <- self$reps + seq_len(n)
      ## combine scenario-level files with all extension-registered input files
      all_files <- unique(c(
        fs::path_abs(self$files, self$path),
        self$list_files(full.names = TRUE)
      ))
      all_files <- all_files[fs::file_exists(all_files)]
      ## auto-include GDAL sidecar files (.tfw, .aux.xml) for any .tif
      tif_files <- all_files[grepl("\\.tif$", all_files, ignore.case = TRUE)]
      sidecars <- fs::path(c(
        paste0(tif_files, ".aux.xml"),
        sub("\\.tif$", ".tfw", tif_files, ignore.case = TRUE)
      ))
      all_files <- unique(c(all_files, sidecars[fs::file_exists(sidecars)]))
      for (i in new_reps) {
        rep_path <- fs::dir_create(fs::path(self$path, sprintf("rep%02d", i)))
        ## overwrite = TRUE so a re-run picks up any input-file changes since
        ## the previous replicate() call (e.g. a regenerated dynamic-fire.txt
        ## after a config patch). LANDIS-II's output files (Landis-log.txt,
        ## log_*.csv, log/, fire/, etc.) live alongside but aren't in
        ## `all_files`, so they're untouched. Mirrors the same fix applied to
        ## the free function `landis_replicate()` in R/landis.R.
        fs::file_copy(all_files, rep_path, overwrite = TRUE)
      }
      private$.reps <- self$reps + n
    }
  ),
  active = list(
    #' @field reps integer number of replicates created for this scenario
    reps = function() {
      private$.reps
    },

    #' @field output_files Character vector of output files always produced by
    #'   LANDIS-II in the scenario directory, regardless of which extensions are
    #'   active. Collected by [scenario()] when writing `output_manifest.txt`.
    output_files = function(value) {
      if (!missing(value)) {
        stop("`output_files` is read-only", call. = FALSE)
      }
      c("Landis-log.txt", "Metadata/LANDIS-II v8.0/LANDIS-II v8.0.xml")
    }
  ),
  private = list(.reps = 0L)
)
