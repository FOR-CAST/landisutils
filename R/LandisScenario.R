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
        if (ext$type %in% type) ext$name else NULL
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
      for (i in new_reps) {
        rep_path <- sprintf("%s_rep%02d", self$path, i) |> fs::dir_create()
        fs::file_copy(file.path(self$path, self$files), rep_path)
      }
      private$.reps <- self$reps + n
    }
  ),
  active = list(
    #' @field reps integer number of replicates created for this scenario
    reps = function() {
      private$.reps
    }
  ),
  private = list(.reps = 0L)
)
