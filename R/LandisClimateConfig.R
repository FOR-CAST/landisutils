#' `LandisClimateConfig` class
#'
#' Keeps track of the input files created for the climate library
#'
#' @export
LandisClimateConfig <- R6Class(
  "LandisClimateConfig",

  public = list(
    #' @field path character specifying the (directory) path to the extension input files
    path = NULL,

    #' @field files character specifying the filenames of the extension input files
    #' (relative to `path`); the principle extension input file should be listed first.
    files = character(0),

    #' @param path character specifying the (directory) path to the extension input files
    initialize = function(path = NA_character_) {
      self$path <- path
    },

    #' @param value if specified, the new value to append to `files`
    add_file = function(value) {
      stopifnot(file.exists(file.path(self$path, value)))
      self$files <- c(self$files, fs::path_tidy(value)) |> unique()
    }
  )
)
