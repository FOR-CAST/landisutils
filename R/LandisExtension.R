#' @keywords internal
.extTypes <- c("succession", "disturbance", "other") ## 'other' includes output extensions

#' `LandisExtension` class
#'
#' Keeps track of the input files created for an extension
#'
#' @export
LandisExtension <- R6Class(
  "LandisExtension",

  public = list(
    #' @field path Character specifying the (directory) path to the extension input files.
    path = NULL,

    #' @field files Character specifying the filenames of the extension input files
    #' (relative to `path`); the principle extension input file should be listed first.
    files = character(0),

    #' @param name Character. The extension name (i.e., it's `LandisData` entry).
    #'
    #' @param type Character specifying the extension type
    #' (must be one of: `r paste(.extTypes, collapse = ", ")`)
    #'
    #' @param path Character specifying the (directory) path to the extension input files.
    #'
    #' @param Timestep Integer.
    initialize = function(name = NULL, type = NA_character_, path = NA_character_, Timestep = 1L) {
      stopifnot(!is.null(name), !is.null(path))

      private$.LandisData <- name
      self$type <- type
      self$path <- path
    },

    #' @param value if specified, the new value to append to `files`
    add_file = function(value) {
      stopifnot(file.exists(file.path(self$path, value)))
      self$files <- c(self$files, fs::path_tidy(value)) |> unique()
    }
  ),

  private = list(.LandisData = NA_character_, .Timestep = NA_integer_, .type = NA_character_),

  active = list(
    #' @field Timestep Integer.
    Timestep = function(value) {
      if (missing(value)) {
        return(private$.Timestep)
      } else {
        stopifnot(as.integer(value) > 0L)
        private$.Timestep <- as.integer(value)
      }
    },

    #' @field type character specifying the extension type
    #' (must be one of: `r paste(.extTypes, collapse = ", ")`)
    type = function(value) {
      if (missing(value)) {
        return(private$.type)
      }
      stopifnot(tolower(value) %in% .extTypes)
      private$.type <- tolower(value)
    }
  )
)
