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

    #' @param LandisData Character. The extension's `LandisData` entry
    #' (e.g. `"Biomass Succession"`).
    #'
    #' @param type Character specifying the extension type
    #' (must be one of: `r paste(.extTypes, collapse = ", ")`)
    #'
    #' @param path Character specifying the (directory) path to the extension input files.
    #'
    #' @param Timestep Integer.
    initialize = function(
      LandisData = NULL,
      type = NA_character_,
      path = NA_character_,
      Timestep = 1L
    ) {
      stopifnot(!is.null(LandisData), !is.null(path))

      private$.LandisData <- LandisData
      self$type <- type
      self$path <- path
    },

    #' @param value if specified, the new value to append to `files`
    add_file = function(value) {
      stopifnot(file.exists(file.path(self$path, value)))
      self$files <- c(self$files, fs::path_tidy(value)) |> unique()
    }
  ),

  private = list(
    .LandisData = NA_character_,
    ## Optional override for the extension name written in scenario.txt's
    ## extension list. Most extensions use the same string in both their own
    ## file's `LandisData` header and the scenario reference, but a few
    ## diverge (e.g. Land Use Plus: file header is `"Land Use"`, scenario
    ## reference is `"Land Use Change"`). When NULL, `scenarioName` falls
    ## back to `.LandisData`.
    .scenarioName = NULL,
    .Timestep = NA_integer_,
    .type = NA_character_
  ),

  active = list(
    #' @field LandisData Character. The extension's `LandisData` entry
    #' (e.g. `"Biomass Succession"`). Read-only; set by each subclass's
    #' `initialize()` via `private$.LandisData`.
    LandisData = function(value) {
      if (!missing(value)) {
        stop(
          "`LandisData` is read-only; set via `private$.LandisData` in initialize()",
          call. = FALSE
        )
      }
      private$.LandisData
    },

    #' @field scenarioName Character. The name to use when this extension is
    #'   referenced in the scenario `.txt` file's extension list. Defaults to
    #'   `LandisData`; subclasses set `private$.scenarioName` only when the
    #'   parser-registered name differs from the file-header name (e.g. Land
    #'   Use Plus). Read-only.
    scenarioName = function(value) {
      if (!missing(value)) {
        stop(
          "`scenarioName` is read-only; set via `private$.scenarioName` in initialize()",
          call. = FALSE
        )
      }
      private$.scenarioName %||% private$.LandisData
    },

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
