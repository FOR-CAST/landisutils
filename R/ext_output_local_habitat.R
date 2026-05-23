#' Local Habitat Suitability Output Extension
#'
#' @include ext_utils.R
#'
#' @references LANDIS-II Local Habitat Suitability Output v3 Extension User Guide
#'   <https://github.com/LANDIS-II-Foundation/Extension-Local-Habitat-Suitability-Output/blob/master/docs/LANDIS-II%20Local%20Habitat%20Suitability%20Output%20v3%20User%20Guide.pdf>
#'
#' @family Local Habitat Output helpers
#'
#' @export
OutputLocalHabitat <- R6Class(
  "OutputLocalHabitat",
  inherit = LandisExtension,
  public = list(
    #' @param path Character. Directory path.
    #' @param Timestep Integer. Inner simulation interval (years).
    #' @param OutputTimestep Integer. Output frequency (years); must be `>= Timestep`.
    #' @param MapFileNames Character. Output filename pattern; must contain
    #'   the literals `{HabitatName}` and `{timestep}`.
    #' @param SuitabilityFiles List of `SuitabilityFile` sub-objects (see
    #'   [suitabilityFile()]) or a character vector of filenames (auto-wrapped).
    initialize = function(
      path,
      Timestep = 1L,
      OutputTimestep = NULL,
      MapFileNames = NULL,
      SuitabilityFiles = list()
    ) {
      stopifnot(!is.null(path))

      ## LandisExtension fields
      private$.LandisData <- "Local Habitat Output"
      self$Timestep <- Timestep

      self$type <- "other"
      self$path <- path
      self$files <- "output-local-habitat.txt" ## file won't exist yet

      ## additional fields for this extension
      self$OutputTimestep <- OutputTimestep %||% Timestep
      self$MapFileNames <- MapFileNames %||% "output/local-habitat/{HabitatName}-{timestep}.tif"
      self$SuitabilityFiles <- SuitabilityFiles
    },

    #' @description Write extension inputs to disk
    write = function() {
      stopifnot(
        !is.null(self$Timestep),
        !is.null(self$OutputTimestep),
        !is.null(self$MapFileNames),
        length(self$SuitabilityFiles) >= 1L
      )

      writeLines(
        c(
          insertLandisData(private$.LandisData),
          insertValue("Timestep", self$Timestep),
          insertValue("OutputTimestep", self$OutputTimestep),
          insertValue("MapFileNames", self$MapFileNames),
          insertSuitabilityFiles(self$SuitabilityFiles)
        ),
        file.path(self$path, self$files[1])
      )

      return(invisible(self))
    }
  ),

  private = list(.OutputTimestep = NULL, .MapFileNames = NULL, .SuitabilityFiles = list()),

  active = list(
    #' @field OutputTimestep Integer. Output frequency (years); must be `>= Timestep`.
    OutputTimestep = function(value) {
      if (missing(value)) {
        return(private$.OutputTimestep)
      } else {
        if (!is.null(value)) {
          stopifnot(as.integer(value) > 0L)
          if (!is.null(private$.Timestep)) {
            stopifnot(as.integer(value) >= private$.Timestep)
          }
        }
        private$.OutputTimestep <- as.integer(value)
      }
    },

    #' @field MapFileNames Character. Output filename pattern; must contain
    #'   `{HabitatName}` and `{timestep}`.
    MapFileNames = function(value) {
      if (missing(value)) {
        return(private$.MapFileNames)
      } else {
        if (!is.null(value)) {
          stopifnot(
            grepl("{HabitatName}", value, fixed = TRUE),
            grepl("{timestep}", value, fixed = TRUE)
          )
        }
        private$.MapFileNames <- value
      }
    },

    #' @field SuitabilityFiles List of `SuitabilityFile` objects, or a
    #'   character vector of relative filenames (auto-wrapped).
    SuitabilityFiles = function(value) {
      if (missing(value)) {
        return(private$.SuitabilityFiles)
      } else {
        if (is.null(value)) {
          private$.SuitabilityFiles <- list()
          return(invisible())
        }
        if (is.character(value)) {
          value <- lapply(value, function(p) suitabilityFile(path = p))
        } else if (inherits(value, "SuitabilityFile")) {
          value <- list(value)
        }
        stopifnot(is.list(value), all(vapply(value, inherits, logical(1), "SuitabilityFile")))
        private$.SuitabilityFiles <- value
      }
    }
  )
)

#' Construct a habitat-suitability file reference
#'
#' Wraps a path to a habitat-suitability definition file. Used by
#' [OutputLocalHabitat] and [OutputWildlifeHabitat].
#'
#' @param path Character. Relative path (from the extension directory) to
#'   the suitability definition file.
#'
#' @returns A list of class `"SuitabilityFile"`.
#'
#' @family Local Habitat Output helpers
#' @family Wildlife Habitat Output helpers
#'
#' @export
suitabilityFile <- function(path) {
  stopifnot(is.character(path), length(path) == 1L, nzchar(path))

  structure(list(path = path), class = "SuitabilityFile")
}

#' Specify the `SuitabilityFiles` block for habitat output extensions
#'
#' Emits the `SuitabilityFiles` keyword followed by one path per line.
#'
#' @param files List of `SuitabilityFile` objects.
#'
#' @template return_insert
#'
#' @family Local Habitat Output helpers
#' @family Wildlife Habitat Output helpers
#'
#' @keywords internal
insertSuitabilityFiles <- function(files) {
  stopifnot(
    is.list(files),
    length(files) >= 1L,
    all(vapply(files, inherits, logical(1), "SuitabilityFile"))
  )

  paths <- vapply(files, function(f) f$path, character(1))

  c(
    glue::glue("SuitabilityFiles    {paths[1]}"),
    if (length(paths) > 1L) {
      vapply(
        paths[-1],
        function(p) {
          as.character(glue::glue("                    {p}"))
        },
        character(1),
        USE.NAMES = FALSE
      )
    },
    glue::glue("")
  )
}
