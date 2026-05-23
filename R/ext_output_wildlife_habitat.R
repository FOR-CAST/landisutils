#' Wildlife Habitat Output Extension
#'
#' @include ext_utils.R
#'
#' @references LANDIS-II Output Wildlife Habitat v3 Extension User Guide
#'   <https://github.com/LANDIS-II-Foundation/Extension-Output-Wildlife-Habitat/blob/master/docs/LANDIS-II%20Wildlife%20Habitat%20Output%20v3%20User%20Guide.pdf>
#'
#' @seealso [suitabilityFile()] (also used by [OutputLocalHabitat]).
#'
#' @family Wildlife Habitat Output helpers
#'
#' @export
OutputWildlifeHabitat <- R6Class(
  "OutputWildlifeHabitat",
  inherit = LandisExtension,
  public = list(
    #' @param path Character. Directory path.
    #' @param Timestep Integer. Inner simulation interval (years).
    #' @param OutputTimestep Integer. Output frequency (years); must be `>= Timestep`.
    #' @param MapFileNames Character. Output filename pattern; must contain
    #'   the literals `{wildlifeName}` and `{timestep}`.
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
      private$.LandisData <- "Wildlife Habitat Output"
      self$Timestep <- Timestep

      self$type <- "other"
      self$path <- path
      self$files <- "output-wildlife-habitat.txt" ## file won't exist yet

      ## additional fields for this extension
      self$OutputTimestep <- OutputTimestep %||% Timestep
      self$MapFileNames <- MapFileNames %||% "output/wildlife-habitat/{wildlifeName}-{timestep}.tif"
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
    #'   `{wildlifeName}` and `{timestep}`.
    MapFileNames = function(value) {
      if (missing(value)) {
        return(private$.MapFileNames)
      } else {
        if (!is.null(value)) {
          stopifnot(
            grepl("{wildlifeName}", value, fixed = TRUE),
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
