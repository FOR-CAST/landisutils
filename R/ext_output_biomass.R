#' Biomass Output Extension
#'
#' @references LANDIS-II Output Biomass v4.0 Extension User Guide
#' <https://github.com/LANDIS-II-Foundation/Extension-Output-Biomass/blob/master/docs/LANDIS-II%20Output%20Biomass%20v4.0%20User%20Guide.pdf>
#'
#' @export
#'
#' @examples
#' output_biomass <- OutputBiomass$new(
#'   path = tempdir(),
#'   Timestep = 10,
#'   MakeTable = "yes",
#'   Species = "all",
#'   LiveMapNames = NULL, # use default
#'   DeadPools = "both"
#'   DeadMapNames = NULL # use default
#' )
#'
#' output_biomass$write()
#'
OutputBiomass <- R6Class(
  "OutputBiomass",
  inherit = LandisExtension,
  public = list(
    #' @param path Character. Directory path.
    #' @param Timestep Integer.
    #' @param MakeTable Logical, or character indicating "yes" or "no".
    #' @param Species Character vector of species names, or "all".
    #' @param LiveMapNames Character. File pattern for writing outputs to disk.
    #' @param DeadPools Character. One of "woody", "non-woody", or "both".
    #' @param DeadMapNames Character. File pattern for writing outputs to disk.
    initialize = function(
      path = NULL,
      Timestep = 10L,
      MakeTable = NULL,
      Species = "all",
      LiveMapNames = NULL,
      DeadPools = "both",
      DeadMapNames = NULL
    ) {
      stopifnot(!is.null(path))

      ## LandisExtension fields
      private$.LandisData <- "Output Biomass"
      self$Timestep <- Timestep

      self$type <- "other"
      self$path <- path
      self$files <- "biomass-output.txt" ## file won't exist yet

      ## additional fields for this extension
      self$MakeTable <- MakeTable %||% "yes"
      self$Species <- Species %||% "all"
      self$LiveMapNames <- LiveMapNames %||% "outputs/biomass/biomass-{species}-{timestep}.tif"
      self$DeadPools <- DeadPools %||% "both"
      self$DeadMapNames <- DeadMapNames %||% "outputs/biomass/biomass-{pool}-{timestep}.tif"
    },

    #' @description Write extension inputs to disk
    write = function() {
      writeLines(
        c(
          insertLandisData(private$.LandisData),
          insertValue("Timestep", self$Timestep),
          insertValue("MakeTable", self$MakeTable),
          glue::glue("Species {paste(self$Species, sep = '\n         ')}"),
          insertFile("MapNames", self$LiveMapNames),
          insertValue("DeadPools", self$DeadPools),
          insertFile("MapNames", self$DeadMapNames)
        ),
        file.path(self$path, self$files[1])
      )
      return(invisible(self))
    }
  ),

  private = list(
    .MakeTable = NULL,
    .Species = NULL,
    .LiveMapNames = NULL,
    .DeadPools = NULL,
    .DeadMapNames = NULL
  ),

  active = list(
    #' @field MakeTable Logical, or character indicating "yes" or "no".
    MakeTable = function(value) {
      if (missing(value)) {
        private$.MakeTable
      } else {
        private$.MakeTable <- yesno(value)
      }
    },

    #' @field Species Character vector of species names, or "all".
    Species = function(value) {
      if (missing(value)) {
        private$.Species
      } else {
        private$.Species <- value
      }
    },

    #' @field LiveMapNames Character. File pattern for writing outputs to disk.
    LiveMapNames = function(value) {
      if (missing(value)) {
        private$.LiveMapNames
      } else {
        stopifnot(grepl("{species}", value, fixed = TRUE), grepl("{timestep}", value, fixed = TRUE))
        private$.LiveMapNames <- value
      }
    },

    #' @field DeadPools Character. One of "woody", "non-woody", or "both".
    DeadPools = function(value) {
      if (missing(value)) {
        private$.DeadPools
      } else {
        stopifnot(private$.DeadPools %in% c("both", "woody", "non-woody"))
        private$.DeadPools <- value
      }
    },

    #' @field DeadMapNames Character. File pattern for writing outputs to disk.
    DeadMapNames = function(value) {
      if (missing(value)) {
        private$.DeadMapNames
      } else {
        stopifnot(grepl("{pool}", value, fixed = TRUE), grepl("{timestep}", value, fixed = TRUE))
        private$.DeadMapNames <- value
      }
    }
  )
)
