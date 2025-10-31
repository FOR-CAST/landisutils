#' Max Species Age Extension
#'
#' @include ext_utils.R
#'
#' @references LANDIS-II Output Max Species Age v4 Extension User Guide
#'   <https://github.com/LANDIS-II-Foundation/Extension-Output-Max-Species-Age/blob/master/docs/LANDIS-II%20Output%20Max%20Species%20Age%20v4%20User%20Guide.pdf>
#'
#' @export
#'
#' @examples
#' max_species_age <- OutputMaxSpeciesAge$new(
#'   path = tempdir(),
#'   Timestep = 10,
#'   MapNames = NULL, # use default
#'   Species = c("pinubank", "acersacc", "tiliamer")
#' )
#'
#' max_species_age$write()
#'
OutputMaxSpeciesAge <- R6Class(
  "OutputMaxSpeciesAge",
  inherit = LandisExtension,
  public = list(
    #' @param path Character. Directory path.
    #' @param Timestep Integer.
    #' @param MapNames Character. File pattern for writing outputs to disk.
    #' @param Species Character vector of species names, or "all".
    initialize = function(path, Timestep = 10, MapNames = NULL, Species = NULL) {
      stopifnot(!is.null(path))

      ## LandisExtension fields
      private$.LandisData <- "Output Max Species Age"
      self$Timestep <- Timestep

      self$type <- "disturbance"
      self$path <- path
      self$files <- "output-max-species-age.txt" ## file won't exist yet

      ## additional fields for this extension
      self$MapNames <- MapNames %||% "outputs/max-spp-age/{species}-{timestep}.tif"
      self$Species <- Species %||% "all"
    },

    #' @description Write extension inputs to disk
    write = function() {
      stopifnot(!is.null(self$Timestep))

      self$Species <- self$Species %||% "all"

      writeLines(
        c(
          insertLandisData(private$.LandisData),
          insertValue("Timestep", self$Timestep),
          insertFile("MapNames", self$MapNames),
          glue::glue("Species {paste(self$Species, sep = '\n         ')}")
        ),
        file.path(self$path, self$files[1])
      )
    }
  ),

  private = list(.MapNames = NULL, .Species = NULL),

  active = list(
    #' @field MapNames Character. File pattern for writing outputs to disk.
    MapNames = function(value) {
      if (missing(value)) {
        private$.MapNames
      } else {
        stopifnot(grepl("{species}", value, fixed = TRUE), grepl("{timestep}", value, fixed = TRUE))
        private$.MapNames <- value
      }
    },

    #' @field Species Character vector of species names, or "all".
    Species = function(value) {
      if (missing(value)) {
        private$.Species
      } else {
        private$.Species <- value
      }
    }
  )
)
