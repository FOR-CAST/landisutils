#' Biomass Output by Age Extension
#'
#' @references LANDIS-II Biomass-by-Age Output v4.0 Extension User Guide
#'   <https://github.com/LANDIS-II-Foundation/Extension-Output-Biomass-By-Age/blob/master/docs/LANDIS-II%20Biomass-by-Age%20Output%20v4.0%20User%20Guide.pdf>
#'
#' @export
#'
#' @examples
#' biomass_by_age <- OutputBiomassByAge(
#'   path = tempdir(),
#'   Timestep = 10,
#'   MapNames = NULL, # use default
#'   Species = c(
#'     "pinubank ageclass1(10-40) ageclass2(15-100)",
#'     "pinuresi ageclass(>200)",
#'     "pinustro ageclass(>250)",
#'     "poputrem ageclass1(<50)"
#'   )
#' )
#'
#' biomass_by_age$write()
#'
OutputBiomassByAge <- R6Class(
  "OutputBiomassByAge",
  inherit = LandisExtension,
  public = list(
    #' @param path Character. Directory path.
    #' @param Timestep Integer.
    #' @param MapNames Character. File pattern for writing outputs to disk.
    #' @param Species Character vector of species names with age classes.
    initialize = function(path = NULL, Timestep = 10L, MapNames = NULL, Species = NULL) {
      stopifnot(!is.null(path))

      ## LandisExtension fields
      private$.LandisData <- "Output Biomass-by-Age"
      self$Timestep <- Timestep

      self$type <- "other"
      self$path <- path
      self$files <- "output-biomass-by-age.txt" ## file won't exist yet

      ## additional fields for this extension
      self$MapNames <- MapNames %||% "outputs/biomass-age/{species}-{ageclass}-{timestep}.tif"
      self$Species <- Species
    },

    #' @description Write extension inputs to disk
    write = function() {
      stopifnot(!is.null(dots$Species), !is.null(dots$Timestep))

      ## NOTE: Species list needs to be provided such that we produce e.g.:
      ## Species  pinubank ageclass1(10-40) ageclass2(15-100)
      ##          pinuresi ageclass(>200)
      ##          pinustro ageclass(>250)
      ##          poputrem ageclass1(<50)

      writeLines(
        c(
          insertLandisData(private$.LandisData),
          insertValue("Timestep", self$Timestep),
          insertFile("MapNames", self$MapNames),
          glue::glue("Species {paste(dots$Species, sep = '\n         ')}")
        ),
        file.path(self$path, self$files[1])
      )

      return(invisible(self))
    }
  ),

  private = list(.MapNames = NULL, .Species = NULL),

  active = list(
    #' @field MapNames Character. File pattern for writing outputs to disk.
    MapNames = function(value) {
      if (missing(value)) {
        private$.MapNames
      } else {
        ## TODO: add validation
        private$.MapNames <- value
      }
    },

    #' @field Species Character vector of species names with age classes.
    Species = function(value) {
      if (missing(value)) {
        private$.Species
      } else {
        ## TODO: add validation
        private$.Species <- value
      }
    }
  )
)
