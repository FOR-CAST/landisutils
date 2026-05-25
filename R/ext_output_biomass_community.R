#' Biomass Community Output Extension
#'
#' @include ext_utils.R
#'
#' @references LANDIS-II Output Biomass Community v3 Extension User Guide
#'   <https://github.com/LANDIS-II-Foundation/Extension-Output-Biomass-Community/blob/master/docs/LANDIS-II%20Output%20Biomass%20Community%20v3%20User%20Guide.pdf>
#'
#' @family Biomass Community Output helpers
#'
#' @export
#'
#' @examples
#' bio_community <- OutputBiomassCommunity$new(path = tempdir(), Timestep = 10)
#' bio_community$write()
#'
OutputBiomassCommunity <- R6Class(
  "OutputBiomassCommunity",
  inherit = LandisExtension,
  public = list(
    #' @param path Character. Directory path.
    #' @param Timestep Integer. Years between community snapshots.
    initialize = function(path, Timestep = 10L) {
      stopifnot(!is.null(path))

      ## LandisExtension fields
      private$.LandisData <- "Output Biomass Community"
      self$Timestep <- Timestep

      self$type <- "other"
      self$path <- path
      self$files <- "output-biomass-community.txt" ## file won't exist yet
    },

    #' @description Write extension inputs to disk
    write = function() {
      stopifnot(!is.null(self$Timestep))

      writeLines(
        c(insertLandisData(private$.LandisData), insertValue("Timestep", self$Timestep)),
        file.path(self$path, self$files[1])
      )

      return(invisible(self))
    }
  )
)
