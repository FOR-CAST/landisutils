#' Cohort Statistics Extension
#'
#' @references LANDIS-II Cohort Statistics v4 Extension User Guide
#' <https://github.com/LANDIS-II-Foundation/Extension-Output-Cohort-Statistics/blob/master/docs/LANDIS-II%20Cohort%20Statistics%20v4%20User%20Guide.pdf>
#'
#' @export
#'
#'
#' @examples
#' # cohort_statistics <- OutputCohortStats$new(
#' #   path = tempdir(),
#' #   Timestep = 10,
#' #   SpeciesAgeStats = c(
#' #     species = c(),
#' #     stats = c()
#' #   ),
#' #   SiteAgeStats = c(
#' #     stats = c()
#' #   ),
#' #   SiteSpeciesStats = c(
#' #     stats = c()
#' #   )
#' # )
#' #
#' # cohort_satistics$write()
#' #
OutputCohortStats <- R6Class(
  "OutputCohortStats",
  inherit = LandisExtension,
  public = list(
    #' @param path Character. Directory path.
    #' @param Timestep Integer.
    #' @param SpeciesAgeStats Named character vector specifying the `species` and `stats`.
    #' @param SpeciesAgeMapNames Character. File pattern for writing outputs to disk.
    #' @param SiteAgeStats Named character vector specifying the `stats`.
    #' @param SiteAgeMapNames Character. File pattern for writing outputs to disk.
    #' @param SiteSpeciesStats Named character vector specifying the `stats`.
    #' @param SiteSpeciesMapNames Character. File pattern for writing outputs to disk.
    initialize = function(
      path = NULL,
      Timestep = 10L,
      SpeciesAgeMapNames = NULL,
      SpeciesAgeStats = NULL,
      SiteAgeMapNames = NULL,
      SiteAgeStats = NULL,
      SiteSpeciesMapNames = NULL,
      SiteSpeciesStats = NULL
    ) {
      stopifnot(!is.null(path))

      ## LandisExtension fields
      private$.LandisData <- "Output Cohort Statistics"
      self$Timestep <- Timestep

      self$type <- "other"
      self$path <- path
      self$files <- "output-cohort-stats.txt" ## file won't exist yet

      ## additional fields for this extension
      self$SpeciesAgeStats <- SpeciesAgeStats %||% private$.species_stats
      self$SpeciesAgeMapNames <- SpeciesAgeMapNames %||%
        "outputs/cohort-stats/{{species}}-{{statistic}}-{{timestep}}.tif"

      self$SiteAgeStats <- SiteAgeStats %||% private$.site_stats
      self$SiteAgeMapNames <- SiteAgeMapNames %||%
        "outputs/cohort-stats/age-{statistic}-{timestep}.tif"

      self$SiteSpeciesStats <- SiteSpeciesStats %||% private$.site_species_stats
      self$SiteSpeciesMapNames <- SiteSpeciesMapNames %||%
        "outputs/cohort-stats/spp-{{statistic}}-{{timestep}}.tif"
    },

    #' @description Write extension inputs to disk
    write = function() {
      writeLines(
        c(
          insertLandisData(private$.LandisData),
          insertValue("Timestep", self$Timestep),

          glue::glue("SpeciesAgeStats"),
          insertFile("MapNames", self$SpeciesAgeMapNames),
          lapply(self$SpeciesAgeStats, function(stat) {
            glue::glue("{stat}    {paste(species, collapse = '  ')}")
          }),

          glue::glue("SiteAgeStats"),
          insertFile("MapNames", self$SiteAgeMapNames),
          lapply(self$SiteAgeStats, function(stat) {
            glue::glue("{stat}")
          }),

          glue::glue("SiteSpeciesStats"),
          insertFile("MapNames", self$SiteSpeciesStats),
          lapply(self$SiteSpeciesStats, function(stat) {
            glue::glue("{stat}")
          })
        ),
        file.path(self$path, self$files[1])
      )

      return(invisible(self))
    }
  ),

  private = list(
    .SpeciesAgeMapNames = NULL,
    .SpeciesAgeStats = NULL,
    .SiteAgeMapNames = NULL,
    .SiteAgeStats = NULL,
    .SiteSpeciesMapNames = NULL,
    .SiteSpeciesStats = NULL,

    .species_stats = c("MAX", "MIN", "AVG", "MED", "SD"),
    .site_stats = c("MAX", "MIN", "AVG", "MED", "SD", "RICH", "EVEN", "COUNT"),
    .site_species_stats = c("RICH")
  ),

  active = list(
    #' @field SpeciesAgeStats Named character vector specifying the `stats` and `species`.
    SpeciesAgeStats = function(value) {
      if (missing(value)) {
        private$.SpeciesAgeStats
      } else {
        stopifnot(all(toupper(value) %in% private$.species_stats))

        private$.SpeciesAgeStats <- toupper(value)
      }
    },

    #' @field SpeciesAgeMapNames Character. File pattern for writing outputs to disk.
    SpeciesAgeMapNames = function(value) {
      if (missing(value)) {
        private$.SpeciesAgeMapNames
      } else {
        stopifnot(
          grepl("{species}", value, fixed = TRUE),
          grepl("{statistic}", value, fixed = TRUE),
          grepl("{timestep}", value, fixed = TRUE)
        )
        private$.SpeciesAgeMapNames <- value
      }
    },

    #' @field SiteAgeStats Named character vector specifying the `stats`.
    SiteAgeStats = function(value) {
      if (missing(value)) {
        private$.SiteAgeStats
      } else {
        stopifnot(all(toupper(value) %in% private$.site_stats))

        private$.SiteAgeStats <- toupper(value)
      }
    },

    #' @field SiteAgeMapNames Character. File pattern for writing outputs to disk.
    SiteAgeMapNames = function(value) {
      if (missing(value)) {
        private$.SiteAgeMapNames
      } else {
        stopifnot(
          grepl("{statistic}", value, fixed = TRUE),
          grepl("{timestep}", value, fixed = TRUE)
        )
        private$.SiteAgeMapNames <- value
      }
    },

    #' @field SiteSpeciesStats Named character vector specifying the `stats`.
    SiteSpeciesStats = function(value) {
      if (missing(value)) {
        private$.SiteSpeciesStats
      } else {
        stopifnot(all(toupper(value) %in% private$.site_species_stats))

        private$.SiteSpeciesStats <- toupper(value)
      }
    },

    #' @field SiteSpeciesMapNames Character. File pattern for writing outputs to disk.
    SiteSpeciesMapNames = function(value) {
      if (missing(value)) {
        private$.SiteSpeciesMapNames
      } else {
        stopifnot(
          grepl("{statistic}", value, fixed = TRUE),
          grepl("{timestep}", value, fixed = TRUE)
        )
        private$.SiteSpeciesMapNames <- value
      }
    }
  )
)
