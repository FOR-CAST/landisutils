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
#' #   SpeciesAgeStats = list(species = c("querrub", "pinustro"), stats = c("MAX")),
#' #   SiteAgeStats = list(stats = c("MAX", "MED", "SD", "RICH", "EVEN")),
#' #   SiteSpeciesStats = list(stats = c("RICH"))
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
    #' @param SpeciesAgeStats Named list specifying the `species` and `stats`
    #'        (e.g., `list(species = c("querrub", "pinustro"), stats = c("MAX", "MIN"))`)
    #' @param SpeciesAgeMapNames Character. File pattern for writing outputs to disk.
    #' @param SiteAgeStats Named list specifying the `stats`
    #'        (e.g., `list(stats = c("MAX", "RICH"))`)
    #' @param SiteAgeMapNames Character. File pattern for writing outputs to disk.
    #' @param SiteSpeciesStats Named list specifying the `stats`
    #'        (e.g., `list(stats = c("RICH"))`).
    #' @param SiteSpeciesMapNames Character. File pattern for writing outputs to disk.
    initialize = function(
      path = NULL,
      Timestep = 10L,
      SpeciesAgeMapNames = NULL,
      SpeciesAgeStats = list(species = NULL, stats = NULL),
      SiteAgeMapNames = NULL,
      SiteAgeStats = list(stats = NULL),
      SiteSpeciesMapNames = NULL,
      SiteSpeciesStats = list(stats = NULL)
    ) {
      stopifnot(!is.null(path))

      ## LandisExtension fields
      private$.LandisData <- "Output Cohort Statistics"
      self$Timestep <- Timestep

      self$type <- "other"
      self$path <- path
      self$files <- "output-cohort-stats.txt" ## file won't exist yet

      ## additional fields for this extension
      if (is.null(SpeciesAgeStats$stats)) {
        SpeciesAgeStats$stats <- private$.species_stats
      }
      self$SpeciesAgeStats <- modifyList(self$SpeciesAgeStats, SpeciesAgeStats)
      self$SpeciesAgeMapNames <- SpeciesAgeMapNames %||%
        "outputs/cohort-stats/{{species}}-{{statistic}}-{{timestep}}.tif"

      if (is.null(SiteAgeStats$stats)) {
        SiteAgeStats$stats <- private$.site_stats
      }
      self$SiteAgeStats <- modifyList(self$SiteAgeStats, SiteAgeStats)
      self$SiteAgeMapNames <- SiteAgeMapNames %||%
        "outputs/cohort-stats/age-{statistic}-{timestep}.tif"

      if (is.null(SiteSpeciesStats$stats)) {
        SiteSpeciesStats$stats <- private$.site_species_stats
      }
      self$SiteSpeciesStats <- modifyList(self$SiteSpeciesStats, SiteSpeciesStats)
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
          lapply(self$SpeciesAgeStats$stats, function(x) {
            glue::glue("{x}    {paste(self$SpeciesAgeStats$species, collapse = '  ')}")
          }) |>
            unlist(),

          glue::glue("SiteAgeStats"),
          insertFile("MapNames", self$SiteAgeMapNames),
          glue::glue_collapse(self$SiteAgeStats$stats, sep = "\n"),

          glue::glue("SiteSpeciesStats"),
          insertFile("MapNames", self$SiteSpeciesMapNames),
          glue::glue_collapse(self$SiteSpeciesStats$stats, sep = "\n")
        ),
        file.path(self$path, self$files[1])
      )

      return(invisible(self))
    }
  ),

  private = list(
    .SpeciesAgeMapNames = NULL,
    .SpeciesAgeStats = list(species = NULL, stats = NULL),
    .SiteAgeMapNames = NULL,
    .SiteAgeStats = list(stats = NULL),
    .SiteSpeciesMapNames = NULL,
    .SiteSpeciesStats = list(stats = NULL),

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
        stopifnot(
          is.list(value),
          all(c("species", "stats") %in% names(value)),
          all(toupper(value$stats) %in% private$.species_stats)
        )

        private$.SpeciesAgeStats$species <- value$species
        private$.SpeciesAgeStats$stats <- toupper(value$stats)
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
        stopifnot(
          is.list(value),
          all(c("stats") %in% names(value)),
          all(toupper(value$stats) %in% private$.site_stats)
        )

        private$.SiteAgeStats$stats <- toupper(value$stats)
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
        stopifnot(
          is.list(value),
          all(c("stats") %in% names(value)),
          all(toupper(value$stats) %in% private$.site_species_stats)
        )

        private$.SiteSpeciesStats$stats <- toupper(value$stats)
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
