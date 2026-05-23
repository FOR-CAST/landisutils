#' Original Wind Extension
#'
#' @include ext_utils.R
#'
#' @references LANDIS-II Original Wind v4.1 Extension User Guide
#' <https://github.com/LANDIS-II-Foundation/Extension-Base-Wind/blob/master/docs/LANDIS-II%20Original%20Wind%20v4%20User%20Guide.pdf>
#'
#' @seealso
#' Helpers that prepare inputs for this extension:
#' [prepWindEventParametersTable()].
#'
#' @family Original Wind helpers
#'
#' @export
OriginalWind <- R6Class(
  "OriginalWind",
  inherit = LandisExtension,
  public = list(
    #' @param path Character. Directory path.
    #' @param Timestep Integer.
    #' @param WindEventParametersTable `data.frame` with columns `Ecoregion`,
    #'   `MaxSize`, `MeanSize`, `MinSize`, `WindRotationPeriod`
    #'   (see [prepWindEventParametersTable()]).
    #' @param WindSeverities `data.frame` with columns `Severity`, `LowerAge`,
    #'   `UpperAge`, `MortalityProbability`; rows must be ordered by decreasing
    #'   `Severity`. Defaults to [defaultWindSeverities()].
    #' @param MapNames Character. File pattern for writing outputs to disk;
    #'   must contain `{timestep}`.
    #' @param SummaryLogFile Character. Relative file path.
    #' @param EventLogFile Character. Relative file path.
    initialize = function(
      path,
      Timestep = NULL,
      WindEventParametersTable = NULL,
      WindSeverities = NULL,
      MapNames = NULL,
      SummaryLogFile = "wind/summary-log.csv",
      EventLogFile = "wind/event-log.csv"
    ) {
      stopifnot(!is.null(path))

      ## LandisExtension fields
      private$.LandisData <- "Original Wind"
      self$Timestep <- Timestep

      self$type <- "disturbance"
      self$path <- path
      self$files <- "original-wind.txt" ## file won't exist yet

      ## additional fields for this extension
      self$WindEventParametersTable <- WindEventParametersTable
      self$WindSeverities <- WindSeverities %||% defaultWindSeverities()
      self$MapNames <- MapNames %||% MapNames("severity", "wind", self$path)
      self$SummaryLogFile <- SummaryLogFile
      self$EventLogFile <- EventLogFile
    },

    #' @description Write extension inputs to disk
    write = function() {
      stopifnot(
        !is.null(self$WindEventParametersTable),
        !is.null(self$WindSeverities),
        !is.null(self$MapNames),
        !is.null(self$SummaryLogFile),
        !is.null(self$EventLogFile)
      )

      writeLines(
        c(
          insertLandisData(private$.LandisData),
          insertValue("Timestep", self$Timestep),
          insertWindEventParametersTable(self$WindEventParametersTable),
          insertWindSeverities(self$WindSeverities),
          insertFile("MapNames", self$MapNames),
          insertFile("SummaryLogFile", self$SummaryLogFile),
          insertFile("EventLogFile", self$EventLogFile)
        ),
        file.path(self$path, self$files[1])
      )

      return(invisible(self))
    }
  ),

  private = list(
    .WindEventParametersTable = NULL,
    .WindSeverities = NULL,
    .MapNames = NULL,
    .SummaryLogFile = NULL,
    .EventLogFile = NULL
  ),

  active = list(
    #' @field WindEventParametersTable `data.frame` with columns `Ecoregion`,
    #'   `MaxSize`, `MeanSize`, `MinSize`, `WindRotationPeriod`.
    WindEventParametersTable = function(value) {
      if (missing(value)) {
        return(private$.WindEventParametersTable)
      } else {
        if (!is.null(value)) {
          stopifnot(
            is.data.frame(value),
            all(
              c("Ecoregion", "MaxSize", "MeanSize", "MinSize", "WindRotationPeriod") %in%
                colnames(value)
            ),
            all(value$MinSize >= 0),
            all(value$MaxSize >= value$MinSize),
            all(value$MeanSize >= value$MinSize),
            all(value$MeanSize <= value$MaxSize),
            all(as.integer(value$WindRotationPeriod) >= 0L)
          )
        }
        private$.WindEventParametersTable <- value
      }
    },

    #' @field WindSeverities `data.frame` with columns `Severity`, `LowerAge`,
    #'   `UpperAge`, `MortalityProbability`.
    WindSeverities = function(value) {
      if (missing(value)) {
        return(private$.WindSeverities)
      } else {
        if (!is.null(value)) {
          stopifnot(
            is.data.frame(value),
            all(c("Severity", "LowerAge", "UpperAge", "MortalityProbability") %in% colnames(value)),
            all(as.integer(value$Severity) >= 0L),
            all(value$LowerAge >= 0, value$LowerAge <= 100),
            all(value$UpperAge >= 0, value$UpperAge <= 100),
            all(value$UpperAge >= value$LowerAge),
            all(value$MortalityProbability >= 0, value$MortalityProbability <= 1),
            !is.unsorted(rev(value$Severity)) ## decreasing order
          )
        }
        private$.WindSeverities <- value
      }
    },

    #' @field MapNames Character. File pattern for writing outputs to disk;
    #'   must contain `{timestep}`.
    MapNames = function(value) {
      if (missing(value)) {
        return(private$.MapNames)
      } else {
        if (!is.null(value)) {
          stopifnot(grepl("{timestep}", value, fixed = TRUE))
        }
        private$.MapNames <- value
      }
    },

    #' @field SummaryLogFile Character. Relative file path.
    SummaryLogFile = function(value) {
      if (missing(value)) {
        return(private$.SummaryLogFile)
      } else {
        private$.SummaryLogFile <- .relPath(value, self$path)
      }
    },

    #' @field EventLogFile Character. Relative file path.
    EventLogFile = function(value) {
      if (missing(value)) {
        return(private$.EventLogFile)
      } else {
        private$.EventLogFile <- .relPath(value, self$path)
      }
    }
  )
)

#' Default `WindSeverities` table for the Original Wind extension
#'
#' Matches the example in the LANDIS-II Original Wind v4.1 User Guide.
#'
#' @returns `data.frame` with 4 columns:
#'   `Severity`, `LowerAge`, `UpperAge`, `MortalityProbability`.
#'
#' @family Original Wind helpers
#'
#' @export
defaultWindSeverities <- function() {
  data.frame(
    Severity = c(5L, 4L, 3L, 2L, 1L),
    LowerAge = c(0L, 20L, 50L, 70L, 85L),
    UpperAge = c(20L, 50L, 70L, 85L, 100L),
    MortalityProbability = c(0.05, 0.10, 0.50, 0.85, 0.95)
  )
}

#' Prepare Original Wind `WindEventParametersTable`
#'
#' @param ecoregion_params `data.frame` with columns `Ecoregion`, `MaxSize`,
#'   `MeanSize`, `MinSize`, `WindRotationPeriod`. Any additional columns are
#'   dropped.
#'
#' @returns `data.frame` suitable for passing as `WindEventParametersTable`.
#'
#' @family Original Wind helpers
#'
#' @export
prepWindEventParametersTable <- function(ecoregion_params) {
  stopifnot(
    is.data.frame(ecoregion_params),
    all(
      c("Ecoregion", "MaxSize", "MeanSize", "MinSize", "WindRotationPeriod") %in%
        colnames(ecoregion_params)
    )
  )

  ecoregion_params[, c("Ecoregion", "MaxSize", "MeanSize", "MinSize", "WindRotationPeriod")]
}

#' Specify the `WindEventParameters` table for the Original Wind extension
#'
#' @param df `data.frame` with columns `Ecoregion`, `MaxSize`, `MeanSize`,
#'   `MinSize`, `WindRotationPeriod`.
#'
#' @template return_insert
#'
#' @family Original Wind helpers
#'
#' @keywords internal
insertWindEventParametersTable <- function(df) {
  stopifnot(
    is.data.frame(df),
    all(c("Ecoregion", "MaxSize", "MeanSize", "MinSize", "WindRotationPeriod") %in% colnames(df))
  )

  cols <- c("Ecoregion", "MaxSize", "MeanSize", "MinSize", "WindRotationPeriod")

  c(
    glue::glue(">> Wind Event Parameters"),
    glue::glue(">>                                  Wind"),
    glue::glue(">>                 Max   Mean  Min  Rotation"),
    glue::glue(">> Ecoregion       Size  Size  Size Period"),
    glue::glue(">> ------------------------------------------"),
    apply(df[, cols, drop = FALSE], 1, function(x) {
      glue::glue_collapse(x, sep = "    ")
    }),
    glue::glue("") ## add blank line after each item group
  )
}

#' Specify the `WindSeverities` table for the Original Wind extension
#'
#' @param df `data.frame` with columns `Severity`, `LowerAge`, `UpperAge`,
#'   `MortalityProbability`. Rows must be ordered by decreasing `Severity`.
#'
#' @template return_insert
#'
#' @family Original Wind helpers
#'
#' @keywords internal
insertWindSeverities <- function(df) {
  stopifnot(
    is.data.frame(df),
    all(c("Severity", "LowerAge", "UpperAge", "MortalityProbability") %in% colnames(df))
  )

  rows <- apply(df, 1, function(x) {
    glue::glue(
      "    {x[['Severity']]}        {x[['LowerAge']]}% to {x[['UpperAge']]}%        ",
      "{x[['MortalityProbability']]}"
    )
  })

  c(
    glue::glue("WindSeverities"),
    glue::glue(""),
    glue::glue(">>              Cohort Age        Mortality"),
    glue::glue(">> Severity     % of longevity    Probability"),
    glue::glue(">> --------     --------------    -----------"),
    rows,
    glue::glue("") ## add blank line after each item group
  )
}
