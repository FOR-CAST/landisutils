#' Linear Wind Disturbance Extension
#'
#' @include ext_utils.R
#'
#' @references LANDIS-II Linear Wind Disturbance v3 Extension User Guide
#' <https://github.com/LANDIS-II-Foundation/Extension-LinearWind/blob/master/docs/LANDIS-II%20Linear%20Wind%20v3%20User%20Guide.pdf>
#'
#' @family Linear Wind helpers
#'
#' @export
LinearWind <- R6Class(
  "LinearWind",
  inherit = LandisExtension,
  public = list(
    #' @param path Character. Directory path.
    #' @param Timestep Integer. Years.
    #' @param NumEventsMean Numeric. Mean number of wind events per 40,000 km^2 per year.
    #' @param NumEventsStDev Numeric. Standard deviation of number of wind events.
    #' @param TornadoLengthLambda Numeric. Weibull lambda for tornado event length (km).
    #' @param TornadoLengthAlpha Numeric. Weibull alpha for tornado event length (km).
    #' @param TornadoWidth Numeric. Mean width of tornado events (km).
    #' @param TornadoIntensityTable Numeric vector of length 5 giving the percentage of
    #'   tornado events with maximum intensity in each class `0.2, 0.4, 0.6, 0.8, 1.0`;
    #'   must sum to 100.
    #' @param TornadoProp Numeric. Proportion of events that are tornadoes (0-1);
    #'   the remainder are derechos.
    #' @param DerechoLengthLambda Numeric. Weibull lambda for derecho event length (km).
    #' @param DerechoLengthAlpha Numeric. Weibull alpha for derecho event length (km).
    #' @param DerechoWidth Numeric. Mean width of derecho events (km).
    #' @param DerechoIntensityTable Numeric vector of length 5 giving the percentage of
    #'   derecho events with maximum intensity in each class `0.2, 0.4, 0.6, 0.8, 1.0`;
    #'   must sum to 100.
    #' @param PropIntensityVar Numeric. Variability in wind intensity within an event (0-1).
    #' @param WindDirectionTable Numeric vector of length 4 giving the percentage of events
    #'   with primary direction `N-S, NE-SW, E-W, SE-NW`; must sum to 100.
    #' @param EcoregionModifiers `data.frame` with columns `Ecoregion`, `Modifier`
    #'   (`Modifier` in `[-1, 1]`); optional.
    #' @param WindSeverities `data.frame` with columns `Severity`, `LowerAge`,
    #'   `UpperAge`, `WindspeedMortalityThreshold`; rows must be ordered by decreasing
    #'   `Severity`. Defaults to [defaultLinearWindSeverities()].
    #' @param IntensityMapNames Character. File pattern for wind intensity output maps;
    #'   must contain the literal `{timestep}` placeholder.
    #' @param SeverityMapNames Character. File pattern for wind severity output maps;
    #'   must contain the literal `{timestep}` placeholder.
    #' @param LogFile Character. Relative file path.
    initialize = function(
      path,
      Timestep = NULL,
      NumEventsMean = NULL,
      NumEventsStDev = NULL,
      TornadoLengthLambda = NULL,
      TornadoLengthAlpha = NULL,
      TornadoWidth = NULL,
      TornadoIntensityTable = NULL,
      TornadoProp = NULL,
      DerechoLengthLambda = NULL,
      DerechoLengthAlpha = NULL,
      DerechoWidth = NULL,
      DerechoIntensityTable = NULL,
      PropIntensityVar = NULL,
      WindDirectionTable = NULL,
      EcoregionModifiers = NULL,
      WindSeverities = NULL,
      IntensityMapNames = NULL,
      SeverityMapNames = NULL,
      LogFile = "linearwind/log.csv"
    ) {
      stopifnot(!is.null(path))

      ## LandisExtension fields
      private$.LandisData <- "Linear Wind"
      self$Timestep <- Timestep

      self$type <- "disturbance"
      self$path <- path
      self$files <- "linear-wind.txt" ## file won't exist yet

      ## additional fields for this extension
      self$NumEventsMean <- NumEventsMean
      self$NumEventsStDev <- NumEventsStDev
      self$TornadoLengthLambda <- TornadoLengthLambda
      self$TornadoLengthAlpha <- TornadoLengthAlpha
      self$TornadoWidth <- TornadoWidth
      self$TornadoIntensityTable <- TornadoIntensityTable
      self$TornadoProp <- TornadoProp
      self$DerechoLengthLambda <- DerechoLengthLambda
      self$DerechoLengthAlpha <- DerechoLengthAlpha
      self$DerechoWidth <- DerechoWidth
      self$DerechoIntensityTable <- DerechoIntensityTable
      self$PropIntensityVar <- PropIntensityVar
      self$WindDirectionTable <- WindDirectionTable
      self$EcoregionModifiers <- EcoregionModifiers
      self$WindSeverities <- WindSeverities %||% defaultLinearWindSeverities()
      self$IntensityMapNames <- IntensityMapNames %||%
        MapNames("intensity", "linearwind", self$path)
      self$SeverityMapNames <- SeverityMapNames %||% MapNames("severity", "linearwind", self$path)
      self$LogFile <- LogFile
    },

    #' @description Write extension inputs to disk
    write = function() {
      stopifnot(
        !is.null(self$NumEventsMean),
        !is.null(self$NumEventsStDev),
        !is.null(self$TornadoLengthLambda),
        !is.null(self$TornadoLengthAlpha),
        !is.null(self$TornadoWidth),
        !is.null(self$TornadoIntensityTable),
        !is.null(self$TornadoProp),
        !is.null(self$DerechoLengthLambda),
        !is.null(self$DerechoLengthAlpha),
        !is.null(self$DerechoWidth),
        !is.null(self$DerechoIntensityTable),
        !is.null(self$PropIntensityVar),
        !is.null(self$WindDirectionTable),
        !is.null(self$WindSeverities),
        !is.null(self$SeverityMapNames),
        !is.null(self$LogFile)
      )

      writeLines(
        c(
          insertLandisData(private$.LandisData),
          insertValue("Timestep", self$Timestep),
          insertValue("NumEventsMean", self$NumEventsMean),
          insertValue("NumEventsStDev", self$NumEventsStDev),
          glue::glue(">> Tornadoes"),
          insertValue("TornadoLengthLambda", self$TornadoLengthLambda, blank_line = FALSE),
          insertValue("TornadoLengthAlpha", self$TornadoLengthAlpha, blank_line = FALSE),
          insertValue("TornadoWidth", self$TornadoWidth, blank_line = FALSE),
          insertLinearWindIntensityTable("TornadoIntensityTable", self$TornadoIntensityTable),
          insertValue("TornadoProp", self$TornadoProp),
          glue::glue(">> Derechos"),
          insertValue("DerechoLengthLambda", self$DerechoLengthLambda, blank_line = FALSE),
          insertValue("DerechoLengthAlpha", self$DerechoLengthAlpha, blank_line = FALSE),
          insertValue("DerechoWidth", self$DerechoWidth, blank_line = FALSE),
          insertLinearWindIntensityTable("DerechoIntensityTable", self$DerechoIntensityTable),
          insertValue("PropIntensityVar", self$PropIntensityVar),
          insertWindDirectionTable(self$WindDirectionTable),
          insertLinearWindEcoregionModifiers(self$EcoregionModifiers),
          insertLinearWindSeverities(self$WindSeverities),
          insertFile("IntensityMapNames", self$IntensityMapNames),
          insertFile("SeverityMapNames", self$SeverityMapNames),
          insertFile("LogFile", self$LogFile)
        ),
        file.path(self$path, self$files[1])
      )

      return(invisible(self))
    }
  ),

  private = list(
    .NumEventsMean = NULL,
    .NumEventsStDev = NULL,
    .TornadoLengthLambda = NULL,
    .TornadoLengthAlpha = NULL,
    .TornadoWidth = NULL,
    .TornadoIntensityTable = NULL,
    .TornadoProp = NULL,
    .DerechoLengthLambda = NULL,
    .DerechoLengthAlpha = NULL,
    .DerechoWidth = NULL,
    .DerechoIntensityTable = NULL,
    .PropIntensityVar = NULL,
    .WindDirectionTable = NULL,
    .EcoregionModifiers = NULL,
    .WindSeverities = NULL,
    .IntensityMapNames = NULL,
    .SeverityMapNames = NULL,
    .LogFile = NULL
  ),

  active = list(
    #' @field NumEventsMean Numeric. Mean number of events per 40,000 km^2 per year.
    NumEventsMean = function(value) {
      if (missing(value)) {
        return(private$.NumEventsMean)
      } else {
        if (!is.null(value)) {
          stopifnot(is.numeric(value), length(value) == 1, value > 0)
        }
        private$.NumEventsMean <- value
      }
    },

    #' @field NumEventsStDev Numeric. Standard deviation of number of events.
    NumEventsStDev = function(value) {
      if (missing(value)) {
        return(private$.NumEventsStDev)
      } else {
        if (!is.null(value)) {
          stopifnot(is.numeric(value), length(value) == 1, value > 0)
        }
        private$.NumEventsStDev <- value
      }
    },

    #' @field TornadoLengthLambda Numeric. Weibull lambda for tornado length (km).
    TornadoLengthLambda = function(value) {
      if (missing(value)) {
        return(private$.TornadoLengthLambda)
      } else {
        if (!is.null(value)) {
          stopifnot(is.numeric(value), length(value) == 1, value > 0)
        }
        private$.TornadoLengthLambda <- value
      }
    },

    #' @field TornadoLengthAlpha Numeric. Weibull alpha for tornado length (km).
    TornadoLengthAlpha = function(value) {
      if (missing(value)) {
        return(private$.TornadoLengthAlpha)
      } else {
        if (!is.null(value)) {
          stopifnot(is.numeric(value), length(value) == 1, value > 0)
        }
        private$.TornadoLengthAlpha <- value
      }
    },

    #' @field TornadoWidth Numeric. Mean width of tornado events (km).
    TornadoWidth = function(value) {
      if (missing(value)) {
        return(private$.TornadoWidth)
      } else {
        if (!is.null(value)) {
          stopifnot(is.numeric(value), length(value) == 1, value > 0)
        }
        private$.TornadoWidth <- value
      }
    },

    #' @field TornadoIntensityTable Numeric vector of 5 percentages (sum 100) for intensity
    #'   classes `0.2, 0.4, 0.6, 0.8, 1.0`.
    TornadoIntensityTable = function(value) {
      if (missing(value)) {
        return(private$.TornadoIntensityTable)
      } else {
        if (!is.null(value)) {
          .checkIntensityTable(value)
        }
        private$.TornadoIntensityTable <- value
      }
    },

    #' @field TornadoProp Numeric. Proportion of events that are tornadoes (0-1).
    TornadoProp = function(value) {
      if (missing(value)) {
        return(private$.TornadoProp)
      } else {
        if (!is.null(value)) {
          stopifnot(is.numeric(value), length(value) == 1, value >= 0, value <= 1)
        }
        private$.TornadoProp <- value
      }
    },

    #' @field DerechoLengthLambda Numeric. Weibull lambda for derecho length (km).
    DerechoLengthLambda = function(value) {
      if (missing(value)) {
        return(private$.DerechoLengthLambda)
      } else {
        if (!is.null(value)) {
          stopifnot(is.numeric(value), length(value) == 1, value > 0)
        }
        private$.DerechoLengthLambda <- value
      }
    },

    #' @field DerechoLengthAlpha Numeric. Weibull alpha for derecho length (km).
    DerechoLengthAlpha = function(value) {
      if (missing(value)) {
        return(private$.DerechoLengthAlpha)
      } else {
        if (!is.null(value)) {
          stopifnot(is.numeric(value), length(value) == 1, value > 0)
        }
        private$.DerechoLengthAlpha <- value
      }
    },

    #' @field DerechoWidth Numeric. Mean width of derecho events (km).
    DerechoWidth = function(value) {
      if (missing(value)) {
        return(private$.DerechoWidth)
      } else {
        if (!is.null(value)) {
          stopifnot(is.numeric(value), length(value) == 1, value > 0)
        }
        private$.DerechoWidth <- value
      }
    },

    #' @field DerechoIntensityTable Numeric vector of 5 percentages (sum 100) for intensity
    #'   classes `0.2, 0.4, 0.6, 0.8, 1.0`.
    DerechoIntensityTable = function(value) {
      if (missing(value)) {
        return(private$.DerechoIntensityTable)
      } else {
        if (!is.null(value)) {
          .checkIntensityTable(value)
        }
        private$.DerechoIntensityTable <- value
      }
    },

    #' @field PropIntensityVar Numeric. Variability in wind intensity within an event (0-1).
    PropIntensityVar = function(value) {
      if (missing(value)) {
        return(private$.PropIntensityVar)
      } else {
        if (!is.null(value)) {
          stopifnot(is.numeric(value), length(value) == 1, value >= 0, value <= 1)
        }
        private$.PropIntensityVar <- value
      }
    },

    #' @field WindDirectionTable Numeric vector of 4 percentages (sum 100) for directions
    #'   `N-S, NE-SW, E-W, SE-NW`.
    WindDirectionTable = function(value) {
      if (missing(value)) {
        return(private$.WindDirectionTable)
      } else {
        if (!is.null(value)) {
          stopifnot(
            is.numeric(value),
            length(value) == 4,
            all(value >= 0, value <= 100),
            isTRUE(all.equal(sum(value), 100))
          )
        }
        private$.WindDirectionTable <- value
      }
    },

    #' @field EcoregionModifiers `data.frame` with columns `Ecoregion`, `Modifier`.
    EcoregionModifiers = function(value) {
      if (missing(value)) {
        return(private$.EcoregionModifiers)
      } else {
        if (!is.null(value)) {
          stopifnot(
            is.data.frame(value),
            all(c("Ecoregion", "Modifier") %in% colnames(value)),
            all(value$Modifier >= -1, value$Modifier <= 1)
          )
        }
        private$.EcoregionModifiers <- value
      }
    },

    #' @field WindSeverities `data.frame` with columns `Severity`, `LowerAge`,
    #'   `UpperAge`, `WindspeedMortalityThreshold`.
    WindSeverities = function(value) {
      if (missing(value)) {
        return(private$.WindSeverities)
      } else {
        if (!is.null(value)) {
          stopifnot(
            is.data.frame(value),
            all(
              c("Severity", "LowerAge", "UpperAge", "WindspeedMortalityThreshold") %in%
                colnames(value)
            ),
            all(as.integer(value$Severity) >= 0L),
            all(value$LowerAge >= 0, value$LowerAge <= 100),
            all(value$UpperAge >= 0, value$UpperAge <= 100),
            all(value$UpperAge >= value$LowerAge),
            all(value$WindspeedMortalityThreshold >= 0, value$WindspeedMortalityThreshold <= 1),
            !is.unsorted(rev(value$Severity)) ## decreasing order
          )
        }
        private$.WindSeverities <- value
      }
    },

    #' @field IntensityMapNames Character. File pattern for wind intensity output maps;
    #'   must contain the literal `{timestep}` placeholder.
    IntensityMapNames = function(value) {
      if (missing(value)) {
        return(private$.IntensityMapNames)
      } else {
        if (!is.null(value)) {
          stopifnot(grepl("{timestep}", value, fixed = TRUE))
        }
        private$.IntensityMapNames <- value
      }
    },

    #' @field SeverityMapNames Character. File pattern for wind severity output maps;
    #'   must contain the literal `{timestep}` placeholder.
    SeverityMapNames = function(value) {
      if (missing(value)) {
        return(private$.SeverityMapNames)
      } else {
        if (!is.null(value)) {
          stopifnot(grepl("{timestep}", value, fixed = TRUE))
        }
        private$.SeverityMapNames <- value
      }
    },

    #' @field LogFile Character. Relative file path.
    LogFile = function(value) {
      if (missing(value)) {
        return(private$.LogFile)
      } else {
        private$.LogFile <- .relPath(value, self$path)
      }
    }
  )
)

#' @keywords internal
.checkIntensityTable <- function(value) {
  stopifnot(
    is.numeric(value),
    length(value) == 5,
    all(value >= 0, value <= 100),
    isTRUE(all.equal(sum(value), 100))
  )
}

#' Default `WindSeverities` table for the Linear Wind extension
#'
#' Matches the example in the LANDIS-II Linear Wind Disturbance v3 User Guide.
#'
#' @returns `data.frame` with 4 columns:
#'   `Severity`, `LowerAge`, `UpperAge`, `WindspeedMortalityThreshold`.
#'
#' @family Linear Wind helpers
#'
#' @export
defaultLinearWindSeverities <- function() {
  data.frame(
    Severity = c(5L, 4L, 3L, 2L, 1L),
    LowerAge = c(0L, 20L, 40L, 60L, 80L),
    UpperAge = c(20L, 40L, 60L, 80L, 100L),
    WindspeedMortalityThreshold = c(0.80, 0.75, 0.70, 0.50, 0.40)
  )
}

#' Specify a Linear Wind intensity table (`TornadoIntensityTable` or `DerechoIntensityTable`)
#'
#' Emits 5 rows with intensity class labels `0.2, 0.4, 0.6, 0.8, 1.0` and the
#' supplied percentages (which must sum to 100).
#'
#' @param name Character. Either `"TornadoIntensityTable"` or `"DerechoIntensityTable"`.
#' @param pct Numeric vector of length 5.
#'
#' @template return_insert
#'
#' @family Linear Wind helpers
#'
#' @keywords internal
insertLinearWindIntensityTable <- function(name, pct) {
  .checkIntensityTable(pct)

  intensities <- c(0.2, 0.4, 0.6, 0.8, 1.0)

  rows <- vapply(
    seq_along(pct),
    function(i) {
      as.character(glue::glue(
        "    {format(pct[i], width = 5)}    >> {format(intensities[i], nsmall = 1)}"
      ))
    },
    character(1)
  )

  c(
    glue::glue("{name}"),
    glue::glue(">> Percent    Having this"),
    glue::glue(">> of events  Intensity"),
    glue::glue(">> -------    ---------"),
    rows,
    glue::glue("") ## add blank line after each item group
  )
}

#' Specify the `WindDirectionTable` for the Linear Wind extension
#'
#' @param pct Numeric vector of length 4 giving percentages for
#'   `N-S, NE-SW, E-W, SE-NW`; must sum to 100.
#'
#' @template return_insert
#'
#' @family Linear Wind helpers
#'
#' @keywords internal
insertWindDirectionTable <- function(pct) {
  stopifnot(is.numeric(pct), length(pct) == 4, isTRUE(all.equal(sum(pct), 100)))

  directions <- c("N-S", "NE-SW", "E-W", "SE-NW")

  rows <- vapply(
    seq_along(pct),
    function(i) {
      as.character(glue::glue("    {format(pct[i], width = 5)}    >> {directions[i]}"))
    },
    character(1)
  )

  c(
    glue::glue("WindDirectionTable"),
    glue::glue(">> Percent    Having this"),
    glue::glue(">> of events  Directionality"),
    glue::glue(">> -------    --------------"),
    rows,
    glue::glue("") ## add blank line after each item group
  )
}

#' Specify the `EcoregionModifiers` table for the Linear Wind extension
#'
#' Ecoregions not in the table are assigned a modifier of 0 (no modification).
#'
#' @param df `data.frame` with columns `Ecoregion`, `Modifier`. May be `NULL`.
#'
#' @template return_insert
#'
#' @family Linear Wind helpers
#'
#' @keywords internal
insertLinearWindEcoregionModifiers <- function(df) {
  header <- c(
    glue::glue("EcoregionModifiers"),
    glue::glue(">> Ecoregion    Modifier"),
    glue::glue(">> ---------    --------")
  )

  if (is.null(df) || nrow(df) == 0) {
    return(c(header, glue::glue("")))
  }

  stopifnot(all(c("Ecoregion", "Modifier") %in% colnames(df)))

  rows <- apply(df[, c("Ecoregion", "Modifier"), drop = FALSE], 1, function(x) {
    glue::glue_collapse(x, sep = "    ")
  })

  c(header, rows, glue::glue(""))
}

#' Specify the `WindSeverities` table for the Linear Wind extension
#'
#' @param df `data.frame` with columns `Severity`, `LowerAge`, `UpperAge`,
#'   `WindspeedMortalityThreshold`. Rows must be ordered by decreasing `Severity`.
#'
#' @template return_insert
#'
#' @family Linear Wind helpers
#'
#' @keywords internal
insertLinearWindSeverities <- function(df) {
  stopifnot(
    is.data.frame(df),
    all(c("Severity", "LowerAge", "UpperAge", "WindspeedMortalityThreshold") %in% colnames(df))
  )

  rows <- apply(df, 1, function(x) {
    glue::glue(
      "    {x[['Severity']]}        {x[['LowerAge']]}% to {x[['UpperAge']]}%        ",
      "{x[['WindspeedMortalityThreshold']]}"
    )
  })

  c(
    glue::glue("WindSeverities"),
    glue::glue(">>              Cohort Age        Windspeed Mortality"),
    glue::glue(">> Severity     % of longevity    Threshold"),
    glue::glue(">> --------     --------------    -------------------"),
    rows,
    glue::glue("") ## add blank line after each item group
  )
}
