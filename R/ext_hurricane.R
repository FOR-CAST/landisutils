#' Biomass Hurricane Extension
#'
#' @include ext_utils.R
#'
#' @references LANDIS-II Biomass Hurricane v3 Extension User Guide
#'   <https://github.com/LANDIS-II-Foundation/Extension-Biomass-Hurricane/blob/master/docs/LANDIS-II%20Biomass%20Hurricane%20v3%20User%20Guide.pdf>
#'
#' @family Hurricane helpers
#'
#' @export
Hurricane <- R6Class(
  "Hurricane",
  inherit = LandisExtension,
  public = list(
    #' @param path Character. Directory path.
    #' @param Timestep Integer. Years.
    #' @param InputUnitsEnglish Logical (or `"yes"`/`"no"`). When `TRUE`,
    #'   wind speeds are in mph; otherwise km/h.
    #' @param HurricaneRandomNumberSeed Optional integer. Seed for the
    #'   hurricane-specific RNG; lets hurricanes vary independently of the
    #'   Core RNG. Omit to fall back to the Core seed.
    #' @param StormOccurrenceProbabilities `data.frame` with columns
    #'   `Storms` (integer) and `Probability` (numeric); the probabilities
    #'   must sum to `1.0`.
    #' @param LowBoundLandfallWindSpeed,ModeLandfallWindSpeed,HighBoundLandfallWindSpeed
    #'   Numeric. Landfall wind-speed distribution parameters (units per
    #'   `InputUnitsEnglish`).
    #' @param CoastalSlope Numeric. Slope factor relative to landscape.
    #' @param MeanStormIntersectionX,MeanStormIntersectionY Numeric. Mean
    #'   storm-centre coordinates.
    #' @param LandfallSigma Numeric. Variance of landfalls around the mean.
    #' @param StormDirectionMu,StormDirectionSigma Numeric. Mean and standard
    #'   deviation of storm direction (degrees).
    #' @param MinimumWindSpeedforDamage Numeric. Damage threshold; should
    #'   match the smallest wind threshold in `WindSpeedVulnerabilities`.
    #' @param ExposureMaps `data.frame` with columns `Degree` (integer) and
    #'   `MapName` (character).
    #' @param WindSpeedVulnerabilities List of `WindSpeedVulnerability`
    #'   sub-objects (see [windSpeedVulnerability()]).
    #' @param MapNames Character. Output filename pattern; must contain
    #'   the literal `{timestep}`. Including `{stormNumber}` is recommended
    #'   so per-storm maps within a timestep do not overwrite one another.
    #' @param LogFile Character. Relative file path for the CSV log.
    #' @param WindReductionTableCSV Optional character. Relative path to a
    #'   wind-reduction-factor CSV (links to the Output Cohort Statistics
    #'   `Evenness` calculation). Omit to disable structure-based wind-speed
    #'   reduction.
    initialize = function(
      path,
      Timestep = NULL,
      InputUnitsEnglish = FALSE,
      HurricaneRandomNumberSeed = NULL,
      StormOccurrenceProbabilities = NULL,
      LowBoundLandfallWindSpeed = NULL,
      ModeLandfallWindSpeed = NULL,
      HighBoundLandfallWindSpeed = NULL,
      CoastalSlope = NULL,
      MeanStormIntersectionX = NULL,
      MeanStormIntersectionY = NULL,
      LandfallSigma = NULL,
      StormDirectionMu = NULL,
      StormDirectionSigma = NULL,
      MinimumWindSpeedforDamage = NULL,
      ExposureMaps = NULL,
      WindSpeedVulnerabilities = list(),
      MapNames = NULL,
      LogFile = "hurricane/hurricane-log.csv",
      WindReductionTableCSV = NULL
    ) {
      stopifnot(!is.null(path))

      ## LandisExtension fields
      private$.LandisData <- "Hurricane"
      self$Timestep <- Timestep

      self$type <- "disturbance"
      self$path <- path
      self$files <- "hurricane.txt" ## file won't exist yet

      ## additional fields
      self$InputUnitsEnglish <- InputUnitsEnglish
      self$HurricaneRandomNumberSeed <- HurricaneRandomNumberSeed
      self$StormOccurrenceProbabilities <- StormOccurrenceProbabilities
      self$LowBoundLandfallWindSpeed <- LowBoundLandfallWindSpeed
      self$ModeLandfallWindSpeed <- ModeLandfallWindSpeed
      self$HighBoundLandfallWindSpeed <- HighBoundLandfallWindSpeed
      self$CoastalSlope <- CoastalSlope
      self$MeanStormIntersectionX <- MeanStormIntersectionX
      self$MeanStormIntersectionY <- MeanStormIntersectionY
      self$LandfallSigma <- LandfallSigma
      self$StormDirectionMu <- StormDirectionMu
      self$StormDirectionSigma <- StormDirectionSigma
      self$MinimumWindSpeedforDamage <- MinimumWindSpeedforDamage
      self$ExposureMaps <- ExposureMaps
      self$WindSpeedVulnerabilities <- WindSpeedVulnerabilities
      self$MapNames <- MapNames %||% "hurricane/max-windspeed-{timestep}-{stormNumber}.tif"
      self$LogFile <- LogFile
      self$WindReductionTableCSV <- WindReductionTableCSV
    },

    #' @description Write extension inputs to disk
    write = function() {
      stopifnot(
        !is.null(self$StormOccurrenceProbabilities),
        !is.null(self$LowBoundLandfallWindSpeed),
        !is.null(self$ModeLandfallWindSpeed),
        !is.null(self$HighBoundLandfallWindSpeed),
        !is.null(self$CoastalSlope),
        !is.null(self$MeanStormIntersectionX),
        !is.null(self$MeanStormIntersectionY),
        !is.null(self$LandfallSigma),
        !is.null(self$StormDirectionMu),
        !is.null(self$StormDirectionSigma),
        !is.null(self$MinimumWindSpeedforDamage),
        !is.null(self$ExposureMaps),
        length(self$WindSpeedVulnerabilities) >= 1L,
        !is.null(self$MapNames),
        !is.null(self$LogFile)
      )

      lines <- c(
        insertLandisData(private$.LandisData),
        insertValue("Timestep", self$Timestep),
        insertValue("InputUnitsEnglish", self$InputUnitsEnglish)
      )
      if (!is.null(self$HurricaneRandomNumberSeed)) {
        lines <- c(lines, insertValue("HurricaneRandomNumberSeed", self$HurricaneRandomNumberSeed))
      }
      lines <- c(
        lines,
        insertStormOccurrenceProbabilities(self$StormOccurrenceProbabilities),
        insertValue("LowBoundLandfallWindSpeed", self$LowBoundLandfallWindSpeed, blank_line = FALSE),
        insertValue("ModeLandfallWindSpeed", self$ModeLandfallWindSpeed, blank_line = FALSE),
        insertValue("HighBoundLandfallWindSpeed", self$HighBoundLandfallWindSpeed),
        insertValue("CoastalSlope", self$CoastalSlope),
        insertValue("MeanStormIntersectionX", self$MeanStormIntersectionX, blank_line = FALSE),
        insertValue("MeanStormIntersectionY", self$MeanStormIntersectionY),
        insertValue("LandfallSigma", self$LandfallSigma),
        insertValue("StormDirectionMu", self$StormDirectionMu, blank_line = FALSE),
        insertValue("StormDirectionSigma", self$StormDirectionSigma),
        insertValue("MinimumWindSpeedforDamage", self$MinimumWindSpeedforDamage),
        insertExposureMaps(self$ExposureMaps),
        insertWindSpeedVulnerabilities(self$WindSpeedVulnerabilities),
        insertValue("MapNames", self$MapNames),
        insertValue("LogFile", self$LogFile)
      )
      if (!is.null(self$WindReductionTableCSV)) {
        lines <- c(lines, insertValue("WindReductionTableCSV", self$WindReductionTableCSV))
      }

      writeLines(lines, file.path(self$path, self$files[1]))

      return(invisible(self))
    }
  ),

  private = list(
    .InputUnitsEnglish = NULL,
    .HurricaneRandomNumberSeed = NULL,
    .StormOccurrenceProbabilities = NULL,
    .LowBoundLandfallWindSpeed = NULL,
    .ModeLandfallWindSpeed = NULL,
    .HighBoundLandfallWindSpeed = NULL,
    .CoastalSlope = NULL,
    .MeanStormIntersectionX = NULL,
    .MeanStormIntersectionY = NULL,
    .LandfallSigma = NULL,
    .StormDirectionMu = NULL,
    .StormDirectionSigma = NULL,
    .MinimumWindSpeedforDamage = NULL,
    .ExposureMaps = NULL,
    .WindSpeedVulnerabilities = list(),
    .MapNames = NULL,
    .LogFile = NULL,
    .WindReductionTableCSV = NULL
  ),

  active = list(
    #' @field InputUnitsEnglish Character `"yes"` / `"no"`.
    InputUnitsEnglish = function(value) {
      if (missing(value)) {
        return(private$.InputUnitsEnglish)
      } else {
        ## Hurricane uses the bare letter `Y` or `N` historically; render as Y/N.
        yn <- yesno(value)
        private$.InputUnitsEnglish <- if (yn == "yes") "Y" else "N"
      }
    },

    #' @field HurricaneRandomNumberSeed Integer.
    HurricaneRandomNumberSeed = function(value) {
      if (missing(value)) {
        return(private$.HurricaneRandomNumberSeed)
      } else {
        if (!is.null(value)) {
          stopifnot(as.integer(value) > 0L)
        }
        private$.HurricaneRandomNumberSeed <- as.integer(value)
      }
    },

    #' @field StormOccurrenceProbabilities `data.frame` with columns `Storms`,
    #'   `Probability`.
    StormOccurrenceProbabilities = function(value) {
      if (missing(value)) {
        return(private$.StormOccurrenceProbabilities)
      } else {
        if (!is.null(value)) {
          stopifnot(
            is.data.frame(value),
            all(c("Storms", "Probability") %in% colnames(value)),
            all(value$Probability >= 0, value$Probability <= 1),
            isTRUE(all.equal(sum(value$Probability), 1.0))
          )
        }
        private$.StormOccurrenceProbabilities <- value
      }
    },

    #' @field LowBoundLandfallWindSpeed Numeric.
    LowBoundLandfallWindSpeed = function(value) {
      if (missing(value)) private$.LowBoundLandfallWindSpeed
      else private$.LowBoundLandfallWindSpeed <- value
    },

    #' @field ModeLandfallWindSpeed Numeric.
    ModeLandfallWindSpeed = function(value) {
      if (missing(value)) private$.ModeLandfallWindSpeed
      else private$.ModeLandfallWindSpeed <- value
    },

    #' @field HighBoundLandfallWindSpeed Numeric.
    HighBoundLandfallWindSpeed = function(value) {
      if (missing(value)) private$.HighBoundLandfallWindSpeed
      else private$.HighBoundLandfallWindSpeed <- value
    },

    #' @field CoastalSlope Numeric.
    CoastalSlope = function(value) {
      if (missing(value)) private$.CoastalSlope
      else private$.CoastalSlope <- value
    },

    #' @field MeanStormIntersectionX Numeric.
    MeanStormIntersectionX = function(value) {
      if (missing(value)) private$.MeanStormIntersectionX
      else private$.MeanStormIntersectionX <- value
    },

    #' @field MeanStormIntersectionY Numeric.
    MeanStormIntersectionY = function(value) {
      if (missing(value)) private$.MeanStormIntersectionY
      else private$.MeanStormIntersectionY <- value
    },

    #' @field LandfallSigma Numeric.
    LandfallSigma = function(value) {
      if (missing(value)) private$.LandfallSigma
      else private$.LandfallSigma <- value
    },

    #' @field StormDirectionMu Numeric.
    StormDirectionMu = function(value) {
      if (missing(value)) private$.StormDirectionMu
      else private$.StormDirectionMu <- value
    },

    #' @field StormDirectionSigma Numeric.
    StormDirectionSigma = function(value) {
      if (missing(value)) private$.StormDirectionSigma
      else private$.StormDirectionSigma <- value
    },

    #' @field MinimumWindSpeedforDamage Numeric.
    MinimumWindSpeedforDamage = function(value) {
      if (missing(value)) private$.MinimumWindSpeedforDamage
      else private$.MinimumWindSpeedforDamage <- value
    },

    #' @field ExposureMaps `data.frame` with columns `Degree`, `MapName`.
    ExposureMaps = function(value) {
      if (missing(value)) {
        return(private$.ExposureMaps)
      } else {
        if (!is.null(value)) {
          stopifnot(
            is.data.frame(value),
            all(c("Degree", "MapName") %in% colnames(value)),
            is.character(value$MapName)
          )
        }
        private$.ExposureMaps <- value
      }
    },

    #' @field WindSpeedVulnerabilities List of `WindSpeedVulnerability` objects.
    WindSpeedVulnerabilities = function(value) {
      if (missing(value)) {
        return(private$.WindSpeedVulnerabilities)
      } else {
        if (is.null(value)) {
          private$.WindSpeedVulnerabilities <- list()
          return(invisible())
        }
        if (inherits(value, "WindSpeedVulnerability")) {
          value <- list(value)
        }
        stopifnot(
          is.list(value),
          all(vapply(value, inherits, logical(1), "WindSpeedVulnerability"))
        )
        private$.WindSpeedVulnerabilities <- value
      }
    },

    #' @field MapNames Character. Output filename pattern; must contain
    #'   `{timestep}`. Including `{stormNumber}` is recommended.
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

    #' @field LogFile Character. Relative path.
    LogFile = function(value) {
      if (missing(value)) {
        return(private$.LogFile)
      } else {
        private$.LogFile <- .relPath(value, self$path)
      }
    },

    #' @field WindReductionTableCSV Character. Relative path.
    WindReductionTableCSV = function(value) {
      if (missing(value)) {
        return(private$.WindReductionTableCSV)
      } else {
        if (!is.null(value)) {
          private$.WindReductionTableCSV <- .relPath(value, self$path)
        } else {
          private$.WindReductionTableCSV <- NULL
        }
      }
    }
  )
)

#' Construct a per-species wind-speed vulnerability curve
#'
#' Bundles a species code, its maximum cohort age, and a set of
#' wind-speed -> mortality-probability pairs into a single object. One or
#' more of these is passed to [Hurricane] via `WindSpeedVulnerabilities`.
#'
#' @param species Character. Species code (e.g. `"poputrem"`).
#' @param maxAge Integer. Maximum cohort age considered.
#' @param mortality Named numeric vector mapping wind-speed thresholds
#'   (names) to mortality probabilities in `[0, 1]` (values). Names will be
#'   coerced to numeric.
#'
#' @returns A list of class `"WindSpeedVulnerability"`.
#'
#' @examples
#' v <- windSpeedVulnerability(
#'   species = "poputrem",
#'   maxAge = 60,
#'   mortality = c("60" = 0.05, "75" = 0.18, "110" = 0.75, "140" = 1.0)
#' )
#'
#' @family Hurricane helpers
#'
#' @export
windSpeedVulnerability <- function(species, maxAge, mortality) {
  stopifnot(
    is.character(species), length(species) == 1L, nzchar(species),
    is.numeric(maxAge), length(maxAge) == 1L, maxAge > 0,
    is.numeric(mortality),
    !is.null(names(mortality)),
    all(nzchar(names(mortality))),
    all(!is.na(suppressWarnings(as.numeric(names(mortality))))),
    all(mortality >= 0, mortality <= 1)
  )

  ## sort by ascending wind-speed threshold
  ord <- order(as.numeric(names(mortality)))
  mortality <- mortality[ord]

  structure(
    list(species = species, maxAge = as.integer(maxAge), mortality = mortality),
    class = "WindSpeedVulnerability"
  )
}

#' Specify the `StormOccurrenceProbabilities` block for the Hurricane extension
#'
#' @param df `data.frame` with columns `Storms`, `Probability`.
#'
#' @template return_insert
#'
#' @family Hurricane helpers
#'
#' @keywords internal
insertStormOccurrenceProbabilities <- function(df) {
  stopifnot(
    is.data.frame(df),
    all(c("Storms", "Probability") %in% colnames(df))
  )

  rows <- apply(df[, c("Storms", "Probability"), drop = FALSE], 1, function(x) {
    glue::glue("{x[['Storms']]}\t{x[['Probability']]}")
  })

  c(
    glue::glue(">> Likelihood a given year will have this number of storms"),
    glue::glue("StormOccurrenceProbabilities"),
    glue::glue(" >> Storms"),
    glue::glue(" >>  Per"),
    glue::glue(" >>  Year       Probability  << Sum must = 1.0"),
    rows,
    glue::glue("")
  )
}

#' Specify the `ExposureMaps` block for the Hurricane extension
#'
#' @param df `data.frame` with columns `Degree`, `MapName`.
#'
#' @template return_insert
#'
#' @family Hurricane helpers
#'
#' @keywords internal
insertExposureMaps <- function(df) {
  stopifnot(
    is.data.frame(df),
    all(c("Degree", "MapName") %in% colnames(df))
  )

  rows <- apply(df[, c("Degree", "MapName"), drop = FALSE], 1, function(x) {
    glue::glue("{x[['Degree']]}\t{x[['MapName']]}")
  })

  c(
    glue::glue("ExposureMaps  << table"),
    glue::glue(">> Column 1 = degree"),
    glue::glue(">> Column 2 = map name"),
    rows,
    glue::glue("")
  )
}

#' Specify the `WindSpeedVulnerabilities` block for the Hurricane extension
#'
#' @param vulns List of `WindSpeedVulnerability` objects (see
#'   [windSpeedVulnerability()]).
#'
#' @template return_insert
#'
#' @family Hurricane helpers
#'
#' @keywords internal
insertWindSpeedVulnerabilities <- function(vulns) {
  stopifnot(
    is.list(vulns),
    length(vulns) >= 1L,
    all(vapply(vulns, inherits, logical(1), "WindSpeedVulnerability"))
  )

  rows <- vapply(vulns, function(v) {
    pairs <- paste(
      vapply(seq_along(v$mortality), function(i) {
        sprintf("%s:%s", names(v$mortality)[i], v$mortality[[i]])
      }, character(1)),
      collapse = "    "
    )
    as.character(glue::glue("   {format(v$species, width = -20)}{format(v$maxAge, width = 6)}    {pairs}"))
  }, character(1))

  c(
    glue::glue("WindSpeedVulnerabilities"),
    glue::glue(">> Species              MaxAge    Mortality Probabilities (windspeed:probability)"),
    rows,
    glue::glue("")
  )
}

#' Default Hurricane wind-speed mortality curve
#'
#' Matches the Trevor / Sitka example used in the upstream `Core8-Hurricane3.0`
#' test input.
#'
#' @returns Named numeric vector mapping wind-speed thresholds to mortality
#'   probabilities.
#'
#' @family Hurricane helpers
#'
#' @export
defaultHurricaneMortalityCurve <- function() {
  c("60" = 0.05, "75" = 0.18, "110" = 0.75, "140" = 1.0)
}
