#' Social-Climate-Fire Extension
#'
#' @include ext_utils.R
#'
#' @references LANDIS-II Social-Climate-Fire v4 User Guide
#'   <https://github.com/LANDIS-II-Foundation/Extension-Social-Climate-Fire/blob/master/docs/LANDIS-II%20Social-Climate-Fire%20v4%20User%20Guide.pdf>
#'
#' @export
#'
#' @examples
#' ## See vignette for usage examples.
#'
SocialClimateFire <- R6Class(
  "SocialClimateFire",
  inherit = LandisExtension,
  public = list(
    #' @param path Character. Directory path.
    #' @param Timestep Integer. Not used (see extension user guide).
    #' @param TimeZeroPET (Optional) Real. Potential Evapotranspiration (PET) for time zero.
    #' @param TimeZeroCWD (Optional) Real. Climate Water Deficit (CWD) for time zero.
    #' @param Species_CSV_File Character. Relative file path.
    #' @param AccidentalIgnitionsMap Character. Relative file path.
    #' @param DynamicAccidentalIgnitionMaps (Optional) `data.frame`.
    #' @param LightningIgnitionsMap Character. Relative file path.
    #' @param DynamicLightningIgnitionsMaps (Optional) `data.frame`.
    #' @param RxIgnitionsMap Character. Relative file path.
    #' @param DynamicRxIgnitionsMaps (Optional) `data.frame`.
    #' @param AccidentalSuppressionMap Character. Relative file path.
    #' @param LightningSuppressionMap Character. Relative file path.
    #' @param RxSuppressionMap Character. Relative file path.
    #' @param DynamicAccidentalSuppressionMaps (Optional) `data.frame`.
    #' @param GroundSlopeFile Character. Relative file path.
    #' @param UphillSlopeAzimuthMap Character. Relative file path.
    #' @param ClayMap Character. Relative file path.
    #' @param LightningIgnitionsCoeffs Real. Parameters B0 and B1 from equation 1 (Scheller et al. 2019).
    #' @param AccidentalIgnitionsCoeffs Real. B0 and B1 from equation 1 (Scheller et al. 2019).
    #' @param IgnitionDistribution Character. One of "Poisson" or "ZeroInflatedPoisson".
    #' @param LightningIgnitionsBinomialCoeffs Real. Parameters $b_z_0$ and $b_z_1$ from equation 2 (Scheller et al. 2019).
    #' @param AccidentalIgnitionsBinomialCoeffs Real. Parameters $b_z_0$ and $b_z_1$ from equation 2 (Scheller et al. 2019).
    #' @param MaximumFineFuels Real. Maximum amount of fine fuels ($g/m^2$).
    #' @param MaximumRxWindSpeed Real. Maximum wind speed ($m/s$) for prescribed fires.
    #' @param MaximumRxFireWeatherIndex (Optional) Real. Maximum Fire Weather Index (FWI) for prescribed fires.
    #' @param MinimumRxFireWeatherIndex (Optional) Real. Minimum Fire Weather Index (FWI) for prescribed fires.
    #' @param MaximumRxTemperature (Optional) Real. Maximum temperature for prescribed fires.
    #' @param MinimumRxRelativeHumidity (Optional) Real. Minimum relative humidity for prescribed fires.
    #' @param MaximumRXFireIntensity Integer. Maximum allowable fire intensity for prescribed fires.
    #' @param NumberRxAnnualFires Integer. Number of prescribed fires attempted per year.
    #' @param NumberRxDailyFires Integer. Number of prescribed fires attempted per day.
    #' @param FirstDayRxFires Integer. First Julian day in which a prescribed fire can begin.
    #' @param LastDayRxFires Integer. Last Julian day in which a prescribed fire can begin.
    #' @param TargetRxSize Real. Maximum size for prescribed fire ($ha$).
    #' @param RxZonesMap (Optional) Character. Relative file path.
    #' @param MaximumSpreadAreaCoeffs Real. Parameters B0, B1, B2 from equation 4 (Scheller et al. 2019).
    #' @param SpreadProbabilityCoeffs Real. Parameters B0, B1, B2 from equation 3 (Scheller et al. 2019).
    #' @param SiteMortalityCoeffs Real. Parameters B0, B1, B2, B3, B4, B5, B6 from equation 7 (Scheller et al. 2019).
    #' @param CohortMortalityCoeffs Real. Parameters B0, B1, B2 from equation 10 (Scheller et al. 2019).
    #' @param LadderFuelMaxAge Integer. Maximum age for ladder fuels.
    #' @param LadderFuelSpeciesList Character. Vector of species codes considered to be ladder fuels.
    #' @param SuppressionMaxWindSpeed Real. Maximum wind speed ($m/s$) for fire suppression.
    #' @param Suppression_CSV_File Character. Relative file path.
    #' @param DeadWoodTable `data.frame`.
    #'
    #' @references
    #' Scheller, R., Kretchun, A., Hawbaker, T.J. & Henne, P.D. (2019). A landscape model of
    #' variable social-ecological fire regimes. Ecological Modelling, 401, 85–93.
    #' \doi{10.1016/j.ecolmodel.2019.03.022}
    initialize = function(
      path,
      Timestep = 1,
      TimeZeroPET = NULL,
      TimeZeroCWD = NULL,
      Species_CSV_File = NULL,
      AccidentalIgnitionsMap = NULL,
      DynamicAccidentalIgnitionMaps = NULL,
      LightningIgnitionsMap = NULL,
      DynamicLightningIgnitionsMaps = NULL,
      RxIgnitionsMap = NULL,
      DynamicRxIgnitionsMaps = NULL,
      AccidentalSuppressionMap = NULL,
      LightningSuppressionMap = NULL,
      RxSuppressionMap = NULL,
      DynamicAccidentalSuppressionMaps = NULL,
      GroundSlopeFile = NULL,
      UphillSlopeAzimuthMap = NULL,
      ClayMap = NULL,
      LightningIgnitionsCoeffs = NULL,
      AccidentalIgnitionsCoeffs = NULL,
      IgnitionDistribution = NULL,
      LightningIgnitionsBinomialCoeffs = NULL,
      AccidentalIgnitionsBinomialCoeffs = NULL,
      MaximumFineFuels = NULL,
      MaximumRxWindSpeed = NULL,
      MaximumRxFireWeatherIndex = NULL,
      MinimumRxFireWeatherIndex = NULL,
      MaximumRxTemperature = NULL,
      MinimumRxRelativeHumidity = NULL,
      MaximumRXFireIntensity = NULL,
      NumberRxAnnualFires = NULL,
      NumberRxDailyFires = NULL,
      FirstDayRxFires = NULL,
      LastDayRxFires = NULL,
      TargetRxSize = NULL,
      RxZonesMap = NULL,
      MaximumSpreadAreaCoeffs = NULL,
      SpreadProbabilityCoeffs = NULL,
      SiteMortalityCoeffs = NULL,
      CohortMortalityCoeffs = NULL,
      LadderFuelMaxAge = NULL,
      LadderFuelSpeciesList = NULL,
      SuppressionMaxWindSpeed = NULL,
      Suppression_CSV_File = NULL,
      DeadWoodTable = NULL
    ) {
      stopifnot(!is.null(path))

      ## LandisExtension fields
      private$.LandisData <- "Social Climate Fire"
      self$Timestep <- Timestep

      self$type <- "disturbance"
      self$path <- path
      self$files <- "social-climate-fire.txt" ## file won't exist yet

      ## additional fields for this extension
      self$TimeZeroPET <- TimeZeroPET %||% NA_real_
      self$TimeZeroCWD <- TimeZeroCWD %||% NA_real_
      self$Species_CSV_File <- Species_CSV_File
      self$AccidentalIgnitionsMap <- AccidentalIgnitionsMap
      self$DynamicAccidentalIgnitionMaps <- DynamicAccidentalIgnitionMaps
      self$LightningIgnitionsMap <- LightningIgnitionsMap
      self$DynamicLightningIgnitionsMaps <- DynamicLightningIgnitionsMaps
      self$RxIgnitionsMap <- RxIgnitionsMap
      self$DynamicRxIgnitionsMaps <- DynamicRxIgnitionsMaps
      self$AccidentalSuppressionMap <- AccidentalSuppressionMap
      self$LightningSuppressionMap <- LightningSuppressionMap
      self$RxSuppressionMap <- RxSuppressionMap
      self$DynamicAccidentalSuppressionMaps <- DynamicAccidentalSuppressionMaps
      self$GroundSlopeFile <- GroundSlopeFile
      self$UphillSlopeAzimuthMap <- UphillSlopeAzimuthMap
      self$ClayMap <- ClayMap
      self$LightningIgnitionsCoeffs <- LightningIgnitionsCoeffs
      self$AccidentalIgnitionsCoeffs <- AccidentalIgnitionsCoeffs
      self$IgnitionDistribution <- IgnitionDistribution
      self$LightningIgnitionsBinomialCoeffs <- LightningIgnitionsBinomialCoeffs
      self$AccidentalIgnitionsBinomialCoeffs <- AccidentalIgnitionsBinomialCoeffs
      self$MaximumFineFuels <- MaximumFineFuels
      self$MaximumRxWindSpeed <- MaximumRxWindSpeed
      self$MaximumRxFireWeatherIndex <- MaximumRxFireWeatherIndex
      self$MinimumRxFireWeatherIndex <- MinimumRxFireWeatherIndex
      self$MaximumRxTemperature <- MaximumRxTemperature
      self$MinimumRxRelativeHumidity <- MinimumRxRelativeHumidity
      self$MaximumRXFireIntensity <- MaximumRXFireIntensity
      self$NumberRxAnnualFires <- NumberRxAnnualFires
      self$NumberRxDailyFires <- NumberRxDailyFires
      self$FirstDayRxFires <- FirstDayRxFires
      self$LastDayRxFires <- LastDayRxFires
      self$TargetRxSize <- TargetRxSize
      self$RxZonesMap <- RxZonesMap
      self$MaximumSpreadAreaCoeffs <- MaximumSpreadAreaCoeffs
      self$SpreadProbabilityCoeffs <- SpreadProbabilityCoeffs
      self$SiteMortalityCoeffs <- SiteMortalityCoeffs
      self$CohortMortalityCoeffs <- CohortMortalityCoeffs
      self$LadderFuelMaxAge <- LadderFuelMaxAge
      self$LadderFuelSpeciesList <- LadderFuelSpeciesList
      self$SuppressionMaxWindSpeed <- SuppressionMaxWindSpeed
      self$Suppression_CSV_File <- Suppression_CSV_File
      self$DeadWoodTable <- DeadWoodTable
    },

    #' @description Write extension inputs to disk
    write = function() {
      stopifnot(
        self$Timestep == 1,
        !is.null(self$Species_CSV_File),
        !is.null(self$AccidentalIgnitionsMap),
        !is.null(self$LightningIgnitionsMap),
        !is.null(self$RxIgnitionsMap),
        !is.null(self$LightningSuppressionMap),
        !is.null(self$RxSuppressionMap),
        !is.null(self$GroundSlopeFile),
        !is.null(self$UphillSlopeAzimuthMap),
        !is.null(self$ClayMap),
        !is.null(self$LightningIgnitionsCoeffs),
        !is.null(self$AccidentalIgnitionsCoeffs),
        !is.null(self$IgnitionDistribution),
        !is.null(self$LightningIgnitionsBinomialCoeffs),
        !is.null(self$AccidentalIgnitionsBinomialCoeffs),
        !is.null(self$MaximumFineFuels),
        !is.null(self$MaximumRxWindSpeed),
        !is.null(self$MaximumRXFireIntensity),
        !is.null(self$NumberRxAnnualFires),
        !is.null(self$NumberRxDailyFires),
        !is.null(self$FirstDayRxFires),
        !is.null(self$LastDayRxFires),
        !is.null(self$TargetRxSize),
        !is.null(self$MaximumSpreadAreaCoeffs),
        !is.null(self$SpreadProbabilityCoeffs),
        !is.null(self$SiteMortalityCoeffs),
        !is.null(self$CohortMortalityCoeffs),
        !is.null(self$LadderFuelMaxAge),
        !is.null(self$LadderFuelSpeciesList),
        !is.null(self$SuppressionMaxWindSpeed),
        !is.null(self$Suppression_CSV_File),
        !is.null(self$DeadWoodTable)
      )

      writeLines(
        c(
          insertLandisData(private$.LandisData),
          insertValue("Timestep", self$Timestep),
          insertValue("TimeZeroPET", self$TimeZeroPET),
          insertValue("TimeZeroCWD", self$TimeZeroCWD),
          insertFile("Species_CSV_File", self$Species_CSV_File),

          ## ignition maps
          insertFile("AccidentalIgnitionsMap", self$AccidentalIgnitionsMap),
          insertDynamicTable("DynamicAccidentalIgnitionMaps", self$DynamicAccidentalIgnitionMaps),
          insertFile("LightningIgnitionsMap", self$LightningIgnitionsMap),
          insertDynamicTable("DynamicLightningIgnitionsMaps", self$DynamicLightningIgnitionsMaps),
          insertFile("RxIgnitionsMap", self$RxIgnitionsMap),
          insertDynamicTable("DynamicRxIgnitionsMaps", self$DynamicRxIgnitionsMaps),

          ## suppression maps
          insertFile("AccidentalSuppressionMap", self$AccidentalSuppressionMap),
          insertFile("LightningSuppressionMap", self$LightningSuppressionMap),
          insertFile("RxSuppressionMap", self$RxSuppressionMap),

          ## topography and soil maps
          insertFile("GroundSlopeFile", self$GroundSlopeFile),
          insertFile("UphillSlopeAzimuthMap", self$UphillSlopeAzimuthMap),
          insertFile("ClayMap", self$ClayMap),

          ## ignition model coeffs
          insertValue("LightningIgnitionsB0", self$LightningIgnitionsCoeffs[1]),
          insertValue("LightningIgnitionsB1", self$LightningIgnitionsCoeffs[2]),
          insertValue("AccidentalIgnitionsB0", self$AccidentalIgnitionsCoeffs[1]),
          insertValue("AccidentalIgnitionsB1", self$AccidentalIgnitionsCoeffs[2]),

          insertValue("IgnitionDistribution", self$IgnitionDistribution),

          insertValue("LightningIgnitionsBinomialB0", self$LightningIgnitionsBinomialCoeffs[1]),
          insertValue("LightningIgnitionsBinomialB1", self$LightningIgnitionsBinomialCoeffs[2]),
          insertValue("AccidentalIgnitionsBinomialB0", self$AccidentalIgnitionsBinomialCoeffs[1]),
          insertValue("AccidentalIgnitionsBinomialB1", self$AccidentalIgnitionsBinomialCoeffs[2]),

          insertValue("MaximumFineFuels", self$MaximumFineFuels),

          ## prescribed fires
          insertValue("MaximumRxWindSpeed", self$MaximumRxWindSpeed),
          insertValue("MaximumRxFireWeatherIndex", self$MaximumRxFireWeatherIndex),
          insertValue("MinimumRxFireWeatherIndex", self$MinimumRxFireWeatherIndex),
          insertValue("MaximumRxTemperature", self$MaximumRxTemperature),
          insertValue("MinimumRxRelativeHumidity", self$MinimumRxRelativeHumidity),
          insertValue("MaximumRXFireIntensity", self$MaximumRXFireIntensity),
          insertValue("NumberRxAnnualFires", self$NumberRxAnnualFires),
          insertValue("NumberRxDailyFires", self$NumberRxDailyFires),
          insertValue("FirstDayRxFires", self$FirstDayRxFires),
          insertValue("LastDayRxFires", self$LastDayRxFires),
          insertValue("TargetRxSize", self$TargetRxSize),
          insertFile("RxZonesMap", self$RxZonesMap),

          ## spread area coeffs
          insertValue("MaximumSpreadAreaB0", self$MaximumSpreadAreaCoeffs[1]),
          insertValue("MaximumSpreadAreaB1", self$MaximumSpreadAreaCoeffs[2]),
          insertValue("MaximumSpreadAreaB2", self$MaximumSpreadAreaCoeffs[3]),

          ## spread probability coeffs
          insertValue("SpreadProbabilityB0", self$SpreadProbabilityCoeffs[1]),
          insertValue("SpreadProbabilityB1", self$SpreadProbabilityCoeffs[2]),
          insertValue("SpreadProbabilityB2", self$SpreadProbabilityCoeffs[3]),
          insertValue("SpreadProbabilityB3", self$SpreadProbabilityCoeffs[4]),

          ## site mortality coeffs
          insertValue("SiteMortalityB0", self$SiteMortalityCoeffs[1]),
          insertValue("SiteMortalityB1", self$SiteMortalityCoeffs[2]),
          insertValue("SiteMortalityB2", self$SiteMortalityCoeffs[3]),
          insertValue("SiteMortalityB3", self$SiteMortalityCoeffs[4]),
          insertValue("SiteMortalityB4", self$SiteMortalityCoeffs[5]),
          insertValue("SiteMortalityB5", self$SiteMortalityCoeffs[6]),
          insertValue("SiteMortalityB6", self$SiteMortalityCoeffs[7]),

          ## cohort mortality coeffs
          insertValue("CohortMortalityB0", self$CohortMortalityCoeffs[1]),
          insertValue("CohortMortalityB1", self$CohortMortalityCoeffs[2]),
          insertValue("CohortMortalityB2", self$CohortMortalityCoeffs[3]),

          ## ladder fuels
          insertValue("LadderFuelMaxAge", self$LadderFuelMaxAge),
          insertLadderFuelSpeciesList(self$LadderFuelSpeciesList),

          ## suppression
          insertValue("SuppressionMaxWindSpeed", self$SuppressionMaxWindSpeed),
          insertFile("Suppression_CSV_File", self$Suppression_CSV_File),
          insertDeadWoodTable(self$DeadWoodTable)
        ),
        file.path(self$path, self$files[1])
      )

      ## declare files
      self$add_file(self$Species_CSV_File)
      self$add_file(self$AccidentalIgnitionsMap)
      self$add_file(self$LightningIgnitionsMap)
      self$add_file(self$RxIgnitionsMap)
      self$add_file(self$AccidentalSuppressionMap)
      self$add_file(self$LightningSuppressionMap)
      self$add_file(self$RxSuppressionMap)
      self$add_file(self$GroundSlopeFile)
      self$add_file(self$UphillSlopeAzimuthMap)
      self$add_file(self$ClayMap)
      if (!is.null(self$RxZonesMap) && !is.na(self$RxZonesMap)) {
        self$add_file(self$RxZonesMap)
      }
      self$add_file(self$Suppression_CSV_File)

      return(invisible(self))
    }
  ),

  private = list(
    .TimeZeroPET = NULL,
    .TimeZeroCWD = NULL,
    .Species_CSV_File = NULL,
    .AccidentalIgnitionsMap = NULL,
    .DynamicAccidentalIgnitionMaps = NULL,
    .LightningIgnitionsMap = NULL,
    .DynamicLightningIgnitionsMaps = NULL,
    .RxIgnitionsMap = NULL,
    .DynamicRxIgnitionsMaps = NULL,
    .AccidentalSuppressionMap = NULL,
    .LightningSuppressionMap = NULL,
    .RxSuppressionMap = NULL,
    .DynamicAccidentalSuppressionMaps = NULL,
    .GroundSlopeFile = NULL,
    .UphillSlopeAzimuthMap = NULL,
    .ClayMap = NULL,
    .LightningIgnitionsCoeffs = NULL,
    .AccidentalIgnitionsCoeffs = NULL,
    .IgnitionDistribution = NULL,
    .LightningIgnitionsBinomialCoeffs = NULL,
    .AccidentalIgnitionsBinomialCoeffs = NULL,
    .MaximumFineFuels = NULL,
    .MaximumRxWindSpeed = NULL,
    .MaximumRxFireWeatherIndex = NULL,
    .MinimumRxFireWeatherIndex = NULL,
    .MaximumRxTemperature = NULL,
    .MinimumRxRelativeHumidity = NULL,
    .MaximumRXFireIntensity = NULL,
    .NumberRxAnnualFires = NULL,
    .NumberRxDailyFires = NULL,
    .FirstDayRxFires = NULL,
    .LastDayRxFires = NULL,
    .TargetRxSize = NULL,
    .RxZonesMap = NULL,
    .MaximumSpreadAreaCoeffs = NULL,
    .SpreadProbabilityCoeffs = NULL,
    .SiteMortalityCoeffs = NULL,
    .CohortMortalityCoeffs = NULL,
    .LadderFuelMaxAge = NULL,
    .LadderFuelSpeciesList = NULL,
    .SuppressionMaxWindSpeed = NULL,
    .Suppression_CSV_File = NULL,
    .DeadWoodTable = NULL
  ),

  active = list(
    #' @field TimeZeroPET (Optional) Real. Potential Evapotranspiration (PET) for time zero.
    TimeZeroPET = function(value) {
      if (missing(value)) {
        return(private$.TimeZeroPET)
      } else {
        private$.TimeZeroPET <- value
      }
    },

    #' @field TimeZeroCWD (Optional) Real. Climate Water Deficit (CWD) for time zero.
    TimeZeroCWD = function(value) {
      if (missing(value)) {
        return(private$.TimeZeroCWD)
      } else {
        private$.TimeZeroCWD <- value
      }
    },

    #' @field Species_CSV_File Character. Relative file path.
    Species_CSV_File = function(value) {
      if (missing(value)) {
        return(private$.Species_CSV_File)
      } else {
        private$.Species_CSV_File <- .relPath(value, self$path)
      }
    },

    #' @field AccidentalIgnitionsMap Character. Relative file path.
    AccidentalIgnitionsMap = function(value) {
      if (missing(value)) {
        return(private$.AccidentalIgnitionsMap)
      } else {
        private$.AccidentalIgnitionsMap <- .relPath(value, self$path)
      }
    },

    #' @field DynamicAccidentalIgnitionMaps (Optional) `data.frame` with columns `Year` and `FileName`.
    DynamicAccidentalIgnitionMaps = function(value) {
      if (missing(value)) {
        return(private$.DynamicAccidentalIgnitionMaps)
      } else {
        if (is.null(value) || is.na(value)) {
          value <- data.frame(Year = integer(0), FileName = character(0))
        } else {
          stopifnot(inherits(value, "data.frame"))
        }

        private$.DynamicAccidentalIgnitionMaps <- value
      }
    },

    #' @field LightningIgnitionsMap Character. Relative file path.
    LightningIgnitionsMap = function(value) {
      if (missing(value)) {
        return(private$.LightningIgnitionsMap)
      } else {
        private$.LightningIgnitionsMap <- .relPath(value, self$path)
      }
    },

    #' @field DynamicLightningIgnitionsMaps (Optional) `data.frame` with columns `Year` and `FileName`.
    DynamicLightningIgnitionsMaps = function(value) {
      if (missing(value)) {
        return(private$.DynamicLightningIgnitionsMaps)
      } else {
        if (is.null(value) || is.na(value)) {
          value <- data.frame(Year = integer(0), FileName = character(0))
        } else {
          stopifnot(inherits(value, "data.frame"))
        }

        private$.DynamicLightningIgnitionsMaps <- value
      }
    },

    #' @field RxIgnitionsMap Character. Relative file path.
    RxIgnitionsMap = function(value) {
      if (missing(value)) {
        return(private$.RxIgnitionsMap)
      } else {
        private$.RxIgnitionsMap <- .relPath(value, self$path)
      }
    },

    #' @field DynamicRxIgnitionsMaps (Optional) `data.frame` with columns `Year` and `FileName`.
    DynamicRxIgnitionsMaps = function(value) {
      if (missing(value)) {
        return(private$.DynamicRxIgnitionsMaps)
      } else {
        if (is.null(value) || is.na(value)) {
          value <- data.frame(Year = integer(0), FileName = character(0))
        } else {
          stopifnot(inherits(value, "data.frame"))
        }

        private$.DynamicRxIgnitionsMaps <- value
      }
    },

    #' @field AccidentalSuppressionMap Character. Relative file path.
    AccidentalSuppressionMap = function(value) {
      if (missing(value)) {
        return(private$.AccidentalSuppressionMap)
      } else {
        private$.AccidentalSuppressionMap <- .relPath(value, self$path)
      }
    },

    #' @field LightningSuppressionMap Character. Relative file path.
    LightningSuppressionMap = function(value) {
      if (missing(value)) {
        return(private$.LightningSuppressionMap)
      } else {
        private$.LightningSuppressionMap <- .relPath(value, self$path)
      }
    },

    #' @field RxSuppressionMap Character. Relative file path.
    RxSuppressionMap = function(value) {
      if (missing(value)) {
        return(private$.RxSuppressionMap)
      } else {
        private$.RxSuppressionMap <- .relPath(value, self$path)
      }
    },

    #' @field DynamicAccidentalSuppressionMaps (Optional) `data.frame` with columns `Year` and `FileName`.
    DynamicAccidentalSuppressionMaps = function(value) {
      if (missing(value)) {
        return(private$.DynamicAccidentalSuppressionMaps)
      } else {
        if (is.null(value) || is.na(value)) {
          value <- data.frame(Year = integer(0), FileName = character(0))
        } else {
          stopifnot(inherits(value, "data.frame"))
        }

        private$.DynamicAccidentalSuppressionMaps <- value
      }
    },

    #' @field GroundSlopeFile Character. Relative file path.
    GroundSlopeFile = function(value) {
      if (missing(value)) {
        return(private$.GroundSlopeFile)
      } else {
        private$.GroundSlopeFile <- .relPath(value, self$path)
      }
    },

    #' @field UphillSlopeAzimuthMap Character. Relative file path.
    UphillSlopeAzimuthMap = function(value) {
      if (missing(value)) {
        return(private$.UphillSlopeAzimuthMap)
      } else {
        private$.UphillSlopeAzimuthMap <- .relPath(value, self$path)
      }
    },

    #' @field ClayMap Character. Relative file path.
    ClayMap = function(value) {
      if (missing(value)) {
        return(private$.ClayMap)
      } else {
        private$.ClayMap <- .relPath(value, self$path)
      }
    },

    #' @field LightningIgnitionsCoeffs Real. Parameters B0 and B1 from equation 1 (Scheller et al. 2019).
    LightningIgnitionsCoeffs = function(value) {
      if (missing(value)) {
        return(private$.LightningIgnitionsCoeffs)
      } else {
        stopifnot(length(value) == 2L)

        private$.LightningIgnitionsCoeffs <- value
      }
    },

    #' @field AccidentalIgnitionsCoeffs Real. B0 and B1 from equation 1 (Scheller et al. 2019).
    AccidentalIgnitionsCoeffs = function(value) {
      if (missing(value)) {
        return(private$.AccidentalIgnitionsCoeffs)
      } else {
        stopifnot(length(value) == 2L)

        private$.AccidentalIgnitionsCoeffs <- value
      }
    },

    #' @field IgnitionDistribution Character. One of "Poisson" or "ZeroInflatedPoisson".
    IgnitionDistribution = function(value) {
      if (missing(value)) {
        return(private$.IgnitionDistribution)
      } else {
        allowed <- c("Poisson", "ZeroInflatedPoisson")
        value <- value %||% allowed[1]

        stopifnot(value %in% allowed)

        private$.IgnitionDistribution <- value
      }
    },

    #' @field LightningIgnitionsBinomialCoeffs Real. Parameters $b_z_0$ and $b_z_1$ from equation 2 (Scheller et al. 2019).
    LightningIgnitionsBinomialCoeffs = function(value) {
      if (missing(value)) {
        return(private$.LightningIgnitionsBinomialCoeffs)
      } else {
        stopifnot(length(value) == 2L)

        private$.LightningIgnitionsBinomialCoeffs <- value
      }
    },

    #' @field AccidentalIgnitionsBinomialCoeffs Real. Parameters $b_z_0$ and $b_z_1$ from equation 2 (Scheller et al. 2019).
    AccidentalIgnitionsBinomialCoeffs = function(value) {
      if (missing(value)) {
        return(private$.AccidentalIgnitionsBinomialCoeffs)
      } else {
        stopifnot(length(value) == 2L)

        private$.AccidentalIgnitionsBinomialCoeffs <- value
      }
    },

    #' @field MaximumFineFuels Real. Maximum amount of fine fuels ($g/m^2$).
    MaximumFineFuels = function(value) {
      if (missing(value)) {
        return(private$.MaximumFineFuels)
      } else {
        private$.MaximumFineFuels <- value
      }
    },

    #' @field MaximumRxWindSpeed Real. Maximum wind speed ($m/s$) for prescribed fires.
    MaximumRxWindSpeed = function(value) {
      if (missing(value)) {
        return(private$.MaximumRxWindSpeed)
      } else {
        private$.MaximumRxWindSpeed <- value
      }
    },

    #' @field MaximumRxFireWeatherIndex (Optional) Real. Maximum Fire Weather Index (FWI) for prescribed fires.
    MaximumRxFireWeatherIndex = function(value) {
      if (missing(value)) {
        return(private$.MaximumRxFireWeatherIndex)
      } else {
        private$.MaximumRxFireWeatherIndex <- value %||% NA_real_
      }
    },

    #' @field MinimumRxFireWeatherIndex (Optional) Real. Minimum Fire Weather Index (FWI) for prescribed fires.
    MinimumRxFireWeatherIndex = function(value) {
      if (missing(value)) {
        return(private$.MinimumRxFireWeatherIndex)
      } else {
        private$.MinimumRxFireWeatherIndex <- value %||% NA_real_
      }
    },

    #' @field MaximumRxTemperature (Optional) Real. Maximum temperature for prescribed fires.
    MaximumRxTemperature = function(value) {
      if (missing(value)) {
        return(private$.MaximumRxTemperature)
      } else {
        private$.MaximumRxTemperature <- value %||% NA_real_
      }
    },

    #' @field MinimumRxRelativeHumidity (Optional) Real. Minimum relative humidity for prescribed fires.
    MinimumRxRelativeHumidity = function(value) {
      if (missing(value)) {
        return(private$.MinimumRxRelativeHumidity)
      } else {
        private$.MinimumRxRelativeHumidity <- value %||% NA_real_
      }
    },

    #' @field MaximumRXFireIntensity Integer. Maximum allowable fire intensity for prescribed fires.
    MaximumRXFireIntensity = function(value) {
      if (missing(value)) {
        return(private$.MaximumRXFireIntensity)
      } else {
        private$.MaximumRXFireIntensity <- value
      }
    },

    #' @field NumberRxAnnualFires Integer. Number of prescribed fires attempted per year.
    NumberRxAnnualFires = function(value) {
      if (missing(value)) {
        return(private$.NumberRxAnnualFires)
      } else {
        private$.NumberRxAnnualFires <- value
      }
    },

    #' @field NumberRxDailyFires Integer. Number of prescribed fires attempted per day.
    NumberRxDailyFires = function(value) {
      if (missing(value)) {
        return(private$.NumberRxDailyFires)
      } else {
        private$.NumberRxDailyFires <- value
      }
    },

    #' @field FirstDayRxFires Integer. First Julian day in which a prescribed fire can begin.
    FirstDayRxFires = function(value) {
      if (missing(value)) {
        return(private$.FirstDayRxFires)
      } else {
        private$.FirstDayRxFires <- value
      }
    },

    #' @field LastDayRxFires Integer. Last Julian day in which a prescribed fire can begin.
    LastDayRxFires = function(value) {
      if (missing(value)) {
        return(private$.LastDayRxFires)
      } else {
        private$.LastDayRxFires <- value
      }
    },

    #' @field TargetRxSize Real. Maximum size for prescribed fire ($ha$).
    TargetRxSize = function(value) {
      if (missing(value)) {
        return(private$.TargetRxSize)
      } else {
        private$.TargetRxSize <- value
      }
    },

    #' @field RxZonesMap (Optional) Character. Relative file path.
    RxZonesMap = function(value) {
      if (missing(value)) {
        return(private$.RxZonesMap)
      } else {
        if (is.null(value) || is.na(value)) {
          private$.RxZonesMap <- fs::path(NA_character_)
        } else {
          private$.RxZonesMap <- .relPath(value, self$path)
        }
      }
    },

    #' @field MaximumSpreadAreaCoeffs Real. Parameters B0, B1, B2 from equation 4 (Scheller et al. 2019).
    MaximumSpreadAreaCoeffs = function(value) {
      if (missing(value)) {
        return(private$.MaximumSpreadAreaCoeffs)
      } else {
        stopifnot(length(value) == 3L)

        private$.MaximumSpreadAreaCoeffs <- value
      }
    },

    #' @field SpreadProbabilityCoeffs Real. Parameters B0, B1, B2 from equation 3 (Scheller et al. 2019).
    SpreadProbabilityCoeffs = function(value) {
      if (missing(value)) {
        return(private$.SpreadProbabilityCoeffs)
      } else {
        stopifnot(length(value) == 4L)

        private$.SpreadProbabilityCoeffs <- value
      }
    },

    #' @field SiteMortalityCoeffs Real. Parameters B0, B1, B2, B3, B4, B5, B6 from equation 7 (Scheller et al. 2019).
    SiteMortalityCoeffs = function(value) {
      if (missing(value)) {
        return(private$.SiteMortalityCoeffs)
      } else {
        stopifnot(length(value) == 7L)

        private$.SiteMortalityCoeffs <- value
      }
    },

    #' @field CohortMortalityCoeffs Real. Parameters B0, B1, B2 from equation 10 (Scheller et al. 2019).
    CohortMortalityCoeffs = function(value) {
      if (missing(value)) {
        return(private$.CohortMortalityCoeffs)
      } else {
        stopifnot(length(value) == 3L)

        private$.CohortMortalityCoeffs <- value
      }
    },

    #' @field LadderFuelMaxAge Integer. Maximum age for ladder fuels.
    LadderFuelMaxAge = function(value) {
      if (missing(value)) {
        return(private$.LadderFuelMaxAge)
      } else {
        private$.LadderFuelMaxAge <- value
      }
    },

    #' @field LadderFuelSpeciesList Character. Vector of species codes considered to be ladder fuels.
    LadderFuelSpeciesList = function(value) {
      if (missing(value)) {
        return(private$.LadderFuelSpeciesList)
      } else {
        private$.LadderFuelSpeciesList <- value
      }
    },

    #' @field SuppressionMaxWindSpeed Real. Maximum wind speed ($m/s$) for fire suppression.
    SuppressionMaxWindSpeed = function(value) {
      if (missing(value)) {
        return(private$.SuppressionMaxWindSpeed)
      } else {
        private$.SuppressionMaxWindSpeed <- value
      }
    },

    #' @field Suppression_CSV_File Character. Relative file path.
    Suppression_CSV_File = function(value) {
      if (missing(value)) {
        return(private$.Suppression_CSV_File)
      } else {
        private$.Suppression_CSV_File <- .relPath(value, self$path)
      }
    },

    #' @field DeadWoodTable `data.frame` with columns `Species` and `Age`.
    DeadWoodTable = function(value) {
      if (missing(value)) {
        return(private$.DeadWoodTable)
      } else {
        if (is.data.frame(value)) {
          stopifnot(ncol(value) == 2L)
        } else if (is.null(value) || is.na(value)) {
          value <- data.frame(Species = character(0), Age = integer(0))
        } else {
          stopifnot(inherits(value, "data.frame"))
        }

        private$.DeadWoodTable <- value
      }
    }
  )
)

#' Prepare `Species_CSV_File` for Social-Climate-Fire extension
#'
#' @param df data.frame corresponding to `Species_CSV_File` table
#'
#' @template return_file
#'
#' @export
prepSpecies_CSV_File <- function(df, path, filename = "species-table.csv") {
  stopifnot(c("SpeciesCode", "AgeDBH", "MaximumBarkThickness") %in% colnames(df))

  file <- file.path(path, filename)
  write.csv(df, file, row.names = FALSE)

  return(file)
}

#' Prepare `Suppression_CSV_File` for Social-Climate-Fire extension
#'
#' @param df data.frame corresponding to `Suppression_CSV_File` table
#'
#' @template return_file
#'
#' @export
prepSuppression_CSV_File <- function(df, path, filename = "suppression.csv") {
  stopifnot(ncol(df) == 7L, all(dplyr::pull(df, 1) %in% c("Accidental", "Lightning", "Rx")))

  file <- file.path(path, filename)
  write.csv(df, file, row.names = FALSE)

  return(file)
}

#' Specify `LadderFuelSpeciesList` for Social-Climate-Fire extension
#'
#' @param x Character. Species names.
#'
#' @template return_insert
#'
#' @keywords internal
insertLadderFuelSpeciesList <- function(x) {
  c(
    glue::glue_collapse(x, sep = "  "),
    glue::glue("") ## add blank line after each item group
  )
}

#' Specify `DeadWoodTable` for Social-Climate-Fire extension
#'
#' @param df `data.frame` with columns `Species` and `Age`.
#'
#' @template return_insert
#'
#' @keywords internal
insertDeadWoodTable <- function(df = NULL) {
  if (!is.data.frame(df)) {
    if (is.null(df) || is.na(df)) {
      df <- data.frame(Species = character(0), Age = integer(0))
    }
  }

  c(
    glue::glue("DeadWoodTable"),
    apply(df, 1, function(x) {
      glue::glue_collapse(x, sep = "    ")
    }),
    glue::glue("") ## add blank line after each item group
  )
}
