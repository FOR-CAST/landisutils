#' Required keys in `NECNSuccession$SoilMaps`
#' @keywords internal
.necnSoilMapKeys <- c(
  ## soil physical maps (section 2.7)
  "SoilDepthMapName",
  "SoilDrainMapName",
  "SoilBaseFlowMapName",
  "SoilStormFlowMapName",
  "SoilFieldCapacityMapName",
  "SoilWiltingPointMapName",
  ## Sand must precede Clay -- the upstream NECN parser checks this order
  ## (verified against ghcr.io/landis-ii-foundation/landis-ii-v8-release:main).
  "SoilPercentSandMapName",
  "SoilPercentClayMapName",
  ## initial soil + dead wood maps (section 2.8)
  "InitialSOM1CsurfMapName",
  "InitialSOM1NsurfMapName",
  "InitialSOM1CsoilMapName",
  "InitialSOM1NsoilMapName",
  "InitialSOM2CMapName",
  "InitialSOM2NMapName",
  "InitialSOM3CMapName",
  "InitialSOM3NMapName",
  "InitialDeadWoodSurfaceMapName",
  ## Upstream parser key is `InitialDeadCoarseRootsMapName`, not `InitialDeadWoodRootsMapName`
  ## (verified against ghcr.io/landis-ii-foundation/landis-ii-v8-release:main).
  "InitialDeadCoarseRootsMapName"
)

#' Allowed keys in `NECNSuccession$OptionalClimateMaps`
#' @keywords internal
.necnOptionalClimateMapKeys <- c(
  "SlopeMapName",
  "AspectMapName",
  "NormalSWAMapName",
  "NormalCWDMapName",
  "NormalTempMapName"
)

#' Allowed keys in `NECNSuccession$OutputMaps`, from Table 3 of user guide.
#' @keywords internal
.necnOutputMapKeys <- c(
  "ANPPMapName",
  "ANPPMapFrequency",
  "ANEEMapName",
  "ANEEMapFrequency",
  "SoilCarbonMapName",
  "SoilNitrogenMapName",
  "TotalCMapName"
)

#' Allowed keys in `NECNSuccession$VariableOverrides`
#' @keywords internal
.necnVariableOverrideKeys <- c(
  "Stormflow",
  "WaterFactor1",
  "WaterFactor2",
  "AnaerobicFactor1",
  "AnaerobicFactor2",
  "AnaerobicFactor3"
)

#' NECN Succession Extension
#'
#' @include ext_utils.R
#'
#' @references LANDIS-II Net Ecosystem CN Succession v8 Extension User Guide
#'   <https://github.com/LANDIS-II-Foundation/Extension-NECN-Succession/blob/master/docs/LANDIS-II%20Net%20Ecosystem%20CN%20Succession%20v8%20User%20Guide.pdf>
#'
#' @seealso
#' Helpers that prepare inputs for this extension:
#' [prepNECNFireReductionParameters()].
#' Shared scenario inputs:
#' [prepClimateConfig()],
#' [prepInitialCommunities()],
#' [prepSpeciesData()].
#'
#' @family NECN Succession helpers
#'
#' @export
NECNSuccession <- R6Class(
  "NECNSuccession",
  inherit = LandisExtension,
  public = list(
    #' @param path Character. Directory path.
    #' @param Timestep Integer.
    #' @template param_SeedingAlgorithm
    #' @param InitialCommunitiesCSV Character. Relative file path to the
    #'   initial communities CSV (see §4 of the user guide).
    #' @param InitialCommunitiesMap Character. Relative file path to the
    #'   initial communities raster.
    #' @param ClimateConfigFile Character. Relative file path to the climate
    #'   library configuration file.
    #' @param SoilMaps Named list of relative file paths for required soil physical
    #'   and initial soil organic matter / dead wood map inputs. Keys must match
    #'   the NECN keywords (Tables 1 and 2 of the user guide): `r .fmtKeys(.necnSoilMapKeys)`.
    #' @param OptionalClimateMaps (Optional) Named list of relative file paths for
    #'   any of `SlopeMapName`, `AspectMapName`, `NormalSWAMapName`,
    #'   `NormalCWDMapName`, `NormalTempMapName`.
    #' @param CalibrateMode Logical, or character indicating "yes" or "no".
    #' @param SmokeModelOutputs Logical, or character indicating "yes" or "no".
    #' @param Write_SWA_Maps Logical, or character indicating "yes" or "no".
    #' @param Write_CWD_Maps Logical, or character indicating "yes" or "no".
    #' @param Write_Temperature_Maps Logical, or character indicating "yes" or "no".
    #' @param Write_Species_Drought_Maps Logical, or character indicating
    #'   "yes" or "no".
    #' @param WaterDecayFunction Character. One of `"Linear"` or `"Ratio"`.
    #' @param ProbabilityEstablishAdjust Numeric. Multiplier applied to the
    #'   probability of establishment. Default `1.0`.
    #' @param InitialMineralN Numeric. Initial mineral N ($g/m^2$).
    #' @param InitialFineFuels Numeric in `[0, 1]`. Fraction of initial dead wood
    #'   allocated to fine fuels.
    #' @param AtmosphericNSlope Numeric. Slope for linear N-deposition model.
    #' @param AtmosphericNIntercept Numeric. Intercept for linear N-deposition model.
    #' @param Latitude Numeric. Study-site latitude (degrees).
    #' @param DenitrificationRate Numeric in `[0, 1]`. Monthly fraction of mineral
    #'   N lost through ammonia volatilization and denitrification.
    #' @param DecayRateSurf Numeric in `[0, 1]`. Max decay rate of SOM1-surface pool.
    #' @param DecayRateSOM1 Numeric in `[0, 1]`. Max decay rate of SOM1-soil pool.
    #' @param DecayRateSOM2 Numeric in `[0, 1]`. Max decay rate of SOM2 (slow) pool.
    #' @param DecayRateSOM3 Numeric in `[0, 1]`. Max decay rate of SOM3 (passive) pool.
    #' @param GrassThresholdMultiplier (Optional) Numeric. Grass/tree competition
    #'   multiplier; see user guide §2.28.
    #' @param OutputMaps (Optional) Named list of optional output-map keywords and values.
    #'   See table 3 of the user guide. Allowed names: `r .fmtKeys(.necnOutputMapKeys)`.
    #'   `*MapName` values are file-name templates and must contain the literal
    #'   `{timestep}` placeholder (user guide §2.29), e.g.
    #'   `"NECN/ANPP-{timestep}.tif"`.
    #' @param CreateInputCommunityMaps Logical, or character indicating "yes" or "no".
    #' @param VariableOverrides (Optional) Named list of numeric override values.
    #'   Allowed names: `r .fmtKeys(.necnVariableOverrideKeys)`.
    #' @param SpeciesParameters Character. Relative file path to the
    #'   species-parameters CSV (see §2.32, Table 4, of the user guide).
    #' @param DroughtMortalityParameters (Optional) Character. Relative file path
    #'   to the drought-mortality-parameters CSV (see §2.33, Table 5, of
    #'   the user guide).
    #' @param FireReductionParameters `data.frame` with columns `FireSeverity`,
    #'   `CoarseDebrisReduction`, `FineLitterReduction`, `CohortWoodReduction`,
    #'   `CohortLeafReduction`, `SOMReduction`. Required even when no fire
    #'   extension is used.
    #' @param HarvestReductionParameters (Optional) `data.frame` with columns
    #'   `PrescriptionName`, `DeadWoodReduction`, `DeadLitterReduction`,
    #'   `CohortWoodRemoval`, `CohortLeafRemoval`.
    initialize = function(
      path,
      Timestep = 10L,
      SeedingAlgorithm = NULL,
      InitialCommunitiesCSV = NULL,
      InitialCommunitiesMap = NULL,
      ClimateConfigFile = NULL,
      SoilMaps = NULL,
      OptionalClimateMaps = NULL,
      CalibrateMode = NULL,
      SmokeModelOutputs = NULL,
      Write_SWA_Maps = NULL,
      Write_CWD_Maps = NULL,
      Write_Temperature_Maps = NULL,
      Write_Species_Drought_Maps = NULL,
      WaterDecayFunction = NULL,
      ProbabilityEstablishAdjust = 1.0,
      InitialMineralN = NULL,
      InitialFineFuels = NULL,
      AtmosphericNSlope = NULL,
      AtmosphericNIntercept = NULL,
      Latitude = NULL,
      DenitrificationRate = NULL,
      DecayRateSurf = NULL,
      DecayRateSOM1 = NULL,
      DecayRateSOM2 = NULL,
      DecayRateSOM3 = NULL,
      GrassThresholdMultiplier = NULL,
      OutputMaps = NULL,
      CreateInputCommunityMaps = NULL,
      VariableOverrides = NULL,
      SpeciesParameters = NULL,
      DroughtMortalityParameters = NULL,
      FireReductionParameters = NULL,
      HarvestReductionParameters = NULL
    ) {
      stopifnot(!is.null(path))

      ## LandisExtension fields
      private$.LandisData <- "NECN Succession"
      self$Timestep <- Timestep

      self$type <- "succession"
      self$path <- path
      self$files <- "necn-succession.txt" ## file won't exist yet

      ## additional fields for this extension
      self$SeedingAlgorithm <- SeedingAlgorithm %||% "WardSeedDispersal"
      self$InitialCommunitiesCSV <- InitialCommunitiesCSV
      self$InitialCommunitiesMap <- InitialCommunitiesMap
      self$ClimateConfigFile <- ClimateConfigFile
      self$SoilMaps <- SoilMaps
      self$OptionalClimateMaps <- OptionalClimateMaps
      self$CalibrateMode <- CalibrateMode %||% FALSE
      self$SmokeModelOutputs <- SmokeModelOutputs %||% FALSE
      self$Write_SWA_Maps <- Write_SWA_Maps %||% FALSE
      self$Write_CWD_Maps <- Write_CWD_Maps %||% FALSE
      self$Write_Temperature_Maps <- Write_Temperature_Maps %||% FALSE
      self$Write_Species_Drought_Maps <- Write_Species_Drought_Maps %||% FALSE
      self$WaterDecayFunction <- WaterDecayFunction %||% "Linear"
      self$ProbabilityEstablishAdjust <- ProbabilityEstablishAdjust %||% 1.0
      self$InitialMineralN <- InitialMineralN
      self$InitialFineFuels <- InitialFineFuels
      self$AtmosphericNSlope <- AtmosphericNSlope
      self$AtmosphericNIntercept <- AtmosphericNIntercept
      self$Latitude <- Latitude
      self$DenitrificationRate <- DenitrificationRate
      self$DecayRateSurf <- DecayRateSurf
      self$DecayRateSOM1 <- DecayRateSOM1
      self$DecayRateSOM2 <- DecayRateSOM2
      self$DecayRateSOM3 <- DecayRateSOM3
      self$GrassThresholdMultiplier <- GrassThresholdMultiplier
      self$OutputMaps <- OutputMaps
      self$CreateInputCommunityMaps <- CreateInputCommunityMaps %||% FALSE
      self$VariableOverrides <- VariableOverrides
      self$SpeciesParameters <- SpeciesParameters
      self$DroughtMortalityParameters <- DroughtMortalityParameters
      self$FireReductionParameters <- FireReductionParameters
      self$HarvestReductionParameters <- HarvestReductionParameters
    },

    #' @description Write extension inputs to disk
    write = function() {
      stopifnot(
        !is.null(self$InitialCommunitiesCSV),
        !is.null(self$InitialCommunitiesMap),
        !is.null(self$ClimateConfigFile),
        !is.null(self$SoilMaps),
        all(.necnSoilMapKeys %in% names(self$SoilMaps)),
        !is.null(self$InitialMineralN),
        !is.null(self$InitialFineFuels),
        !is.null(self$AtmosphericNSlope),
        !is.null(self$AtmosphericNIntercept),
        !is.null(self$Latitude),
        !is.null(self$DenitrificationRate),
        !is.null(self$DecayRateSurf),
        !is.null(self$DecayRateSOM1),
        !is.null(self$DecayRateSOM2),
        !is.null(self$DecayRateSOM3),
        !is.null(self$SpeciesParameters),
        !is.null(self$FireReductionParameters)
      )

      writeLines(
        c(
          insertLandisData(private$.LandisData),
          insertValue("Timestep", self$Timestep),
          insertValue("SeedingAlgorithm", self$SeedingAlgorithm),
          insertFile("InitialCommunitiesCSV", self$InitialCommunitiesCSV),
          insertFile("InitialCommunitiesMap", self$InitialCommunitiesMap),
          insertFile("ClimateConfigFile", self$ClimateConfigFile),
          unlist(lapply(.necnSoilMapKeys, function(k) insertFile(k, self$SoilMaps[[k]]))),
          unlist(lapply(names(self$OptionalClimateMaps %||% list()), function(k) {
            insertFile(k, self$OptionalClimateMaps[[k]])
          })),
          insertValue("CalibrateMode", self$CalibrateMode),
          insertValue("SmokeModelOutputs", self$SmokeModelOutputs),
          insertValue("Write_SWA_Maps", self$Write_SWA_Maps),
          insertValue("Write_CWD_Maps", self$Write_CWD_Maps),
          insertValue("Write_Temperature_Maps", self$Write_Temperature_Maps),
          insertValue("Write_Species_Drought_Maps", self$Write_Species_Drought_Maps),
          insertValue("WaterDecayFunction", self$WaterDecayFunction),
          insertValue("ProbabilityEstablishAdjust", self$ProbabilityEstablishAdjust),
          insertValue("InitialMineralN", self$InitialMineralN),
          insertValue("InitialFineFuels", self$InitialFineFuels),
          insertValue("AtmosphericNSlope", self$AtmosphericNSlope),
          insertValue("AtmosphericNIntercept", self$AtmosphericNIntercept),
          insertValue("Latitude", self$Latitude),
          insertValue("DenitrificationRate", self$DenitrificationRate),
          insertValue("DecayRateSurf", self$DecayRateSurf),
          insertValue("DecayRateSOM1", self$DecayRateSOM1),
          insertValue("DecayRateSOM2", self$DecayRateSOM2),
          insertValue("DecayRateSOM3", self$DecayRateSOM3),
          if (!is.null(self$GrassThresholdMultiplier)) {
            insertValue("GrassThresholdMultiplier", self$GrassThresholdMultiplier)
          },
          unlist(lapply(names(self$OutputMaps %||% list()), function(k) {
            insertValue(k, self$OutputMaps[[k]])
          })),
          ## Only emit CreateInputCommunityMaps when "yes": when "no", the
          ## parser still expects an InputCommunityMapFrequency keyword to
          ## follow, and we don't currently generate one.
          if (isTRUE(self$CreateInputCommunityMaps == "yes")) {
            insertValue("CreateInputCommunityMaps", "yes")
          },
          unlist(lapply(names(self$VariableOverrides %||% list()), function(k) {
            insertValue(k, self$VariableOverrides[[k]])
          })),
          insertFile("SpeciesParameters", self$SpeciesParameters),
          if (!is.null(self$DroughtMortalityParameters)) {
            insertFile("DroughtMortalityParameters", self$DroughtMortalityParameters)
          },
          insertNECNFireReductionParameters(self$FireReductionParameters),
          if (!is.null(self$HarvestReductionParameters)) {
            insertNECNHarvestReductionParameters(self$HarvestReductionParameters)
          }
        ),
        file.path(self$path, self$files[1])
      )

      ## register input files so the scenario can collect them
      self$add_file(self$InitialCommunitiesCSV)
      self$add_file(self$InitialCommunitiesMap)
      self$add_file(self$ClimateConfigFile)
      self$add_file(self$SpeciesParameters)
      if (!is.null(self$DroughtMortalityParameters)) {
        self$add_file(self$DroughtMortalityParameters)
      }
      for (k in .necnSoilMapKeys) {
        self$add_file(self$SoilMaps[[k]])
      }
      for (k in names(self$OptionalClimateMaps %||% list())) {
        self$add_file(self$OptionalClimateMaps[[k]])
      }

      return(invisible(self))
    }
  ),

  private = list(
    .SeedingAlgorithm = NULL,
    .InitialCommunitiesCSV = NULL,
    .InitialCommunitiesMap = NULL,
    .ClimateConfigFile = NULL,
    .SoilMaps = NULL,
    .OptionalClimateMaps = NULL,
    .CalibrateMode = NULL,
    .SmokeModelOutputs = NULL,
    .Write_SWA_Maps = NULL,
    .Write_CWD_Maps = NULL,
    .Write_Temperature_Maps = NULL,
    .Write_Species_Drought_Maps = NULL,
    .WaterDecayFunction = NULL,
    .ProbabilityEstablishAdjust = NULL,
    .InitialMineralN = NULL,
    .InitialFineFuels = NULL,
    .AtmosphericNSlope = NULL,
    .AtmosphericNIntercept = NULL,
    .Latitude = NULL,
    .DenitrificationRate = NULL,
    .DecayRateSurf = NULL,
    .DecayRateSOM1 = NULL,
    .DecayRateSOM2 = NULL,
    .DecayRateSOM3 = NULL,
    .GrassThresholdMultiplier = NULL,
    .OutputMaps = NULL,
    .CreateInputCommunityMaps = NULL,
    .VariableOverrides = NULL,
    .SpeciesParameters = NULL,
    .DroughtMortalityParameters = NULL,
    .FireReductionParameters = NULL,
    .HarvestReductionParameters = NULL
  ),

  active = list(
    #' @template field_SeedingAlgorithm
    SeedingAlgorithm = function(value) {
      if (missing(value)) {
        return(private$.SeedingAlgorithm)
      } else {
        .checkSeedingAlgorithm(value)
        private$.SeedingAlgorithm <- value
      }
    },

    #' @field InitialCommunitiesCSV Character. Relative file path.
    InitialCommunitiesCSV = function(value) {
      if (missing(value)) {
        return(private$.InitialCommunitiesCSV)
      } else {
        private$.InitialCommunitiesCSV <- .relPath(value, self$path)
      }
    },

    #' @field InitialCommunitiesMap Character. Relative file path.
    InitialCommunitiesMap = function(value) {
      if (missing(value)) {
        return(private$.InitialCommunitiesMap)
      } else {
        private$.InitialCommunitiesMap <- .relPath(value, self$path)
      }
    },

    #' @field ClimateConfigFile Character. Relative file path.
    ClimateConfigFile = function(value) {
      if (missing(value)) {
        return(private$.ClimateConfigFile)
      } else {
        private$.ClimateConfigFile <- .relPath(value, self$path)
      }
    },

    #' @field SoilMaps Named list of relative file paths. Required keys: `r .fmtKeys(.necnSoilMapKeys)`.
    SoilMaps = function(value) {
      if (missing(value)) {
        return(private$.SoilMaps)
      } else {
        if (is.null(value)) {
          private$.SoilMaps <- NULL
          return(invisible())
        }
        stopifnot(is.list(value), !is.null(names(value)), all(names(value) %in% .necnSoilMapKeys))
        private$.SoilMaps <- lapply(value, .relPath, start = self$path)
      }
    },

    #' @field OptionalClimateMaps Named list of relative file paths.
    #'   Allowed keys: `r .fmtKeys(.necnOptionalClimateMapKeys)`.
    OptionalClimateMaps = function(value) {
      if (missing(value)) {
        return(private$.OptionalClimateMaps)
      } else {
        if (is.null(value)) {
          private$.OptionalClimateMaps <- NULL
          return(invisible())
        }
        stopifnot(
          is.list(value),
          !is.null(names(value)),
          all(names(value) %in% .necnOptionalClimateMapKeys)
        )
        private$.OptionalClimateMaps <- lapply(value, .relPath, start = self$path)
      }
    },

    #' @field CalibrateMode Logical, or character indicating "yes" or "no".
    CalibrateMode = function(value) {
      if (missing(value)) {
        return(private$.CalibrateMode)
      } else {
        private$.CalibrateMode <- yesno(value)
      }
    },

    #' @field SmokeModelOutputs Logical, or character indicating "yes" or "no".
    SmokeModelOutputs = function(value) {
      if (missing(value)) {
        return(private$.SmokeModelOutputs)
      } else {
        private$.SmokeModelOutputs <- yesno(value)
      }
    },

    #' @field Write_SWA_Maps Logical, or character indicating "yes" or "no".
    Write_SWA_Maps = function(value) {
      if (missing(value)) {
        return(private$.Write_SWA_Maps)
      } else {
        private$.Write_SWA_Maps <- yesno(value)
      }
    },

    #' @field Write_CWD_Maps Logical, or character indicating "yes" or "no".
    Write_CWD_Maps = function(value) {
      if (missing(value)) {
        return(private$.Write_CWD_Maps)
      } else {
        private$.Write_CWD_Maps <- yesno(value)
      }
    },

    #' @field Write_Temperature_Maps Logical, or character indicating "yes" or "no".
    Write_Temperature_Maps = function(value) {
      if (missing(value)) {
        return(private$.Write_Temperature_Maps)
      } else {
        private$.Write_Temperature_Maps <- yesno(value)
      }
    },

    #' @field Write_Species_Drought_Maps Logical, or character indicating "yes" or "no".
    Write_Species_Drought_Maps = function(value) {
      if (missing(value)) {
        return(private$.Write_Species_Drought_Maps)
      } else {
        private$.Write_Species_Drought_Maps <- yesno(value)
      }
    },

    #' @field WaterDecayFunction Character. One of `"Linear"` or `"Ratio"`.
    WaterDecayFunction = function(value) {
      if (missing(value)) {
        return(private$.WaterDecayFunction)
      } else {
        stopifnot(value %in% c("Linear", "Ratio"))
        private$.WaterDecayFunction <- value
      }
    },

    #' @field ProbabilityEstablishAdjust Numeric.
    ProbabilityEstablishAdjust = function(value) {
      if (missing(value)) {
        return(private$.ProbabilityEstablishAdjust)
      } else {
        stopifnot(is.numeric(value), value >= 0)
        private$.ProbabilityEstablishAdjust <- value
      }
    },

    #' @field InitialMineralN Numeric (g m-2).
    InitialMineralN = function(value) {
      if (missing(value)) {
        return(private$.InitialMineralN)
      } else {
        private$.InitialMineralN <- value
      }
    },

    #' @field InitialFineFuels Numeric in `[0, 1]`.
    InitialFineFuels = function(value) {
      if (missing(value)) {
        return(private$.InitialFineFuels)
      } else {
        if (!is.null(value)) {
          stopifnot(value >= 0, value <= 1)
        }
        private$.InitialFineFuels <- value
      }
    },

    #' @field AtmosphericNSlope Numeric.
    AtmosphericNSlope = function(value) {
      if (missing(value)) {
        return(private$.AtmosphericNSlope)
      } else {
        private$.AtmosphericNSlope <- value
      }
    },

    #' @field AtmosphericNIntercept Numeric.
    AtmosphericNIntercept = function(value) {
      if (missing(value)) {
        return(private$.AtmosphericNIntercept)
      } else {
        private$.AtmosphericNIntercept <- value
      }
    },

    #' @field Latitude Numeric (degrees).
    Latitude = function(value) {
      if (missing(value)) {
        return(private$.Latitude)
      } else {
        if (!is.null(value)) {
          stopifnot(value >= -90, value <= 90)
        }
        private$.Latitude <- value
      }
    },

    #' @field DenitrificationRate Numeric in `[0, 1]`.
    DenitrificationRate = function(value) {
      if (missing(value)) {
        return(private$.DenitrificationRate)
      } else {
        if (!is.null(value)) {
          stopifnot(value >= 0, value <= 1)
        }
        private$.DenitrificationRate <- value
      }
    },

    #' @field DecayRateSurf Numeric in `[0, 1]`.
    DecayRateSurf = function(value) {
      if (missing(value)) {
        return(private$.DecayRateSurf)
      } else {
        if (!is.null(value)) {
          stopifnot(value >= 0, value <= 1)
        }
        private$.DecayRateSurf <- value
      }
    },

    #' @field DecayRateSOM1 Numeric in `[0, 1]`.
    DecayRateSOM1 = function(value) {
      if (missing(value)) {
        return(private$.DecayRateSOM1)
      } else {
        if (!is.null(value)) {
          stopifnot(value >= 0, value <= 1)
        }
        private$.DecayRateSOM1 <- value
      }
    },

    #' @field DecayRateSOM2 Numeric in `[0, 1]`.
    DecayRateSOM2 = function(value) {
      if (missing(value)) {
        return(private$.DecayRateSOM2)
      } else {
        if (!is.null(value)) {
          stopifnot(value >= 0, value <= 1)
        }
        private$.DecayRateSOM2 <- value
      }
    },

    #' @field DecayRateSOM3 Numeric in `[0, 1]`.
    DecayRateSOM3 = function(value) {
      if (missing(value)) {
        return(private$.DecayRateSOM3)
      } else {
        if (!is.null(value)) {
          stopifnot(value >= 0, value <= 1)
        }
        private$.DecayRateSOM3 <- value
      }
    },

    #' @field GrassThresholdMultiplier Numeric.
    GrassThresholdMultiplier = function(value) {
      if (missing(value)) {
        return(private$.GrassThresholdMultiplier)
      } else {
        private$.GrassThresholdMultiplier <- value
      }
    },

    #' @field OutputMaps Named list. See table 3 of the user guide.
    #'   Allowed names: `r .fmtKeys(.necnOutputMapKeys)`. `*MapName` values are file-name
    #'   templates and must contain the literal `{timestep}` placeholder
    #'   (user guide §2.29) -- LANDIS-II replaces it with the simulation
    #'   year, e.g. `"NECN/ANPP-{timestep}.tif"`.
    OutputMaps = function(value) {
      if (missing(value)) {
        return(private$.OutputMaps)
      } else {
        if (is.null(value)) {
          private$.OutputMaps <- NULL
          return(invisible())
        }
        stopifnot(is.list(value), !is.null(names(value)), all(names(value) %in% .necnOutputMapKeys))
        for (k in names(value)) {
          if (endsWith(k, "MapName")) {
            stopifnot(
              "OutputMaps `*MapName` values must contain the literal `{timestep}` placeholder." = is.character(value[[
                k
              ]]) &&
                grepl("{timestep}", value[[k]], fixed = TRUE)
            )
          }
        }
        private$.OutputMaps <- value
      }
    },

    #' @field CreateInputCommunityMaps Logical, or character indicating "yes" or "no".
    CreateInputCommunityMaps = function(value) {
      if (missing(value)) {
        return(private$.CreateInputCommunityMaps)
      } else {
        private$.CreateInputCommunityMaps <- yesno(value)
      }
    },

    #' @field VariableOverrides Named list. Allowed names: `r .fmtKeys(.necnVariableOverrideKeys)`.
    VariableOverrides = function(value) {
      if (missing(value)) {
        return(private$.VariableOverrides)
      } else {
        if (is.null(value)) {
          private$.VariableOverrides <- NULL
          return(invisible())
        }
        stopifnot(
          is.list(value),
          !is.null(names(value)),
          all(names(value) %in% .necnVariableOverrideKeys)
        )
        private$.VariableOverrides <- value
      }
    },

    #' @field SpeciesParameters Character. Relative file path.
    SpeciesParameters = function(value) {
      if (missing(value)) {
        return(private$.SpeciesParameters)
      } else {
        private$.SpeciesParameters <- .relPath(value, self$path)
      }
    },

    #' @field DroughtMortalityParameters Character. Relative file path.
    DroughtMortalityParameters = function(value) {
      if (missing(value)) {
        return(private$.DroughtMortalityParameters)
      } else {
        if (is.null(value)) {
          private$.DroughtMortalityParameters <- NULL
        } else {
          private$.DroughtMortalityParameters <- .relPath(value, self$path)
        }
      }
    },

    #' @field FireReductionParameters `data.frame`.
    FireReductionParameters = function(value) {
      if (missing(value)) {
        return(private$.FireReductionParameters)
      } else {
        private$.FireReductionParameters <- value
      }
    },

    #' @field HarvestReductionParameters `data.frame`.
    HarvestReductionParameters = function(value) {
      if (missing(value)) {
        return(private$.HarvestReductionParameters)
      } else {
        private$.HarvestReductionParameters <- value
      }
    }
  )
)

#' Prepare NECN Succession `FireReductionParameters` table
#'
#' Defaults adapted from the LANDIS-II NECN Succession v8 user guide; users
#' should tune to their study system.
#'
#' @param df (Optional) `data.frame` with columns `FireSeverity`,
#'   `CoarseDebrisReduction`, `FineLitterReduction`, `CohortWoodReduction`,
#'   `CohortLeafReduction`, and `SOMReduction`. When `NULL`, a sensible default
#'   table with 5 fire-severity classes is returned.
#'
#' @returns data.frame
#'
#' @family NECN Succession helpers
#'
#' @export
prepNECNFireReductionParameters <- function(df = NULL) {
  if (is.null(df)) {
    df <- data.table(
      FireSeverity = 1L:5L,
      CoarseDebrisReduction = c(0.00, 0.05, 0.20, 0.50, 0.80),
      FineLitterReduction = c(0.10, 0.25, 0.50, 0.75, 1.00),
      CohortWoodReduction = c(0.00, 0.05, 0.20, 0.50, 0.80),
      CohortLeafReduction = c(0.10, 0.25, 0.50, 0.75, 1.00),
      SOMReduction = c(0.00, 0.05, 0.10, 0.15, 0.20)
    )
  } else {
    df <- as.data.table(df)
  }

  df[, FireSeverity := as.integer(FireSeverity)]
  for (col in c(
    "CoarseDebrisReduction",
    "FineLitterReduction",
    "CohortWoodReduction",
    "CohortLeafReduction",
    "SOMReduction"
  )) {
    stopifnot(all(df[[col]] >= 0.0), all(df[[col]] <= 1.0))
  }

  return(as.data.frame(df))
}

#' Specify NECN Succession `FireReductionParameters` table
#'
#' @param df `data.frame` (see [prepNECNFireReductionParameters()]).
#'
#' @template return_insert
#'
#' @family NECN Succession helpers
#'
#' @keywords internal
insertNECNFireReductionParameters <- function(df) {
  stopifnot(
    is.data.frame(df),
    all(
      c(
        "FireSeverity",
        "CoarseDebrisReduction",
        "FineLitterReduction",
        "CohortWoodReduction",
        "CohortLeafReduction",
        "SOMReduction"
      ) %in%
        colnames(df)
    )
  )

  cols <- c(
    "FireSeverity",
    "CoarseDebrisReduction",
    "FineLitterReduction",
    "CohortWoodReduction",
    "CohortLeafReduction",
    "SOMReduction"
  )

  c(
    glue::glue("FireReductionParameters"),
    glue::glue(">>  Severity  CoarseDebris  FineLitter    Cohort        Cohort        SOM"),
    glue::glue(">>  Fire      Reduction     Reduction     WoodReduction LeafReduction Reduction"),
    glue::glue(">>  --------------------------------------------------------------------------"),
    apply(df[, cols, drop = FALSE], 1, function(x) {
      glue::glue_collapse(x, sep = "    ")
    }),
    glue::glue("") ## add blank line after each item group
  )
}

#' Specify NECN Succession `HarvestReductionParameters` table
#'
#' @param df `data.frame` with columns `PrescriptionName`, `DeadWoodReduction`,
#'   `DeadLitterReduction`, `SOMReduction`, `CohortWoodRemoval`, `CohortLeafRemoval`.
#'   `SOMReduction` is required by the runtime NECN parser even though the v8
#'   user guide §2.35 documents only the other four numeric columns.
#'
#' @template return_insert
#'
#' @family NECN Succession helpers
#'
#' @keywords internal
insertNECNHarvestReductionParameters <- function(df) {
  cols <- c(
    "PrescriptionName",
    "DeadWoodReduction",
    "DeadLitterReduction",
    "SOMReduction",
    "CohortWoodRemoval",
    "CohortLeafRemoval"
  )
  stopifnot(is.data.frame(df), all(cols %in% colnames(df)))

  for (col in cols[-1]) {
    stopifnot(all(df[[col]] >= 0.0), all(df[[col]] <= 1.0))
  }

  c(
    glue::glue("HarvestReductionParameters"),
    glue::glue(">>  Name    DeadWood    DeadLitter    SOM         Cohort        Cohort"),
    glue::glue(">>          Reduction   Reduction     Reduction   WoodRemoval   LeafRemoval"),
    glue::glue(">>  --------------------------------------------------------------"),
    apply(df[, cols, drop = FALSE], 1, function(x) {
      glue::glue_collapse(x, sep = "    ")
    }),
    glue::glue("") ## add blank line after each item group
  )
}
