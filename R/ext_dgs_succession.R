#' Required keys in `DGSSuccession$SoilMaps`
#'
#' Soil physical maps and initial soil organic / dead-wood maps required by the
#' DGS Succession extension (§§2.10–2.20 of the user guide).
#'
#' @keywords internal
.dgsSoilMapKeys <- c(
  ## soil physical maps
  "SoilDepthMapName",
  "SoilDrainMapName",
  "SoilBaseFlowMapName",
  "SoilStormFlowMapName",
  "SoilFieldCapacityMapName",
  "SoilWiltingPointMapName",
  "SoilPercentSandMapName",
  "SoilPercentClayMapName",
  "SoilBulkDensityMapName",
  "SoilParticleDensityMapName",
  ## initial soil C/N + dead wood maps
  "InitialSOC_PrimaryMapName",
  "InitialSON_PrimaryMapName",
  "InitialDeadWoodSurfaceMapName",
  "InitialDeadCoarseRootsMapName"
)

#' Scalar parameter keys for the DGS Succession DAMM-McNIP block
#'
#' Scalar parameters required by the DGS Succession parser, in the exact
#' order the parser reads them. Names match the keyword strings in the
#' upstream `InputParameterParser.cs`. The user-guide parameters
#' `SoilMoistureA` and `SoilMoistureB` are intentionally omitted: they are
#' commented out in the parser and not consumed by v2.0. The user-guide
#' name `ActEnergyDOCDepoly` is misnamed; the parser uses `ActEnergyDOCUptake`.
#'
#' @keywords internal
.dgsDammMcNIPKeys <- c(
  "InitialMicrobialC",
  "InitialMicrobialN",
  "InitialEnzymeConc",
  "ActEnergySOMDepoly",
  "ActEnergyDOCUptake",
  "ExpConstSOMDepoly",
  "ExpConstDOCUptake",
  "FractionSOMUnprotect",
  "CNEnzymes",
  "KmSOMDepoly",
  "KmDOCUptake",
  "EnzTurnRate",
  "MicrobialTurnRate",
  "CarbonUseEfficiency",
  "PropEnzymeSOM",
  "PropCEnzymeProduction",
  "PropNEnzymeProduction",
  "FractDeadMicrobialBiomassSOM",
  "MMConstantO2",
  "DiffConstantO2",
  "DiffConstantSOMLiquid",
  "FractionVolumeO2",
  "DOCFraction",
  "DONFraction",
  "FractionLitterToDOC"
)

#' DGS (DAMM-McNiP, GIPL, SHAW) Succession Extension
#'
#' R6 class representing the LANDIS-II DGS Succession (v2.0) extension's
#' main input file. DGS integrates a NECN-style vegetation dynamics model
#' with the DAMM-McNiP soil C/N model, the SHAW physically-based hydrology
#' / energy balance model, and the GIPL deep-soil permafrost model.
#'
#' The DGS extension must be run with Social Climate Fire (v4.01) and the
#' Output Reclass extension (v4.0).
#'
#' @include ext_utils.R
#'
#' @references LANDIS-II DGS Succession v2.0 Extension User Guide
#'   <https://github.com/LANDIS-II-Foundation/Extension-DGS-Succession/blob/master/docs/LANDIS-II%20DGS%20Succession%20v2.0%20User%20Guide.pdf>
#'
#' @seealso
#' Helpers that prepare inputs for this extension:
#' [prepDGSFireReductionParameters()].
#' Shared scenario inputs:
#' [prepClimateConfig()],
#' [prepInitialCommunities()],
#' [prepSpeciesData()].
#'
#' @family DGS Succession helpers
#'
#' @export
DGSSuccession <- R6Class(
  "DGSSuccession",
  inherit = LandisExtension,
  public = list(
    #' @param path Character. Directory path for the extension input files.
    #' @param Timestep Integer (years).
    #' @param CalibrateMode Logical, or character indicating "yes" or "no".
    #'   When `TRUE`, an additional `DGS-calibrate-log.csv` is produced;
    #'   intended for single-cell runs only.
    #' @param ClimateConfigFile Character. Relative file path to the climate
    #'   library configuration file (see §2.4 of the user guide).
    #' @param AtmosphericNSlope Numeric. Slope for the linear N-deposition
    #'   model (`Total N = AtmosphericNSlope * precipitation + AtmosphericNIntercept`).
    #'   The user-guide prose refers to this as `AtmosNslope`, but the
    #'   parser keyword is `AtmosphericNSlope`.
    #' @param AtmosphericNIntercept Numeric. Intercept for the linear
    #'   N-deposition model. The user-guide prose calls this `AtmosNinter`,
    #'   but the parser keyword is `AtmosphericNIntercept`.
    #' @param InitialCommunities Character. Relative file path to the initial
    #'   communities text file (see §7 of the user guide).
    #' @param InitialCommunitiesMap Character. Relative file path to the
    #'   initial communities raster.
    #' @param Latitude Numeric. Study-site latitude (degrees).
    #' @param ShawGiplConfigFile Character. Relative file path to the
    #'   SHAW/GIPL configuration file that names the SHAW/GIPL input files
    #'   (see §3 of the user guide).
    #' @param ShawGiplFiles (Optional) Character vector of additional SHAW/GIPL
    #'   input files (e.g., `ListThus`, `ShawGeneralInputs`, `ShawPlantTypes`,
    #'   `ShawSoilTypes`, `GiplProperties`, `Unfrozen.txt`) to register so
    #'   the scenario can collect them.
    #' @param SoilMaps Named list of relative file paths for the required soil
    #'   physical and initial soil C/N / dead-wood map inputs. Required keys:
    #'   `r .fmtKeys(.dgsSoilMapKeys)`.
    #' @param InitialFineFuels Numeric in `[0, 1]`. Fraction of initial dead
    #'   wood allocated to fine fuels.
    #' @param InitialMineralN Numeric. Initial mineral N ($g/m^2$).
    #' @param DenitrificationRate Numeric in `[0, 1]`. Monthly fraction of
    #'   mineral N lost through volatilization and denitrification.
    #' @param WaterDecayFunction Character. One of `"Linear"` or `"Ratio"`.
    #' @param DammMcNIPParameters Named list of numeric scalar parameters for
    #'   the DAMM-McNiP soil C/N block. Required keys (all must be supplied):
    #'   `r .fmtKeys(.dgsDammMcNIPKeys)`.
    #' @template param_SeedingAlgorithm
    #' @param ProbabilityEstablishAdjust Numeric. Multiplier applied to the
    #'   probability of establishment. Default `1.0`.
    #' @param SpeciesParameters Character. Relative file path to the
    #'   species-parameters CSV (see §2.54 of the user guide).
    #' @param FireReductionParameters `data.frame` with columns
    #'   `FireSeverity`, `CoarseDebrisReduction`, `FineLitterReduction`,
    #'   `CohortWoodReduction`, `CohortLeafReduction`, `OrganicHorizonReduction`.
    #'   Required even when no fire extension is used.
    #' @param HarvestReductionParameters (Optional) `data.frame` with columns
    #'   `PrescriptionName`, `DeadWoodReduction`, `DeadLitterReduction`,
    #'   `SOMReduction`, `CohortWoodRemoval`, `CohortLeafRemoval`. The
    #'   `SOMReduction` column is required by the parser even though the
    #'   user guide (§2.56) does not list it.
    initialize = function(
      path,
      Timestep = 1L,
      CalibrateMode = NULL,
      ClimateConfigFile = NULL,
      AtmosphericNSlope = NULL,
      AtmosphericNIntercept = NULL,
      InitialCommunities = NULL,
      InitialCommunitiesMap = NULL,
      Latitude = NULL,
      ShawGiplConfigFile = NULL,
      ShawGiplFiles = NULL,
      SoilMaps = NULL,
      InitialFineFuels = NULL,
      InitialMineralN = NULL,
      DenitrificationRate = NULL,
      WaterDecayFunction = NULL,
      DammMcNIPParameters = NULL,
      SeedingAlgorithm = NULL,
      ProbabilityEstablishAdjust = 1.0,
      SpeciesParameters = NULL,
      FireReductionParameters = NULL,
      HarvestReductionParameters = NULL
    ) {
      stopifnot(!is.null(path))

      ## LandisExtension fields
      private$.LandisData <- "DGS Succession"
      self$Timestep <- Timestep

      self$type <- "succession"
      self$path <- path
      self$files <- "dgs-succession.txt" ## file won't exist yet

      ## additional fields for this extension
      self$CalibrateMode <- CalibrateMode %||% FALSE
      self$ClimateConfigFile <- ClimateConfigFile
      self$AtmosphericNSlope <- AtmosphericNSlope
      self$AtmosphericNIntercept <- AtmosphericNIntercept
      self$InitialCommunities <- InitialCommunities
      self$InitialCommunitiesMap <- InitialCommunitiesMap
      self$Latitude <- Latitude
      self$ShawGiplConfigFile <- ShawGiplConfigFile
      self$ShawGiplFiles <- ShawGiplFiles
      self$SoilMaps <- SoilMaps
      self$InitialFineFuels <- InitialFineFuels
      self$InitialMineralN <- InitialMineralN
      self$DenitrificationRate <- DenitrificationRate
      self$WaterDecayFunction <- WaterDecayFunction %||% "Linear"
      self$DammMcNIPParameters <- DammMcNIPParameters
      self$SeedingAlgorithm <- SeedingAlgorithm %||% "WardSeedDispersal"
      self$ProbabilityEstablishAdjust <- ProbabilityEstablishAdjust %||% 1.0
      self$SpeciesParameters <- SpeciesParameters
      self$FireReductionParameters <- FireReductionParameters
      self$HarvestReductionParameters <- HarvestReductionParameters
    },

    #' @description Write extension inputs to disk
    write = function() {
      stopifnot(
        !is.null(self$ClimateConfigFile),
        !is.null(self$AtmosphericNSlope),
        !is.null(self$AtmosphericNIntercept),
        !is.null(self$InitialCommunities),
        !is.null(self$InitialCommunitiesMap),
        !is.null(self$Latitude),
        !is.null(self$ShawGiplConfigFile),
        !is.null(self$SoilMaps),
        all(.dgsSoilMapKeys %in% names(self$SoilMaps)),
        !is.null(self$InitialFineFuels),
        !is.null(self$InitialMineralN),
        !is.null(self$DenitrificationRate),
        !is.null(self$DammMcNIPParameters),
        all(.dgsDammMcNIPKeys %in% names(self$DammMcNIPParameters)),
        !is.null(self$SpeciesParameters),
        !is.null(self$FireReductionParameters)
      )

      writeLines(
        c(
          insertLandisData(private$.LandisData),
          insertValue("Timestep", self$Timestep),
          insertValue("CalibrateMode", self$CalibrateMode),
          insertFile("ClimateConfigFile", self$ClimateConfigFile),
          insertValue("AtmosphericNSlope", self$AtmosphericNSlope),
          insertValue("AtmosphericNIntercept", self$AtmosphericNIntercept),
          insertFile("InitialCommunities", self$InitialCommunities),
          insertFile("InitialCommunitiesMap", self$InitialCommunitiesMap),
          insertValue("Latitude", self$Latitude),
          insertFile("ShawGiplConfigFile", self$ShawGiplConfigFile),
          unlist(lapply(.dgsSoilMapKeys, function(k) insertFile(k, self$SoilMaps[[k]]))),
          insertValue("InitialFineFuels", self$InitialFineFuels),
          insertValue("InitialMineralN", self$InitialMineralN),
          insertValue("DenitrificationRate", self$DenitrificationRate),
          insertValue("WaterDecayFunction", self$WaterDecayFunction),
          unlist(lapply(.dgsDammMcNIPKeys, function(k) {
            insertValue(k, self$DammMcNIPParameters[[k]])
          })),
          insertValue("SeedingAlgorithm", self$SeedingAlgorithm),
          insertValue("ProbabilityEstablishAdjust", self$ProbabilityEstablishAdjust),
          insertFile("SpeciesParameters", self$SpeciesParameters),
          insertDGSFireReductionParameters(self$FireReductionParameters),
          if (!is.null(self$HarvestReductionParameters)) {
            insertDGSHarvestReductionParameters(self$HarvestReductionParameters)
          }
        ),
        file.path(self$path, self$files[1])
      )

      ## register input files so the scenario can collect them
      self$add_file(self$ClimateConfigFile)
      self$add_file(self$InitialCommunities)
      self$add_file(self$InitialCommunitiesMap)
      self$add_file(self$ShawGiplConfigFile)
      self$add_file(self$SpeciesParameters)
      for (k in .dgsSoilMapKeys) {
        self$add_file(self$SoilMaps[[k]])
      }
      for (f in self$ShawGiplFiles %||% character(0)) {
        self$add_file(f)
      }

      return(invisible(self))
    }
  ),

  private = list(
    .CalibrateMode = NULL,
    .ClimateConfigFile = NULL,
    .AtmosphericNSlope = NULL,
    .AtmosphericNIntercept = NULL,
    .InitialCommunities = NULL,
    .InitialCommunitiesMap = NULL,
    .Latitude = NULL,
    .ShawGiplConfigFile = NULL,
    .ShawGiplFiles = NULL,
    .SoilMaps = NULL,
    .InitialFineFuels = NULL,
    .InitialMineralN = NULL,
    .DenitrificationRate = NULL,
    .WaterDecayFunction = NULL,
    .DammMcNIPParameters = NULL,
    .SeedingAlgorithm = NULL,
    .ProbabilityEstablishAdjust = NULL,
    .SpeciesParameters = NULL,
    .FireReductionParameters = NULL,
    .HarvestReductionParameters = NULL
  ),

  active = list(
    #' @field CalibrateMode Logical, or character indicating "yes" or "no".
    CalibrateMode = function(value) {
      if (missing(value)) {
        return(private$.CalibrateMode)
      } else {
        private$.CalibrateMode <- yesno(value)
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

    #' @field InitialCommunities Character. Relative file path.
    InitialCommunities = function(value) {
      if (missing(value)) {
        return(private$.InitialCommunities)
      } else {
        private$.InitialCommunities <- .relPath(value, self$path)
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

    #' @field ShawGiplConfigFile Character. Relative file path.
    ShawGiplConfigFile = function(value) {
      if (missing(value)) {
        return(private$.ShawGiplConfigFile)
      } else {
        private$.ShawGiplConfigFile <- .relPath(value, self$path)
      }
    },

    #' @field ShawGiplFiles Character vector of relative file paths.
    ShawGiplFiles = function(value) {
      if (missing(value)) {
        return(private$.ShawGiplFiles)
      } else {
        if (is.null(value)) {
          private$.ShawGiplFiles <- NULL
          return(invisible())
        }
        private$.ShawGiplFiles <- vapply(
          value,
          .relPath,
          character(1),
          start = self$path,
          USE.NAMES = FALSE
        )
      }
    },

    #' @field SoilMaps Named list of relative file paths. Required keys:
    #'   `r .fmtKeys(.dgsSoilMapKeys)`.
    SoilMaps = function(value) {
      if (missing(value)) {
        return(private$.SoilMaps)
      } else {
        if (is.null(value)) {
          private$.SoilMaps <- NULL
          return(invisible())
        }
        stopifnot(is.list(value), !is.null(names(value)), all(names(value) %in% .dgsSoilMapKeys))
        private$.SoilMaps <- lapply(value, .relPath, start = self$path)
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

    #' @field InitialMineralN Numeric (g m-2).
    InitialMineralN = function(value) {
      if (missing(value)) {
        return(private$.InitialMineralN)
      } else {
        private$.InitialMineralN <- value
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

    #' @field WaterDecayFunction Character. One of `"Linear"` or `"Ratio"`.
    WaterDecayFunction = function(value) {
      if (missing(value)) {
        return(private$.WaterDecayFunction)
      } else {
        stopifnot(value %in% c("Linear", "Ratio"))
        private$.WaterDecayFunction <- value
      }
    },

    #' @field DammMcNIPParameters Named list of numeric scalar parameters.
    #'   Required keys: `r .fmtKeys(.dgsDammMcNIPKeys)`.
    DammMcNIPParameters = function(value) {
      if (missing(value)) {
        return(private$.DammMcNIPParameters)
      } else {
        if (is.null(value)) {
          private$.DammMcNIPParameters <- NULL
          return(invisible())
        }
        stopifnot(is.list(value), !is.null(names(value)), all(names(value) %in% .dgsDammMcNIPKeys))
        private$.DammMcNIPParameters <- value
      }
    },

    #' @template field_SeedingAlgorithm
    SeedingAlgorithm = function(value) {
      if (missing(value)) {
        return(private$.SeedingAlgorithm)
      } else {
        .checkSeedingAlgorithm(value)
        private$.SeedingAlgorithm <- value
      }
    },

    #' @field ProbabilityEstablishAdjust Numeric. Default `1.0`.
    ProbabilityEstablishAdjust = function(value) {
      if (missing(value)) {
        return(private$.ProbabilityEstablishAdjust)
      } else {
        stopifnot(is.numeric(value), value >= 0)
        private$.ProbabilityEstablishAdjust <- value
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

#' Default DGS Succession DAMM-McNiP scalar parameters
#'
#' Returns a named list with a default value for each of the required
#' DAMM-McNiP scalar parameters (`r .fmtKeys(.dgsDammMcNIPKeys)`),
#' intended as a starting point for calibration. Users should tune these
#' values to their study system; defaults are taken from the upstream
#' LANDIS-II DGS v2.0 single-cell Alaska test input
#' (`testing/Core8-DGS_version2.0/DGS_Succession_AKinput_020725.txt`).
#'
#' @returns Named list of numeric values.
#'
#' @family DGS Succession helpers
#'
#' @export
defaultDGSDammMcNIPParameters <- function() {
  list(
    InitialMicrobialC = 1.9703,
    InitialMicrobialN = 0.197,
    InitialEnzymeConc = 0.0339,
    ActEnergySOMDepoly = 66.2675487981142,
    ActEnergyDOCUptake = 67.8225452963818,
    ExpConstSOMDepoly = 116986794418,
    ExpConstDOCUptake = 115130563903,
    FractionSOMUnprotect = 0.0004048,
    CNEnzymes = 2.86940,
    KmSOMDepoly = 0.00249,
    KmDOCUptake = 0.33217,
    EnzTurnRate = 0.00095780,
    MicrobialTurnRate = 0.0001437,
    CarbonUseEfficiency = 0.357329,
    PropEnzymeSOM = 0.522748,
    PropCEnzymeProduction = 0.551334,
    PropNEnzymeProduction = 0.50446,
    FractDeadMicrobialBiomassSOM = 0.551334,
    MMConstantO2 = 0.128,
    DiffConstantO2 = 1.676,
    DiffConstantSOMLiquid = 3.3386,
    FractionVolumeO2 = 0.205165,
    DOCFraction = 0.0016669,
    DONFraction = 0.0016669,
    FractionLitterToDOC = 0.066
  )
}

#' Prepare DGS Succession `FireReductionParameters` table
#'
#' Defaults adapted from the LANDIS-II DGS Succession v2.0 user guide; users
#' should tune to their study system.
#'
#' @param df (Optional) `data.frame` with columns `FireSeverity`,
#'   `CoarseDebrisReduction`, `FineLitterReduction`, `CohortWoodReduction`,
#'   `CohortLeafReduction`, and `OrganicHorizonReduction`. When `NULL`, a
#'   default table with 5 fire-severity classes is returned.
#'
#' @returns data.frame
#'
#' @family DGS Succession helpers
#'
#' @export
prepDGSFireReductionParameters <- function(df = NULL) {
  if (is.null(df)) {
    df <- data.table(
      FireSeverity = 1L:5L,
      CoarseDebrisReduction = c(0.00, 0.05, 0.20, 0.50, 0.80),
      FineLitterReduction = c(0.10, 0.25, 0.50, 0.75, 1.00),
      CohortWoodReduction = c(0.00, 0.05, 0.20, 0.50, 0.80),
      CohortLeafReduction = c(0.10, 0.25, 0.50, 0.75, 1.00),
      OrganicHorizonReduction = c(0.00, 0.05, 0.10, 0.15, 0.20)
    )
  } else {
    df <- as.data.table(df)
  }

  df[, FireSeverity := as.integer(FireSeverity)]
  stopifnot(all(df[["FireSeverity"]] >= 1L), all(df[["FireSeverity"]] <= 10L))
  for (col in c(
    "CoarseDebrisReduction",
    "FineLitterReduction",
    "CohortWoodReduction",
    "CohortLeafReduction",
    "OrganicHorizonReduction"
  )) {
    stopifnot(all(df[[col]] >= 0.0), all(df[[col]] <= 1.0))
  }

  return(as.data.frame(df))
}

#' Specify DGS Succession `FireReductionParameters` table
#'
#' @param df `data.frame` (see [prepDGSFireReductionParameters()]).
#'
#' @template return_insert
#'
#' @family DGS Succession helpers
#'
#' @keywords internal
insertDGSFireReductionParameters <- function(df) {
  cols <- c(
    "FireSeverity",
    "CoarseDebrisReduction",
    "FineLitterReduction",
    "CohortWoodReduction",
    "CohortLeafReduction",
    "OrganicHorizonReduction"
  )
  stopifnot(is.data.frame(df), all(cols %in% colnames(df)))

  c(
    glue::glue("FireReductionParameters"),
    glue::glue(
      ">>  Severity  CoarseDebris  FineLitter    Cohort        Cohort        OrganicHorizon"
    ),
    glue::glue(">>  Fire      Reduction     Reduction     WoodReduction LeafReduction Reduction"),
    glue::glue(">>  ---------------------------------------------------------------------------"),
    apply(df[, cols, drop = FALSE], 1, function(x) {
      glue::glue_collapse(x, sep = "    ")
    }),
    glue::glue("") ## add blank line after each item group
  )
}

#' Specify DGS Succession `HarvestReductionParameters` table
#'
#' The DGS parser reads six columns per row: prescription name, coarse
#' (dead) wood reduction, fine (dead) litter reduction, SOM reduction,
#' cohort wood removal, and cohort leaf removal. The `SOMReduction`
#' column is required even though it is not listed in user-guide
#' §2.56.
#'
#' @param df `data.frame` with columns `PrescriptionName`, `DeadWoodReduction`,
#'   `DeadLitterReduction`, `SOMReduction`, `CohortWoodRemoval`,
#'   `CohortLeafRemoval`.
#'
#' @template return_insert
#'
#' @family DGS Succession helpers
#'
#' @keywords internal
insertDGSHarvestReductionParameters <- function(df) {
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
    glue::glue(">>  Name    DeadWood    DeadLitter    SOM          Cohort        Cohort"),
    glue::glue(">>          Reduction   Reduction     Reduction    WoodRemoval   LeafRemoval"),
    glue::glue(">>  --------------------------------------------------------------------------"),
    apply(df[, cols, drop = FALSE], 1, function(x) {
      glue::glue_collapse(x, sep = "    ")
    }),
    glue::glue("") ## add blank line after each item group
  )
}
