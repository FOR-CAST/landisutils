#' Allowed `PnETGenericParameters` keys
#'
#' Names of generic PnET parameters that may appear in a `PnETGenericParameters`
#' input file (see §7 of the LANDIS-II PnET-Succession v6.0 user guide).
#' Any parameter listed here may instead be set in the
#' `PnETGenericDefaultParameters.txt` file shipped with the extension; values
#' supplied here override the defaults.
#'
#' @keywords internal
.pnetGenericParameterKeys <- c(
  "MaxCanopyLayers",
  "LayerThreshRatio",
  "PARunits",
  "IMAX",
  "DVPD1",
  "DVPD2",
  "BFolResp",
  "MaintResp",
  "TOroot",
  "TOwood",
  "Q10",
  "FolLignin",
  "KWdLit",
  "InitialNSC",
  "CFracBiomass",
  "PrecipEvents",
  "PrecipEventsWithReplacement",
  "ETExtCoeff",
  "RETCropCoeff",
  "PreventEstablishment",
  "Wythers",
  "DTEMP",
  "MaxPest",
  "AmaxFrac",
  "InvertPest",
  "SoilIceDepth",
  "LeakageFrostDepth",
  "FrostFactor",
  "InitialCommunitiesSpinup",
  "SpinupWaterStress",
  "Parallel",
  "CohortStacking",
  "CanopySumScale",
  "CO2HalfSatEff"
)

#' Required `PnETSpeciesParameters` columns
#'
#' Required columns of the `PnETSpeciesParameters` input file (§8 of the
#' user guide). The first column gives the species name; remaining columns are
#' physiological parameters that vary by species.
#'
#' @keywords internal
.pnetSpeciesRequiredCols <- c(
  "SpeciesCode",
  "FolN",
  "SLWmax",
  "SLWDel",
  "TOfol",
  "AmaxA",
  "AmaxB",
  "HalfSat",
  "H3",
  "H4",
  "PsnAgeRed",
  "PsnTMin",
  "PsnTOpt",
  "k",
  "FracFol",
  "FrActWd"
)

#' Optional `PnETSpeciesParameters` columns
#'
#' Optional columns of the `PnETSpeciesParameters` input file (§8).
#' Many of these parameters can alternatively be supplied as generic values
#' in the `PnETGenericParameters` file; species-level values always take
#' precedence.
#'
#' @keywords internal
.pnetSpeciesOptionalCols <- c(
  "MaxLAI",
  "TORoot",
  "TOWood",
  "H1",
  "H2",
  "LeafOnMinT",
  "PsnTMax",
  "ColdTol",
  "DNSC",
  "FracBelowG",
  "EstMoist",
  "EstRad",
  "CO2HalfSatEff",
  "CO2AMaxBEff",
  "O3StomataSens",
  "O3GrowthSens",
  "MaxFolN",
  "FolNShape",
  "MaxFracFol",
  "FracFolShape",
  "LifeForm",
  "MossScalar"
)

#' Required `EcoregionParameters` columns (PnET-Succession)
#'
#' Required columns of the PnET-Succession ecoregion-parameters input file
#' (§9 of the user guide). The first column gives the ecoregion name.
#' `ClimateFileName` may be omitted if the climate library is used (i.e., when
#' `ClimateConfigFile` is supplied at the top level).
#'
#' @keywords internal
.pnetEcoregionRequiredCols <- c(
  "EcoregionName",
  "SoilType",
  "RootingDepth",
  "PrecLossFrac",
  "EvapDepth",
  "LeakageFrac",
  "PrecIntConst",
  "SnowSublimFrac"
)

#' Optional `EcoregionParameters` columns (PnET-Succession)
#'
#' Optional columns of the PnET-Succession ecoregion-parameters input file.
#'
#' @keywords internal
.pnetEcoregionOptionalCols <- c(
  "Latitude",
  "ClimateFileName",
  "RunoffCapture",
  "MossDepth",
  "WinterSTD"
)

#' Disturbance pool reductions used in `DisturbanceReductions`
#'
#' Rows of the PnET-Succession `DisturbanceReductions` table (§10 of the
#' user guide). Each row specifies how a disturbance type partitions biomass
#' into / out of the dead pools.
#'
#' @keywords internal
.pnetDisturbancePools <- c(
  "WoodReduction",
  "FolReduction",
  "RootReduction",
  "DeadWoodReduction",
  "LitterReduction"
)

#' PnET-Succession Extension
#'
#' Builds the LANDIS-II PnET-Succession v6.0 extension input file
#' (compatible with LANDIS-II v8). The top-level `PnET-Succession` parameters
#' are written via this class' `$write()` method; auxiliary inputs (PnET
#' generic / species / ecoregion parameters, disturbance reductions,
#' Saxton-and-Rawls soil parameters, climate, and PnET output sites) are
#' written by the companion `prep*()` helpers.
#'
#' @include ext_utils.R
#'
#' @references LANDIS-II PnET-Succession v6.0 Extension User Guide
#'   <https://github.com/LANDIS-II-Foundation/Extension-PnET-Succession/blob/master/deploy/docs/LANDIS-II%20PnET-Succession%20v6.0%20User%20Guide%20Jan21%202026.pdf>
#'
#' @seealso
#' Helpers that prepare inputs for this extension:
#' [prepPnETGenericParameters()],
#' [prepPnETSpeciesParameters()],
#' [prepPnETEcoregionParameters()],
#' [prepPnETDisturbanceReductions()],
#' [prepPnETClimateFile()],
#' [prepPNEToutputsites()].
#' Shared scenario inputs:
#' [prepClimateConfig()],
#' [prepInitialCommunities()].
#'
#' @family PnET Succession helpers
#'
#' @export
PnETSuccession <- R6Class(
  "PnETSuccession",
  inherit = LandisExtension,
  public = list(
    #' @param path Character. Directory path.
    #' @param Timestep Integer. Succession timestep in years (recommended <= 10).
    #' @param StartYear Integer. Climate year in which the simulation begins.
    #' @template param_SeedingAlgorithm
    #' @param Latitude (Optional) Numeric. Global study-site latitude in degrees.
    #'   Can alternatively be set per-ecoregion in the `EcoregionParameters` file.
    #' @param PNEToutputsites (Optional) Character. Relative file path to the
    #'   `PNEToutputsites` input file (see §12 of the user guide).
    #' @param InitialCommunities Character. Relative file path to the
    #'   initial communities CSV (see §4 of the user guide).
    #' @param InitialCommunitiesMap Character. Relative file path to the
    #'   initial communities raster.
    #' @param LitterMap (Optional) Character. Relative file path to the
    #'   initial leaf litter map ($g/m^2$).
    #' @param WoodyDebrisMap (Optional) Character. Relative file path to the
    #'   initial dead woody debris map ($g/m^2$).
    #' @param PnETGenericParameters (Optional) Character. Relative file path
    #'   to the PnET generic parameters file (§7).
    #' @param PnETSpeciesParameters Character. Relative file path to the PnET
    #'   species parameters file (§8).
    #' @param EcoregionParameters Character. Relative file path to the PnET
    #'   ecoregion parameters file (§9).
    #' @param DisturbanceReductions (Optional) Character. Relative file path
    #'   to the disturbance reductions file (§10).
    #' @param ClimateConfigFile (Optional) Character. Relative file path to
    #'   the LANDIS-II climate library configuration file. When omitted, the
    #'   climate file(s) referenced from `EcoregionParameters` are used.
    #' @param SaxtonAndRawlsParameters (Optional) Character. Relative file
    #'   path to an override Saxton-and-Rawls soil parameter file.
    #' @param CohortBinSize (Optional) Integer. Number of years represented
    #'   by an age cohort; must be `>= Timestep`.
    initialize = function(
      path = NULL,
      Timestep = 10L,
      StartYear = NULL,
      SeedingAlgorithm = NULL,
      Latitude = NULL,
      PNEToutputsites = NULL,
      InitialCommunities = NULL,
      InitialCommunitiesMap = NULL,
      LitterMap = NULL,
      WoodyDebrisMap = NULL,
      PnETGenericParameters = NULL,
      PnETSpeciesParameters = NULL,
      EcoregionParameters = NULL,
      DisturbanceReductions = NULL,
      ClimateConfigFile = NULL,
      SaxtonAndRawlsParameters = NULL,
      CohortBinSize = NULL
    ) {
      stopifnot(!is.null(path))

      ## LandisExtension fields
      private$.LandisData <- "PnET-Succession"
      self$Timestep <- Timestep

      self$type <- "succession"
      self$path <- path
      self$files <- "pnet-succession.txt" ## file won't exist yet

      ## additional fields for this extension
      self$StartYear <- StartYear
      self$SeedingAlgorithm <- SeedingAlgorithm %||% "WardSeedDispersal"
      self$Latitude <- Latitude
      self$PNEToutputsites <- PNEToutputsites
      self$InitialCommunities <- InitialCommunities
      self$InitialCommunitiesMap <- InitialCommunitiesMap
      self$LitterMap <- LitterMap
      self$WoodyDebrisMap <- WoodyDebrisMap
      self$PnETGenericParameters <- PnETGenericParameters
      self$PnETSpeciesParameters <- PnETSpeciesParameters
      self$EcoregionParameters <- EcoregionParameters
      self$DisturbanceReductions <- DisturbanceReductions
      self$ClimateConfigFile <- ClimateConfigFile
      self$SaxtonAndRawlsParameters <- SaxtonAndRawlsParameters
      self$CohortBinSize <- CohortBinSize
    },

    #' @description Write extension inputs to disk
    write = function() {
      stopifnot(
        !is.null(self$StartYear),
        !is.null(self$SeedingAlgorithm),
        !is.null(self$InitialCommunities),
        !is.null(self$InitialCommunitiesMap),
        !is.null(self$PnETSpeciesParameters),
        !is.null(self$EcoregionParameters)
      )

      writeLines(
        c(
          insertLandisData(private$.LandisData),
          insertValue("Timestep", self$Timestep),
          insertValue("StartYear", self$StartYear),
          insertValue("SeedingAlgorithm", self$SeedingAlgorithm),
          if (!is.null(self$Latitude)) insertValue("Latitude", self$Latitude),
          if (!is.null(self$PNEToutputsites)) {
            insertFile("PNEToutputsites", self$PNEToutputsites)
          },
          insertFile("InitialCommunities", self$InitialCommunities),
          insertFile("InitialCommunitiesMap", self$InitialCommunitiesMap),
          if (!is.null(self$LitterMap)) insertFile("LitterMap", self$LitterMap),
          if (!is.null(self$WoodyDebrisMap)) {
            insertFile("WoodyDebrisMap", self$WoodyDebrisMap)
          },
          if (!is.null(self$PnETGenericParameters)) {
            insertFile("PnETGenericParameters", self$PnETGenericParameters)
          },
          insertFile("PnETSpeciesParameters", self$PnETSpeciesParameters),
          insertFile("EcoregionParameters", self$EcoregionParameters),
          if (!is.null(self$DisturbanceReductions)) {
            insertFile("DisturbanceReductions", self$DisturbanceReductions)
          },
          if (!is.null(self$ClimateConfigFile)) {
            insertFile("ClimateConfigFile", self$ClimateConfigFile)
          },
          if (!is.null(self$SaxtonAndRawlsParameters)) {
            insertFile("SaxtonAndRawlsParameters", self$SaxtonAndRawlsParameters)
          },
          if (!is.null(self$CohortBinSize)) {
            insertValue("CohortBinSize", self$CohortBinSize)
          }
        ),
        file.path(self$path, self$files[1])
      )

      ## register input files so the scenario can collect them
      self$add_file(self$InitialCommunities)
      self$add_file(self$InitialCommunitiesMap)
      if (!is.null(self$PNEToutputsites)) {
        self$add_file(self$PNEToutputsites)
      }
      if (!is.null(self$LitterMap)) {
        self$add_file(self$LitterMap)
      }
      if (!is.null(self$WoodyDebrisMap)) {
        self$add_file(self$WoodyDebrisMap)
      }
      if (!is.null(self$PnETGenericParameters)) {
        self$add_file(self$PnETGenericParameters)
      }
      self$add_file(self$PnETSpeciesParameters)
      self$add_file(self$EcoregionParameters)
      if (!is.null(self$DisturbanceReductions)) {
        self$add_file(self$DisturbanceReductions)
      }
      if (!is.null(self$ClimateConfigFile)) {
        self$add_file(self$ClimateConfigFile)
      }
      if (!is.null(self$SaxtonAndRawlsParameters)) {
        self$add_file(self$SaxtonAndRawlsParameters)
      }

      return(invisible(self))
    }
  ),

  private = list(
    .StartYear = NULL,
    .SeedingAlgorithm = NULL,
    .Latitude = NULL,
    .PNEToutputsites = NULL,
    .InitialCommunities = NULL,
    .InitialCommunitiesMap = NULL,
    .LitterMap = NULL,
    .WoodyDebrisMap = NULL,
    .PnETGenericParameters = NULL,
    .PnETSpeciesParameters = NULL,
    .EcoregionParameters = NULL,
    .DisturbanceReductions = NULL,
    .ClimateConfigFile = NULL,
    .SaxtonAndRawlsParameters = NULL,
    .CohortBinSize = NULL
  ),

  active = list(
    #' @field StartYear Integer. Climate year in which the simulation begins.
    StartYear = function(value) {
      if (missing(value)) {
        return(private$.StartYear)
      } else {
        if (is.null(value)) {
          private$.StartYear <- NULL
          return(invisible())
        }
        stopifnot(as.integer(value) > 0L)
        private$.StartYear <- as.integer(value)
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

    #' @field Latitude Numeric (optional). Degrees.
    Latitude = function(value) {
      if (missing(value)) {
        return(private$.Latitude)
      } else {
        if (is.null(value)) {
          private$.Latitude <- NULL
          return(invisible())
        }
        stopifnot(is.numeric(value), value >= -90, value <= 90)
        private$.Latitude <- value
      }
    },

    #' @field PNEToutputsites Character (optional). Relative file path.
    PNEToutputsites = function(value) {
      if (missing(value)) {
        return(private$.PNEToutputsites)
      } else {
        private$.PNEToutputsites <- if (is.null(value)) NULL else .relPath(value, self$path)
      }
    },

    #' @field InitialCommunities Character. Relative file path.
    InitialCommunities = function(value) {
      if (missing(value)) {
        return(private$.InitialCommunities)
      } else {
        private$.InitialCommunities <- if (is.null(value)) NULL else .relPath(value, self$path)
      }
    },

    #' @field InitialCommunitiesMap Character. Relative file path.
    InitialCommunitiesMap = function(value) {
      if (missing(value)) {
        return(private$.InitialCommunitiesMap)
      } else {
        private$.InitialCommunitiesMap <- if (is.null(value)) NULL else .relPath(value, self$path)
      }
    },

    #' @field LitterMap Character (optional). Relative file path.
    LitterMap = function(value) {
      if (missing(value)) {
        return(private$.LitterMap)
      } else {
        private$.LitterMap <- if (is.null(value)) NULL else .relPath(value, self$path)
      }
    },

    #' @field WoodyDebrisMap Character (optional). Relative file path.
    WoodyDebrisMap = function(value) {
      if (missing(value)) {
        return(private$.WoodyDebrisMap)
      } else {
        private$.WoodyDebrisMap <- if (is.null(value)) NULL else .relPath(value, self$path)
      }
    },

    #' @field PnETGenericParameters Character (optional). Relative file path.
    PnETGenericParameters = function(value) {
      if (missing(value)) {
        return(private$.PnETGenericParameters)
      } else {
        private$.PnETGenericParameters <- if (is.null(value)) NULL else .relPath(value, self$path)
      }
    },

    #' @field PnETSpeciesParameters Character. Relative file path.
    PnETSpeciesParameters = function(value) {
      if (missing(value)) {
        return(private$.PnETSpeciesParameters)
      } else {
        private$.PnETSpeciesParameters <- if (is.null(value)) NULL else .relPath(value, self$path)
      }
    },

    #' @field EcoregionParameters Character. Relative file path.
    EcoregionParameters = function(value) {
      if (missing(value)) {
        return(private$.EcoregionParameters)
      } else {
        private$.EcoregionParameters <- if (is.null(value)) NULL else .relPath(value, self$path)
      }
    },

    #' @field DisturbanceReductions Character (optional). Relative file path.
    DisturbanceReductions = function(value) {
      if (missing(value)) {
        return(private$.DisturbanceReductions)
      } else {
        private$.DisturbanceReductions <- if (is.null(value)) NULL else .relPath(value, self$path)
      }
    },

    #' @field ClimateConfigFile Character (optional). Relative file path.
    ClimateConfigFile = function(value) {
      if (missing(value)) {
        return(private$.ClimateConfigFile)
      } else {
        private$.ClimateConfigFile <- if (is.null(value)) NULL else .relPath(value, self$path)
      }
    },

    #' @field SaxtonAndRawlsParameters Character (optional). Relative file path.
    SaxtonAndRawlsParameters = function(value) {
      if (missing(value)) {
        return(private$.SaxtonAndRawlsParameters)
      } else {
        private$.SaxtonAndRawlsParameters <- if (is.null(value)) {
          NULL
        } else {
          .relPath(value, self$path)
        }
      }
    },

    #' @field CohortBinSize Integer (optional). Must be `>= Timestep`.
    CohortBinSize = function(value) {
      if (missing(value)) {
        return(private$.CohortBinSize)
      } else {
        if (is.null(value)) {
          private$.CohortBinSize <- NULL
          return(invisible())
        }
        stopifnot(as.integer(value) >= self$Timestep)
        private$.CohortBinSize <- as.integer(value)
      }
    }
  )
)

#' Prepare PnET-Succession Generic Parameters file
#'
#' Writes a `PnETGenericParameters`-format text file (§7 of the
#' user guide). Any parameter omitted falls back to the defaults provided in
#' `PnETGenericDefaultParameters.txt` shipped with the extension. Allowed
#' parameter names: `r .fmtKeys(.pnetGenericParameterKeys)`.
#'
#' @param params Named list (or two-column `data.frame` with columns `name` and
#'   `value`) of generic-parameter settings. Names must be drawn from the
#'   allowed parameter list above. Boolean values are coerced via [yesno()].
#'
#' @template param_path
#'
#' @template param_filename
#'
#' @template return_file
#'
#' @family PnET Succession helpers
#'
#' @export
prepPnETGenericParameters <- function(params = NULL, path, filename = "PnETGenericParameters.txt") {
  if (is.data.frame(params)) {
    stopifnot(ncol(params) == 2L)
    nm <- as.character(params[[1L]])
    vl <- params[[2L]]
    params <- stats::setNames(as.list(vl), nm)
  }

  stopifnot(is.list(params), !is.null(names(params)))
  unknown <- setdiff(names(params), .pnetGenericParameterKeys)
  if (length(unknown) > 0L) {
    stop(
      "unknown PnETGenericParameters: ",
      paste(unknown, collapse = ", "),
      ". Allowed: ",
      paste(.pnetGenericParameterKeys, collapse = ", ")
    )
  }

  rendered <- vapply(
    names(params),
    function(k) {
      v <- params[[k]]
      if (is.logical(v)) {
        v <- yesno(v)
      }
      paste0(k, "  ", format(v, scientific = FALSE))
    },
    character(1),
    USE.NAMES = FALSE
  )

  file <- file.path(path, filename)
  writeLines(
    c(
      insertLandisData("PnETGenericParameters"),
      "PnETGenericParameters Value",
      ">>----------------------------",
      rendered,
      ""
    ),
    file
  )

  return(file)
}

#' Prepare PnET-Succession Species Parameters file
#'
#' Writes the PnET species parameters file (§8 of the user guide). One
#' row per species. Required columns are listed in
#' [.pnetSpeciesRequiredCols]; allowed optional columns are listed in
#' [.pnetSpeciesOptionalCols]. Unknown columns trigger an error.
#'
#' @param df `data.frame` with one row per species. The first column must be
#'   `SpeciesCode` (species name as it appears in the LANDIS-II species file).
#'
#' @template param_path
#'
#' @template param_filename
#'
#' @template return_file
#'
#' @family PnET Succession helpers
#'
#' @export
prepPnETSpeciesParameters <- function(df = NULL, path, filename = "PnETSpeciesParameters.txt") {
  stopifnot(is.data.frame(df), nrow(df) > 0L)

  cols <- colnames(df)
  missing_required <- setdiff(.pnetSpeciesRequiredCols, cols)
  if (length(missing_required) > 0L) {
    stop(
      "missing required PnETSpeciesParameters columns: ",
      paste(missing_required, collapse = ", ")
    )
  }
  unknown <- setdiff(cols, c(.pnetSpeciesRequiredCols, .pnetSpeciesOptionalCols))
  if (length(unknown) > 0L) {
    stop(
      "unknown PnETSpeciesParameters columns: ",
      paste(unknown, collapse = ", "),
      ". Allowed: ",
      paste(c(.pnetSpeciesRequiredCols, .pnetSpeciesOptionalCols), collapse = ", ")
    )
  }

  ## SpeciesCode first; remaining columns in df-supplied order
  cols <- c("SpeciesCode", setdiff(cols, "SpeciesCode"))
  df <- df[, cols, drop = FALSE]

  file <- file.path(path, filename)
  writeLines(
    c(
      insertLandisData("PnETSpeciesParameters"),
      paste("PnETSpeciesParameters", paste(cols[-1L], collapse = "  ")),
      paste(">>", paste(rep("----------", length(cols)), collapse = "  ")),
      apply(df, 1L, function(x) paste(x, collapse = "  ")),
      ""
    ),
    file
  )

  return(file)
}

#' Prepare PnET-Succession Ecoregion Parameters file
#'
#' Writes the PnET ecoregion parameters file (§9 of the user guide).
#' One row per active ecoregion. Required columns are listed in
#' [.pnetEcoregionRequiredCols]; allowed optional columns are listed in
#' [.pnetEcoregionOptionalCols]. `ClimateFileName` is required when the
#' top-level `ClimateConfigFile` is not in use.
#'
#' @param df `data.frame` with one row per ecoregion. The first column must be
#'   `EcoregionName` (matching the ecoregion names in the LANDIS-II ecoregion
#'   input file).
#'
#' @template param_path
#'
#' @template param_filename
#'
#' @template return_file
#'
#' @family PnET Succession helpers
#'
#' @export
prepPnETEcoregionParameters <- function(df = NULL, path, filename = "EcoregionParameters.txt") {
  stopifnot(is.data.frame(df), nrow(df) > 0L)

  cols <- colnames(df)
  missing_required <- setdiff(.pnetEcoregionRequiredCols, cols)
  if (length(missing_required) > 0L) {
    stop("missing required EcoregionParameters columns: ", paste(missing_required, collapse = ", "))
  }
  unknown <- setdiff(cols, c(.pnetEcoregionRequiredCols, .pnetEcoregionOptionalCols))
  if (length(unknown) > 0L) {
    stop(
      "unknown EcoregionParameters columns: ",
      paste(unknown, collapse = ", "),
      ". Allowed: ",
      paste(c(.pnetEcoregionRequiredCols, .pnetEcoregionOptionalCols), collapse = ", ")
    )
  }

  cols <- c("EcoregionName", setdiff(cols, "EcoregionName"))
  df <- df[, cols, drop = FALSE]

  file <- file.path(path, filename)
  writeLines(
    c(
      insertLandisData("EcoregionParameters"),
      paste("EcoregionParameters", paste(cols[-1L], collapse = "  ")),
      paste(">>", paste(rep("----------", length(cols)), collapse = "  ")),
      apply(df, 1L, function(x) paste(x, collapse = "  ")),
      ""
    ),
    file
  )

  return(file)
}

#' Default PnET-Succession `DisturbanceReductions` table
#'
#' Returns a sensible default [data.frame] for the PnET-Succession
#' `DisturbanceReductions` input file with columns for the four most common
#' disturbance types (`fire`, `wind`, `harvest`, `bda`). Values are taken from the
#' example in §10.1 of the user guide and should be tuned to the
#' study system. The first column (`Pool`) names the dead-pool reduction,
#' drawn from [.pnetDisturbancePools].
#'
#' @returns `data.frame`
#'
#' @family PnET Succession helpers
#'
#' @export
defaultPnETDisturbanceReductions <- function() {
  data.frame(
    Pool = .pnetDisturbancePools,
    fire = c(0.33, 1.00, 0.00, 0.70, 0.90),
    wind = c(0.00, 0.00, 0.00, 0.00, 0.00),
    harvest = c(0.70, 0.00, 0.00, 0.00, 0.10),
    bda = c(0.00, 0.00, 0.00, 0.00, 0.00),
    stringsAsFactors = FALSE
  )
}

#' Prepare PnET-Succession Disturbance Reductions file
#'
#' Writes the disturbance reductions file (§10 of the user guide),
#' which specifies how each disturbance partitions biomass into / out of the
#' dead pools. Rows correspond to the five reductions in
#' [.pnetDisturbancePools]; remaining columns are the disturbance types
#' active in the scenario (e.g. `fire`, `wind`, `harvest`, `bda`).
#'
#' @param df `data.frame` whose first column is `Pool` (with values in
#'   [.pnetDisturbancePools]) and whose remaining columns are disturbance
#'   types. Use [defaultPnETDisturbanceReductions()] for a starting point.
#'   Reduction values must be in `[0, 1]`.
#'
#' @template param_path
#'
#' @template param_filename
#'
#' @template return_file
#'
#' @family PnET Succession helpers
#'
#' @export
prepPnETDisturbanceReductions <- function(df = NULL, path, filename = "DisturbanceReductions.txt") {
  if (is.null(df)) {
    df <- defaultPnETDisturbanceReductions()
  }

  stopifnot(
    is.data.frame(df),
    "Pool" %in% colnames(df),
    ncol(df) >= 2L,
    all(df$Pool %in% .pnetDisturbancePools),
    all(.pnetDisturbancePools %in% df$Pool)
  )

  disturb_cols <- setdiff(colnames(df), "Pool")
  for (col in disturb_cols) {
    stopifnot(all(df[[col]] >= 0.0), all(df[[col]] <= 1.0))
  }

  ## ensure rows are in canonical pool order
  df <- df[match(.pnetDisturbancePools, df$Pool), c("Pool", disturb_cols), drop = FALSE]

  file <- file.path(path, filename)
  writeLines(
    c(
      insertLandisData("DisturbanceReductions"),
      paste("DisturbanceReductions", paste(disturb_cols, collapse = "  ")),
      paste(">>", paste(rep("----------", ncol(df)), collapse = "  ")),
      apply(df, 1L, function(x) paste(x, collapse = "  ")),
      ""
    ),
    file
  )

  return(file)
}

#' Prepare PnET-Succession Climate input file
#'
#' Writes a space-delimited PnET climate file containing monthly weather
#' observations (§6 of the user guide). This file is referenced from
#' each ecoregion (via `ClimateFileName`) when `ClimateConfigFile` is not
#' in use. The first line is a header; subsequent lines are monthly
#' observations in chronological order. Ozone (`O3`) is optional.
#'
#' @param df `data.frame` with columns `Year`, `Month`, `TMax`, `TMin`, `PAR`,
#'   `Prec`, `CO2`, and optionally `O3`. `Year` may be a 4-digit integer or a
#'   range string of the form `"1700-1979"` (see §6.3.1).
#'
#' @template param_path
#'
#' @template param_filename
#'
#' @template return_file
#'
#' @family PnET Succession helpers
#'
#' @export
prepPnETClimateFile <- function(df = NULL, path, filename = "PnET_climate.txt") {
  required_cols <- c("Year", "Month", "TMax", "TMin", "PAR", "Prec", "CO2")
  stopifnot(
    is.data.frame(df),
    all(required_cols %in% colnames(df)),
    all(colnames(df) %in% c(required_cols, "O3"))
  )

  ## enforce header column order; O3 trails when present
  cols <- c(required_cols, intersect("O3", colnames(df)))
  df <- df[, cols, drop = FALSE]

  stopifnot(
    all(df$Month >= 1L),
    all(df$Month <= 12L),
    all(df$PAR >= 0),
    all(df$Prec >= 0),
    all(df$CO2 > 0)
  )

  file <- file.path(path, filename)
  writeLines(
    c(paste(cols, collapse = " "), apply(df, 1L, function(x) paste(x, collapse = " "))),
    file
  )

  return(file)
}

#' Prepare PnET-Succession `PNEToutputsites` input file
#'
#' Writes the `PNEToutputsites` input file (§12 of the user guide),
#' which lists individual sites at which detailed PnET output tables are
#' requested. Two coordinate forms are supported:
#'
#' - **map coordinates** (`coords = "map"`): four numeric columns
#'   `MapCoordinatesX`, `MapCoordinatesY`, `MapCoordinatesMaxX`,
#'   `MapCoordinatesMaxY`.
#' - **row/column coordinates** (`coords = "rowcol"`): two integer columns
#'   `Row`, `Column`.
#'
#' @param df `data.frame` whose first column (`Site`) names each site, with
#'   the remaining columns matching `coords`.
#' @param coords Character, one of `"map"` or `"rowcol"`. Default `"rowcol"`.
#'
#' @template param_path
#'
#' @template param_filename
#'
#' @template return_file
#'
#' @family PnET Succession helpers
#'
#' @export
prepPNEToutputsites <- function(
  df = NULL,
  path,
  coords = c("rowcol", "map"),
  filename = "PNEToutputsites.txt"
) {
  coords <- match.arg(coords)
  stopifnot(is.data.frame(df), nrow(df) > 0L, "Site" %in% colnames(df))

  expected <- if (coords == "map") {
    c("Site", "MapCoordinatesX", "MapCoordinatesY", "MapCoordinatesMaxX", "MapCoordinatesMaxY")
  } else {
    c("Site", "Row", "Column")
  }
  missing_cols <- setdiff(expected, colnames(df))
  if (length(missing_cols) > 0L) {
    stop(
      "missing PNEToutputsites columns for coords = '",
      coords,
      "': ",
      paste(missing_cols, collapse = ", ")
    )
  }
  df <- df[, expected, drop = FALSE]

  header <- paste("PNEToutputsites", paste(expected[-1L], collapse = "  "))
  rule <- paste(">>", paste(rep("----------", length(expected)), collapse = "  "))

  file <- file.path(path, filename)
  writeLines(
    c(
      insertLandisData("PNEToutputsites"),
      header,
      rule,
      apply(df, 1L, function(x) paste(x, collapse = "  ")),
      ""
    ),
    file
  )

  return(file)
}
