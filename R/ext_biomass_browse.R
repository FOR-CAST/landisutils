#' Biomass Browse Extension
#'
#' @section LANDIS-II v8 compatibility:
#' Biomass Browse has not yet been updated by the LANDIS-II developers for
#' LANDIS-II v8; the most recent upstream release (v2.0, May 2022) targets
#' the v7 core only. This R6 class tracks the v2.0 schema and exercises the
#' input-file generation, but `$write()`-produced files cannot currently be
#' run through a LANDIS-II v8 console. Constructing a `BiomassBrowse` object
#' emits a one-time `warning()` to remind users of this. The warning will be
#' removed once a v8-compatible Biomass Browse release is available.
#'
#' @include ext_utils.R
#'
#' @references LANDIS-II Browse Disturbance v2.0 Extension User Guide
#'   <https://github.com/LANDIS-II-Foundation/Extension-Biomass-Browse/blob/master/docs/LANDIS-II%20Biomass%20Browse%20v2.0%20User%20Guide.pdf>
#'
#' @family Biomass Browse helpers
#'
#' @export
BiomassBrowse <- R6Class(
  "BiomassBrowse",
  inherit = LandisExtension,
  public = list(
    #' @param path Character. Directory path.
    #' @param Timestep Integer. Years between browse updates (typically 1;
    #'   §4.2.1).
    #' @param SpeciesTable `data.frame` with six columns matching §4.2.2:
    #'   `Species` (character), `Preference`, `GrowthReductionThreshold`,
    #'   `GrowthReductionMaximum`, `MortalityThreshold`, `MortalityMaximum`
    #'   (all numeric in `[0, 1]`).
    #' @param ZoneMap Character. Relative path to the population-zones
    #'   raster (§4.2.3).
    #' @param BrowseMethod Character. One of `"Population"` or `"BDI"`
    #'   (§4.2.4).
    #' @param DefinedPopulationFile Character. Relative path to the
    #'   population / BDI definition file (§4.2.5).
    #' @param DynamicPopulation List of dynamic-population parameters
    #'   (§4.2.6) when running in dynamic mode; pass `NULL` for
    #'   static/defined population. Required keys (all numeric):
    #'   `r .fmtKeys(.browseDynamicPopulationKeys)`.
    #' @param ConsumptionRate Integer (kg/yr/individual; §4.2.7).
    #' @param ANPPForageProp Numeric in `[0, 1]`; proportion of `ANPP` that
    #'   counts as forage (§4.2.8; default 0.66).
    #' @param MinBrowsePropinReach Numeric in `[0, 1]`; minimum proportion
    #'   of browse within reach for a cohort to be browsed (§4.2.9).
    #' @param BrowseBiomassThreshold Numeric in `[0, 1]`; proportion of
    #'   ecoregion max biomass at which cohorts begin to escape browse
    #'   (§4.2.12).
    #' @param BrowseBiomassThresholdMin,BrowseBiomassThresholdMax (Optional)
    #'   Numeric in `[0, 1]`; thresholds for the
    #'   `ForageInReachMethod = "LinearEachCohort"` form (§4.2.10–4.2.11).
    #' @param EscapeBrowsePropLong Numeric in `[0, 1]`; proportion of
    #'   longevity at which cohorts are considered escaped from browse
    #'   (§4.2.13).
    #' @param CalibrateMode Character (`"ON"` / `"OFF"`); optional, default
    #'   `"OFF"` (§4.2.14).
    #' @param GrowthReduction Character (`"ON"` / `"OFF"`); optional, default
    #'   `"ON"` (§4.2.15).
    #' @param Mortality Character (`"ON"` / `"OFF"`); optional, default
    #'   `"ON"` (§4.2.16).
    #' @param CountNonForageinSitePref Logical; optional, default `FALSE`
    #'   (§4.2.17).
    #' @param UseInitBiomassAsForage Logical; optional, default `FALSE`
    #'   (§4.2.18).
    #' @param ForageInReachMethod Character; one of `"Ordered"` or
    #'   `"LinearEachCohort"`; optional, default `"Ordered"` (§4.2.19).
    #' @param ForageQuantity (Optional) Integer; HSI neighborhood radius for
    #'   forage-quantity component (§4.2.20.1). Either this or `SitePreference`
    #'   (or both) must be provided to compute HSI maps.
    #' @param SitePreference (Optional) Integer; HSI neighborhood radius for
    #'   site-preference component (§4.2.20.2).
    #' @param SitePrefMapNames,SiteForageMapNames,SiteHSIMapNames,SitePopulationMapNames,BiomassRemovedMapNames
    #'   (Optional) Character. Output filename patterns; each must contain
    #'   `{timestep}` (§4.2.21).
    #' @param LogFile Character. Relative path to the events CSV log
    #'   (§4.2.22).
    initialize = function(
      path,
      Timestep = 1L,
      SpeciesTable = NULL,
      ZoneMap = NULL,
      BrowseMethod = "Population",
      DefinedPopulationFile = NULL,
      DynamicPopulation = NULL,
      ConsumptionRate = NULL,
      ANPPForageProp = 0.66,
      MinBrowsePropinReach = NULL,
      BrowseBiomassThreshold = NULL,
      BrowseBiomassThresholdMin = NULL,
      BrowseBiomassThresholdMax = NULL,
      EscapeBrowsePropLong = NULL,
      CalibrateMode = NULL,
      GrowthReduction = NULL,
      Mortality = NULL,
      CountNonForageinSitePref = NULL,
      UseInitBiomassAsForage = NULL,
      ForageInReachMethod = NULL,
      ForageQuantity = NULL,
      SitePreference = NULL,
      SitePrefMapNames = NULL,
      SiteForageMapNames = NULL,
      SiteHSIMapNames = NULL,
      SitePopulationMapNames = NULL,
      BiomassRemovedMapNames = NULL,
      LogFile = "browse/browse-log.csv"
    ) {
      stopifnot(!is.null(path))

      warning(
        "Biomass Browse has not yet been updated for LANDIS-II v8; ",
        "the input file produced by `$write()` will only be parseable ",
        "by a v7-compatible Biomass Browse release. ",
        "See the package README and `?BiomassBrowse` for status.",
        call. = FALSE
      )

      ## LandisExtension fields
      private$.LandisData <- "Biomass Browse"
      self$Timestep <- Timestep

      self$type <- "disturbance"
      self$path <- path
      self$files <- "biomass-browse.txt" ## file won't exist yet

      ## additional fields for this extension
      self$SpeciesTable <- SpeciesTable
      self$ZoneMap <- ZoneMap
      self$BrowseMethod <- BrowseMethod
      self$DefinedPopulationFile <- DefinedPopulationFile
      self$DynamicPopulation <- DynamicPopulation
      self$ConsumptionRate <- ConsumptionRate
      self$ANPPForageProp <- ANPPForageProp
      self$MinBrowsePropinReach <- MinBrowsePropinReach
      self$BrowseBiomassThreshold <- BrowseBiomassThreshold
      self$BrowseBiomassThresholdMin <- BrowseBiomassThresholdMin
      self$BrowseBiomassThresholdMax <- BrowseBiomassThresholdMax
      self$EscapeBrowsePropLong <- EscapeBrowsePropLong
      self$CalibrateMode <- CalibrateMode
      self$GrowthReduction <- GrowthReduction
      self$Mortality <- Mortality
      self$CountNonForageinSitePref <- CountNonForageinSitePref
      self$UseInitBiomassAsForage <- UseInitBiomassAsForage
      self$ForageInReachMethod <- ForageInReachMethod
      self$ForageQuantity <- ForageQuantity
      self$SitePreference <- SitePreference
      self$SitePrefMapNames <- SitePrefMapNames
      self$SiteForageMapNames <- SiteForageMapNames
      self$SiteHSIMapNames <- SiteHSIMapNames
      self$SitePopulationMapNames <- SitePopulationMapNames
      self$BiomassRemovedMapNames <- BiomassRemovedMapNames
      self$LogFile <- LogFile
    },

    #' @description Write extension inputs to disk
    write = function() {
      stopifnot(
        !is.null(self$SpeciesTable),
        !is.null(self$ZoneMap),
        !is.null(self$DefinedPopulationFile),
        !is.null(self$ConsumptionRate),
        !is.null(self$MinBrowsePropinReach),
        !is.null(self$BrowseBiomassThreshold),
        !is.null(self$EscapeBrowsePropLong),
        !is.null(self$LogFile),
        ## HSI inputs: at least one neighborhood radius required to compute HSI
        !is.null(self$ForageQuantity) || !is.null(self$SitePreference)
      )

      writeLines(
        c(
          insertLandisData(private$.LandisData),
          insertValue("Timestep", self$Timestep),
          insertBrowseSpeciesTable(self$SpeciesTable),
          glue::glue(">> Browser population Inputs"),
          insertValue("ZoneMap", self$ZoneMap, blank_line = FALSE),
          insertValue("BrowseMethod", self$BrowseMethod, blank_line = FALSE),
          insertValue("DefinedPopulationFile", self$DefinedPopulationFile, blank_line = FALSE),
          insertBrowseDynamicPopulation(self$DynamicPopulation),
          insertValue("ConsumptionRate", self$ConsumptionRate),
          glue::glue(">> Forage Inputs"),
          insertValue("ANPPForageProp", self$ANPPForageProp, blank_line = FALSE),
          insertValue("MinBrowsePropinReach", self$MinBrowsePropinReach, blank_line = FALSE),
          insertValue("BrowseBiomassThreshold", self$BrowseBiomassThreshold, blank_line = FALSE),
          if (!is.null(self$BrowseBiomassThresholdMin)) {
            insertValue(
              "BrowseBiomassThresholdMin",
              self$BrowseBiomassThresholdMin,
              blank_line = FALSE
            )
          },
          if (!is.null(self$BrowseBiomassThresholdMax)) {
            insertValue(
              "BrowseBiomassThresholdMax",
              self$BrowseBiomassThresholdMax,
              blank_line = FALSE
            )
          },
          insertValue("EscapeBrowsePropLong", self$EscapeBrowsePropLong),
          glue::glue(">> Options"),
          if (!is.null(self$CalibrateMode)) {
            insertValue("CalibrateMode", self$CalibrateMode, blank_line = FALSE)
          },
          if (!is.null(self$GrowthReduction)) {
            insertValue("GrowthReduction", self$GrowthReduction, blank_line = FALSE)
          },
          if (!is.null(self$Mortality)) {
            insertValue("Mortality", self$Mortality, blank_line = FALSE)
          },
          if (!is.null(self$CountNonForageinSitePref)) {
            insertValue(
              "CountNonForageinSitePref",
              self$CountNonForageinSitePref,
              blank_line = FALSE
            )
          },
          if (!is.null(self$UseInitBiomassAsForage)) {
            insertValue("UseInitBiomassAsForage", self$UseInitBiomassAsForage, blank_line = FALSE)
          },
          if (!is.null(self$ForageInReachMethod)) {
            insertValue("ForageInReachMethod", self$ForageInReachMethod, blank_line = FALSE)
          },
          glue::glue(""),
          glue::glue(">> HSI Inputs"),
          if (!is.null(self$ForageQuantity)) {
            insertValue("ForageQuantity", self$ForageQuantity, blank_line = FALSE)
          },
          if (!is.null(self$SitePreference)) {
            insertValue("SitePreference", self$SitePreference, blank_line = FALSE)
          },
          glue::glue(""),
          glue::glue(">> Output Maps"),
          if (!is.null(self$SitePrefMapNames)) {
            insertValue("SitePrefMapNames", self$SitePrefMapNames, blank_line = FALSE)
          },
          if (!is.null(self$SiteForageMapNames)) {
            insertValue("SiteForageMapNames", self$SiteForageMapNames, blank_line = FALSE)
          },
          if (!is.null(self$SiteHSIMapNames)) {
            insertValue("SiteHSIMapNames", self$SiteHSIMapNames, blank_line = FALSE)
          },
          if (!is.null(self$SitePopulationMapNames)) {
            insertValue("SitePopulationMapNames", self$SitePopulationMapNames, blank_line = FALSE)
          },
          if (!is.null(self$BiomassRemovedMapNames)) {
            insertValue("BiomassRemovedMapNames", self$BiomassRemovedMapNames, blank_line = FALSE)
          },
          glue::glue(""),
          insertValue("LogFile", self$LogFile)
        ),
        file.path(self$path, self$files[1])
      )

      return(invisible(self))
    }
  ),

  private = list(
    .SpeciesTable = NULL,
    .ZoneMap = NULL,
    .BrowseMethod = NULL,
    .DefinedPopulationFile = NULL,
    .DynamicPopulation = NULL,
    .ConsumptionRate = NULL,
    .ANPPForageProp = NULL,
    .MinBrowsePropinReach = NULL,
    .BrowseBiomassThreshold = NULL,
    .BrowseBiomassThresholdMin = NULL,
    .BrowseBiomassThresholdMax = NULL,
    .EscapeBrowsePropLong = NULL,
    .CalibrateMode = NULL,
    .GrowthReduction = NULL,
    .Mortality = NULL,
    .CountNonForageinSitePref = NULL,
    .UseInitBiomassAsForage = NULL,
    .ForageInReachMethod = NULL,
    .ForageQuantity = NULL,
    .SitePreference = NULL,
    .SitePrefMapNames = NULL,
    .SiteForageMapNames = NULL,
    .SiteHSIMapNames = NULL,
    .SitePopulationMapNames = NULL,
    .BiomassRemovedMapNames = NULL,
    .LogFile = NULL
  ),

  active = list(
    #' @field SpeciesTable `data.frame` with the six columns enumerated in
    #'   `.browseSpeciesTableCols`.
    SpeciesTable = function(value) {
      if (missing(value)) {
        return(private$.SpeciesTable)
      } else {
        if (!is.null(value)) {
          stopifnot(
            is.data.frame(value),
            all(.browseSpeciesTableCols %in% colnames(value)),
            all(value$Preference >= 0, value$Preference <= 1),
            all(value$GrowthReductionThreshold >= 0, value$GrowthReductionThreshold <= 1),
            all(value$GrowthReductionMaximum >= 0, value$GrowthReductionMaximum <= 1),
            all(value$MortalityThreshold >= 0, value$MortalityThreshold <= 1),
            all(value$MortalityMaximum >= 0, value$MortalityMaximum <= 1)
          )
        }
        private$.SpeciesTable <- value
      }
    },

    #' @field ZoneMap Character. Relative path to the population-zones raster.
    ZoneMap = function(value) {
      if (missing(value)) {
        return(private$.ZoneMap)
      } else {
        if (!is.null(value)) {
          private$.ZoneMap <- .relPath(value, self$path)
        } else {
          private$.ZoneMap <- NULL
        }
      }
    },

    #' @field BrowseMethod Character. One of `"Population"`, `"BDI"`.
    BrowseMethod = function(value) {
      if (missing(value)) {
        return(private$.BrowseMethod)
      } else {
        if (!is.null(value)) {
          stopifnot(value %in% .browseMethods)
        }
        private$.BrowseMethod <- value
      }
    },

    #' @field DefinedPopulationFile Character. Relative path.
    DefinedPopulationFile = function(value) {
      if (missing(value)) {
        return(private$.DefinedPopulationFile)
      } else {
        if (!is.null(value)) {
          private$.DefinedPopulationFile <- .relPath(value, self$path)
        } else {
          private$.DefinedPopulationFile <- NULL
        }
      }
    },

    #' @field DynamicPopulation List of dynamic-population parameters, or `NULL`.
    #'   Required keys (all numeric): `r .fmtKeys(.browseDynamicPopulationKeys)`.
    DynamicPopulation = function(value) {
      if (missing(value)) {
        return(private$.DynamicPopulation)
      } else {
        if (!is.null(value)) {
          stopifnot(
            is.list(value),
            all(.browseDynamicPopulationKeys %in% names(value)),
            all(vapply(value[.browseDynamicPopulationKeys], is.numeric, logical(1)))
          )
        }
        private$.DynamicPopulation <- value
      }
    },

    #' @field ConsumptionRate Integer.
    ConsumptionRate = function(value) {
      if (missing(value)) {
        return(private$.ConsumptionRate)
      } else {
        if (!is.null(value)) {
          stopifnot(is.numeric(value), length(value) == 1L, value > 0)
        }
        private$.ConsumptionRate <- as.integer(value)
      }
    },

    #' @field ANPPForageProp Numeric in `[0, 1]`.
    ANPPForageProp = function(value) {
      if (missing(value)) {
        return(private$.ANPPForageProp)
      } else {
        if (!is.null(value)) {
          stopifnot(is.numeric(value), length(value) == 1L, value >= 0, value <= 1)
        }
        private$.ANPPForageProp <- value
      }
    },

    #' @field MinBrowsePropinReach Numeric in `[0, 1]`.
    MinBrowsePropinReach = function(value) {
      if (missing(value)) {
        return(private$.MinBrowsePropinReach)
      } else {
        if (!is.null(value)) {
          stopifnot(is.numeric(value), length(value) == 1L, value >= 0, value <= 1)
        }
        private$.MinBrowsePropinReach <- value
      }
    },

    #' @field BrowseBiomassThreshold Numeric in `[0, 1]`.
    BrowseBiomassThreshold = function(value) {
      if (missing(value)) {
        return(private$.BrowseBiomassThreshold)
      } else {
        if (!is.null(value)) {
          stopifnot(is.numeric(value), length(value) == 1L, value >= 0, value <= 1)
        }
        private$.BrowseBiomassThreshold <- value
      }
    },

    #' @field BrowseBiomassThresholdMin Numeric in `[0, 1]`.
    BrowseBiomassThresholdMin = function(value) {
      if (missing(value)) {
        return(private$.BrowseBiomassThresholdMin)
      } else {
        if (!is.null(value)) {
          stopifnot(is.numeric(value), length(value) == 1L, value >= 0, value <= 1)
        }
        private$.BrowseBiomassThresholdMin <- value
      }
    },

    #' @field BrowseBiomassThresholdMax Numeric in `[0, 1]`.
    BrowseBiomassThresholdMax = function(value) {
      if (missing(value)) {
        return(private$.BrowseBiomassThresholdMax)
      } else {
        if (!is.null(value)) {
          stopifnot(is.numeric(value), length(value) == 1L, value >= 0, value <= 1)
        }
        private$.BrowseBiomassThresholdMax <- value
      }
    },

    #' @field EscapeBrowsePropLong Numeric in `[0, 1]`.
    EscapeBrowsePropLong = function(value) {
      if (missing(value)) {
        return(private$.EscapeBrowsePropLong)
      } else {
        if (!is.null(value)) {
          stopifnot(is.numeric(value), length(value) == 1L, value >= 0, value <= 1)
        }
        private$.EscapeBrowsePropLong <- value
      }
    },

    #' @field CalibrateMode Character `"ON"`/`"OFF"`.
    CalibrateMode = function(value) {
      if (missing(value)) {
        return(private$.CalibrateMode)
      } else {
        if (!is.null(value)) {
          value <- toupper(value)
          stopifnot(value %in% c("ON", "OFF"))
        }
        private$.CalibrateMode <- value
      }
    },

    #' @field GrowthReduction Character `"ON"`/`"OFF"`.
    GrowthReduction = function(value) {
      if (missing(value)) {
        return(private$.GrowthReduction)
      } else {
        if (!is.null(value)) {
          value <- toupper(value)
          stopifnot(value %in% c("ON", "OFF"))
        }
        private$.GrowthReduction <- value
      }
    },

    #' @field Mortality Character `"ON"`/`"OFF"`.
    Mortality = function(value) {
      if (missing(value)) {
        return(private$.Mortality)
      } else {
        if (!is.null(value)) {
          value <- toupper(value)
          stopifnot(value %in% c("ON", "OFF"))
        }
        private$.Mortality <- value
      }
    },

    #' @field CountNonForageinSitePref Character `"TRUE"`/`"FALSE"` (set
    #'   from a logical).
    CountNonForageinSitePref = function(value) {
      if (missing(value)) {
        return(private$.CountNonForageinSitePref)
      } else {
        if (!is.null(value)) {
          private$.CountNonForageinSitePref <- if (isTRUE(value)) "TRUE" else "FALSE"
        } else {
          private$.CountNonForageinSitePref <- NULL
        }
      }
    },

    #' @field UseInitBiomassAsForage Character `"TRUE"`/`"FALSE"` (set
    #'   from a logical).
    UseInitBiomassAsForage = function(value) {
      if (missing(value)) {
        return(private$.UseInitBiomassAsForage)
      } else {
        if (!is.null(value)) {
          private$.UseInitBiomassAsForage <- if (isTRUE(value)) "TRUE" else "FALSE"
        } else {
          private$.UseInitBiomassAsForage <- NULL
        }
      }
    },

    #' @field ForageInReachMethod One of `"Ordered"`, `"LinearEachCohort"`.
    ForageInReachMethod = function(value) {
      if (missing(value)) {
        return(private$.ForageInReachMethod)
      } else {
        if (!is.null(value)) {
          stopifnot(value %in% .browseForageInReachMethods)
        }
        private$.ForageInReachMethod <- value
      }
    },

    #' @field ForageQuantity Integer (neighborhood radius).
    ForageQuantity = function(value) {
      if (missing(value)) {
        return(private$.ForageQuantity)
      } else {
        if (!is.null(value)) {
          stopifnot(is.numeric(value), length(value) == 1L, value >= 0)
        }
        private$.ForageQuantity <- if (!is.null(value)) as.integer(value) else NULL
      }
    },

    #' @field SitePreference Integer (neighborhood radius).
    SitePreference = function(value) {
      if (missing(value)) {
        return(private$.SitePreference)
      } else {
        if (!is.null(value)) {
          stopifnot(is.numeric(value), length(value) == 1L, value >= 0)
        }
        private$.SitePreference <- if (!is.null(value)) as.integer(value) else NULL
      }
    },

    #' @field SitePrefMapNames Character. Output filename pattern.
    SitePrefMapNames = function(value) {
      if (missing(value)) {
        return(private$.SitePrefMapNames)
      } else {
        if (!is.null(value)) {
          stopifnot(grepl("{timestep}", value, fixed = TRUE))
        }
        private$.SitePrefMapNames <- value
      }
    },

    #' @field SiteForageMapNames Character. Output filename pattern.
    SiteForageMapNames = function(value) {
      if (missing(value)) {
        return(private$.SiteForageMapNames)
      } else {
        if (!is.null(value)) {
          stopifnot(grepl("{timestep}", value, fixed = TRUE))
        }
        private$.SiteForageMapNames <- value
      }
    },

    #' @field SiteHSIMapNames Character. Output filename pattern.
    SiteHSIMapNames = function(value) {
      if (missing(value)) {
        return(private$.SiteHSIMapNames)
      } else {
        if (!is.null(value)) {
          stopifnot(grepl("{timestep}", value, fixed = TRUE))
        }
        private$.SiteHSIMapNames <- value
      }
    },

    #' @field SitePopulationMapNames Character. Output filename pattern.
    SitePopulationMapNames = function(value) {
      if (missing(value)) {
        return(private$.SitePopulationMapNames)
      } else {
        if (!is.null(value)) {
          stopifnot(grepl("{timestep}", value, fixed = TRUE))
        }
        private$.SitePopulationMapNames <- value
      }
    },

    #' @field BiomassRemovedMapNames Character. Output filename pattern.
    BiomassRemovedMapNames = function(value) {
      if (missing(value)) {
        return(private$.BiomassRemovedMapNames)
      } else {
        if (!is.null(value)) {
          stopifnot(grepl("{timestep}", value, fixed = TRUE))
        }
        private$.BiomassRemovedMapNames <- value
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

#' Required `SpeciesTable` columns for the Biomass Browse extension
#' @keywords internal
.browseSpeciesTableCols <- c(
  "Species",
  "Preference",
  "GrowthReductionThreshold",
  "GrowthReductionMaximum",
  "MortalityThreshold",
  "MortalityMaximum"
)

#' Allowed Biomass Browse `BrowseMethod` values
#' @keywords internal
.browseMethods <- c("Population", "BDI")

#' Allowed Biomass Browse `ForageInReachMethod` values
#' @keywords internal
.browseForageInReachMethods <- c("Ordered", "LinearEachCohort")

#' Required keys for the dynamic-browser-population block
#' @keywords internal
.browseDynamicPopulationKeys <- c(
  "RMin",
  "RMax",
  "MortalityMin",
  "MortalityMax",
  "PredationMin",
  "PredationMax",
  "HarvestMin",
  "HarvestMax"
)

#' Specify the `SpeciesTable` block for the Biomass Browse extension
#'
#' @param df `data.frame` with columns enumerated in `.browseSpeciesTableCols`.
#'
#' @template return_insert
#'
#' @family Biomass Browse helpers
#'
#' @keywords internal
insertBrowseSpeciesTable <- function(df) {
  stopifnot(is.data.frame(df), all(.browseSpeciesTableCols %in% colnames(df)))

  rows <- apply(df[, .browseSpeciesTableCols, drop = FALSE], 1, function(x) {
    sprintf(
      "   %-12s %-7s %-10s %-7s %-10s %s",
      x[["Species"]],
      x[["Preference"]],
      x[["GrowthReductionThreshold"]],
      x[["GrowthReductionMaximum"]],
      x[["MortalityThreshold"]],
      x[["MortalityMaximum"]]
    )
  })

  c(
    glue::glue("SpeciesTable"),
    glue::glue(">>                          --GrowthReduction--    -----Mortality-----"),
    glue::glue(">> Name         Preference  Threshold     Max     Threshold     Max"),
    glue::glue(">> --------     ----------  ---------     ---     ---------     ---"),
    rows,
    glue::glue("")
  )
}

#' Specify the dynamic-browser-population rows for the Biomass Browse extension
#'
#' Emits the keyword `DynamicPopulation` followed by `RMin`, `RMax`,
#' `MortalityMin`, `MortalityMax`, `PredationMin`, `PredationMax`,
#' `HarvestMin`, `HarvestMax` (one keyword per line). Returns
#' `character(0)` when `dyn` is `NULL`.
#'
#' @param dyn Named list of dynamic-population parameters.
#'
#' @template return_insert
#'
#' @family Biomass Browse helpers
#'
#' @keywords internal
insertBrowseDynamicPopulation <- function(dyn) {
  if (is.null(dyn)) {
    return(character(0))
  }
  stopifnot(is.list(dyn), all(.browseDynamicPopulationKeys %in% names(dyn)))

  c(
    glue::glue("DynamicPopulation"),
    vapply(
      .browseDynamicPopulationKeys,
      function(k) {
        as.character(insertValue(k, dyn[[k]], blank_line = FALSE))
      },
      character(1),
      USE.NAMES = FALSE
    ),
    glue::glue("")
  )
}
