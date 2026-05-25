#' Forest Roads Simulation Extension
#'
#' @include ext_utils.R
#'
#' @references LANDIS-II Forest Roads Simulation v2 Extension User Guide
#'   <https://github.com/Klemet/LANDIS-II-Forest-Roads-Simulation-extension/blob/master/docs/LANDIS-II%20Forest%20Roads%20Simulation%20v2.0%20User%20Guide.pdf>
#'
#' @family Forest Roads Simulation helpers
#'
#' @export
ForestRoadsSimulation <- R6Class(
  "ForestRoadsSimulation",
  inherit = LandisExtension,
  public = list(
    #' @param path Character. Directory path.
    #' @param Timestep Integer. Years between road-construction cycles.
    #' @param HeuristicForNetworkConstruction Character. One of `"Random"`,
    #'   `"ClosestFirst"`, `"FarthestFirst"` (case-insensitive). Normalized on
    #'   write to the form expected by the upstream parser
    #'   (`"Closestfirst"`, `"Farthestfirst"`).
    #' @param SkiddingDistance Numeric. Distance (m) wood can be skidded
    #'   before requiring a road.
    #' @param LoopingBehavior Logical (or `"yes"`/`"no"`). Whether road
    #'   looping is enabled.
    #' @param LoopingMinDistance,LoopingMaxDistance,LoopingMaxPercentageOfRoads,LoopingMaxCost,LoopingProbability
    #'   Numeric. Required when `LoopingBehavior` is truthy.
    #' @param OutputsOfRoadNetworkMaps Character. Output path for road-network
    #'   raster maps.
    #' @param OutputsOfRoadLog Character. Output directory for road-construction
    #'   logs.
    #' @param RasterOfBuildableZones Character. Input raster path defining
    #'   buildable zones.
    #' @param InitialRoadNetworkMap Character. Input raster path with the
    #'   initial road network.
    #' @param DistanceCost Numeric. Base cost per unit distance.
    #' @param CoarseElevationRaster Character. Coarse-elevation raster path.
    #' @param CoarseElevationCosts List of `ElevationCostRange` objects (see
    #'   [elevationCostRange()]).
    #' @param FineElevationRaster (Optional) Character. Fine-elevation raster
    #'   path; pass `NULL` or `"none"` to disable.
    #' @param FineElevationCosts (Optional) List of `ElevationCostRange`
    #'   objects; required when `FineElevationRaster` is set.
    #' @param CoarseWaterRaster (Optional) Character. Coarse-water raster path.
    #' @param CoarseWaterCost (Optional) Numeric. Required when
    #'   `CoarseWaterRaster` is set.
    #' @param FineWaterRaster (Optional) Character. Fine-water raster path.
    #' @param FineWaterCost (Optional) Numeric. Required when
    #'   `FineWaterRaster` is set.
    #' @param SoilsRaster (Optional) Character. Soils raster path.
    #' @param SimulationOfRoadAging Logical (or `"yes"`/`"no"`).
    #' @param SimulationOfWoodFlux Logical (or `"yes"`/`"no"`).
    #' @param RoadTypes List of `RoadType` objects (see [roadType()]).
    #' @param RoadTypesForExitingWood List of `ExitRoadType` objects (see
    #'   [exitRoadType()]).
    initialize = function(
      path,
      Timestep = NULL,
      HeuristicForNetworkConstruction = "ClosestFirst",
      SkiddingDistance = NULL,
      LoopingBehavior = FALSE,
      LoopingMinDistance = NULL,
      LoopingMaxDistance = NULL,
      LoopingMaxPercentageOfRoads = NULL,
      LoopingMaxCost = NULL,
      LoopingProbability = NULL,
      OutputsOfRoadNetworkMaps = NULL,
      OutputsOfRoadLog = NULL,
      RasterOfBuildableZones = NULL,
      InitialRoadNetworkMap = NULL,
      DistanceCost = NULL,
      CoarseElevationRaster = NULL,
      CoarseElevationCosts = list(),
      FineElevationRaster = NULL,
      FineElevationCosts = NULL,
      CoarseWaterRaster = NULL,
      CoarseWaterCost = NULL,
      FineWaterRaster = NULL,
      FineWaterCost = NULL,
      SoilsRaster = NULL,
      SimulationOfRoadAging = FALSE,
      SimulationOfWoodFlux = FALSE,
      RoadTypes = list(),
      RoadTypesForExitingWood = list()
    ) {
      stopifnot(!is.null(path))

      ## LandisExtension fields
      private$.LandisData <- "Forest Roads Simulation"
      self$Timestep <- Timestep

      self$type <- "disturbance"
      self$path <- path
      self$files <- "forest-roads-simulation.txt" ## file won't exist yet

      ## additional fields
      self$HeuristicForNetworkConstruction <- HeuristicForNetworkConstruction
      self$SkiddingDistance <- SkiddingDistance
      self$LoopingBehavior <- LoopingBehavior
      self$LoopingMinDistance <- LoopingMinDistance
      self$LoopingMaxDistance <- LoopingMaxDistance
      self$LoopingMaxPercentageOfRoads <- LoopingMaxPercentageOfRoads
      self$LoopingMaxCost <- LoopingMaxCost
      self$LoopingProbability <- LoopingProbability
      self$OutputsOfRoadNetworkMaps <- OutputsOfRoadNetworkMaps %||%
        "output/disturbances/roads/roadNetwork.tif"
      self$OutputsOfRoadLog <- OutputsOfRoadLog %||% "output/disturbances/roads/"
      self$RasterOfBuildableZones <- RasterOfBuildableZones
      self$InitialRoadNetworkMap <- InitialRoadNetworkMap
      self$DistanceCost <- DistanceCost
      self$CoarseElevationRaster <- CoarseElevationRaster
      self$CoarseElevationCosts <- CoarseElevationCosts
      self$FineElevationRaster <- FineElevationRaster
      self$FineElevationCosts <- FineElevationCosts
      self$CoarseWaterRaster <- CoarseWaterRaster
      self$CoarseWaterCost <- CoarseWaterCost
      self$FineWaterRaster <- FineWaterRaster
      self$FineWaterCost <- FineWaterCost
      self$SoilsRaster <- SoilsRaster
      self$SimulationOfRoadAging <- SimulationOfRoadAging
      self$SimulationOfWoodFlux <- SimulationOfWoodFlux
      self$RoadTypes <- RoadTypes
      self$RoadTypesForExitingWood <- RoadTypesForExitingWood
    },

    #' @description Write extension inputs to disk
    write = function() {
      stopifnot(
        !is.null(self$SkiddingDistance),
        !is.null(self$RasterOfBuildableZones),
        !is.null(self$InitialRoadNetworkMap),
        !is.null(self$DistanceCost),
        !is.null(self$CoarseElevationRaster),
        length(self$CoarseElevationCosts) >= 1L,
        length(self$RoadTypes) >= 1L,
        length(self$RoadTypesForExitingWood) >= 1L
      )

      if (self$LoopingBehavior == "yes") {
        stopifnot(
          !is.null(self$LoopingMinDistance),
          !is.null(self$LoopingMaxDistance),
          !is.null(self$LoopingMaxPercentageOfRoads),
          !is.null(self$LoopingMaxCost),
          !is.null(self$LoopingProbability)
        )
      }

      fine_elev_set <- !is.null(self$FineElevationRaster) && self$FineElevationRaster != "none"
      if (fine_elev_set) {
        stopifnot(length(self$FineElevationCosts) >= 1L)
      }

      coarse_water_set <- !is.null(self$CoarseWaterRaster) && self$CoarseWaterRaster != "none"
      if (coarse_water_set) {
        stopifnot(!is.null(self$CoarseWaterCost))
      }

      fine_water_set <- !is.null(self$FineWaterRaster) && self$FineWaterRaster != "none"
      if (fine_water_set) {
        stopifnot(!is.null(self$FineWaterCost))
      }

      writeLines(
        c(
          insertLandisData(private$.LandisData),
          insertValue("Timestep", self$Timestep),
          insertValue(
            "HeuristicForNetworkConstruction",
            .frsHeuristicForWrite(self$HeuristicForNetworkConstruction)
          ),
          insertValue("SkiddingDistance", self$SkiddingDistance),
          insertValue("LoopingBehavior", self$LoopingBehavior),
          if (self$LoopingBehavior == "yes") {
            c(
              insertValue("LoopingMinDistance", self$LoopingMinDistance, blank_line = FALSE),
              insertValue("LoopingMaxDistance", self$LoopingMaxDistance, blank_line = FALSE),
              insertValue(
                "LoopingMaxPercentageOfRoads",
                self$LoopingMaxPercentageOfRoads,
                blank_line = FALSE
              ),
              insertValue("LoopingMaxCost", self$LoopingMaxCost, blank_line = FALSE),
              insertValue("LoopingProbability", self$LoopingProbability)
            )
          },
          insertValue("OutputsOfRoadNetworkMaps", self$OutputsOfRoadNetworkMaps),
          insertValue("OutputsOfRoadLog", self$OutputsOfRoadLog),
          insertValue("RasterOfBuildableZones", self$RasterOfBuildableZones),
          insertValue("InitialRoadNetworkMap", self$InitialRoadNetworkMap),
          insertValue("DistanceCost", self$DistanceCost),
          insertValue("CoarseElevationRaster", self$CoarseElevationRaster),
          insertElevationCosts("CoarseElevationCosts", self$CoarseElevationCosts, "Additional"),
          insertValue("FineElevationRaster", self$FineElevationRaster %||% "none"),
          if (fine_elev_set) {
            insertElevationCosts("FineElevationCosts", self$FineElevationCosts, "Multiplication")
          },
          insertValue("CoarseWaterRaster", self$CoarseWaterRaster %||% "none"),
          if (coarse_water_set) {
            insertValue("CoarseWaterCost", self$CoarseWaterCost)
          },
          insertValue("FineWaterRaster", self$FineWaterRaster %||% "none"),
          if (fine_water_set) {
            insertValue("FineWaterCost", self$FineWaterCost)
          },
          insertValue("SoilsRaster", self$SoilsRaster %||% "none"),
          insertValue("SimulationOfRoadAging", self$SimulationOfRoadAging),
          insertValue("SimulationOfWoodFlux", self$SimulationOfWoodFlux),
          insertRoadTypes(
            self$RoadTypes,
            includeFlux = self$SimulationOfWoodFlux == "yes",
            includeAge = self$SimulationOfRoadAging == "yes"
          ),
          insertExitRoadTypes(self$RoadTypesForExitingWood)
        ),
        file.path(self$path, self$files[1])
      )

      return(invisible(self))
    }
  ),

  private = list(
    .HeuristicForNetworkConstruction = NULL,
    .SkiddingDistance = NULL,
    .LoopingBehavior = NULL,
    .LoopingMinDistance = NULL,
    .LoopingMaxDistance = NULL,
    .LoopingMaxPercentageOfRoads = NULL,
    .LoopingMaxCost = NULL,
    .LoopingProbability = NULL,
    .OutputsOfRoadNetworkMaps = NULL,
    .OutputsOfRoadLog = NULL,
    .RasterOfBuildableZones = NULL,
    .InitialRoadNetworkMap = NULL,
    .DistanceCost = NULL,
    .CoarseElevationRaster = NULL,
    .CoarseElevationCosts = list(),
    .FineElevationRaster = NULL,
    .FineElevationCosts = list(),
    .CoarseWaterRaster = NULL,
    .CoarseWaterCost = NULL,
    .FineWaterRaster = NULL,
    .FineWaterCost = NULL,
    .SoilsRaster = NULL,
    .SimulationOfRoadAging = NULL,
    .SimulationOfWoodFlux = NULL,
    .RoadTypes = list(),
    .RoadTypesForExitingWood = list()
  ),

  active = list(
    #' @field HeuristicForNetworkConstruction Character. One of
    #'   [.frsHeuristics]; matched case-insensitively, stored in canonical
    #'   user-guide form.
    HeuristicForNetworkConstruction = function(value) {
      if (missing(value)) {
        private$.HeuristicForNetworkConstruction
      } else {
        if (!is.null(value)) {
          stopifnot(is.character(value), length(value) == 1L)
          idx <- match(tolower(value), tolower(.frsHeuristics))
          if (is.na(idx)) {
            stop(
              "HeuristicForNetworkConstruction must be one of: ",
              paste(.frsHeuristics, collapse = ", ")
            )
          }
          value <- .frsHeuristics[idx]
        }
        private$.HeuristicForNetworkConstruction <- value
      }
    },

    #' @field SkiddingDistance Numeric.
    SkiddingDistance = function(value) {
      if (missing(value)) {
        private$.SkiddingDistance
      } else {
        if (!is.null(value)) {
          stopifnot(is.numeric(value), length(value) == 1L, value >= 0)
        }
        private$.SkiddingDistance <- value
      }
    },

    #' @field LoopingBehavior Character `"yes"`/`"no"`.
    LoopingBehavior = function(value) {
      if (missing(value)) {
        private$.LoopingBehavior
      } else {
        private$.LoopingBehavior <- yesno(value)
      }
    },

    #' @field LoopingMinDistance Numeric.
    LoopingMinDistance = function(value) {
      if (missing(value)) {
        private$.LoopingMinDistance
      } else {
        private$.LoopingMinDistance <- value
      }
    },

    #' @field LoopingMaxDistance Numeric.
    LoopingMaxDistance = function(value) {
      if (missing(value)) {
        private$.LoopingMaxDistance
      } else {
        private$.LoopingMaxDistance <- value
      }
    },

    #' @field LoopingMaxPercentageOfRoads Numeric.
    LoopingMaxPercentageOfRoads = function(value) {
      if (missing(value)) {
        private$.LoopingMaxPercentageOfRoads
      } else {
        private$.LoopingMaxPercentageOfRoads <- value
      }
    },

    #' @field LoopingMaxCost Numeric.
    LoopingMaxCost = function(value) {
      if (missing(value)) {
        private$.LoopingMaxCost
      } else {
        private$.LoopingMaxCost <- value
      }
    },

    #' @field LoopingProbability Numeric.
    LoopingProbability = function(value) {
      if (missing(value)) {
        private$.LoopingProbability
      } else {
        private$.LoopingProbability <- value
      }
    },

    #' @field OutputsOfRoadNetworkMaps Character.
    OutputsOfRoadNetworkMaps = function(value) {
      if (missing(value)) {
        private$.OutputsOfRoadNetworkMaps
      } else {
        private$.OutputsOfRoadNetworkMaps <- value
      }
    },

    #' @field OutputsOfRoadLog Character.
    OutputsOfRoadLog = function(value) {
      if (missing(value)) {
        private$.OutputsOfRoadLog
      } else {
        private$.OutputsOfRoadLog <- value
      }
    },

    #' @field RasterOfBuildableZones Character.
    RasterOfBuildableZones = function(value) {
      if (missing(value)) {
        private$.RasterOfBuildableZones
      } else {
        private$.RasterOfBuildableZones <- value
      }
    },

    #' @field InitialRoadNetworkMap Character.
    InitialRoadNetworkMap = function(value) {
      if (missing(value)) {
        private$.InitialRoadNetworkMap
      } else {
        private$.InitialRoadNetworkMap <- value
      }
    },

    #' @field DistanceCost Numeric.
    DistanceCost = function(value) {
      if (missing(value)) {
        private$.DistanceCost
      } else {
        private$.DistanceCost <- value
      }
    },

    #' @field CoarseElevationRaster Character.
    CoarseElevationRaster = function(value) {
      if (missing(value)) {
        private$.CoarseElevationRaster
      } else {
        private$.CoarseElevationRaster <- value
      }
    },

    #' @field CoarseElevationCosts List of `ElevationCostRange` objects.
    CoarseElevationCosts = function(value) {
      if (missing(value)) {
        return(private$.CoarseElevationCosts)
      } else {
        if (is.null(value)) {
          private$.CoarseElevationCosts <- list()
          return(invisible())
        }
        if (inherits(value, "ElevationCostRange")) {
          value <- list(value)
        }
        stopifnot(is.list(value), all(vapply(value, inherits, logical(1), "ElevationCostRange")))
        private$.CoarseElevationCosts <- value
      }
    },

    #' @field FineElevationRaster Character.
    FineElevationRaster = function(value) {
      if (missing(value)) {
        private$.FineElevationRaster
      } else {
        private$.FineElevationRaster <- value
      }
    },

    #' @field FineElevationCosts List of `ElevationCostRange` objects.
    FineElevationCosts = function(value) {
      if (missing(value)) {
        return(private$.FineElevationCosts)
      } else {
        if (is.null(value)) {
          private$.FineElevationCosts <- list()
          return(invisible())
        }
        if (inherits(value, "ElevationCostRange")) {
          value <- list(value)
        }
        stopifnot(is.list(value), all(vapply(value, inherits, logical(1), "ElevationCostRange")))
        private$.FineElevationCosts <- value
      }
    },

    #' @field CoarseWaterRaster Character.
    CoarseWaterRaster = function(value) {
      if (missing(value)) {
        private$.CoarseWaterRaster
      } else {
        private$.CoarseWaterRaster <- value
      }
    },

    #' @field CoarseWaterCost Numeric.
    CoarseWaterCost = function(value) {
      if (missing(value)) {
        private$.CoarseWaterCost
      } else {
        private$.CoarseWaterCost <- value
      }
    },

    #' @field FineWaterRaster Character.
    FineWaterRaster = function(value) {
      if (missing(value)) {
        private$.FineWaterRaster
      } else {
        private$.FineWaterRaster <- value
      }
    },

    #' @field FineWaterCost Numeric.
    FineWaterCost = function(value) {
      if (missing(value)) {
        private$.FineWaterCost
      } else {
        private$.FineWaterCost <- value
      }
    },

    #' @field SoilsRaster Character.
    SoilsRaster = function(value) {
      if (missing(value)) {
        private$.SoilsRaster
      } else {
        private$.SoilsRaster <- value
      }
    },

    #' @field SimulationOfRoadAging Character `"yes"`/`"no"`.
    SimulationOfRoadAging = function(value) {
      if (missing(value)) {
        private$.SimulationOfRoadAging
      } else {
        private$.SimulationOfRoadAging <- yesno(value)
      }
    },

    #' @field SimulationOfWoodFlux Character `"yes"`/`"no"`.
    SimulationOfWoodFlux = function(value) {
      if (missing(value)) {
        private$.SimulationOfWoodFlux
      } else {
        private$.SimulationOfWoodFlux <- yesno(value)
      }
    },

    #' @field RoadTypes List of `RoadType` objects.
    RoadTypes = function(value) {
      if (missing(value)) {
        return(private$.RoadTypes)
      } else {
        if (is.null(value)) {
          private$.RoadTypes <- list()
          return(invisible())
        }
        if (inherits(value, "RoadType")) {
          value <- list(value)
        }
        stopifnot(is.list(value), all(vapply(value, inherits, logical(1), "RoadType")))
        private$.RoadTypes <- value
      }
    },

    #' @field RoadTypesForExitingWood List of `ExitRoadType` objects.
    RoadTypesForExitingWood = function(value) {
      if (missing(value)) {
        return(private$.RoadTypesForExitingWood)
      } else {
        if (is.null(value)) {
          private$.RoadTypesForExitingWood <- list()
          return(invisible())
        }
        if (inherits(value, "ExitRoadType")) {
          value <- list(value)
        }
        stopifnot(is.list(value), all(vapply(value, inherits, logical(1), "ExitRoadType")))
        private$.RoadTypesForExitingWood <- value
      }
    }
  )
)

#' Construct an elevation cost-range entry
#'
#' @param lower,upper Numeric. Inclusive elevation thresholds (m).
#' @param value Numeric. Additive (for `CoarseElevationCosts`) or
#'   multiplicative (for `FineElevationCosts`) cost.
#'
#' @returns A list of class `"ElevationCostRange"`.
#'
#' @family Forest Roads Simulation helpers
#'
#' @export
elevationCostRange <- function(lower, upper, value) {
  stopifnot(
    is.numeric(lower),
    length(lower) == 1L,
    is.numeric(upper),
    length(upper) == 1L,
    upper >= lower,
    is.numeric(value),
    length(value) == 1L
  )

  structure(list(lower = lower, upper = upper, value = value), class = "ElevationCostRange")
}

#' Construct a non-exit road type
#'
#' @param id Integer. Road type ID.
#' @param name Character. Road type name (no whitespace).
#' @param costMultiplier Numeric. Multiplicative cost value.
#' @param fluxMin,fluxMax Numeric. Required when the parent
#'   [ForestRoadsSimulation]'s `SimulationOfWoodFlux` is enabled.
#' @param maxAge Integer. Required when the parent's `SimulationOfRoadAging`
#'   is enabled.
#'
#' @returns A list of class `"RoadType"`.
#'
#' @family Forest Roads Simulation helpers
#'
#' @export
roadType <- function(id, name, costMultiplier, fluxMin = NULL, fluxMax = NULL, maxAge = NULL) {
  stopifnot(
    !is.null(id),
    as.integer(id) > 0L,
    is.character(name),
    length(name) == 1L,
    !grepl("\\s", name),
    is.numeric(costMultiplier),
    length(costMultiplier) == 1L
  )

  if (!is.null(fluxMin) || !is.null(fluxMax)) {
    stopifnot(
      !is.null(fluxMin),
      !is.null(fluxMax),
      is.numeric(fluxMin),
      is.numeric(fluxMax),
      fluxMax >= fluxMin
    )
  }
  if (!is.null(maxAge)) {
    stopifnot(as.integer(maxAge) > 0L)
  }

  structure(
    list(
      id = as.integer(id),
      name = name,
      costMultiplier = costMultiplier,
      fluxMin = fluxMin,
      fluxMax = fluxMax,
      maxAge = if (!is.null(maxAge)) as.integer(maxAge) else NULL
    ),
    class = "RoadType"
  )
}

#' Construct an exit road type (where wood leaves the landscape)
#'
#' @param id Integer. Road type ID.
#' @param name Character. Road type name (no whitespace).
#'
#' @returns A list of class `"ExitRoadType"`.
#'
#' @family Forest Roads Simulation helpers
#'
#' @export
exitRoadType <- function(id, name) {
  stopifnot(
    !is.null(id),
    as.integer(id) > 0L,
    is.character(name),
    length(name) == 1L,
    !grepl("\\s", name)
  )

  structure(list(id = as.integer(id), name = name), class = "ExitRoadType")
}

#' Specify a `*ElevationCosts` table for the Forest Roads extension
#'
#' @param name Character. Either `"CoarseElevationCosts"` or
#'   `"FineElevationCosts"`.
#' @param ranges List of `ElevationCostRange` objects.
#' @param valueLabel Character. Header label for the third column
#'   (`"Additional"` or `"Multiplication"`).
#'
#' @template return_insert
#'
#' @family Forest Roads Simulation helpers
#'
#' @keywords internal
insertElevationCosts <- function(name, ranges, valueLabel) {
  stopifnot(
    is.list(ranges),
    length(ranges) >= 1L,
    all(vapply(ranges, inherits, logical(1), "ElevationCostRange"))
  )

  rows <- vapply(
    ranges,
    function(r) {
      as.character(glue::glue("    {r$lower}    {r$upper}    {r$value}"))
    },
    character(1)
  )

  c(
    glue::glue("{name}"),
    glue::glue(">> Lower elevation    Upper elevation    {valueLabel}"),
    glue::glue(">>    threshold          threshold          value"),
    rows,
    glue::glue("")
  )
}

#' Specify the `RoadTypes` table for the Forest Roads extension
#'
#' Column emission depends on whether the parent extension's
#' `SimulationOfWoodFlux` and `SimulationOfRoadAging` are enabled.
#'
#' @param roads List of `RoadType` objects.
#' @param includeFlux Logical.
#' @param includeAge Logical.
#'
#' @template return_insert
#'
#' @family Forest Roads Simulation helpers
#'
#' @keywords internal
insertRoadTypes <- function(roads, includeFlux, includeAge) {
  stopifnot(
    is.list(roads),
    length(roads) >= 1L,
    all(vapply(roads, inherits, logical(1), "RoadType"))
  )

  if (includeFlux) {
    stopifnot(all(vapply(roads, function(r) !is.null(r$fluxMin), logical(1))))
  }
  if (includeAge) {
    stopifnot(all(vapply(roads, function(r) !is.null(r$maxAge), logical(1))))
  }

  rows <- vapply(
    roads,
    function(r) {
      fields <- character(0)
      if (includeFlux) {
        fields <- c(fields, sprintf("%g", r$fluxMin), sprintf("%g", r$fluxMax))
      }
      fields <- c(fields, as.character(r$id), sprintf("%g", r$costMultiplier))
      if (includeAge) {
        fields <- c(fields, as.character(r$maxAge))
      }
      fields <- c(fields, r$name)
      paste0("    ", paste(fields, collapse = "    "))
    },
    character(1)
  )

  header_cols <- character(0)
  sep_cols <- character(0)
  if (includeFlux) {
    header_cols <- c(header_cols, "Lower Flux", "Upper Flux")
    sep_cols <- c(sep_cols, "----------", "----------")
  }
  header_cols <- c(header_cols, "ID", "CostMult")
  sep_cols <- c(sep_cols, "--", "--------")
  if (includeAge) {
    header_cols <- c(header_cols, "MaxAge")
    sep_cols <- c(sep_cols, "------")
  }
  header_cols <- c(header_cols, "Name")
  sep_cols <- c(sep_cols, "----")

  c(
    glue::glue("RoadTypes"),
    paste0(">> ", paste(header_cols, collapse = "    ")),
    paste0(">> ", paste(sep_cols, collapse = "    ")),
    rows,
    glue::glue("")
  )
}

#' Specify the `RoadTypesForExitingWood` table for the Forest Roads extension
#'
#' @param roads List of `ExitRoadType` objects.
#'
#' @template return_insert
#'
#' @family Forest Roads Simulation helpers
#'
#' @keywords internal
insertExitRoadTypes <- function(roads) {
  stopifnot(
    is.list(roads),
    length(roads) >= 1L,
    all(vapply(roads, inherits, logical(1), "ExitRoadType"))
  )

  rows <- vapply(
    roads,
    function(r) {
      as.character(glue::glue("    {r$id}    {r$name}"))
    },
    character(1)
  )

  c(
    glue::glue("RoadTypesForExitingWood"),
    glue::glue(">> ID    Name"),
    glue::glue(">> --    ----"),
    rows,
    glue::glue("")
  )
}

#' Allowed Forest Roads Simulation `HeuristicForNetworkConstruction` values
#'
#' Canonical user-guide spelling. The upstream parser does case-sensitive
#' string comparison against `"Random"`, `"Closestfirst"`, `"Farthestfirst"`,
#' so [.frsHeuristicForWrite()] translates these canonical names to the
#' parser-accepted form before they are written to the extension's input file.
#'
#' @keywords internal
.frsHeuristics <- c("Random", "ClosestFirst", "FarthestFirst")

#' Translate a heuristic name to the upstream-parser form
#'
#' @param x Character. A value from [.frsHeuristics].
#'
#' @returns Character. The lowercase-`"first"` form expected by the FRS
#'   extension's input parser.
#'
#' @keywords internal
.frsHeuristicForWrite <- function(x) {
  switch(x, Random = "Random", ClosestFirst = "Closestfirst", FarthestFirst = "Farthestfirst", x)
}
