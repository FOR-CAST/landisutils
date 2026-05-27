#' Biomass Harvest Extension
#'
#' @include ext_utils.R
#'
#' @references LANDIS-II Biomass Harvest v7 Extension User Guide
#'   <https://github.com/LANDIS-II-Foundation/Extension-Biomass-Harvest/blob/master/docs/LANDIS-II%20Harvest%20v7%20User%20Guide.pdf>
#'
#' @family Biomass Harvest helpers
#'
#' @export
BiomassHarvest <- R6Class(
  "BiomassHarvest",
  inherit = LandisExtension,
  public = list(
    #' @param path Character. Directory path.
    #' @param Timestep Integer.
    #' @param ManagementAreas Character. Relative file path to the management areas raster.
    #' @param Stands Character. Relative file path to the stands raster.
    #' @param Prescriptions List of `HarvestPrescription` objects (see [harvestPrescription()]).
    #' @param HarvestImplementations `data.frame` with columns `MgmtArea`, `Prescription`,
    #'   `HarvestArea`, and optionally `BeginTime` and `EndTime`.
    #' @param PrescriptionMaps (Optional) Character. Filename pattern for prescription output
    #'   maps; must contain the literal `{timestep}`.
    #' @param BiomassMaps (Optional) Character. Filename pattern for biomass-removed output
    #'   maps; must contain the literal `{timestep}`.
    #' @param EventLog Character. Relative file path for the event-level CSV log.
    #' @param SummaryLog Character. Relative file path for the summary CSV log.
    initialize = function(
      path,
      Timestep = NULL,
      ManagementAreas = NULL,
      Stands = NULL,
      Prescriptions = list(),
      HarvestImplementations = NULL,
      PrescriptionMaps = NULL,
      BiomassMaps = NULL,
      EventLog = "biomass-harvest/log.csv",
      SummaryLog = "biomass-harvest/summarylog.csv"
    ) {
      stopifnot(!is.null(path))

      ## LandisExtension fields
      ## NOTE: the v7 user guide §3.1 states this value must be "Harvest",
      ## but the currently-released extension requires "Biomass Harvest",
      ## and explicitly errors otherwise. Use "Biomass Harvest".
      ## <https://github.com/LANDIS-II-Foundation/Extension-Biomass-Harvest/issues/67>
      private$.LandisData <- "Biomass Harvest"
      self$Timestep <- Timestep

      self$type <- "disturbance"
      self$path <- path
      self$files <- "biomass-harvest.txt" ## file won't exist yet

      ## additional fields for this extension
      self$ManagementAreas <- ManagementAreas
      self$Stands <- Stands
      self$Prescriptions <- Prescriptions
      self$HarvestImplementations <- HarvestImplementations
      self$PrescriptionMaps <- PrescriptionMaps %||% MapNames("prescripts", "harvest", self$path)
      self$BiomassMaps <- BiomassMaps %||% MapNames("biomass-removed", "harvest", self$path)
      self$EventLog <- EventLog
      self$SummaryLog <- SummaryLog
    },

    #' @param value `HarvestPrescription` object to append to `$Prescriptions`.
    add_prescription = function(value) {
      stopifnot(inherits(value, "HarvestPrescription"))
      private$.Prescriptions <- c(private$.Prescriptions, list(value))
    },

    #' @description Write extension inputs to disk
    write = function() {
      stopifnot(
        !is.null(self$ManagementAreas),
        !is.null(self$Stands),
        length(self$Prescriptions) >= 1L,
        !is.null(self$HarvestImplementations),
        !is.null(self$EventLog),
        !is.null(self$SummaryLog)
      )

      writeLines(
        c(
          insertLandisData(private$.LandisData),
          insertValue("Timestep", self$Timestep),
          insertFile("ManagementAreas", self$ManagementAreas),
          insertFile("Stands", self$Stands),
          unlist(lapply(self$Prescriptions, insertPrescription)),
          insertHarvestImplementations(self$HarvestImplementations),
          if (!is.null(self$PrescriptionMaps)) {
            insertValue("PrescriptionMaps", self$PrescriptionMaps)
          },
          if (!is.null(self$BiomassMaps)) {
            insertValue("BiomassMaps", self$BiomassMaps)
          },
          insertFile("EventLog", self$EventLog),
          insertFile("SummaryLog", self$SummaryLog)
        ),
        file.path(self$path, self$files[1])
      )

      self$add_file(self$ManagementAreas)
      self$add_file(self$Stands)

      return(invisible(self))
    }
  ),

  private = list(
    .ManagementAreas = NULL,
    .Stands = NULL,
    .Prescriptions = list(),
    .HarvestImplementations = NULL,
    .PrescriptionMaps = NULL,
    .BiomassMaps = NULL,
    .EventLog = NULL,
    .SummaryLog = NULL
  ),

  active = list(
    #' @field ManagementAreas Character. Relative file path.
    ManagementAreas = function(value) {
      if (missing(value)) {
        return(private$.ManagementAreas)
      } else {
        private$.ManagementAreas <- .relPath(value, self$path)
      }
    },

    #' @field Stands Character. Relative file path.
    Stands = function(value) {
      if (missing(value)) {
        return(private$.Stands)
      } else {
        private$.Stands <- .relPath(value, self$path)
      }
    },

    #' @field Prescriptions List of `HarvestPrescription` objects.
    Prescriptions = function(value) {
      if (missing(value)) {
        return(private$.Prescriptions)
      } else {
        if (is.null(value)) {
          value <- list()
        } else if (inherits(value, "HarvestPrescription")) {
          value <- list(value)
        }
        stopifnot(is.list(value), all(vapply(value, inherits, logical(1), "HarvestPrescription")))
        private$.Prescriptions <- value
      }
    },

    #' @field HarvestImplementations `data.frame` with required columns `MgmtArea`,
    #'   `Prescription`, `HarvestArea`, and optional columns `BeginTime`, `EndTime`.
    HarvestImplementations = function(value) {
      if (missing(value)) {
        return(private$.HarvestImplementations)
      } else {
        if (!is.null(value)) {
          stopifnot(
            is.data.frame(value),
            all(c("MgmtArea", "Prescription", "HarvestArea") %in% colnames(value))
          )
        }
        private$.HarvestImplementations <- value
      }
    },

    #' @field PrescriptionMaps (Optional) Character. File pattern with literal `{timestep}`.
    PrescriptionMaps = function(value) {
      if (missing(value)) {
        return(private$.PrescriptionMaps)
      } else {
        private$.PrescriptionMaps <- value
      }
    },

    #' @field BiomassMaps (Optional) Character. File pattern with literal `{timestep}`.
    BiomassMaps = function(value) {
      if (missing(value)) {
        return(private$.BiomassMaps)
      } else {
        private$.BiomassMaps <- value
      }
    },

    #' @field EventLog Character. Relative file path.
    EventLog = function(value) {
      if (missing(value)) {
        return(private$.EventLog)
      } else {
        private$.EventLog <- .relPath(value, self$path)
      }
    },

    #' @field SummaryLog Character. Relative file path.
    SummaryLog = function(value) {
      if (missing(value)) {
        return(private$.SummaryLog)
      } else {
        private$.SummaryLog <- .relPath(value, self$path)
      }
    },

    output_files = function(value) {
      if (!missing(value)) {
        stop("`output_files` is read-only", call. = FALSE)
      }
      c(self$EventLog, self$SummaryLog)
    }
  )
)

#' Allowed Biomass Harvest stand-ranking methods
#' @keywords internal
.harvestStandRanking <- c(
  "Economic",
  "MaxCohortAge",
  "RegulateAges",
  "Random",
  "FireHazard",
  "TimeSinceDisturbance"
)

#' Allowed Biomass Harvest site-selection methods
#' @keywords internal
.harvestSiteSelection <- c("Complete", "CompleteStandSpread", "PartialStandSpread", "PatchCutting")

#' Allowed Biomass Harvest cohort-removal methods
#' @keywords internal
.harvestCohortsRemoved <- c("ClearCut", "PlantOnly", "SpeciesList")

#' Allowed Biomass Harvest adjacency types
#' @keywords internal
.harvestAdjacencyType <- c("StandAge", "TimeSinceLastHarvested")

#' Construct a Biomass Harvest prescription
#'
#' Returns a validated prescription specification. One or more of these is passed
#' to [BiomassHarvest] via `Prescriptions`.
#'
#' @param name Character. Prescription name (no spaces); referenced from
#'   `HarvestImplementations`.
#' @param StandRanking Character. One of `"Economic"`, `"MaxCohortAge"`,
#'   `"RegulateAges"`, `"Random"`, `"FireHazard"`, or `"TimeSinceDisturbance"`.
#' @param EconomicRankTable `data.frame` with columns `Species`, `EconomicRank`,
#'   `MinimumAge`. Required when `StandRanking = "Economic"`.
#' @param FireHazardTable `data.frame` with columns `FuelType`, `FuelTypeRank`.
#'   Required when `StandRanking = "FireHazard"`.
#' @param TimeSinceLastFire,TimeSinceLastWind Integer. Years. Exactly one is
#'   required when `StandRanking = "TimeSinceDisturbance"`.
#' @param MinimumAge,MaximumAge (Optional) Integer years.
#' @param StandAdjacency (Optional) Integer. Years.
#' @param AdjacencyType (Optional) Character. One of `"StandAge"` or
#'   `"TimeSinceLastHarvested"`.
#' @param AdjacencyNeighborSetAside (Optional) Integer. Years.
#' @param MinimumTimeSinceLastHarvest (Optional) Integer. Years.
#' @param ForestTypeTable (Optional) `data.frame` with columns `InclusionRule`,
#'   `AgeRange`, `PercentCells`, `Species`. Multiple species may be
#'   space-separated in the `Species` column.
#' @param PresalvageYears (Optional) Integer. Years.
#' @param SiteSelection Character. One of `"Complete"`, `"CompleteStandSpread"`,
#'   `"PartialStandSpread"`, or `"PatchCutting"`.
#' @param MinTargetSize,MaxTargetSize Numeric (hectares). Required when
#'   `SiteSelection` is `"CompleteStandSpread"` or `"PartialStandSpread"`.
#' @param PatchPercentage Numeric in (0, 100]. Required when
#'   `SiteSelection = "PatchCutting"`.
#' @param PatchSize Numeric (hectares). Required when
#'   `SiteSelection = "PatchCutting"`.
#' @param AllowOverlap,RepeatExactCells (Optional) Logical. Only applies to
#'   `SiteSelection = "PatchCutting"`.
#' @param MinTimeSinceDamage (Optional) Integer.
#' @param PreventEstablishment Logical. If `TRUE`, emits the bare
#'   `PreventEstablishment` keyword.
#' @param CohortsRemoved Character. One of `"ClearCut"`, `"PlantOnly"`, or
#'   `"SpeciesList"`.
#' @param SpeciesList `data.frame` with columns `Species` and `Cohorts`.
#'   Required when `CohortsRemoved = "SpeciesList"`.
#' @param Plant (Optional) Character vector of species codes to plant after harvest.
#' @param SingleRepeat (Optional) Integer. Interval (years) for a single-repeat
#'   harvest.
#' @param SingleRepeatCohortsRemoved (Optional) Character. `CohortsRemoved` method
#'   to use for the repeat entry; required when `SingleRepeat` is set.
#' @param SingleRepeatSpeciesList (Optional) `data.frame` like `SpeciesList`,
#'   used when `SingleRepeatCohortsRemoved = "SpeciesList"`.
#' @param SingleRepeatPlant (Optional) Character vector of species to plant on
#'   the repeat entry.
#' @param MultipleRepeat (Optional) Integer. Interval (years) for a multiple-repeat
#'   harvest.
#' @param TimesToRepeat (Optional) Integer. Caps the number of multiple-repeat
#'   iterations.
#'
#' @returns A list of class `"HarvestPrescription"`.
#'
#' @family Biomass Harvest helpers
#'
#' @export
harvestPrescription <- function(
  name,
  StandRanking = NULL,
  EconomicRankTable = NULL,
  FireHazardTable = NULL,
  TimeSinceLastFire = NULL,
  TimeSinceLastWind = NULL,
  MinimumAge = NULL,
  MaximumAge = NULL,
  StandAdjacency = NULL,
  AdjacencyType = NULL,
  AdjacencyNeighborSetAside = NULL,
  MinimumTimeSinceLastHarvest = NULL,
  ForestTypeTable = NULL,
  PresalvageYears = NULL,
  SiteSelection = NULL,
  MinTargetSize = NULL,
  MaxTargetSize = NULL,
  PatchPercentage = NULL,
  PatchSize = NULL,
  AllowOverlap = NULL,
  RepeatExactCells = NULL,
  MinTimeSinceDamage = NULL,
  PreventEstablishment = FALSE,
  CohortsRemoved = NULL,
  SpeciesList = NULL,
  Plant = NULL,
  SingleRepeat = NULL,
  SingleRepeatCohortsRemoved = NULL,
  SingleRepeatSpeciesList = NULL,
  SingleRepeatPlant = NULL,
  MultipleRepeat = NULL,
  TimesToRepeat = NULL
) {
  stopifnot(
    is.character(name),
    length(name) == 1L,
    !grepl("\\s", name),
    StandRanking %in% .harvestStandRanking,
    SiteSelection %in% .harvestSiteSelection,
    CohortsRemoved %in% .harvestCohortsRemoved
  )

  if (StandRanking == "Economic") {
    stopifnot(
      !is.null(EconomicRankTable),
      is.data.frame(EconomicRankTable),
      all(c("Species", "EconomicRank", "MinimumAge") %in% colnames(EconomicRankTable))
    )
  }
  if (StandRanking == "FireHazard") {
    stopifnot(
      !is.null(FireHazardTable),
      is.data.frame(FireHazardTable),
      all(c("FuelType", "FuelTypeRank") %in% colnames(FireHazardTable))
    )
  }
  if (StandRanking == "TimeSinceDisturbance") {
    stopifnot(sum(!is.null(TimeSinceLastFire), !is.null(TimeSinceLastWind)) == 1L)
  }
  if (!is.null(AdjacencyType)) {
    stopifnot(AdjacencyType %in% .harvestAdjacencyType)
  }

  if (SiteSelection %in% c("CompleteStandSpread", "PartialStandSpread")) {
    stopifnot(
      !is.null(MinTargetSize),
      !is.null(MaxTargetSize),
      MinTargetSize >= 0,
      MaxTargetSize >= MinTargetSize
    )
  }
  if (SiteSelection == "PatchCutting") {
    stopifnot(
      !is.null(PatchPercentage),
      !is.null(PatchSize),
      PatchPercentage > 0,
      PatchPercentage <= 100,
      PatchSize > 0
    )
  }

  if (CohortsRemoved == "SpeciesList") {
    stopifnot(
      !is.null(SpeciesList),
      is.data.frame(SpeciesList),
      all(c("Species", "Cohorts") %in% colnames(SpeciesList))
    )
  }

  if (!is.null(SingleRepeat)) {
    stopifnot(
      is.numeric(SingleRepeat),
      SingleRepeat > 0,
      !is.null(SingleRepeatCohortsRemoved),
      SingleRepeatCohortsRemoved %in% .harvestCohortsRemoved
    )
    if (SingleRepeatCohortsRemoved == "SpeciesList") {
      stopifnot(
        !is.null(SingleRepeatSpeciesList),
        is.data.frame(SingleRepeatSpeciesList),
        all(c("Species", "Cohorts") %in% colnames(SingleRepeatSpeciesList))
      )
    }
  }
  if (!is.null(MultipleRepeat)) {
    stopifnot(is.numeric(MultipleRepeat), MultipleRepeat > 0)
    if (!is.null(TimesToRepeat)) {
      stopifnot(as.integer(TimesToRepeat) > 0L)
    }
  }
  stopifnot(is.null(SingleRepeat) || is.null(MultipleRepeat))

  structure(
    list(
      name = name,
      StandRanking = StandRanking,
      EconomicRankTable = EconomicRankTable,
      FireHazardTable = FireHazardTable,
      TimeSinceLastFire = TimeSinceLastFire,
      TimeSinceLastWind = TimeSinceLastWind,
      MinimumAge = MinimumAge,
      MaximumAge = MaximumAge,
      StandAdjacency = StandAdjacency,
      AdjacencyType = AdjacencyType,
      AdjacencyNeighborSetAside = AdjacencyNeighborSetAside,
      MinimumTimeSinceLastHarvest = MinimumTimeSinceLastHarvest,
      ForestTypeTable = ForestTypeTable,
      PresalvageYears = PresalvageYears,
      SiteSelection = SiteSelection,
      MinTargetSize = MinTargetSize,
      MaxTargetSize = MaxTargetSize,
      PatchPercentage = PatchPercentage,
      PatchSize = PatchSize,
      AllowOverlap = AllowOverlap,
      RepeatExactCells = RepeatExactCells,
      MinTimeSinceDamage = MinTimeSinceDamage,
      PreventEstablishment = isTRUE(PreventEstablishment),
      CohortsRemoved = CohortsRemoved,
      SpeciesList = SpeciesList,
      Plant = Plant,
      SingleRepeat = SingleRepeat,
      SingleRepeatCohortsRemoved = SingleRepeatCohortsRemoved,
      SingleRepeatSpeciesList = SingleRepeatSpeciesList,
      SingleRepeatPlant = SingleRepeatPlant,
      MultipleRepeat = MultipleRepeat,
      TimesToRepeat = TimesToRepeat
    ),
    class = "HarvestPrescription"
  )
}

#' Specify a Biomass Harvest prescription block
#'
#' @param rx `HarvestPrescription` object (see [harvestPrescription()]).
#'
#' @template return_insert
#'
#' @family Biomass Harvest helpers
#'
#' @keywords internal
insertPrescription <- function(rx) {
  stopifnot(inherits(rx, "HarvestPrescription"))

  c(
    glue::glue(">> ----------------------------------------------------------------"),
    glue::glue("Prescription    {rx$name}"),
    glue::glue(""),
    insertStandRanking(rx),
    insertValue("MinimumAge", rx$MinimumAge %||% NA),
    insertValue("MaximumAge", rx$MaximumAge %||% NA),
    insertValue("StandAdjacency", rx$StandAdjacency %||% NA),
    insertValue("AdjacencyType", rx$AdjacencyType %||% NA),
    insertValue("AdjacencyNeighborSetAside", rx$AdjacencyNeighborSetAside %||% NA),
    insertValue("MinimumTimeSinceLastHarvest", rx$MinimumTimeSinceLastHarvest %||% NA),
    insertForestTypeTable(rx$ForestTypeTable),
    insertValue("PresalvageYears", rx$PresalvageYears %||% NA),
    insertSiteSelection(rx),
    insertValue("MinTimeSinceDamage", rx$MinTimeSinceDamage %||% NA),
    if (isTRUE(rx$PreventEstablishment)) c("PreventEstablishment", ""),
    insertCohortsRemoved(rx$CohortsRemoved, rx$SpeciesList),
    insertPlant(rx$Plant),
    insertSingleRepeat(rx),
    insertMultipleRepeat(rx)
  )
}

#' Specify the `StandRanking` portion of a Biomass Harvest prescription
#'
#' @param rx `HarvestPrescription` object.
#'
#' @template return_insert
#'
#' @family Biomass Harvest helpers
#'
#' @keywords internal
insertStandRanking <- function(rx) {
  method <- rx$StandRanking

  header <- glue::glue("StandRanking    {method}")

  if (method == "Economic") {
    c(header, "", insertEconomicRankTable(rx$EconomicRankTable))
  } else if (method == "FireHazard") {
    c(header, "", insertFireHazardTable(rx$FireHazardTable))
  } else if (method == "TimeSinceDisturbance") {
    sub <- if (!is.null(rx$TimeSinceLastFire)) {
      glue::glue("TimeSinceLastFire    {as.integer(rx$TimeSinceLastFire)}")
    } else {
      glue::glue("TimeSinceLastWind    {as.integer(rx$TimeSinceLastWind)}")
    }
    c(header, sub, "")
  } else {
    c(header, "")
  }
}

#' Specify the `EconomicRankTable` for a Biomass Harvest prescription
#'
#' @param df `data.frame` with columns `Species`, `EconomicRank`, `MinimumAge`.
#'
#' @template return_insert
#'
#' @family Biomass Harvest helpers
#'
#' @keywords internal
insertEconomicRankTable <- function(df) {
  c(
    glue::glue(">> Species    Economic Rank    Minimum Age"),
    glue::glue(">> -------    -------------    -----------"),
    apply(df[, c("Species", "EconomicRank", "MinimumAge")], 1, function(x) {
      glue::glue_collapse(x, sep = "    ")
    }),
    glue::glue("")
  )
}

#' Specify the `FireHazardTable` for a Biomass Harvest prescription
#'
#' @param df `data.frame` with columns `FuelType`, `FuelTypeRank`.
#'
#' @template return_insert
#'
#' @family Biomass Harvest helpers
#'
#' @keywords internal
insertFireHazardTable <- function(df) {
  c(
    glue::glue(">> Fuel Type Index    Fuel Type Rank"),
    glue::glue(">> ---------------    --------------"),
    apply(df[, c("FuelType", "FuelTypeRank")], 1, function(x) {
      glue::glue_collapse(x, sep = "    ")
    }),
    glue::glue("")
  )
}

#' Specify a `ForestTypeTable` for a Biomass Harvest prescription
#'
#' @param df (Optional) `data.frame` with columns `InclusionRule`, `AgeRange`,
#'   `PercentCells`, `Species`. When `NULL`, nothing is written.
#'
#' @template return_insert
#'
#' @family Biomass Harvest helpers
#'
#' @keywords internal
insertForestTypeTable <- function(df) {
  if (is.null(df)) {
    return(character(0))
  }
  stopifnot(
    is.data.frame(df),
    all(c("InclusionRule", "AgeRange", "PercentCells", "Species") %in% colnames(df))
  )

  c(
    glue::glue("ForestTypeTable"),
    glue::glue(">> InclusionRule    ageRange    percentCells    species"),
    glue::glue(">> -------------    --------    ------------    -------"),
    apply(df[, c("InclusionRule", "AgeRange", "PercentCells", "Species")], 1, function(x) {
      glue::glue_collapse(x, sep = "    ")
    }),
    glue::glue("")
  )
}

#' Specify the `SiteSelection` portion of a Biomass Harvest prescription
#'
#' @param rx `HarvestPrescription` object.
#'
#' @template return_insert
#'
#' @family Biomass Harvest helpers
#'
#' @keywords internal
insertSiteSelection <- function(rx) {
  method <- rx$SiteSelection

  line <- if (method %in% c("CompleteStandSpread", "PartialStandSpread")) {
    glue::glue("SiteSelection    {method}    {rx$MinTargetSize}    {rx$MaxTargetSize}")
  } else if (method == "PatchCutting") {
    pct <- appendPercent(as.character(rx$PatchPercentage))
    extras <- c(
      if (isTRUE(rx$AllowOverlap)) "AllowOverlap",
      if (!is.null(rx$RepeatExactCells)) {
        glue::glue("RepeatExactCells    {yesno(rx$RepeatExactCells)}")
      }
    )
    c(glue::glue("SiteSelection    PatchCutting    {pct}    {rx$PatchSize}"), extras)
  } else {
    glue::glue("SiteSelection    Complete")
  }

  c(line, "")
}

#' Specify the `CohortsRemoved` portion of a Biomass Harvest prescription
#'
#' @param method Character. One of `"ClearCut"`, `"PlantOnly"`, or `"SpeciesList"`.
#' @param species_list (Optional) `data.frame` with columns `Species` and `Cohorts`.
#'   Required when `method = "SpeciesList"`.
#'
#' @template return_insert
#'
#' @family Biomass Harvest helpers
#'
#' @keywords internal
insertCohortsRemoved <- function(method, species_list = NULL) {
  if (method == "SpeciesList") {
    c(
      glue::glue("CohortsRemoved    SpeciesList"),
      glue::glue(">> Species    Cohorts"),
      glue::glue(">> -------    ---------"),
      apply(species_list[, c("Species", "Cohorts")], 1, function(x) {
        glue::glue_collapse(x, sep = "    ")
      }),
      glue::glue("")
    )
  } else {
    c(glue::glue("CohortsRemoved    {method}"), glue::glue(""))
  }
}

#' Specify the `Plant` portion of a Biomass Harvest prescription
#'
#' @param species (Optional) Character vector of species codes.
#'
#' @template return_insert
#'
#' @family Biomass Harvest helpers
#'
#' @keywords internal
insertPlant <- function(species) {
  if (is.null(species) || length(species) == 0L) {
    return(character(0))
  }
  c(glue::glue("Plant    {glue::glue_collapse(species, sep = ' ')}"), glue::glue(""))
}

#' Specify a `SingleRepeat` harvest in a Biomass Harvest prescription
#'
#' @param rx `HarvestPrescription` object.
#'
#' @template return_insert
#'
#' @family Biomass Harvest helpers
#'
#' @keywords internal
insertSingleRepeat <- function(rx) {
  if (is.null(rx$SingleRepeat)) {
    return(character(0))
  }

  c(
    glue::glue("SingleRepeat    {as.integer(rx$SingleRepeat)}"),
    glue::glue(""),
    insertCohortsRemoved(rx$SingleRepeatCohortsRemoved, rx$SingleRepeatSpeciesList),
    insertPlant(rx$SingleRepeatPlant)
  )
}

#' Specify a `MultipleRepeat` harvest in a Biomass Harvest prescription
#'
#' @param rx `HarvestPrescription` object.
#'
#' @template return_insert
#'
#' @family Biomass Harvest helpers
#'
#' @keywords internal
insertMultipleRepeat <- function(rx) {
  if (is.null(rx$MultipleRepeat)) {
    return(character(0))
  }

  lines <- glue::glue("MultipleRepeat    {as.integer(rx$MultipleRepeat)}")
  if (!is.null(rx$TimesToRepeat)) {
    lines <- c(lines, glue::glue("TimesToRepeat    {as.integer(rx$TimesToRepeat)}"))
  }
  c(lines, glue::glue(""))
}

#' Specify the `HarvestImplementations` table
#'
#' @param df `data.frame` with required columns `MgmtArea`, `Prescription`,
#'   `HarvestArea` and optional columns `BeginTime`, `EndTime`.
#'
#' @template return_insert
#'
#' @family Biomass Harvest helpers
#'
#' @keywords internal
insertHarvestImplementations <- function(df) {
  stopifnot(is.data.frame(df), all(c("MgmtArea", "Prescription", "HarvestArea") %in% colnames(df)))

  has_begin <- "BeginTime" %in% colnames(df)
  has_end <- "EndTime" %in% colnames(df)
  cols <- c("MgmtArea", "Prescription", "HarvestArea")
  if (has_begin) {
    cols <- c(cols, "BeginTime")
  }
  if (has_end) {
    cols <- c(cols, "EndTime")
  }

  header <- if (has_end) {
    glue::glue(">> Mgmt Area    Prescription    Harvest Area    Begin Time    End Time")
  } else if (has_begin) {
    glue::glue(">> Mgmt Area    Prescription    Harvest Area    Begin Time")
  } else {
    glue::glue(">> Mgmt Area    Prescription    Harvest Area")
  }
  sep <- if (has_end) {
    glue::glue(">> ---------    ------------    ------------    ----------    --------")
  } else if (has_begin) {
    glue::glue(">> ---------    ------------    ------------    ----------")
  } else {
    glue::glue(">> ---------    ------------    ------------")
  }

  c(
    glue::glue("HarvestImplementations"),
    header,
    sep,
    apply(df[, cols, drop = FALSE], 1, function(x) {
      glue::glue_collapse(x, sep = "    ")
    }),
    glue::glue("")
  )
}
