#' Biomass Succession Extension
#'
#' @include ext_utils.R
#'
#' @references LANDIS-II Biomass Succession v7 Extension User Guide
#' <https://github.com/LANDIS-II-Foundation/Extension-Biomass-Succession/blob/master/docs/LANDIS-II%20Biomass%20Succession%20v7%20User%20Guide.pdf>
#'
#' @seealso
#' Helpers that prepare inputs for this extension:
#' [prepMinRelativeBiomass()],
#' [prepEcoregionParameters()],
#' [prepSpeciesEcoregionDataFile()],
#' [prepFireReductionParameters()],
#' [prepHarvestReductionParameters()].
#' Shared scenario inputs:
#' [prepClimateConfig()],
#' [prepInitialCommunities()],
#' [prepSpeciesData()].
#'
#' @family Biomass Succession helpers
#'
#' @export
BiomassSuccession <- R6Class(
  "BiomassSuccession",
  inherit = LandisExtension,
  public = list(
    #' @param path Character. Directory path.
    #' @param Timestep Integer.
    #' @template param_SeedingAlgorithm
    #' @param InitialCommunitiesFiles Character. Relative file paths.
    #' @param ClimateConfigFile Character. Relative file path.
    #' @param CalibrateMode Logical, or character indicating "yes" or "no".
    #' @param SpinupCohorts Logical, or character indicating "yes" or "no".
    #' @param SpinupMortalityFraction Real.
    #' @param MinRelativeBiomass `data.frame`.
    #' @param SufficientLight `data.frame`.
    #' @param SpeciesDataFile Character. Relative file path.
    #' @param EcoregionParameters `data.frame`.
    #' @param SpeciesEcoregionDataFile Character. Relative file path.
    #' @param FireReductionParameters `data.frame`.
    #' @param HarvestReductionParameters `data.frame`.
    initialize = function(
      path = NULL,
      Timestep = 10L,
      SeedingAlgorithm = NULL,
      InitialCommunitiesFiles = NULL,
      ClimateConfigFile = NULL,
      CalibrateMode = NULL,
      SpinupCohorts = NULL,
      SpinupMortalityFraction = NULL,
      MinRelativeBiomass = NULL,
      SufficientLight = NULL,
      SpeciesDataFile = NULL,
      EcoregionParameters = NULL,
      SpeciesEcoregionDataFile = NULL,
      FireReductionParameters = NULL,
      HarvestReductionParameters = NULL
    ) {
      stopifnot(!is.null(path))

      ## LandisExtension fields
      private$.LandisData <- "Biomass Succession"
      self$Timestep <- Timestep

      self$type <- "succession"
      self$path <- path
      self$files <- "biomass-succession.txt" ## file won't exist yet

      ## additional fields for this extension
      self$SeedingAlgorithm <- SeedingAlgorithm %||% "WardSeedDispersal"
      self$InitialCommunitiesFiles <- InitialCommunitiesFiles
      self$ClimateConfigFile <- ClimateConfigFile
      self$CalibrateMode <- CalibrateMode %||% FALSE
      self$SpinupCohorts <- SpinupCohorts
      self$SpinupMortalityFraction <- SpinupMortalityFraction
      self$MinRelativeBiomass <- MinRelativeBiomass
      self$SufficientLight <- SufficientLight
      self$SpeciesDataFile <- SpeciesDataFile
      self$EcoregionParameters <- EcoregionParameters
      self$SpeciesEcoregionDataFile <- SpeciesEcoregionDataFile
      self$FireReductionParameters <- FireReductionParameters
      self$HarvestReductionParameters <- HarvestReductionParameters
    },

    #' @description Write extension inputs to disk
    write = function() {
      stopifnot(!is.null(self$FireReductionParameters))

      writeLines(
        c(
          insertLandisData(private$.LandisData),
          insertValue("Timestep", self$Timestep),
          insertValue("SeedingAlgorithm", self$SeedingAlgorithm),
          insertInitialCommunities(self$InitialCommunitiesFiles), ## TODO
          insertFile("ClimateConfigFile", self$ClimateConfigFile),
          insertValue("CalibrateMode", self$CalibrateMode),
          ## SpinupCohorts / SpinupMortalityFraction are OPTIONAL and absent from the Core8
          ## CoreV8.0-BiomassSuccession7.0 grammar -- the v8 parser aborts ("Found SpinupCohorts but
          ## expected MinRelativeBiomass") if they appear. Only emit them when explicitly set.
          if (!is.null(self$SpinupCohorts)) insertValue("SpinupCohorts", self$SpinupCohorts),
          if (!is.null(self$SpinupMortalityFraction)) {
            insertValue("SpinupMortalityFraction", self$SpinupMortalityFraction)
          },
          insertMinRelativeBiomass(self$MinRelativeBiomass),
          insertSufficientLight(self$SufficientLight),
          insertSpeciesDataFile(self$SpeciesDataFile, core = FALSE),
          insertEcoregionParameters(self$EcoregionParameters),
          insertSpeciesEcoregionDataFile(self$SpeciesEcoregionDataFile),
          insertFireReductionParameters(self$FireReductionParameters),
          insertHarvestReductionParameters(self$HarvestReductionParameters)
        ),
        file.path(self$path, self$files[1])
      )

      self$add_file(self$ClimateConfigFile)
      self$add_file(self$InitialCommunitiesFiles)
      self$add_file(self$SpeciesDataFile)
      self$add_file(self$SpeciesEcoregionDataFile)

      return(invisible(self))
    }
  ),

  private = list(
    .SeedingAlgorithm = NULL,
    .InitialCommunitiesFiles = NULL,
    .ClimateConfigFile = NULL,
    .CalibrateMode = NULL,
    .SpinupCohorts = NULL,
    .SpinupMortalityFraction = NULL,
    .MinRelativeBiomass = NULL,
    .SufficientLight = NULL,
    .SpeciesDataFile = NULL,
    .EcoregionParameters = NULL,
    .SpeciesEcoregionDataFile = NULL,
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

    #' @field InitialCommunitiesFiles Character. Relative file paths.
    InitialCommunitiesFiles = function(value) {
      if (missing(value)) {
        return(private$.InitialCommunitiesFiles)
      } else {
        private$.InitialCommunitiesFiles <- .relPath(value, self$path)
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

    #' @field CalibrateMode Logical, or character indicating "yes" or "no".
    CalibrateMode = function(value) {
      if (missing(value)) {
        return(private$.CalibrateMode)
      } else {
        private$.CalibrateMode <- yesno(value)
      }
    },

    #' @field SpinupCohorts Logical, or character indicating "yes" or "no". Optional (NULL = omit).
    SpinupCohorts = function(value) {
      if (missing(value)) {
        return(private$.SpinupCohorts)
      } else {
        private$.SpinupCohorts <- if (is.null(value)) NULL else yesno(value)
      }
    },

    #' @field SpinupMortalityFraction Real. Optional (NULL = omit).
    SpinupMortalityFraction = function(value) {
      if (missing(value)) {
        return(private$.SpinupMortalityFraction)
      } else if (is.null(value)) {
        private$.SpinupMortalityFraction <- NULL
      } else {
        stopifnot(value >= 0.0, value <= 0.5)
        private$.SpinupMortalityFraction <- value
      }
    },

    #' @field MinRelativeBiomass `data.frame`.
    MinRelativeBiomass = function(value) {
      if (missing(value)) {
        return(private$.MinRelativeBiomass)
      } else {
        private$.MinRelativeBiomass <- value
      }
    },

    #' @field SufficientLight `data.frame`.
    SufficientLight = function(value) {
      if (missing(value)) {
        return(private$.SufficientLight)
      } else {
        private$.SufficientLight <- value
      }
    },

    #' @field SpeciesDataFile Character. Relative file path.
    SpeciesDataFile = function(value) {
      if (missing(value)) {
        return(private$.SpeciesDataFile)
      } else {
        private$.SpeciesDataFile <- .relPath(value, self$path)
      }
    },

    #' @field EcoregionParameters `data.frame`.
    EcoregionParameters = function(value) {
      if (missing(value)) {
        return(private$.EcoregionParameters)
      } else {
        private$.EcoregionParameters <- value
      }
    },

    #' @field SpeciesEcoregionDataFile Character. Relative file path.
    SpeciesEcoregionDataFile = function(value) {
      if (missing(value)) {
        return(private$.SpeciesEcoregionDataFile)
      } else {
        private$.SpeciesEcoregionDataFile <- .relPath(value, self$path)
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

#' Prepare Biomass Succession Extension `MinRelativeBiomass` Table
#'
#' @param df data.frame corresponding to `MinRelativeBiomass` table
#'
#' @returns data.frame
#'
#' @family Biomass Succession helpers
#'
#' @export
prepMinRelativeBiomass <- function(df = NULL) {
  stopifnot(!is.null(df))

  if (identical(colnames(df), c("ecoregionGroup", "X1", "X2", "X3", "X4", "X5"))) {
    erg <- as.character(df$ecoregionGroup)
    mrb <- apply(df[, -1], 2, function(x) {
      ## values are percents; the `%` symbol is required by LANDIS-II
      val <- x * 100
      stopifnot(val >= 0, val <= 100)
      paste0(val, "%")
    })
    out <- cbind(erg, mrb) |> t() |> data.frame()
  }

  return(out)
}

#' Specify Biomass Succession Extension `MinRelativeBiomass` Table
#'
#' @param df data.frame corresponding to `MinRelativeBiomass` table
#'
#' @template return_insert
#'
#' @family Biomass Succession helpers
#'
insertMinRelativeBiomass <- function(df = NULL) {
  stopifnot(!is.null(df), is(df, "data.frame"))

  # MinRelativeBiomass
  # >> Shade
  # >> Class Ecoregions
  # >> ----- ------------
  #          eco1 eco2
  #        1 25% 20%
  #        2 35% 30%
  #        3 45% 40%
  #        4 60% 50%
  #        5 95% 80%

  ## Drop the leading `ShadeClass` column: row 1 carries the ecoregion labels
  ## and rows 2-6 carry the percent-biomass values per ecoregion. The shade
  ## class label (1..5) is hard-coded as the row prefix in the output, so the
  ## column itself must not appear in the joined values.
  c(
    glue::glue("MinRelativeBiomass"),
    glue::glue(">> Shade Class    Ecoregions"),
    glue::glue(">> -----------    ------------------------"),
    paste0("                  ", .collapseRow(df, 1)),
    paste0("   1              ", .collapseRow(df, 2)),
    paste0("   2              ", .collapseRow(df, 3)),
    paste0("   3              ", .collapseRow(df, 4)),
    paste0("   4              ", .collapseRow(df, 5)),
    paste0("   5              ", .collapseRow(df, 6)),
    glue::glue("") ## add blank line after each item group
  )
}

#' Specify Biomass Succession Extension `SufficientLight` Table
#'
#' @param df data.frame corresponding to the `SufficientLight` table
#'
#' @template return_insert
#'
#' @family Biomass Succession helpers
#'
insertSufficientLight <- function(df) {
  ## Per-cell formatter: `vapply` walks columns one at a time, so each `x` is
  ## a length-1 vector — safe to pass through `as.numeric()` without the
  ## tibble-row pitfall (`format(<tibble>, ...)` would print the tibble).
  fmt <- function(x) format(as.numeric(x), nsmall = 2)
  c(
    glue::glue("SufficientLight"),
    glue::glue(">> Shade Class  Probability by Actual Shade"),
    glue::glue(">> -----------  ----------------------------------"),
    glue::glue(">>              0     1     2     3     4     5"),
    paste0("   1            ", .collapseRow(df, 1, fmt = fmt)),
    paste0("   2            ", .collapseRow(df, 2, fmt = fmt)),
    paste0("   3            ", .collapseRow(df, 3, fmt = fmt)),
    paste0("   4            ", .collapseRow(df, 4, fmt = fmt)),
    paste0("   5            ", .collapseRow(df, 5, fmt = fmt)),
    glue::glue("") ## add blank line after each item group
  )
}

#' Create Ecoregion Parameters Table
#'
#' @param df data.frame corresponding to the ecoregion parameters table
#'
#' @returns data.frame
#'
#' @family Biomass Succession helpers
#'
#' @export
prepEcoregionParameters <- function(df) {
  ## sum by year, then take the mean annual total (as integer)
  df <- dplyr::group_by(df, Year) |>
    dplyr::summarise_if(is.numeric, sum) |>
    dplyr::mutate(Month = NULL) |>
    dplyr::ungroup() |>
    dplyr::summarise_all(mean) |>
    round(0) |>
    dplyr::mutate(Year = NULL) |>
    tidyr::pivot_longer(cols = everything(), names_to = "Ecoregions", values_to = "AET")

  return(df)
}

#' Specify `EcoregionParameters` table
#'
#' @param df data.frame
#'
#' @template return_insert
#'
#' @family Biomass Succession helpers
#'
insertEcoregionParameters <- function(df) {
  c(
    glue::glue("EcoregionParameters"),
    glue::glue(">>    AET (mm)"),
    apply(df, 1, function(x) {
      glue::glue_collapse(x, sep = "  ")
    }),
    glue::glue("") ## add blank line after each item group
  )
}

#' Create Species Ecoregion Data File
#'
#' @param df data.frame
#'
#' @template param_path
#'
#' @template return_file
#'
#' @family Biomass Succession helpers
#'
#' @export
prepSpeciesEcoregionDataFile <- function(df, path) {
  df <- df |>
    dplyr::mutate(
      ecoregionGroup = as.character(ecoregionGroup),
      ProbMortality = 0.0, ## TODO: this is missing???
      year = as.integer(year)
    ) |>
    dplyr::relocate(
      ## re-order columns while renaming them
      Year = year,
      EcoregionName = ecoregionGroup,
      SpeciesCode = speciesCode,
      ProbEstablish = establishprob,
      ProbMortality,
      ANPPmax = maxANPP,
      BiomassMax = maxB
    ) |>
    na.omit()

  file <- file.path(path, "species-ecoregion.csv") ## TODO
  write.csv(df, file, row.names = FALSE)

  return(file)
}

#' Specify `SpeciesEcoregionData` File
#'
#' @template param_file
#'
#' @template return_insert
#'
#' @family Biomass Succession helpers
#'
insertSpeciesEcoregionDataFile <- function(file) {
  insertFile("SpeciesEcoregionDataFile", file)
}

#' Prepare Biomass Succession Extension `FireReductionParameters` Table
#'
#' @param df `data.frame` with columns `FireSeverity`, `WoodReduction`, and `LitterReduction`.
#'
#' @returns data.frame
#'
#' @family Biomass Succession helpers
#'
#' @export
prepFireReductionParameters <- function(df = NULL) {
  if (is.null(df)) {
    df <- data.table(
      FireSeverity = 1L:5L,
      ## TODO: need WoodReduction and LitterReduction from data
      WoodReduction = c(0.00, 0.00, 0.00, 0.00, 0.00), ## [0, 1]
      LitterReduction = c(0.00, 0.25, 0.50, 0.75, 1.00) ## [0, 1]
    )
  } else {
    df <- as.data.table(df)
  }

  df[, FireSeverity := as.integer(FireSeverity)]
  stopifnot(
    all(df[["WoodReduction"]] >= 0.0),
    all(df[["WoodReduction"]] <= 1.0),
    all(df[["LitterReduction"]] >= 0.0),
    all(df[["LitterReduction"]] <= 1.0)
  )

  return(as.data.frame(df))
}

#' Specify Biomass Succession Extension `FireReductionParameters` Table
#'
#' @param df data.frame
#'
#' @template return_insert
#'
#' @family Biomass Succession helpers
#'
insertFireReductionParameters <- function(df) {
  c(
    glue::glue("FireReductionParameters"),
    glue::glue(">>  Severity    WoodLitter    Litter"),
    glue::glue(">>  Fire        Reduction     Reduction"),
    apply(df, 1, function(x) {
      glue::glue_collapse(x, sep = "  ")
    }),
    glue::glue("") ## add blank line after each item group
  )
}

#' Prepare Biomass Succession Extension `HarvestReductionParameters` Table
#'
#' @param df data.frame
#'
#' @returns data.frame
#'
#' @family Biomass Succession helpers
#'
#' @export
prepHarvestReductionParameters <- function(df = NULL) {
  if (is.null(df)) {
    ## TODO: need defaults from data rather than landis test file
    df <- data.table(
      PrescriptionName = c("MaxAgeClearcut", "PatchCutting"),
      DeadWoodReduction = c(0.5, 1.0),
      DeadLitterReduction = c(0.15, 1.0),
      CohortWoodRemoval = c(0.8, 1.0),
      CohortLeafRemoval = c(0.0, 0.0)
    )
  } else {
    df <- as.data.table(df)
  }

  ## enforce typing and bounds
  stopifnot(
    all(df[["DeadWoodReduction"]] >= 0.0),
    all(df[["DeadWoodReduction"]] <= 1.0),
    all(df[["DeadLitterReduction"]] >= 0.0),
    all(df[["DeadLitterReduction"]] <= 1.0),
    all(df[["CohortWoodRemoval"]] >= 0.0),
    all(df[["CohortWoodRemoval"]] <= 1.0),
    all(df[["CohortLeafRemoval"]] >= 0.0),
    all(df[["CohortLeafRemoval"]] <= 1.0)
  )

  return(as.data.frame(df))
}

#' Specify Biomass Succession Extension `HarvestReductionParameters` Table
#'
#' @param df data.frame
#'
#' @template return_insert
#'
#' @family Biomass Succession helpers
#'
insertHarvestReductionParameters <- function(df) {
  c(
    glue::glue("HarvestReductionParameters"),
    glue::glue(">>      Name    DeadWood    DeadLitter    Cohort        Cohort"),
    glue::glue(">>              Reduction   Reduction     WoodRemoval   LeafRemoval"),
    apply(df, 1, function(x) {
      glue::glue_collapse(x, sep = "      ")
    }),
    glue::glue("") ## add blank line after each item group
  )
}
