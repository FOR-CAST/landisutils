#' Biomass Succession Extension
#'
#' @include ext_utils.R
#'
#' @references LANDIS-II Biomass Succession v7 Extension User Guide
#' <https://github.com/LANDIS-II-Foundation/Extension-Biomass-Succession/blob/master/docs/LANDIS-II%20Biomass%20Succession%20v7%20User%20Guide.pdf>
#'
#' @export
BiomassSuccession <- R6Class(
  "DynamicFuels",
  inherit = LandisExtension,
  public = list(
    #' @param path Character. Directory path.
    #' @param Timestep Integer.
    #' @param SeedingAlgorithm Character. Dispersal algorithm to use.
    #'        One of `"WardSeedDispersal"`, `"NoDispersal"`, `"UniversalDispersal"`.
    #' @param InitialCommunitiesFiles Character. Relative file paths.
    #' @param ClimateConfigFile Character. Relative file path.
    #' @param CalibrateMode Logical, or character indicating "yes" or "no".
    #' @param MinRelativeBiomass `data.frame`.
    #' @param SufficientLight `data.frame`.
    #' @param SpeciesDataFile Character. Relative file path.
    #' @param EcoregionParameters `data.frame`.
    #' @param SpeciesEcoregionDataFile Character. Relative file path.
    #' @param FireReductionParameters `data.frame`.
    #' @param HarvestReductionParameters `data.frame`.
    initialize = function(
      SeedingAlgorithm = NULL,
      InitialCommunitiesFiles = NULL,
      ClimateConfigFile = NULL,
      CalibrateMode = FALSE,
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

      self$type <- "disturbance"
      self$path <- path
      self$files <- "biomass-succession.txt" ## file won't exist yet

      ## additional fields for this extension
      self$SeedingAlgorithm <- SeedingAlgorithm %||% "WardSeedDispersal"
      self$InitialCommunitiesFiles <- InitialCommunitiesFiles
      self$ClimateConfigFile <- ClimateConfigFile
      self$CalibrateMode <- CalibrateMode
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

      ext$add_file(self$ClimateConfigFile)
      ext$add_file(self$InitialCommunitiesFiles)
      ext$add_file(self$SpeciesDataFile)
      ext$add_file(self$SpeciesEcoregionDataFile)

      return(invisible(self))
    }
  ),

  private = list(
    .SeedingAlgorithm = NULL,
    .InitialCommunitiesFiles = NULL,
    .ClimateConfigFile = NULL,
    .CalibrateMode = NULL,
    .MinRelativeBiomass = NULL,
    .SufficientLight = NULL,
    .SpeciesDataFile = NULL,
    .EcoregionParameters = NULL,
    .SpeciesEcoregionDataFile = NULL,
    .FireReductionParameters = NULL,
    .HarvestReductionParameters = NULL
  ),

  active = list(
    #' @field SeedingAlgorithm Character. Dispersal algorithm to use.
    #' One of `"WardSeedDispersal"`, `"NoDispersal"`, `"UniversalDispersal"`.
    SeedingAlgorithm = function(value) {
      if (missing(value)) {
        return(private$.SeedingAlgorithm)
      } else {
        stopifnot(value %in% c("WardSeedDispersal", "NoDispersal", "UniversalDispersal"))

        private$.SeedingAlgorithm <- value
      }
    },

    #' @field InitialCommunitiesFiles Character. Relative file paths.
    InitialCommunitiesFiles = function(value) {
      if (missing(value)) {
        return(private$.InitialCommunitiesFiles)
      } else {
        private$.InitialCommunitiesFiles <- .relPath(value, path)
      }
    },

    #' @field ClimateConfigFile Character. Relative file path.
    ClimateConfigFile = function(value) {
      if (missing(value)) {
        return(private$.ClimateConfigFile)
      } else {
        private$.ClimateConfigFile <- .relPath(value, path)
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
        private$.SpeciesDataFile <- .relPath(value, path)
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
        private$.SpeciesEcoregionDataFile <- .relPath(value, path)
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

  c(
    glue::glue("MinRelativeBiomass"),
    glue::glue(">> Shade Class    Ecoregions"),
    glue::glue(">> -----------    ------------------------"),
    paste0("                  ", glue::glue("{df[1, ]}") |> glue::glue_collapse(sep = "  ")),
    paste0("   1              ", glue::glue("{df[2, ]}") |> glue::glue_collapse(sep = "  ")),
    paste0("   2              ", glue::glue("{df[3, ]}") |> glue::glue_collapse(sep = "  ")),
    paste0("   3              ", glue::glue("{df[4, ]}") |> glue::glue_collapse(sep = "  ")),
    paste0("   4              ", glue::glue("{df[5, ]}") |> glue::glue_collapse(sep = "  ")),
    paste0("   5              ", glue::glue("{df[6, ]}") |> glue::glue_collapse(sep = "  ")),
    glue::glue("") ## add blank line after each item group
  )
}

#' Specify Biomass Succession Extension `SufficientLight` Table
#'
#' @param df data.frame corresponding to the `SufficientLight` table
#'
#' @template return_insert
#'
insertSufficientLight <- function(df) {
  c(
    glue::glue("SufficientLight"),
    glue::glue(">> Shade Class  Probability by Actual Shade"),
    glue::glue(">> -----------  ----------------------------------"),
    glue::glue(">>              0     1     2     3     4     5"),
    paste0(
      "   1            ",
      glue::glue("{format(df[1, -1], nsmall = 2)}") |> glue::glue_collapse(sep = "  ")
    ),
    paste0(
      "   2            ",
      glue::glue("{format(df[2, -1], nsmall = 2)}") |> glue::glue_collapse(sep = "  ")
    ),
    paste0(
      "   3            ",
      glue::glue("{format(df[3, -1], nsmall = 2)}") |> glue::glue_collapse(sep = "  ")
    ),
    paste0(
      "   4            ",
      glue::glue("{format(df[4, -1], nsmall = 2)}") |> glue::glue_collapse(sep = "  ")
    ),
    paste0(
      "   5            ",
      glue::glue("{format(df[5, -1], nsmall = 2)}") |> glue::glue_collapse(sep = "  ")
    ),
    glue::glue("") ## add blank line after each item group
  )
}

#' Create Ecoregion Parameters Table
#'
#' @param df data.frame corresponding to the ecoregion parameters table
#'
#' @returns data.frame
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
insertSpeciesEcoregionDataFile <- function(file) {
  insertFile("SpeciesEcoregionDataFile", file)
}

#' Prepare Biomass Succession Extension `FireReductionParameters` Table
#'
#' @param df `data.frame` with columns `FireSeverity`, `WoodReduction`, and `LitterReduction`.
#'
#' @returns data.frame
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
