#' Create  Succession Input File
#'
#' Follows the  Biomass Succession User Guide.
#'
#' @template param_path
#'
#' @param ... arguments passed to other functions:
#'   - `CalibrateMode`;
#'   - `ClimateConfigFile`;
#'   - `EcoregionParametersFiles`;
#'   - `FireReductionParameters`;
#'   - `HarvestReductionParameters`;
#'   - `InitialCommunitiesFiles`;
#'   - `MinRelativeBiomass`;
#'   - `SeedingAlgorithm`;
#'   - `SpeciesDataFile`;
#'   - `SpeciesEcoregionDataFile`;
#'   - `SufficientLight`;
#'   - `Timestep`;
#'
#' @export
BiomassSuccessionInput <- function(path, ...) {
  stopifnot(
    !is.null(path)
  )

  dots <- list(...)
  stopifnot(
    !is.null(dots$FireReductionParameters)
  )

  file <- file.path(path, "biomass_succession.txt")
  writeLines(c(
    LandisData("Biomass Succession"),
    insertTimestep(dots$Timestep),
    insertSeedingAlgorithm(dots$SeedingAlgorithm),
    insertInitialCommunities(dots$InitialCommunitiesFiles),
    insertClimateConfigFile(dots$ClimateConfigFile),
    insertCalibrateMode(dots$CalibrateMode),
    insertMinRelativeBiomass(dots$MinRelativeBiomass),
    insertSufficientLight(dots$SufficientLight),
    insertSpeciesDataFile(dots$SpeciesDataFile),
    insertEcoregionParameters(dots$EcoregionParametersFiles),
    insertSpeciesEcoregionDataFile(dots$SpeciesEcoregionDataFile),
    insertFireReductionParameters(dots$FireReductionParameters), ## TODO
    insertHarvestReductionParameters(dots$HarvestReductionParameters) ## TODO
  ), file)

  return(file)
}

#' Specify  Biomass Succession Extension `SeedingAlgorithm`
#'
#' @param algo Seed dispersal algorithm to use.
#' One of `"WardSeedDispersal"`, `"NoDispersal"`, `"UniversalDispersal"`.
#'
#' @template return_insert
#'
#' @export
#'
insertSeedingAlgorithm <- function(algo = NULL) {
  allowed <- c("WardSeedDispersal", "NoDispersal", "UniversalDispersal")
  algo <- algo %||% allowed[1]

  stopifnot(algo %in% allowed)

  c(
    glue::glue("SeedingAlgorithm    {algo}"),
    glue::glue("") ## add blank line after each item group
  )
}

#' Specify Biomass Succession Extension `insertCalibrateMode`
#'
#' @param cal_mode Logical. Optional parameter.
#'
#' @template return_insert
#'
#' @export
#'
insertCalibrateMode <- function(cal_mode = FALSE) {
  if (isTRUE(cal_mode)) {
    c(
      glue::glue("CalibrateMode    yes << optional parameter"),
      glue::glue("") ## add blank line after each item group
    )
  } else {
    c(
      glue::glue("CalibrateMode    no << optional parameter"),
      glue::glue("") ## add blank line after each item group
    )
  }
}

#' Prepare Biomass Succession Extension `MinRelativeBiomass` Table
#'
#' @param df data.frame corresponding to `MinRelativeBiomass` table
#'
#' @returns data.frame
#'
#' @export
prepMinRelativeBiomass <- function(df = NULL) {
  browser() ## TODO: don't use by ecoregionGroup but by ecoregion!
  if (colnames(df) == c("ecoregionGroup", "X1", "X2", "X3", "X4", "X5")) {
    df <- t(df)
  }

  return(df)
}

#' Specify Biomass Succession Extension `MinRelativeBiomass` Table
#'
#' @param df data.frame corresponding to `MinRelativeBiomass` table
#'
#' @template return_insert
#'
#' @export
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
    glue::glue(">> MinRelativeBiomass"),
    glue::glue(">> Shade Class    Ecoregions"),
    glue::glue(">> -----------    ------------------------"),
    paste0(
      "   1              ",
      glue::glue("{df[1, ]}") |> glue::glue_collapse(sep = "  ") ## TODO: is the '%' needed?
    ),
    paste0(
      "   2              ",
      glue::glue("{df[2, ]}") |> glue::glue_collapse(sep = "  ")
    ),
    paste0(
      "   3              ",
      glue::glue("{df[3, ]}") |> glue::glue_collapse(sep = "  ")
    ),
    paste0(
      "   4              ",
      glue::glue("{df[4, ]}") |> glue::glue_collapse(sep = "  ")
    ),
    paste0(
      "   5              ",
      glue::glue("{df[5, ]}") |> glue::glue_collapse(sep = "  ")
    ),
    paste0(
      "   6              ",
      glue::glue("{df[6, ]}") |> glue::glue_collapse(sep = "  ")
    ),
    glue::glue("") ## add blank line after each item group
  )
}

#' Specify Biomass Succession Extension `SufficientLight` Table
#'
#' @param df data.frame corresponding to the `SufficientLight` table
#'
#' @template return_insert
#'
#' @export
insertSufficientLight <- function(df) {
  c(
    glue::glue("SufficientLight"),
    glue::glue(">> Shade Class  Probability by Actual Shade"),
    glue::glue(">> -----------  ---------------------------"),
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
  df <- df |>
    dplyr::filter(active == "yes") |>
    dplyr::mutate(
      Ecoregions = as.character(ecoregionGroup),
      AET = 600, ## TODO: need values from data
      active = NULL,
      ecoregionGroup = NULL
    )

  return(df)
}

#' Specify `EcoregionParameters` table
#'
#' @param df
#'
#' @template return_insert
#'
#' @export
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
#' @template return_file
#'
#' @export
#'
prepSpeciesEcoregionDataFile <- function(df, path) {
  df <- df |>
    dplyr::mutate(
      Year = 0,
      EcoregionName = ecoregionGroup,
      SpeciesCode = speciesCode,
      ProbEstablish = establishprob,
      ProbMortality = 0.0, ## TODO: this is missing???
      ANPPmax = maxANPP,
      BiomassMax = maxB,
      .keep = "used"
    )

  file <- file.path(path, "species-ecoregion.csv") ## TODO
  write.csv(df, file)

  return(file)
}

#' Specify `SpeciesEcoregionData` File
#'
#' @param file
#'
#' @template return_insert
#'
#' @export
insertSpeciesEcoregionDataFile <- function(file) {
  c(
    glue::glue("SpeciesEcoregionDataFile    \"{file}\""),
    glue::glue("") ## add blank line after each item group
  )
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
#' @export
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
#' @export
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
