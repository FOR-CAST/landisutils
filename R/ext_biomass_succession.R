#' Create  Succession Input File
#'
#' Follows the  Biomass Succession User Guide.
#'
#' @param path
#'
#' @param ... arguments passed to other functions:
#'   - `CalibrateMode`;
#'   - `ClimateConfigFile` (can be NULL; optional for v7);
#'   - `EcoregionParametersFiles`;
#'   - `FireReductionParameters`;
#'   - `HarvestReductionParameters`;
#'   - `InitialCommunitiesFiles`;
#'   - `MinRelativeBiomass`;
#'   - `SeedingAlgorithm`;
#'   - `SpeciesDataFile`;
#'   - `SpeciesEcoregionDataFile`;
#'   - `SufficientLight`;
#'   - `SpinupMortalityFraction`;
#'   - `Timestep`;
#'
#' @export
BiomassSuccessionInput <- function(path, ...) {
  stopifnot(
    !is.null(path)
  )
  checkVersion(version)

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
    insertSpinupMortalityFraction(dots$SpinupMortalityFraction),
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


#' Specify Biomass Succession Extension `SpinupMortalityFraction`
#'
#' @template return_insert
#'
#' @export
#'
insertSpinupMortalityFraction <- function(frac = NULL) {
  if (is.null(frac)) {
    c(
      glue::glue(">> SpinupMortalityFraction    {0.001} << optional parameter"),
      glue::glue("") ## add blank line after each item group
    )
  } else {
    c(
      glue::glue("SpinupMortalityFraction    {frac} << optional parameter"),
      glue::glue("") ## add blank line after each item group
    )
  }
}

#' Prepare Biomass Succession Extension `MinRelativeBiomass` Table
#'
#' @param df data.frame
#'
#' @template LANDIS_version
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
#' @param df data.frame
#'
#' @template LANDIS_version
#'
#' @template return_insert
#'
#' @export
#'
insertMinRelativeBiomass <- function(df = NULL) {
  stopifnot(!is.null(df), is(df, "data.frame"))

  c(
    glue::glue(">> MinRelativeBiomass"),
    glue::glue(">> Shade Class    Ecoregions"),
    glue::glue(">> -----------    ------------------------"),
    paste0(
      "   1              ",
      glue::glue("{df[1, ]}") |> glue::glue_collapse(sep = "  ")
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
#' @param df data.frame
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

#' Prepare Biomass Succession Extension `FireReductionParameters` Table
#'
#' @param df `data.frame` with columns `FireSeverity`, `WoodReduction`, and `LitterReduction`.
#'
#' @template LANDIS_version
#'
#' @returns data.frame
#'
#' @export
prepFireReductionParameters <- function(df = NULL) {
  browser() ## TODO

  if (is.null(df)) {
    df <- data.table(
      FireSeverity = 1L:5L,
      WoodReduction = c(), ## [0, 1]
      LitterReduction = c() ## [0, 1]
    )
  } else {
    df <- as.data.table(df)
  }

  df[, FireSeverity := as.integer(FireSeverity)]
  df[, between(WoodReduction, 0.0, 1.0)]
  df[, between(LitterReduction, 0.0, 1.0)]

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
  browser() ## TODO
  c(
    glue::glue("") ## add blank line after each item group
  )
}

#' Prepare Biomass Succession Extension `HarvestReductionParameters` Table
#'
#' @param df data.frame
#'
#' @template LANDIS_version
#'
#' @returns data.frame
#'
#' @export
prepHarvestReductionParameters <- function(df = NULL) {
  browser() ## TODO

  if (is.null(df)) {
    df <- data.table(
      PrescriptionName = c(), ## TODO
      DeadWoodReduction = c(), ## [0, 1]
      DeadLitterReduction = c(), ## [0, 1]
      CohortWoodRemoval = c(), ## [0, 1]
      CohortLeafRemoval = c() ## [0, 1]
    )
  } else {
    df <- as.data.table(df)
  }

  df[, FireSeverity := as.integer(FireSeverity)]
  df[, between(DeadWoodReduction, 0.0, 1.0)]
  df[, between(DeadLitterReduction, 0.0, 1.0)]
  df[, between(CohortWoodRemoval, 0.0, 1.0)]
  df[, between(CohortLeafRemoval, 0.0, 1.0)]

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
  browser() ## TODO
  c(
    glue::glue("") ## add blank line after each item group
  )
}
