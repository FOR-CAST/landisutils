#' Species Data File
#'
#' @param df data.frame corresponding to the species data table
#'
#' @template param_path
#'
#' @param type character, corresponding to one fo the following types:
#'             - "core": generates core species data (`.txt`) file;
#'             - "fire": generates `.csv` version for use with fire extensions;
#'             - "succession": generates `.csv` version for use with succession extensions;
#'
#' @template return_file
#'
#' @aliases prepSpecies_CSV_File prepSpeciesInputFile
#' @export
prepSpeciesData <- function(df = NULL, path = NULL, type = NULL) {
  stopifnot(
    !is.null(df),
    !is.null(path),
    !is.null(type), type %in% c("core", "fire", "succession")
  )
  path <- .checkPath(path)

  SpeciesData <- df |>
    dplyr::select(
      ## drop these columns
      !c(Area, hardsoft, speciesCode, mANPPproportion, inflationFactor, growthCurveSource)
    ) |>
    dplyr::rename(
      SpeciesCode = species,

      ## core parameters
      Longevity = longevity,
      SexualMaturity = sexualmature,
      SeedDispDistEff = seeddistance_eff,
      SeedDispDistMax = seeddistance_max,
      VegReprodProb = resproutprob,
      SproutAgeMin = resproutage_min,
      SproutAgeMax = resproutage_max,
      PostFireRegen = postfireregen,

      ## Succession parameters
      LeafLongevity = leaflongevity,
      WoodDecayRate = wooddecayrate,
      MortalityCurve = mortalityshape,
      GrowthCurve = growthcurve,
      LeafLignin = leafLignin,
      ShadeTolerance = shadetolerance,
      FireTolerance = firetolerance    ## also used for fire
    ) |>
    dplyr::mutate(
      ShadeTolerance = as.integer(ShadeTolerance),
      FireTolerance = as.integer(FireTolerance)
    )

  if (type == "core") {
    SpeciesData <- SpeciesData |>
      dplyr::select(SpeciesCode, Longevity, SexualMaturity, SeedDispDistEff, SeedDispDistMax,
                    VegReprodProb, SproutAgeMin, SproutAgeMax, PostFireRegen)
    file <- file.path(path, "species-core.txt")
    writeLines(c(
      LandisData("Species"),
      glue::glue(">> {glue::glue_collapse(colnames(SpeciesData), sep = '  ')}"),
      glue::glue(">> {glue::glue_collapse(rep('----------  ', ncol(SpeciesData)))}"),
      apply(SpeciesData, MARGIN = 1, FUN = function(x) {
        glue::glue("   {x}") |> glue::glue_collapse(sep = "   ")
      }),
      glue::glue("")
    ),
    file)
  } else if (type == "fire") {
    SpeciesData <- SpeciesData |>
      dplyr::select(SpeciesCode, FireTolerance)
    file <- file.path(path, "species-original-fire.csv")
    write.csv(SpeciesData, file, row.names = FALSE)
  } else if (type == "succession") {
    SpeciesData <- SpeciesData |>
      dplyr::select(SpeciesCode, LeafLongevity, WoodDecayRate, MortalityCurve, GrowthCurve,
                    LeafLignin, ShadeTolerance, FireTolerance)
    file <- file.path(path, "species.csv")
    write.csv(SpeciesData, file, row.names = FALSE)
  }

  return(file)
}

#' Specify Species Data File
#'
#' @template param_file
#'
#' @param core logical, indicating whether to insert species input data file (`Species` if `TRUE`),
#'             or `SpeciesDataFile` (if `FALSE`) for use with succession extensions.
#'
#' @template return_insert
#'
#' @export
insertSpeciesDataFile <- function(file, core = NULL) {
  stopifnot(!is.null(core))

  if (isTRUE(core)) {
    c(
      glue::glue("Species    \"{file}\""),
      glue::glue("") ## add blank line after each item group
    )
  } else {
    c(
      glue::glue("SpeciesDataFile    \"{file}\""),
      glue::glue("") ## add blank line after each item group
    )
  }
}
