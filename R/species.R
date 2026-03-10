#' Species Data File
#'
#' @param df data.frame corresponding to the species data table
#'
#' @param type character, corresponding to one of the following types:
#'             - "core": generates core species data (`.txt`) file;
#'             - "fire": generates `.csv` version for use with fire extensions;
#'             - "succession": generates `.csv` version for use with succession extensions;
#'
#' @template param_path
#'
#' @template param_filename
#'
#' @template return_file
#'
#' @aliases prepSpecies_CSV_File prepSpeciesInputFile
#' @export
prepSpeciesData <- function(df = NULL, type = NULL, path = NULL, filename = NULL) {
  stopifnot(!is.null(df), !is.null(path), !is.null(type), type %in% c("core", "fire", "succession"))
  path <- .checkPath(path)

  SpeciesData <- df |>
    dplyr::select(
      ## drop these columns
      -dplyr::any_of(c(
        "Area",
        "hardsoft",
        "speciesCode",
        "mANPPproportion",
        "inflationFactor",
        "growthCurveSource"
      ))
    ) |>
    dplyr::rename(dplyr::any_of(c(
      SpeciesCode = "species",

      ## core parameters
      Longevity = "longevity",
      SexualMaturity = "sexualmature",
      SeedDispDistEff = "seeddistance_eff",
      SeedDispDistMax = "seeddistance_max",
      VegReprodProb = "resproutprob",
      SproutAgeMin = "resproutage_min",
      SproutAgeMax = "resproutage_max",
      PostFireRegen = "postfireregen",

      ## Succession parameters
      LeafLongevity = "leaflongevity",
      WoodDecayRate = "wooddecayrate",
      MortalityCurve = "mortalityshape",
      GrowthCurve = "growthcurve",
      LeafLignin = "leafLignin",
      ShadeTolerance = "shadetolerance",
      FireTolerance = "firetolerance" ## also used for fire
    ))) |>
    dplyr::mutate(dplyr::across(
      dplyr::any_of(c("ShadeTolerance", "FireTolerance")),
      .fns = as.integer
    ))

  if (type == "core") {
    SpeciesData <- SpeciesData |>
      dplyr::select(
        SpeciesCode,
        Longevity,
        SexualMaturity,
        SeedDispDistEff,
        SeedDispDistMax,
        VegReprodProb,
        SproutAgeMin,
        SproutAgeMax,
        PostFireRegen
      )
    filename <- filename %||% "species-core.txt"
    file <- file.path(path, filename)
    writeLines(
      c(
        insertLandisData("Species"),
        glue::glue(">> {glue::glue_collapse(colnames(SpeciesData), sep = '  ')}"),
        glue::glue(">> {glue::glue_collapse(rep('----------  ', ncol(SpeciesData)))}"),
        apply(SpeciesData, MARGIN = 1, FUN = function(x) {
          glue::glue("   {x}") |> glue::glue_collapse(sep = "   ")
        }),
        glue::glue("")
      ),
      file
    )
  } else if (type == "fire") {
    SpeciesData <- SpeciesData |>
      dplyr::select(dplyr::any_of(c(
        "SpeciesCode",
        "FireTolerance",
        "AgeDBH",
        "MaximumBarkThickness"
      )))
    filename <- filename %||% "species-fire.csv"
    file <- file.path(path, filename)
    write.csv(SpeciesData, file, row.names = FALSE)
  } else if (type == "succession") {
    SpeciesData <- SpeciesData |>
      dplyr::select(
        SpeciesCode,
        LeafLongevity,
        WoodDecayRate,
        MortalityCurve,
        GrowthCurve,
        LeafLignin,
        ShadeTolerance,
        FireTolerance
      )
    filename <- filename %||% "species.csv"
    file <- file.path(path, filename)
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
    insertFile("Species", file)
  } else {
    insertFile("SpeciesDataFile", file)
  }
}
