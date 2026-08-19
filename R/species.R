## Columns Biomass Succession declares as `double` (v7 User Guide 2.12.1-2.12.6). ShadeTolerance and
## FireTolerance are excluded deliberately: the extension types those as integers.
.biomass_double_cols <- c(
  "LeafLongevity",
  "WoodDecayRate",
  "MortalityCurve",
  "GrowthCurve",
  "LeafLignin"
)

## Format a numeric as text that always carries a decimal point.
##
## Biomass Succession parses species.csv into a .NET DataTable, and DataTable infers each column's
## TYPE FROM THE FIRST DATA ROW. A whole number written bare -- `write.csv()` renders 0 as "0" -- types
## the column Int32, and every later decimal in that column then aborts the run at extension load:
##
##   Couldn't store <0.1> in GrowthCurve Column.  Expected type is Int32.
##
## The values are legal; the file simply has to declare itself. A sweep whose first species happened
## to carry a fractional value ran fine while an otherwise identical sweep starting at 0 did not,
## which is how this surfaced.
##
## Appending ".0" rather than formatting to fixed digits keeps the exact value: 0.062 stays 0.062
## rather than becoming 0.0620000.
.as_landis_double <- function(x) {
  if (!is.numeric(x)) {
    return(x)
  }
  s <- format(x, trim = TRUE, scientific = FALSE)
  out <- ifelse(grepl(".", s, fixed = TRUE), s, paste0(s, ".0"))
  ifelse(is.na(x), NA_character_, out)
}

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
#' @seealso Used by succession extensions ([BiomassSuccession], [DGSSuccession],
#'   [ForCS], [NECNSuccession]) when `type = "core"` or `"succession"`, and by
#'   fire extensions ([OriginalFire], [SocialClimateFire]) when `type = "fire"`.
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
    ## `quote = 1L` keeps SpeciesCode quoted, as before, while leaving the numeric columns bare --
    ## they are character vectors now (see .as_landis_double()) and would otherwise be quoted.
    SpeciesData <- dplyr::mutate(
      SpeciesData,
      dplyr::across(dplyr::any_of(.biomass_double_cols), .as_landis_double)
    )
    write.csv(SpeciesData, file, row.names = FALSE, quote = 1L)
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
