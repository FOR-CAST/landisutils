#' Create LANDIS-II scenario files
#'
#' @param name Character. Label to use as a filename and label for this scenario.
#'
#' @param extensions Named list of `r paste(.extTypes, collapse = " , ")` extensions.
#'
#' @template param_path
#'
#' @param ... arguments passed to other functions:
#'   - `CellLength` Numeric. Size of ecoregion raster cells (in $m$);
#'   - `DisturbancesRandomOrder` Logical. Should disturbances be applied in a random order?
#'   - `Duration` Numeric. Number of years to run the simulation;
#'   - `EcoregionsFiles` List of length 2 containing character file paths (TODO);
#'   - `RandomNumberSeed` Integer. Seed used to initialize the LANDIS-II random number generator;
#'   - `SpeciesInputFile` Character. (TODO);
#'
#' @template return_file
#'
#' @export
scenario <- function(name = NULL, extensions = NULL, path = NULL, ...) {
  dots <- list(...)

  stopifnot(
    !is.null(name), length(name) == 1, is.character(name),

    !is.null(extensions), is.list(extensions), all(names(extensions) %in% .extTypes),

    !is.null(path), length(path) == 1, is.character(path),

    ## mandatory dots params
    !is.null(dots$Duration), length(dots$Duration) == 1, is.numeric(dots$Duration),

    !is.null(dots$EcoregionsFiles), length(dots$EcoregionsFiles) == 2,
    all(is.character(dots$EcoregionsFiles)),

    !is.null(dots$SpeciesInputFile), length(dots$SpeciesInputFile) == 1,
    is.character(dots$SpeciesInputFile)
  )
  path <- .checkPath(path)

  ## optional dots params
  if (!is.null(dots$RandomNumberSeed)) {
    stopifnot(
      length(dots$RandomNumberSeed) == 1, is.numeric(dots$RandomNumberSeed), dots$RandomNumberSeed > 0
    )
  }

  if (!is.null(dots$CellLength)) {
    stopifnot(
      length(dots$CellLength) == 1, is.numeric(dots$CellLength), dots$CellLength > 0
    )
  }

  ## ensure *relative* file paths inserted into config files
  dots$SpeciesInputFile <- fs::path_rel(dots$SpeciesInputFile, path)
  dots$EcoregionsFiles <- fs::path_rel(dots$EcoregionsFiles, path)
  extensions <- lapply(extensions, function(exts) {
    lapply(exts, function(ext) {
      ext_file <- fs::path_rel(ext, path)
      names(ext_file) <- names(ext)
      ext_file
    })
  })

  file <- file.path(path, glue::glue("{name}.txt"))
  writeLines(c(
    LandisData("Scenario"),
    insertDuration(dots$Duration),
    insertSpeciesDataFile(dots$SpeciesInputFile, core = TRUE),
    insertEcoregionsFiles(dots$EcoregionsFiles),
    insertCellLength(dots$CellLength), ## TODO: get this from data
    insertSuccessionExtensions(extensions$succession),
    insertDisturbanceExtensions(extensions$disturbance),
    insertDisturbancesRandomOrder(dots$DisturbancesRandomOrder),
    insertOtherExtensions(extensions$other),
    insertRandomNumberSeed(dots$RandomNumberSeed)
  ), file)

  return(file)
}

#' Specify Scenario `CellLength`
#'
#' @param cell_length integer specifying the length of a cell's edge in the ecoregions map
#'
#' @template return_insert
#'
#' @export
insertCellLength <- function(cell_length) {
  c(
    glue::glue("CellLength {cell_length}"),
    glue::glue("") ## add blank line after each item group
  )
}

#' Specify Scenario Duration
#'
#' @param duration integer specifying the number of years of simulation
#'
#' @template return_insert
#'
#' @export
insertDuration <- function(duration) {
  c(
    glue::glue("Duration {duration}"),
    glue::glue("") ## add blank line after each item group
  )
}

#' Specify Scenario Extensions Tables
#'
#' A scenario must specify exactly one succession extension,
#' zero or more disturbance extensions, and zero or more other extensions.
#'
#' @param exts Named list of extensions specifying the initialization file to use.
#'
#' @template return_insert
#'
#' @examples
#' list("Biomass Succession" = "biomass-succession.txt") |>
#'   insertSuccessionExtensions()
#'
#' list(
#'   "Base Fire" = "base-fire.txt",
#'   "Base Harvest" = "base-harvest.txt"
#'  ) |>
#'   insertDisturbanceExtensions()
#'
#' list(
#'   "Output Biomass By Age" = "output-biomass-by-age.txt",
#'   "Output Cohort Statistics" = "output-cohort-statistics.txt"
#' ) |>
#'   insertOtherExtensions()
#'
#' @export
#' @rdname insertExtensions
insertSuccessionExtensions <- function(exts = NULL) {
  stopifnot(
    !is.null(exts), !is.null(names(exts)),
    length(exts) == 1 ## can only use one succession extension
  )

  c(
    glue::glue(">> Succession Extension    Initialization File"),
    glue::glue(">> --------------------    -------------------"),
    glue::glue("   \"{names(exts)}\"        {exts}"),
    glue::glue("") ## add blank line after each item group
  )
}

#' @export
#' @rdname insertExtensions
insertDisturbanceExtensions <- function(exts = NULL) {
  if (!is.null(exts) && !is.null(names(exts))) {
    c(
      glue::glue(">> Disturbance Extensions    Initialization File"),
      glue::glue(">> ----------------------    -------------------"),
      lapply(names(exts), function(x) {
        glue::glue("   \"{x}\"        {exts[[x]]}")
      }) |>
        unlist(),
      glue::glue("") ## add blank line after each item group
    )
  } else {
    c(
      glue::glue(">> Disturbance Extensions    Initialization File"),
      glue::glue(">> ----------------------    -------------------"),
      glue::glue("") ## add blank line after each item group
    )
  }
}

#' @export
#' @rdname insertExtensions
insertOtherExtensions <- function(exts = NULL) {
  if (!is.null(exts) && !is.null(names(exts))) {
    c(
      glue::glue(">> Other Extensions            Initialization File"),
      glue::glue(">> ------------------------    -----------------------"),
      lapply(names(exts), function(x) {
        glue::glue("   \"{x}\"        {exts[[x]]}")
      }) |>
        unlist(),
      glue::glue("") ## add blank line after each item group
    )
  } else {
    c(
      glue::glue(">> Other Extensions            Initialization File"),
      glue::glue(">> ------------------------    -----------------------"),
      glue::glue("") ## add blank line after each item group
    )
  }
}

#' Specify Scenario `DisturbanceRandomOrder`
#'
#' Optional, although if not specified the default is `FALSE`.
#' (LANDIS-II runs disturbance extensions in the order specified).
#'
#' @param x Logical. Should disturbances be applied in a random order?
#'
#' @template return_file
#'
#' @export
insertDisturbancesRandomOrder <- function(x) {
  yesno <- isTRUE(x) || tolower(x) %in% c("y", "yes")

  c(
    glue::glue("DisturbancesRandomOrder    no  << optional parameter"),
    glue::glue("") ## add blank line after each item group
  )
}

#' Specify Scenario `RandomNumberSeed`
#'
#' Optional.
#'
#' @param seed Integer (>0). Seed to initialize LANDIS-II random number generator.
#'
#' @template return_file
#'
#' @export
insertRandomNumberSeed <- function(seed) {
  if (is.null(seed)) {
    c(
      glue::glue(">> RandomNumberSeed    4357  << optional parameter"),
      glue::glue("") ## add blank line after each item group
    )
  } else {
    seed <- as.integer(seed)
    stopifnot(seed > 0)

    c(
      glue::glue("RandomNumberSeed    {seed}  << optional parameter"),
      glue::glue("") ## add blank line after each item group
    )
  }
}

#' Replicate a LANDIS-II scenario
#'
#' @template param_file
#'
#' @param reps integer, number of replicates to generate
#'
#' @return TODO
#'
#' @export
#'
replicate <- function(file = NULL, reps = NULL) {
  stopifnot(
    !is.null(file),
    !is.null(reps)
  )

  ## TODO:
  ## 1. readLines the scenario "seed" file
  ## 2. for all_reps in 1:reps replace outputPath with outputPath/repID
  ## 3. copy all inputs?
  ## 4. profit

  return(all_files)
}
