#' Create LANDIS-II scenario files
#'
#' @param name Character. Label to use as a filename and label for this scenario.
#'
#' @param duration Numeric. Number of years to run the simulation.
#'
#' @param cell_length Numeric. Size of ecoregion raster cells (in $m$).
#'
#' @param distRndOrder Logical. Should disturbances be applied in a random order?
#'
#' @param extensions Named list of `r paste(.extTypes, collapse = " , ")` extensions.
#'
#' @param path Character. Path specifying a directory to use for the scenario runs.
#'
#' @template LANDIS_version
#'
#' @template return_file
#'
#' @export
scenario <- function(name = NULL, duration = NULL, cell_length = NULL, distRndOrder = FALSE,
                     extensions = NULL, seed = NULL, path = NULL, version = landisVersion()) {
  stopifnot(
    !is.null(name), length(name) == 1, is.character(name),
    !is.null(duration), length(duration) == 1, is.numeric(duration),
    !is.null(cell_length), length(cell_length) == 1, is.numeric(cell_length),
    !is.null(extensions), is.list(extensions), all(names(extensions) %in% .extTypes),
    !is.null(name),
    !is.null(path)
  )
  checkVersion(version)

  LandisData <- "Scenario"
  Duration <- duration

  Species <- insertSpeciesDataFile() ## needs file
  Ecoregions <- insertEcoregions() ## needs files

  CellLength <- cell_length ## TODO: get this from data. does LANDIS do so automatically?

  SuccessionExtensions <- insertSuccessionExtensions(extensions$succession)

  DisturbanceExtensions <- insertDisturbanceExtensions(extensions$disturbance)

  DisturbancesRandomOrder <- insertDisturbancesRandomOrder(distRndOrder)

  OtherExtensions <- insertOtherExtensions(extensions$other)

  RandomNumberSeed <- insertRandomNumberSeed(seed)

  ## TODO
  file <- glue::glue("{file.path(path, name)}.txt")
  writeLines(c(
    .landisutilsHeader,
    LandisData,
    Duration,
    Species,
    Ecoregions,
    CellLength,
    SuccessionExtensions,
    DisturbanceExtensions,
    OtherExtensions,
    RandomNumberSeed
  ), file)
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
