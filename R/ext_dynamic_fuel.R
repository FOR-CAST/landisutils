#' Create Biomass Fuel System Input File
#'
#' Follows the  Biomass Fuel System User Guide.
#'
#' @template param_path
#'
#' @param ... arguments passed to other functions:
#'   - `DeadFirMaxAge`;
#'   - `DisturbanceConversionTable` (optional);
#'   - `EcoregionTable` (optional);
#'   - `FuelTypes`;
#'   - `HardoodMaximum` (optional);
#'   - `MapFileNames`;
#'   - `PctConiferMapName`;
#'   - `PctDeadFirMapName`;
#'   - `SpeciesFuelCoefficients`;
#'   - `Timestep`;
#'
#' @return `LandisExtension` object
#'
#' @export
BiomassFuelSytemInput <- function(path, ...) {
  stopifnot(!is.null(path))

  dots <- list(...)
  stopifnot(!is.null(dots$FireReductionParameters))

  ## ensure *relative* file paths inserted into config files
  dots$ClimateConfigFile <- fs::path_rel(dots$ClimateConfigFile, path)

  file <- file.path(path, "dynamic-fuel-system.txt")
  writeLines(
    c(
      LandisData("Dynamic Fuels"),
      insertTimestep(dots$Timestep),
      insertSpeciesFuelCoefficients(dots$SpeciesFuelCoefficients),
      insertHardoodMaximum(dots$HardoodMaximum),
      insertDeadFirMaxAge(dots$DeadFirMaxAge),
      insertFuelTypesTable(dots$FuelTypes),
      insertEcoregionTable(dots$EcoregionTable),
      insertDisturbanceConversionTable(dots$DisturbanceConversionTable),
      insertMapFileNames(dots$MapFileNames),
      insertPctConiferMapName(dots$PctConiferMapName),
      insertPctDeadFirMapName(dots$PctDeadFirMapName)
    ),
    file
  )

  ext <- LandisExtension$new(name = "Dynamic Fuel System", type = "disturbance", path = path)
  ext$add_file(basename(file))

  return(ext)
}

#' Specify Dynamic Fuel Extension's Species Fuel Coefficients
#'
#' @param df data.frame corresponding to Species Fuel Coefficients
#'
#' @template return_insert
#'
#' @export
insertSpeciesFuelCoefficients <- function(df) {
  c(
    glue::glue(">> Species    Fuel"),
    glue::glue(">>            Coefficient"),
    glue::glue(">> -------    -----------"),
    apply(df, 1, function(x) {
      glue::glue_collapse(x, sep = "    ")
    }),
    glue::glue("") ## add blank line after each item group
  )
}

#' Specify Dynamic Fuel Extension's Hardwood Maximum
#'
#' @param hwmax (optional) Integer. Percent hardwood value.
#'
#' @template return_insert
#'
#' @export
insertHardoodMaximum <- function(hwmax = 0L) {
  hwmax <- as.integer(hwmax)
  stopifnot(dplyr::between(hwmax, 0L, 100L))

  insertValue("HardoodMaximum", hwmax)
}

#' Specify Dynamic Fuel Extension's Dead Fir Maximum Age
#'
#' @param maxage Integer. The duration of influence for the BDA extension's dead conifer index.
#'
#' @template return_insert
#'
#' @export
insertDeadFirMaxAge <- function(maxage = 15L) {
  stopifnot(maxage >= 0L) ## TODO: use better default?

  insertValue("DeadFirMaxAge", maxage)
}

#' Create Dynamic Fire Extension's Fuel Types Table
#'
#' Assign fuel types to one of the Canadian Fire Behavior Prediction System fuel types.
#'
#' @returns data.frame for use with [insertFuelTypesTable()]
#'
#' @references
#' Natural Resources Canada. 2019. Fire Behaviour Prediction System Fuel Type Dsecriptions.
#'   <https://cwfis.cfs.nrcan.gc.ca/background/fueltypes/c1>
#'
#' @export
prepFuelTypesTable <- function() {
  browser() ## TODO
  df <- data.frame(
    FuelType = integer(0),
    BaseFuel = character(0),
    AgeMin = integer(0),
    AgeMax = integer(0),
    Species = character(0)
  )

  return(df)
}

#' Specify Dynamic Fuel Extension's Fuel Types Table
#'
#' @param df data.frame corresponding to `FuelTypesTable`, with columns:
#'   `FuelType` (int), `BaseFuel` (char), `AgeMin` (int), `AgeMax` (int), `Species` (char).
#'
#' @template return_insert
#'
#' @export
insertFuelTypesTable <- function(df) {
  stopifnot(
    ncol(df) == 5,
    identical(colnames(df), c("FuelType", "BaseFuel", "AgeMin", "AgeMax", "Species"))
  )

  df <- df |>
    dplyr::mutate(AgeRange = glue::glue("{AgeMin} to {AgeMax}")) |>
    dplyr::mutate(AgeMin = NULL, AgeMax = NULL)

  c(
    glue::glue("FuelTypes"),
    glue::glue(">> Fuel Type    Base Fuel    Age Range    Species"),
    glue::glue(">> ---------    ---------    ---------    ----------------"),
    apply(df, 1, function(x) {
      glue::glue_collapse(x, sep = "    ")
    }),
    glue::glue("") ## add blank line after each item group
  )
}

#' Specify Dynamic Fuel Extension's Ecoregion Table
#'
#' @param df data.frame corresponding to `EcoregionTable`, with columns:
#'   `FuelType` (int) and `Ecoregion` (char).
#'
#' @template return_insert
#'
#' @export
insertEcoregionTable <- function(df = NULL) {
  if (is.null(df)) {
    df <- data.frame(FuelType = integer(0), Ecoregion = character(0))
  }

  c(
    glue::glue("EcoregionTable"),
    glue::glue(">> Fuel Type    Ecoregion"),
    glue::glue(">> ---------    ---------"),
    apply(df, 1, function(x) {
      glue::glue_collapse(x, sep = "    ")
    }),
    glue::glue("") ## add blank line after each item group
  )
}

#' Create Dynamic Fire Extension's Disturbance Conversion Table
#'
#' @returns data.frame for use with [insertDisturbanceConversionTable()]
#'
#' @export
prepDisturbanceConversionTable <- function() {
  browser() ## TODO
  df <- data.frame(FuelType = integer(0), Duration = integer(0), Prescription = character(0))

  return(df)
}

#' Specify Dynamic Fuel Extension's Disturbance Conversion Table
#'
#' @param df data.frame corresponding to `DisturbanceConversiodfnTable`, with columns:
#'   `FuelType` (int), `Duration` (int), and `Prescription` (char).
#'
#' @template return_insert
#'
#' @export
insertDisturbanceConversionTable <- function(DisturbanceConversiodfnTable) {
  stopifnot(ncol(df) == 3, identical(colnames(df), c("FuelType", "Duration", "Prescription")))

  c(
    glue::glue("DisturbanceConversionTable"),
    glue::glue(">> Fuel Type    Duration    Prescription"),
    glue::glue(">> ---------    --------    ------------"),
    apply(df, 1, function(x) {
      glue::glue_collapse(x, sep = "    ")
    }),
    glue::glue("") ## add blank line after each item group
  )
}

#' Specify the Dynamic Fuel Extension's Fuel Type Map Names
#'
#' @template param_path
#'
#' @template return_insert
#'
#' @export
insertMapFileNames <- function(path) {
  path <- fs::path_rel(file.path(path, "fire"), path)

  ## NOTE: careful using glue() here; need literal {timestep}, so use {{timestep}}
  file <- glue::glue("{path}/FuelType-{{timestep}}.tif")

  insertFile("MapFileNames", file)
}

#' Specify the Dynamic Fuel Extension's Percent Conifer Map Name
#'
#' @template param_path
#'
#' @template return_insert
#'
#' @export
insertPctConiferMapName <- function(path) {
  path <- fs::path_rel(file.path(path, "fire"), path)

  ## NOTE: careful using glue() here; need literal {timestep}, so use {{timestep}}
  file <- glue::glue("{path}/PctConifer-{{timestep}}.tif")

  insertFile("PctConiferMapName", file)
}

#' Specify the Dynamic Fuel Extension's Percent Dead Fir Map Name
#'
#' @template param_path
#'
#' @template return_insert
#'
#' @export
insertPctDeadFirMapName <- function(path) {
  path <- fs::path_rel(file.path(path, "fire"), path)

  ## NOTE: careful using glue() here; need literal {timestep}, so use {{timestep}}
  file <- glue::glue("{path}/PctDeadFir-{{timestep}}.tif")

  insertFile("PctDeadFirMapName", file)
}
