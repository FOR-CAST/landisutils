#' Create Original Fire Input File
#'
#' Follows the Original Fire User Guide.
#'
#' @template param_path
#'
#' @param ... arguments passed to other functions:
#'   - `DynamicFireRegionsTable` (optional);
#'   - `FireRegionParametersTable`;
#'   - `FireDamageTable`;
#'   - `FuelCurveTable`;
#'   - `InitialFireRegionsMap`;
#'   - `LogFile`;
#'   - `MapNames`;
#'   - `Species_CSV_File`;
#'   - `SummaryLogFile`;
#'   - `Timestep`;
#'   - `WindCurveTable` (optional);
#'
#' @export
#' @aliases BaseFireInput
OriginalFireInput <- function(path, ...) {
  stopifnot(!is.null(path))

  dots <- list(...)
  stopifnot(
    !is.null(dots$FireRegionParametersTable),
    !is.null(dots$InitialFireRegionsMap),
    !is.null(dots$LogFile),
    !is.null(dots$Species_CSV_File),
    !is.null(dots$SummaryLogFile)
  )

  ## ensure *relative* file paths inserted into config files
  dots$InitialFireRegionsMap <- fs::path_rel(dots$InitialFireRegionsMap, path)
  dots$LogFile <- fs::path_rel(dots$LogFile, path)
  dots$Species_CSV_File <- fs::path_rel(dots$Species_CSV_File, path)
  dots$SummaryLogFile <- fs::path_rel(dots$SummaryLogFile, path)

  file <- file.path(path, "original-fire.txt")
  writeLines(
    c(
      LandisData("Original Fire"),
      insertTimestep(dots$Timestep),
      insertSpecies_CSV_File(dots$Species_CSV_File),
      insertFireRegionParametersTable(dots$FireRegionParametersTable),
      insertInitialFireRegionsMap(dots$InitialFireRegionsMap),
      insertDynamicFireRegionsTable(dots$DynamicFireRegionsTable),
      insertFuelCurveTable(dots$FuelCurveTable),
      insertWindCurveTable(dots$WindCurveTable),
      insertFireDamageTable(dots$FireDamageTable),
      insertMapNames(path),
      insertLogFile(dots$LogFile),
      insertSummaryLogFile(dots$SummaryLogFile)
    ),
    file
  )

  ext <- LandisExtension$new(name = "Original Fire", type = "disturbance", path = path)
  ext$add_file(basename(file))
  ext$add_file(dots$InitialFireRegionsMap)
  ext$add_file(dots$Species_CSV_File)

  return(ext)
}

#' Specify Original Fire Extension `Species_CSV_File`
#'
#' @template param_file
#'
#' @template return_insert
#'
#' @export
insertSpecies_CSV_File <- function(file) {
  insertFile("Species_CSV_File", file)
}

#' Prepare Original Fire Extension `FireRegionParameters` Table
#'
#' @param sf `sf` polygon object
#'
#' @returns data.frame
#'
#' @export
prepFireRegionParametersTable <- function(sf) {
  sf::st_drop_geometry(sf) |>
    dplyr::mutate(
      FireRegionName = glue::glue("FRT_{FRT}"), ## TODO: use 1st col, not hardcoded 'FRT'
      MapCode = PolyID,
      MeanSize = xBar * cellSize,
      MinSize = cellSize, ## always one pixel?
      MaxSize = emfs_ha,
      IgnitionProb = pIgnition,
      k = round(1.0 / (empiricalBurnRate))
    ) |>
    dplyr::select(FireRegionName, MapCode, MeanSize, MinSize, MaxSize, IgnitionProb, k) |>
    dplyr::bind_rows(
      ## must include values for mapcode 0 (i.e., the NAs)
      data.frame(
        FireRegionName = "FRT_0", ## TODO: use 1st col, not hardcoded 'FRT'
        MapCode = 0,
        MeanSize = 0,
        MinSize = 0, ## always one pixel?
        MaxSize = 0,
        IgnitionProb = 0,
        k = 0
      )
    )
}

#' Specify Fire Region Parameters Table
#'
#' @param df data.frame corresponding to Fire Region Parameters Table
#'
#' @template return_insert
#'
#' @export
insertFireRegionParametersTable <- function(df) {
  c(
    glue::glue(">> Fire Region Parameters"),
    glue::glue(">> "),
    glue::glue(">> Region  Map    Mean  Min   Max   Ignition  Fire"),
    glue::glue(">> Name    Code   Size  Size  Size  Prob      k"),
    glue::glue(">> -----------------------------------------------"),
    apply(df, 1, function(x) {
      glue::glue_collapse(x, sep = "    ")
    }),
    glue::glue("") ## add blank line after each item group
  )
}

#' Create Original Fire `InitialFireRegionsMap`
#'
#' @param r `SpatRaster` corresponding to initial fire regions map
#'
#' @template param_file
#'
#' @template return_file
#'
#' @export
prepInitialFireRegionsMap <- function(r, file = "fire-regions-map.tif") {
  terra::writeRaster(
    r,
    file,
    overwrite = TRUE,
    # datatype = "INT2U", ## corresponds best to 65535 values; but LANDIS doesn't like it?
    datatype = "INT2S", ## this works, but limits mapcodes to 32767
    NAflag = 0L
  )

  return(file)
}

#' Specify `InitialFireRegionsMap` file
#'
#' @template param_file
#'
#' @template return_insert
#'
#' @export
insertInitialFireRegionsMap <- function(file) {
  insertFile("InitialFireRegionsMap", file)
}

#' Specify Original Fire `FuelCurveTable`
#'
#' @param df data.frame
#'
#' @template return_insert
#'
#' @export
insertFuelCurveTable <- function(df) {
  c(
    glue::glue("FuelCurveTable"),
    glue::glue(">> Fireregion    S1  S2  S3  S4  S5"),
    glue::glue(">> --------------------------------"),
    apply(df, 1, function(x) {
      glue::glue_collapse(x, sep = "    ")
    }),
    glue::glue("") ## add blank line after each item group
  )
}

#' Specify Original Fire `WindCurveTable`
#'
#' @param df data.frame or NULL
#'
#' @template return_insert
#'
#' @export
insertWindCurveTable <- function(df) {
  c(
    glue::glue("WindCurveTable"),
    glue::glue(">> Ecoregion    S5  S4  S3  S2  S1"),
    glue::glue(">> -------------------------------"),
    if (!is.null(df)) {
      apply(df, 1, function(x) {
        glue::glue_collapse(x, sep = "  ")
      })
    },
    glue::glue("") ## add blank line after each item group
  )
}

#' Calibrate Original Fire
#'
#' @export
calibrate_fire <- function() {
  ## TODO
}
