#' Create `Ecoregions` and `EcoregionsMap` Files
#'
#' @param ecoregion `data.frame`
#'
#' @param ecoregionMap `SpatRaster`
#'
#' @param path character
#'
#' @template return_file
#'
#' @export
prepEcoregionsFiles <- function(ecoregion, ecoregionMap, path = NULL) {
  stopifnot(
    !is.null(path)
  )
  path <- .checkPath(path)

  ## see LandR::makeEcoregionDT (maybe LandR::ecoregionProducer ??)
  ## see LandR::makeEcoregionMap

  ecoregionMap[is.na(ecoregionMap[]) | is.nan(ecoregionMap[])] <- 0L
  er_range <- terra::minmax(ecoregionMap)

  stopifnot(er_range[1] >= 0, er_range[2] <= 65535) ## LANDIS-II requires mapcodes [0, 65535];

  ecoregionsMapFile <- file.path(path, "ecoregions.tif")
  terra::writeRaster(ecoregionMap, ecoregionsMapFile, overwrite = TRUE)

  ecoregionsTable <- as.data.frame(ecoregion) |>
    dplyr::rename(Active = active) |>
    dplyr::mutate(
      MapCode = as.integer(ecoregionGroup),
      Name = paste0("\"", as.character(ecoregionGroup), "\""),
      Description = paste0("\"", as.character(ecoregionGroup), "\""), ## TODO: use meaningful desc
      ecoregionGroup = NULL
    ) |>
    rbind(data.frame(
      Active = "no ",
      MapCode = 0L,
      Name = "none",
      Description = "\"non-forest or not in study area\""
    ))

  ecoregionsFile <- file.path(path, "ecoregions.txt")
  writeLines(c(
    LandisData("Ecoregions"),
    glue::glue(">> Active  MapCode  Name      Description"),
    glue::glue(">> ------  -------  --------  -----------------------"),
    apply(ecoregionsTable, MARGIN = 1, FUN = function(x) {
      glue::glue("   {x}") |> glue::glue_collapse(sep = "   ")
    }, simplify = TRUE),
    glue::glue("")
  ), ecoregionsFile)

  return(c(ecoregionsFile, ecoregionsMapFile))
}

#' Specify `Ecoregions` and `EcoregionsMap` Files
#'
#' @param files
#'
#' @template return_insert
#'
#' @export
#' @family scenario-write
insertEcoregionsFiles <- function(files) {
  c(
    glue::glue("Ecoregions    \"{files[1]}\""),
    glue::glue("EcoregionsMap    \"{files[2]}\""),
    glue::glue("") ## add blank line after each item group
  )
}

#' Create Ecoregion Parameters Table
#'
#' @param speciesEcoregion data.frame
#'
#' @template LANDIS_version
#'
#' @template return_insert
#'
#' @export
#'
prepEcoregionParameters <- function(speciesEcoregion) {
  ecoregionParameters <- speciesEcoregion |>
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
}

#' Specify `EcoregionParameters` table
#'
#' @param files
#'
#' @template return_insert
#'
#' @export
#' @family scenario-write
insertEcoregionParameters <- function(files) {
  c(
    glue::glue("Ecoregions    \"{files[1]}\""),
    glue::glue("EcoregionsMap    \"{files[2]}\""),
    glue::glue("") ## add blank line after each item group
  )
}

#' Create Species Ecoregion Data File
#'
#' @param speciesLayers `SpatRaster`
#'
#' @template LANDIS_version
#'
#' @template return_insert
#'
#' @export
#'
prepSpeciesEcoregionDataFile <- function(version = landisVersion()) {
  checkVersion(version)

  ## TODO
}
