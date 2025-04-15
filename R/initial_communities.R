#' @keywords internal
collapseSpp <- function(x) {
  x[nzchar(as.character(x))] |>
    sort() |>
    paste0(collapse = "__")
}

#' Simplify cohorts
#'
#' Reduce the number of cohorts / pixel groups for LANDIS-II, which only supports (integer)
#' initial community map codes between 0 and 65535.
#'
#' @note Ideally, the user should reduce the number of cohorts upstream
#'       (i.e., in `Biomass_borealDataPrep`), to ensure consistency of all data inputs.
#'
#' @param cohortData A `data.table` containing cohort information (see \pkg{LandR})
#'
#' @param pixelGroupMap A `SpatRaster` identifying the locations of the pixel groups in `cohortData`
#'
#' @param ageBin integer specifying the bin width for the new age categories
#'
#' @returns list containing updated `cohortData` and `pixelGroupMap` objects
#'
#' @export
simplifyCohorts <- function(cohortData, pixelGroupMap, ageBin = 20) {
  browser()
  ## TODO: revisit this simplification (ideally simplification done upstream in B_bDP)
  cd <- data.table::copy(cohortData)
  cd[, community := lapply(.SD, collapseSpp), by = pixelGroup, .SDcols = "speciesCode"]
  cd[, newAge := as.integer(age %/% ageBin * ageBin + ageBin / 2)]
  cd[, newPixelGroup := .GRP, by = c("community", "ecoregionGroup")]
  cd[, newB := as.integer(newAge / max(newAge) * mean(B)), by = c("newPixelGroup", "speciesCode")]

  stopifnot(
    all(cd[["newPixelGroup"]] >= 0L),
    all(cd[["newPixelGroup"]] <= 65535L)
  )

  pgm <- terra::deepcopy(pixelGroupMap) |>
    terra::classify(unique(cd[, .(pixelGroup, newPixelGroup)]))

  set(cd, NULL, c("community"), NULL)
  setnames(cd, c("newAge", "newB", "newPixelGroup"), c("age", "B", "pixelGroup"))

  return(list(cd, pgm))
}

#' Create `InitialCommunities` and `InitialCommunitiesMap` Files
#'
#' @param cohortData A `data.table` containing cohort information (see LandR)
#'
#' @param pixelGroupMap A `SpatRaster` identifying the locations of the pixel groups in `cohortData`
#'
#' @template return_file
#'
#' @export
#' @rdname prepInitialCommunities
prepInitialCommunities <- function(cohortData, pixelGroupMap, path) {
  .checkPath(path)

  if (!is.null(cohortData) && is(cohortData, "data.table") &&
      !is.null(pixelGroupMap) && is(pixelGroupMap, "SpatRaster")) {
    initialCommunities <- data.table::copy(cohortData)
    initialCommunities[, MapCode := as.integer(pixelGroup)]
    initialCommunities[, CohortAge := as.integer(age)]
    initialCommunities[, CohortBiomass := as.integer(B)]
    initialCommunities[, SpeciesName := as.character(speciesCode)]

    cols2keep <- c("MapCode", "SpeciesName", "CohortAge", "CohortBiomass")
    initialCommunities <- initialCommunities[, cols2keep, with = FALSE]
    initialCommunities <- unique(initialCommunities)
    setkeyv(initialCommunities, cols2keep[1:3])
    initialCommunities <- list(
      data.table(MapCode = 0L, SpeciesName = NA_character_, CohortAge = 0L, CohortBiomass = 0L),
      initialCommunities
    ) |>
      rbindlist()

    initialCommunitiesMap <- terra::deepcopy(pixelGroupMap)
  } else {
    stop("")
  }

  stopifnot(
    all(initialCommunities[["MapCode"]] >= 0L),
    all(initialCommunities[["MapCode"]] <= 65535L)
  )

  ## write files
  initialCommunitiesMapFile <- file.path(path, "initial-communities.tif")
  terra::writeRaster(initialCommunitiesMap, initialCommunitiesMapFile, overwrite = TRUE)

  initialCommunitiesFile <- file.path(path, "initial-communities.csv")
  fwrite(initialCommunities, initialCommunitiesFile)

  return(c(initialCommunities, initialCommunitiesMap))
}

#' Specify `InitialCommunities` and `InitialCommunitiesMap` Files
#'
#' @param files
#'
#' @template return_insert
#'
#' @export
insertInitialCommunities <- function(files) {
  c(
    glue::glue("InitialCommunities    \"{files[1]}\""),
    glue::glue("InitialCommunitiesMap    \"{files[2]}\""),
    glue::glue("") ## add blank line after each item group
  )
}
