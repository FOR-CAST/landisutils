#' @keywords internal
collapseSpp <- function(x) {
  x[nzchar(as.character(x))] |>
    sort() |>
    paste0(collapse = "__")
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
  stopifnot(is(pixelGroupMap, "SpatRaster"))

  .checkPath(path)
browser() ## TODO for csv
  initialCommunities <- data.table::copy(cohortData)
  initialCommunities[, community := lapply(.SD, collapseSpp), by = pixelGroup, .SDcols = "speciesCode"]
  initialCommunities[, MapCode := as.factor(community), by = pixelGroup]
  initialCommunities[, SpeciesName := TODO]

  ## simplify cohorts (large number of communities take a long time to write to disk + lots of RAM)
  ## TODO: revisit this simplification
  ageinc <- 40
  initialCommunities[, newAge := as.integer(age %/% ageinc * ageinc + ageinc / 2)]
  initialCommunities[, newBiomass := as.integer(B)] ## TODO
  initialCommunities[, newPixelGroup := .GRP, by = c("community")] ## by ecoregionGroup ??

  initialCommunitiesMap <- terra::deepcopy(pixelGroupMap) |>
    terra::classify(unique(initialCommunities[, .(pixelGroup, newPixelGroup)]))
  initialCommunitiesMap[is.na(initialCommunitiesMap[])] <- 0L

  cols2rm <- c("age", "B", "community", "ecoregionGroup", "pixelGroup", "totalBiomass")
  data.table::set(initialCommunities, NULL, cols2rm, NULL)
  data.table::setnames(initialCommunities, c("newAge", "newPixelGroup"), c("CohortAge", "pixelGroup"))
  initialCommunities <- unique(initialCommunities)

  ## enforce types and bounds
  initialCommunities[, between(MapCode, 0L, 65535L)]

  ## write files
  initialCommunitiesMapFile <- file.path(path, "initial-communities.tif")
  terra::writeRaster(initialCommunitiesMap, initialCommunitiesMapFile, overwrite = TRUE)

  browser() ## TODO: "The CSV format requires a header with the following names: X, Y, Z." ???????
  initialCommunitiesFile <- file.path(path, "initial-communities.csv")
  write.csv(initialCommunities, file = initialCommunitiesFile)

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
