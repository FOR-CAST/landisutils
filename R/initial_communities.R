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
#' @template LANDIS_version
#'
#' @template return_file
#'
#' @export
#' @rdname prepInitialCommunities
prepInitialCommunitiesFromCohortData <- function(cohortData, pixelGroupMap,
                                                 path, version = landisVersion()) {
  checkVersion(version)
  stopifnot(is(pixelGroupMap, "SpatRaster"))

  .checkPath(path)

  initialCommunities <- data.table::copy(cohortData)
  initialCommunities[, community := lapply(.SD, collapseSpp), by = pixelGroup, .SDcols = "speciesCode"]
  initialCommunities[, mapcode := as.factor(community), by = pixelGroup]

  ## simplify cohorts (large number of communities take a long time to write to disk + lots of RAM)
  ## TODO: revisit this simplification
  ageinc <- 40
  initialCommunities[, newAge := as.integer(age %/% ageinc * ageinc + ageinc / 2)]
  initialCommunities[, newPixelGroup := .GRP, by = c("community")] ## by ecoregionGroup ??

  initialCommunitiesMap <- terra::deepcopy(pixelGroupMap) |>
    terra::classify(unique(initialCommunities[, .(pixelGroup, newPixelGroup)]))
  initialCommunitiesMap[is.na(initialCommunitiesMap[])] <- 0L

  cols2rm <- c("age", "B", "community", "ecoregionGroup", "pixelGroup", "totalBiomass")
  data.table::set(initialCommunities, NULL, cols2rm, NULL)
  data.table::setnames(initialCommunities, c("newAge", "newPixelGroup"), c("age", "pixelGroup"))
  initialCommunities <- unique(initialCommunities)

  ## write files
  initialCommunitiesMapFile <- file.path(path, "initial-communities.tif")
  terra::writeRaster(initialCommunitiesMap, initialCommunitiesMapFile, overwrite = TRUE)

  if (version == 7) {
    file_t <- file.path(path, "initial-communities.txt")
    writeLines(c(
      LandisData("Initial Communities"),
      glue::glue(""), ## add blank line after each item group

      glue::glue(">> MapCode 0 corresponds to inactive pixels"),
      glue::glue(""),

      lapply(unique(initialCommunities$pixelGroup), function(i) {
        subdt <- initialCommunities[pixelGroup == i, ]
        community <- unique(subdt$mapcode)
        ages <- unique(subdt$age) |> sort() |> paste(collapse = " ")
        species <- unique(subdt$speciesCode)

        c(
          glue::glue(">> {community}"),
          glue::glue("MapCode {i}"),
          glue::glue("   {species} {ages}"),
          glue::glue("")
        )
      }) |>
        unlist()
    ), file_t)
  } else {
    file_t <- file.path(path, "initial-communities.csv")

    browser() ## TODO as csv

    write.csv(initialCommunities, file = file) ## TODO
  }

  return(c(initialCommunities, initialCommunitiesMap))
}

#' @param speciesLayers `SpatRaster` of species cover, with each layer corresponding to a species.
#'
#' @param standAgeMap `SpatRaster` of stand ages.
#'
#' @export
#' @rdname prepInitialCommunities
prepInitialCommunities <- function(speciesLayers, standAgeMap,
                                   path, version = landisVersion()) {
  checkVersion(version)
  stopifnot(
    is(speciesLayers, "SpatRaster"),
    is(standAgeMap, "SpatRaster")
  )
  terra::compareGeom(speciesLayers, standAgeMap, res = TRUE)

  .checkPath(path)
browser()
  age <- terra::values(standAgeMap, mat = FALSE)
  speciesNames <- terra::names(speciesLayers)

  communities <- lapply(speciesLayers, function(lyr) {
    cover <- terra::values(lyr, mat = FALSE)

    data.table::data.table(
      id = seq_along(cover),
      species = terra::names(lyr),
      present = cover > 0,
      age = age
    )
  }) |>
    data.table::rbindlist()
  communities[, species := data.table::fifelse(present, species, "", "")]


  initialCommunities <- data.table::copy(communities)
  initialCommunities[, community := lapply(.SD, collapseSpp), by = id, .SDcols = "species"]
  data.table::set(initialCommunities, NULL, c("species", "present"), NULL)
  initialCommunities[, mapcode := as.factor(community), by = id]
  data.table::set(initialCommunities, NULL, c("community"), NULL)
  initialCommunities <- unique(initialCommunities, by = c("id", "mapcode"))

  initialCommunitiesMap <- terra::rast(standAgeMap)
  terra::values(initialCommunitiesMap) <- initialCommunities$mapcode
  initialCommunitiesMap[initialCommunities[mapcode == "", ][["id"]]] <- 0L

  initialCommunitiesMapFile <- file.path(path, "initial-communities.tif")
  terra::writeRaster(initialCommunitiesMap, initialCommunitiesMapFile, overwrite = TRUE)

  initialCommunitiesNames <- levels(initialCommunities$mapcode)
browser()
  ## write to file
  lapply(unique(as.integer(initialCommunities$mapcode)), function(i) {
    subdt <- initialCommunities[as.integer(mapcode) == i, ]
    community <- unique(subdt$mapcode)

    communities[id %in% subdt$id & present == TRUE, ]

    if (community == "") {
      c(
        glue::glue(">> MapCode {i} corresponds to inactive pixels"),
        glue::glue("")
      )
    } else {
      c(
        glue::glue(">> {community}"),
        glue::glue("MapCode {i}"),
        glue::glue("   {}"), ## TODO: paste row per species, with ages
        glue::glue("")
      )
    }
  })

  ## TODO

  if (version == 7) {
    file_t <- filepath(path, "initial-communities.txt")
    writeLines(c(
      LandisData("Initial Communities"),
      ## TODO
      glue::glue("") ## add blank line after each item group
    ), file_t) ## TODO
  } else {
    file_t <- filepath(path, "initial-communities.csv")
    write.csv(initialCommunities, file = file) ## TODO
  }

  return(file)
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
