#' @keywords internal
collapseSpp <- function(x) {
  x[nzchar(as.character(x))] |> sort() |> paste0(collapse = "__")
}

#' Simplify cohorts
#'
#' Reduce the number of cohorts / pixel groups for LANDIS-II, which only supports (integer)
#' initial community map codes between 0 and 65535.
#'
#' Biomass is CONSERVED across the merge. Each merged community takes, per species,
#' the mean of the stand totals of the pixel groups it pools, and that total is then
#' divided among the retained age classes in proportion to age, so the oldest class
#' still carries the most. Before 0.0.128 each age class instead received its own
#' scaled copy of the mean COHORT biomass, so a community's biomass grew with the
#' number of age classes pooled into it rather than being conserved: measured on one
#' landscape, initial communities carried a median 440 t/ha against 116 t/ha observed,
#' and exceeded the succession extension's own `maxB` by four to thirteen times.
#'
#' The shares are computed over the DISTINCT retained age classes, because
#' [prepInitialCommunities()] deduplicates its rows: partitioning across duplicate
#' rows would lose whatever those duplicates carried. Integer rounding leaves the
#' per-species total within a few g/m2 of the target.
#'
#' Conservation is per merged community, and the mean it takes over the pooled
#' pixel groups is UNWEIGHTED: a pixel group covering one cell counts the same as
#' one covering a thousand. Landscape biomass is therefore conserved only up to
#' that weighting, and on a landscape where 1.6 million pixel groups collapse to
#' 4,153 communities the unweighted community mean sat about 1.2 times the
#' pixel-group mean (151 against 126 t/ha), because small communities count
#' equally with large ones. Weighting by pixel count would remove that; it is
#' deliberately left alone here because it changes which stands the communities
#' represent, not just their arithmetic.
#'
#' @note Ideally, the user should reduce the number of cohorts upstream
#'       (i.e., in `Biomass_borealDataPrep`), to ensure consistency of all data inputs.
#'
#' @param cohortData A `data.table` with columns `pixelGroup`, `speciesCode`,
#'   `age`, `B`, and `ecoregionGroup` describing cohort composition per pixel group
#'
#' @param pixelGroupMap A `SpatRaster` identifying the locations of the pixel groups in `cohortData`
#'
#' @param ageBin integer specifying the bin width for the new age categories
#'
#' @returns list containing updated `cohortData` and `pixelGroupMap` objects
#'
#' @export
simplifyCohorts <- function(cohortData, pixelGroupMap, ageBin = 20) {
  ## TODO: revisit this simplification (ideally simplification done upstream in B_bDP)
  cd <- data.table::copy(cohortData)
  cd[, community := lapply(.SD, collapseSpp), by = pixelGroup, .SDcols = "speciesCode"]
  cd[, newAge := as.integer(age %/% ageBin * ageBin + ageBin / 2)]
  cd[, newPixelGroup := .GRP, by = c("community", "ecoregionGroup")]

  stopifnot(all(cd[["newPixelGroup"]] >= 0L), all(cd[["newPixelGroup"]] <= 65535L))

  ## TODO: reclassification is very slow
  pgm <- terra::deepcopy(pixelGroupMap) |>
    terra::classify(unique(cd[, .(pixelGroup, newPixelGroup)]))

  ## Conserve biomass across the merge. The quantity that must survive is the STAND total per
  ## species, not the mean of its cohorts: a merged community stands in for many pixel groups, so
  ## it takes the mean of their per-species totals and divides that among the age classes it keeps.
  ## Weights are age-proportional, as before, so the oldest class still carries the most -- what
  ## changes is that they now sum to one instead of each being an independent copy of the mean.
  pg_totals <- cd[,
    list(B_pg = sum(B, na.rm = TRUE)),
    by = c("newPixelGroup", "speciesCode", "pixelGroup")
  ]
  targets <- pg_totals[, list(B_target = mean(B_pg)), by = c("newPixelGroup", "speciesCode")]

  ## DISTINCT age classes: prepInitialCommunities() deduplicates, so shares spread across duplicate
  ## rows would be silently dropped with them.
  shares <- unique(cd[, list(newPixelGroup, speciesCode, newAge)])
  shares[, w := newAge / sum(newAge), by = c("newPixelGroup", "speciesCode")]
  shares[targets, B_target := i.B_target, on = c("newPixelGroup", "speciesCode")]
  shares[, newB := as.integer(round(w * B_target))]

  cd[shares, newB := i.newB, on = c("newPixelGroup", "speciesCode", "newAge")]

  set(cd, NULL, c("age", "B", "community", "pixelGroup"), NULL)
  setnames(cd, c("newAge", "newB", "newPixelGroup"), c("age", "B", "pixelGroup"))

  return(list(cd, pgm))
}

#' Create `InitialCommunities` and `InitialCommunitiesMap` Files
#'
#' @param cohortData A `data.table` with columns `pixelGroup`, `speciesCode`,
#'   `age`, `B`, and `ecoregionGroup` describing cohort composition per pixel group
#'
#' @param pixelGroupMap A `SpatRaster` identifying the locations of the pixel groups in `cohortData`
#'
#' @template param_path
#'
#' @template return_file
#'
#' @seealso Used by succession extensions: [BiomassSuccession], [DGSSuccession],
#'   [ForCS], [NECNSuccession], and [PnETSuccession].
#'
#' @export
#' @rdname prepInitialCommunities
prepInitialCommunities <- function(cohortData, pixelGroupMap, path) {
  .checkPath(path)

  stopifnot(
    !is.null(cohortData) &&
      is(cohortData, "data.table") &&
      !is.null(pixelGroupMap) &&
      is(pixelGroupMap, "SpatRaster")
  )

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
    data.table(MapCode = 0L, SpeciesName = "NA", CohortAge = 0L, CohortBiomass = 0L),
    initialCommunities
  ) |>
    rbindlist()

  initialCommunitiesMap <- terra::deepcopy(pixelGroupMap) |> terra::as.int()

  stopifnot(
    all(initialCommunities[["MapCode"]] >= 0L),
    all(initialCommunities[["MapCode"]] <= 65535L)
  )

  ## write files
  initialCommunitiesMapFile <- file.path(path, "initial-communities.tif")
  terra::writeRaster(
    initialCommunitiesMap,
    initialCommunitiesMapFile,
    overwrite = TRUE,
    ## NOT INT2U, despite map codes being positive: LANDIS-II's GDAL reader takes only byte/short/int/
    ## float/double, so the unsigned types abort the run at extension-init. landis_datatype() widens to
    ## INT4S past 32767 rather than silently capping there.
    datatype = landis_datatype(max(terra::minmax(initialCommunitiesMap)[2], 0)),
    NAflag = 0L
  )

  initialCommunitiesFile <- file.path(path, "initial-communities.csv")
  fwrite(initialCommunities, initialCommunitiesFile)

  return(c(initialCommunitiesFile, initialCommunitiesMapFile))
}

#' Specify `InitialCommunities` and `InitialCommunitiesMap` Files
#'
#' @param files character, specifying the paths to the initial communities
#'              and initial communities map files.
#'
#' @template return_insert
#'
#' @export
insertInitialCommunities <- function(files) {
  c(
    insertValue("InitialCommunities", files[1], blank_line = FALSE),
    insertValue("InitialCommunitiesMap", files[2], blank_line = FALSE),
    glue::glue("") ## add blank line after each item group
  )
}
