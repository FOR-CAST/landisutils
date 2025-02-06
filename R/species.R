#' Species Data File
#'
#' @param species data.frame
#'
#' @template LANDIS_version
#'
#' @template return_insert
#'
#' @export
prepSpeciesData <- function(species = NULL, path = NULL, version = landisVersion()) {
  stopifnot(
    !is.null(species),
    !is.null(path)
  )
  path <- .checkPath(path)
  checkVersion(version)

  SpeciesData <- species |>
    dplyr::mutate(
      SpeciesCode = species,
      LeafLongevity = leaflongevity,
      WoodDecayRate = wooddecayrate,
      MortalityCurve = mortalityshape,
      GrowthCurve = growthcurve,
      LeafLignin = leafLignin
    )

  if (version == 7) {
    file <- file.path(path, "species.txt") ## TODO: confirm v7 works with csv
    writeLines(SpeciesData, file) ## TODO
  } else if (version == 8) {
    file <- file.path(path, "species.csv")
    write.csv(SpeciesData, file)
  }

  return(file)
}

#' Specify Species Data File
#'
#' @param file
#'
#' @template return_insert
#'
#' @export
insertSpeciesDataFile <- function(file) {
  c(
    glue::glue("Species    \"{file}\""),
    glue::glue("") ## add blank line after each item group
  )
}
