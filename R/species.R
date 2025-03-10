#' Species Data File
#'
#' @param df data.frame corresponding to the species data table
#'
#' @template return_file
#'
#' @export
prepSpeciesData <- function(df = NULL, path = NULL) {
  stopifnot(
    !is.null(species),
    !is.null(path)
  )
  path <- .checkPath(path)

  SpeciesData <- species |>
    dplyr::mutate(
      SpeciesCode = species,
      LeafLongevity = leaflongevity,
      WoodDecayRate = wooddecayrate,
      MortalityCurve = mortalityshape,
      GrowthCurve = growthcurve,
      LeafLignin = leafLignin
    )

  file <- file.path(path, "species.csv")
  write.csv(SpeciesData, file)

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
