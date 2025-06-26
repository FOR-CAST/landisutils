#' Create Biomass Output Extension Input File
#'
#' @template param_path
#'
#' @param ... arguments passed to other functions:
#'   - `DeadPools`;
#'   - `MakeTable`;
#'   - Dead Biomass `MapNames`;
#'   - Live Biomass `MapNames`;
#'   - `Species`;
#'   - `Timestep`;
#'
#' @template return_file
#'
#' @export
#'
#' @examples
#' OutputBiomassInput <- OutputBiomassInput(
#'   path = tempdir(),
#'   Timestep = 10,
#'   MakeTable = "yes",
#'   Species = "all",
#'   ## MapNames defaults cannot be changed at this time
#'   DeadPools = "both"
#' )
#'
OutputBiomassInput <- function(path, ...) {
  stopifnot(
    !is.null(path)
  )

  dots <- list(...)
  stopifnot(
    !is.null(dots$Timestep)
  )

  dots$MakeTable <- dots$MakeTable %||% "yes"
  stopifnot(dots$MakeTable %in% c("yes", "no", "Y", "N", TRUE, FALSE))
  dots$MakeTable <- ifelse(dots$MakeTable %in% c("yes", "Y", TRUE), "yes", "no")

  dots$Species <- dots$Species %||% "all"

  dots$DeadPools <- dots$DeadPools %||% "both"
  stopifnot(dots$DeadPools %in% c("both", "woody", "non-woody"))

  file <- file.path(path, "output-biomass.txt")
  writeLines(c(
    LandisData("Output Biomass"),
    insertTimestep(dots$Timestep),
    glue::glue("MakeTable {dots$MakeTable}"),
    glue::glue("Species {paste(dots$Species, sep = '\n         ')}"),

    ## NOTE: careful using glue() here; need literal {timestep} etc., so use {{timestep}} etc.
    glue::glue("MapNames  \"outputs/biomass/biomass-{{species}}-{{timestep}}.tif\""),

    glue::glue("DeadPools  {dots$DeadPools}"),

    ## NOTE: careful using glue() here; need literal {timestep} etc., so use {{timestep}} etc.
    glue::glue("MapNames  \"outputs/biomass/biomass-{{pool}}-{{timestep}}.tif\"")
  ), file)

  ext <- LandisExtension$new(
    name = "Output Biomass",
    type = "other",
    path = path
  )
  ext$add_file(basename(file))

  return(ext)
}

#' Create Biomass Output by Age Extension Input File
#'
#' @template param_path
#'
#' @param ... arguments passed to other functions:
#'   - `MapNames`;
#'   - `Species`;
#'   - `Timestep`;
#'
#' @template return_file
#'
#' @export
#'
#' @examples
#' BiomassAgeInput <- OutputBiomassAgeInput(
#'   path = tempdir(),
#'   Timestep = 10,
#'   ## MapNames defaults cannot be changed at this time
#'   Species = c(
#'     "pinubank ageclass1(10-40) ageclass2(15-100)",
#'     "pinuresi ageclass(>200)",
#'     "pinustro ageclass(>250)",
#'     "poputrem ageclass1(<50)"
#'   )
#' )
#'
OutputBiomassAgeInput <- function(path, ...) {
  stopifnot(
    !is.null(path)
  )

  dots <- list(...)
  stopifnot(
    !is.null(dots$Species),
    !is.null(dots$Timestep)
  )

  ## NOTE: Species list needs to be provided such that we produce e.g.:
  ## Species  pinubank ageclass1(10-40) ageclass2(15-100)
  ##          pinuresi ageclass(>200)
  ##          pinustro ageclass(>250)
  ##          poputrem ageclass1(<50)

  file <- file.path(path, "output-biomass-by-age.txt")
  writeLines(c(
    LandisData("Output Biomass-by-Age"),
    insertTimestep(dots$Timestep),
    ## NOTE: careful using glue() here; need literal {timestep} etc., so use {{timestep}} etc.
    glue::glue("MapNames  \"outputs/biomass-age/{{species}}-{{ageclass}}-{{timestep}}.tif\""),
    glue::glue("Species {paste(dots$Species, sep = '\n         ')}")
  ), file)

  ext <- LandisExtension$new(
    name = "Output Biomass-by-Age",
    type = "other",
    path = path
  )
  ext$add_file(basename(file))

  return(ext)
}

#' Create Max Species Age Extension Input File
#'
#' @template param_path
#'
#' @param ... arguments passed to other functions:
#'   - `SpeciesAgeStats`;
#'   - `SiteAgeStats`;
#'   - `SiteSpeciesStats`
#'   - `Timestep`;
#'
#' @template return_file
#'
#' @export
#'
#' @examples
#' # CohortStatsInput <- OutputCohortStatsInput(
#' #   path = tempdir(),
#' #   Timestep = 10,
#' #   ## MapNames defaults cannot be changed at this time
#' #   SpeciesAgeStats = c(
#' #     species = c(),
#' #     stats = c()
#' #   ),
#' #   SiteAgeStats = c(
#' #     stats = c()
#' #   ),
#' #   SiteSpeciesStats = c(
#' #     stats = c()
#' #   )
#' # )
#'
OutputCohortStatsInput <- function(path, ...) {
  stopifnot(
    !is.null(path)
  )

  dots <- list(...)

  species_stats <- c("MAX", "MIN", "AVG", "MED", "SD")
  site_stats <- c("MAX", "MIN", "AVG", "MED", "SD", "RICH", "EVEN", "COUNT")
  site_species_stats <- c("RICH")

  dots$SpeciesAgeStats$stats <- toupper(dots$SpeciesAgeStats$stats) %||% species_stats
  dots$SiteAgeStats$stats <- toupper(dots$SiteAgeStats$stats) %||% site_stats
  dots$SiteSpeciesStats$stats <- toupper(dots$SiteSpeciesStats$stats) %||% site_species_stats

  stopifnot(
    !is.null(dots$Timestep),
    all(dots$SpeciesAgeStats$stats %in% species_stats),
    all(dots$SiteAgeStats$stats %in% site_stats),
    all(dots$SiteSpeciesStats$stats %in% site_species_stats)
  )

  file <- file.path(path, "output-cohort-stats.txt")
  writeLines(c(
    LandisData("Output Cohort Statistics"),
    insertTimestep(dots$Timestep),

    glue::glue("SpeciesAgeStats"),
    ## NOTE: careful using glue() here; need literal {timestep} etc., so use {{timestep}} etc.
    glue::glue("MapNames  \"outputs/cohort-stats/{{species}}-{{statistic}}-{{timestep}}.tif\""),
    lapply(dots$SpeciesAgeStats$stats, function(stat) {
      glue::glue("{stat}    {paste(species, collapse = '  ')}")
    }),

    glue::glue("SpeciesAgeStats"),
    ## NOTE: careful using glue() here; need literal {timestep} etc., so use {{timestep}} etc.
    glue::glue("MapNames  \"outputs/cohort-stats/age-{{statistic}}-{{timestep}}.tif\""),
    lapply(dots$SpeciesAgeStats$stats, function(stat) {
      glue::glue("{stat}")
    }),

    glue::glue("SiteSpeciesStats"),
    ## NOTE: careful using glue() here; need literal {timestep} etc., so use {{timestep}} etc.
    glue::glue("MapNames  \"outputs/cohort-stats/spp-{{statistic}}-{{timestep}}.tif\""),
    lapply(dots$SiteSpeciesStats$stats, function(stat) {
      glue::glue("{stat}")
    })
  ), file)

  ext <- LandisExtension$new(
    name = "Output Cohort Statistics",
    type = "other",
    path = path
  )
  ext$add_file(basename(file))

  return(ext)
}

#' Create Max Species Age Extension Input File
#'
#' @template param_path
#'
#' @param ... arguments passed to other functions:
#'   - `MapNames`;
#'   - `Species`;
#'   - `Timestep`;
#'
#' @template return_file
#'
#' @export
#'
#' @examples
#' MaxSppAgeInput <- OutputBiomassAgeInput(
#'   path = tempdir(),
#'   Timestep = 10,
#'   ## MapNames defaults cannot be changed at this time
#'   Species = c("pinubank", "acersacc", "tiliamer")
#' )
#'
OutputMaxSppAgeInput <- function(path, ...) {
  stopifnot(
    !is.null(path)
  )

  dots <- list(...)
  stopifnot(
    !is.null(dots$Timestep)
  )

  dots$Species <- dots$Species %||% "all"

  file <- file.path(path, "output-max-species-age.txt")
  writeLines(c(
    LandisData("Output Max Species Age"),
    insertTimestep(dots$Timestep),
    ## NOTE: careful using glue() here; need literal {timestep} etc., so use {{timestep}} etc.
    glue::glue("MapNames  \"outputs/max-spp-age/{{species}}-{{timestep}}.tif\""),
    glue::glue("Species {paste(dots$Species, sep = '\n         ')}")
  ), file)

  ext <- LandisExtension$new(
    name = "Output Max Species Age",
    type = "other",
    path = path
  )
  ext$add_file(basename(file))

  return(ext)
}
