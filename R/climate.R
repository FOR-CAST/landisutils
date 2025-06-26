#' Create `ClimateConfigFile`
#'
#' @template param_path
#'
#' @param ... additional climate config arguments:
#' - `ClimateTimeSeries`;
#' - `ClimateFile`;
#' - `SpinUpClimateTimeSeries`;
#' - `SpinUpClimateFile`;
#' - `GenerateClimateOutputFiles`;
#' - `UsingFireClimate`;
#' - `FineFuelMoistureCode`;
#' - `DuffMoistureCode`;
#' - `DroughtCode`;
#' - `FirstDayFire`;
#' - `LastDayFire`;
#'
#' @return `LandisClimateConfig` object
#'
#' @export
#'
prepClimateConfig <- function(path, ...) {
  dots <- list(...)

  allowedTimeSeries <- c(
    "Daily_AverageAllYears", "Daily_RandomYears", "Daily_SequencedYears",
    "Monthly_AverageAllYears", "Monthly_RandomYears", "Monthly_SequencedYears"
  )

  stopifnot(
    !is.null(dots$ClimateFile),
    !is.null(dots$SpinUpClimateFile),
    dots$ClimateTimeSeries %in% allowedTimeSeries,
    dots$SpinUpClimateTimeSeries %in% allowedTimeSeries
  )

  dots$GenerateClimateOutputFiles <- dots$GenerateClimateOutputFiles %||% "yes"
  dots$UsingFireClimate <- dots$UsingFireClimate %||% "no"

  dots$FineFuelMoistureCode <- dots$FineFuelMoistureCode %||% 100 ## 85
  dots$DuffMoistureCode <- dots$DuffMoistureCode %||% 100 ## 6
  dots$DroughtCode <- dots$DroughtCode %||% 100 ## 15
  dots$FirstDayFire <- dots$FirstDayFire %||% 30
  dots$LastDayFire <- dots$LastDayFire %||% 320
  dots$AtmosphericPressure <- dots$AtmosphericPressure %||% 100

  ## ensure *relative* file paths inserted into config files
  dots$ClimateFile <- fs::path_rel(dots$ClimateFile, path)
  dots$SpinUpClimateFile <- fs::path_rel(dots$SpinUpClimateFile, path)

  ## based on sample file:
  ## https://github.com/LANDIS-II-Foundation/Extension-Biomass-Succession/blob/master/
  ##   testings/CoreV8.0-BiomassSuccession7.0/biomass-succession_ClimateGenerator.txt
  file <- file.path(path, "climate-config.txt")
  writeLines(
    c(
      LandisData("Climate Config"),
      glue::glue("ClimateTimeSeries           {dots$ClimateTimeSeries}"),
      glue::glue("ClimateFile                 {dots$ClimateFile}"),
      glue::glue(""),
      glue::glue("SpinUpClimateTimeSeries     {dots$SpinUpClimateTimeSeries}"),
      glue::glue("SpinUpClimateFile           {dots$SpinUpClimateFile}"),
      glue::glue(""),
      glue::glue("GenerateClimateOutputFiles  {dots$GenerateClimateOutputFiles}"),
      glue::glue("UsingFireClimate            {dots$UsingFireClimate}  << Optional parameter; default is no."),
      glue::glue(""),
      glue::glue(">> FineFuelMoistureCode     {dots$FineFuelMoistureCode}"), ## TODO: uncomment
      glue::glue(">> DuffMoistureCode         {dots$DuffMoistureCode}"),     ## TODO: uncomment
      glue::glue(">> DroughtCode              {dots$DroughtCode}"),          ## TODO: uncomment
      glue::glue(">> FirstDayFire             {dots$FirstDayFire}"),         ## TODO: uncomment
      glue::glue(">> LastDayFire              {dots$LastDayFire}"),          ## TODO: uncomment
      glue::glue(">> AtmosphericPressure      {dots$AtmosphericPressure}"),  ## TODO: uncomment
      glue::glue("") ## add blank line after each item group
    ),
    file
  )

  cc <- LandisClimateConfig$new(path = path)
  cc$add_file(basename(file))
  cc$add_file(dots$ClimateFile)
  cc$add_file(dots$SpinUpClimateFile)

  return(cc)
}

#' Specify `ClimateConfigFile`
#'
#' @template param_file
#'
#' @template return_insert
#'
#' @export
insertClimateConfigFile <- function(file) {
  c(
    glue::glue("ClimateConfigFile    \"{file}\""),
    glue::glue("") ## add blank line after each item group
  )
}

#' Write LANDIS-II Climate Input File to disk
#'
#' Simple wrapper around [utils::write.csv()].
#'
#' @param df data.frame corresponding to Climate Input Data table
#'
#' @template param_file
#'
#' @template return_file
#'
#' @export
writeClimateData <- function(df, file) {
  write.csv(df, file, quote = FALSE, row.names = FALSE)

  return(file)
}
