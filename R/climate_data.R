.climateCachePath <- function() {
  defaultCacheDir <- tools::R_user_dir("landisutils", "cache")

  getOption("landisutils.cache.path", defaultCacheDir)
}

#' PROJ4 strings for forest data
#'
#' Projection information for the forest data sources used by this package.
#'
#' @export
#' @rdname proj_forest_data
proj_nrcan_lcc <- paste("+proj=lcc +lat_1=49 +lat_2=77 +lat_0=0 +lon_0=-95",
  "+x_0=0 +y_0=0 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0")

#' PROJ4 strings for climate data
#'
#' Projection information for the climate data sources used by this package.
#'
#' @export
#' @rdname proj_climate_data
proj_daymet <- paste("+proj=lcc +lat_1=25 +lat_2=60 +lat_0=42.5 +lon_0=-100",
  "+x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs")

#' @keywords internal
var_landis <- function(var) {
  ## Climate variable names must align with variable names in Table 1 of Climate Library manual;
  ## they aren't case-sensitive, but matching them to table just in case.
  ##
  ## NOTE: Minimum temperature, maximum temperature, and precipitation are
  ##       required by the climate library. The units of temperature are
  ##       always Celsius and precipitation is always in cm.
  ##       Other parameters are optional and include PAR (photosynthetically
  ##       active radiation), CO2 concentration, ozone concentration, wind speed,
  ##       wind direction, and nitrogen deposition. Wind direction must be
  ##       expressed in terms of degrees where the wind is coming from
  ##       (note this is opposite of the typical convention used by met stations).
  switch(
    var,
    aet = "AET",              ## TerraClim AET
    co2 = "CO2",              ## TODO: CO2
    ndep = "Ndep",            ## TODO: nitrogen deposition
    ozone = "ozone",          ## TODO: ozone (O3)
    par = "PAR",              ## TODO:
    pet = "PET",              ## TODO: potential evapotranspiration
    prcp = "precip",          ## DAYMET: preciptation
    ppt = "precip",           ## TerraClim: preciptation
    rh = "RH",                ## TODO: rel humidity
    rmax = "maxRH",           ## TODO: max rel humidity
    rmin = "minRH",           ## TODO: min rel humidity
    sh = "SH",                ## TODO: specific humidity
    srad = "SWR",             ## DAYMET shortwave radiation
    temp = "temp",            ## TODO: mean temperature
    tmax = "Tmax",            ## DAYMET max temperature
    tmin = "Tmin",            ## DAYMET min temperature
    wnddir = "windDirection", ## TODO: wind 'from' direction
    ws = "windSpeed",         ## TerraClim wind speed
    wndNrt = "windNorthing",  ## TODO: wind northing
    wndEst = "windEasting",   ## TODO: wind easting

    stop(glue::glue("unknown mapping for climate variable {var}"))
  )
}

.prep_daily_weather_year <- function(year, var, studyArea, id) {
  ## We want a data.frame with columns:
  ##   Year  Month  Variable  Eco1 Eco2 Eco3 ...

  ## SpatRaster |> df |> long_df |> wide_df
  climateR::getDaymet(
    AOI = studyArea,
    varname = var,
    startDate = glue::glue("{year}-01-01"),
    endDate = glue::glue("{year}-12-31"),
    verbose = FALSE
  ) |>
    zonal::execute_zonal(
      geom = studyArea,
      ID = id,
      fun = "mean",
      join = FALSE # TRUE joins geometries, keeping as sf object
    ) |>
    tidyr::pivot_longer(
      cols = starts_with("mean"),
      names_to = c(NA, "Year", "Month", "Day", NA, NA),
      names_prefix = "mean.",
      names_sep = "(-|_)",
      values_to = "Value"
    ) |>
    dplyr::mutate(
      ## convert prcp from mm to cm
      Value = dplyr::case_when(var == "prcp" ~ Value / 10, .default = Value)
    ) |>
    tidyr::pivot_wider(
      names_from = all_of(id),
      values_from = "Value"
    ) |>
    dplyr::mutate(
      Year = as.integer(Year),
      Month = as.integer(Month),
      Day = as.integer(Day),
      Variable = var_landis(var),
      .after = "Day"
    ) |>
    reproducible::Cache(
      cachePath = .climateCachePath(),
      userTags = c(var, year)
    )
}

.prep_daily_weather_var <- function(var, years, studyArea, id) {
  furrr::future_map(
    .x = years,
    .f = .prep_daily_weather_year,
    var = var,
    studyArea = studyArea,
    id = id
  ) |>
    purrr::list_rbind()
}

#' Prepare Climate Data
#'
#' Download and prepare climate data for use with LANDIS-II simulations:
#'
#' 1. Climate data for the `studyArea` are downloaded and converted to `SpatRaster`
#'    using the \pkg{climateR} package;
#' 2. These are then summarized by zone (specified by `id`) using the \pkg{zonal} package;
#' 3. The tabular data are pivoted as required for ingestion by LANDIS-II Climate Library.
#'
#' @section Historical daily weather:
#' Daymet provides daily North American weather 1980-present (<https://daymet.ornl.gov/>).
#' Daymet variables: `dayl`, `prcp`, `srad`, `swe`, `tmax`, `tmin`, `vp`.
#' Use [prep_daily_weather()] for Daymet weather data.
#'
#' @section Historical monthly weather:
#' Terra Climate provides monthly North American weather 1980-present
#' (<https://www.climatologylab.org/terraclimate.html>).
#' TerraClim variables: `aet`, `def`, `PDSI`, `pet`, `ppt`, `q`, `soil`, `srad`, `swe`,
#' `tmax`, `tmin`, `vap`, `vpd`, `ws`.
#' Use [prep_monthly_weather()] for TerraClim weather data.
#'
#' @section Climate projections:
#' TODO
#'
#' @section Caching:
#' Caching is enabled by default, with the cache location configurable by setting the
#' `landisutils.cache.path` option.
#'
#' @param vars character specifying the climate variables.
#'
#' @param studyArea `sf` polygons object delineating e.g., ecoregions or fire zones.
#'
#' @param id character specifying the name of the column/field to use for zonal summaries.
#'
#' @param years integer vector specifying the years.
#'
#' @returns `tbl_df`
#'
#' @examples
#' if (requireNamespace("climateR", quietly = TRUE) &&
#'   requireNamespace("zonal", quietly = TRUE)) {
#'   ## use BEC zones in random study area in BC
#'   ecoregionPolys <- landisutils::test_ecoregionPolys
#'
#'   if (interactive()) plot(frpFRT["PolyID"])
#'
#'   clim_years <- 2011:2012 ## availability is 1980 to last year
#'
#'   ## get historic daily weather data from Daymet
#'   daily_climvars <- c("prcp", "tmax", "tmin")
#'   daily_weather <- prep_daily_weather(
#'     vars = daily_climvars,
#'     years = clim_years,
#'     studyArea = ecoregionPolys,
#'     id = "PolyID"
#'   )
#'
#'   head(daily_weather)
#'
#'   ## get historic monthly weather from TerraClim
#'   monthly_climvars <- c("ppt", "tmax", "tmin")
#'   monthly_weather <- prep_monthly_weather(
#'     vars = monthly_climvars,
#'     years = clim_years,
#'     studyArea = ecoregionPolys,
#'     id = "PolyID"
#'   )
#'
#'   head(monthly_weather)
#' }
#' @export
#' @rdname prep_climate_data
prep_daily_weather <- function(vars = NULL, years = NULL, studyArea = NULL, id = NULL) {
  stopifnot(
    requireNamespace("climateR", quietly = TRUE),
    requireNamespace("furrr", quietly = TRUE),
    requireNamespace("purrr", quietly = TRUE),
    requireNamespace("reproducible", quietly = TRUE),
    requireNamespace("zonal", quietly = TRUE)
  )

  stopifnot(
    !is.null(vars),
    !is.null(studyArea),
    !is.null(id),
    !is.null(years)
  )

  df <- purrr::map(
    .x = vars,
    .f = .prep_daily_weather_var,
    years = years,
    studyArea = studyArea,
    id = id
  ) |>
    purrr::list_rbind()

  df
}

.prep_monthly_weather_year <- function(year, var, studyArea, id) {
  ## We want a data.frame with columns:
  ##   Year  Month  Variable  Eco1 Eco2 Eco3 ...

  ## SpatRaster |> df |> long_df |> wide_df
  climateR::getTerraClim(
    AOI = studyArea,
    varname = var,
    startDate = glue::glue("{year}-01-01"),
    endDate = glue::glue("{year}-12-31"),
    verbose = FALSE
  ) |>
    zonal::execute_zonal(
      geom = studyArea,
      ID = id,
      join = FALSE # TRUE joins geometries, keeping as sf object
    ) |>
    tidyr::pivot_longer(
      cols = starts_with("mean"),
      names_to = c(NA, "Year", "Month", "Day", NA),
      names_prefix = "mean.",
      names_sep = "(-|_)",
      values_to = "Value"
    ) |>
    dplyr::mutate(
      ## convert ppt from mm to cm
      Value = dplyr::case_when(var == "ppt" ~ Value / 10, .default = Value)
    ) |>
    tidyr::pivot_wider(
      names_from = all_of(id),
      values_from = "Value"
    ) |>
    dplyr::mutate(
      Year = as.integer(Year),
      Month = as.integer(Month),
      Day = NULL,
      Variable = var_landis(var),
      .after = "Month"
    ) |>
    reproducible::Cache(
      cachePath = .climateCachePath(),
      userTags = c(var, year)
    )
}

.prep_monthly_weather_var <- function(var, years, studyArea, id) {
  furrr::future_map(
    .x = years,
    .f = .prep_monthly_weather_year,
    var = var,
    studyArea = studyArea,
    id = id
  ) |>
    purrr::list_rbind()
}

#' @export
#' @rdname prep_climate_data
prep_monthly_weather <- function(vars = NULL, years = NULL, studyArea = NULL, id = NULL) {
  stopifnot(
    requireNamespace("climateR", quietly = TRUE),
    requireNamespace("furrr", quietly = TRUE),
    requireNamespace("purrr", quietly = TRUE),
    requireNamespace("reproducible", quietly = TRUE),
    requireNamespace("zonal", quietly = TRUE)
  )

  stopifnot(
    !is.null(vars),
    !is.null(studyArea),
    !is.null(id),
    !is.null(years)
  )

  df <- purrr::map(
    .x = vars,
    .f = .prep_monthly_weather_var,
    years = years,
    studyArea = studyArea,
    id = id
  ) |>
    purrr::list_rbind() |>
    dplyr::filter(Year <= tail(years, 1)) ## match end year

  df
}
