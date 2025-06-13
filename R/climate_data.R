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
#' @section Climate projections:
#' TODO
#'
#' @param var character specifying the climate variable.
#'
#' @param studyArea `sf` polygons object delineating e.g., ecoregions or fire zones.
#'
#' @param id character specifying the name of the column/field to use for zonal summaries.
#'
#' @param start,end character specifying the start and end dates as `"YYYY-M-DD"`.
#'
#' @returns `tbl_df`
#'
#' @examples
#' if (requireNamespace("bcdata", quietly = TRUE) &&
#'   requireNamespace("climateR", quietly = TRUE) &&
#'   requireNamespace("scfmutils", quietly = TRUE) &&
#'   requireNamespace("zonal", quietly = TRUE)) {
#'   ## define study area
#'   ## use BEC zones in random study area in BC
#'   studyAreaBC <- terra::vect(cbind(-122.14, 52.14), crs = "epsg:4326") |>
#'     terra::project(paste("+proj=lcc +lat_1=49 +lat_2=77 +lat_0=0 +lon_0=-95",
#'       "+x_0=0 +y_0=0 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0")) |>
#'     SpaDES.tools::randomStudyArea(seed = 60, size = 1e10)
#'
#'   ## we can safely ignore the following warnings:
#'   ## "attribute variables are assumed to be spatially constant throughout all geometries"
#'   ecoregionPolys <- suppressWarnings({
#'     scfmutils::prepInputsFireRegimePolys(studyArea = studyAreaBC, type = "BECNDT")
#'   })
#'
#'   if (interactive()) plot(frpFRT["PolyID"])
#'
#'   clim_years <- 2011:2012 ## availability is 1980 to last year
#'
#'   ## get historic daily weather data from Daymet
#'   daily_climvars <- c("prcp", "tmax", "tmin")
#'   daily_weather <- purrr::map(
#'     .x = daily_climvars,
#'     .f = prep_daily_weather,
#'     studyArea = ecoregionPolys,
#'     id = "PolyID",
#'     start = glue::glue("{head(clim_years, 1)}-01-01"),
#'     end = glue::glue("{tail(clim_years, 1)}-12-31")
#'   ) |>
#'     purrr::list_rbind()
#'
#'   head(daily_weather)
#'
#'   ## get historic monthly weather from TerraClim
#'   monthly_climvars <- c("ppt", "tmax", "tmin")
#'   monthly_weather <- purrr::map(
#'     .x = monthly_climvars,
#'     .f = prep_monthly_weather,
#'     studyArea = ecoregionPolys,
#'     id = "PolyID",
#'     start = glue::glue("{head(clim_years, 1)}-01-01"),
#'     end = glue::glue("{tail(clim_years, 1)}-12-31")
#'   ) |>
#'     purrr::list_rbind() |>
#'     dplyr::filter(Year <= tail(clim_years, 1)) ## match end year
#'
#'   head(monthly_weather)
#' }
#' @export
#' @rdname prep_climate_data
prep_daily_weather <- function(var = NULL, studyArea = NULL, id = NULL, start = NULL, end = NULL) {
  stopifnot(
    requireNamespace("climateR", quietly = TRUE),
    requireNamespace("zonal", quietly = TRUE)
  )

  stopifnot(
    !is.null(var),
    !is.null(studyArea),
    !is.null(id),
    !is.null(start),
    !is.null(end)
  )

  ## We want a data.frame with columns:
  ##   Year  Month  Day  Variable  Eco1 Eco2 Eco3 ...

  ## SpatRaster |> df |> long_df |> wide_df
  df <- climateR::getDaymet(
    AOI = studyArea,
    varname = var,
    startDate = start,
    endDate = end,
    verbose = FALSE
  ) |>
    zonal::execute_zonal(
      geom = studyArea,
      ID = id,
      join = FALSE # TRUE joins geometries, keeping as sf object
    ) |>
    tidyr::pivot_longer(
      cols = all_of(starts_with("mean")),
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
      names_from = id,
      values_from = "Value"
    ) |>
    dplyr::mutate(
      Year = as.integer(Year),
      Month = as.integer(Month),
      Day = as.integer(Day),
      Variable = var_landis(var),
      .after = "Day"
    )

  df
}

#' @export
#' @rdname prep_climate_data
prep_monthly_weather <- function(var = NULL, studyArea = NULL, id = NULL, start = NULL, end = NULL) {
  stopifnot(
    requireNamespace("climateR", quietly = TRUE),
    requireNamespace("zonal", quietly = TRUE)
  )

  stopifnot(
    !is.null(var),
    !is.null(studyArea),
    !is.null(id),
    !is.null(start),
    !is.null(end)
  )

  ## We want a data.frame with columns:
  ##   Year  Month  Variable  Eco1 Eco2 Eco3 ...

  ## SpatRaster |> df |> long_df |> wide_df
  df <- climateR::getTerraClim(
    AOI = studyArea,
    varname = var,
    startDate = start,
    endDate = end,
    verbose = FALSE
  ) |>
    zonal::execute_zonal(
      geom = studyArea,
      ID = id,
      join = FALSE # TRUE joins geometries, keeping as sf object
    ) |>
    tidyr::pivot_longer(
      cols = all_of(starts_with("mean")),
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
      names_from = id,
      values_from = "Value"
    ) |>
    dplyr::mutate(
      Year = as.integer(Year),
      Month = as.integer(Month),
      Day = NULL,
      Variable = var_landis(var),
      .after = "Month"
    )

  df
}
