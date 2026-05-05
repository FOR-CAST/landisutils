.climateCachePath <- function() {
  defaultCacheDir <- tools::R_user_dir("landisutils", "cache")

  getOption("landisutils.cache.path", defaultCacheDir)
}

#' Stable short hash of a study-area object
#'
#' Returns a short hexadecimal hash (`xxhash64`) of the serialized object, used
#' to namespace climate caches so that distinct study areas don't collide.
#' Two study-area objects that are equal under [identical()] return the same
#' hash; otherwise the hashes differ.
#'
#' @param studyArea any R object — typically an `sf` polygons object.
#'
#' @returns character scalar — a 16-character hex hash.
#'
#' @keywords internal
.studyArea_hash <- function(studyArea) {
  digest::digest(studyArea, algo = "xxhash64")
}

#' PROJ4 strings for forest data
#'
#' Projection information for the forest data sources used by this package.
#'
#' @export
#' @rdname proj_forest_data
proj_nrcan_lcc <- paste(
  "+proj=lcc +lat_1=49 +lat_2=77 +lat_0=0 +lon_0=-95",
  "+x_0=0 +y_0=0 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0"
)

#' PROJ4 strings for climate data
#'
#' Projection information for the climate data sources used by this package.
#'
#' @export
#' @rdname proj_climate_data
proj_daymet <- paste(
  "+proj=lcc +lat_1=25 +lat_2=60 +lat_0=42.5 +lon_0=-100",
  "+x_0=0 +y_0=0 +ellps=WGS84 +units=m +no_defs"
)

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
    aet = "AET", ## TerraClim AET
    co2 = "CO2", ## TODO: CO2
    ndep = "Ndep", ## TODO: nitrogen deposition
    ozone = "ozone", ## TODO: ozone (O3)
    par = "PAR", ## TODO:
    pet = "PET", ## TODO: potential evapotranspiration
    prcp = "precip", ## DAYMET: preciptation
    ppt = "precip", ## TerraClim: preciptation
    rh = "RH", ## TODO: rel humidity
    rmax = "maxRH", ## TODO: max rel humidity
    rmin = "minRH", ## TODO: min rel humidity
    sh = "SH", ## TODO: specific humidity
    srad = "SWR", ## DAYMET shortwave radiation
    temp = "temp", ## TODO: mean temperature
    tmax = "Tmax", ## DAYMET max temperature
    tmin = "Tmin", ## DAYMET min temperature
    wnddir = "windDirection", ## TODO: wind 'from' direction
    ws = "windSpeed", ## TerraClim wind speed
    wndNrt = "windNorthing", ## TODO: wind northing
    wndEst = "windEasting", ## TODO: wind easting

    stop(glue::glue("unknown mapping for climate variable {var}"))
  )
}
