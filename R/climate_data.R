.climateCachePath <- function() {
  defaultCacheDir <- tools::R_user_dir("landisutils", "cache")

  getOption("landisutils.cache.path", defaultCacheDir)
}

#' Robust `BioSIM::generateWeather()` wrapper: request staggering + exponential-backoff retry
#'
#' The BioSIM web API can be slow or transiently unavailable when many requests arrive at once, and the
#' `BioSIM` R client exposes no timeout/retry/delay knobs (`biosimclient.config()` only tunes
#' `nbNearestNeighbours` and climate-generation/test flags). A bare `generateWeather()` call therefore has
#' no protection: a transient failure aborts the whole fetch, and a stalled call hangs indefinitely. This
#' wrapper adds (all tunable via `options()`):
#' \itemize{
#'   \item a random pre-request **stagger delay** so concurrent `furrr` workers don't hit the server in
#'         lockstep -- `landisutils.biosim.request_delay` = `c(min, max)` seconds (default `c(1, 5)`);
#'   \item **exponential backoff with jitter** on failure -- `landisutils.biosim.max_attempts` (default
#'         `5L`) total tries, waiting `landisutils.biosim.backoff_base * 2^(attempt - 1)` seconds (base
#'         default `15`) between tries, so repeated failures back off progressively (gentler on an
#'         overwhelmed server);
#'   \item an optional **per-attempt timeout** -- `landisutils.biosim.timeout` seconds (default `Inf` =
#'         off) via [base::setTimeLimit()]; on timeout the J4R client is reset
#'         ([BioSIM::shutdownClient()]) before the next try so a stalled socket can't poison it.
#' }
#'
#' NOTE: these options are read at call time INSIDE the fetch (which often runs in a `furrr`/`crew`
#' worker), so to tune them set the `options()` somewhere the worker inherits -- e.g. the project
#' `.Rprofile` -- NOT a file only the main process sources (a `targets` `_local.R`).
#'
#' @param ... passed verbatim to [BioSIM::generateWeather()].
#' @returns the [BioSIM::generateWeather()] result; errors after exhausting `max_attempts`.
#' @keywords internal
.biosim_generate_weather <- function(...) {
  delay <- getOption("landisutils.biosim.request_delay", c(1, 5))
  max_attempts <- as.integer(getOption("landisutils.biosim.max_attempts", 5L))
  backoff_base <- getOption("landisutils.biosim.backoff_base", 15)
  timeout <- getOption("landisutils.biosim.timeout", Inf)

  ## stagger this worker's first hit so concurrent workers don't fire simultaneously
  if (length(delay) == 2L && max(delay) > 0) {
    Sys.sleep(stats::runif(1L, min(delay), max(delay)))
  }

  for (attempt in seq_len(max_attempts)) {
    if (is.finite(timeout)) {
      setTimeLimit(elapsed = timeout, transient = FALSE)
    }
    res <- tryCatch(BioSIM::generateWeather(...), error = function(e) e)
    if (is.finite(timeout)) {
      setTimeLimit() ## clear the limit
    }

    if (!inherits(res, "error")) {
      return(res)
    }

    ## a timeout likely left the J4R socket mid-response -- reset the client before retrying
    if (is.finite(timeout)) {
      try(BioSIM::shutdownClient(), silent = TRUE)
    }
    if (attempt == max_attempts) {
      stop(
        glue::glue(
          "BioSIM::generateWeather() failed after {max_attempts} attempts: {conditionMessage(res)}"
        ),
        call. = FALSE
      )
    }
    wait <- backoff_base * 2^(attempt - 1L) * stats::runif(1L, 0.75, 1.25)
    message(glue::glue(
      "[landisutils] BioSIM request failed (attempt {attempt}/{max_attempts}): ",
      "{conditionMessage(res)} -- backing off {round(wait)}s"
    ))
    Sys.sleep(wait)
  }
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
