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
#' ## define study area
#' studyAreaBC <- terra::vect(cbind(-122.14, 52.14), crs = "epsg:4326") |>
#'   terra::project(proj_nrcan_lcc) |>
#'   SpaDES.tools::randomStudyArea(seed = 60, size = 1e10) |>
#'   scfmutils::prepInputsFireRegimePolys(studyArea = _, type = "FRT")
#'
#' if (interactive()) plot(frpFRT["FRT"])
#'
#' ## get historic daily weather data
#' daily_climvars <- c("prcp", "tmax", "tmin")
#' daily_weather <- purrr::map(
#'   .x = daily_climvars,
#'   .f = prep_daily_weather,
#'   studyArea = studyAreaBC,
#'   id = "FRT",
#'   start = "2018-01-01",
#'   end = "2019-12-31"
#' ) |>
#'   purrr::list_rbind()
#'
#' head(daily_weather)
#'
#' @export
#' @rdname prep_climate_data
prep_daily_weather <- function(var = NULL, studyArea = NULL, id = NULL, start = NULL, end = NULL) {
  stopifnot(
    !is.null(var),
    !is.null(studyArea),
    !is.null(id),
    !is.null(start),
    !is.null(end)
  )

  ## We want a data.frame with columns:
  ##   Year  Month  Day  Variable  Eco1 Eco2 Eco3 ...

  ## SpatRaster |> sf |> long_df |> wide_df
  df <- climateR::getDaymet(
    AOI = studyArea,
    varname = "prcp",
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
      cols = starts_with("mean"),
      names_to = c(NA, "Year", "Month", "Day", NA, NA),
      names_prefix = "mean.",
      names_sep = "(-|_)",
      values_to = "Value"
    ) |>
    tidyr::pivot_wider(
      names_from = id,
      values_from = "Value"
    ) |>
    dplyr::mutate(
      Variable = "precip",
      .after = "Day"
    )

  df
}
