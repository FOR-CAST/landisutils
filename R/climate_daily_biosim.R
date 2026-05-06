#' Get an elevation raster for a study area
#'
#' Fetches an elevation raster covering `studyArea` using
#' [elevatr::get_elev_raster()] (AWS Terrain Tiles) and converts to a
#' `SpatRaster` in lon-lat (`EPSG:4326`). The result seeds point locations for
#' BioSIM via [create_locations_df()].
#'
#' @param studyArea `sf` or `SpatVector` polygons object delineating the area
#'   of interest.
#'
#' @param z integer zoom level passed to [elevatr::get_elev_raster()]; higher
#'   values give finer resolution. Default `9` (~250 m at mid-latitudes).
#'
#' @param tmp_dir character. Directory for `elevatr`'s per-tile downloads.
#'   Defaults to `<landisutils.cache.path>/elevatr_tiles/`, which keeps the
#'   AWS Terrain Tile fragments inside the package cache instead of leaking
#'   into the R session's global `tempdir()` (`elevatr`'s own default).
#'
#' @returns `SpatRaster` of elevations in metres, in lon-lat CRS.
#'
#' @seealso [create_locations_df()], [get_clim_daily()]
#'
#' @export
get_elevation_rast <- function(
  studyArea,
  z = 9,
  tmp_dir = file.path(.climateCachePath(), "elevatr_tiles")
) {
  stopifnot(
    requireNamespace("elevatr", quietly = TRUE),
    requireNamespace("sf", quietly = TRUE),
    requireNamespace("terra", quietly = TRUE)
  )

  if (inherits(studyArea, "SpatVector")) {
    studyArea <- sf::st_as_sf(studyArea)
  }
  studyArea <- sf::st_transform(studyArea, crs = 4326)

  fs::dir_create(tmp_dir)

  elevatr::get_elev_raster(
    locations = studyArea,
    src = "aws",
    z = z,
    tmp_dir = tmp_dir,
    verbose = FALSE
  ) |>
    terra::rast()
}

#' Build batched point-location table from a study area
#'
#' Extracts the lon-lat / elevation of every cell in `elev` that overlaps
#' `studyArea`, tags each cell with its containing polygon's `id` value
#' (column `EcoID`) and a numeric `BatchID`, and splits into a list of
#' `batch_size`-row data frames suitable for chunked dispatch through
#' [get_clim_daily()].
#'
#' @param elev `SpatRaster` of elevations in lon-lat CRS (typically from
#'   [get_elevation_rast()]).
#'
#' @param studyArea `sf` or `SpatVector` polygons object delineating the area
#'   of interest.
#'
#' @param id character. Name of the field in `studyArea` identifying ecoregions
#'   (e.g. `"PolyID"`).
#'
#' @param batch_size integer. Maximum cells per batch. Default `1000`.
#'
#' @returns A list of data frames with columns `ID`, `Longitude`, `Latitude`,
#'   `Elevation`, `EcoID`, `BatchID` (one element per batch).
#'
#' @seealso [get_elevation_rast()], [get_clim_daily()]
#'
#' @export
create_locations_df <- function(elev, studyArea, id, batch_size = 1000) {
  stopifnot(
    requireNamespace("terra", quietly = TRUE),
    !missing(id),
    is.character(id),
    length(id) == 1L
  )

  if (inherits(studyArea, "sf")) {
    studyArea <- terra::vect(studyArea)
  }

  studyArea <- terra::project(studyArea, terra::crs(elev))

  stopifnot(id %in% names(studyArea))

  cellMatrix <- terra::cells(elev, studyArea)
  poly_idx <- cellMatrix[, 1]
  cellIDs <- cellMatrix[, 2]
  coords <- terra::xyFromCell(elev, cellIDs)
  eco_ids <- terra::values(studyArea)[[id]][poly_idx]
  elevations <- terra::values(elev, mat = FALSE)[cellIDs]

  batch_id <- seq_along(cellIDs) %/% batch_size

  data.frame(
    ID = cellIDs,
    Longitude = coords[, 1],
    Latitude = coords[, 2],
    Elevation = elevations,
    EcoID = eco_ids,
    BatchID = batch_id
  ) |>
    split(batch_id)
}

## Allowed values for the BioSIM `rcp` and `climModel` arguments forwarded by
## [get_clim_daily()] / [get_clim_monthly()] / [prep_daily_weather()] /
## [prep_monthly_weather_biosim()]. Kept as package-internal constants so
## validation logic stays in one place.
.biosim_rcp_choices <- c("CONSTANT_CLIMATE", "RCP45", "RCP85")
.biosim_clim_model_choices <- c("RCM4", "GCM4", "Hadley")

#' @keywords internal
.biosim_scenario_tag <- function(rcp, clim_model) {
  paste0(rcp, "_", clim_model)
}

#' Fetch a single year of daily BioSIM weather for one location batch
#'
#' Wraps [BioSIM::generateWeather()] for the `ClimaticEx_Daily` model,
#' fetching one calendar year for the cells in `locations_batch[[1]]`.
#' Output is appended to an Arrow CSV dataset under
#' `path/ClimaticEx_Daily/<studyArea_hash>/<rcp>_<clim_model>/`, partitioned by
#' `YEAR`/`BatchID`. The function is intended to be run as a dynamic target
#' across many `(batch, year)` combinations; existing partition files are
#' detected via `file.exists()` and skipped, so re-runs are cheap.
#'
#' For years within BioSIM's observational range, `rcp` and `clim_model` have
#' no effect on the returned data, but they do still namespace the cache.
#' For future years they select the CMIP5-era projection: `rcp` is one of
#' `"CONSTANT_CLIMATE"`, `"RCP45"`, or `"RCP85"`; `clim_model` is one of
#' `"RCM4"` (Canadian Regional Climate Model 4, CanESM2-driven), `"GCM4"`
#' (CGCM4 / CanESM2-family GCM), or `"Hadley"` (HadGEM).
#'
#' Rate-limiting against the BioSIM web service is the **caller's**
#' responsibility - cap parallel workers (e.g. `future::plan(multisession,
#' workers = 4)`) or stagger orchestrator calls.
#' This function performs no internal throttling.
#'
#' @template section_climate_topography
#'
#' @param locations_batch list whose first element is a data frame with columns
#'   `ID`, `Latitude`, `Longitude`, `Elevation`, `EcoID`, `BatchID`. Typically
#'   one element of [create_locations_df()]'s output, wrapped in a list.
#'
#' @param year integer single calendar year to fetch.
#'
#' @param studyArea_hash character. Short hash of the study-area object (see
#'   `.studyArea_hash()`) used as a cache subdirectory so that distinct study
#'   areas don't collide.
#'
#' @param path character. Directory under which the `ClimaticEx_Daily/` arrow
#'   dataset is written. Default uses the package climate cache
#'   (`getOption("landisutils.cache.path")`).
#'
#' @param rcp character. BioSIM representative concentration pathway. One of
#'   `"CONSTANT_CLIMATE"`, `"RCP45"`, `"RCP85"`. Default `"RCP45"` (BioSIM's
#'   own default).
#'
#' @param clim_model character. BioSIM climate model. One of `"RCM4"`,
#'   `"GCM4"`, `"Hadley"`. Default `"RCM4"` (BioSIM's own default).
#'
#' @returns Path (character) to the partition CSV for `(year, BatchID)`.
#'
#' @seealso [get_elevation_rast()], [create_locations_df()],
#'   [assemble_climate_library_file()], [get_fwi_daily()]
#'
#' @export
get_clim_daily <- function(
  locations_batch,
  year,
  studyArea_hash,
  path = .climateCachePath(),
  rcp = "RCP45",
  clim_model = "RCM4"
) {
  stopifnot(
    requireNamespace("BioSIM", quietly = TRUE),
    requireNamespace("arrow", quietly = TRUE),
    is.character(studyArea_hash),
    length(studyArea_hash) == 1L
  )
  rcp <- match.arg(rcp, .biosim_rcp_choices)
  clim_model <- match.arg(clim_model, .biosim_clim_model_choices)

  locations <- locations_batch[[1]]
  batch_id <- unique(locations$BatchID)
  stopifnot(length(batch_id) == 1L)

  scenario_tag <- .biosim_scenario_tag(rcp, clim_model)
  dataset <- file.path(path, "ClimaticEx_Daily", studyArea_hash, scenario_tag) |> fs::dir_create()
  out_file <- file.path(dataset, paste0("YEAR=", year), paste0("BatchID=", batch_id), "part-0.csv")

  if (!file.exists(out_file)) {
    ClimaticEx_Daily_year <- BioSIM::generateWeather(
      modelNames = "ClimaticEx_Daily",
      fromYr = year,
      toYr = year,
      id = locations$ID,
      latDeg = locations$Latitude,
      longDeg = locations$Longitude,
      elevM = locations$Elevation,
      rcp = rcp,
      climModel = clim_model,
      additionalParms = NULL
    )$ClimaticEx_Daily |>
      dplyr::mutate(dplyr::across(dplyr::where(is.numeric), ~ replace(., . == -9999, NA))) |>
      dplyr::rename(CellID = KeyID, YEAR = Year, MONTH = Month, DAY = Day) |>
      dplyr::mutate(
        DATE = as.Date(paste0(YEAR, "-", MONTH, "-", DAY), format = "%Y-%m-%d"),
        .before = "YEAR"
      ) |>
      dplyr::mutate(JULIAN_DAY = as.integer(format(DATE, "%j")), .after = "DAY") |>
      dplyr::mutate(ID = paste0(YEAR, "_", JULIAN_DAY), .before = "YEAR") |>
      dplyr::mutate(BatchID = batch_id)

    eco_lookup <- data.frame(CellID = locations$ID, EcoID = locations$EcoID)
    ClimaticEx_Daily_year <- dplyr::left_join(ClimaticEx_Daily_year, eco_lookup, by = "CellID")

    stopifnot(
      all(ClimaticEx_Daily_year$Prcp >= 0, na.rm = TRUE),
      all(ClimaticEx_Daily_year$RelH >= 0, na.rm = TRUE),
      all(ClimaticEx_Daily_year$RelH <= 100, na.rm = TRUE),
      all(ClimaticEx_Daily_year$WndS >= 0, na.rm = TRUE),
      all(ClimaticEx_Daily_year$WndD >= 0, na.rm = TRUE),
      all(ClimaticEx_Daily_year$WndD <= 360, na.rm = TRUE),
      all(ClimaticEx_Daily_year$SRad >= 0, na.rm = TRUE)
    )

    ClimaticEx_Daily_year <- dplyr::group_by(ClimaticEx_Daily_year, YEAR, BatchID)

    arrow::write_dataset(ClimaticEx_Daily_year, path = dataset, format = "csv")
  }

  out_file
}

## map lowercase climate-variable names (used by the public `prep_daily_weather()`
## API) to the BioSIM `ClimaticEx_Daily` model's column names.
.daily_var_to_biosim <- c(
  prcp = "Prcp",
  tmax = "Tmax",
  tmin = "Tmin",
  rh = "RelH",
  ws = "WndS",
  wnddir = "WndD",
  srad = "SRad",
  temp = "Tair"
)

#' Prepare Climate Data
#'
#' Download and prepare climate data for use with LANDIS-II simulations.
#'
#' [prep_daily_weather()] retrieves daily weather from BioSIM
#' (`ClimaticEx_Daily` model) for cells in `studyArea`, summarizes by ecoregion
#' (`id`), and pivots to the LANDIS-II Climate Library wide format. Internally
#' it chains [get_elevation_rast()], [create_locations_df()], [get_clim_daily()]
#' (per batch / per year), and [assemble_climate_library_file()]. Per-batch /
#' per-year results are cached as an Arrow CSV dataset under
#' `getOption("landisutils.cache.path")`, partitioned by
#' `<rcp>_<clim_model>`/`YEAR`/`BatchID`.
#'
#' [prep_monthly_weather()] retrieves monthly weather from TerraClim via the
#' \pkg{climateR} package and summarizes by zone using the \pkg{zonal} package.
#'
#' [prep_monthly_weather_biosim()] retrieves monthly weather from BioSIM
#' (`Climatic_Monthly` model) using the same point-batch / Arrow caching
#' pipeline as [prep_daily_weather()], so cells are co-located with the daily
#' workflow. Internally it chains [get_elevation_rast()], [create_locations_df()],
#' [get_clim_monthly()] (per batch / per year), and
#' [assemble_climate_library_file_monthly()]. Per-batch / per-year results are
#' cached as an Arrow CSV dataset under `getOption("landisutils.cache.path")`,
#' partitioned by `<rcp>_<clim_model>`/`YEAR`/`BatchID`.
#'
#' [prep_monthly_weather_climr()] retrieves monthly weather from
#' [climr](https://github.com/bcgov/climr), a ClimateNA-style change-factor
#' downscaler against a PRISM/DAYMET composite reference grid. Cells come from
#' the same elevation/point machinery as the BioSIM pipeline
#' ([get_elevation_rast()], [create_locations_df()]); a single
#' [climr::downscale()] call per year is dispatched via [furrr::future_map()].
#' climr maintains its own on-disk cache, configured via
#' `options("climr.cache.path" = ...)`; the wrapper defaults this to a
#' `climr/` subdirectory of `getOption("landisutils.cache.path")` when the
#' option is unset.
#'
#' @section Historical daily weather (BioSIM):
#' BioSIM provides modelled daily North American weather. The
#' `ClimaticEx_Daily` model exposes columns `Prcp`, `Tmin`, `Tmax`, `Tair`,
#' `RelH`, `WndS`, `WndD`, `SRad`. Pass the corresponding lowercase variable
#' names to `vars` (e.g. `c("prcp", "tmax", "tmin", "rh", "ws", "wnddir",
#' "srad")`).
#'
#' @section Historical monthly weather (TerraClim):
#' Terra Climate provides monthly North American weather 1980-present
#' (<https://www.climatologylab.org/terraclimate.html>).
#' TerraClim variables: `aet`, `def`, `PDSI`, `pet`, `ppt`, `q`, `soil`, `srad`,
#' `swe`, `tmax`, `tmin`, `vap`, `vpd`, `ws`.
#' Use [prep_monthly_weather()] for TerraClim weather data.
#'
#' @section Historical monthly weather (BioSIM):
#' BioSIM's `Climatic_Monthly` model exposes columns `TotalPrcp`, `MeanTmin`,
#' `MeanTmax`, `MeanTair`, `MeanRelH`, `TotalRadiation` (among others); wind
#' variables come from a companion `ClimaticWind_Monthly` model fetched in the
#' same call (`WindSpeed` plus 36 directional-frequency columns `W0`..`W350`,
#' from which a frequency-weighted circular-mean wind from-direction is
#' computed). Pass the corresponding lowercase variable names to `vars` (e.g.
#' `c("prcp", "tmax", "tmin", "temp", "rh", "srad", "ws", "wnddir")`).
#' Use [prep_monthly_weather_biosim()] for BioSIM monthly weather.
#'
#' @section Historical monthly weather (climr):
#' \pkg{climr} provides ClimateNA-style change-factor downscaling against a
#' PRISM/DAYMET composite reference grid for North America, with observational
#' time series spanning 1901-2024
#' (`obs_ts_dataset = "climatena"` or `"cru.gpcc"`). Supported lowercase
#' `vars`: `prcp`, `tmax`, `tmin`, `temp`, `rh`, `srad`, `cmd` (climatic
#' moisture deficit). Wind variables (`ws`, `wnddir`) are **not** provided by
#' climr - use [prep_monthly_weather_biosim()] for wind. Use
#' [prep_monthly_weather_climr()] for climr monthly weather.
#'
#' @section Climate projections (BioSIM, CMIP5):
#' [prep_daily_weather()] and [prep_monthly_weather_biosim()] accept `rcp` and
#' `clim_model` arguments that are forwarded to BioSIM. `rcp` is one of
#' `"CONSTANT_CLIMATE"` (1991-2020 climate replayed), `"RCP45"`, or `"RCP85"`.
#' `clim_model` is one of `"RCM4"` (Canadian Regional Climate Model 4,
#' CanESM2-driven), `"GCM4"` (CGCM4 / CanESM2-family GCM), or `"Hadley"`
#' (HadGEM). For years within BioSIM's observational range these arguments
#' have no effect on the returned data; they always namespace the on-disk
#' cache (`<rcp>_<clim_model>` subdirectory under `<studyArea_hash>/`).
#'
#' @section Climate projections (climr, CMIP6):
#' [prep_monthly_weather_climr()] accepts `gcms`, `ssps`, and `max_run`
#' arguments forwarded to [climr::downscale()] for CMIP6 GCM projections. When
#' `gcms` is `NULL` the function runs in observational mode; otherwise it runs
#' in projection mode using `gcm_ssp_years = years`. The bcgov-recommended
#' eight-member ensemble subset is exposed as the package constant
#' [`climr_ensemble_8`]; pass `gcms = climr_ensemble_8` to use it.
#' [assemble_climate_library_file_monthly_climr()] averages over GCMs and SSPs
#' to give a single LANDIS-II-compatible value per `(Year, Month, EcoID,
#' Variable)`. See [climr::list_gcms()], [climr::list_ssps()] for the full
#' choice sets.
#'
#' @section Rate-limiting:
#' BioSIM is a remote web service. When parallelizing over batches/years (e.g.
#' with `future::plan(multisession)`), keep the worker count modest (\eqn{\le}
#' 4) to avoid hammering the service.
#'
#' @section Caching:
#' Per-batch / per-year BioSIM fetches are cached as Arrow CSV partitions under
#' the directory given by `getOption("landisutils.cache.path")`. Re-runs that
#' find an existing partition skip the BioSIM call.
#' [prep_monthly_weather_climr()] relies on climr's own on-disk cache instead;
#' the cache root is `getOption("climr.cache.path")`, defaulting (for the
#' duration of the call) to `file.path(getOption("landisutils.cache.path"),
#' "climr")` when unset.
#'
#' @template section_climate_topography
#'
#' @param vars character specifying the climate variables (lowercase, e.g.
#'   `"prcp"`, `"tmax"`, `"tmin"`).
#'
#' @param studyArea `sf` polygons object delineating e.g., ecoregions or fire
#'   zones.
#'
#' @param id character specifying the name of the column/field in `studyArea`
#'   identifying ecoregions.
#'
#' @param years integer vector specifying the years.
#'
#' @param batch_size integer maximum cells per BioSIM batch call. Default
#'   `1000`. Smaller values yield more parallel-friendly chunks.
#'
#' @param z integer zoom level passed to [elevatr::get_elev_raster()] for the
#'   underlying elevation raster. Higher values give a finer raster (more
#'   point cells fed to BioSIM/climr); lower values give a coarser raster
#'   (fewer cells, fewer batches, faster fetches). Default `9` (~250 m at
#'   mid-latitudes). For tests / smoke runs, values around `5`-`7` typically
#'   yield a single BioSIM batch over a small study area.
#'
#' @param obs_ts_dataset character. climr observational time-series dataset.
#'   One of `"climatena"` or `"cru.gpcc"`. Default `"climatena"`. Only used
#'   by [prep_monthly_weather_climr()] in observational mode.
#'
#' @param rcp character. BioSIM representative concentration pathway. One of
#'   `"CONSTANT_CLIMATE"`, `"RCP45"`, or `"RCP85"`. Default `"RCP45"`. Only
#'   used by the BioSIM pipelines ([prep_daily_weather()],
#'   [prep_monthly_weather_biosim()]). See *Climate projections (BioSIM,
#'   CMIP5)*.
#'
#' @param clim_model character. BioSIM climate model. One of `"RCM4"`,
#'   `"GCM4"`, or `"Hadley"`. Default `"RCM4"`. Only used by the BioSIM
#'   pipelines.
#'
#' @param gcms character vector of CMIP6 GCM names (subset of
#'   [climr::list_gcms()]). When `NULL` (default), [prep_monthly_weather_climr()]
#'   runs in observational mode; when set, in projection mode. The
#'   bcgov-recommended eight-member ensemble is exposed as
#'   [`climr_ensemble_8`]. Only used by [prep_monthly_weather_climr()].
#'
#' @param ssps character vector of CMIP6 SSP names (subset of
#'   [climr::list_ssps()], e.g. `"ssp245"`). Required when `gcms` is set. Only
#'   used by [prep_monthly_weather_climr()].
#'
#' @param max_run integer number of ensemble runs per GCM to fetch from climr
#'   (averaged together when `ensemble_mean = TRUE`, the climr default).
#'   Default `0L` (climr's own default; uses the ensemble mean only). Only
#'   used by [prep_monthly_weather_climr()].
#'
#' @returns `tbl_df` with columns `Year`, `Month`, `Variable`, and one column
#'   per ecoregion (named by `id` values). Daily output ([prep_daily_weather()])
#'   additionally carries a `Day` column.
#'
#' @examples
#' if (requireNamespace("BioSIM", quietly = TRUE) &&
#'   requireNamespace("elevatr", quietly = TRUE) &&
#'   requireNamespace("arrow", quietly = TRUE)) {
#'   # ecoregionPolys <- landisutils::test_ecoregionPolys
#'   # clim_years <- 2018:2019
#'   # daily_weather <- prep_daily_weather(
#'   #   vars = c("prcp", "tmax", "tmin"),
#'   #   years = clim_years,
#'   #   studyArea = ecoregionPolys,
#'   #   id = "PolyID"
#'   # )
#'   # head(daily_weather)
#' }
#' @export
#' @rdname prep_climate_data
prep_daily_weather <- function(
  vars = NULL,
  years = NULL,
  studyArea = NULL,
  id = NULL,
  batch_size = 1000,
  z = 9,
  rcp = "RCP45",
  clim_model = "RCM4"
) {
  stopifnot(
    requireNamespace("BioSIM", quietly = TRUE),
    requireNamespace("arrow", quietly = TRUE),
    requireNamespace("elevatr", quietly = TRUE),
    requireNamespace("furrr", quietly = TRUE),
    requireNamespace("purrr", quietly = TRUE),
    requireNamespace("sf", quietly = TRUE)
  )

  stopifnot(!is.null(vars), !is.null(studyArea), !is.null(id), !is.null(years))
  rcp <- match.arg(rcp, .biosim_rcp_choices)
  clim_model <- match.arg(clim_model, .biosim_clim_model_choices)

  unknown <- setdiff(vars, names(.daily_var_to_biosim))
  if (length(unknown)) {
    stop(
      "unknown daily climate variable(s): ",
      glue::glue_collapse(unknown, sep = ", "),
      ". Known: ",
      glue::glue_collapse(names(.daily_var_to_biosim), sep = ", ")
    )
  }
  biosim_vars <- unname(.daily_var_to_biosim[vars])

  elev <- get_elevation_rast(studyArea, z = z)
  locations_batches <- create_locations_df(
    elev = elev,
    studyArea = studyArea,
    id = id,
    batch_size = batch_size
  )

  cache_root <- .climateCachePath()
  sa_hash <- .studyArea_hash(studyArea)
  scenario_tag <- .biosim_scenario_tag(rcp, clim_model)
  grid <- expand.grid(
    batch_idx = seq_along(locations_batches),
    year = years,
    KEEP.OUT.ATTRS = FALSE,
    stringsAsFactors = FALSE
  )

  furrr::future_map2(.x = grid$batch_idx, .y = grid$year, .f = function(b, y) {
    get_clim_daily(
      locations_batches[b],
      year = y,
      studyArea_hash = sa_hash,
      path = cache_root,
      rcp = rcp,
      clim_model = clim_model
    )
  })

  assemble_climate_library_file(
    dataset_path = file.path(cache_root, "ClimaticEx_Daily", sa_hash, scenario_tag),
    vars = biosim_vars,
    id_col = "EcoID"
  ) |>
    dplyr::filter(Year %in% years)
}

#' Assemble LANDIS-II Climate Library wide-format table from a BioSIM Arrow dataset
#'
#' Reads the partitioned Arrow CSV dataset written by [get_clim_daily()],
#' summarizes the requested variables by `(Year, Month, Day, EcoID)`, applies
#' unit conversions to match LANDIS-II conventions, and pivots to the wide
#' format ingested by `LandisClimateConfig`. The result is the same shape that
#' [prep_daily_weather()] returns and is suitable for [writeClimateData()].
#'
#' Unit conversions applied:
#' \itemize{
#'   \item `Prcp` (mm) \eqn{\to} cm (\eqn{\div 10})
#'   \item `WndS` (km/h) \eqn{\to} m/s (\eqn{\div 3.6})
#' }
#'
#' BioSIM `WndD` is reported as the wind \emph{from-direction} (degrees), which
#' is what the LANDIS-II Climate Library expects.
#'
#' @param dataset_path character. Path to the `ClimaticEx_Daily` Arrow dataset
#'   directory (containing `YEAR=...`/`BatchID=...`/`part-0.csv` partitions).
#'
#' @param vars character vector of BioSIM column names to retain (e.g.
#'   `c("Prcp", "Tmin", "Tmax")`).
#'
#' @param id_col character. Name of the ecoregion-id column in the dataset.
#'   Default `"EcoID"`.
#'
#' @returns `tbl_df` with columns `Year`, `Month`, `Day`, `Variable`, and one
#'   column per ecoregion id.
#'
#' @template section_climate_topography
#'
#' @seealso [get_clim_daily()], [prep_daily_weather()], [writeClimateData()]
#'
#' @export
assemble_climate_library_file <- function(dataset_path, vars, id_col = "EcoID") {
  stopifnot(
    requireNamespace("arrow", quietly = TRUE),
    is.character(vars),
    length(vars) >= 1L,
    is.character(id_col),
    length(id_col) == 1L
  )

  biosim_to_landis <- c(
    Prcp = "precip",
    Tmin = "Tmin",
    Tmax = "Tmax",
    Tair = "temp",
    RelH = "RH",
    WndS = "windSpeed",
    WndD = "windDirection",
    SRad = "SWR"
  )

  unknown <- setdiff(vars, names(biosim_to_landis))
  if (length(unknown)) {
    stop(
      "unknown BioSIM variable(s): ",
      glue::glue_collapse(unknown, sep = ", "),
      ". Known: ",
      glue::glue_collapse(names(biosim_to_landis), sep = ", ")
    )
  }

  df <- arrow::open_dataset(dataset_path, format = "csv") |> dplyr::collect()

  keep_cols <- c("YEAR", "MONTH", "DAY", id_col, vars)
  missing_cols <- setdiff(keep_cols, names(df))
  if (length(missing_cols)) {
    stop("dataset is missing expected column(s): ", glue::glue_collapse(missing_cols, sep = ", "))
  }
  df <- df[, keep_cols, drop = FALSE]

  if ("Prcp" %in% vars) {
    df$Prcp <- df$Prcp / 10 ## mm -> cm
  }
  if ("WndS" %in% vars) {
    df$WndS <- df$WndS / 3.6 ## km/h -> m/s
  }

  long <- df |>
    tidyr::pivot_longer(cols = dplyr::all_of(vars), names_to = "Variable", values_to = "Value") |>
    dplyr::group_by(YEAR, MONTH, DAY, .data[[id_col]], Variable) |>
    dplyr::summarise(Value = mean(Value, na.rm = TRUE), .groups = "drop") |>
    dplyr::mutate(Variable = unname(biosim_to_landis[Variable]))

  wide <- long |>
    tidyr::pivot_wider(names_from = dplyr::all_of(id_col), values_from = "Value") |>
    dplyr::rename(Year = YEAR, Month = MONTH, Day = DAY) |>
    dplyr::arrange(Year, Month, Day, Variable)

  tibble::as_tibble(wide)
}

#' Assemble Social-Climate-Fire-shaped daily climate CSV from a BioSIM Arrow dataset
#'
#' A thin wrapper around [assemble_climate_library_file()] that rewrites the
#' output `Variable` column from LANDIS-II Climate Library spellings (`Tmin`,
#' `Tmax`, `windSpeed`, `windDirection`, `RH`, `SWR`) to the lowercase
#' Social-Climate-Fire spellings (`mintemp`, `maxtemp`, `windspeed`,
#' `winddirection`, `rh`, `swr`). The resulting `tbl_df` - and the CSV it
#' produces via [writeClimateData()] - is analogous to the LANDIS-II Social-Climate-Fire
#' v4 reference input
#' [`LTB_ClimateInputs_91_10_v2.csv`](https://github.com/LANDIS-II-Foundation/Extension-Social-Climate-Fire/blob/master/Testing/Core8-SocialClimateFire4.0/LTB_ClimateInputs_91_10_v2.csv).
#'
#' Social Climate Fire reads daily fire-weather indirectly through the LANDIS-II
#' Climate Library (`Daily_RandomYears` / `Daily_AverageAllYears` time series),
#' which then computes FWI internally. The Climate Library parser is
#' case-insensitive and accepts both naming conventions, so this helper exists
#' to produce a CSV whose `Variable` column visually matches the
#' Social-Climate-Fire reference; [assemble_climate_library_file()] (which keeps
#' Climate-Library-style spellings) is equally valid input to the parser.
#'
#' @inheritParams assemble_climate_library_file
#'
#' @returns `tbl_df` with the same shape as [assemble_climate_library_file()]
#'   but `Variable` values rewritten to the lowercase Social-Climate-Fire
#'   convention (`precip`, `mintemp`, `maxtemp`, `temp`, `rh`, `windspeed`,
#'   `winddirection`, `swr`).
#'
#' @template section_climate_topography
#'
#' @family Social Climate Fire helpers
#'
#' @seealso [assemble_climate_library_file()], [get_clim_daily()],
#'   [writeClimateData()]
#'
#' @export
assemble_climate_library_file_scf <- function(dataset_path, vars, id_col = "EcoID") {
  landis_to_scf <- c(
    precip = "precip",
    Tmin = "mintemp",
    Tmax = "maxtemp",
    temp = "temp",
    RH = "rh",
    windSpeed = "windspeed",
    windDirection = "winddirection",
    SWR = "swr"
  )

  df <- assemble_climate_library_file(dataset_path, vars = vars, id_col = id_col)
  df$Variable <- unname(landis_to_scf[df$Variable])
  df
}

## non-exported cffdrs helpers - resolved lazily on first use
.cffdrs_fn <- function(name) {
  utils::getFromNamespace(name, "cffdrs")
}

#' Fetch a single year of daily BioSIM Fire Weather Index for one location batch
#'
#' Wraps [BioSIM::generateWeather()] for the `FWI_Daily` model, fetching one
#' calendar year for the cells in `locations_batch[[1]]`. Output is appended
#' to an Arrow CSV dataset under `path/FWI_Daily/`, partitioned by
#' `YEAR`/`BatchID`. Like [get_clim_daily()], the function is intended to be
#' run as a dynamic target across many `(batch, year)` combinations; existing
#' partition files are detected via `file.exists()` and skipped, so re-runs
#' are cheap.
#'
#' BioSIM's `FWI_Daily` model is known to occasionally produce implausibly
#' large `FFMC`, `ISI`, and `FWI` values
#' (see <https://github.com/RNCan/BioSimClient_R/issues/14>). To work around
#' this, `FFMC` values \eqn{>} 101 are recomputed from the underlying
#' `T`/`RH`/`WS`/`Prcp` columns using \pkg{cffdrs}, and `ISI` and `FWI` are
#' then recomputed from the corrected `FFMC`. The unused `DSR` column is
#' dropped.
#'
#' Rate-limiting against the BioSIM web service is the **caller's**
#' responsibility - cap parallel workers (e.g. `future::plan(multisession,
#' workers = 4)`) or stagger orchestrator calls. A small random delay
#' (5-30 s) is applied per call to spread BioSIM hits when many parallel
#' workers start at once.
#'
#' @template section_climate_topography
#'
#' @param locations_batch list whose first element is a data frame with columns
#'   `ID`, `Latitude`, `Longitude`, `Elevation`, `EcoID`, `BatchID`. Typically
#'   one element of [create_locations_df()]'s output, wrapped in a list.
#'
#' @param year integer single calendar year to fetch.
#'
#' @param studyArea_hash character. Short hash of the study-area object (see
#'   `.studyArea_hash()`) used as a cache subdirectory so that distinct study
#'   areas don't collide.
#'
#' @param params list of additional parameters forwarded to BioSIM as
#'   `additionalParms = list(FWI_Daily = params)`. Default `NULL`.
#'
#' @param path character. Directory under which the `FWI_Daily/` arrow dataset
#'   is written. Default uses the package climate cache
#'   (`getOption("landisutils.cache.path")`).
#'
#' @returns Path (character) to the partition CSV for `(year, BatchID)`.
#'
#' @seealso [get_elevation_rast()], [create_locations_df()], [get_clim_daily()]
#'
#' @export
get_fwi_daily <- function(
  locations_batch,
  year,
  studyArea_hash,
  params = NULL,
  path = .climateCachePath()
) {
  stopifnot(
    requireNamespace("BioSIM", quietly = TRUE),
    requireNamespace("arrow", quietly = TRUE),
    is.character(studyArea_hash),
    length(studyArea_hash) == 1L
  )

  locations <- locations_batch[[1]]
  batch_id <- unique(locations$BatchID)
  stopifnot(length(batch_id) == 1L)

  dataset <- file.path(path, "FWI_Daily", studyArea_hash) |> fs::dir_create()
  out_file <- file.path(dataset, paste0("YEAR=", year), paste0("BatchID=", batch_id), "part-0.csv")

  if (!file.exists(out_file)) {
    Sys.sleep(sample(seq(5, 30, 1), 1)) ## spread BioSIM hits across workers

    FWI_Daily_year <- BioSIM::generateWeather(
      modelNames = "FWI_Daily",
      fromYr = year,
      toYr = year,
      id = locations$ID,
      latDeg = locations$Latitude,
      longDeg = locations$Longitude,
      elevM = locations$Elevation,
      additionalParms = list(FWI_Daily = params)
    )$FWI_Daily

    FWI_cleaned <- FWI_Daily_year |>
      dplyr::mutate(dplyr::across(dplyr::where(is.numeric), ~ replace(., . == -9999, NA))) |>
      tidyr::drop_na(FWI) |>
      dplyr::rename(CellID = KeyID, YEAR = Year, MONTH = Month, DAY = Day) |>
      dplyr::mutate(
        DATE = as.Date(paste0(YEAR, "-", MONTH, "-", DAY), format = "%Y-%m-%d"),
        .before = "YEAR"
      ) |>
      dplyr::mutate(JULIAN_DAY = as.integer(format(DATE, "%j")), .after = "DAY") |>
      dplyr::mutate(ID = paste0(YEAR, "_", JULIAN_DAY), .before = "YEAR") |>
      dplyr::mutate(BatchID = batch_id)

    eco_lookup <- data.frame(CellID = locations$ID, EcoID = locations$EcoID)
    FWI_cleaned <- dplyr::left_join(FWI_cleaned, eco_lookup, by = "CellID")

    ## FFMC must be corrected first because ISI (and hence FWI) is derived from it.
    .fine_fuel_moisture_code <- .cffdrs_fn("fine_fuel_moisture_code")
    .initial_spread_index <- .cffdrs_fn("initial_spread_index")
    .fire_weather_index <- .cffdrs_fn("fire_weather_index")

    FWI_fixed <- FWI_cleaned |>
      dplyr::mutate(
        FFMC = dplyr::if_else(
          FFMC <= 101,
          FFMC,
          .fine_fuel_moisture_code(
            ffmc_yda = dplyr::lag(FFMC),
            temp = .data$T,
            rh = RH,
            ws = WS,
            prec = Prcp
          )
        )
      ) |>
      dplyr::mutate(
        ISI = .initial_spread_index(FFMC, WS),
        FWI = .fire_weather_index(ISI, BUI),
        ## TODO: should also correct DSR; not used downstream so dropped
        DSR = NULL
      )

    stopifnot(
      all(FWI_fixed$RH >= 0, na.rm = TRUE),
      all(FWI_fixed$RH <= 100, na.rm = TRUE),
      all(FWI_fixed$WD >= 0, na.rm = TRUE),
      all(FWI_fixed$WD <= 360, na.rm = TRUE),
      all(FWI_fixed$FFMC >= 0, na.rm = TRUE),
      all(FWI_fixed$FFMC <= 101, na.rm = TRUE),
      all(FWI_fixed$DMC >= 0, na.rm = TRUE),
      all(FWI_fixed$DC >= 0, na.rm = TRUE),
      all(FWI_fixed$ISI >= 0, na.rm = TRUE),
      all(FWI_fixed$ISI < 100, na.rm = TRUE),
      all(FWI_fixed$BUI >= 0, na.rm = TRUE),
      all(FWI_fixed$FWI >= 0, na.rm = TRUE)
    )

    FWI_fixed <- dplyr::group_by(FWI_fixed, YEAR, BatchID)

    arrow::write_dataset(FWI_fixed, path = dataset, format = "csv")
  }

  out_file
}
