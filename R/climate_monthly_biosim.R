## map lowercase climate-variable names (used by the public
## `prep_monthly_weather_biosim()` API) to BioSIM monthly model column names.
## Wind variables (`ws`, `wnddir`) come from the `ClimaticWind_Monthly` model;
## all others come from `Climatic_Monthly`. [get_clim_monthly()] fetches both
## models and merges them into a single per-(year, batch) partition so that
## downstream assembly can treat them uniformly.
.monthly_var_to_biosim <- c(
  prcp = "TotalPrcp",
  tmax = "MeanTmax",
  tmin = "MeanTmin",
  temp = "MeanTair",
  rh = "MeanRelH",
  srad = "TotalRadiation",
  ws = "WndS",
  wnddir = "WndD"
)

## Compute a frequency-weighted circular mean direction (degrees, 0-360) from
## the 36 directional-frequency columns `W0`, `W10`, ..., `W350` produced by
## BioSIM's `ClimaticWind_Monthly` model. Returns `NA` for rows where all
## frequencies are zero or NA.
.wind_dir_circular_mean <- function(freq_matrix) {
  deg_seq <- seq(0, 350, by = 10)
  rad <- deg_seq * pi / 180
  freq_matrix[is.na(freq_matrix)] <- 0
  total <- rowSums(freq_matrix)
  sum_sin <- as.numeric(freq_matrix %*% sin(rad))
  sum_cos <- as.numeric(freq_matrix %*% cos(rad))
  out <- (atan2(sum_sin, sum_cos) * 180 / pi + 360) %% 360
  out[total == 0] <- NA_real_
  out
}

#' BioSIM monthly + wind pull for one batch of locations, formatted to climate-only rows
#'
#' Internal worker shared by [get_clim_monthly()] (per-study-area cache) and the global per-cell cache in
#' [prep_monthly_weather_biosim()]. Returns ONLY climate columns keyed by `CellID` (no `EcoID`/`BatchID`):
#' ecoregion membership is study-area-specific and is applied later, at assemble time.
#'
#' @param locations data.frame with `ID`, `Latitude`, `Longitude`, `Elevation` (one row per cell).
#' @param year integer year to fetch.
#' @param rcp,clim_model BioSIM scenario.
#' @returns data.frame: `CellID`, `YEAR`, `MONTH`, the BioSIM monthly climate columns, `WndS`, `WndD`.
#' @keywords internal
.fetch_clim_monthly_batch <- function(locations, year, rcp, clim_model) {
  raw <- .biosim_generate_weather(
    modelNames = c("Climatic_Monthly", "ClimaticWind_Monthly"),
    fromYr = year,
    toYr = year,
    id = locations$ID,
    latDeg = locations$Latitude,
    longDeg = locations$Longitude,
    elevM = locations$Elevation,
    rcp = rcp,
    climModel = clim_model,
    additionalParms = NULL
  )

  cm <- raw$Climatic_Monthly |>
    dplyr::mutate(dplyr::across(dplyr::where(is.numeric), ~ replace(., . == -9999, NA))) |>
    dplyr::rename(CellID = KeyID, YEAR = Year, MONTH = Month)

  wind_df <- raw$ClimaticWind_Monthly
  wind_df[wind_df == -9999] <- NA
  deg_cols <- paste0("W", seq(0, 350, by = 10))
  wind_summary <- data.frame(
    CellID = wind_df$KeyID,
    YEAR = wind_df$Year,
    MONTH = wind_df$Month,
    WndS = wind_df$WindSpeed,
    WndD = .wind_dir_circular_mean(as.matrix(wind_df[, deg_cols, drop = FALSE]))
  )

  cm <- dplyr::left_join(cm, wind_summary, by = c("CellID", "YEAR", "MONTH"))

  stopifnot(
    all(cm$TotalPrcp >= 0, na.rm = TRUE),
    all(cm$MeanRelH >= 0, na.rm = TRUE),
    all(cm$MeanRelH <= 100, na.rm = TRUE),
    all(cm$TotalRadiation >= 0, na.rm = TRUE),
    all(cm$WndS >= 0, na.rm = TRUE),
    all(cm$WndD >= 0, na.rm = TRUE),
    all(cm$WndD <= 360, na.rm = TRUE)
  )

  cm
}

#' Fetch + cache one year of monthly BioSIM weather for one location batch (per-study-area store)
#'
#' Wraps [.fetch_clim_monthly_batch()] and writes the result to an Arrow CSV dataset under
#' `path/Climatic_Monthly/<studyArea_hash>/<rcp>_<clim_model>/`, partitioned by `YEAR`/`BatchID`; existing
#' partitions are detected via `file.exists()` and skipped, so re-runs are cheap. For the SHARED global-id
#' store (cross-study-area reuse) use [prep_monthly_weather_biosim()]'s `ref_grid` path instead.
#'
#' @param locations_batch list whose first element is a data frame with `ID`, `Latitude`, `Longitude`,
#'   `Elevation`, `EcoID`, `BatchID` (one element of [create_locations_df()]'s output, wrapped in a list).
#' @param year integer single calendar year to fetch.
#' @param studyArea_hash character. Short study-area hash used as a cache subdirectory.
#' @param path character. Directory for the `Climatic_Monthly/` dataset (default: package climate cache).
#' @param rcp,clim_model BioSIM scenario (see [get_clim_daily()] for the choice sets).
#' @returns Path (character) to the partition CSV for `(year, BatchID)`.
#' @seealso [create_locations_df()], [assemble_climate_library_file_monthly()]
#' @export
get_clim_monthly <- function(
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
  dataset <- file.path(path, "Climatic_Monthly", studyArea_hash, scenario_tag) |> fs::dir_create()
  out_file <- file.path(dataset, paste0("YEAR=", year), paste0("BatchID=", batch_id), "part-0.csv")

  if (!file.exists(out_file)) {
    Climatic_Monthly_year <- .fetch_clim_monthly_batch(locations, year, rcp, clim_model) |>
      dplyr::mutate(ID = paste0(YEAR, "_", MONTH), .before = "YEAR") |>
      dplyr::mutate(BatchID = batch_id)

    eco_lookup <- data.frame(CellID = locations$ID, EcoID = locations$EcoID)
    Climatic_Monthly_year <- dplyr::left_join(Climatic_Monthly_year, eco_lookup, by = "CellID")

    Climatic_Monthly_year <- dplyr::group_by(Climatic_Monthly_year, YEAR, BatchID)

    arrow::write_dataset(Climatic_Monthly_year, path = dataset, format = "csv")
  }

  out_file
}

#' Stable id for a fixed climate reference grid (namespaces the global BioSIM cache).
#'
#' Hashes the grid's geometry (extent, resolution, CRS code, cell count) so the same reference grid
#' always maps to the same cache namespace -- the basis for cross-study-area reuse + accumulation.
#' @keywords internal
.ref_grid_hash <- function(ref_grid) {
  key <- paste(
    paste(as.vector(terra::ext(ref_grid)), collapse = ","),
    paste(terra::res(ref_grid), collapse = "x"),
    as.character(terra::crs(ref_grid, describe = TRUE)$code),
    terra::ncell(ref_grid),
    sep = "_"
  )
  paste0("refgrid-", substr(digest::digest(key, algo = "xxhash64"), 1, 16))
}

#' CellIDs already present in the global monthly store for one year (integer(0) if none).
#' @keywords internal
.cached_cellids_global <- function(dataset_dir, year) {
  ## read the YEAR=<year> hive partition directly (avoids pushing a partition-column filter to Arrow)
  year_dir <- file.path(dataset_dir, paste0("YEAR=", year))
  if (!dir.exists(year_dir) || length(list.files(year_dir, recursive = TRUE)) == 0L) {
    return(integer(0))
  }
  arrow::open_dataset(year_dir, format = "csv") |>
    dplyr::distinct(CellID) |>
    dplyr::collect() |>
    dplyr::pull(CellID)
}

#' Append newly-fetched climate-only rows to the global monthly store (partitioned by YEAR).
#'
#' Uses a content-derived basename so concurrent/serial appends never clobber existing partition files;
#' callers only ever fetch MISSING cells, so no de-duplication is required on read.
#' @keywords internal
.append_clim_monthly_global <- function(df, dataset_dir, year) {
  fs::dir_create(dataset_dir)
  df$YEAR <- year
  tag <- substr(digest::digest(sort(unique(df$CellID)), algo = "xxhash64"), 1, 12)
  arrow::write_dataset(
    dplyr::group_by(df, YEAR),
    path = dataset_dir,
    format = "csv",
    basename_template = paste0("part-", tag, "-{i}.csv")
  )
  invisible(dataset_dir)
}

#' Assemble LANDIS-II Climate Library wide-format table from a monthly BioSIM Arrow dataset
#'
#' Reads the partitioned Arrow CSV dataset written by [get_clim_monthly()],
#' summarizes the requested variables by `(Year, Month, EcoID)`, applies unit
#' conversions to match LANDIS-II conventions, and pivots to the wide format
#' ingested by `LandisClimateConfig`. The result has the same shape that
#' [prep_monthly_weather()] returns and is suitable for [writeClimateData()].
#'
#' Unit conversions applied:
#' \itemize{
#'   \item `TotalPrcp` (mm) \eqn{\to} cm (\eqn{\div 10})
#'   \item `WndS` (km/h) \eqn{\to} m/s (\eqn{\div 3.6})
#' }
#'
#' BioSIM `WndD` is reported as the wind \emph{from-direction} (degrees), which
#' is what the LANDIS-II Climate Library expects. The monthly value here is a
#' frequency-weighted circular mean across the model's 36 direction bins (see
#' [get_clim_monthly()]); when summarising across cells within an ecoregion
#' the same arithmetic-mean convention as the daily pipeline is applied.
#'
#' @param dataset_path character. Path to the `Climatic_Monthly` Arrow dataset
#'   directory (containing `YEAR=...`/`BatchID=...`/`part-0.csv` partitions).
#'
#' @param vars character vector of BioSIM column names to retain (e.g.
#'   `c("TotalPrcp", "MeanTmin", "MeanTmax", "WndS", "WndD")`).
#'
#' @param id_col character. Name of the ecoregion-id column. Default `"EcoID"`.
#'
#' @param cell_eco optional data.frame mapping `CellID` -> ecoregion id (`id_col`). Supply for the GLOBAL
#'   per-cell store (which is climate-only, no `EcoID`): the ecoregion grouping is study-area-specific and
#'   is applied here by joining this map. When `NULL`, the dataset is assumed to already carry `id_col`.
#' @param cell_ids optional integer vector of `CellID`s to retain (the calling study area's cells); used
#'   to filter a shared global store down to one study area before summarising. `NULL` keeps all cells.
#'
#' @returns `tbl_df` with columns `Year`, `Month`, `Variable`, and one
#'   column per ecoregion id.
#'
#' @seealso [get_clim_monthly()], [prep_monthly_weather_biosim()],
#'   [writeClimateData()]
#'
#' @export
assemble_climate_library_file_monthly <- function(
  dataset_path,
  vars,
  id_col = "EcoID",
  cell_eco = NULL,
  cell_ids = NULL
) {
  stopifnot(
    requireNamespace("arrow", quietly = TRUE),
    is.character(vars),
    length(vars) >= 1L,
    is.character(id_col),
    length(id_col) == 1L
  )

  biosim_to_landis <- c(
    TotalPrcp = "precip",
    MeanTmin = "Tmin",
    MeanTmax = "Tmax",
    MeanTair = "temp",
    MeanRelH = "RH",
    TotalRadiation = "SWR",
    WndS = "windSpeed",
    WndD = "windDirection"
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

  ds <- arrow::open_dataset(dataset_path, format = "csv")
  if (!is.null(cell_ids)) {
    ds <- dplyr::filter(ds, CellID %in% cell_ids)
  }
  df <- dplyr::collect(ds)
  if (!is.null(cell_eco)) {
    ## global store is climate-only: attach this study area's ecoregion membership before summarising
    df <- dplyr::left_join(df, cell_eco, by = "CellID")
  }

  keep_cols <- c("YEAR", "MONTH", id_col, vars)
  missing_cols <- setdiff(keep_cols, names(df))
  if (length(missing_cols)) {
    stop("dataset is missing expected column(s): ", glue::glue_collapse(missing_cols, sep = ", "))
  }
  df <- df[, keep_cols, drop = FALSE]

  if ("TotalPrcp" %in% vars) {
    df$TotalPrcp <- df$TotalPrcp / 10 ## mm -> cm
  }
  if ("WndS" %in% vars) {
    df$WndS <- df$WndS / 3.6 ## km/h -> m/s
  }

  long <- df |>
    tidyr::pivot_longer(cols = dplyr::all_of(vars), names_to = "Variable", values_to = "Value") |>
    dplyr::group_by(YEAR, MONTH, .data[[id_col]], Variable) |>
    dplyr::summarise(Value = mean(Value, na.rm = TRUE), .groups = "drop") |>
    dplyr::mutate(Variable = unname(biosim_to_landis[Variable]))

  wide <- long |>
    tidyr::pivot_wider(names_from = dplyr::all_of(id_col), values_from = "Value") |>
    dplyr::rename(Year = YEAR, Month = MONTH) |>
    dplyr::arrange(Year, Month, Variable)

  tibble::as_tibble(wide)
}

#' @param ref_grid optional fixed reference `SpatRaster` for the GLOBAL per-cell cache. Its cell numbers
#'   are taken as STABLE GLOBAL ids and its cell VALUES are read as the BioSIM elevation (`elevM`) -- so it
#'   MUST carry elevation values (e.g. a DEM resampled onto the grid). When supplied, the monthly pull is
#'   cached by these global ids in one shared, accumulating store, so overlapping / nested study areas
#'   reuse cells already fetched. When `NULL` (default), the per-study-area elevatr grid (`z`) is used.
#' @export
#' @rdname prep_climate_data
prep_monthly_weather_biosim <- function(
  vars = NULL,
  years = NULL,
  studyArea = NULL,
  id = NULL,
  batch_size = 1000,
  z = 9,
  ref_grid = NULL,
  rcp = "RCP45",
  clim_model = "RCM4"
) {
  stopifnot(
    requireNamespace("BioSIM", quietly = TRUE),
    requireNamespace("arrow", quietly = TRUE),
    requireNamespace("purrr", quietly = TRUE),
    requireNamespace("sf", quietly = TRUE)
  )
  if (is.null(ref_grid)) {
    stopifnot(requireNamespace("elevatr", quietly = TRUE))
  }

  stopifnot(!is.null(vars), !is.null(studyArea), !is.null(id), !is.null(years))
  rcp <- match.arg(rcp, .biosim_rcp_choices)
  clim_model <- match.arg(clim_model, .biosim_clim_model_choices)

  unknown <- setdiff(vars, names(.monthly_var_to_biosim))
  if (length(unknown)) {
    stop(
      "unknown monthly climate variable(s): ",
      glue::glue_collapse(unknown, sep = ", "),
      ". Known: ",
      glue::glue_collapse(names(.monthly_var_to_biosim), sep = ", ")
    )
  }
  biosim_vars <- unname(.monthly_var_to_biosim[vars])

  cache_root <- .climateCachePath()
  scenario_tag <- .biosim_scenario_tag(rcp, clim_model)

  ## Grid + cache namespace. With a fixed reference grid (ref_grid) the cell numbers are GLOBAL ids and
  ## the cache is shared across study areas; without one we fall back to the per-study-area elevatr grid.
  if (is.null(ref_grid)) {
    grid_rast <- get_elevation_rast(studyArea, z = z)
    cache_ns <- .studyArea_hash(studyArea)
  } else {
    grid_rast <- ref_grid
    cache_ns <- .ref_grid_hash(ref_grid)
  }
  dataset_dir <- file.path(cache_root, "Climatic_Monthly", cache_ns, scenario_tag)

  locations_batches <- create_locations_df(
    elev = grid_rast,
    studyArea = studyArea,
    id = id,
    batch_size = batch_size
  )

  ## purrr (NOT furrr): pulls run SEQUENTIALLY within this call. The caller (targets/crew) owns
  ## cross-branch parallelism + caching; an internal furrr fan-out under a parallel ambient future plan
  ## spawns many simultaneous BioSIM requests and overwhelms the shared web service into a J4R socket hang.
  if (is.null(ref_grid)) {
    ## --- legacy per-study-area path: per-(YEAR, BatchID) cache files, no cross-study-area reuse ---
    grid <- expand.grid(
      batch_idx = seq_along(locations_batches),
      year = years,
      KEEP.OUT.ATTRS = FALSE,
      stringsAsFactors = FALSE
    )
    purrr::map2(.x = grid$batch_idx, .y = grid$year, .f = function(b, y) {
      get_clim_monthly(
        locations_batches[b],
        year = y,
        studyArea_hash = cache_ns,
        path = cache_root,
        rcp = rcp,
        clim_model = clim_model
      )
    })
    return(
      assemble_climate_library_file_monthly(dataset_dir, vars = biosim_vars, id_col = "EcoID") |>
        dplyr::filter(Year %in% years)
    )
  }

  ## --- global per-cell path: store keyed by GLOBAL CellID, climate-only, fetch only MISSING cells ---
  ## so subsequent / overlapping study-area runs add to and draw from one shared, accumulating store.
  locations_all <- do.call(rbind, locations_batches)
  purrr::walk(years, function(y) {
    cached <- .cached_cellids_global(dataset_dir, y)
    missing_ids <- setdiff(locations_all$ID, cached)
    if (length(missing_ids) == 0L) {
      return(invisible(NULL))
    }
    loc_missing <- locations_all[locations_all$ID %in% missing_ids, , drop = FALSE]
    fetch_batches <- split(loc_missing, seq_len(nrow(loc_missing)) %/% batch_size)
    fetched <- purrr::map(
      fetch_batches,
      ~ .fetch_clim_monthly_batch(.x, year = y, rcp = rcp, clim_model = clim_model)
    )
    .append_clim_monthly_global(do.call(rbind, fetched), dataset_dir, y)
  })

  ## ecoregion membership is applied here (study-area-specific), filtering the global store to this
  ## study area's cells and joining its CellID -> EcoID map.
  cell_eco <- unique(data.frame(CellID = locations_all$ID, EcoID = locations_all$EcoID))
  assemble_climate_library_file_monthly(
    dataset_dir,
    vars = biosim_vars,
    id_col = "EcoID",
    cell_eco = cell_eco,
    cell_ids = locations_all$ID
  ) |>
    dplyr::filter(Year %in% years)
}
