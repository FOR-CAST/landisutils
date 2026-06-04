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

#' Fetch a single year of monthly BioSIM weather for one location batch
#'
#' Wraps [BioSIM::generateWeather()] for the `Climatic_Monthly` and
#' `ClimaticWind_Monthly` models, fetching one calendar year for the cells in
#' `locations_batch[[1]]`. Wind direction is summarised from the model's 36
#' directional-frequency columns (`W0`, `W10`, ..., `W350`) into a single
#' weighted circular mean (`WndD`, degrees from-direction). Output is appended
#' to an Arrow CSV dataset under
#' `path/Climatic_Monthly/<studyArea_hash>/<rcp>_<clim_model>/`, partitioned by
#' `YEAR`/`BatchID`. The function is intended to be run as a dynamic target
#' across many `(batch, year)` combinations; existing partition files are
#' detected via `file.exists()` and skipped, so re-runs are cheap.
#'
#' For years within BioSIM's observational range, `rcp` and `clim_model` have
#' no effect on the returned data, but they do still namespace the cache.
#' For future years they select the CMIP5-era projection (see
#' [get_clim_daily()] for the choice sets).
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
#' @param path character. Directory under which the `Climatic_Monthly/` arrow
#'   dataset is written. Default uses the package climate cache
#'   (`getOption("landisutils.cache.path")`).
#'
#' @param rcp character. BioSIM representative concentration pathway. One of
#'   `"CONSTANT_CLIMATE"`, `"RCP45"`, `"RCP85"`. Default `"RCP45"`.
#'
#' @param clim_model character. BioSIM climate model. One of `"RCM4"`,
#'   `"GCM4"`, `"Hadley"`. Default `"RCM4"`.
#'
#' @returns Path (character) to the partition CSV for `(year, BatchID)`.
#'
#' @seealso [get_elevation_rast()], [create_locations_df()],
#'   [assemble_climate_library_file_monthly()]
#'
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

    Climatic_Monthly_year <- raw$Climatic_Monthly |>
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

    Climatic_Monthly_year <- Climatic_Monthly_year |>
      dplyr::left_join(wind_summary, by = c("CellID", "YEAR", "MONTH")) |>
      dplyr::mutate(ID = paste0(YEAR, "_", MONTH), .before = "YEAR") |>
      dplyr::mutate(BatchID = batch_id)

    eco_lookup <- data.frame(CellID = locations$ID, EcoID = locations$EcoID)
    Climatic_Monthly_year <- dplyr::left_join(Climatic_Monthly_year, eco_lookup, by = "CellID")

    stopifnot(
      all(Climatic_Monthly_year$TotalPrcp >= 0, na.rm = TRUE),
      all(Climatic_Monthly_year$MeanRelH >= 0, na.rm = TRUE),
      all(Climatic_Monthly_year$MeanRelH <= 100, na.rm = TRUE),
      all(Climatic_Monthly_year$TotalRadiation >= 0, na.rm = TRUE),
      all(Climatic_Monthly_year$WndS >= 0, na.rm = TRUE),
      all(Climatic_Monthly_year$WndD >= 0, na.rm = TRUE),
      all(Climatic_Monthly_year$WndD <= 360, na.rm = TRUE)
    )

    Climatic_Monthly_year <- dplyr::group_by(Climatic_Monthly_year, YEAR, BatchID)

    arrow::write_dataset(Climatic_Monthly_year, path = dataset, format = "csv")
  }

  out_file
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
#' @param id_col character. Name of the ecoregion-id column in the dataset.
#'   Default `"EcoID"`.
#'
#' @returns `tbl_df` with columns `Year`, `Month`, `Variable`, and one
#'   column per ecoregion id.
#'
#' @seealso [get_clim_monthly()], [prep_monthly_weather_biosim()],
#'   [writeClimateData()]
#'
#' @export
assemble_climate_library_file_monthly <- function(dataset_path, vars, id_col = "EcoID") {
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

  df <- arrow::open_dataset(dataset_path, format = "csv") |> dplyr::collect()

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

#' @export
#' @rdname prep_climate_data
prep_monthly_weather_biosim <- function(
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
    get_clim_monthly(
      locations_batches[b],
      year = y,
      studyArea_hash = sa_hash,
      path = cache_root,
      rcp = rcp,
      clim_model = clim_model
    )
  })

  assemble_climate_library_file_monthly(
    dataset_path = file.path(cache_root, "Climatic_Monthly", sa_hash, scenario_tag),
    vars = biosim_vars,
    id_col = "EcoID"
  ) |>
    dplyr::filter(Year %in% years)
}
