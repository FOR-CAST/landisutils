## map lowercase climate-variable names (used by the public
## `prep_monthly_weather_climr()` API) to climr's ClimateNA-style monthly
## variable bases. [.climr_monthly_codes()] expands each base into its 12
## per-month codes (e.g. `prcp` -> `PPT_01`..`PPT_12`).
.monthly_var_to_climr_base <- c(
  prcp = "PPT",
  tmax = "Tmax",
  tmin = "Tmin",
  temp = "Tave",
  rh = "RH",
  srad = "Rad",
  cmd = "CMD"
)

## map climr base variable names to LANDIS-II Climate Library variable names.
.climr_base_to_landis <- c(
  PPT = "precip",
  Tmax = "Tmax",
  Tmin = "Tmin",
  Tave = "temp",
  RH = "RH",
  Rad = "SWR",
  CMD = "CMD"
)

## climr does not expose wind variables; reject these with a clear error so
## users are pointed at `prep_monthly_weather_biosim()` instead.
.climr_unsupported <- c("ws", "wnddir")

#' bcgov-recommended eight-member climr GCM ensemble
#'
#' A character vector naming the eight GCMs that the
#' [bcgov ensemble-selection guidance](https://bcgov.github.io/climr/articles/guidance_ensembleSelection.html)
#' identifies as a defensible subset of the full 13-model ensemble returned by
#' [climr::list_gcms()]. The subset is more consistent with the IPCC
#' assessment of climate sensitivity than the full ensemble.
#'
#' @returns Character vector of length 8.
#'
#' @seealso [prep_monthly_weather_climr()], [climr::list_gcms()]
#'
#' @examples
#' climr_ensemble_8
#' @export
climr_ensemble_8 <- c(
  "ACCESS-ESM1-5",
  "CNRM-ESM2-1",
  "EC-Earth3",
  "GFDL-ESM4",
  "GISS-E2-1-G",
  "MIROC6",
  "MPI-ESM1-2-HR",
  "MRI-ESM2-0"
)

#' @keywords internal
.climr_scenario_tag <- function(gcms, ssps, max_run, obs_ts_dataset) {
  if (is.null(gcms)) {
    paste0("obs_", obs_ts_dataset)
  } else {
    paste0("gcm_", paste(ssps, collapse = "-"), "_run", max_run)
  }
}

#' @keywords internal
.climr_monthly_codes <- function(vars) {
  bases <- unname(.monthly_var_to_climr_base[vars])
  months <- sprintf("%02d", 1:12)
  as.vector(outer(bases, months, paste, sep = "_"))
}

#' @keywords internal
.climr_build_xyz <- function(studyArea, id, batch_size, z) {
  elev <- get_elevation_rast(studyArea, z = z)
  batches <- create_locations_df(
    elev = elev,
    studyArea = studyArea,
    id = id,
    batch_size = batch_size
  )
  flat <- do.call(rbind, batches)
  xyz <- data.frame(
    lon = flat$Longitude,
    lat = flat$Latitude,
    elev = flat$Elevation,
    id = flat$ID
  )
  eco_lookup <- data.frame(id = flat$ID, EcoID = flat$EcoID)
  list(xyz = xyz, eco_lookup = eco_lookup)
}

#' Fetch one year of monthly climr weather and write Arrow CSV partitions
#'
#' Wraps [climr::downscale()] for one calendar `year` against the points in
#' `xyz`, then splits the long-format output by base variable and writes Arrow
#' CSV partitions under
#' `path/Climr_Monthly/<studyArea_hash>/<scenario_tag>/`. Partitions are keyed
#' by `Variable`/`Year` in observational mode and additionally by `GCM`/`SSP`
#' in projection mode. Existing per-variable / per-year partition trees are
#' detected via [dir.exists()] and skipped, so re-runs are cheap.
#'
#' The function runs in two modes:
#' \describe{
#'   \item{Observational (default)}{`gcms = NULL`. Uses `obs_years = year` and
#'     `obs_ts_dataset` for ClimateNA-style historical anomalies.}
#'   \item{Projection}{`gcms` set (e.g. [`climr_ensemble_8`]) and `ssps` set
#'     (e.g. `"ssp245"`). Uses `gcm_ssp_years = year` to fetch CMIP6 GCM
#'     time-series projections; `max_run` controls how many runs per GCM are
#'     averaged (with `ensemble_mean = TRUE`, climr's default).}
#' }
#'
#' Per the climr API, the climr cache itself is configured by setting
#' `options("climr.cache.path" = ...)` (climr GitHub issue 274). The orchestrator
#' [prep_monthly_weather_climr()] sets a sensible default via
#' [withr::local_options()] for the duration of a call; if you call this
#' helper directly you should do the same.
#'
#' @template section_climate_topography
#'
#' @param year integer single calendar year to fetch. In projection mode, must
#'   lie within [climr::list_gcm_ssp_years()].
#'
#' @param xyz `data.frame` with columns `lon`, `lat`, `elev`, `id` (one row per
#'   cell). See `.climr_build_xyz()` (internal) or build manually.
#'
#' @param vars character vector of lowercase variable names (e.g.
#'   `c("prcp", "tmax", "tmin")`); see [prep_monthly_weather_climr()] for the
#'   supported set.
#'
#' @param obs_ts_dataset character. climr observational time-series dataset.
#'   One of `"climatena"` or `"cru.gpcc"`. Default `"climatena"`. Ignored when
#'   `gcms` is set.
#'
#' @param studyArea_hash character. Short hash of the study-area object (see
#'   `.studyArea_hash()`) used as a cache subdirectory so that distinct study
#'   areas don't collide.
#'
#' @param path character. Directory under which the `Climr_Monthly/` arrow
#'   dataset is written. Default uses the package climate cache
#'   (`getOption("landisutils.cache.path")`).
#'
#' @param gcms character vector of CMIP6 GCM names (subset of
#'   [climr::list_gcms()]). When `NULL` (default), runs in observational mode.
#'   When set, runs in projection mode and `ssps` is required.
#'
#' @param ssps character vector of CMIP6 SSP names (subset of
#'   [climr::list_ssps()], e.g. `"ssp245"`). Required when `gcms` is set.
#'
#' @param max_run integer number of ensemble runs per GCM to fetch. With
#'   `ensemble_mean = TRUE` (climr's default and what we use), `max_run = 0L`
#'   returns only the per-GCM ensemble mean. Default `0L`.
#'
#' @returns Path (character) to the scenario-tagged dataset directory (i.e.
#'   the directory containing the `Variable=...` partitions).
#'
#' @seealso [assemble_climate_library_file_monthly_climr()],
#'   [prep_monthly_weather_climr()], [`climr_ensemble_8`]
#'
#' @export
get_clim_monthly_climr <- function(
  year,
  xyz,
  vars,
  studyArea_hash,
  obs_ts_dataset = "climatena",
  path = .climateCachePath(),
  gcms = NULL,
  ssps = NULL,
  max_run = 0L
) {
  stopifnot(
    requireNamespace("arrow", quietly = TRUE),
    requireNamespace("climr", quietly = TRUE),
    is.character(studyArea_hash),
    length(studyArea_hash) == 1L
  )
  if (!is.null(gcms) && is.null(ssps)) {
    stop("`ssps` must be supplied when `gcms` is set (projection mode).")
  }

  bases <- unname(.monthly_var_to_climr_base[vars])
  scenario_tag <- .climr_scenario_tag(gcms, ssps, max_run, obs_ts_dataset)
  dataset <- file.path(path, "Climr_Monthly", studyArea_hash, scenario_tag) |> fs::dir_create()

  needed_bases <- bases[
    !vapply(
      bases,
      function(b) {
        d <- file.path(dataset, paste0("Variable=", b), paste0("Year=", year))
        dir.exists(d) && length(list.files(d, recursive = TRUE)) > 0L
      },
      logical(1)
    )
  ]

  if (length(needed_bases) > 0L) {
    needed_codes <- as.vector(outer(needed_bases, sprintf("%02d", 1:12), paste, sep = "_"))

    if (is.null(gcms)) {
      raw <- climr::downscale(
        xyz = xyz,
        obs_years = year,
        obs_ts_dataset = obs_ts_dataset,
        vars = needed_codes,
        cache = TRUE
      )
    } else {
      raw <- climr::downscale(
        xyz = xyz,
        gcms = gcms,
        ssps = ssps,
        gcm_ssp_years = year,
        vars = needed_codes,
        max_run = max_run,
        ensemble_mean = TRUE,
        return_refperiod = FALSE,
        cache = TRUE
      )
    }

    df <- as.data.frame(raw)
    missing_cols <- setdiff(needed_codes, names(df))
    if (length(missing_cols)) {
      stop(
        "climr output is missing expected variable column(s): ",
        glue::glue_collapse(missing_cols, sep = ", ")
      )
    }

    if (is.null(gcms)) {
      keep_extra <- character()
      group_extra <- character()
    } else {
      ## climr projection output adds context columns: GCM, SSP, RUN, PERIOD.
      ## Keep GCM and SSP for downstream ensemble-mean assembly; PERIOD is
      ## redundant with `year` and RUN is collapsed by ensemble_mean = TRUE.
      keep_extra <- intersect(c("GCM", "SSP"), names(df))
      group_extra <- keep_extra
    }

    long <- df[, c("id", keep_extra, needed_codes), drop = FALSE] |>
      tidyr::pivot_longer(
        cols = dplyr::all_of(needed_codes),
        names_to = c("Variable", "Month"),
        names_sep = "_",
        values_to = "Value"
      ) |>
      dplyr::mutate(
        Month = as.integer(Month),
        Year = as.integer(year)
      ) |>
      dplyr::select(dplyr::all_of(c("id", keep_extra, "Year", "Month", "Variable", "Value"))) |>
      dplyr::group_by(dplyr::across(dplyr::all_of(c("Variable", "Year", group_extra))))

    arrow::write_dataset(long, path = dataset, format = "csv")
  }

  dataset
}

#' Assemble LANDIS-II Climate Library wide-format table from a climr Arrow dataset
#'
#' Reads the partitioned Arrow CSV dataset written by
#' [get_clim_monthly_climr()], joins per-cell rows to ecoregion ids via
#' `eco_lookup`, summarises by `(Year, Month, EcoID, Variable)` with
#' `mean(..., na.rm = TRUE)`, applies unit conversions to match LANDIS-II
#' conventions, and pivots to wide format keyed by ecoregion id.
#'
#' For projection-mode partitions (which carry extra `GCM` and `SSP` columns),
#' the same group-and-mean step collapses across GCMs/SSPs/cells, yielding a
#' single multi-GCM ensemble-mean value per `(Year, Month, EcoID, Variable)`
#' as expected by the LANDIS-II Climate Library.
#'
#' Unit conversions applied:
#' \itemize{
#'   \item `PPT` (mm) \eqn{\to} cm (\eqn{\div 10})
#' }
#'
#' @param dataset_path character. Path to a scenario-tagged `Climr_Monthly`
#'   Arrow dataset directory (containing `Variable=...`/`Year=...`/...
#'   partitions). Typically `<cache_root>/Climr_Monthly/<sa_hash>/<scenario_tag>`.
#'
#' @param vars character vector of lowercase variable names (e.g.
#'   `c("prcp", "tmax", "tmin")`); same set accepted by
#'   [prep_monthly_weather_climr()].
#'
#' @param eco_lookup `data.frame` with columns `id` (cell id, matching the
#'   `id` column in the cached partitions) and `EcoID` (ecoregion id from the
#'   study-area `id` field). Typically built by `.climr_build_xyz()` (internal).
#'
#' @returns `tbl_df` with columns `Year`, `Month`, `Variable`, and one column
#'   per ecoregion id.
#'
#' @seealso [get_clim_monthly_climr()], [prep_monthly_weather_climr()],
#'   [writeClimateData()]
#'
#' @export
assemble_climate_library_file_monthly_climr <- function(dataset_path, vars, eco_lookup) {
  stopifnot(
    requireNamespace("arrow", quietly = TRUE),
    is.character(vars),
    length(vars) >= 1L,
    is.data.frame(eco_lookup),
    all(c("id", "EcoID") %in% names(eco_lookup))
  )

  bases <- unname(.monthly_var_to_climr_base[vars])

  df <- arrow::open_dataset(dataset_path, format = "csv") |>
    dplyr::filter(Variable %in% bases) |>
    dplyr::collect()

  if (nrow(df) == 0L) {
    stop(
      "no climr partitions found in '",
      dataset_path,
      "' for vars: ",
      glue::glue_collapse(vars, sep = ", ")
    )
  }

  df |>
    dplyr::left_join(eco_lookup, by = "id") |>
    dplyr::group_by(Year, Month, EcoID, Variable) |>
    dplyr::summarise(Value = mean(Value, na.rm = TRUE), .groups = "drop") |>
    dplyr::mutate(
      Value = dplyr::if_else(Variable == "PPT", Value / 10, Value),
      Variable = unname(.climr_base_to_landis[Variable])
    ) |>
    tidyr::pivot_wider(names_from = "EcoID", values_from = "Value") |>
    dplyr::arrange(Year, Month, Variable) |>
    tibble::as_tibble()
}

#' @export
#' @rdname prep_climate_data
prep_monthly_weather_climr <- function(
  vars = NULL,
  years = NULL,
  studyArea = NULL,
  id = NULL,
  batch_size = 1000,
  z = 9,
  obs_ts_dataset = c("climatena", "cru.gpcc"),
  gcms = NULL,
  ssps = NULL,
  max_run = 0L
) {
  stopifnot(
    requireNamespace("arrow", quietly = TRUE),
    requireNamespace("climr", quietly = TRUE),
    requireNamespace("elevatr", quietly = TRUE),
    requireNamespace("furrr", quietly = TRUE),
    requireNamespace("purrr", quietly = TRUE),
    requireNamespace("sf", quietly = TRUE),
    requireNamespace("withr", quietly = TRUE)
  )

  stopifnot(!is.null(vars), !is.null(studyArea), !is.null(id), !is.null(years))
  obs_ts_dataset <- match.arg(obs_ts_dataset)

  if (!is.null(gcms)) {
    if (is.null(ssps)) {
      stop("`ssps` must be supplied when `gcms` is set (projection mode).")
    }
    bad_gcms <- setdiff(gcms, climr::list_gcms())
    if (length(bad_gcms)) {
      stop(
        "unknown GCM(s) for climr: ",
        glue::glue_collapse(bad_gcms, sep = ", "),
        ". See climr::list_gcms()."
      )
    }
    bad_ssps <- setdiff(ssps, climr::list_ssps())
    if (length(bad_ssps)) {
      stop(
        "unknown SSP(s) for climr: ",
        glue::glue_collapse(bad_ssps, sep = ", "),
        ". See climr::list_ssps()."
      )
    }
    bad_years <- setdiff(years, climr::list_gcm_ssp_years())
    if (length(bad_years)) {
      stop(
        "year(s) outside climr GCM SSP timeseries range: ",
        glue::glue_collapse(bad_years, sep = ", "),
        ". See climr::list_gcm_ssp_years()."
      )
    }
  }

  bad <- intersect(vars, .climr_unsupported)
  if (length(bad)) {
    stop(
      "climr does not provide wind variables: ",
      glue::glue_collapse(bad, sep = ", "),
      ". Use prep_monthly_weather_biosim() instead."
    )
  }
  unknown <- setdiff(vars, names(.monthly_var_to_climr_base))
  if (length(unknown)) {
    stop(
      "unknown climr monthly variable(s): ",
      glue::glue_collapse(unknown, sep = ", "),
      ". Known: ",
      glue::glue_collapse(names(.monthly_var_to_climr_base), sep = ", ")
    )
  }

  ## climr's cache is keyed off `getOption("climr.cache.path")` (see
  ## climr/R/cache.R). climr GitHub issue 274 proposes an argument-based override;
  ## until that lands, set the option for the duration of this call.
  withr::local_options(climr.cache.path = getOption(
    "climr.cache.path",
    file.path(.climateCachePath(), "climr")
  ))

  loc <- .climr_build_xyz(studyArea, id = id, batch_size = batch_size, z = z)

  cache_root <- .climateCachePath()
  sa_hash <- .studyArea_hash(studyArea)
  scenario_tag <- .climr_scenario_tag(gcms, ssps, max_run, obs_ts_dataset)

  furrr::future_map(years, function(yr) {
    get_clim_monthly_climr(
      year = yr,
      xyz = loc$xyz,
      vars = vars,
      studyArea_hash = sa_hash,
      obs_ts_dataset = obs_ts_dataset,
      path = cache_root,
      gcms = gcms,
      ssps = ssps,
      max_run = max_run
    )
  })

  assemble_climate_library_file_monthly_climr(
    dataset_path = file.path(cache_root, "Climr_Monthly", sa_hash, scenario_tag),
    vars = vars,
    eco_lookup = loc$eco_lookup
  ) |>
    dplyr::filter(Year %in% years)
}
