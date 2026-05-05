#' Fetch a single year-variable of monthly TerraClim weather
#'
#' Wraps [climateR::getTerraClim()] for one (variable, year), summarises by
#' ecoregion via [zonal::execute_zonal()], and writes the long-format result
#' as an Arrow CSV partition under `path/TerraClim_Monthly/`, partitioned by
#' `Variable`/`YEAR`. Existing partitions are skipped via `file.exists()`, so
#' re-runs are cheap. Unit conversions (e.g. mm -> cm for precipitation) and
#' translation to LANDIS-II Climate Library variable names are deferred to
#' [assemble_climate_library_file_monthly_terraclim()].
#'
#' @template section_climate_topography
#'
#' @param var character single TerraClim variable name (lowercase, e.g.
#'   `"ppt"`, `"tmax"`, `"tmin"`, `"aet"`).
#'
#' @param year integer single calendar year to fetch.
#'
#' @param studyArea `sf` polygons object delineating ecoregions.
#'
#' @param id character. Name of the polygon-id column in `studyArea`.
#'
#' @param studyArea_hash character or `NULL`. Short hash of the study-area
#'   object used as a cache subdirectory so that distinct study areas don't
#'   collide. Defaults to `.studyArea_hash()` of `studyArea`.
#'
#' @param path character. Directory under which the `TerraClim_Monthly/` arrow
#'   dataset is written. Default uses the package climate cache
#'   (`getOption("landisutils.cache.path")`).
#'
#' @returns Path (character) to the partition CSV for `(var, year)`.
#'
#' @seealso [assemble_climate_library_file_monthly_terraclim()],
#'   [prep_monthly_weather()]
#'
#' @export
get_clim_monthly_terraclim <- function(
  var,
  year,
  studyArea,
  id,
  studyArea_hash = .studyArea_hash(studyArea),
  path = .climateCachePath()
) {
  stopifnot(
    requireNamespace("arrow", quietly = TRUE),
    requireNamespace("climateR", quietly = TRUE),
    requireNamespace("zonal", quietly = TRUE),
    is.character(studyArea_hash),
    length(studyArea_hash) == 1L
  )

  dataset <- file.path(path, "TerraClim_Monthly", studyArea_hash) |> fs::dir_create()
  out_file <- file.path(dataset, paste0("Variable=", var), paste0("YEAR=", year), "part-0.csv")

  if (!file.exists(out_file)) {
    long <- climateR::getTerraClim(
      AOI = studyArea,
      varname = var,
      startDate = glue::glue("{year}-01-01"),
      endDate = glue::glue("{year}-12-31"),
      verbose = FALSE
    ) |>
      zonal::execute_zonal(geom = studyArea, ID = id, join = FALSE) |>
      tidyr::pivot_longer(
        cols = starts_with("mean"),
        names_to = c(NA, "Year", "Month", "Day", NA),
        names_prefix = "mean.",
        names_sep = "(-|_)",
        values_to = "Value"
      ) |>
      dplyr::rename(EcoID = dplyr::all_of(id)) |>
      dplyr::mutate(
        Year = as.integer(Year),
        Month = as.integer(Month),
        Day = NULL,
        Variable = var
      ) |>
      dplyr::select(EcoID, Year, Month, Variable, Value) |>
      dplyr::group_by(Variable, Year)

    arrow::write_dataset(long, path = dataset, format = "csv")
  }

  out_file
}

#' Assemble LANDIS-II Climate Library wide-format table from a TerraClim Arrow dataset
#'
#' Reads the partitioned Arrow CSV dataset written by
#' [get_clim_monthly_terraclim()], applies unit conversions, translates raw
#' TerraClim variable names to LANDIS-II Climate Library names via
#' `var_landis()`, and pivots to wide format keyed by ecoregion id. The result
#' is the same shape that [prep_monthly_weather()] returns and is suitable for
#' [writeClimateData()].
#'
#' Unit conversions applied:
#' \itemize{
#'   \item `ppt` (mm) \eqn{\to} cm (\eqn{\div 10})
#' }
#'
#' @param dataset_path character. Path to the `TerraClim_Monthly` Arrow dataset
#'   directory (containing `Variable=...`/`YEAR=...`/`part-0.csv` partitions).
#'
#' @param vars character vector of TerraClim variable names to retain (lowercase,
#'   e.g. `c("ppt", "tmax", "tmin")`).
#'
#' @param id_col character. Name of the ecoregion-id column in the dataset.
#'   Default `"EcoID"`.
#'
#' @returns `tbl_df` with columns `Year`, `Month`, `Variable`, and one
#'   column per ecoregion id.
#'
#' @template section_climate_topography
#'
#' @seealso [get_clim_monthly_terraclim()], [prep_monthly_weather()],
#'   [writeClimateData()]
#'
#' @export
assemble_climate_library_file_monthly_terraclim <- function(dataset_path, vars, id_col = "EcoID") {
  stopifnot(
    requireNamespace("arrow", quietly = TRUE),
    is.character(vars),
    length(vars) >= 1L,
    is.character(id_col),
    length(id_col) == 1L
  )

  df <- arrow::open_dataset(dataset_path, format = "csv") |>
    dplyr::filter(Variable %in% vars) |>
    dplyr::collect()

  if (nrow(df) == 0L) {
    stop(
      "no TerraClim partitions found in '",
      dataset_path,
      "' for vars: ",
      glue::glue_collapse(vars, sep = ", ")
    )
  }

  df <- dplyr::mutate(
    df,
    Value = dplyr::if_else(Variable == "ppt", Value / 10, Value),
    Variable = vapply(Variable, var_landis, character(1))
  )

  df |>
    tidyr::pivot_wider(names_from = dplyr::all_of(id_col), values_from = "Value") |>
    dplyr::arrange(Year, Month, Variable) |>
    tibble::as_tibble()
}

#' @export
#' @rdname prep_climate_data
prep_monthly_weather <- function(vars = NULL, years = NULL, studyArea = NULL, id = NULL) {
  stopifnot(
    requireNamespace("arrow", quietly = TRUE),
    requireNamespace("climateR", quietly = TRUE),
    requireNamespace("furrr", quietly = TRUE),
    requireNamespace("purrr", quietly = TRUE),
    requireNamespace("zonal", quietly = TRUE)
  )

  stopifnot(!is.null(vars), !is.null(studyArea), !is.null(id), !is.null(years))

  cache_root <- .climateCachePath()
  sa_hash <- .studyArea_hash(studyArea)
  grid <- expand.grid(var = vars, year = years, KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE)

  furrr::future_map2(.x = grid$var, .y = grid$year, .f = function(v, y) {
    get_clim_monthly_terraclim(
      var = v,
      year = y,
      studyArea = studyArea,
      id = id,
      studyArea_hash = sa_hash,
      path = cache_root
    )
  })

  assemble_climate_library_file_monthly_terraclim(
    dataset_path = file.path(cache_root, "TerraClim_Monthly", sa_hash),
    vars = vars,
    id_col = "EcoID"
  ) |>
    dplyr::filter(Year %in% years)
}
