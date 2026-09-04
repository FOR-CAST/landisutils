## Forest Product Sector Module (FPSM) output ------------------------------------------------------
##
## Readers and aggregators for `FPS_raw_out.csv`, the counterpart to the ForCS
## `log_Summary` helpers in `forcs_output.R` and following the same shape: read
## one replicate, publish it atomically to a partitioned parquet, open the set
## lazily as an Arrow dataset.
##
## UNITS. FPSM converts the ForCS flux logs from g C m^-2 by multiplying by the
## square of the configured cell length and by 1e-6, then sums across cells. Its
## output is therefore an absolute landscape total in TONNES OF CARBON, even
## though the user guide labels the columns t C ha^-1 -- the two coincide only
## because a 100 m cell is exactly one hectare. Every column produced here is
## named `_tC` accordingly. FPSM reports carbon, never CO2e.

## Row semantics of `FPS_raw_out.csv`, established by reading the shipped example
## outputs rather than from the user guide, which describes types 1-3 only as
## "debugging codes that can be ignored":
##
##   Type 1  landfill pools decaying (1000/1004 -> 1009/1010, and on to gases)
##   Type 2  dump pools decaying aerobically (1001/1003 -> gas)
##   Type 3  combustion (1002/1005 -> gas), emission only
##   Type 4  END-OF-YEAR STOCK in a special pool (landfill/dump); Market is 0
##   Type 5  END-OF-YEAR STOCK in a secondary product pool, and the amount
##           retiring out of it; Market is a real market code
##
## `AmountEmitted` does NOT always mean an emission to air: on type 5 it is the
## carbon leaving a product pool for a disposal pool, and on type 1 it can be a
## transfer to the degradable sub-pool. Only rows whose destination is 2006 or
## 2007 are atmospheric.
.fps_gas_codes <- c(E_CO2 = 2006L, E_CH4 = 2007L)

## Types carrying the annual end-of-year stock reports.
.fps_stock_types <- c(product = 5L, special = 4L)

#' Read one or more FPSM `FPS_raw_out.csv` files
#'
#' Attaches `scenario` / `replicate` labels derived from the directory structure.
#' The run directory is expected at either `<scenario>/<replicate>/FPS_raw_out.csv`
#' or `<scenario>/<replicate>/fps/FPS_raw_out.csv`; a parent directory named `fps`
#' is skipped when deriving the labels, so both layouts work.
#'
#' @param paths Character vector of `FPS_raw_out.csv` paths.
#' @param run_name Optional scenario directory path; when `NULL` (default) the
#'   scenario label is derived from `paths`.
#'
#' @returns A tibble with leading `scenario` / `replicate` columns and the eight
#'   FPSM output fields, `To_Gas/Pool` renamed to `ToPool`. Amounts are tonnes of
#'   carbon (see the file header note on units).
#'
#' @family FPSM helpers
#'
#' @export
read_fps_raw_out <- function(paths, run_name = NULL) {
  if (length(paths) == 0L) {
    return(tibble::tibble())
  }
  purrr::map(paths, function(p) {
    dirs <- fs::path_split(fs::path_dir(p))[[1L]]
    ## Skip a trailing `fps/` working directory so both layouts derive alike.
    if (length(dirs) && identical(utils::tail(dirs, 1L), "fps")) {
      dirs <- utils::head(dirs, -1L)
    }
    replicate_dir <- utils::tail(dirs, 1L)
    scenario_label <- if (is.null(run_name)) {
      utils::tail(utils::head(dirs, -1L), 1L)
    } else {
      basename(run_name)
    }
    df <- utils::read.csv(p, strip.white = TRUE)
    names(df) <- c(
      "Type",
      "YearCreated",
      "YearReported",
      "Market",
      "FromPool",
      "ToPool",
      "AmountEmitted",
      "AmountRetained"
    )
    tibble::as_tibble(df) |>
      dplyr::mutate(scenario = scenario_label, replicate = replicate_dir, .before = 1L)
  }) |>
    dplyr::bind_rows()
}

#' Summarise FPSM output into annual carbon stocks and atmospheric emissions
#'
#' Reduces the per-transfer raw output to one row per
#' `(scenario, replicate, year)`.
#'
#' **The terminal year is not comparable to the others and is dropped by
#' default.** FPSM writes its annual end-of-year stock reports (types 4 and 5)
#' up to the second-to-last simulated year, then emits a different, partial set
#' of residual rows for the final year: types 1 and 2 only, and only for the
#' pools that decay. In the shipped complex example the special-pool stock is
#' 225 t C in year 19, and year 20 carries no type 4 or 5 row at all: this
#' summary would report 0 there, while a naive sum over every retained row would
#' report 76 t C. Both are artefacts of that reporting change rather than a real
#' collapse, so carrying the final year into a stock time series draws a cliff
#' that did not happen.
#' The cut is derived from the data -- the last year with a type 4 or 5 report --
#' not from a hard-coded year.
#'
#' @param raw Tibble from [read_fps_raw_out()].
#' @param drop_terminal_year Logical. Drop years after the last annual stock
#'   report. `FALSE` keeps them; read the note above before doing so.
#'
#' @returns A tibble with `scenario`, `replicate`, `year`, and the tonnes of
#'   carbon in products (`products_tC`) and in special pools such as landfills
#'   and dumps (`special_pools_tC`), plus atmospheric emissions in that year as
#'   carbon (`emitted_co2_tC`, `emitted_ch4_tC`). Emissions are carbon, not
#'   CO2e and not CH4 mass; convert downstream.
#'
#' @family FPSM helpers
#'
#' @export
fps_pools <- function(raw, drop_terminal_year = TRUE) {
  if (nrow(raw) == 0L) {
    return(tibble::tibble())
  }
  keep <- raw
  if (isTRUE(drop_terminal_year)) {
    last_full <- keep |>
      dplyr::filter(.data$Type %in% .fps_stock_types) |>
      dplyr::group_by(.data$scenario, .data$replicate) |>
      dplyr::summarise(last_year = max(.data$YearReported), .groups = "drop")
    keep <- keep |>
      dplyr::inner_join(last_full, by = c("scenario", "replicate")) |>
      dplyr::filter(.data$YearReported <= .data$last_year) |>
      dplyr::select(-"last_year")
  }
  keep |>
    dplyr::group_by(.data$scenario, .data$replicate, year = .data$YearReported) |>
    dplyr::summarise(
      products_tC = sum(.data$AmountRetained[.data$Type == .fps_stock_types[["product"]]]),
      special_pools_tC = sum(.data$AmountRetained[.data$Type == .fps_stock_types[["special"]]]),
      emitted_co2_tC = sum(.data$AmountEmitted[.data$ToPool == .fps_gas_codes[["E_CO2"]]]),
      emitted_ch4_tC = sum(.data$AmountEmitted[.data$ToPool == .fps_gas_codes[["E_CH4"]]]),
      .groups = "drop"
    ) |>
    dplyr::arrange(.data$scenario, .data$replicate, .data$year)
}

#' Annual FPSM stocks broken out by pool
#'
#' The per-pool counterpart to [fps_pools()], for stacked figures.
#'
#' No `drop_terminal_year` argument is needed here: this function keeps only the
#' annual end-of-year stock reports (types 4 and 5), and the terminal residual
#' rows are types 1 and 2, so they are already excluded. See [fps_pools()] for
#' why that distinction matters.
#'
#' @param raw Tibble from [read_fps_raw_out()].
#'
#' @returns A tibble with `scenario`, `replicate`, `year`, `pool` (the FPSM code),
#'   `kind` (`"product"` or `"special"`) and `stock_tC`.
#'
#' @family FPSM helpers
#'
#' @export
fps_stocks_by_pool <- function(raw) {
  if (nrow(raw) == 0L) {
    return(tibble::tibble())
  }
  dplyr::filter(raw, .data$Type %in% .fps_stock_types, .data$AmountRetained > 0) |>
    dplyr::group_by(
      .data$scenario,
      .data$replicate,
      year = .data$YearReported,
      pool = .data$FromPool,
      kind = ifelse(.data$Type == .fps_stock_types[["product"]], "product", "special")
    ) |>
    dplyr::summarise(stock_tC = sum(.data$AmountRetained), .groups = "drop") |>
    dplyr::arrange(.data$scenario, .data$replicate, .data$year, .data$pool)
}

#' Write one replicate's FPSM raw output to a partitioned parquet
#'
#' The writer counterpart to [open_fps_raw_out_dataset()], mirroring
#' [write_forcs_log_summary_parquet()] including its atomic publish: the parquet
#' is written to a temporary file and then [fs::file_move()]d into place, so a
#' concurrent reader or a retried write never sees a partial file. `scenario` is
#' embedded as a data column so several per-scenario roots can be unioned.
#'
#' @param src_path Path to one replicate's `FPS_raw_out.csv`.
#' @param scenario_dir Scenario directory to publish under. When `NULL` (default)
#'   it is derived from `src_path`, skipping an `fps/` working directory.
#' @param subdir Path within the scenario directory for the dataset root.
#' @param staging_dir Optional directory for the temporary parquet; `NULL`
#'   (default) stages in the destination so the move is an atomic rename.
#'
#' @returns The written parquet path.
#'
#' @family FPSM helpers
#'
#' @export
write_fps_raw_out_parquet <- function(
  src_path,
  scenario_dir = NULL,
  subdir = "_aggregates/fps_raw_out",
  staging_dir = NULL
) {
  .need("arrow", "Writing a parquet summary")
  df <- read_fps_raw_out(src_path)
  if (nrow(df) == 0L) {
    stop("write_fps_raw_out_parquet(): empty input for ", src_path, call. = FALSE)
  }
  rep <- unique(df$replicate)
  stopifnot(length(unique(df$scenario)) == 1L, length(rep) == 1L)
  if (is.null(scenario_dir)) {
    dirs <- fs::path_split(fs::path_dir(src_path))[[1L]]
    if (length(dirs) && identical(utils::tail(dirs, 1L), "fps")) {
      dirs <- utils::head(dirs, -1L)
    }
    scenario_dir <- fs::path_join(utils::head(dirs, -1L))
  }
  dst_dir <- fs::path(scenario_dir, subdir, paste0("replicate=", rep))
  fs::dir_create(dst_dir)
  dst <- fs::path(dst_dir, "part-0.parquet")
  tmp_root <- staging_dir %||% dst_dir
  fs::dir_create(tmp_root)
  tmp <- tempfile("part-", tmpdir = tmp_root, fileext = ".parquet")
  arrow::write_parquet(df, tmp)
  fs::file_move(tmp, dst)
  as.character(dst)
}

#' Open the FPSM raw-output Arrow dataset for one or more scenarios
#'
#' Mirrors [open_forcs_log_summary_dataset()]: a single root opens directly,
#' several roots under different parents are combined into a `UnionDataset`
#' (Arrow cannot treat Hive trees under different parents as one dataset), and
#' missing roots are dropped.
#'
#' @param dataset_roots Character vector of
#'   `<scenario>/_aggregates/fps_raw_out` paths.
#'
#' @returns An Arrow `Dataset` (lazy), or `NULL` if no roots exist.
#'
#' @family FPSM helpers
#'
#' @export
open_fps_raw_out_dataset <- function(dataset_roots) {
  .need("arrow", "Opening a parquet dataset")
  existing <- dataset_roots[dir.exists(dataset_roots)]
  if (length(existing) == 0L) {
    return(NULL)
  }
  if (length(existing) == 1L) {
    return(arrow::open_dataset(existing, partitioning = "replicate"))
  }
  child_ds <- lapply(existing, function(p) arrow::open_dataset(p, partitioning = "replicate"))
  arrow::open_dataset(child_ds)
}
