## LANDIS-II ForCS log_Summary output helpers ------------------------------------------------------

#' Read ForCS `log_Summary.csv` files for one scenario (all replicates)
#'
#' Reads one or more `log_Summary.csv` files and attaches `scenario`/`replicate`
#' labels derived from the directory structure (`<scenario>/<replicate>/log_Summary.csv`),
#' optionally masking to the core study area.
#'
#' @param paths Character vector of `log_Summary.csv` file paths for one scenario.
#' @param run_name Relative scenario directory path (e.g. `"LANDIS-II/ForCS_only"`);
#'   when `NULL` (default) the scenario label is derived from the path.
#' @param cell_mask Optional `data.frame` with integer `row`/`column` columns
#'   identifying cells in the BUFFERED simulation grid that correspond to the core
#'   study area; when provided, only those cells are retained. Derive `row`/`column`
#'   by spatially intersecting the buffered initial-communities raster with the core
#'   study-area boundary (the buffered and core grids index the same physical cell
#'   differently).
#'
#' @return A tibble combining all replicates with leading `scenario`/`replicate`
#'   columns, or an empty tibble when `paths` is empty.
#'
#' @family ForCS output helpers
#'
#' @export
read_forcs_log_summary <- function(paths, run_name = NULL, cell_mask = NULL) {
  if (length(paths) == 0L) {
    return(tibble::tibble())
  }
  purrr::map(paths, function(p) {
    replicate_dir <- basename(dirname(p))
    scenario_label <- if (is.null(run_name)) basename(dirname(dirname(p))) else basename(run_name)
    df <- utils::read.csv(p) |>
      tibble::as_tibble() |>
      dplyr::mutate(scenario = scenario_label, replicate = replicate_dir, .before = 1L)
    if (!is.null(cell_mask)) {
      df <- dplyr::semi_join(df, cell_mask, by = c("row", "column"))
    }
    df
  }) |>
    dplyr::bind_rows()
}

#' Write one replicate's ForCS `log_Summary` to a partitioned parquet
#'
#' Reads one replicate's `log_Summary.csv` via [read_forcs_log_summary()], masks
#' to core cells, and writes it to
#' `<scenario_dir>/<subdir>/replicate=<rep>/part-0.parquet`, where `scenario_dir`
#' is inferred as `dirname(dirname(src_path))`. This is the writer counterpart to
#' [open_forcs_log_summary_dataset()]; `scenario` is embedded as a data column so
#' several per-scenario roots can be unioned and filtered without touching the
#' directory layout.
#'
#' The publish is atomic: the parquet is written to a temporary file and then
#' [fs::file_move()]d into place, so a concurrent reader or a retried write never
#' observes a partial file -- safe for many replicate writers running at once
#' against an NFS output directory. When `staging_dir` is supplied the temporary
#' is written there (e.g. per-host scratch, keeping the interim bytes off NFS)
#' and moved cross-filesystem; the default stages in the destination directory so
#' the move is a same-filesystem atomic rename.
#'
#' @param src_path Path to one replicate's `log_Summary.csv`.
#' @param cell_mask Optional `data.frame` with `row`/`column` columns identifying
#'   the core-area cells to retain (see [read_forcs_log_summary()]).
#' @param subdir Path within the scenario directory for the dataset root (default
#'   `"_aggregates/forcs_log_summary"`, matching [open_forcs_log_summary_dataset()]).
#' @param staging_dir Optional directory for the temporary parquet before it is
#'   moved into place; `NULL` (default) stages in the destination directory
#'   (same-filesystem atomic rename).
#'
#' @return The written parquet path.
#'
#' @family ForCS output helpers
#'
#' @export
write_forcs_log_summary_parquet <- function(
  src_path,
  cell_mask = NULL,
  subdir = "_aggregates/forcs_log_summary",
  staging_dir = NULL
) {
  df <- read_forcs_log_summary(src_path, run_name = NULL, cell_mask = cell_mask)
  if (nrow(df) == 0L) {
    stop("write_forcs_log_summary_parquet(): empty input for ", src_path, call. = FALSE)
  }
  rep <- unique(df$replicate)
  stopifnot(length(unique(df$scenario)) == 1L, length(rep) == 1L)
  dst_dir <- file.path(dirname(dirname(src_path)), subdir, paste0("replicate=", rep))
  fs::dir_create(dst_dir)
  dst <- file.path(dst_dir, "part-0.parquet")
  tmp_root <- staging_dir %||% dst_dir
  fs::dir_create(tmp_root)
  tmp <- tempfile("part-", tmpdir = tmp_root, fileext = ".parquet")
  arrow::write_parquet(df, tmp)
  fs::file_move(tmp, dst)
  dst
}

#' Open the ForCS `log_Summary` Arrow dataset for one or more scenarios
#'
#' Opens each existing `<scenario>/_aggregates/forcs_log_summary` root as an Arrow
#' dataset partitioned by `replicate`. A single root opens directly; multiple roots
#' under different parents are opened individually and combined into a
#' `UnionDataset` (Arrow cannot treat Hive trees under different parents as one
#' dataset). Missing roots (e.g. scenarios whose runs errored) are dropped.
#'
#' @param dataset_roots Character vector of `<scenario>/_aggregates/forcs_log_summary`
#'   paths.
#'
#' @return An Arrow `Dataset` (lazy), or `NULL` if no roots exist.
#'
#' @family ForCS output helpers
#'
#' @export
open_forcs_log_summary_dataset <- function(dataset_roots) {
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
