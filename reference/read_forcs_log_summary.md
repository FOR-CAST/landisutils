# Read ForCS `log_Summary.csv` files for one scenario (all replicates)

Reads one or more `log_Summary.csv` files and attaches
`scenario`/`replicate` labels derived from the directory structure
(`<scenario>/<replicate>/log_Summary.csv`), optionally masking to the
core study area.

## Usage

``` r
read_forcs_log_summary(paths, run_name = NULL, cell_mask = NULL)
```

## Arguments

- paths:

  Character vector of `log_Summary.csv` file paths for one scenario.

- run_name:

  Relative scenario directory path (e.g. `"LANDIS-II/ForCS_only"`); when
  `NULL` (default) the scenario label is derived from the path.

- cell_mask:

  Optional `data.frame` with integer `row`/`column` columns identifying
  cells in the BUFFERED simulation grid that correspond to the core
  study area; when provided, only those cells are retained. Derive
  `row`/`column` by spatially intersecting the buffered
  initial-communities raster with the core study-area boundary (the
  buffered and core grids index the same physical cell differently).

## Value

A tibble combining all replicates with leading `scenario`/`replicate`
columns, or an empty tibble when `paths` is empty.

## See also

Other ForCS output helpers:
[`open_forcs_log_summary_dataset()`](https://for-cast.github.io/landisutils/reference/open_forcs_log_summary_dataset.md),
[`write_forcs_log_summary_parquet()`](https://for-cast.github.io/landisutils/reference/write_forcs_log_summary_parquet.md)
