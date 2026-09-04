# Read one or more FPSM `FPS_raw_out.csv` files

Attaches `scenario` / `replicate` labels derived from the directory
structure. The run directory is expected at either
`<scenario>/<replicate>/FPS_raw_out.csv` or
`<scenario>/<replicate>/fps/FPS_raw_out.csv`; a parent directory named
`fps` is skipped when deriving the labels, so both layouts work.

## Usage

``` r
read_fps_raw_out(paths, run_name = NULL)
```

## Arguments

- paths:

  Character vector of `FPS_raw_out.csv` paths.

- run_name:

  Optional scenario directory path; when `NULL` (default) the scenario
  label is derived from `paths`.

## Value

A tibble with leading `scenario` / `replicate` columns and the eight
FPSM output fields, `To_Gas/Pool` renamed to `ToPool`. Amounts are
tonnes of carbon (see the file header note on units).

## See also

Other FPSM helpers:
[`fps_output_files()`](https://for-cast.github.io/landisutils/reference/fps_output_files.md),
[`fps_pools()`](https://for-cast.github.io/landisutils/reference/fps_pools.md),
[`fps_run_docker()`](https://for-cast.github.io/landisutils/reference/fps_run_docker.md),
[`fps_stocks_by_pool()`](https://for-cast.github.io/landisutils/reference/fps_stocks_by_pool.md),
[`open_fps_raw_out_dataset()`](https://for-cast.github.io/landisutils/reference/open_fps_raw_out_dataset.md),
[`write_fps_raw_out_parquet()`](https://for-cast.github.io/landisutils/reference/write_fps_raw_out_parquet.md)
