# Open the FPSM raw-output Arrow dataset for one or more scenarios

Mirrors
[`open_forcs_log_summary_dataset()`](https://for-cast.github.io/landisutils/reference/open_forcs_log_summary_dataset.md):
a single root opens directly, several roots under different parents are
combined into a `UnionDataset` (Arrow cannot treat Hive trees under
different parents as one dataset), and missing roots are dropped.

## Usage

``` r
open_fps_raw_out_dataset(dataset_roots)
```

## Arguments

- dataset_roots:

  Character vector of `<scenario>/_aggregates/fps_raw_out` paths.

## Value

An Arrow `Dataset` (lazy), or `NULL` if no roots exist.

## See also

Other FPSM helpers:
[`fps_output_files()`](https://for-cast.github.io/landisutils/reference/fps_output_files.md),
[`fps_pools()`](https://for-cast.github.io/landisutils/reference/fps_pools.md),
[`fps_run_docker()`](https://for-cast.github.io/landisutils/reference/fps_run_docker.md),
[`fps_stocks_by_pool()`](https://for-cast.github.io/landisutils/reference/fps_stocks_by_pool.md),
[`read_fps_raw_out()`](https://for-cast.github.io/landisutils/reference/read_fps_raw_out.md),
[`write_fps_raw_out_parquet()`](https://for-cast.github.io/landisutils/reference/write_fps_raw_out_parquet.md)
