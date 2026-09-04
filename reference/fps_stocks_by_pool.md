# Annual FPSM stocks broken out by pool

The per-pool counterpart to
[`fps_pools()`](https://for-cast.github.io/landisutils/reference/fps_pools.md),
for stacked figures.

## Usage

``` r
fps_stocks_by_pool(raw)
```

## Arguments

- raw:

  Tibble from
  [`read_fps_raw_out()`](https://for-cast.github.io/landisutils/reference/read_fps_raw_out.md).

## Value

A tibble with `scenario`, `replicate`, `year`, `pool` (the FPSM code),
`kind` (`"product"` or `"special"`) and `stock_tC`.

## Details

No `drop_terminal_year` argument is needed here: this function keeps
only the annual end-of-year stock reports (types 4 and 5), and the
terminal residual rows are types 1 and 2, so they are already excluded.
See
[`fps_pools()`](https://for-cast.github.io/landisutils/reference/fps_pools.md)
for why that distinction matters.

## See also

Other FPSM helpers:
[`fps_output_files()`](https://for-cast.github.io/landisutils/reference/fps_output_files.md),
[`fps_pools()`](https://for-cast.github.io/landisutils/reference/fps_pools.md),
[`fps_run_docker()`](https://for-cast.github.io/landisutils/reference/fps_run_docker.md),
[`open_fps_raw_out_dataset()`](https://for-cast.github.io/landisutils/reference/open_fps_raw_out_dataset.md),
[`read_fps_raw_out()`](https://for-cast.github.io/landisutils/reference/read_fps_raw_out.md),
[`write_fps_raw_out_parquet()`](https://for-cast.github.io/landisutils/reference/write_fps_raw_out_parquet.md)
