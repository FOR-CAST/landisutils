# Files FPSM writes into its working directory

FPSM creates these three under fixed names in the current working
directory, which is why each run needs a directory of its own.

## Usage

``` r
fps_output_files()
```

## Value

Character vector of file names.

## See also

Other FPSM helpers:
[`fps_pools()`](https://for-cast.github.io/landisutils/reference/fps_pools.md),
[`fps_run_docker()`](https://for-cast.github.io/landisutils/reference/fps_run_docker.md),
[`fps_stocks_by_pool()`](https://for-cast.github.io/landisutils/reference/fps_stocks_by_pool.md),
[`open_fps_raw_out_dataset()`](https://for-cast.github.io/landisutils/reference/open_fps_raw_out_dataset.md),
[`read_fps_raw_out()`](https://for-cast.github.io/landisutils/reference/read_fps_raw_out.md),
[`write_fps_raw_out_parquet()`](https://for-cast.github.io/landisutils/reference/write_fps_raw_out_parquet.md)
