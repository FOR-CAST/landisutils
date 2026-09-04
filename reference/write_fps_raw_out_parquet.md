# Write one replicate's FPSM raw output to a partitioned parquet

The writer counterpart to
[`open_fps_raw_out_dataset()`](https://for-cast.github.io/landisutils/reference/open_fps_raw_out_dataset.md),
mirroring
[`write_forcs_log_summary_parquet()`](https://for-cast.github.io/landisutils/reference/write_forcs_log_summary_parquet.md)
including its atomic publish: the parquet is written to a temporary file
and then
[`fs::file_move()`](https://fs.r-lib.org/reference/file_move.html)d into
place, so a concurrent reader or a retried write never sees a partial
file. `scenario` is embedded as a data column so several per-scenario
roots can be unioned.

## Usage

``` r
write_fps_raw_out_parquet(
  src_path,
  scenario_dir = NULL,
  subdir = "_aggregates/fps_raw_out",
  staging_dir = NULL
)
```

## Arguments

- src_path:

  Path to one replicate's `FPS_raw_out.csv`.

- scenario_dir:

  Scenario directory to publish under. When `NULL` (default) it is
  derived from `src_path`, skipping an `fps/` working directory.

- subdir:

  Path within the scenario directory for the dataset root.

- staging_dir:

  Optional directory for the temporary parquet; `NULL` (default) stages
  in the destination so the move is an atomic rename.

## Value

The written parquet path.

## See also

Other FPSM helpers:
[`fps_output_files()`](https://for-cast.github.io/landisutils/reference/fps_output_files.md),
[`fps_pools()`](https://for-cast.github.io/landisutils/reference/fps_pools.md),
[`fps_run_docker()`](https://for-cast.github.io/landisutils/reference/fps_run_docker.md),
[`fps_stocks_by_pool()`](https://for-cast.github.io/landisutils/reference/fps_stocks_by_pool.md),
[`open_fps_raw_out_dataset()`](https://for-cast.github.io/landisutils/reference/open_fps_raw_out_dataset.md),
[`read_fps_raw_out()`](https://for-cast.github.io/landisutils/reference/read_fps_raw_out.md)
