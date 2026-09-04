# Run the Forest Product Sector Module in a Docker container

Runs FPSM over one directory containing a configuration file and the two
ForCS flux logs it names, with that directory bind-mounted as the
container's working directory. FPSM writes
[`fps_output_files()`](https://for-cast.github.io/landisutils/reference/fps_output_files.md)
there under fixed names, so each run needs its own directory.

## Usage

``` r
fps_run_docker(
  run_dir,
  config_file = "fps.txt",
  image = NULL,
  assembly = "/opt/fps/Landis.Extension.FPS-v1.dll",
  console = NULL,
  pull = FALSE,
  cpu_limit = 1,
  mem_limit = "1g",
  error_on_log = TRUE,
  check_headers = TRUE
)
```

## Arguments

- run_dir:

  Character. Directory holding the configuration and flux logs;
  bind-mounted as the container working directory.

- config_file:

  Character. Configuration file name, relative to `run_dir`.

- image:

  Character. Container image. Defaults to the `landisutils.fps.image`
  option.

- assembly:

  Character. Path to the FPSM assembly inside the image. Passed
  explicitly with `--entrypoint dotnet` rather than relying on the
  image's own `ENTRYPOINT`, so the function works with any image
  carrying dotnet and the assembly.

- console:

  Character. Path to the `docker` executable; defaults to
  [`landis_find_docker()`](https://for-cast.github.io/landisutils/reference/landis_find_docker.md).

- pull:

  Logical. `docker pull` before running, so the captured digest reflects
  the registry rather than a possibly stale local copy.

- cpu_limit, mem_limit:

  Resource caps. FPSM is single-threaded and peaks well under 64 MB even
  on a 400-year replicate, so the defaults are generous. `NULL` or `Inf`
  omits the corresponding flag.

- error_on_log:

  Logical. Fail when `FPS_log.txt` is non-empty.

- check_headers:

  Logical. Perform the flux-log header assertion.

## Value

Character vector of paths to the files in
[`fps_output_files()`](https://for-cast.github.io/landisutils/reference/fps_output_files.md),
in that order, suitable for a `targets::tar_target(format = "file")`.

## Details

Three pre-flight checks run before the container starts, each guarding a
failure mode that is otherwise silent or obscure:

- the input files named in the configuration must exist **with exactly
  that case**. The shipped FPSM examples name `log_fluxDOM.csv` beside a
  file called `log_FluxDOM.csv`, which works on Windows and aborts on
  Linux.

- each flux log's header must still match the positions FPSM indexes
  (see `.fps_flux_columns`), because FPSM performs no header validation
  and would otherwise read a reordered column as the wrong quantity.

- on completion, a non-empty `FPS_log.txt` is treated as an error by
  default. That file collects the *non-fatal* problems FPSM detects,
  which include carbon that was never allocated to any pool, so a silent
  run is the only acceptable one.

## See also

[`landis_run_docker()`](https://for-cast.github.io/landisutils/reference/landis_run_docker.md),
[`landis_find_docker()`](https://for-cast.github.io/landisutils/reference/landis_find_docker.md)

Other FPSM helpers:
[`fps_output_files()`](https://for-cast.github.io/landisutils/reference/fps_output_files.md),
[`fps_pools()`](https://for-cast.github.io/landisutils/reference/fps_pools.md),
[`fps_stocks_by_pool()`](https://for-cast.github.io/landisutils/reference/fps_stocks_by_pool.md),
[`open_fps_raw_out_dataset()`](https://for-cast.github.io/landisutils/reference/open_fps_raw_out_dataset.md),
[`read_fps_raw_out()`](https://for-cast.github.io/landisutils/reference/read_fps_raw_out.md),
[`write_fps_raw_out_parquet()`](https://for-cast.github.io/landisutils/reference/write_fps_raw_out_parquet.md)
