# Read per-rep resource logs written by `landis_run_docker()` / `landis_run_local()`

Discovers all `docker_resources.log` and `local_resources.log` files
under `run_dir` (typically a scenario directory containing per-rep
subdirectories) and returns one row per rep with every key-value pair
the run helpers wrote. Always includes `elapsed_sec` and
`peak_mem_bytes`; rows from runs produced by landisutils \>= 0.0.22
additionally include `host_cpu_model`, `host_cpu_cores`, and
`host_ram_bytes`. Older logs return `NA` for missing fields.

## Usage

``` r
read_landis_resource_logs(run_dir)
```

## Arguments

- run_dir:

  Character. Scenario directory to search (recursively) for resource
  logs. Returns an empty data.frame when `run_dir` does not exist or
  contains no logs.

## Value

A `data.frame` with columns `replicate, source` (one of `"docker"` or
`"local"`), and one numeric or character column per recorded key.

## Details

Used by per-scenario report templates to surface run-time / memory /
host statistics in build-provenance appendices.

## See also

Other LANDIS-II execution helpers:
[`host_cpu_info()`](https://for-cast.github.io/landisutils/reference/host_cpu_info.md),
[`landis_find()`](https://for-cast.github.io/landisutils/reference/landis_find.md),
[`landis_find_docker()`](https://for-cast.github.io/landisutils/reference/landis_find_docker.md),
[`landis_replicate()`](https://for-cast.github.io/landisutils/reference/landis_replicate.md),
[`landis_run_docker()`](https://for-cast.github.io/landisutils/reference/landis_run_docker.md),
[`landis_run_local()`](https://for-cast.github.io/landisutils/reference/landis_run_local.md),
[`tar_landis()`](https://for-cast.github.io/landisutils/reference/tar_landis.md)
