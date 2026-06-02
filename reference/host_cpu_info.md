# Host CPU and RAM identification (cross-platform)

Identifies the CPU model, logical core count, and total RAM of the
machine running the current R process. Each field falls back to `NA` if
it can't be determined.

## Usage

``` r
host_cpu_info()
```

## Value

A list with elements `model` (character), `n_logical` (integer), and
`ram_bytes` (numeric).

## Details

Implementation by platform:

- **Linux**: reads `/proc/cpuinfo` (`model name`) and `/proc/meminfo`
  (`MemTotal`).

- **macOS** (`Darwin`): `sysctl -n machdep.cpu.brand_string` for the CPU
  model, `sysctl -n hw.memsize` for RAM.

- **Windows**: `PROCESSOR_IDENTIFIER` environment variable for the CPU
  model, `wmic ComputerSystem get TotalPhysicalMemory` for RAM.

- Logical core count uses
  [`parallel::detectCores()`](https://rdrr.io/r/parallel/detectCores.html)
  on every platform.

Used by
[`landis_run_docker()`](https://for-cast.github.io/landisutils/reference/landis_run_docker.md)
and
[`landis_run_local()`](https://for-cast.github.io/landisutils/reference/landis_run_local.md)
to append host context to each rep's resource log so downstream
provenance tooling can identify what host produced any given replicate's
outputs.

## See also

Other LANDIS-II execution helpers:
[`landis_find()`](https://for-cast.github.io/landisutils/reference/landis_find.md),
[`landis_find_docker()`](https://for-cast.github.io/landisutils/reference/landis_find_docker.md),
[`landis_pool_exec()`](https://for-cast.github.io/landisutils/reference/landis_pool_exec.md),
[`landis_pool_restart_one()`](https://for-cast.github.io/landisutils/reference/landis_pool_restart_one.md),
[`landis_pool_start()`](https://for-cast.github.io/landisutils/reference/landis_pool_start.md),
[`landis_pool_stop()`](https://for-cast.github.io/landisutils/reference/landis_pool_stop.md),
[`landis_replicate()`](https://for-cast.github.io/landisutils/reference/landis_replicate.md),
[`landis_run_docker()`](https://for-cast.github.io/landisutils/reference/landis_run_docker.md),
[`landis_run_local()`](https://for-cast.github.io/landisutils/reference/landis_run_local.md),
[`read_landis_resource_logs()`](https://for-cast.github.io/landisutils/reference/read_landis_resource_logs.md),
[`tar_landis()`](https://for-cast.github.io/landisutils/reference/tar_landis.md),
[`write_landis_scenario_file()`](https://for-cast.github.io/landisutils/reference/write_landis_scenario_file.md)
