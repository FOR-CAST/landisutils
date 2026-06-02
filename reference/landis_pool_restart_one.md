# Restart a single container in a warm Docker pool

Stops + removes the container at index `idx` if it exists, then starts a
fresh replacement with identical config (image, scratch_root bind-mount,
user, cpu_limit, mem_limit) using a new auto-generated container name.
The pool object's `$names[idx]` is updated to point at the new
container.

## Usage

``` r
landis_pool_restart_one(pool, idx)
```

## Arguments

- pool:

  A `landis_pool` object from
  [`landis_pool_start()`](https://for-cast.github.io/landisutils/reference/landis_pool_start.md).

- idx:

  Integer. 1-based index of the container to replace.

## Value

The pool (invisibly), with `$names[idx]` updated to the new container
name.

## Details

Intended for use by
[`landis_pool_exec()`](https://for-cast.github.io/landisutils/reference/landis_pool_exec.md)'s
`retries` mechanism, but also safe to call directly when a calibration
driver detects a container is wedged or has been OOM-killed.

## See also

[`landis_pool_start()`](https://for-cast.github.io/landisutils/reference/landis_pool_start.md),
[`landis_pool_exec()`](https://for-cast.github.io/landisutils/reference/landis_pool_exec.md),
[`landis_pool_stop()`](https://for-cast.github.io/landisutils/reference/landis_pool_stop.md)

Other LANDIS-II execution helpers:
[`host_cpu_info()`](https://for-cast.github.io/landisutils/reference/host_cpu_info.md),
[`landis_find()`](https://for-cast.github.io/landisutils/reference/landis_find.md),
[`landis_find_docker()`](https://for-cast.github.io/landisutils/reference/landis_find_docker.md),
[`landis_pool_exec()`](https://for-cast.github.io/landisutils/reference/landis_pool_exec.md),
[`landis_pool_start()`](https://for-cast.github.io/landisutils/reference/landis_pool_start.md),
[`landis_pool_stop()`](https://for-cast.github.io/landisutils/reference/landis_pool_stop.md),
[`landis_replicate()`](https://for-cast.github.io/landisutils/reference/landis_replicate.md),
[`landis_run_docker()`](https://for-cast.github.io/landisutils/reference/landis_run_docker.md),
[`landis_run_local()`](https://for-cast.github.io/landisutils/reference/landis_run_local.md),
[`read_landis_resource_logs()`](https://for-cast.github.io/landisutils/reference/read_landis_resource_logs.md),
[`tar_landis()`](https://for-cast.github.io/landisutils/reference/tar_landis.md),
[`write_landis_scenario_file()`](https://for-cast.github.io/landisutils/reference/write_landis_scenario_file.md)
