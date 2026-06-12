# Stop and remove all containers in a warm Docker pool

Sends `docker stop` (graceful SIGTERM, the container's trap exits
cleanly) then `docker rm -f` (idempotent belt-and-suspenders) for every
container in the pool. Safe to call multiple times.

## Usage

``` r
landis_pool_stop(pool, timeout_sec = 10)
```

## Arguments

- pool:

  A `landis_pool` object from
  [`landis_pool_start()`](https://for-cast.github.io/landisutils/reference/landis_pool_start.md).

- timeout_sec:

  Numeric. Graceful-stop timeout passed to `docker stop`. Default 10.

## Value

The pool (invisibly), with `stopped_at` added.

## Details

Typical use is via `on.exit(landis_pool_stop(pool), add = TRUE)` so the
pool is cleaned up even when the calling DEoptim driver errors out.

## See also

[`landis_pool_start()`](https://for-cast.github.io/landisutils/reference/landis_pool_start.md),
[`landis_pool_exec()`](https://for-cast.github.io/landisutils/reference/landis_pool_exec.md)

Other LANDIS-II execution helpers:
[`host_cpu_info()`](https://for-cast.github.io/landisutils/reference/host_cpu_info.md),
[`landis_archive_rep()`](https://for-cast.github.io/landisutils/reference/landis_archive_rep.md),
[`landis_find()`](https://for-cast.github.io/landisutils/reference/landis_find.md),
[`landis_find_docker()`](https://for-cast.github.io/landisutils/reference/landis_find_docker.md),
[`landis_pool_exec()`](https://for-cast.github.io/landisutils/reference/landis_pool_exec.md),
[`landis_pool_restart_one()`](https://for-cast.github.io/landisutils/reference/landis_pool_restart_one.md),
[`landis_pool_start()`](https://for-cast.github.io/landisutils/reference/landis_pool_start.md),
[`landis_replicate()`](https://for-cast.github.io/landisutils/reference/landis_replicate.md),
[`landis_run_docker()`](https://for-cast.github.io/landisutils/reference/landis_run_docker.md),
[`landis_run_local()`](https://for-cast.github.io/landisutils/reference/landis_run_local.md),
[`read_landis_resource_logs()`](https://for-cast.github.io/landisutils/reference/read_landis_resource_logs.md),
[`tar_landis()`](https://for-cast.github.io/landisutils/reference/tar_landis.md),
[`write_landis_scenario_file()`](https://for-cast.github.io/landisutils/reference/write_landis_scenario_file.md)
