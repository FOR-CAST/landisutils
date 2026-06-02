# Execute a command in one of the warm-pool containers

Runs `docker exec --workdir <workdir> <container> <command> <args>` in
the container at index `idx` of `pool`. Captures stdout/stderr to disk
if requested. Each invocation is a fresh dotnet process in the container
– no in-process state from prior calls carries over – and the per-trial
working directory isolates output files.

## Usage

``` r
landis_pool_exec(
  pool,
  idx,
  workdir,
  command,
  args = character(),
  timeout_sec = NULL,
  stdout_log = NULL,
  stderr_log = NULL,
  extra_env = NULL,
  retries = 0L
)
```

## Arguments

- pool:

  A `landis_pool` object from
  [`landis_pool_start()`](https://for-cast.github.io/landisutils/reference/landis_pool_start.md).

- idx:

  Integer. 1-based container index within the pool.

- workdir:

  Character. Working directory INSIDE the container (i.e., under
  `/scratch/`). Must correspond to a host path under
  `pool$scratch_root`.

- command:

  Character. Executable to run in the container (e.g., "dotnet").

- args:

  Character vector. Arguments passed to `command`.

- timeout_sec:

  Numeric or NULL. Maximum wait time. NULL = no timeout.

- stdout_log, stderr_log:

  Character or NULL. Paths to write captured output.

- extra_env:

  Named character. Extra environment variables to set via `--env`.
  Default sets `HOME=/tmp` and `DOTNET_BUNDLE_EXTRACT_BASE_DIR` to a
  unique-per-call tempdir, isolating dotnet's per-user caches between
  trials.

- retries:

  Integer \>= 0. If the exec command fails with a non-zero exit status,
  restart the container via
  [`landis_pool_restart_one()`](https://for-cast.github.io/landisutils/reference/landis_pool_restart_one.md)
  and try again, up to `retries` extra attempts. Default 0 = fail-fast
  (matches prior behaviour). Useful when long calibrations occasionally
  see container crashes (OOM, daemon hiccup) without wanting to abort.

## Value

A list with `status` (integer exit code), `elapsed_sec` (numeric),
`container` (the container name), and `attempts` (1 + number of retries
actually consumed).

## Details

For LANDIS-II calibration trials specifically, see
[`sim_landis()`](https://for-cast.github.io/landisutils/reference/sim_landis.md)
which wraps this with the trial-directory copy + dynamic-fire.txt patch
logic.

## See also

[`landis_pool_start()`](https://for-cast.github.io/landisutils/reference/landis_pool_start.md),
[`landis_pool_stop()`](https://for-cast.github.io/landisutils/reference/landis_pool_stop.md),
[`landis_pool_restart_one()`](https://for-cast.github.io/landisutils/reference/landis_pool_restart_one.md)

Other LANDIS-II execution helpers:
[`host_cpu_info()`](https://for-cast.github.io/landisutils/reference/host_cpu_info.md),
[`landis_find()`](https://for-cast.github.io/landisutils/reference/landis_find.md),
[`landis_find_docker()`](https://for-cast.github.io/landisutils/reference/landis_find_docker.md),
[`landis_pool_restart_one()`](https://for-cast.github.io/landisutils/reference/landis_pool_restart_one.md),
[`landis_pool_start()`](https://for-cast.github.io/landisutils/reference/landis_pool_start.md),
[`landis_pool_stop()`](https://for-cast.github.io/landisutils/reference/landis_pool_stop.md),
[`landis_replicate()`](https://for-cast.github.io/landisutils/reference/landis_replicate.md),
[`landis_run_docker()`](https://for-cast.github.io/landisutils/reference/landis_run_docker.md),
[`landis_run_local()`](https://for-cast.github.io/landisutils/reference/landis_run_local.md),
[`read_landis_resource_logs()`](https://for-cast.github.io/landisutils/reference/read_landis_resource_logs.md),
[`tar_landis()`](https://for-cast.github.io/landisutils/reference/tar_landis.md),
[`write_landis_scenario_file()`](https://for-cast.github.io/landisutils/reference/write_landis_scenario_file.md)
