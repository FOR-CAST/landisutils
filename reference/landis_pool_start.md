# Start a warm Docker pool for LANDIS-II calibration

Spawns `n` detached containers from `image`, each bind-mounting
`scratch_root` to `/scratch` inside. Each container runs a `sleep`-based
busy loop that responds to `docker stop` by exiting cleanly. The
intended workflow is to dispatch many
[`landis_pool_exec()`](https://for-cast.github.io/landisutils/reference/landis_pool_exec.md)
calls to the pool, then call
[`landis_pool_stop()`](https://for-cast.github.io/landisutils/reference/landis_pool_stop.md)
to tear it down – typically via
[`on.exit()`](https://rdrr.io/r/base/on.exit.html) in the calling
driver.

## Usage

``` r
landis_pool_start(
  n,
  image = NULL,
  scratch_root,
  cpu_limit = 4,
  mem_limit = "8g",
  pull = FALSE,
  name_prefix = NULL
)
```

## Arguments

- n:

  Integer. Number of pool containers to start. Typically the number of
  parallel DEoptim workers.

- image:

  Character or NULL. Docker image reference. NULL =
  `getOption("landisutils.docker.image")`.

- scratch_root:

  Character. Host directory bind-mounted to `/scratch` inside every
  container. Must be a stable absolute path. All per-trial directories
  used by
  [`landis_pool_exec()`](https://for-cast.github.io/landisutils/reference/landis_pool_exec.md)
  must be SUBDIRECTORIES of this.

- cpu_limit:

  Numeric or NULL. Per-container `--cpus` cap. Default 4.

- mem_limit:

  Character or NULL. Per-container `--memory` cap. Default `"8g"`.

- pull:

  Logical. When TRUE, `docker pull` the image before starting.

- name_prefix:

  Character or NULL. Optional prefix for the container name; the rest is
  auto-generated to avoid collisions across concurrent pools.

## Value

A list with `names` (container names), `image`, `scratch_root`, `n`,
`started_at`, and `digest` (image RepoDigest if available).

## See also

[`landis_pool_exec()`](https://for-cast.github.io/landisutils/reference/landis_pool_exec.md),
[`landis_pool_stop()`](https://for-cast.github.io/landisutils/reference/landis_pool_stop.md),
[`landis_run_docker()`](https://for-cast.github.io/landisutils/reference/landis_run_docker.md)

Other LANDIS-II execution helpers:
[`host_cpu_info()`](https://for-cast.github.io/landisutils/reference/host_cpu_info.md),
[`landis_archive_rep()`](https://for-cast.github.io/landisutils/reference/landis_archive_rep.md),
[`landis_find()`](https://for-cast.github.io/landisutils/reference/landis_find.md),
[`landis_find_docker()`](https://for-cast.github.io/landisutils/reference/landis_find_docker.md),
[`landis_pool_exec()`](https://for-cast.github.io/landisutils/reference/landis_pool_exec.md),
[`landis_pool_restart_one()`](https://for-cast.github.io/landisutils/reference/landis_pool_restart_one.md),
[`landis_pool_stop()`](https://for-cast.github.io/landisutils/reference/landis_pool_stop.md),
[`landis_replicate()`](https://for-cast.github.io/landisutils/reference/landis_replicate.md),
[`landis_run_docker()`](https://for-cast.github.io/landisutils/reference/landis_run_docker.md),
[`landis_run_local()`](https://for-cast.github.io/landisutils/reference/landis_run_local.md),
[`read_landis_resource_logs()`](https://for-cast.github.io/landisutils/reference/read_landis_resource_logs.md),
[`tar_landis()`](https://for-cast.github.io/landisutils/reference/tar_landis.md),
[`write_landis_scenario_file()`](https://for-cast.github.io/landisutils/reference/write_landis_scenario_file.md)
