# Run a LANDIS-II simulation inside a Docker container

Runs LANDIS-II in an ephemeral Docker container, blocking until the
simulation completes. The scenario directory is bind-mounted to `/sim`
inside the container. Stdout and stderr are written to
`<scenario_dir>/log/docker_stdout.log` and `docker_stderr.log`.
Wall-clock elapsed time and peak memory use (polled every 2 s from
`docker stats`) are reported on completion and written to
`<scenario_dir>/log/docker_resources.log`. The image's immutable
`sha256` digest is captured into `<scenario_dir>/log/docker_image.log`
so downstream provenance tools can pin runs to a specific image
regardless of subsequent tag movement.

## Usage

``` r
landis_run_docker(
  scenario_dir,
  scenario_file = "scenario.txt",
  image = NULL,
  console = NULL,
  pull = FALSE,
  cpu_limit = 4,
  mem_limit = "8g",
  mem_margin = 1.5,
  post_completion_timeout_sec = 300
)
```

## Arguments

- scenario_dir:

  Character. Path to the scenario directory (resolved to absolute before
  mounting).

- scenario_file:

  Character. Scenario filename relative to `scenario_dir`.

- image:

  Character or `NULL`. Docker image reference. Defaults to
  `getOption("landisutils.docker.image")` (set by `.onLoad()` to
  `"ghcr.io/landis-ii-foundation/landis-ii-v8-release:main"`).

- console:

  Character or `NULL`. Path to `Landis.Console.dll` **inside the
  container**. Defaults to `NULL`, which calls
  [`landis_find_docker()`](https://for-cast.github.io/landisutils/reference/landis_find_docker.md)
  at run time (reads `getOption("landisutils.docker.console")`).

- pull:

  Logical. When `TRUE`, run `docker pull <image>` before the simulation
  so the recorded digest reflects the current registry rather than a
  stale local copy. Defaults to `FALSE` to keep runs reproducible across
  iterations of an already-cached image.

- cpu_limit:

  Numeric or `NULL`. Hard CPU cap for the container
  (`docker run --cpus`). Default `4`: LANDIS-II compute is
  single-threaded but the .NET runtime spins up 9-11 OS threads, so 4 is
  a comfortable headroom default. Pass `NULL` for no CPU limit.

- mem_limit:

  Numeric byte count, character (e.g. `"8g"`, `"512m"`), `NULL`, or
  `Inf`. Baseline RAM cap (`docker run --memory`). Default `"8g"`. When
  a prior `<rep_dir>/log/*_resources.log` exists with a recorded
  `peak_mem_bytes`, the cap is raised to `peak_mem_bytes * mem_margin`
  if that exceeds the baseline – a rep that fit last time will never be
  killed by this cap on a rerun. When **no** prior log exists for the
  rep (first run, or rep dir freshly deleted), the cap is dropped
  entirely so the first run can discover what it needs.

- mem_margin:

  Numeric. Headroom factor applied to a previously observed peak when
  auto-raising `mem_limit`. Default `1.5`.

- post_completion_timeout_sec:

  Numeric. Grace period (seconds) after the string
  `"Model run is complete."` first appears in the container's stdout
  before the watchdog SIGTERMs the container. Some long ForCS + Dynamic
  Fire scenarios with many output extensions log this completion marker
  but then spin in the .NET runtime shutdown path indefinitely (outputs
  are already on disk, so the sim itself completed cleanly). On timeout
  the container is stopped and exit codes 137/143 are remapped to `0`.
  Set to `Inf` to disable the watchdog. Default `300` (5 min).

## Value

Named list with `exit_code` (integer), `elapsed_sec` (numeric), and
`peak_mem_bytes` (numeric), returned invisibly.

## See also

[`landis_find_docker()`](https://for-cast.github.io/landisutils/reference/landis_find_docker.md),
[`landis_find()`](https://for-cast.github.io/landisutils/reference/landis_find.md),
[`landis_run()`](https://for-cast.github.io/landisutils/reference/landis_run.md),
[`landis_run_local()`](https://for-cast.github.io/landisutils/reference/landis_run_local.md),
[`tar_landis()`](https://for-cast.github.io/landisutils/reference/tar_landis.md)

Other LANDIS-II execution helpers:
[`host_cpu_info()`](https://for-cast.github.io/landisutils/reference/host_cpu_info.md),
[`landis_find()`](https://for-cast.github.io/landisutils/reference/landis_find.md),
[`landis_find_docker()`](https://for-cast.github.io/landisutils/reference/landis_find_docker.md),
[`landis_pool_exec()`](https://for-cast.github.io/landisutils/reference/landis_pool_exec.md),
[`landis_pool_restart_one()`](https://for-cast.github.io/landisutils/reference/landis_pool_restart_one.md),
[`landis_pool_start()`](https://for-cast.github.io/landisutils/reference/landis_pool_start.md),
[`landis_pool_stop()`](https://for-cast.github.io/landisutils/reference/landis_pool_stop.md),
[`landis_replicate()`](https://for-cast.github.io/landisutils/reference/landis_replicate.md),
[`landis_run_local()`](https://for-cast.github.io/landisutils/reference/landis_run_local.md),
[`read_landis_resource_logs()`](https://for-cast.github.io/landisutils/reference/read_landis_resource_logs.md),
[`tar_landis()`](https://for-cast.github.io/landisutils/reference/tar_landis.md),
[`write_landis_scenario_file()`](https://for-cast.github.io/landisutils/reference/write_landis_scenario_file.md)
