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
  pull = FALSE
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
[`landis_replicate()`](https://for-cast.github.io/landisutils/reference/landis_replicate.md),
[`landis_run_local()`](https://for-cast.github.io/landisutils/reference/landis_run_local.md),
[`read_landis_resource_logs()`](https://for-cast.github.io/landisutils/reference/read_landis_resource_logs.md),
[`tar_landis()`](https://for-cast.github.io/landisutils/reference/tar_landis.md)
