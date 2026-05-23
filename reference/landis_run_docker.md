# Run a LANDIS-II simulation inside a Docker container

Runs LANDIS-II in an ephemeral Docker container, blocking until the
simulation completes. The scenario directory is bind-mounted to `/sim`
inside the container. Stdout and stderr are written to
`<scenario_dir>/log/docker_stdout.log` and `docker_stderr.log`.

## Usage

``` r
landis_run_docker(
  scenario_dir,
  scenario_file = "scenario.txt",
  image = NULL,
  console = NULL
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
  container**. Defaults to `NULL`, which uses the standard path for the
  official LANDIS-II v8 Docker images.

## Value

Integer exit code, invisibly.

## See also

[`landis_find()`](https://for-cast.github.io/landisutils/reference/landis_find.md),
[`landis_run()`](https://for-cast.github.io/landisutils/reference/landis_run.md),
[`landis_run_local()`](https://for-cast.github.io/landisutils/reference/landis_run_local.md),
[`tar_landis()`](https://for-cast.github.io/landisutils/reference/tar_landis.md)

Other LANDIS-II execution helpers:
[`landis_run_local()`](https://for-cast.github.io/landisutils/reference/landis_run_local.md),
[`tar_landis()`](https://for-cast.github.io/landisutils/reference/tar_landis.md)
