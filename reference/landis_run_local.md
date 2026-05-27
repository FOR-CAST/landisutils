# Run a LANDIS-II simulation locally (synchronous)

Runs LANDIS-II directly via `dotnet`, blocking until the simulation
completes. Stdout and stderr are written to
`<scenario_dir>/log/local_stdout.log` and `local_stderr.log`. Wall-clock
elapsed time and peak memory use (polled every 2 s via the `ps` package)
are reported on completion and written to
`<scenario_dir>/log/local_resources.log`. Works on Linux, macOS, and
Windows.

## Usage

``` r
landis_run_local(scenario_dir, scenario_file = "scenario.txt", console = NULL)
```

## Arguments

- scenario_dir:

  Character. Path to the scenario directory (resolved to absolute before
  use).

- scenario_file:

  Character. Scenario filename relative to `scenario_dir`.

- console:

  Character or `NULL`. Path to `Landis.Console.dll`. Defaults to `NULL`,
  which calls
  [`landis_find()`](https://for-cast.github.io/landisutils/reference/landis_find.md)
  at run time.

## Value

Named list with `exit_code` (integer), `elapsed_sec` (numeric), and
`peak_mem_bytes` (numeric), returned invisibly.

## See also

[`landis_find()`](https://for-cast.github.io/landisutils/reference/landis_find.md),
[`landis_find_docker()`](https://for-cast.github.io/landisutils/reference/landis_find_docker.md),
[`landis_run()`](https://for-cast.github.io/landisutils/reference/landis_run.md),
[`landis_run_docker()`](https://for-cast.github.io/landisutils/reference/landis_run_docker.md),
[`tar_landis()`](https://for-cast.github.io/landisutils/reference/tar_landis.md)

Other LANDIS-II execution helpers:
[`landis_find()`](https://for-cast.github.io/landisutils/reference/landis_find.md),
[`landis_find_docker()`](https://for-cast.github.io/landisutils/reference/landis_find_docker.md),
[`landis_replicate()`](https://for-cast.github.io/landisutils/reference/landis_replicate.md),
[`landis_run_docker()`](https://for-cast.github.io/landisutils/reference/landis_run_docker.md),
[`tar_landis()`](https://for-cast.github.io/landisutils/reference/tar_landis.md)
