# Report the version of a LANDIS-II console

Runs the console with no scenario file. It prints its version banner –
`LANDIS-II 8.0 (8)` – and then exits with an error about the missing
scenario, so a non-zero status is expected and is not treated as
failure.

## Usage

``` r
landis_version(
  console = NULL,
  image = NULL,
  container = NULL,
  timeout = 60,
  check_version = FALSE
)
```

## Arguments

- console:

  Character or `NULL`. Path to `Landis.Console.dll`; resolved via
  [`landis_find()`](https://for-cast.github.io/landisutils/reference/landis_find.md)
  when `NULL` and no `image`/`container` is given.

- image:

  Character or `NULL`. Docker image to probe.

- container:

  Character or `NULL`. Running container to probe.

- timeout:

  Numeric. Seconds to wait for the console to print its banner.

- check_version:

  Logical. Passed to
  [`landis_find()`](https://for-cast.github.io/landisutils/reference/landis_find.md)
  when `console` is `NULL`; `FALSE` here avoids infinite recursion.

## Value

A [numeric_version](https://rdrr.io/r/base/numeric_version.html), or
`NA` when the console, `dotnet`, or the banner cannot be found.

## Details

The console can be a local install, a Docker `image` (probed with a
throwaway container), or a `container` that is already running (probed
with `docker exec`). Supply at most one of the three; `console` is the
default.

## See also

[`landis_find()`](https://for-cast.github.io/landisutils/reference/landis_find.md),
[`landis_assert_version()`](https://for-cast.github.io/landisutils/reference/landis_assert_version.md)

Other LANDIS-II execution helpers:
[`host_cpu_info()`](https://for-cast.github.io/landisutils/reference/host_cpu_info.md),
[`landis_archive_rep()`](https://for-cast.github.io/landisutils/reference/landis_archive_rep.md),
[`landis_assert_version()`](https://for-cast.github.io/landisutils/reference/landis_assert_version.md),
[`landis_find()`](https://for-cast.github.io/landisutils/reference/landis_find.md),
[`landis_find_docker()`](https://for-cast.github.io/landisutils/reference/landis_find_docker.md),
[`landis_pool_exec()`](https://for-cast.github.io/landisutils/reference/landis_pool_exec.md),
[`landis_pool_restart_one()`](https://for-cast.github.io/landisutils/reference/landis_pool_restart_one.md),
[`landis_pool_start()`](https://for-cast.github.io/landisutils/reference/landis_pool_start.md),
[`landis_pool_stop()`](https://for-cast.github.io/landisutils/reference/landis_pool_stop.md),
[`landis_replicate()`](https://for-cast.github.io/landisutils/reference/landis_replicate.md),
[`landis_run_docker()`](https://for-cast.github.io/landisutils/reference/landis_run_docker.md),
[`landis_run_local()`](https://for-cast.github.io/landisutils/reference/landis_run_local.md),
[`landis_target_version()`](https://for-cast.github.io/landisutils/reference/landis_target_version.md),
[`read_landis_resource_logs()`](https://for-cast.github.io/landisutils/reference/read_landis_resource_logs.md),
[`tar_landis()`](https://for-cast.github.io/landisutils/reference/tar_landis.md),
[`validate_landis_scenario()`](https://for-cast.github.io/landisutils/reference/validate_landis_scenario.md),
[`write_landis_scenario_file()`](https://for-cast.github.io/landisutils/reference/write_landis_scenario_file.md)
