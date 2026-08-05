# Find the LANDIS-II console for a local installation

Returns the path to `Landis.Console.dll` for a locally-installed
LANDIS-II. Resolution order: the `LANDIS_CONSOLE` environment variable,
then a filesystem search under `/opt` for a `build/Release/` path.

## Usage

``` r
landis_find(check_version = TRUE, required_major = landis_target_version())
```

## Arguments

- check_version:

  Logical. When `TRUE` (default), verify the console's major version
  with
  [`landis_assert_version()`](https://for-cast.github.io/landisutils/reference/landis_assert_version.md)
  and stop if it is not `required_major`. A version that cannot be
  determined also stops – see that function for why, and for the
  opt-out.

- required_major:

  Integer. Required LANDIS-II major version. Defaults to
  [`landis_target_version()`](https://for-cast.github.io/landisutils/reference/landis_target_version.md),
  the generation this package is built for.

## Value

Character. Path to `Landis.Console.dll`, or `NA_character_` if not
found.

## Details

The `/opt` search is a Linux convention and finds nothing on Windows,
where `LANDIS_CONSOLE` is the only route. `method = "local"` is the
default there (see
[`tar_landis()`](https://for-cast.github.io/landisutils/reference/tar_landis.md)),
so a Windows user must set that variable.

## See also

[`landis_assert_version()`](https://for-cast.github.io/landisutils/reference/landis_assert_version.md),
[`landis_version()`](https://for-cast.github.io/landisutils/reference/landis_version.md),
[`landis_find_docker()`](https://for-cast.github.io/landisutils/reference/landis_find_docker.md),
[`landis_run_local()`](https://for-cast.github.io/landisutils/reference/landis_run_local.md),
[`tar_landis()`](https://for-cast.github.io/landisutils/reference/tar_landis.md)

Other LANDIS-II execution helpers:
[`host_cpu_info()`](https://for-cast.github.io/landisutils/reference/host_cpu_info.md),
[`landis_archive_rep()`](https://for-cast.github.io/landisutils/reference/landis_archive_rep.md),
[`landis_assert_version()`](https://for-cast.github.io/landisutils/reference/landis_assert_version.md),
[`landis_find_docker()`](https://for-cast.github.io/landisutils/reference/landis_find_docker.md),
[`landis_pool_exec()`](https://for-cast.github.io/landisutils/reference/landis_pool_exec.md),
[`landis_pool_restart_one()`](https://for-cast.github.io/landisutils/reference/landis_pool_restart_one.md),
[`landis_pool_start()`](https://for-cast.github.io/landisutils/reference/landis_pool_start.md),
[`landis_pool_stop()`](https://for-cast.github.io/landisutils/reference/landis_pool_stop.md),
[`landis_replicate()`](https://for-cast.github.io/landisutils/reference/landis_replicate.md),
[`landis_run_docker()`](https://for-cast.github.io/landisutils/reference/landis_run_docker.md),
[`landis_run_local()`](https://for-cast.github.io/landisutils/reference/landis_run_local.md),
[`landis_target_version()`](https://for-cast.github.io/landisutils/reference/landis_target_version.md),
[`landis_version()`](https://for-cast.github.io/landisutils/reference/landis_version.md),
[`read_landis_resource_logs()`](https://for-cast.github.io/landisutils/reference/read_landis_resource_logs.md),
[`tar_landis()`](https://for-cast.github.io/landisutils/reference/tar_landis.md),
[`write_landis_scenario_file()`](https://for-cast.github.io/landisutils/reference/write_landis_scenario_file.md)
