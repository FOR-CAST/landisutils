# Require a specific LANDIS-II core generation before running anything

Probes the LANDIS-II console that a run is about to use and throws
unless its major version is `version`. Exactly one of `image`,
`container` or `console` identifies what to probe; the result is
memoized per process against that key, so a pool probes once rather than
once per replicate.

## Usage

``` r
landis_assert_version(
  version = landis_target_version(),
  image = NULL,
  container = NULL,
  console = NULL
)
```

## Arguments

- version:

  Integer. Required major version. Defaults to
  [`landis_target_version()`](https://for-cast.github.io/landisutils/reference/landis_target_version.md),
  i.e. the generation this package is built for.

- image:

  Character. Docker image to probe with a throwaway container.

- container:

  Character. Name of a running container to probe via `docker exec`.

- console:

  Character. Path to `Landis.Console.dll` for a local (non-Docker) run.

## Value

The detected version string (invisibly), e.g. `"8.0"`.

## Details

An **undetectable** version is treated as a failure, not as permission
to proceed: a probe that returns nothing is indistinguishable from a
console of the wrong generation that never announced itself, and
proceeding is the exact failure this guards against. Set
`options(landisutils.skip_version_check = TRUE)` to bypass the check
where that is genuinely wanted – an explicit, visible opt-out rather
than a silent one.

## See also

[`landis_target_version()`](https://for-cast.github.io/landisutils/reference/landis_target_version.md),
[`landis_pool_start()`](https://for-cast.github.io/landisutils/reference/landis_pool_start.md),
[`landis_run_docker()`](https://for-cast.github.io/landisutils/reference/landis_run_docker.md)

Other LANDIS-II execution helpers:
[`host_cpu_info()`](https://for-cast.github.io/landisutils/reference/host_cpu_info.md),
[`landis_archive_rep()`](https://for-cast.github.io/landisutils/reference/landis_archive_rep.md),
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
[`landis_version()`](https://for-cast.github.io/landisutils/reference/landis_version.md),
[`read_landis_resource_logs()`](https://for-cast.github.io/landisutils/reference/read_landis_resource_logs.md),
[`tar_landis()`](https://for-cast.github.io/landisutils/reference/tar_landis.md),
[`write_landis_scenario_file()`](https://for-cast.github.io/landisutils/reference/write_landis_scenario_file.md)
