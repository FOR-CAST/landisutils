# LANDIS-II per-replicate resource use (provenance)

Summarises per-replicate elapsed time and peak memory (mean +/- SD
across the replicates of a single scenario) from the resource logs read
by
[`read_landis_resource_logs()`](https://for-cast.github.io/landisutils/reference/read_landis_resource_logs.md),
formatted for a build-provenance report appendix.

## Usage

``` r
prov_run_resources(run_dir)
```

## Arguments

- run_dir:

  Character path to a scenario run directory.

## Value

A three-column `data.frame` (`Metric`, `Mean`, `SD`) with an `n_reps`
attribute, or `NULL` when no resource logs are present (e.g. the run is
still in flight).

## See also

Other provenance helpers:
[`landis_image_info()`](https://for-cast.github.io/landisutils/reference/landis_image_info.md),
[`parse_landis_log_versions()`](https://for-cast.github.io/landisutils/reference/parse_landis_log_versions.md),
[`prov_landis_container()`](https://for-cast.github.io/landisutils/reference/prov_landis_container.md),
[`prov_landis_versions()`](https://for-cast.github.io/landisutils/reference/prov_landis_versions.md),
[`prov_stochasticity()`](https://for-cast.github.io/landisutils/reference/prov_stochasticity.md)
