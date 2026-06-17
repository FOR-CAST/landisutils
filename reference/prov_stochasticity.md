# LANDIS-II stochasticity summary (provenance)

Reports the base random number seed (from the first replicate's
`Landis-log.txt`) and the replicate count (`rep*` subdirectories) under
`run_dir`, formatted for a build-provenance report appendix.

## Usage

``` r
prov_stochasticity(run_dir)
```

## Arguments

- run_dir:

  Character path to a scenario run directory.

## Value

A two-column `data.frame` (`Field`, `Value`).

## See also

Other provenance helpers:
[`landis_image_info()`](https://for-cast.github.io/landisutils/reference/landis_image_info.md),
[`parse_landis_log_versions()`](https://for-cast.github.io/landisutils/reference/parse_landis_log_versions.md),
[`prov_landis_container()`](https://for-cast.github.io/landisutils/reference/prov_landis_container.md),
[`prov_landis_versions()`](https://for-cast.github.io/landisutils/reference/prov_landis_versions.md),
[`prov_run_resources()`](https://for-cast.github.io/landisutils/reference/prov_run_resources.md)
