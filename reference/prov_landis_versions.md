# LANDIS-II console and extension versions (provenance)

Parses the first replicate's `Landis-log.txt` under `run_dir` via
[`parse_landis_log_versions()`](https://for-cast.github.io/landisutils/reference/parse_landis_log_versions.md)
and formats a compact `data.frame` for a build-provenance report
appendix.

## Usage

``` r
prov_landis_versions(run_dir)
```

## Arguments

- run_dir:

  Character path to a scenario run directory.

## Value

A two-column `data.frame` (`Component`, `Version(s)`), or `NULL` when no
`Landis-log.txt` is found under `run_dir`.

## See also

Other provenance helpers:
[`landis_image_info()`](https://for-cast.github.io/landisutils/reference/landis_image_info.md),
[`parse_landis_log_versions()`](https://for-cast.github.io/landisutils/reference/parse_landis_log_versions.md),
[`prov_landis_container()`](https://for-cast.github.io/landisutils/reference/prov_landis_container.md),
[`prov_run_resources()`](https://for-cast.github.io/landisutils/reference/prov_run_resources.md),
[`prov_stochasticity()`](https://for-cast.github.io/landisutils/reference/prov_stochasticity.md)
