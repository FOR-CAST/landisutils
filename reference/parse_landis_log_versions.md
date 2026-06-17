# Parse a LANDIS-II `Landis-log.txt` for version metadata

Extracts the console banner, user-supplied random number seed, and the
succession / disturbance / other extension version blocks from the
header of a LANDIS-II log file. The log format is identical across
scenarios (the LANDIS-II console writes the same banner on every run),
so only the first ~30 lines are read.

## Usage

``` r
parse_landis_log_versions(path)
```

## Arguments

- path:

  Character path to a `Landis-log.txt`.

## Value

A named list with elements:

- `console`:

  Console version string (e.g. `"LANDIS-II 8.0 (8)"`), or
  `NA_character_`.

- `seed`:

  Numeric user-supplied seed, or `NA_real_`.

- `succession`:

  Character vector of `"Name vX.Y.Z"` for the succession extension
  (length 0 when absent).

- `disturbance`:

  As `succession`, for disturbance extensions.

- `other`:

  As `succession`, for other extensions.

When `path` does not exist, every element takes its empty default.

## See also

Other provenance helpers:
[`landis_image_info()`](https://for-cast.github.io/landisutils/reference/landis_image_info.md),
[`prov_landis_container()`](https://for-cast.github.io/landisutils/reference/prov_landis_container.md),
[`prov_landis_versions()`](https://for-cast.github.io/landisutils/reference/prov_landis_versions.md),
[`prov_run_resources()`](https://for-cast.github.io/landisutils/reference/prov_run_resources.md),
[`prov_stochasticity()`](https://for-cast.github.io/landisutils/reference/prov_stochasticity.md)
