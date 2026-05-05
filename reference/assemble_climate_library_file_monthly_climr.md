# Assemble LANDIS-II Climate Library wide-format table from a climr Arrow dataset

Reads the partitioned Arrow CSV dataset written by
[`get_clim_monthly_climr()`](https://for-cast.github.io/landisutils/reference/get_clim_monthly_climr.md),
joins per-cell rows to ecoregion ids via `eco_lookup`, summarises by
`(Year, Month, EcoID, Variable)` with `mean(..., na.rm = TRUE)`, applies
unit conversions to match LANDIS-II conventions, and pivots to wide
format keyed by ecoregion id.

## Usage

``` r
assemble_climate_library_file_monthly_climr(dataset_path, vars, eco_lookup)
```

## Arguments

- dataset_path:

  character. Path to a scenario-tagged `Climr_Monthly` Arrow dataset
  directory (containing `Variable=...`/`Year=...`/... partitions).
  Typically `<cache_root>/Climr_Monthly/<sa_hash>/<scenario_tag>`.

- vars:

  character vector of lowercase variable names (e.g.
  `c("prcp", "tmax", "tmin")`); same set accepted by
  [`prep_monthly_weather_climr()`](https://for-cast.github.io/landisutils/reference/prep_climate_data.md).

- eco_lookup:

  `data.frame` with columns `id` (cell id, matching the `id` column in
  the cached partitions) and `EcoID` (ecoregion id from the study-area
  `id` field). Typically built by `.climr_build_xyz()` (internal).

## Value

`tbl_df` with columns `Year`, `Month`, `Variable`, and one column per
ecoregion id.

## Details

For projection-mode partitions (which carry extra `GCM` and `SSP`
columns), the same group-and-mean step collapses across GCMs/SSPs/cells,
yielding a single multi-GCM ensemble-mean value per
`(Year, Month, EcoID, Variable)` as expected by the LANDIS-II Climate
Library.

Unit conversions applied:

- `PPT` (mm) \\\to\\ cm (\\\div 10\\)

## See also

[`get_clim_monthly_climr()`](https://for-cast.github.io/landisutils/reference/get_clim_monthly_climr.md),
[`prep_monthly_weather_climr()`](https://for-cast.github.io/landisutils/reference/prep_climate_data.md),
[`writeClimateData()`](https://for-cast.github.io/landisutils/reference/writeClimateData.md)
