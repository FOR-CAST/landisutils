# Fetch + cache one year of monthly BioSIM weather for one location batch (per-study-area store)

Wraps
[`.fetch_clim_monthly_batch()`](https://for-cast.github.io/landisutils/reference/dot-fetch_clim_monthly_batch.md)
and writes the result to an Arrow CSV dataset under
`path/Climatic_Monthly/<studyArea_hash>/<rcp>_<clim_model>/`,
partitioned by `YEAR`/`BatchID`; existing partitions are detected via
[`file.exists()`](https://rdrr.io/r/base/files.html) and skipped, so
re-runs are cheap. For the SHARED global-id store (cross-study-area
reuse) use
[`prep_monthly_weather_biosim()`](https://for-cast.github.io/landisutils/reference/prep_climate_data.md)'s
`ref_grid` path instead.

## Usage

``` r
get_clim_monthly(
  locations_batch,
  year,
  studyArea_hash,
  path = .climateCachePath(),
  rcp = "RCP45",
  clim_model = "RCM4"
)
```

## Arguments

- locations_batch:

  list whose first element is a data frame with `ID`, `Latitude`,
  `Longitude`, `Elevation`, `EcoID`, `BatchID` (one element of
  [`create_locations_df()`](https://for-cast.github.io/landisutils/reference/create_locations_df.md)'s
  output, wrapped in a list).

- year:

  integer single calendar year to fetch.

- studyArea_hash:

  character. Short study-area hash used as a cache subdirectory.

- path:

  character. Directory for the `Climatic_Monthly/` dataset (default:
  package climate cache).

- rcp, clim_model:

  BioSIM scenario (see
  [`get_clim_daily()`](https://for-cast.github.io/landisutils/reference/get_clim_daily.md)
  for the choice sets).

## Value

Path (character) to the partition CSV for `(year, BatchID)`.

## See also

[`create_locations_df()`](https://for-cast.github.io/landisutils/reference/create_locations_df.md),
[`assemble_climate_library_file_monthly()`](https://for-cast.github.io/landisutils/reference/assemble_climate_library_file_monthly.md)
