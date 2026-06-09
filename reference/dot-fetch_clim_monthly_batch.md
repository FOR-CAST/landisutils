# BioSIM monthly + wind pull for one batch of locations, formatted to climate-only rows

Internal worker shared by
[`get_clim_monthly()`](https://for-cast.github.io/landisutils/reference/get_clim_monthly.md)
(per-study-area cache) and the global per-cell cache in
[`prep_monthly_weather_biosim()`](https://for-cast.github.io/landisutils/reference/prep_climate_data.md).
Returns ONLY climate columns keyed by `CellID` (no `EcoID`/`BatchID`):
ecoregion membership is study-area-specific and is applied later, at
assemble time.

## Usage

``` r
.fetch_clim_monthly_batch(locations, year, rcp, clim_model)
```

## Arguments

- locations:

  data.frame with `ID`, `Latitude`, `Longitude`, `Elevation` (one row
  per cell).

- year:

  integer year to fetch.

- rcp, clim_model:

  BioSIM scenario.

## Value

data.frame: `CellID`, `YEAR`, `MONTH`, the BioSIM monthly climate
columns, `WndS`, `WndD`.
