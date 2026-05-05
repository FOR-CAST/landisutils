# Assemble LANDIS-II Climate Library wide-format table from a monthly BioSIM Arrow dataset

Reads the partitioned Arrow CSV dataset written by
[`get_clim_monthly()`](https://for-cast.github.io/landisutils/reference/get_clim_monthly.md),
summarizes the requested variables by `(Year, Month, EcoID)`, applies
unit conversions to match LANDIS-II conventions, and pivots to the wide
format ingested by `LandisClimateConfig`. The result has the same shape
that
[`prep_monthly_weather()`](https://for-cast.github.io/landisutils/reference/prep_climate_data.md)
returns and is suitable for
[`writeClimateData()`](https://for-cast.github.io/landisutils/reference/writeClimateData.md).

## Usage

``` r
assemble_climate_library_file_monthly(dataset_path, vars, id_col = "EcoID")
```

## Arguments

- dataset_path:

  character. Path to the `Climatic_Monthly` Arrow dataset directory
  (containing `YEAR=...`/`BatchID=...`/`part-0.csv` partitions).

- vars:

  character vector of BioSIM column names to retain (e.g.
  `c("TotalPrcp", "MeanTmin", "MeanTmax", "WndS", "WndD")`).

- id_col:

  character. Name of the ecoregion-id column in the dataset. Default
  `"EcoID"`.

## Value

`tbl_df` with columns `Year`, `Month`, `Variable`, and one column per
ecoregion id.

## Details

Unit conversions applied:

- `TotalPrcp` (mm) \\\to\\ cm (\\\div 10\\)

- `WndS` (km/h) \\\to\\ m/s (\\\div 3.6\\)

BioSIM `WndD` is reported as the wind *from-direction* (degrees), which
is what the LANDIS-II Climate Library expects. The monthly value here is
a frequency-weighted circular mean across the model's 36 direction bins
(see
[`get_clim_monthly()`](https://for-cast.github.io/landisutils/reference/get_clim_monthly.md));
when summarising across cells within an ecoregion the same
arithmetic-mean convention as the daily pipeline is applied.

## See also

[`get_clim_monthly()`](https://for-cast.github.io/landisutils/reference/get_clim_monthly.md),
[`prep_monthly_weather_biosim()`](https://for-cast.github.io/landisutils/reference/prep_climate_data.md),
[`writeClimateData()`](https://for-cast.github.io/landisutils/reference/writeClimateData.md)
