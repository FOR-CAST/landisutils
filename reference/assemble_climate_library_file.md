# Assemble LANDIS-II Climate Library wide-format table from a BioSIM Arrow dataset

Reads the partitioned Arrow CSV dataset written by
[`get_clim_daily()`](https://for-cast.github.io/landisutils/reference/get_clim_daily.md),
summarizes the requested variables by `(Year, Month, Day, EcoID)`,
applies unit conversions to match LANDIS-II conventions, and pivots to
the wide format ingested by `LandisClimateConfig`. The result is the
same shape that
[`prep_daily_weather()`](https://for-cast.github.io/landisutils/reference/prep_climate_data.md)
returns and is suitable for
[`writeClimateData()`](https://for-cast.github.io/landisutils/reference/writeClimateData.md).

## Usage

``` r
assemble_climate_library_file(dataset_path, vars, id_col = "EcoID")
```

## Arguments

- dataset_path:

  character. Path to the `ClimaticEx_Daily` Arrow dataset directory
  (containing `YEAR=...`/`BatchID=...`/`part-0.csv` partitions).

- vars:

  character vector of BioSIM column names to retain (e.g.
  `c("Prcp", "Tmin", "Tmax")`).

- id_col:

  character. Name of the ecoregion-id column in the dataset. Default
  `"EcoID"`.

## Value

`tbl_df` with columns `Year`, `Month`, `Day`, `Variable`, and one column
per ecoregion id.

## Details

Unit conversions applied:

- `Prcp` (mm) \\\to\\ cm (\\\div 10\\)

- `WndS` (km/h) \\\to\\ m/s (\\\div 3.6\\)

BioSIM `WndD` is reported as the wind *from-direction* (degrees), which
is what the LANDIS-II Climate Library expects.

## Topographic considerations

The climate data sources used by this package (BioSIM, Daymet,
TerraClim) are **not PRISM-derived**. PRISM
(<https://prism.oregonstate.edu/>) applies topographically-aware
interpolation (slope, aspect, elevation, coastal proximity, temperature
inversions) when downscaling station observations to a grid (Daly et al.
2008); the sources wrapped here use simpler interpolation schemes that
can produce **large discrepancies in areas of complex topography**
(mountainous terrain, steep elevation gradients, rain shadows):
inter-product differences of 5-60% in annual precipitation (Henn et al.
2018) and \>6 \\^\circ\\C in temperature (Walton & Hall 2018) have been
documented in the western US. Carefully evaluate the suitability of the
chosen source for your study area, especially in topographically
heterogeneous landscapes.

## References

Daly, C., Halbleib, M., Smith, J.I., Gibson, W.P., Doggett, M.K.,
Taylor, G.H., Curtis, J., & Pasteris, P.P. (2008). Physiographically
sensitive mapping of climatological temperature and precipitation across
the conterminous United States. *International Journal of Climatology*,
28(15), 2031-2064.
[doi:10.1002/joc.1688](https://doi.org/10.1002/joc.1688)

Henn, B., Newman, A.J., Livneh, B., Daly, C., & Lundquist, J.D. (2018).
An assessment of differences in gridded precipitation datasets in
complex terrain. *Journal of Hydrology*, 556, 1205-1219.
[doi:10.1016/j.jhydrol.2017.03.008](https://doi.org/10.1016/j.jhydrol.2017.03.008)

## See also

[`get_clim_daily()`](https://for-cast.github.io/landisutils/reference/get_clim_daily.md),
[`prep_daily_weather()`](https://for-cast.github.io/landisutils/reference/prep_climate_data.md),
[`writeClimateData()`](https://for-cast.github.io/landisutils/reference/writeClimateData.md)
