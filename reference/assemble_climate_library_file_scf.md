# Assemble Social-Climate-Fire-shaped daily climate CSV from a BioSIM Arrow dataset

A thin wrapper around
[`assemble_climate_library_file()`](https://for-cast.github.io/landisutils/reference/assemble_climate_library_file.md)
that rewrites the output `Variable` column from LANDIS-II Climate
Library spellings (`Tmin`, `Tmax`, `windSpeed`, `windDirection`, `RH`,
`SWR`) to the lowercase Social-Climate-Fire spellings (`mintemp`,
`maxtemp`, `windspeed`, `winddirection`, `rh`, `swr`). The resulting
`tbl_df` - and the CSV it produces via
[`writeClimateData()`](https://for-cast.github.io/landisutils/reference/writeClimateData.md) -
is analogous to the LANDIS-II Social-Climate-Fire v4 reference input
[`LTB_ClimateInputs_91_10_v2.csv`](https://github.com/LANDIS-II-Foundation/Extension-Social-Climate-Fire/blob/master/Testing/Core8-SocialClimateFire4.0/LTB_ClimateInputs_91_10_v2.csv).

## Usage

``` r
assemble_climate_library_file_scf(dataset_path, vars, id_col = "EcoID")
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

`tbl_df` with the same shape as
[`assemble_climate_library_file()`](https://for-cast.github.io/landisutils/reference/assemble_climate_library_file.md)
but `Variable` values rewritten to the lowercase Social-Climate-Fire
convention (`precip`, `mintemp`, `maxtemp`, `temp`, `rh`, `windspeed`,
`winddirection`, `swr`).

## Details

Social Climate Fire reads daily fire-weather indirectly through the
LANDIS-II Climate Library (`Daily_RandomYears` / `Daily_AverageAllYears`
time series), which then computes FWI internally. The Climate Library
parser is case-insensitive and accepts both naming conventions, so this
helper exists to produce a CSV whose `Variable` column visually matches
the Social-Climate-Fire reference;
[`assemble_climate_library_file()`](https://for-cast.github.io/landisutils/reference/assemble_climate_library_file.md)
(which keeps Climate-Library-style spellings) is equally valid input to
the parser.

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

[`assemble_climate_library_file()`](https://for-cast.github.io/landisutils/reference/assemble_climate_library_file.md),
[`get_clim_daily()`](https://for-cast.github.io/landisutils/reference/get_clim_daily.md),
[`writeClimateData()`](https://for-cast.github.io/landisutils/reference/writeClimateData.md)

Other Social Climate Fire helpers:
[`SocialClimateFire`](https://for-cast.github.io/landisutils/reference/SocialClimateFire.md),
[`insertDeadWoodTable()`](https://for-cast.github.io/landisutils/reference/insertDeadWoodTable.md),
[`insertLadderFuelSpeciesList()`](https://for-cast.github.io/landisutils/reference/insertLadderFuelSpeciesList.md),
[`prepSuppression_CSV_File()`](https://for-cast.github.io/landisutils/reference/prepSuppression_CSV_File.md),
[`prepTopographyFile()`](https://for-cast.github.io/landisutils/reference/prepTopographyFile.md)
