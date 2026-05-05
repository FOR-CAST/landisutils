# Fetch a single year of monthly BioSIM weather for one location batch

Wraps
[`BioSIM::generateWeather()`](https://rdrr.io/pkg/BioSIM/man/generateWeather.html)
for the `Climatic_Monthly` and `ClimaticWind_Monthly` models, fetching
one calendar year for the cells in `locations_batch[[1]]`. Wind
direction is summarised from the model's 36 directional-frequency
columns (`W0`, `W10`, ..., `W350`) into a single weighted circular mean
(`WndD`, degrees from-direction). Output is appended to an Arrow CSV
dataset under
`path/Climatic_Monthly/<studyArea_hash>/<rcp>_<clim_model>/`,
partitioned by `YEAR`/`BatchID`. The function is intended to be run as a
dynamic target across many `(batch, year)` combinations; existing
partition files are detected via
[`file.exists()`](https://rdrr.io/r/base/files.html) and skipped, so
re-runs are cheap.

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

  list whose first element is a data frame with columns `ID`,
  `Latitude`, `Longitude`, `Elevation`, `EcoID`, `BatchID`. Typically
  one element of
  [`create_locations_df()`](https://for-cast.github.io/landisutils/reference/create_locations_df.md)'s
  output, wrapped in a list.

- year:

  integer single calendar year to fetch.

- studyArea_hash:

  character. Short hash of the study-area object (see
  [`.studyArea_hash()`](https://for-cast.github.io/landisutils/reference/dot-studyArea_hash.md))
  used as a cache subdirectory so that distinct study areas don't
  collide.

- path:

  character. Directory under which the `Climatic_Monthly/` arrow dataset
  is written. Default uses the package climate cache
  (`getOption("landisutils.cache.path")`).

- rcp:

  character. BioSIM representative concentration pathway. One of
  `"CONSTANT_CLIMATE"`, `"RCP45"`, `"RCP85"`. Default `"RCP45"`.

- clim_model:

  character. BioSIM climate model. One of `"RCM4"`, `"GCM4"`,
  `"Hadley"`. Default `"RCM4"`.

## Value

Path (character) to the partition CSV for `(year, BatchID)`.

## Details

For years within BioSIM's observational range, `rcp` and `clim_model`
have no effect on the returned data, but they do still namespace the
cache. For future years they select the CMIP5-era projection (see
[`get_clim_daily()`](https://for-cast.github.io/landisutils/reference/get_clim_daily.md)
for the choice sets).

Rate-limiting against the BioSIM web service is the **caller's**
responsibility - cap parallel workers (e.g.
`future::plan(multisession, workers = 4)`) or stagger orchestrator
calls. This function performs no internal throttling.

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

[`get_elevation_rast()`](https://for-cast.github.io/landisutils/reference/get_elevation_rast.md),
[`create_locations_df()`](https://for-cast.github.io/landisutils/reference/create_locations_df.md),
[`assemble_climate_library_file_monthly()`](https://for-cast.github.io/landisutils/reference/assemble_climate_library_file_monthly.md)
