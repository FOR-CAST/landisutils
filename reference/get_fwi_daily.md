# Fetch a single year of daily BioSIM Fire Weather Index for one location batch

Wraps
[`BioSIM::generateWeather()`](https://rdrr.io/pkg/BioSIM/man/generateWeather.html)
for the `FWI_Daily` model, fetching one calendar year for the cells in
`locations_batch[[1]]`. Output is appended to an Arrow CSV dataset under
`path/FWI_Daily/`, partitioned by `YEAR`/`BatchID`. Like
[`get_clim_daily()`](https://for-cast.github.io/landisutils/reference/get_clim_daily.md),
the function is intended to be run as a dynamic target across many
`(batch, year)` combinations; existing partition files are detected via
[`file.exists()`](https://rdrr.io/r/base/files.html) and skipped, so
re-runs are cheap.

## Usage

``` r
get_fwi_daily(
  locations_batch,
  year,
  studyArea_hash,
  params = NULL,
  path = .climateCachePath()
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

- params:

  list of additional parameters forwarded to BioSIM as
  `additionalParms = list(FWI_Daily = params)`. Default `NULL`.

- path:

  character. Directory under which the `FWI_Daily/` arrow dataset is
  written. Default uses the package climate cache
  (`getOption("landisutils.cache.path")`).

## Value

Path (character) to the partition CSV for `(year, BatchID)`.

## Details

BioSIM's `FWI_Daily` model is known to occasionally produce implausibly
large `FFMC`, `ISI`, and `FWI` values (see
<https://github.com/RNCan/BioSimClient_R/issues/14>). To work around
this, `FFMC` values \\\>\\ 101 are recomputed from the underlying
`T`/`RH`/`WS`/`Prcp` columns using cffdrs, and `ISI` and `FWI` are then
recomputed from the corrected `FFMC`. The unused `DSR` column is
dropped.

Rate-limiting against the BioSIM web service is the **caller's**
responsibility - cap parallel workers (e.g.
`future::plan(multisession, workers = 4)`) or stagger orchestrator
calls. A small random delay (5-30 s) is applied per call to spread
BioSIM hits when many parallel workers start at once.

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
[`get_clim_daily()`](https://for-cast.github.io/landisutils/reference/get_clim_daily.md)
