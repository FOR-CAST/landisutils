# Fetch a single year-variable of monthly TerraClim weather

Wraps
[`climateR::getTerraClim()`](https://rdrr.io/pkg/climateR/man/getTerraClim.html)
for one (variable, year), summarises by ecoregion via
[`zonal::execute_zonal()`](https://rdrr.io/pkg/zonal/man/execute_zonal.html),
and writes the long-format result as an Arrow CSV partition under
`path/TerraClim_Monthly/`, partitioned by `Variable`/`YEAR`. Existing
partitions are skipped via
[`file.exists()`](https://rdrr.io/r/base/files.html), so re-runs are
cheap. Unit conversions (e.g. mm -\> cm for precipitation) and
translation to LANDIS-II Climate Library variable names are deferred to
[`assemble_climate_library_file_monthly_terraclim()`](https://for-cast.github.io/landisutils/reference/assemble_climate_library_file_monthly_terraclim.md).

## Usage

``` r
get_clim_monthly_terraclim(
  var,
  year,
  studyArea,
  id,
  studyArea_hash = .studyArea_hash(studyArea),
  path = .climateCachePath()
)
```

## Arguments

- var:

  character single TerraClim variable name (lowercase, e.g. `"ppt"`,
  `"tmax"`, `"tmin"`, `"aet"`).

- year:

  integer single calendar year to fetch.

- studyArea:

  `sf` polygons object delineating ecoregions.

- id:

  character. Name of the polygon-id column in `studyArea`.

- studyArea_hash:

  character or `NULL`. Short hash of the study-area object used as a
  cache subdirectory so that distinct study areas don't collide.
  Defaults to
  [`.studyArea_hash()`](https://for-cast.github.io/landisutils/reference/dot-studyArea_hash.md)
  of `studyArea`.

- path:

  character. Directory under which the `TerraClim_Monthly/` arrow
  dataset is written. Default uses the package climate cache
  (`getOption("landisutils.cache.path")`).

## Value

Path (character) to the partition CSV for `(var, year)`.

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

[`assemble_climate_library_file_monthly_terraclim()`](https://for-cast.github.io/landisutils/reference/assemble_climate_library_file_monthly_terraclim.md),
[`prep_monthly_weather()`](https://for-cast.github.io/landisutils/reference/prep_climate_data.md)
