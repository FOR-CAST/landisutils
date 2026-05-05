# Fetch one year of monthly climr weather and write Arrow CSV partitions

Wraps
[`climr::downscale()`](https://bcgov.github.io/climr/reference/downscale.html)
for one calendar `year` against the points in `xyz`, then splits the
long-format output by base variable and writes Arrow CSV partitions
under `path/Climr_Monthly/<studyArea_hash>/<scenario_tag>/`. Partitions
are keyed by `Variable`/`Year` in observational mode and additionally by
`GCM`/`SSP` in projection mode. Existing per-variable / per-year
partition trees are detected via
[`dir.exists()`](https://rdrr.io/r/base/files2.html) and skipped, so
re-runs are cheap.

## Usage

``` r
get_clim_monthly_climr(
  year,
  xyz,
  vars,
  studyArea_hash,
  obs_ts_dataset = "climatena",
  path = .climateCachePath(),
  gcms = NULL,
  ssps = NULL,
  max_run = 0L
)
```

## Arguments

- year:

  integer single calendar year to fetch. In projection mode, must lie
  within
  [`climr::list_gcm_ssp_years()`](https://bcgov.github.io/climr/reference/data-option-lists.html).

- xyz:

  `data.frame` with columns `lon`, `lat`, `elev`, `id` (one row per
  cell). See `.climr_build_xyz()` (internal) or build manually.

- vars:

  character vector of lowercase variable names (e.g.
  `c("prcp", "tmax", "tmin")`); see
  [`prep_monthly_weather_climr()`](https://for-cast.github.io/landisutils/reference/prep_climate_data.md)
  for the supported set.

- studyArea_hash:

  character. Short hash of the study-area object (see
  [`.studyArea_hash()`](https://for-cast.github.io/landisutils/reference/dot-studyArea_hash.md))
  used as a cache subdirectory so that distinct study areas don't
  collide.

- obs_ts_dataset:

  character. climr observational time-series dataset. One of
  `"climatena"` or `"cru.gpcc"`. Default `"climatena"`. Ignored when
  `gcms` is set.

- path:

  character. Directory under which the `Climr_Monthly/` arrow dataset is
  written. Default uses the package climate cache
  (`getOption("landisutils.cache.path")`).

- gcms:

  character vector of CMIP6 GCM names (subset of
  [`climr::list_gcms()`](https://bcgov.github.io/climr/reference/data-option-lists.html)).
  When `NULL` (default), runs in observational mode. When set, runs in
  projection mode and `ssps` is required.

- ssps:

  character vector of CMIP6 SSP names (subset of
  [`climr::list_ssps()`](https://bcgov.github.io/climr/reference/data-option-lists.html),
  e.g. `"ssp245"`). Required when `gcms` is set.

- max_run:

  integer number of ensemble runs per GCM to fetch. With
  `ensemble_mean = TRUE` (climr's default and what we use),
  `max_run = 0L` returns only the per-GCM ensemble mean. Default `0L`.

## Value

Path (character) to the scenario-tagged dataset directory (i.e. the
directory containing the `Variable=...` partitions).

## Details

The function runs in two modes:

- Observational (default):

  `gcms = NULL`. Uses `obs_years = year` and `obs_ts_dataset` for
  ClimateNA-style historical anomalies.

- Projection:

  `gcms` set (e.g.
  [`climr_ensemble_8`](https://for-cast.github.io/landisutils/reference/climr_ensemble_8.md))
  and `ssps` set (e.g. `"ssp245"`). Uses `gcm_ssp_years = year` to fetch
  CMIP6 GCM time-series projections; `max_run` controls how many runs
  per GCM are averaged (with `ensemble_mean = TRUE`, climr's default).

Per the climr API, the climr cache itself is configured by setting
`options("climr.cache.path" = ...)` (climr GitHub issue 274). The
orchestrator
[`prep_monthly_weather_climr()`](https://for-cast.github.io/landisutils/reference/prep_climate_data.md)
sets a sensible default via
[`withr::local_options()`](https://withr.r-lib.org/reference/with_options.html)
for the duration of a call; if you call this helper directly you should
do the same.

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

[`assemble_climate_library_file_monthly_climr()`](https://for-cast.github.io/landisutils/reference/assemble_climate_library_file_monthly_climr.md),
[`prep_monthly_weather_climr()`](https://for-cast.github.io/landisutils/reference/prep_climate_data.md),
[`climr_ensemble_8`](https://for-cast.github.io/landisutils/reference/climr_ensemble_8.md)
