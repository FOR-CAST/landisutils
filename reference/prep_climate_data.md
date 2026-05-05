# Prepare Climate Data

Download and prepare climate data for use with LANDIS-II simulations.

## Usage

``` r
prep_daily_weather(
  vars = NULL,
  years = NULL,
  studyArea = NULL,
  id = NULL,
  batch_size = 1000,
  z = 9,
  rcp = "RCP45",
  clim_model = "RCM4"
)

prep_monthly_weather_biosim(
  vars = NULL,
  years = NULL,
  studyArea = NULL,
  id = NULL,
  batch_size = 1000,
  z = 9,
  rcp = "RCP45",
  clim_model = "RCM4"
)

prep_monthly_weather_climr(
  vars = NULL,
  years = NULL,
  studyArea = NULL,
  id = NULL,
  batch_size = 1000,
  z = 9,
  obs_ts_dataset = c("climatena", "cru.gpcc"),
  gcms = NULL,
  ssps = NULL,
  max_run = 0L
)

prep_monthly_weather(vars = NULL, years = NULL, studyArea = NULL, id = NULL)
```

## Arguments

- vars:

  character specifying the climate variables (lowercase, e.g. `"prcp"`,
  `"tmax"`, `"tmin"`).

- years:

  integer vector specifying the years.

- studyArea:

  `sf` polygons object delineating e.g., ecoregions or fire zones.

- id:

  character specifying the name of the column/field in `studyArea`
  identifying ecoregions.

- batch_size:

  integer maximum cells per BioSIM batch call. Default `1000`. Smaller
  values yield more parallel-friendly chunks.

- z:

  integer zoom level passed to
  [`elevatr::get_elev_raster()`](https://rdrr.io/pkg/elevatr/man/get_elev_raster.html)
  for the underlying elevation raster. Higher values give a finer raster
  (more point cells fed to BioSIM/climr); lower values give a coarser
  raster (fewer cells, fewer batches, faster fetches). Default `9` (~250
  m at mid-latitudes). For tests / smoke runs, values around `5`-`7`
  typically yield a single BioSIM batch over a small study area.

- rcp:

  character. BioSIM representative concentration pathway. One of
  `"CONSTANT_CLIMATE"`, `"RCP45"`, or `"RCP85"`. Default `"RCP45"`. Only
  used by the BioSIM pipelines (`prep_daily_weather()`,
  `prep_monthly_weather_biosim()`). See *Climate projections (BioSIM,
  CMIP5)*.

- clim_model:

  character. BioSIM climate model. One of `"RCM4"`, `"GCM4"`, or
  `"Hadley"`. Default `"RCM4"`. Only used by the BioSIM pipelines.

- obs_ts_dataset:

  character. climr observational time-series dataset. One of
  `"climatena"` or `"cru.gpcc"`. Default `"climatena"`. Only used by
  `prep_monthly_weather_climr()` in observational mode.

- gcms:

  character vector of CMIP6 GCM names (subset of
  [`climr::list_gcms()`](https://bcgov.github.io/climr/reference/data-option-lists.html)).
  When `NULL` (default), `prep_monthly_weather_climr()` runs in
  observational mode; when set, in projection mode. The
  bcgov-recommended eight-member ensemble is exposed as
  [`climr_ensemble_8`](https://for-cast.github.io/landisutils/reference/climr_ensemble_8.md).
  Only used by `prep_monthly_weather_climr()`.

- ssps:

  character vector of CMIP6 SSP names (subset of
  [`climr::list_ssps()`](https://bcgov.github.io/climr/reference/data-option-lists.html),
  e.g. `"ssp245"`). Required when `gcms` is set. Only used by
  `prep_monthly_weather_climr()`.

- max_run:

  integer number of ensemble runs per GCM to fetch from climr (averaged
  together when `ensemble_mean = TRUE`, the climr default). Default `0L`
  (climr's own default; uses the ensemble mean only). Only used by
  `prep_monthly_weather_climr()`.

## Value

`tbl_df` with columns `Year`, `Month`, `Variable`, and one column per
ecoregion (named by `id` values). Daily output (`prep_daily_weather()`)
additionally carries a `Day` column.

## Details

`prep_daily_weather()` retrieves daily weather from BioSIM
(`ClimaticEx_Daily` model) for cells in `studyArea`, summarizes by
ecoregion (`id`), and pivots to the LANDIS-II Climate Library wide
format. Internally it chains
[`get_elevation_rast()`](https://for-cast.github.io/landisutils/reference/get_elevation_rast.md),
[`create_locations_df()`](https://for-cast.github.io/landisutils/reference/create_locations_df.md),
[`get_clim_daily()`](https://for-cast.github.io/landisutils/reference/get_clim_daily.md)
(per batch / per year), and
[`assemble_climate_library_file()`](https://for-cast.github.io/landisutils/reference/assemble_climate_library_file.md).
Per-batch / per-year results are cached as an Arrow CSV dataset under
`getOption("landisutils.cache.path")`, partitioned by
`<rcp>_<clim_model>`/`YEAR`/`BatchID`.

`prep_monthly_weather()` retrieves monthly weather from TerraClim via
the climateR package and summarizes by zone using the zonal package.

`prep_monthly_weather_biosim()` retrieves monthly weather from BioSIM
(`Climatic_Monthly` model) using the same point-batch / Arrow caching
pipeline as `prep_daily_weather()`, so cells are co-located with the
daily workflow. Internally it chains
[`get_elevation_rast()`](https://for-cast.github.io/landisutils/reference/get_elevation_rast.md),
[`create_locations_df()`](https://for-cast.github.io/landisutils/reference/create_locations_df.md),
[`get_clim_monthly()`](https://for-cast.github.io/landisutils/reference/get_clim_monthly.md)
(per batch / per year), and
[`assemble_climate_library_file_monthly()`](https://for-cast.github.io/landisutils/reference/assemble_climate_library_file_monthly.md).
Per-batch / per-year results are cached as an Arrow CSV dataset under
`getOption("landisutils.cache.path")`, partitioned by
`<rcp>_<clim_model>`/`YEAR`/`BatchID`.

`prep_monthly_weather_climr()` retrieves monthly weather from
[climr](https://github.com/bcgov/climr), a ClimateNA-style change-factor
downscaler against a PRISM/DAYMET composite reference grid. Cells come
from the same elevation/point machinery as the BioSIM pipeline
([`get_elevation_rast()`](https://for-cast.github.io/landisutils/reference/get_elevation_rast.md),
[`create_locations_df()`](https://for-cast.github.io/landisutils/reference/create_locations_df.md));
a single
[`climr::downscale()`](https://bcgov.github.io/climr/reference/downscale.html)
call per year is dispatched via
[`furrr::future_map()`](https://furrr.futureverse.org/reference/future_map.html).
climr maintains its own on-disk cache, configured via
`options("climr.cache.path" = ...)`; the wrapper defaults this to a
`climr/` subdirectory of `getOption("landisutils.cache.path")` when the
option is unset.

## Historical daily weather (BioSIM)

BioSIM provides modelled daily North American weather. The
`ClimaticEx_Daily` model exposes columns `Prcp`, `Tmin`, `Tmax`, `Tair`,
`RelH`, `WndS`, `WndD`, `SRad`. Pass the corresponding lowercase
variable names to `vars` (e.g.
`c("prcp", "tmax", "tmin", "rh", "ws", "wnddir", "srad")`).

## Historical monthly weather (TerraClim)

Terra Climate provides monthly North American weather 1980-present
(<https://www.climatologylab.org/terraclimate.html>). TerraClim
variables: `aet`, `def`, `PDSI`, `pet`, `ppt`, `q`, `soil`, `srad`,
`swe`, `tmax`, `tmin`, `vap`, `vpd`, `ws`. Use `prep_monthly_weather()`
for TerraClim weather data.

## Historical monthly weather (BioSIM)

BioSIM's `Climatic_Monthly` model exposes columns `TotalPrcp`,
`MeanTmin`, `MeanTmax`, `MeanTair`, `MeanRelH`, `TotalRadiation` (among
others); wind variables come from a companion `ClimaticWind_Monthly`
model fetched in the same call (`WindSpeed` plus 36
directional-frequency columns `W0`..`W350`, from which a
frequency-weighted circular-mean wind from-direction is computed). Pass
the corresponding lowercase variable names to `vars` (e.g.
`c("prcp", "tmax", "tmin", "temp", "rh", "srad", "ws", "wnddir")`). Use
`prep_monthly_weather_biosim()` for BioSIM monthly weather.

## Historical monthly weather (climr)

climr provides ClimateNA-style change-factor downscaling against a
PRISM/DAYMET composite reference grid for North America, with
observational time series spanning 1901-2024
(`obs_ts_dataset = "climatena"` or `"cru.gpcc"`). Supported lowercase
`vars`: `prcp`, `tmax`, `tmin`, `temp`, `rh`, `srad`, `cmd` (climatic
moisture deficit). Wind variables (`ws`, `wnddir`) are **not** provided
by climr - use `prep_monthly_weather_biosim()` for wind. Use
`prep_monthly_weather_climr()` for climr monthly weather.

## Climate projections (BioSIM, CMIP5)

`prep_daily_weather()` and `prep_monthly_weather_biosim()` accept `rcp`
and `clim_model` arguments that are forwarded to BioSIM. `rcp` is one of
`"CONSTANT_CLIMATE"` (1991-2020 climate replayed), `"RCP45"`, or
`"RCP85"`. `clim_model` is one of `"RCM4"` (Canadian Regional Climate
Model 4, CanESM2-driven), `"GCM4"` (CGCM4 / CanESM2-family GCM), or
`"Hadley"` (HadGEM). For years within BioSIM's observational range these
arguments have no effect on the returned data; they always namespace the
on-disk cache (`<rcp>_<clim_model>` subdirectory under
`<studyArea_hash>/`).

## Climate projections (climr, CMIP6)

`prep_monthly_weather_climr()` accepts `gcms`, `ssps`, and `max_run`
arguments forwarded to
[`climr::downscale()`](https://bcgov.github.io/climr/reference/downscale.html)
for CMIP6 GCM projections. When `gcms` is `NULL` the function runs in
observational mode; otherwise it runs in projection mode using
`gcm_ssp_years = years`. The bcgov-recommended eight-member ensemble
subset is exposed as the package constant
[`climr_ensemble_8`](https://for-cast.github.io/landisutils/reference/climr_ensemble_8.md);
pass `gcms = climr_ensemble_8` to use it.
[`assemble_climate_library_file_monthly_climr()`](https://for-cast.github.io/landisutils/reference/assemble_climate_library_file_monthly_climr.md)
averages over GCMs and SSPs to give a single LANDIS-II-compatible value
per `(Year, Month, EcoID, Variable)`. See
[`climr::list_gcms()`](https://bcgov.github.io/climr/reference/data-option-lists.html),
[`climr::list_ssps()`](https://bcgov.github.io/climr/reference/data-option-lists.html)
for the full choice sets.

## Rate-limiting

BioSIM is a remote web service. When parallelizing over batches/years
(e.g. with `future::plan(multisession)`), keep the worker count modest
(\\\le\\ 4) to avoid hammering the service.

## Caching

Per-batch / per-year BioSIM fetches are cached as Arrow CSV partitions
under the directory given by `getOption("landisutils.cache.path")`.
Re-runs that find an existing partition skip the BioSIM call.
`prep_monthly_weather_climr()` relies on climr's own on-disk cache
instead; the cache root is `getOption("climr.cache.path")`, defaulting
(for the duration of the call) to
`file.path(getOption("landisutils.cache.path"), "climr")` when unset.

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

## Examples

``` r
if (requireNamespace("BioSIM", quietly = TRUE) &&
  requireNamespace("elevatr", quietly = TRUE) &&
  requireNamespace("arrow", quietly = TRUE)) {
  # ecoregionPolys <- landisutils::test_ecoregionPolys
  # clim_years <- 2018:2019
  # daily_weather <- prep_daily_weather(
  #   vars = c("prcp", "tmax", "tmin"),
  #   years = clim_years,
  #   studyArea = ecoregionPolys,
  #   id = "PolyID"
  # )
  # head(daily_weather)
}
#> NULL
```
