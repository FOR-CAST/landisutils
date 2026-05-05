# Preparing Climate Data for Use With LANDIS-II

``` r

library(landisutils)
```

## Define a ‘study area’ or ‘area of interest’

``` r

## random study area in BC using BEC zones as ecoregions
ecoregionPolys <- landisutils::test_ecoregionPolys

plot(ecoregionPolys["PolyID"])
```

## Historical weather data

``` r

clim_years <- 2018:2019 ## availability is 1980 to last year

clim_data_path <- withr::local_tempdir("climate_data_")
```

### Daily Climate Data (BioSIM)

Daily weather is fetched from BioSIM using the `ClimaticEx_Daily` model.
The high-level wrapper
[`prep_daily_weather()`](https://for-cast.github.io/landisutils/reference/prep_climate_data.md)
takes a study-area polygons object and a column name identifying
ecoregions:

``` r

climvars_daily <- c("prcp", "tmax", "tmin")

daily_weather <- prep_daily_weather(
  vars = climvars_daily,
  years = clim_years,
  studyArea = ecoregionPolys,
  id = "PolyID"
)

head(daily_weather)

clim_file <- file.path(clim_data_path, "climate-data-daily.csv")
writeClimateData(daily_weather, clim_file)
```

Internally,
[`prep_daily_weather()`](https://for-cast.github.io/landisutils/reference/prep_climate_data.md)
chains four lower-level helpers that you can call directly when you need
finer control (e.g. to schedule per-batch fetches as dynamic targets):

``` r

## 1. Build an elevation raster covering the study area (lon-lat).
elev <- get_elevation_rast(ecoregionPolys, z = 9)

## 2. Tile cells of `elev` that fall inside `ecoregionPolys` into batches.
##    Each row carries Longitude/Latitude/Elevation, plus EcoID (from the
##    studyArea field named by `id`) and BatchID (for parallel dispatch).
locations_batch <- create_locations_df(
  elev = elev,
  studyArea = ecoregionPolys,
  id = "PolyID",
  batch_size = 500
)

## 3. Fetch one (batch, year) at a time. Cap parallelism to <= 4 workers
##    so we don't overload the BioSIM web service. The cache is namespaced
##    by a short hash of `studyArea` so distinct study areas don't collide.
future::plan(future::multisession, workers = 2)

sa_hash <- landisutils:::.studyArea_hash(ecoregionPolys)

grid <- expand.grid(batch = seq_along(locations_batch), year = clim_years)
furrr::future_map2(grid$batch, grid$year, function(b, y) {
  get_clim_daily(
    locations_batch[b], year = y,
    studyArea_hash = sa_hash, path = clim_data_path
  )
})

## 4. Read the partitioned Arrow CSV dataset, summarize by ecoregion,
##    and pivot to LANDIS-II Climate Library wide format.
daily_weather <- assemble_climate_library_file(
  dataset_path = file.path(clim_data_path, "ClimaticEx_Daily", sa_hash),
  vars = c("Prcp", "Tmin", "Tmax"),
  id_col = "EcoID"
)
```

#### Daily climate inputs for Social Climate Fire

The CSV that
[`prep_daily_weather()`](https://for-cast.github.io/landisutils/reference/prep_climate_data.md)
/
[`assemble_climate_library_file()`](https://for-cast.github.io/landisutils/reference/assemble_climate_library_file.md)
produces is consumed by the LANDIS-II Climate Library
(`Daily_RandomYears` / `Daily_AverageAllYears` time series). The [Social
Climate
Fire](https://github.com/LANDIS-II-Foundation/Extension-Social-Climate-Fire)
extension reads daily fire-weather indirectly through the Climate
Library and computes FWI internally.
[`assemble_climate_library_file_scf()`](https://for-cast.github.io/landisutils/reference/assemble_climate_library_file_scf.md)
is a thin wrapper that emits the same table with the lowercase
`Variable` spellings used by the Social-Climate-Fire reference input
([`LTB_ClimateInputs_91_10_v2.csv`](https://github.com/LANDIS-II-Foundation/Extension-Social-Climate-Fire/blob/master/Testing/Core8-SocialClimateFire4.0/LTB_ClimateInputs_91_10_v2.csv)):
`precip`, `maxtemp`, `mintemp`, `windspeed`, `winddirection`. The
Climate Library parser is case-insensitive, so either spelling works as
input — pick whichever your scenario template expects.

``` r

## populate the BioSIM cache with the 5 SCF variables
prep_daily_weather(
  vars = c("prcp", "tmax", "tmin", "ws", "wnddir"),
  years = clim_years,
  studyArea = ecoregionPolys,
  id = "PolyID"
)

sa_hash <- landisutils:::.studyArea_hash(ecoregionPolys)
scenario_tag <- landisutils:::.biosim_scenario_tag("RCP45", "RCM4")

scf_inputs <- assemble_climate_library_file_scf(
  dataset_path = file.path(
    clim_data_path, "ClimaticEx_Daily", sa_hash, scenario_tag
  ),
  vars = c("Prcp", "Tmin", "Tmax", "WndS", "WndD"),
  id_col = "EcoID"
)

clim_file <- file.path(clim_data_path, "scf-climate-inputs.csv")
writeClimateData(scf_inputs, clim_file)
```

### Monthly Climate Data (TerraClim)

As above, we can get monthly data using the following recipe:

``` r

climvars_monthly <- c("ppt", "tmax", "tmin")

monthly_weather <- prep_monthly_weather(
  vars = climvars_monthly,
  years = clim_years,
  studyArea = ecoregionPolys,
  id = "PolyID"
)

head(monthly_weather)

clim_file <- file.path(clim_data_path, "climate-data-monthly.csv")
writeClimateData(monthly_weather, clim_file)
```

We can also use this recipe to get monthly `AET` data to prepare the
`EcoregionParameters` table:

``` r

aet_df <- prep_monthly_weather(
  vars = "aet",
  years = clim_years,
  studyArea = ecoregionPolys,
  id = "PolyID"
)

head(aet_df)

erp_df <- prepEcoregionParameters(aet_df)

head(erp_df)
```

### Monthly Climate Data (BioSIM)

If you want monthly weather co-located with the daily BioSIM cells (same
elevation raster, same point batches), use
[`prep_monthly_weather_biosim()`](https://for-cast.github.io/landisutils/reference/prep_climate_data.md)
— it shares the elevation/batching machinery with
[`prep_daily_weather()`](https://for-cast.github.io/landisutils/reference/prep_climate_data.md)
and fetches BioSIM’s `Climatic_Monthly` model. Wind variables (`ws`,
`wnddir`) are pulled in the same call from BioSIM’s companion
`ClimaticWind_Monthly` model; wind from-direction is summarised across
the model’s 36 directional bins (`W0`..`W350`) using a
frequency-weighted circular mean:

``` r

climvars_monthly_biosim <- c("prcp", "tmax", "tmin", "temp", "rh", "srad", "ws", "wnddir")

monthly_weather_biosim <- prep_monthly_weather_biosim(
  vars = climvars_monthly_biosim,
  years = clim_years,
  studyArea = ecoregionPolys,
  id = "PolyID"
)

head(monthly_weather_biosim)

clim_file <- file.path(clim_data_path, "climate-data-monthly-biosim.csv")
writeClimateData(monthly_weather_biosim, clim_file)
```

The lower-level helpers
([`get_clim_monthly()`](https://for-cast.github.io/landisutils/reference/get_clim_monthly.md),
[`assemble_climate_library_file_monthly()`](https://for-cast.github.io/landisutils/reference/assemble_climate_library_file_monthly.md))
mirror the daily pipeline if you need to schedule per-batch fetches as
dynamic targets:

``` r

elev <- get_elevation_rast(ecoregionPolys, z = 9)
locations_batch <- create_locations_df(
  elev = elev,
  studyArea = ecoregionPolys,
  id = "PolyID",
  batch_size = 500
)

future::plan(future::multisession, workers = 2)

sa_hash <- landisutils:::.studyArea_hash(ecoregionPolys)

grid <- expand.grid(batch = seq_along(locations_batch), year = clim_years)
furrr::future_map2(grid$batch, grid$year, function(b, y) {
  get_clim_monthly(
    locations_batch[b], year = y,
    studyArea_hash = sa_hash, path = clim_data_path
  )
})

monthly_weather_biosim <- assemble_climate_library_file_monthly(
  dataset_path = file.path(clim_data_path, "Climatic_Monthly", sa_hash),
  vars = c("TotalPrcp", "MeanTmin", "MeanTmax", "WndS", "WndD"),
  id_col = "EcoID"
)
```

### Monthly Climate Data (climr)

[climr](https://github.com/bcgov/climr) provides ClimateNA-style
change-factor downscaling against a PRISM/DAYMET composite reference
grid for North America. Historical observational years span 1901-2024
(`obs_ts_dataset = "climatena"` or `"cru.gpcc"`). Wind variables are not
available from climr - use
[`prep_monthly_weather_biosim()`](https://for-cast.github.io/landisutils/reference/prep_climate_data.md)
for wind.

``` r

climvars_monthly_climr <- c("prcp", "tmax", "tmin", "temp", "rh", "srad", "cmd")

monthly_weather_climr <- prep_monthly_weather_climr(
  vars = climvars_monthly_climr,
  years = clim_years,
  studyArea = ecoregionPolys,
  id = "PolyID"
)

head(monthly_weather_climr)

clim_file <- file.path(clim_data_path, "climate-data-monthly-climr.csv")
writeClimateData(monthly_weather_climr, clim_file)
```

The lower-level helpers
([`get_clim_monthly_climr()`](https://for-cast.github.io/landisutils/reference/get_clim_monthly_climr.md),
[`assemble_climate_library_file_monthly_climr()`](https://for-cast.github.io/landisutils/reference/assemble_climate_library_file_monthly_climr.md))
mirror the BioSIM pipeline if you need to schedule per-year fetches as
dynamic targets. climr’s own on-disk cache is configured via
`options("climr.cache.path" = ...)`;
[`prep_monthly_weather_climr()`](https://for-cast.github.io/landisutils/reference/prep_climate_data.md)
defaults this to a `climr/` subdirectory of
`getOption("landisutils.cache.path")` for the duration of the call.

## Climate projections

The same `prep_*()` functions used above for historical/observational
weather can also produce **future** weather under climate-change
scenarios. Two complementary sources are wired in:

- **BioSIM** (CMIP5-era):
  [`prep_daily_weather()`](https://for-cast.github.io/landisutils/reference/prep_climate_data.md)
  and
  [`prep_monthly_weather_biosim()`](https://for-cast.github.io/landisutils/reference/prep_climate_data.md)
  accept `rcp` and `clim_model` arguments forwarded to
  [`BioSIM::generateWeather()`](https://rdrr.io/pkg/BioSIM/man/generateWeather.html).
  `rcp` is one of `"CONSTANT_CLIMATE"`, `"RCP45"`, `"RCP85"`;
  `clim_model` is one of `"RCM4"` (Canadian Regional Climate Model 4,
  CanESM2-driven), `"GCM4"` (CGCM4 / CanESM2-family GCM), or `"Hadley"`
  (HadGEM).
- **climr** (CMIP6):
  [`prep_monthly_weather_climr()`](https://for-cast.github.io/landisutils/reference/prep_climate_data.md)
  accepts `gcms`, `ssps`, and `max_run` arguments forwarded to
  [`climr::downscale()`](https://bcgov.github.io/climr/reference/downscale.html).
  Pass any subset of
  [`climr::list_gcms()`](https://bcgov.github.io/climr/reference/data-option-lists.html)
  (or use the bcgov-recommended 8-member subset exposed as
  `climr_ensemble_8`) and any subset of
  [`climr::list_ssps()`](https://bcgov.github.io/climr/reference/data-option-lists.html).

The BioSIM and climr caches are namespaced by scenario, so historical
and future fetches for the same study area coexist on disk without
colliding.

``` r

clim_years_future <- 2041:2042
```

### Daily future weather (BioSIM, CanESM2 + RCP 4.5)

Pass `rcp` and `clim_model` straight through to
[`prep_daily_weather()`](https://for-cast.github.io/landisutils/reference/prep_climate_data.md).
The Canadian fourth-generation GCM (CGCM4 / CanESM2-family) corresponds
to BioSIM’s `clim_model = "GCM4"`:

``` r

climvars_daily <- c("prcp", "tmax", "tmin")

daily_weather_future <- prep_daily_weather(
  vars = climvars_daily,
  years = clim_years_future,
  studyArea = ecoregionPolys,
  id = "PolyID",
  rcp = "RCP45",
  clim_model = "GCM4"
)

head(daily_weather_future)

clim_file <- file.path(clim_data_path, "climate-data-daily-rcp45-canesm.csv")
writeClimateData(daily_weather_future, clim_file)
```

The same arguments are also accepted by
[`prep_monthly_weather_biosim()`](https://for-cast.github.io/landisutils/reference/prep_climate_data.md),
which lets you pull wind variables (`ws`, `wnddir`) under the matching
scenario - climr does not expose wind, so pair it with BioSIM when you
need wind under a future projection.

### Monthly future weather (climr, 8-member ensemble + SSP2-4.5)

[`prep_monthly_weather_climr()`](https://for-cast.github.io/landisutils/reference/prep_climate_data.md)
switches into projection mode whenever `gcms` is non-`NULL`. The
bcgov-recommended 8-member subset of
[`climr::list_gcms()`](https://bcgov.github.io/climr/reference/data-option-lists.html)
is exposed for convenience as `climr_ensemble_8`; see
<https://bcgov.github.io/climr/articles/guidance_ensembleSelection.html>
for the rationale. SSP2-4.5 (`"ssp245"`) is the standard CMIP6 analogue
to CMIP5 RCP 4.5:

``` r

climvars_monthly_climr <- c("prcp", "tmax", "tmin", "temp", "rh", "srad", "cmd")

monthly_weather_climr_future <- prep_monthly_weather_climr(
  vars = climvars_monthly_climr,
  years = clim_years_future,
  studyArea = ecoregionPolys,
  id = "PolyID",
  gcms = climr_ensemble_8,
  ssps = "ssp245",
  max_run = 0L
)

head(monthly_weather_climr_future)

clim_file <- file.path(clim_data_path, "climate-data-monthly-climr-ssp245-ens8.csv")
writeClimateData(monthly_weather_climr_future, clim_file)
```

Each row is the **multi-GCM ensemble mean** across the 8 members, taken
inside
[`assemble_climate_library_file_monthly_climr()`](https://for-cast.github.io/landisutils/reference/assemble_climate_library_file_monthly_climr.md),
so the output has the same `(Year, Month, Variable, <ecoregion-id>...)`
shape as the observational pipeline. Set `max_run > 0` to additionally
average over multiple runs per GCM (climr defaults to ensemble-mean
only, `max_run = 0`).
