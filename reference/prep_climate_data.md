# Prepare Climate Data

Download and prepare climate data for use with LANDIS-II simulations:

## Usage

``` r
prep_daily_weather(vars = NULL, years = NULL, studyArea = NULL, id = NULL)

prep_monthly_weather(vars = NULL, years = NULL, studyArea = NULL, id = NULL)
```

## Arguments

- vars:

  character specifying the climate variables.

- years:

  integer vector specifying the years.

- studyArea:

  `sf` polygons object delineating e.g., ecoregions or fire zones.

- id:

  character specifying the name of the column/field to use for zonal
  summaries.

## Value

`tbl_df`

## Details

1.  Climate data for the `studyArea` are downloaded and converted to
    `SpatRaster` using the climateR package;

2.  These are then summarized by zone (specified by `id`) using the
    zonal package;

3.  The tabular data are pivoted as required for ingestion by LANDIS-II
    Climate Library.

## Historical daily weather

Daymet provides daily North American weather 1980-present
(<https://daymet.ornl.gov/>). Daymet variables: `dayl`, `prcp`, `srad`,
`swe`, `tmax`, `tmin`, `vp`. Use `prep_daily_weather()` for Daymet
weather data.

## Historical monthly weather

Terra Climate provides monthly North American weather 1980-present
(<https://www.climatologylab.org/terraclimate.html>). TerraClim
variables: `aet`, `def`, `PDSI`, `pet`, `ppt`, `q`, `soil`, `srad`,
`swe`, `tmax`, `tmin`, `vap`, `vpd`, `ws`. Use `prep_monthly_weather()`
for TerraClim weather data.

## Climate projections

TODO

## Caching

Caching is enabled by default, with the cache location configurable by
setting the `landisutils.cache.path` option.

## Examples

``` r
if (requireNamespace("climateR", quietly = TRUE) &&
  requireNamespace("zonal", quietly = TRUE)) {
  ## use BEC zones in random study area in BC
  # ecoregionPolys <- landisutils::test_ecoregionPolys

  # if (interactive()) plot(frpFRT["PolyID"])

  # clim_years <- 2011:2012 ## availability is 1980 to last year

  ## get historic daily weather data from Daymet
  # daily_climvars <- c("prcp", "tmax", "tmin")
  # daily_weather <- prep_daily_weather(
  #   vars = daily_climvars,
  #   years = clim_years,
  #   studyArea = ecoregionPolys,
  #   id = "PolyID"
  # )

  # head(daily_weather)

  ## get historic monthly weather from TerraClim
  # monthly_climvars <- c("ppt", "tmax", "tmin")
  # monthly_weather <- prep_monthly_weather(
  #   vars = monthly_climvars,
  #   years = clim_years,
  #   studyArea = ecoregionPolys,
  #   id = "PolyID"
  # )

  # head(monthly_weather)
}
#> NULL
```
