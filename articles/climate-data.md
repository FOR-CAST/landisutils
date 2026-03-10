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

### BioSIM

``` r
climvars <- c(
  ## BioSIM::getModelHelp("ClimaticEx_Daily")
  "Prcp", "SRad", "RelH", "Tair", "Tmax", "Tmin", "WndD", "WndS",
  
  ## BioSIM::getModelHelp("Potential_Evapotranspiration_Daily")
  ## BioSIM::getModelHelp("Potential_Evapotranspiration_Ex_Daily")
  "PET"
)

## TODO: use Prcp / 10 (must be in cm);
## TODO: convert WndS from km/h to m/s;
## TODO: verify wind direction is the FROM Direction;

BioSIM::generateWeather(
  modelNames = c("ClimaticEx_Daily", "Potential_Evapotranspiration_Daily"),
  fromYr = head(clim_years, 1),
  toYr = tail(clim_years, 1),
  id = ,      ## TODO: e.g., 1:length(longDeg)
  latDeg = ,  ## TODO: extract points from gridded polygon
  longDeg = , ## TODO: extract points from gridded polygon
  elevM = rep(NA, length(longDeg)),
  rep = 1,
  repModel = 1,
  rcp = "RCP45",      ## shouldn't matter for historical, but FRV to use SSPs not RCPs;
  climModel = "RCM4", ## shouldn't matter for historical, but FRV to use SSPs not RCPs;
  additionalParms = NULL
)
## TODO: gridded values need to be summarized by ecolocation; i.e., `zonal`
```

### Daily Climate Data (Daymet)

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

We can also use this recipe to get monthly AET data to prepare the
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

## Climate projections

See
<https://mikejohnson51.github.io/climateR/articles/intro.html#climate-projections>

``` r
## TODO
```
