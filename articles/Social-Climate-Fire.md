# Preparing Social-Climate-Fire Inputs

``` r
library(landisutils)

tmp_pth <- withr::local_tempdir("example_SocialClimateFire_")
```

## Sample extension inputs

``` r
species_table <- tibble::tribble(
  ~SpeciesCode , ~AgeDBH , ~MaximumBarkThickness ,
  "PinuJeff"   ,     100 ,                    10 ,
  "PinuLamb"   ,     100 ,                    10 ,
  "CaloDecu"   ,     100 ,                    10 ,
  "AbieConc"   ,     100 ,                    10 ,
  "AbieMagn"   ,     100 ,                    10 ,
  "PinuCont"   ,     100 ,                    10 ,
  "PinuMont"   ,     100 ,                    10 ,
  "TsugMert"   ,     100 ,                    10 ,
  "PinuAlbi"   ,     100 ,                    10 ,
  "PopuTrem"   ,     100 ,                    10 ,
  "NonnResp"   ,     100 ,                    10 ,
  "NonnSeed"   ,     100 ,                    10 ,
  "FixnResp"   ,     100 ,                    10 ,
  "FixnSeed"   ,     100 ,                    10
)

species_file <- prepSpeciesData(
  df = species_table,
  type = "fire",
  path = tmp_pth,
  filename = "SCRPPLE_Spp_Table.csv"
)

ltg_ign_coeffs <- c(-8.5, 0.03)
acc_ign_coeffs <- c(-8.5, 0.03)

ltg_ign_zip_coeffs <- c(-8.5, 0.03)
acc_ign_zip_coeffs <- c(-8.5, 0.03)

max_spread_area_coeffs <- c(10, -2.5, -2.5)

spread_prob_coeffs <- c(-1.79, 0.06, -0.915, 0.0126)

## fmt: table
site_mortality_coeffs <- c(
   0.0059     , 0.00050 , -0.000010 , -0.0002200 ,
  -0.00000050 , 0.00000 ,  0.00000
)

cohort_mortality_coeffs <- c(-0.703, -0.9908, 0.009)

ladder_fuel_species <- c("AbieConc", "AbieMagn", "PinuJeff", "PinuCont")

suppression_table <- tibble::tribble(
  ~IgnitionType , ~Mapcode , ~Suppress_Category_0 , ~FWI_Break_1 , ~Suppress_Category_1 , ~FWI_Break_2 , ~Suppress_Category_2 ,
  "Accidental"  ,        1 ,                   10 ,           20 ,                   20 ,           30 ,                    0 ,
  "Accidental"  ,        2 ,                   30 ,           20 ,                   95 ,           30 ,                   10 ,
  "Accidental"  ,        3 ,                   95 ,           20 ,                   95 ,           30 ,                   75 ,
  "Lightning"   ,        1 ,                   10 ,           20 ,                   20 ,           30 ,                    0 ,
  "Lightning"   ,        2 ,                   30 ,           20 ,                   95 ,           30 ,                   10 ,
  "Lightning"   ,        3 ,                   95 ,           20 ,                   95 ,           30 ,                   75 ,
  "Rx"          ,        1 ,                   10 ,           20 ,                   20 ,           30 ,                    0 ,
  "Rx"          ,        2 ,                   30 ,           20 ,                   95 ,           30 ,                   10 ,
  "Rx"          ,        3 ,                   95 ,           20 ,                   95 ,           30 ,                   75
)

suppression_file <- prepSuppression_CSV_File(
  suppression_table,
  path = tmp_pth,
  filename = "Example_Suppression_Input.csv"
)

deadwood <- tibble::tribble(
  ~species   , ~age ,
  "PinuJeff" ,   22 ,
  "CaloDecu" ,   33 ,
  "PinuCont" ,   38 ,
  "PinuLamb" ,   27 ,
  "AbieMagn" ,   28 ,
  "AbieConc" ,   29
)
```

## Extension configuration

``` r
## don't need working files, they just need to exist
acc_ign_map <- file.path(tmp_pth, "Accidental_Ignition_Map.img")
ltg_ign_map <- file.path(tmp_pth, "Lightning_Ignition_Map.img")
rx_ign_map <- file.path(tmp_pth, "Lightning_Ignition_Map.img")

acc_supp_map <- file.path(tmp_pth, "suppress3.img")
ltg_supp_map <- file.path(tmp_pth, "suppress3.img")
rx_supp_map <- file.path(tmp_pth, "suppress3.img")

gs_file <- file.path(tmp_pth, "GroundSlope.gis")
ua_file <- file.path(tmp_pth, "UphillSlope.gis")
cm_file <- file.path(tmp_pth, "random9.img")

rx_zones <- file.path(tmp_pth, "fire-zones.gis") ## optional

## fmt: table
raster_files <- c(
  acc_ign_map  , ltg_ign_map  , rx_ign_map  ,
  acc_supp_map , ltg_supp_map , rx_supp_map ,
  gs_file      , ua_file      , cm_file     ,
  rx_zones
)
purrr::walk2(.x = rep("", length(raster_files)), .y = raster_files, .f = writeLines)

## create the Social-Climate-Fire extension config object
ext_social_climate_fire <- SocialClimateFire$new(
  path = tmp_pth,
  Timestep = 1,
  TimeZeroPET = NULL, ## optional
  TimeZeroCWD = NULL, ## optional
  Species_CSV_File = species_file,

  ## ignition maps
  AccidentalIgnitionsMap = acc_ign_map,
  DynamicAccidentalIgnitionMaps = NULL, ## optional
  LightningIgnitionsMap = ltg_ign_map,
  DynamicLightningIgnitionsMaps = NULL, ## optional
  RxIgnitionsMap = rx_ign_map,
  DynamicRxIgnitionsMaps = NULL, ## optional

  ## suppression maps
  AccidentalSuppressionMap = acc_supp_map,
  LightningSuppressionMap = ltg_supp_map,
  RxSuppressionMap = rx_supp_map,
  DynamicAccidentalSuppressionMaps = NULL, ## optional

  ## topography files
  GroundSlopeFile = gs_file,
  UphillSlopeAzimuthMap = ua_file,
  ClayMap = cm_file,

  ## ignition model coefficients
  LightningIgnitionsCoeffs = ltg_ign_coeffs,
  AccidentalIgnitionsCoeffs = acc_ign_coeffs,
  IgnitionDistribution = "ZeroInflatedPoisson",
  LightningIgnitionsBinomialCoeffs = ltg_ign_zip_coeffs,
  AccidentalIgnitionsBinomialCoeffs = acc_ign_zip_coeffs,
  MaximumFineFuels = 500.0,

  ## prescribed fire burn window parameters
  MaximumRxWindSpeed = 80.0,
  MaximumRxFireWeatherIndex = 80.0, ## optional
  MinimumRxFireWeatherIndex = 1.0, ## optional
  MaximumRxTemperature = 35.0, ## optional
  MinimumRxRelativeHumidity = 22.0, ## optional
  MaximumRXFireIntensity = 1,
  NumberRxAnnualFires = 10,
  NumberRxDailyFires = 1,
  FirstDayRxFires = 2,
  LastDayRxFires = 300,
  TargetRxSize = 40,
  RxZonesMap = rx_zones, ## optional

  MaximumSpreadAreaCoeffs = max_spread_area_coeffs,
  SpreadProbabilityCoeffs = spread_prob_coeffs,
  SiteMortalityCoeffs = site_mortality_coeffs,
  CohortMortalityCoeffs = cohort_mortality_coeffs,

  LadderFuelMaxAge = 40, ## TODO
  LadderFuelSpeciesList = ladder_fuel_species,
  SuppressionMaxWindSpeed = 100,
  Suppression_CSV_File = suppression_file,
  DeadWoodTable = deadwood
)

## write the Social-Climate-Fire extension config file
ext_social_climate_fire$write()
```

## Verify configuration files

``` r
readLines(file.path(tmp_pth, ext_social_climate_fire$files[1])) |>
  cat(sep = "\n")
#> >> generated by `landisutils` (v0.0.1.9017) on Wed Mar 11 20:23:12 2026
#> >> do not edit by hand; manual changes to this file may be overwritten
#> 
#> LandisData  "Social Climate Fire"
#> 
#> Timestep    1
#> 
#> 
#> 
#> Species_CSV_File    "SCRPPLE_Spp_Table.csv"
#> 
#> AccidentalIgnitionsMap    "Accidental_Ignition_Map.img"
#> 
#> DynamicAccidentalIgnitionMaps
#> >> Year    FileName
#> >> ----    ------------------------
#> 
#> LightningIgnitionsMap    "Lightning_Ignition_Map.img"
#> 
#> DynamicLightningIgnitionsMaps
#> >> Year    FileName
#> >> ----    ------------------------
#> 
#> RxIgnitionsMap    "Lightning_Ignition_Map.img"
#> 
#> DynamicRxIgnitionsMaps
#> >> Year    FileName
#> >> ----    ------------------------
#> 
#> AccidentalSuppressionMap    "suppress3.img"
#> 
#> LightningSuppressionMap    "suppress3.img"
#> 
#> RxSuppressionMap    "suppress3.img"
#> 
#> GroundSlopeFile    "GroundSlope.gis"
#> 
#> UphillSlopeAzimuthMap    "UphillSlope.gis"
#> 
#> ClayMap    "random9.img"
#> 
#> LightningIgnitionsB0    -8.5
#> LightningIgnitionsB1    0.03
#> AccidentalIgnitionsB0    -8.5
#> AccidentalIgnitionsB1    0.03
#> 
#> IgnitionDistribution    "ZeroInflatedPoisson"
#> 
#> LightningIgnitionsBinomialB0    -8.5
#> LightningIgnitionsBinomialB1    0.03
#> AccidentalIgnitionsBinomialB0    -8.5
#> AccidentalIgnitionsBinomialB1    0.03
#> MaximumFineFuels    500
#> 
#> MaximumRxWindSpeed    80
#> MaximumRxFireWeatherIndex    80
#> MinimumRxFireWeatherIndex    1
#> MaximumRxTemperature    35
#> MinimumRxRelativeHumidity    22
#> MaximumRXFireIntensity    1
#> NumberRxAnnualFires    10
#> NumberRxDailyFires    1
#> FirstDayRxFires    2
#> LastDayRxFires    300
#> TargetRxSize    40
#> RxZonesMap    "fire-zones.gis"
#> 
#> MaximumSpreadAreaB0    10
#> MaximumSpreadAreaB1    -2.5
#> MaximumSpreadAreaB2    -2.5
#> 
#> SpreadProbabilityB0    -1.79
#> SpreadProbabilityB1    0.06
#> SpreadProbabilityB2    -0.915
#> SpreadProbabilityB3    0.0126
#> 
#> SiteMortalityB0    0.0059
#> SiteMortalityB1    0.0005
#> SiteMortalityB2    -0.00001
#> SiteMortalityB3    -0.00022
#> SiteMortalityB4    -0.0000005
#> SiteMortalityB5    0
#> SiteMortalityB6    0
#> 
#> CohortMortalityB0    -0.703
#> CohortMortalityB1    -0.9908
#> CohortMortalityB2    0.009
#> 
#> LadderFuelMaxAge    40
#> 
#> LadderFuelSpeciesList
#> AbieConc  AbieMagn  PinuJeff  PinuCont
#> 
#> SuppressionMaxWindSpeed    100
#> 
#> Suppression_CSV_File    "Example_Suppression_Input.csv"
#> 
#> DeadWoodTable
#> PinuJeff    22
#> CaloDecu    33
#> PinuCont    38
#> PinuLamb    27
#> AbieMagn    28
#> AbieConc    29
```

*skip species table and raster files*

``` r
readLines(file.path(tmp_pth, ext_social_climate_fire$files[10])) |>
  cat(sep = "\n")
#> "IgnitionType","Mapcode","Suppress_Category_0","FWI_Break_1","Suppress_Category_1","FWI_Break_2","Suppress_Category_2"
#> "Accidental",1,10,20,20,30,0
#> "Accidental",2,30,20,95,30,10
#> "Accidental",3,95,20,95,30,75
#> "Lightning",1,10,20,20,30,0
#> "Lightning",2,30,20,95,30,10
#> "Lightning",3,95,20,95,30,75
#> "Rx",1,10,20,20,30,0
#> "Rx",2,30,20,95,30,10
#> "Rx",3,95,20,95,30,75
```

## Cleanup

``` r
withr::deferred_run()
#> Ran 1/1 deferred expressions
```
