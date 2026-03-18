# Preparing Dynamic Fire and Fuels Inputs

``` r
library(landisutils)

tmp_pth <- withr::local_tempdir("example_DynamicFireFuels_")
```

## Sample extension inputs

### Dynamic Fire

All sample values from Dynamic Fire v4.0 test files (for LANDIS-II v8):
<https://github.com/LANDIS-II-Foundation/Extension-Dynamic-Fire-System/blob/master/testings/Core8-DynamicFire4.0/dynamic-fire_SetUpFire.txt>

``` r
fire_sizes <- tibble::tribble(
  ~EcoCode , ~EcoName , ~Mu , ~Sigma , ~Max ,
         1 , "fire1"  , 4.0 , 0.1    ,   50 ,
         2 , "fire2"  , 4.5 , 0.3    ,   50 ,
         3 , "fire3"  , 5.0 , 0.5    ,   50 ,
  ) |> dplyr::bind_cols(tibble::tribble(
    ~SpFMCLo , ~SpFMCHi , ~SpHiProp , ~SumFMCLo , ~SumFMCHi , ~SumHiProp , 
          85 ,      100 ,      0.50 ,        92 ,       120 ,       0.50 ,
          85 ,      105 ,      0.70 ,        94 ,       120 ,       0.40 ,
          85 ,      110 ,      0.60 ,        90 ,       120 ,       0.70
    )) |> dplyr::bind_cols(tibble::tribble(
     ~FallFMCLo , ~FallFMCHi , ~FallHiProp , ~OpenFuelIndex , ~NumFires ,
            120 ,        120 ,        0.50 ,              2 , 0.5       ,
            120 ,        120 ,        0.50 ,              2 , 0.2       ,
            120 ,        120 ,        0.50 ,             16 , 0.1
     ))

dyn_ecoregion <- prepDynamicEcoregionTable() ## optional

season <- tibble::tribble(
  ~Name    , ~LeafStatus , ~PropFire , ~PercentCuring , ~DayLengthProp ,
  "Spring" , "LeafOff"   , 0.20      ,              50 , 1.0            ,
  "Summer" , "LeafOn"    , 0.50      ,              51 , 1.0            ,
  "Fall"   , "LeafOff"   , 0.30      ,             100 , 1.0
)

fuel_type <- defaultFuelTypeTable()

fire_damage <- defaultFireDamageTable()

log_file <- file.path(tmp_pth, "fire/dynamic-fire-event-log.csv")
sum_log_file <- file.path(tmp_pth, "fire/summary-log.csv")

## these files don't need to be functional, just need to exist
ifrm_file <- file.path(tmp_pth, "Ecoregions.tif")
gs_file <- file.path(tmp_pth, "GroundSlope.tif")
ua_file <- file.path(tmp_pth, "UphillSlope.tif")
iwdb_file <- file.path(tmp_pth, "dynamic-fire_WeatherData.csv")

all_files <- c(ifrm_file, gs_file, ua_file, iwdb_file)
purrr::walk2(.x = rep("", length(all_files)), .y = all_files, .f = writeLines)
```

### Dynamic Fuels

All sample values from Dynamic Fuels v4.0 test files (for LANDIS-II v8):
<https://github.com/LANDIS-II-Foundation/Extension-Dynamic-Fire-System/blob/master/testings/Core8-DynamicFire4.0/dynamic-fire_SetUpFuel.txt>

``` r
spp_fuel_coeffs <- tibble::tribble(
  ~Species   , ~FuelCoefficient ,
  "abiebals" , 1.00             ,
  "acerrubr" , 0.50             ,
  "acersacc" , 1.00             ,
  "betualle" , 1.00             ,
  "fraxamer" , 1.00             ,
  "piceglau" , 1.00             ,
  "pinubank" , 1.00             ,
  "pinuresi" , 1.00             ,
  "pinustro" , 1.00             ,
  "poputrem" , 1.00             ,
  "querelli" , 1.00             ,
  "querrubr" , 1.00             ,
  "thujocci" , 1.00             ,
  "tiliamer" , 1.00
)

## NOTE: `AgeMin` and `AgeMax` specified here (will be converted to range internally)
fuel_types <- tibble::tribble(
  ~FuelType , ~BaseFuel           , ~AgeMin , ~AgeMax , ~Species                                                                                                         ,
          1 , "Conifer"           ,       0 ,     900 , list("thujocci")                                                                                                 ,
          2 , "Conifer"           ,       0 ,     500 , list("piceglau", "abiebals")                                                                                     ,
          3 , "Conifer"           ,      41 ,     100 , list("pinubank")                                                                                                 ,
          4 , "Conifer"           ,       0 ,      40 , list("pinubank")                                                                                                 ,
         16 , "Open"              ,       0 ,      20 , list("pinustro", "pinuresi")                                                                                     ,
          5 , "Conifer"           ,      20 ,     400 , list("pinustro", "pinuresi")                                                                                     ,
          6 , "ConiferPlantation" ,       0 ,     100 , list("piceglau", "-abiebals", "-pinubank", "-pinustro")                                                          ,
          8 , "Deciduous"         ,       0 ,     300 , list("acerrubr", "acersacc", "betualle", "fraxamer", "poputrem", "betupapy", "querelli", "querrubr", "tiliamer")
)

disturb_conv <- tibble::tribble(
  ~Fuel , ~Type , ~Duration       , ~Prescription    ,
     14 ,    20 , "WindSeverity3" , "AspenClearcut"  ,
     13 ,    20 , "WindSeverity4" , "MaxAgeClearcut" ,
     15 ,    20 , "WindSeverity5" , ""
)
```

## Extension configuration

### Dynamic Fire

``` r
## create the dynamic fire extension config object
ext_dyn_fire <- DynamicFire$new(
  path = tmp_pth,
  Timestep = 10,
  EventSizeType = "size_based",
  BuildUpIndex = "yes",
  WeatherRandomizer = 0L,
  FireSizesTable = fire_sizes,
  InitialFireEcoregionsMap = ifrm_file,
  DynamicEcoregionTable = dyn_ecoregion, ## optional
  GroundSlopeFile = gs_file,
  UphillSlopeAzimuthMap = ua_file,
  SeasonTable = season,
  InitialWeatherDatabase = iwdb_file,
  DynamicWeatherTable = NULL, ## not used
  FuelTypeTable = fuel_type,
  SeverityCalibrationFactor = 1.0,
  FireDamageTable = fire_damage,
  MapNames = NULL, # use default
  LogFile = log_file,
  SummaryLogFile = sum_log_file
)

ext_dyn_fire$write()
```

### Dynamic Fuels

``` r
## create the dynamic fuels extension config object
ext_dyn_fuel <- DynamicFuels$new(
  path = tmp_pth,
  Timestep = 10,
  SpeciesFuelCoefficients = spp_fuel_coeffs,
  HardwoodMaximum = 15L,
  DeadFirMaxAge = 15L, ## not needed w/o BDA extension
  FuelTypes = fuel_types,
  EcoregionTable = data.frame(FuelType = integer(0), Ecoregion = character(0)),
  DisturbanceConversionTable = disturb_conv,
  MapFileNames = NULL, ## use default
  PctConiferMapName = NULL, ## use default
  PctDeadFirMapName = NULL ## use default
)

ext_dyn_fuel$write()
```

## Verify configuration files

### Dynamic Fire

``` r
readLines(file.path(tmp_pth, ext_dyn_fire$files[1])) |>
  cat(sep = "\n")
#> >> generated by `landisutils` (v0.0.2) on Wed Mar 18 17:26:29 2026
#> >> do not edit by hand; manual changes to this file may be overwritten
#> 
#> LandisData  "Dynamic Fire System"
#> 
#> Timestep    10
#> 
#> EventSizeType    "size_based"
#> 
#> BuildUpIndex    yes
#> 
#> WeatherRandomizer    0
#> 
#> >> Fire Sizes (parameters applied for both size and duration based)
#> >> 
#> >> EcoCode    EcoName    Mu    Sigma    Max    SpFMCLo    SpFMCHi    SpHiProp    SumFMCLo    SumFMCHi    SumHiProp    FallFMCLo    FallFMCHi    FallHiProp    OpenFuelIndex    NumFires
#> >> ----------------------------------------------------------------
#> 1    fire1    4.0    0.1    50    85    100    0.5    92    120    0.5    120    120    0.5     2    0.5
#> 2    fire2    4.5    0.3    50    85    105    0.7    94    120    0.4    120    120    0.5     2    0.2
#> 3    fire3    5.0    0.5    50    85    110    0.6    90    120    0.7    120    120    0.5    16    0.1
#> 
#> InitialFireEcoregionsMap    "Ecoregions.tif"
#> 
#> DynamicEcoregionTable
#> >> Year    FileName
#> >> ----    ------------------------
#> 
#> GroundSlopeFile    "GroundSlope.tif"
#> 
#> UphillSlopeAzimuthMap    "UphillSlope.tif"
#> 
#> SeasonTable
#> >>           Leaf      Proportion    Percent    DayLength
#> >> Name      Status    Fire          Curing     Proportion
#> >> -----------------------------------------------
#> Spring    LeafOff    0.2     50    1
#> Summer    LeafOn    0.5     51    1
#> Fall    LeafOff    0.3    100    1
#> 
#> InitialWeatherDatabase    "dynamic-fire_WeatherData.csv"
#> 
#> DynamicWeatherTable
#> >> Year    FileName
#> >> ----    ------------------------
#> 
#> FuelTypeTable
#> >> Allowed base types:     Conifer, ConiferPlantation, Deciduous, Slash, Open.
#> >> Allowed surface types:  See Canadian Fire Behavior System (CFBS).
#> >> Index    Base    Surface    IgnProb    a    b    c    q    BUI    maxBE    CBH
#> >>          Type        Type   
#> >> ----------------------------------------------------------------
#>  1    Conifer    C1    1.0     90    0.0649    4.5    0.90     72    1.076     2
#>  2    Conifer    C2    1.0    110    0.0282    1.5    0.70     64    1.321     3
#>  3    Conifer    C3    1.0    110    0.0444    3.0    0.75     62    1.261     8
#>  4    Conifer    C4    1.0    110    0.0293    1.5    0.80     66    1.184     4
#>  5    Conifer    C5    1.0     30    0.0697    4.0    0.80     56    1.220    18
#>  6    ConiferPlantation    C6    1.0     30    0.0800    3.0    0.80     62    1.197     7
#>  7    Conifer    C7    1.0     45    0.0305    2.0    0.85    106    1.134    10
#>  8    Deciduous    D1    0.5     30    0.0232    1.6    0.90     32    1.179     0
#>  9    Conifer    M1    1.0      0    0.0000    0.0    0.80     50    1.250     6
#> 10    Conifer    M2    1.0      0    0.0000    0.0    0.80     50    1.250     6
#> 11    Conifer    M3    1.0      0    0.0000    0.0    0.80     50    1.250     6
#> 12    Conifer    M4    1.0      0    0.0000    0.0    0.80     50    1.250     6
#> 13    Slash    S1    1.0     75    0.0297    1.3    0.75     38    1.460     0
#> 14    Slash    S2    1.0     40    0.0438    1.7    0.75     63    1.256     0
#> 15    Slash    S3    1.0     55    0.0829    3.2    0.75     31    1.590     0
#> 16    Open    O1a    1.0    190    0.0310    1.4    1.00      1    1.000     0
#> 17    Open    O1b    1.0    250    0.0350    1.7    1.00      1    1.000     0
#> 
#> SeverityCalibrationFactor    1
#> 
#> FireDamageTable
#> >> Cohort Age      FireSeverity - 
#> >> % of longevity  FireTolerance
#> >> --------------  ---------------
#> 20%      -2
#> 50%      -1
#> 85%      0
#> 100%      1
#> 
#> MapNames    "fire/severity-{timestep}.tif"
#> 
#> LogFile    "fire/dynamic-fire-event-log.csv"
#> 
#> SummaryLogFile    "fire/summary-log.csv"
```

*skip species table and raster files*

### Dynamic Fuels

``` r
readLines(file.path(tmp_pth, ext_dyn_fuel$files[1])) |>
  cat(sep = "\n")
#> >> generated by `landisutils` (v0.0.2) on Wed Mar 18 17:26:29 2026
#> >> do not edit by hand; manual changes to this file may be overwritten
#> 
#> LandisData  "Dynamic Fuels"
#> 
#> Timestep    10
#> 
#> >> Species    Fuel
#> >>            Coefficient
#> >> -------    -----------
#> abiebals    1.0
#> acerrubr    0.5
#> acersacc    1.0
#> betualle    1.0
#> fraxamer    1.0
#> piceglau    1.0
#> pinubank    1.0
#> pinuresi    1.0
#> pinustro    1.0
#> poputrem    1.0
#> querelli    1.0
#> querrubr    1.0
#> thujocci    1.0
#> tiliamer    1.0
#> 
#> HardwoodMaximum    15
#> 
#> DeadFirMaxAge    15
#> 
#> FuelTypes
#> >> Fuel Type    Base Fuel    Age Range    Species
#> >> ---------    ---------    ---------    ----------------
#> 1    Conifer    list("thujocci") 0 to 900
#> 2    Conifer    list("piceglau", "abiebals") 0 to 500
#> 3    Conifer    list("pinubank") 41 to 100
#> 4    Conifer    list("pinubank") 0 to 40
#> 16    Open    list("pinustro", "pinuresi") 0 to 20
#> 5    Conifer    list("pinustro", "pinuresi") 20 to 400
#> 6    ConiferPlantation    list("piceglau", "-abiebals", "-pinubank", "-pinustro") 0 to 100
#> 8    Deciduous    list("acerrubr", "acersacc", "betualle", "fraxamer", "poputrem", "betupapy", "querelli", "querrubr", "tiliamer") 0 to 300
#> 
#> EcoregionTable
#> >> Fuel Type    Ecoregion
#> >> ---------    ---------
#> 
#> DisturbanceConversionTable
#> >> Fuel  Type    Duration    Prescription
#> >> ----  ---    --------    ------------
#> 14    20    WindSeverity3    AspenClearcut
#> 13    20    WindSeverity4    MaxAgeClearcut
#> 15    20    WindSeverity5    
#> 
#> MapFileNames    "fire/FuelType-{timestep}.tif"
#> 
#> PctConiferMapName    "fire/PctConifer-{timestep}.tif"
#> 
#> PctDeadFirMapName    "fire/PctDeadFir-{timestep}.tif"
```

*skip species table and raster files*

## Cleanup

``` r
withr::deferred_run()
#> Ran 1/1 deferred expressions
```
