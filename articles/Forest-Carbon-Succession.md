# Preparing Forest Carbon Succession (ForCS) Inputs

``` r
library(landisutils)

tmp_pth <- withr::local_tempdir("example_ForCS_")
```

## Sample extension inputs

``` r
clim_df <- tibble::tribble(
  ~year , ~ecoregion , ~avg_temp ,
      0 , "eco1"     , 5         ,
      0 , "eco2"     , 2         ,
      5 , "eco1"     , 5.5       ,
      5 , "eco2"     , 2.5       ,
     10 , "eco1"     , 6         ,
     10 , "eco2"     , 3         ,
     20 , "eco1"     , 7         ,
     20 , "eco2"     , 5
)

disturb_fire_dom <- tibble::tribble(
  ~intensity , ~from_dom , ~to_air , ~to_dom , ~to_fps ,
           1 ,         1 , 0.5     , 0.0     , 0.0     ,
           1 ,         3 , 0.2     , 0.0     , 0.0     ,
           2 ,         1 , 0.5     , 0.0     , 0.0     ,
           2 ,         2 , 0.25    , 0.0     , 0.0     ,
           2 ,         3 , 0.35    , 0.0     , 0.0     ,
           2 ,         8 , 0.0     , 0.5     , 0.0     ,
           2 ,         9 , 0.0     , 1.0     , 0.0     ,
           3 ,         1 , 0.65    , 0.0     , 0.0     ,
           3 ,         2 , 0.35    , 0.0     , 0.0     ,
           3 ,         3 , 0.4     , 0.0     , 0.0     ,
           3 ,         8 , 0.0     , 0.75    , 0.0     ,
           3 ,         9 , 0.5     , 0.5     , 0.0     ,
           4 ,         1 , 1.0     , 0.0     , 0.0     ,
           4 ,         2 , 0.5     , 0.0     , 0.0     ,
           4 ,         3 , 0.4     , 0.0     , 0.0     ,
           4 ,         5 , 0.1     , 0.0     , 0.0     ,
           4 ,         8 , 0.0     , 1.0     , 0.0     ,
           4 ,         9 , 0.7     , 0.3     , 0.0     ,
           5 ,         1 , 1.0     , 0.0     , 0.0     ,
           5 ,         2 , 0.65    , 0.0     , 0.0     ,
           5 ,         3 , 0.45    , 0.0     , 0.0     ,
           5 ,         5 , 0.1     , 0.0     , 0.0     ,
           5 ,         8 , 0.1     , 0.9     , 0.0     ,
           5 ,         9 , 0.7     , 0.3     , 0.0
)

disturb_other_dom <- tibble::tribble(
  ~disturb_type , ~from_dom , ~to_air , ~to_dom , ~to_fps ,
  "Clearcut"    ,         1 , 0.0     , 0.0     , 0.0     ,
  "Clearcut"    ,         8 , 0.0     , 0.4     , 0.6     ,
  "wind"        ,         8 , 0.0     , 0.8     , 0.0     ,
  "wind"        ,         9 , 0.0     , 1.0     , 0.0
)

disturb_fire_biomass = tibble::tribble(
  ~intensity , ~from_biomass , ~to_air , ~to_fps , ~to_dom ,
           1 ,             1 , 0       ,       0 , 1       ,
           1 ,             2 , 0.5     ,       0 , 0.5     ,
           1 ,             3 , 0       ,       0 , 1       ,
           1 ,             5 , 0       ,       0 , 1       ,
           1 ,             6 , 0       ,       0 , 1       ,
           2 ,             1 , 0       ,       0 , 1       ,
           2 ,             2 , 0.7     ,       0 , 0.3     ,
           2 ,             3 , 0.1     ,       0 , 0.9     ,
           2 ,             5 , 0       ,       0 , 1       ,
           2 ,             6 , 0       ,       0 , 1       ,
           3 ,             1 , 0       ,       0 , 1       ,
           3 ,             2 , 0.75    ,       0 , 0.25    ,
           3 ,             3 , 0.1     ,       0 , 0.9     ,
           3 ,             5 , 0       ,       0 , 1       ,
           3 ,             6 , 0       ,       0 , 1       ,
           4 ,             1 , 0       ,       0 , 1       ,
           4 ,             2 , 1       ,       0 , 0       ,
           4 ,             3 , 0.2     ,       0 , 0.8     ,
           4 ,             5 , 0       ,       0 , 1       ,
           4 ,             6 , 0       ,       0 , 1       ,
           5 ,             1 , 0       ,       0 , 1       ,
           5 ,             2 , 1       ,       0 , 0       ,
           5 ,             3 , 0.3     ,       0 , 0.7     ,
           5 ,             5 , 0       ,       0 , 1       ,
           5 ,             6 , 0       ,       0 , 1
)

disturb_other_biomass = tibble::tribble(
  ~disturb_type , ~from_biomass , ~to_air , ~to_fps , ~to_dom ,
  "Clearcut"    ,             1 , 0       ,       1 , 0       ,
  "Clearcut"    ,             2 , 0       ,       0 , 1       ,
  "Clearcut"    ,             3 , 0       ,       0 , 1       ,
  "Clearcut"    ,             5 , 0       ,       0 , 1       ,
  "Clearcut"    ,             6 , 0       ,       0 , 1       ,
  "wind"        ,             1 , 0       ,       0 , 1       ,
  "wind"        ,             2 , 0       ,       0 , 1       ,
  "wind"        ,             3 , 0       ,       0 , 1       ,
  "wind"        ,             5 , 0       ,       0 , 1       ,
  "wind"        ,             6 , 0       ,       0 , 1       ,
  "bda"         ,             1 , 0       ,       0 , 1       ,
  "bda"         ,             2 , 0.3     ,       0 , 0.7     ,
  "bda"         ,             3 , 0.2     ,       0 , 0.8     ,
  "bda"         ,             5 , 0       ,       0 , 1       ,
  "bda"         ,             6 , 0       ,       0 , 1
)

snags <- tibble::tribble(
  ~species   , ~age_at_death , ~time_since_death , ~cause  ,
  "querelli" ,            53 ,                 5 , "other" ,
  "pinubank" ,            88 ,                12 , "bda"
)

output_tables <- data.frame(Biomass = 1, DOM_Pools = 1, Fluxes = 1, Summary = 1)

soil_spinup <- data.frame(Flag = 1, Tolerance = 1.0, MaxIter = 20)

avail_light_biomass <- data.frame(
  Class = 1L:5L,
  eco1 = c(30, 35, 55, 80, 100),
  eco2 = c(30, 35, 55, 80, 100)
)

light_est <- data.frame(
  class = 1L:5L,
  X0 = c(1.0, 0.5, 0.2, 0.1, 0.1),
  X1 = c(0.5, 1.0, 0.2, 0.1, 0.5),
  X2 = c(0.0, 0.5, 1.0, 0.2, 0.2),
  X3 = c(0.0, 0.0, 0.5, 1.0, 0.2),
  X4 = c(0.0, 0.0, 0.0, 0.5, 1.0),
  X5 = c(0.0, 0.0, 0.0, 0.5, 1.0)
)

species_params <- tibble::tribble(
  ~species   , ~leaf_long , ~mort_shp , ~min_age_merch , ~merch_shp_a , ~merch_shp_b , ~nonmerch_biomass_soil , ~growth_shp ,
  "pinubank" , 3.0        ,        10 ,             10 , 0.7546       , 0.983        , 0.25                   , 0.25        ,
  "querelli" , 1.0        ,        10 ,             30 , 0.7546       , 0.983        , 0.25                   , 0.25
)

dom_pools <- tibble::tribble(
  ~id , ~name                   , ~prop_to_atmosphere ,
    1 , "Very Fast Aboveground" , 0.815               ,
    2 , "Very Fast Belowground" , 0.83                ,
    3 , "Fast Aboveground"      , 0.83                ,
    4 , "Fast Belowground"      , 0.83                ,
    5 , "Medium"                , 0.83                ,
    6 , "Slow Aboveground"      , 1                   ,
    7 , "Slow Belowground"      , 1                   ,
    8 , "Stem Snag"             , 0.83                ,
    9 , "Other Snag"            , 0.83                ,
   10 , "Extra pool"            , 0.83
)

ecosppdom_params <- tibble::tribble(
  ~ecoregion , ~species   , ~dom_pool , ~decay_rate , ~amt_t0 , ~q10_ref_temp_10c ,
  "eco1"     , "pinubank" ,         1 , 0.355       ,   53.04 , 2.65              ,
  "eco1"     , "pinubank" ,         2 , 0.5         ,  295.4  , 2                 ,
  "eco1"     , "pinubank" ,         3 , 0.1435      , 1395.49 , 2                 ,
  "eco1"     , "pinubank" ,         4 , 0.0374      , 1360.62 , 2                 ,
  "eco1"     , "pinubank" ,         5 , 0.015       ,  863.88 , 2                 ,
  "eco1"     , "pinubank" ,         6 , 0.0033      , 1656.13 , 2.65              ,
  "eco1"     , "pinubank" ,         7 , 0.0187      , 8451.22 , 2                 ,
  "eco1"     , "pinubank" ,         8 , 0.07175     , 7466.54 , 2                 ,
  "eco1"     , "pinubank" ,         9 , 0.07        , 2036.14 , 2                 ,
  "eco1"     , "pinubank" ,        10 , 0           ,    0    , 2                 ,
  "eco2"     , "pinubank" ,         1 , 0.355       ,   53.04 , 2.65              ,
  "eco2"     , "pinubank" ,         2 , 0.5         ,  295.4  , 2                 ,
  "eco2"     , "pinubank" ,         3 , 0.1435      , 1395.49 , 2                 ,
  "eco2"     , "pinubank" ,         4 , 0.0374      , 1360.62 , 2                 ,
  "eco2"     , "pinubank" ,         5 , 0.015       ,  863.88 , 2                 ,
  "eco2"     , "pinubank" ,         6 , 0.0033      , 1656.13 , 2.65              ,
  "eco2"     , "pinubank" ,         7 , 0.0187      , 8451.22 , 2                 ,
  "eco2"     , "pinubank" ,         8 , 0.07175     , 7466.54 , 2                 ,
  "eco2"     , "pinubank" ,         9 , 0.07        , 2036.14 , 2                 ,
  "eco2"     , "pinubank" ,        10 , 0           ,    0    , 2                 ,
  "eco1"     , "querelli" ,         1 , 0.355       ,   53.04 , 2.65              ,
  "eco1"     , "querelli" ,         2 , 0.5         ,  295.4  , 2                 ,
  "eco1"     , "querelli" ,         3 , 0.1435      , 1395.49 , 2                 ,
  "eco1"     , "querelli" ,         4 , 0.0374      , 1360.62 , 2                 ,
  "eco1"     , "querelli" ,         5 , 0.015       ,  863.88 , 2                 ,
  "eco1"     , "querelli" ,         6 , 0.0033      , 1656.13 , 2.65              ,
  "eco1"     , "querelli" ,         7 , 0.0187      , 8451.22 , 2                 ,
  "eco1"     , "querelli" ,         8 , 0.07175     , 7466.54 , 2                 ,
  "eco1"     , "querelli" ,         9 , 0.07        , 2036.14 , 2                 ,
  "eco1"     , "querelli" ,        10 , 0           ,    0    , 2                 ,
  "eco2"     , "querelli" ,         1 , 0.355       ,   53.04 , 2.65              ,
  "eco2"     , "querelli" ,         2 , 0.5         ,  295.4  , 2                 ,
  "eco2"     , "querelli" ,         3 , 0.1435      , 1395.49 , 2                 ,
  "eco2"     , "querelli" ,         4 , 0.0374      , 1360.62 , 2                 ,
  "eco2"     , "querelli" ,         5 , 0.015       ,  863.88 , 2                 ,
  "eco2"     , "querelli" ,         6 , 0.0033      , 1656.13 , 2.65              ,
  "eco2"     , "querelli" ,         7 , 0.0187      , 8451.22 , 2                 ,
  "eco2"     , "querelli" ,         8 , 0.07175     , 7466.54 , 2                 ,
  "eco2"     , "querelli" ,         9 , 0.07        , 2036.14 , 2                 ,
  "eco2"     , "querelli" ,        10 , 0           ,    0    , 2
)

forcs_props <- data.frame(
  BiomassFine = 0.5,
  BiomassCoarse = 0.5,
  AnnualSlowAGtoSlowBG = 0.006,
  AnnualStemSnagToMedium = 0.032,
  AnnualBranchSnagToFastAG = 0.1
)

anpp_timeseries <- tibble::tribble(
  ~year , ~ecoregion , ~species   , ~anpp , ~anpp_std ,
      0 , "eco1"     , "pinubank" ,   648 ,         0 ,
      0 , "eco1"     , "querelli" ,  1415 ,         0 ,
      0 , "eco2"     , "pinubank" ,   648 ,         0 ,
      0 , "eco2"     , "querelli" ,  1415 ,         0
)

maxb_timeseries <- tibble::tribble(
  ~year , ~ecoregion , ~species   , ~maxb ,
      0 , "eco1"     , "pinubank" , 15000 ,
      0 , "eco1"     , "querelli" , 25000 ,
      0 , "eco2"     , "pinubank" , 15000 ,
      0 , "eco2"     , "querelli" , 25000
)

est_prob <- tibble::tribble(
  ~year , ~ecoregion , ~species   , ~probability ,
      0 , "eco1"     , "pinubank" , 0.1          ,
      0 , "eco1"     , "querelli" , 0.1          ,
      0 , "eco2"     , "pinubank" , 0.1          ,
      0 , "eco2"     , "querelli" , 0.1
)

root_dynamics <- tibble::tribble(
  ~ecoregion , ~species   , ~min_abio , ~root_abio , ~prop_fine_rt , ~fr_turnover , ~cr_turnover ,
  "eco1"     , "pinubank" ,         0 , 0.403      , 0.18          , 0.6          , 0.02         ,
  "eco1"     , "pinubank" ,      5000 , 0.292      , 0.1           , 0.6          , 0.02         ,
  "eco1"     , "querelli" ,         0 , 0.403      , 0.18          , 1            , 0.02         ,
  "eco2"     , "pinubank" ,         0 , 0.433      , 0.18          , 0.6          , 0.02         ,
  "eco2"     , "querelli" ,         0 , 0.403      , 0.18          , 1            , 0.02         ,
  "eco2"     , "querelli" ,      5000 , 0.292      , 0.1           , 0.6          , 0.02
)
```

## Extension configuration

``` r
## write the climate file
clim_file <- prepClimateFile(
  df = clim_df,
  path = tmp_pth,
  filename = "ForCS_climate.txt"
)

## write the disturbance matrix file
dm_file <- prepDisturbanceMatrixFile(
  DisturbFireTransferDOM = disturb_fire_dom,
  DisturbOtherTransferDOM = disturb_other_dom,
  DisturbFireTransferBiomass = disturb_fire_biomass,
  DisturbOtherTransferBiomass = disturb_other_biomass,
  path = tmp_pth,
  filename = "ForCS_DM.txt"
)

## write the snag file
snag_file <- prepSnagFile(
  snags,
  path = tmp_pth,
  filename = "ForCS_snags.txt"
)

## don't need working files, they just need to exist
init_comm_files <- c(
  file.path(tmp_pth, "initial-communities.txt"),
  file.path(tmp_pth, "initial-communities.gis")
)
purrr::walk2(.x = c("", ""), .y = init_comm_files, .f = writeLines)

## create the ForCS extension config object
ext_forcs <- ForCS$new(
  path = tmp_pth,
  Timestep = 1,
  SeedingAlgorithm = "WardSeedDispersal",
  ClimateFile = clim_file,
  InitialCommunitiesFiles = init_comm_files,
  DisturbanceMatrixFile = dm_file,
  SnagFile = snag_file,
  OutputTables = output_tables,
  SoilSpinupControls = soil_spinup,
  AvailableLightBiomass = avail_light_biomass,
  LightEstablishmentTable = light_est,
  SpeciesParameters = species_params,
  DOMPools = dom_pools,
  EcoSppDOMParameters = ecosppdom_params,
  ForCSProportions = forcs_props,
  ANPPTimeSeries = anpp_timeseries,
  MaxBiomassTimeSeries = maxb_timeseries,
  EstablishProbabilities = est_prob,
  RootDynamics = root_dynamics
)

## write the ForCS extension config file
ext_forcs$write()
```

## Verify configuration files

``` r
readLines(file.path(tmp_pth, ext_forcs$files[1])) |>
  cat(sep = "\n")
#> >> generated by `landisutils` (v0.0.1.9017) on Wed Mar 11 20:23:07 2026
#> >> do not edit by hand; manual changes to this file may be overwritten
#> 
#> LandisData  "ForC Succession"
#> 
#> Timestep    1
#> 
#> ClimateFile    "ForCS_climate.txt"
#> 
#> InitialCommunities    "initial-communities.txt"
#> InitialCommunitiesMap    "initial-communities.gis"
#> 
#> DisturbanceMatrixFile    "ForCS_DM.txt"
#> 
#> SnagFile    "ForCS_snags.txt"
#> 
#> ForCSOutput
#> >> Output interval
#> >> Biomass  DOM_pools  Fluxes  Summary
#> >> -----------------------------------
#>     1        1        1        1
#> 
#> SoilSpinup
#> >>  On/Off  Tolerance  Max
#> >>  Flag    %          Iterations
#> 1  1  20
#> 
#> AvailableLightBiomass
#> >>  Shade
#> >>  Class   Ecoregions
#>    eco1   eco2
#> 1  30%  30%
#> 2  35%  35%
#> 3  55%  55%
#> 4  80%  80%
#> 5  100%  100%
#> 
#> LightEstablishmentTable
#> >>  Spp Shade        Probability
#> >>  Class            by Actual Shade
#> >>  ----------------------------------
#> >>        0    1    2    3    4    5
#>    1            1.00  0.50  0.00  0.00  0.00  0.00
#>    2            0.50  1.00  0.50  0.00  0.00  0.00
#>    3            0.20  0.20  1.00  0.50  0.00  0.00
#>    4            0.10  0.10  0.20  1.00  0.50  0.50
#>    5            0.10  0.50  0.20  0.20  1.00  1.00
#> 
#> SpeciesParameters
#> >>  Species  Leaf  Mortal  Merchant  Merch    Merch     Prop       Growth
#> >>           Long  Shape   Stems     Shape    Shape     Non-merch  Shape
#> >>                 Param   Min Age   Param a  Param b   to FastAG  Param
#> >>  ---------------------------------------------------------------------
#> pinubank  3  10  10  0.7546  0.983  0.25  0.25
#> querelli  1  10  30  0.7546  0.983  0.25  0.25
#> 
#> DOMPools
#> >>  ID    Name            Proportion to
#> >>                        Atmosphere
#> >>  -----------------------------------
#>  1  Very Fast Aboveground  0.815
#>  2  Very Fast Belowground  0.830
#>  3  Fast Aboveground  0.830
#>  4  Fast Belowground  0.830
#>  5  Medium  0.830
#>  6  Slow Aboveground  1.000
#>  7  Slow Belowground  1.000
#>  8  Stem Snag  0.830
#>  9  Other Snag  0.830
#> 10  Extra pool  0.830
#> 
#> EcoSppDOMParameters
#> >>  Ecoregion  Species  DOM   Decay  Amount  Q10 Ref
#> >>                      Pool  Rate   at T0   Temp 10C
#> >>  -------------------------------------------------
#> eco1  pinubank   1  0.35500    53.04  2.65
#> eco1  pinubank   2  0.50000   295.40  2.00
#> eco1  pinubank   3  0.14350  1395.49  2.00
#> eco1  pinubank   4  0.03740  1360.62  2.00
#> eco1  pinubank   5  0.01500   863.88  2.00
#> eco1  pinubank   6  0.00330  1656.13  2.65
#> eco1  pinubank   7  0.01870  8451.22  2.00
#> eco1  pinubank   8  0.07175  7466.54  2.00
#> eco1  pinubank   9  0.07000  2036.14  2.00
#> eco1  pinubank  10  0.00000     0.00  2.00
#> eco2  pinubank   1  0.35500    53.04  2.65
#> eco2  pinubank   2  0.50000   295.40  2.00
#> eco2  pinubank   3  0.14350  1395.49  2.00
#> eco2  pinubank   4  0.03740  1360.62  2.00
#> eco2  pinubank   5  0.01500   863.88  2.00
#> eco2  pinubank   6  0.00330  1656.13  2.65
#> eco2  pinubank   7  0.01870  8451.22  2.00
#> eco2  pinubank   8  0.07175  7466.54  2.00
#> eco2  pinubank   9  0.07000  2036.14  2.00
#> eco2  pinubank  10  0.00000     0.00  2.00
#> eco1  querelli   1  0.35500    53.04  2.65
#> eco1  querelli   2  0.50000   295.40  2.00
#> eco1  querelli   3  0.14350  1395.49  2.00
#> eco1  querelli   4  0.03740  1360.62  2.00
#> eco1  querelli   5  0.01500   863.88  2.00
#> eco1  querelli   6  0.00330  1656.13  2.65
#> eco1  querelli   7  0.01870  8451.22  2.00
#> eco1  querelli   8  0.07175  7466.54  2.00
#> eco1  querelli   9  0.07000  2036.14  2.00
#> eco1  querelli  10  0.00000     0.00  2.00
#> eco2  querelli   1  0.35500    53.04  2.65
#> eco2  querelli   2  0.50000   295.40  2.00
#> eco2  querelli   3  0.14350  1395.49  2.00
#> eco2  querelli   4  0.03740  1360.62  2.00
#> eco2  querelli   5  0.01500   863.88  2.00
#> eco2  querelli   6  0.00330  1656.13  2.65
#> eco2  querelli   7  0.01870  8451.22  2.00
#> eco2  querelli   8  0.07175  7466.54  2.00
#> eco2  querelli   9  0.07000  2036.14  2.00
#> eco2  querelli  10  0.00000     0.00  2.00
#> 
#> ForCSProportions
#> >>  Biomass  Biomass  Annual     Annual     Annual
#> >>  Fine     Coarse   SlowAG     StemSnag   BranchSnag
#> >>                    to SlowBG  to Medium  to FastAG
#> >>  --------------------------------------------------
#> 0.5  0.5  0.006  0.032  0.1
#> 
#> ANPPTimeSeries
#> >>  Year  Ecoregion  Species  ANPP       ANPP-std
#> >>                            (g/m2/yr)
#> >>  ---------------------------------------------
#> 0  eco1  pinubank   648  0
#> 0  eco1  querelli  1415  0
#> 0  eco2  pinubank   648  0
#> 0  eco2  querelli  1415  0
#> 
#> MaxBiomassTimeSeries
#> >>  Year  Ecoregion  Species  Max Biomass (g/m2)
#> >>  --------------------------------------------
#> 0  eco1  pinubank  15000
#> 0  eco1  querelli  25000
#> 0  eco2  pinubank  15000
#> 0  eco2  querelli  25000
#> 
#> EstablishProbabilities
#> >>  Year  Ecoregion  Species  Probability
#> >>  -------------------------------------
#> 0  eco1  pinubank  0.1
#> 0  eco1  querelli  0.1
#> 0  eco2  pinubank  0.1
#> 0  eco2  querelli  0.1
#> 
#> RootDynamics
#> >>  Ecoregion  Species  MinABio  Root  PropFineRt  Frturnover  Crturnover
#> >>                      (g/m2)   Abio
#> >>  ---------------------------------------------------------------------
#> eco1  pinubank     0  0.403  0.18  0.6  0.02
#> eco1  pinubank  5000  0.292  0.10  0.6  0.02
#> eco1  querelli     0  0.403  0.18  1.0  0.02
#> eco2  pinubank     0  0.433  0.18  0.6  0.02
#> eco2  querelli     0  0.403  0.18  1.0  0.02
#> eco2  querelli  5000  0.292  0.10  0.6  0.02
```

``` r
readLines(file.path(tmp_pth, ext_forcs$files[2])) |>
  cat(sep = "\n")
#> >> generated by `landisutils` (v0.0.1.9017) on Wed Mar 11 20:23:07 2026
#> >> do not edit by hand; manual changes to this file may be overwritten
#> 
#> LandisData  "ForC Succession"
#> 
#> 
#> ClimateTable
#> >> Time  Ecoregion  AvgT
#> >> Step             (C)
#> >> ---------------------
#>  0  eco1  5.0
#>  0  eco2  2.0
#>  5  eco1  5.5
#>  5  eco2  2.5
#> 10  eco1  6.0
#> 10  eco2  3.0
#> 20  eco1  7.0
#> 20  eco2  5.0
```

*skip initial communities files*

``` r
readLines(file.path(tmp_pth, ext_forcs$files[5])) |>
  cat(sep = "\n")
#> >> generated by `landisutils` (v0.0.1.9017) on Wed Mar 11 20:23:07 2026
#> >> do not edit by hand; manual changes to this file may be overwritten
#> 
#> LandisData  "ForC Succession"
#> 
#> 
#> DisturbFireTransferDOM
#> >> Intensity  From     To   To   To
#> >>            Biomass  Air  DOM  FPS
#> >> -----------------------------------
#> 1  1  0.5  0  0
#> 1  3  0.2  0  0
#> 2  1  0.5  0  0
#> 2  2  0.25  0  0
#> 2  3  0.35  0  0
#> 2  8  0  0.5  0
#> 2  9  0  1  0
#> 3  1  0.65  0  0
#> 3  2  0.35  0  0
#> 3  3  0.4  0  0
#> 3  8  0  0.75  0
#> 3  9  0.5  0.5  0
#> 4  1  1  0  0
#> 4  2  0.5  0  0
#> 4  3  0.4  0  0
#> 4  5  0.1  0  0
#> 4  8  0  1  0
#> 4  9  0.7  0.3  0
#> 5  1  1  0  0
#> 5  2  0.65  0  0
#> 5  3  0.45  0  0
#> 5  5  0.1  0  0
#> 5  8  0.1  0.9  0
#> 5  9  0.7  0.3  0
#> 
#> DisturbOtherTransferDOM
#> >> Disturbance  From  To   To   To
#> >> Type         DOM   Air  DOM  FPS
#> >> --------------------------------
#> Clearcut  1  0  0.0  0.0
#> Clearcut  8  0  0.4  0.6
#> wind  8  0  0.8  0.0
#> wind  9  0  1.0  0.0
#> 
#> DisturbFireTransferBiomass
#> >> Intensity  From     To   To   To
#> >>            Biomass  Air  FPS  DOM
#> >> -----------------------------------
#> 1  1  0  0  1
#> 1  2  0.5  0  0.5
#> 1  3  0  0  1
#> 1  5  0  0  1
#> 1  6  0  0  1
#> 2  1  0  0  1
#> 2  2  0.7  0  0.3
#> 2  3  0.1  0  0.9
#> 2  5  0  0  1
#> 2  6  0  0  1
#> 3  1  0  0  1
#> 3  2  0.75  0  0.25
#> 3  3  0.1  0  0.9
#> 3  5  0  0  1
#> 3  6  0  0  1
#> 4  1  0  0  1
#> 4  2  1  0  0
#> 4  3  0.2  0  0.8
#> 4  5  0  0  1
#> 4  6  0  0  1
#> 5  1  0  0  1
#> 5  2  1  0  0
#> 5  3  0.3  0  0.7
#> 5  5  0  0  1
#> 5  6  0  0  1
#> 
#> DisturbOtherTransferBiomass
#> >> Disturbance  From     To   To   To
#> >> Type         Biomass  Air  FPS  DOM
#> >> -----------------------------------
#> Clearcut  1  0.0  1  0.0
#> Clearcut  2  0.0  0  1.0
#> Clearcut  3  0.0  0  1.0
#> Clearcut  5  0.0  0  1.0
#> Clearcut  6  0.0  0  1.0
#> wind  1  0.0  0  1.0
#> wind  2  0.0  0  1.0
#> wind  3  0.0  0  1.0
#> wind  5  0.0  0  1.0
#> wind  6  0.0  0  1.0
#> bda  1  0.0  0  1.0
#> bda  2  0.3  0  0.7
#> bda  3  0.2  0  0.8
#> bda  5  0.0  0  1.0
#> bda  6  0.0  0  1.0
```

``` r
readLines(file.path(tmp_pth, ext_forcs$files[6])) |>
  cat(sep = "\n")
#> >> generated by `landisutils` (v0.0.1.9017) on Wed Mar 11 20:23:07 2026
#> >> do not edit by hand; manual changes to this file may be overwritten
#> 
#> LandisData  "ForC Succession"
#> 
#> 
#> SnagData
#> >> Species  AgeAtDeath  TimeSinceDeath  Cause
#> >> ------------------------------------------
#> querelli  53   5  other
#> pinubank  88  12  bda
```

## Cleanup

``` r
withr::deferred_run()
#> Ran 1/1 deferred expressions
```
