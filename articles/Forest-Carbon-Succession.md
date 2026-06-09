# Preparing Forest Carbon Succession (ForCS) Inputs

``` r

library(landisutils)

tmp_pth <- withr::local_tempdir("example_ForCS_")
```

## Sample extension inputs

The `ForCS` R6 class targets the **v4.0.2 schema** (LANDIS-II v8 core).
Sample values below are taken verbatim from the upstream v8 test
scenario and the v4.0.2 user guide:

- <https://github.com/LANDIS-II-Foundation/Extension-ForCS-Succession/blob/master/testing/v8%20Scenario/FORC-successionv4simpleroots.txt>
- <https://github.com/LANDIS-II-Foundation/Extension-ForCS-Succession/blob/master/deploy/installer/LANDIS-II%20CForC%20Succession%20v4.0.2%20User%20Guide%20September%202025.pdf>

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
  "querelli" ,            40 ,                 1 , "other" ,
  "pinubank" ,            88 ,                12 , "wind"
)

output_tables <- data.frame(Biomass = 1, DOM_Pools = 1, Fluxes = 1, Summary = 1)

for_cs_map_control <- data.frame(
  BiomassC = 1, SDOMC = 1, NBP = 1, NEP = 1, NPP = 1, RH = 1, ToFPS = 1
)

spin_up <- data.frame(Flag = 1, BiomassSpinUpFlag = 1, Tolerance = 1.0, MaxIter = 20)

avail_light_biomass <- data.frame(
  Class = 1L:5L,
  eco1 = c(30, 35, 55, 80, 100),
  eco2 = c(30, 35, 55, 80, 100)
)

light_est <- data.frame(
  class = 1L:5L,
  X0 = c(1.0, 0.5, 0.0, 0.0, 0.0),
  X1 = c(0.0, 1.0, 0.5, 0.0, 0.0),
  X2 = c(0.0, 0.0, 1.0, 0.5, 0.0),
  X3 = c(0.0, 0.0, 0.5, 1.0, 0.5),
  X4 = c(0.0, 0.0, 0.0, 0.0, 1.0),
  X5 = c(0.0, 0.0, 0.0, 0.0, 0.5)
)

## v4.0.2 schema: 10 columns (adds ShadeTolerance + FireTolerance)
species_params <- tibble::tribble(
  ~species   , ~leaf_long , ~mort_shp , ~merch_min_age , ~merch_a , ~merch_b , ~prop_non_merch , ~growth_shp , ~shade_tol , ~fire_tol ,
  "pinubank" , 3.0        ,        10 ,              5 , 0.7546   , 0.983    , 0.25            , 0.9         ,         1L ,        2L ,
  "querelli" , 1.0        ,        10 ,              5 , 0.7546   , 0.983    , 0.25            , 0.9         ,         2L ,        4L
)

dom_pools <- tibble::tribble(
  ~id , ~name                   , ~prop_to_atmosphere ,
    1 , "Very Fast Aboveground" , 0.815               ,
    2 , "Very Fast Belowground" , 0.83                ,
    3 , "Fast Aboveground"      , 0.83                ,
    4 , "Fast Belowground"      , 0.83                ,
    5 , "Medium"                , 0.83                ,
    6 , "Slow Aboveground"      , 0.83                ,
    7 , "Slow Belowground"      , 0.83                ,
    8 , "Stem Snag"             , 0.83                ,
    9 , "Other Snag"            , 0.83                ,
   10 , "Extra pool"            , 0.83
)

## EcoSppDOMParameters: per-pool decay parameters (v4.0.2 user guide §3 sample)
ecosppdom_pinubank <- tibble::tribble(
  ~dom_pool , ~decay_rate , ~amt_t0 , ~q10 ,
          1 , 0.355       ,    1.49 , 2.65 ,
          2 , 0.5         ,    0.07 , 2    ,
          3 , 0.1435      ,  158.48 , 2    ,
          4 , 0.0374      ,  288.71 , 2    ,
          5 , 0.015       , 1349.40 , 2    ,
          6 , 0.0033      , 1927.71 , 2    ,
          7 , 0.0187      ,  851.21 , 2    ,
          8 , 0.07175     ,  314.88 , 2    ,
          9 , 0.07        ,   45.53 , 2    ,
         10 , 0           ,    0.00 , 2
)
ecosppdom_querelli <- tibble::tribble(
  ~dom_pool , ~decay_rate , ~amt_t0 , ~q10 ,
          1 , 0.355       ,    0.34 , 2.65 ,
          2 , 0.5         ,    0.02 , 2    ,
          3 , 0.1435      ,    5.15 , 2    ,
          4 , 0.0374      ,  143.23 , 2    ,
          5 , 0.015       , 2476.27 , 2    ,
          6 , 0.0033      , 4075.40 , 2    ,
          7 , 0.0187      , 2339.42 , 2    ,
          8 , 0.07175     ,    7.45 , 2    ,
          9 , 0.07        ,    1.97 , 2    ,
         10 , 0           ,    0.00 , 2
)
ecosppdom_params <- dplyr::bind_rows(
  dplyr::mutate(ecosppdom_pinubank, ecoregion = "eco1", species = "pinubank"),
  dplyr::mutate(ecosppdom_pinubank, ecoregion = "eco2", species = "pinubank"),
  dplyr::mutate(ecosppdom_querelli, ecoregion = "eco1", species = "querelli"),
  dplyr::mutate(ecosppdom_querelli, ecoregion = "eco2", species = "querelli")
) |>
  dplyr::select(ecoregion, species, dom_pool, decay_rate, amt_t0, q10)

forcs_props <- data.frame(
  BiomassFine = 0.5,
  BiomassCoarse = 0.5,
  AnnualSlowAGtoSlowBG = 0.006,
  AnnualStemSnagToMedium = 0.032,
  AnnualBranchSnagToFastAG = 0.1
)

anpp_timeseries <- tibble::tribble(
  ~year , ~ecoregion , ~species   , ~anpp , ~anpp_std ,
      0 , "eco1"     , "pinubank" ,   648 ,       0.1 ,
      0 , "eco1"     , "querelli" ,  1415 ,       0.1 ,
      0 , "eco2"     , "pinubank" ,   648 ,       0.1 ,
      0 , "eco2"     , "querelli" ,  1415 ,       0.1
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

## "simpleroots" variant: one row per (ecoregion, species), MinABio = 0
root_dynamics <- tibble::tribble(
  ~ecoregion , ~species   , ~min_abio , ~root_abio , ~prop_fine_rt , ~fr_turnover , ~cr_turnover ,
  "eco1"     , "pinubank" ,         0 , 0.399      , 0.18          , 0.6          , 0.02         ,
  "eco1"     , "querelli" ,         0 , 0.403      , 0.18          , 1            , 0.02         ,
  "eco2"     , "pinubank" ,         0 , 0.399      , 0.18          , 0.6          , 0.02         ,
  "eco2"     , "querelli" ,         0 , 0.403      , 0.18          , 1            , 0.02
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
  ForCSClimateFile = clim_file,
  InitialCommunitiesFiles = init_comm_files,
  DisturbanceMatrixFile = dm_file,
  SnagFile = snag_file,
  OutputTables = output_tables,
  ForCSMapControl = for_cs_map_control,
  MapOutputInterval = 1,
  SpinUp = spin_up,
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
#> >> generated by `landisutils` (v0.0.36) on Tue Jun  9 22:13:49 2026
#> >> do not edit by hand; manual changes to this file may be overwritten
#> 
#> LandisData  "ForC Succession"
#> 
#> Timestep    1
#> 
#> SeedingAlgorithm    WardSeedDispersal
#> 
#> ForCSClimateFile    ForCS_climate.txt
#> 
#> InitialCommunities    initial-communities.txt
#> InitialCommunitiesMap    initial-communities.gis
#> 
#> DisturbanceMatrixFile    ForCS_DM.txt
#> 
#> SnagFile    ForCS_snags.txt
#> 
#> ForCSOutput
#> >> Output interval
#> >> Biomass  DOM_pools  Fluxes  Summary
#> >> -----------------------------------
#>     1        1        1        1
#> 
#> ForCSMapControl
#> >>  BiomassC  SDOMC  NBP  NEP  NPP  RH  ToFPS
#> >>  -----------------------------------------
#> 1  1  1  1  1  1  1
#> 
#> MapOutputInterval    1
#> 
#> SpinUp
#> >>  On/Off  Biomass    Tolerance  Max
#> >>  Flag    Spin-up    %          Iterations
#> >>          Flag
#> >>  ----------------------------------------
#> 1  1  1  20
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
#>    1            1.00  0.00  0.00  0.00  0.00  0.00
#>    2            0.50  1.00  0.00  0.00  0.00  0.00
#>    3            0.00  0.50  1.00  0.50  0.00  0.00
#>    4            0.00  0.00  0.50  1.00  0.00  0.00
#>    5            0.00  0.00  0.00  0.50  1.00  0.50
#> 
#> SpeciesParameters
#> >>  Species  Leaf  Mortal  Merchant  Merch    Merch     Prop       Growth  Shade      Fire
#> >>           Long  Shape   Stems     Shape    Shape     Non-merch  Shape   Tolerance  Tolerance
#> >>                 Param   Min Age   Param a  Param b   to FastAG  Param
#> >>  ----------------------------------------------------------------------------------------
#> pinubank  3  10  5  0.7546  0.983  0.25  0.9  1  2
#> querelli  1  10  5  0.7546  0.983  0.25  0.9  2  4
#> 
#> DOMPools
#> >>  ID    Name            Proportion to
#> >>                        Atmosphere
#> >>  -----------------------------------
#>  1  "Very Fast Aboveground"  0.815
#>  2  "Very Fast Belowground"  0.830
#>  3  "Fast Aboveground"  0.830
#>  4  "Fast Belowground"  0.830
#>  5  "Medium"  0.830
#>  6  "Slow Aboveground"  0.830
#>  7  "Slow Belowground"  0.830
#>  8  "Stem Snag"  0.830
#>  9  "Other Snag"  0.830
#> 10  "Extra pool"  0.830
#> 
#> EcoSppDOMParameters  "ForCS_EcoSppDOMParameters.csv"
#> 
#> ForCSProportions
#> >>  Biomass  Biomass  Annual     Annual     Annual
#> >>  Fine     Coarse   SlowAG     StemSnag   BranchSnag
#> >>                    to SlowBG  to Medium  to FastAG
#> >>  --------------------------------------------------
#> 0.5  0.5  0.006  0.032  0.1
#> 
#> ANPPTimeSeries   "ForCS_ANPPTimeSeries.csv"
#> 
#> MaxBiomassTimeSeries "ForCS_MaxBiomassTimeSeries.csv"
#> 
#> EstablishProbabilities   "ForCS_EstablishProbabilities.csv"
#> 
#> RootDynamics
#> >>  Ecoregion  Species  MinABio  Root  PropFineRt  Frturnover  Crturnover
#> >>                      (g/m2)   Abio
#> >>  ---------------------------------------------------------------------
#> eco1  pinubank  0  0.399  0.18  0.6  0.02
#> eco1  querelli  0  0.403  0.18  1.0  0.02
#> eco2  pinubank  0  0.399  0.18  0.6  0.02
#> eco2  querelli  0  0.403  0.18  1.0  0.02
```

``` r

readLines(file.path(tmp_pth, ext_forcs$files[2])) |>
  cat(sep = "\n")
#> >> generated by `landisutils` (v0.0.36) on Tue Jun  9 22:13:49 2026
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
#> >> generated by `landisutils` (v0.0.36) on Tue Jun  9 22:13:49 2026
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
#> >> generated by `landisutils` (v0.0.36) on Tue Jun  9 22:13:49 2026
#> >> do not edit by hand; manual changes to this file may be overwritten
#> 
#> LandisData  "ForC Succession"
#> 
#> 
#> SnagData
#> >> Species  AgeAtDeath  TimeSinceDeath  Cause
#> >> ------------------------------------------
#> querelli  40   1  other
#> pinubank  88  12  wind
```

## Cleanup

``` r

withr::deferred_run()
#> Ran 1/1 deferred expressions
```
