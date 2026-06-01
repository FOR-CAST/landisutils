# Assembling a Multi-Extension Scenario

`landisutils` lets you compose a LANDIS-II scenario from one succession
extension, any number of disturbance extensions, and any number of
“other” (typically output) extensions. This vignette walks through
wiring up a representative mix of extensions and writing the full set of
input files to a scenario directory.

The data here are skeletal — the goal is to show the API, not to produce
a biologically-meaningful run. Each section’s parameter values are taken
verbatim from the corresponding extension’s upstream LANDIS-II Core8
test input on GitHub (cited in each chunk), so the demonstration mirrors
what the LANDIS-II developers themselves use to exercise each extension.
For real workflows, each `prep*()` helper would draw from
project-specific upstream data.

``` r

library(landisutils)

tmp_pth <- withr::local_tempdir("example_MultiExt_")
```

## Stub input files

[`scenario()`](https://for-cast.github.io/landisutils/reference/scenario.md)
validates that input files exist on disk. For a demonstration we just
`writeLines("")` empty stubs at each path the extensions will reference.

``` r

clim_file <- file.path(tmp_pth, "climate.txt")
init_comm_files <- c(
  file.path(tmp_pth, "initial-communities.csv"),
  file.path(tmp_pth, "initial-communities.tif")
)
spp_file <- file.path(tmp_pth, "SpeciesData.csv")
spperd_file <- file.path(tmp_pth, "SppEcoregionData.csv")
ecoregion_files <- c(file.path(tmp_pth, "ecoregions.txt"), file.path(tmp_pth, "ecoregions.tif"))
species_input_file <- file.path(tmp_pth, "Core_species_data.txt")

stub_files <- c(
  clim_file,
  init_comm_files,
  spp_file,
  spperd_file,
  ecoregion_files,
  species_input_file
)
for (f in stub_files) {
  writeLines("", f)
}
```

## Succession: Biomass Succession

A scenario must specify exactly one succession extension. Here we use
`BiomassSuccession`. Tables match the upstream Core8 test input verbatim
(<https://github.com/LANDIS-II-Foundation/Extension-Biomass-Succession/blob/master/testings/CoreV8.0-BiomassSuccession7.0/biomass-succession.txt>).

``` r

min_rel_b <- tibble::tribble(
  ~ShadeClass , ~Eco1 , ~Eco2 ,
  NA_integer_ , "101" , "102" ,
  1L          , "25%" , "25%" ,
  2L          , "45%" , "45%" ,
  3L          , "56%" , "56%" ,
  4L          , "70%" , "70%" ,
  5L          , "90%" , "90%"
)

suff_light <- tibble::tribble(
  ~class , ~X0 , ~X1 , ~X2  , ~X3  , ~X4  , ~X5  ,
  1L     , 1.0 , 0.5 , 0.25 , 0.0  , 0.0  , 0.0  ,
  2L     , 1.0 , 1.0 , 0.5  , 0.25 , 0.0  , 0.0  ,
  3L     , 1.0 , 1.0 , 1.0  , 0.5  , 0.25 , 0.0  ,
  4L     , 1.0 , 1.0 , 1.0  , 1.0  , 0.5  , 0.25 ,
  5L     , 0.1 , 0.5 , 1.0  , 1.0  , 1.0  , 1.0
)

erp_df <- tibble::tribble(
  ~ecoregion , ~AET ,
  "101"      ,  600 ,
  "102"      ,  600
)

frp_df <- tibble::tribble(
  ~Severity , ~WoodLitterReduct , ~LitterReduct ,
  1L        , 0.0               , 0.5           ,
  2L        , 0.0               , 0.75          ,
  3L        , 0.0               , 1.0
)

hrp_df <- tibble::tribble(
  ~Name            , ~WoodLitterReduct , ~LitterReduct , ~CohortWoodRemoval , ~CohortLeafRemoval ,
  "MaxAgeClearcut" , 0.5               , 0.15          , 0.8                , 0.0                ,
  "PatchCutting"   , 1.0               , 1.0           , 1.0                , 0.0
)

ext_succ <- BiomassSuccession$new(
  path = tmp_pth,
  Timestep = 10,
  SeedingAlgorithm = "WardSeedDispersal",
  InitialCommunitiesFiles = init_comm_files,
  ClimateConfigFile = clim_file,
  SpinupCohorts = FALSE,
  SpinupMortalityFraction = 0.05,
  MinRelativeBiomass = min_rel_b,
  SufficientLight = suff_light,
  SpeciesDataFile = spp_file,
  EcoregionParameters = erp_df,
  SpeciesEcoregionDataFile = spperd_file,
  FireReductionParameters = frp_df,
  HarvestReductionParameters = hrp_df
)
ext_succ$write()
```

## Disturbance 1: Original Fire

`OriginalFire` writes a `FireRegionParametersTable` (one row per fire
regime), references a fire-species CSV and an initial-fire-regions
raster, and accepts fuel-curve / wind-curve / fire-damage tables. Values
below come from the upstream Core8 test input
(<https://github.com/LANDIS-II-Foundation/Extension-Base-Fire/blob/master/testings/Core8.0-OriginalFire5.0/original-fire.txt>).

``` r

fire_spp_file <- file.path(tmp_pth, "species-fire.csv")
fire_regions_map <- file.path(tmp_pth, "fire-regions-map.tif")
for (f in c(fire_spp_file, fire_regions_map)) {
  writeLines("", f)
}

frp_table <- data.frame(
  FireRegionName = c("eco1", "eco2"),
  MapCode = c(1L, 2L),
  MeanSize = c(100, 200),
  MinSize = c(4, 6),
  MaxSize = c(400, 600),
  IgnitionProb = c(0.001, 0.001),
  k = c(100L, 50L)
)

fuel_curve <- data.frame(
  FireRegionName = c("eco1", "eco2"),
  S1 = c(10, 5),
  S2 = c(20, 15),
  S3 = c(50, 20),
  S4 = c(70, -1),
  S5 = c(120, -1)
)

wind_curve <- data.frame(
  FireRegionName = c("eco1", "eco2"),
  S5 = c(-1, 1),
  S4 = c(-1, 5),
  S3 = c(1, 15),
  S2 = c(10, 20),
  S1 = c(20, 30)
)

ext_fire <- OriginalFire$new(
  path = tmp_pth,
  Timestep = 5L,
  Species_CSV_File = fire_spp_file,
  FireRegionParametersTable = frp_table,
  InitialFireRegionsMap = fire_regions_map,
  FuelCurveTable = fuel_curve,
  WindCurveTable = wind_curve,
  FireDamageTable = defaultFireDamageTable()
)
ext_fire$write()
```

## Disturbance 2: EDA with one agent

EDA wraps any number of pathogen agents. Each
[`edaAgent()`](https://for-cast.github.io/landisutils/reference/edaAgent.md)
call emits a per-agent `eda/<name>.txt` file referenced from the main
`EDA` config. Parameters and species rows below come from the upstream
Core8 *Phytophthora ramorum* test agent
(<https://github.com/LANDIS-II-Foundation/Extension-Base-EDA/blob/master/tests/Core8.0-EDA3.0/P_ramorum.txt>).

``` r

eda_sp <- data.frame(
  Species = c("Umbecali", "Lithdens"),
  LowAge = c(5, 5),
  LowScore = c(3, 2),
  MediumAge = c(15, 20),
  MediumScore = c(6, 4),
  HighAge = c(40, 60),
  HighScore = c(10, 7),
  VulnLowAge = c(999, 5),
  VulnLowMortProb = c(0, 0.14),
  VulnMediumAge = c(999, 15),
  VulnMediumMortProb = c(0, 0.25),
  VulnHighAge = c(999, 30),
  VulnHighMortProb = c(0, 0.30),
  CFSConifer = c("no", "yes"),
  MortalityPlot = c("no", "yes")
)

ext_eda <- EDA$new(
  path = tmp_pth,
  Timestep = 1L,
  Agents = list(edaAgent(
    name = "ramorum",
    SHIMode = "mean",
    TransmissionRate = 1.8,
    AcquisitionRate = 0.4,
    DispersalType = "STATIC",
    DispersalKernel = "PowerLaw",
    DispersalMaxDist = 1000,
    AlphaCoef = 3.55,
    EDASpeciesParameters = eda_sp
  ))
)
ext_eda$write()

ext_eda$files
#> [1] "eda.txt"         "eda/ramorum.txt"
```

## Other 1: Land Use Plus

`LandUsePlus` accepts a list of
[`landUseType()`](https://for-cast.github.io/landisutils/reference/landUseType.md)
sub-objects, each containing one or more
[`landCoverChange()`](https://for-cast.github.io/landisutils/reference/landCoverChange.md)
blocks. The three land-use types below are taken from the upstream Core8
test input
(<https://github.com/LANDIS-II-Foundation/Extension-Land-Use-Plus/blob/master/testing/Core8.0-LUC4.0/land-use-change-inputs.txt>).

``` r

ext_lu <- LandUsePlus$new(
  path = tmp_pth,
  Timestep = 1L,
  InputMaps = "landuse-{timestep}.tif",
  SiteLog = "output/land-use/site-log.csv",
  LandUses = list(
    landUseType(
      name = "forest",
      mapCode = 4L,
      allowHarvest = "no",
      changes = landCoverChange(type = "NoChange")
    ),
    landUseType(
      name = "FIPSandHarvest",
      mapCode = 3L,
      allowHarvest = "yes",
      changes = list(
        landCoverChange(
          type = "RemoveTrees",
          repeatHarvest = TRUE,
          cohorts = list(
            cohortSelector(
              species = "acerrubr",
              ranges = data.frame(low = 1, high = 200, percent = 20)
            ),
            cohortSelector(
              species = "pinustro",
              ranges = data.frame(low = 50, high = 300, percent = 30)
            )
          )
        ),
        landCoverChange(
          type = "InsectDefoliation",
          cohorts = cohortSelector(
            species = "querrubr",
            ranges = data.frame(low = 1, high = 300, percent = 60)
          )
        )
      )
    ),
    landUseType(
      name = "FIPS",
      mapCode = 1L,
      allowHarvest = "yes",
      changes = landCoverChange(
        type = "InsectDefoliation",
        cohorts = list(
          cohortSelector(
            species = "querrubr",
            ranges = data.frame(low = 1, high = 300, percent = 75)
          ),
          cohortSelector(
            species = "pinustro",
            ranges = data.frame(low = c(1, 71), high = c(62, 200), percent = c(20, 25))
          )
        )
      )
    )
  )
)
ext_lu$write()
```

## Other 2: Local Habitat Suitability output

Suitability files are passed as
[`suitabilityFile()`](https://for-cast.github.io/landisutils/reference/suitabilityFile.md)
sub-objects (or as a plain character vector that’s auto-wrapped). The
four files below mirror the upstream Core8 test input
(<https://github.com/LANDIS-II-Foundation/Extension-Local-Habitat-Suitability-Output/blob/master/test/Core8-LocalHabitat3.0/Habitat_input.txt>).

``` r

ext_lh <- OutputLocalHabitat$new(
  path = tmp_pth,
  Timestep = 1L,
  OutputTimestep = 10L,
  MapFileNames = "output/habitat/{HabitatName}-{timestep}.tif",
  SuitabilityFiles = c(
    "AgeClass_ForestType_example.txt",
    "AgeClass_TimeSinceFire_example.txt",
    "ForestType_TimeSinceFire_example.txt",
    "ForestType_TimeSinceHarvest_example.txt"
  )
)
ext_lh$write()
```

## Assemble the scenario

[`scenario()`](https://for-cast.github.io/landisutils/reference/scenario.md)
partitions the supplied `extensions` list by `type` (succession /
disturbance / other) and writes the top-level scenario `.txt`
referencing each extension’s primary input file.

``` r

climate_cfg <- LandisClimateConfig$new(path = tmp_pth)
climate_cfg$add_file(basename(clim_file))

scen <- scenario(
  name = "multi_ext_demo",
  extensions = list(ext_succ, ext_fire, ext_eda, ext_lu, ext_lh),
  climate_config = climate_cfg,
  path = tmp_pth,
  CellLength = 100,
  DisturbancesRandomOrder = FALSE,
  Duration = 10,
  EcoregionsFiles = ecoregion_files,
  RandomNumberSeed = 147,
  SpeciesInputFile = species_input_file
)

scen$path
#> /tmp/RtmpTxKs5i/example_MultiExt_31f26ddba9dc
```

## Inspect the generated scenario file

``` r

readLines(file.path(scen$path, "multi_ext_demo.txt")) |> cat(sep = "\n")
#> >> generated by `landisutils` (v0.0.22) on Mon Jun  1 15:19:03 2026
#> >> do not edit by hand; manual changes to this file may be overwritten
#> 
#> LandisData  "Scenario"
#> 
#> Duration    10
#> 
#> Species    Core_species_data.txt
#> 
#> Ecoregions    ecoregions.txt
#> EcoregionsMap    ecoregions.tif
#> 
#> CellLength    100
#> 
#> >> Succession Extension    Initialization File
#> >> --------------------    -------------------
#>    "Biomass Succession"        biomass-succession.txt
#> 
#> >> Disturbance Extensions    Initialization File
#> >> ----------------------    -------------------
#>    "Original Fire"        original-fire.txt
#>    "EDA"        eda.txt
#>    "Land Use Change"        land-use.txt
#> 
#> DisturbancesRandomOrder    no
#> 
#> >> Other Extensions            Initialization File
#> >> ------------------------    -----------------------
#>    "Local Habitat Output"        output-local-habitat.txt
#> 
#> RandomNumberSeed    147  << optional parameter
```

## Cleanup

``` r

withr::deferred_run()
#> Ran 1/1 deferred expressions
```
