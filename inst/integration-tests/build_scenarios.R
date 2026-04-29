#!/usr/bin/env Rscript

## Build runnable LANDIS-II scenarios for integration testing.
##
## Each scenario uses upstream LANDIS-II Foundation reference inputs (downloaded
## at run time from the relevant extension repository's `testings/` directory)
## as the data files (initial communities, ecoregions, species, climate, ...),
## and uses the {landisutils} package API to (re)generate the LANDIS-II
## scenario file and per-extension config files on top of that data.
##
## A LANDIS-II run on each resulting directory therefore validates the syntax
## of the package-generated config files against the LANDIS-II console parser.
##
## Usage: Rscript inst/integration-tests/build_scenarios.R <output-dir>
##   On exit, prints the absolute path of each scenario subdirectory to stdout
##   (one per line), so the caller can iterate over them.

suppressPackageStartupMessages({
  library(landisutils)
})

args <- commandArgs(trailingOnly = TRUE)
if (length(args) < 1L) {
  stop("usage: build_scenarios.R <output-dir>", call. = FALSE)
}
out_dir <- args[1]
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
out_dir <- normalizePath(out_dir, mustWork = TRUE)

message(sprintf("Building scenarios in: %s", out_dir))

download_refs <- function(base_url, dest_dir, filenames) {
  dir.create(dest_dir, recursive = TRUE, showWarnings = FALSE)
  for (f in filenames) {
    src <- paste0(base_url, "/", f)
    dst <- file.path(dest_dir, f)
    message(sprintf("  fetching: %s", f))
    rc <- utils::download.file(src, dst, mode = "wb", quiet = TRUE)
    if (rc != 0L || !file.exists(dst)) {
      stop(sprintf("failed to download %s", src), call. = FALSE)
    }
  }
}

scenarios <- character(0)

## ---------------------------------------------------------------------------
## Scenario: Biomass Succession (no disturbances, no outputs)
##
## Reference data: LANDIS-II-Foundation/Extension-Biomass-Succession,
##   testings/CoreV8.0-BiomassSuccession7.0
## ---------------------------------------------------------------------------
{
  scen_name <- "biomass_succession"
  scen_path <- file.path(out_dir, scen_name)

  ref_base <- paste0(
    "https://raw.githubusercontent.com/",
    "LANDIS-II-Foundation/Extension-Biomass-Succession/master/",
    "testings/CoreV8.0-BiomassSuccession7.0"
  )
  ref_files <- c(
    "Core_species_data.txt",
    "SpeciesData.csv",
    "SppEcoregionData.csv",
    "biomass-succession_ClimateGenerator.txt",
    "biomass-succession_InitialCommunities.csv",
    "ecoregions.tif",
    "ecoregions.txt",
    "initial-communities.tif",
    ## referenced by biomass-succession_ClimateGenerator.txt (ClimateFile +
    ## SpinUpClimateFile); LANDIS-II resolves this relative to the scenario dir.
    "PRISM_data_AFRI_4.18.13_v2.csv"
  )
  download_refs(ref_base, scen_path, ref_files)

  ## Values mirror the upstream reference biomass-succession.txt so that they
  ## are compatible with the reference SpeciesData.csv / SppEcoregionData.csv /
  ## ecoregions.txt the file points at.
  min_rel_b <- tibble::tribble(
    ~ShadeClass , ~Eco1 , ~Eco2  ,
    NA_integer_ , "101" , "102"  ,
    1L          , "25%" , "25%"  ,
    2L          , "45%" , "45%"  ,
    3L          , "56%" , "56%"  ,
    4L          , "70%" , "70%"  ,
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

  ext_bs <- BiomassSuccession$new(
    path = scen_path,
    Timestep = 10,
    SeedingAlgorithm = "WardSeedDispersal",
    InitialCommunitiesFiles = c(
      file.path(scen_path, "biomass-succession_InitialCommunities.csv"),
      file.path(scen_path, "initial-communities.tif")
    ),
    ClimateConfigFile = file.path(scen_path, "biomass-succession_ClimateGenerator.txt"),
    CalibrateMode = NULL,
    SpinupCohorts = FALSE,
    SpinupMortalityFraction = 0.05,
    MinRelativeBiomass = min_rel_b,
    SufficientLight = suff_light,
    SpeciesDataFile = file.path(scen_path, "SpeciesData.csv"),
    EcoregionParameters = erp_df,
    SpeciesEcoregionDataFile = file.path(scen_path, "SppEcoregionData.csv"),
    FireReductionParameters = frp_df,
    HarvestReductionParameters = hrp_df
  )
  ext_bs$write()

  climate_cfg <- LandisClimateConfig$new(path = scen_path)
  climate_cfg$add_file("biomass-succession_ClimateGenerator.txt")

  scen <- scenario(
    name = scen_name,
    extensions = list(ext_bs),
    climate_config = climate_cfg,
    path = scen_path,
    CellLength = 100,
    DisturbancesRandomOrder = FALSE,
    Duration = 10,
    EcoregionsFiles = c(
      file.path(scen_path, "ecoregions.txt"),
      file.path(scen_path, "ecoregions.tif")
    ),
    RandomNumberSeed = 147,
    SpeciesInputFile = file.path(scen_path, "Core_species_data.txt")
  )

  scenarios <- c(scenarios, scen$path)
}

## ---------------------------------------------------------------------------
## Scenario: Biomass Succession + the extensions installed in the
##           `landis-ii-v8-release` docker image.
##
## The image bundles a curated subset of the LANDIS-II ecosystem; extensions
## like Deer Browse, Root Rot, EDA, Hurricane, LandUsePlus, OutputLandscape-
## Habitat, OutputBiomassPnET and PnETSuccession are NOT registered in it, so
## including them here would cause LANDIS-II to abort scenario parsing with
## "No extension with the name ...". This scenario therefore only exercises:
##   - OutputBiomassCommunity                      (other)
##   - OutputLocalHabitat / OutputWildlifeHabitat  (other; suitabilityFile())
##
## Two image-supported extensions parse cleanly but need fuller fixtures
## before they will run end-to-end, so they are intentionally omitted here:
##   - MagicHarvest: parser accepts the package-generated `magic-harvest.txt`
##     (the `NoHarvestReInitialization` field correctly emits the boolean
##     token via `truefalse()`), but the extension's `Run()` needs both an
##     actual wrapped harvest extension (e.g. Biomass Harvest) in the
##     scenario and a real `ProcessToLaunch` / `ProcessArguments` pair
##     pointing at an external script.
##   - ForestRoadsSimulation: parser accepts the package-generated config,
##     but the extension's `Initialize()` needs ecologically meaningful
##     raster inputs (a buildable-zone map and an initial road network
##     with sawmill/main-road cells) plus a harvest extension to drive
##     road construction.
##
## Reuses the same reference inputs as the first scenario (downloaded again
## into its own directory). The intent is to exercise `$write()` on each
## supported class against the LANDIS-II console parser; the ecological
## inputs themselves are skeletal.
## ---------------------------------------------------------------------------
{
  scen_name <- "biomass_succession_plus_extras"
  scen_path <- file.path(out_dir, scen_name)

  download_refs(ref_base, scen_path, ref_files)

  ext_bs2 <- BiomassSuccession$new(
    path = scen_path,
    Timestep = 10,
    SeedingAlgorithm = "WardSeedDispersal",
    InitialCommunitiesFiles = c(
      file.path(scen_path, "biomass-succession_InitialCommunities.csv"),
      file.path(scen_path, "initial-communities.tif")
    ),
    ClimateConfigFile = file.path(scen_path, "biomass-succession_ClimateGenerator.txt"),
    CalibrateMode = NULL,
    SpinupCohorts = FALSE,
    SpinupMortalityFraction = 0.05,
    MinRelativeBiomass = min_rel_b,
    SufficientLight = suff_light,
    SpeciesDataFile = file.path(scen_path, "SpeciesData.csv"),
    EcoregionParameters = erp_df,
    SpeciesEcoregionDataFile = file.path(scen_path, "SppEcoregionData.csv"),
    FireReductionParameters = frp_df,
    HarvestReductionParameters = hrp_df
  )
  ext_bs2$write()

  ## Suitability files are loaded by the habitat output extensions; pull a
  ## real example from each upstream extension's test/ directory.
  download_refs(
    paste0(
      "https://raw.githubusercontent.com/",
      "LANDIS-II-Foundation/Extension-Local-Habitat-Suitability-Output/master/",
      "test/Core8-LocalHabitat3.0"
    ),
    scen_path,
    c("AgeClass_ForestType_example.txt")
  )
  download_refs(
    paste0(
      "https://raw.githubusercontent.com/",
      "LANDIS-II-Foundation/Extension-Output-Wildlife-Habitat/master/",
      "test/Core8-Wildlife3.0"
    ),
    scen_path,
    c("BBWOAgeForestType.txt")
  )

  ext_bc <- OutputBiomassCommunity$new(path = scen_path, Timestep = 10L)
  ext_bc$write()

  ext_lh <- OutputLocalHabitat$new(
    path = scen_path,
    Timestep = 10L, OutputTimestep = 10L,
    SuitabilityFiles = c("AgeClass_ForestType_example.txt")
  )
  ext_lh$write()

  ext_wh <- OutputWildlifeHabitat$new(
    path = scen_path,
    Timestep = 10L, OutputTimestep = 10L,
    SuitabilityFiles = c("BBWOAgeForestType.txt")
  )
  ext_wh$write()

  climate_cfg2 <- LandisClimateConfig$new(path = scen_path)
  climate_cfg2$add_file("biomass-succession_ClimateGenerator.txt")

  scen2 <- scenario(
    name = scen_name,
    extensions = list(
      ext_bs2,
      ext_bc, ext_lh, ext_wh
    ),
    climate_config = climate_cfg2,
    path = scen_path,
    CellLength = 100,
    DisturbancesRandomOrder = FALSE,
    Duration = 10,
    EcoregionsFiles = c(
      file.path(scen_path, "ecoregions.txt"),
      file.path(scen_path, "ecoregions.tif")
    ),
    RandomNumberSeed = 147,
    SpeciesInputFile = file.path(scen_path, "Core_species_data.txt")
  )

  scenarios <- c(scenarios, scen2$path)
}

## ---------------------------------------------------------------------------
## Emit the scenario directories so the workflow can iterate over them.
## ---------------------------------------------------------------------------
cat(scenarios, sep = "\n")
cat("\n")
