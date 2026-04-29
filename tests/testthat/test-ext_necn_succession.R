testthat::test_that("NECN Succession inputs are properly created", {
  ## NOTE: reference values from the LANDIS-II Core v8 / NECN Succession v8 test input:
  ## testing/Core8-NECN8-Landscape/NECN_LTB_landscape.txt
  ## <https://github.com/LANDIS-II-Foundation/Extension-NECN-Succession/tree/master/testing/Core8-NECN8-Landscape>

  tmp_pth <- withr::local_tempdir("test_NECNSuccession_")

  ## stub input files -- the scaffolding only checks that referenced paths exist,
  ## not that their contents are valid LANDIS-II files.
  touch <- function(name) {
    f <- file.path(tmp_pth, name)
    writeLines("", f)
    f
  }

  ic_csv <- touch("Initial_Community_Landscape.csv")
  ic_map <- touch("initial-communities.tif")
  climate_cfg <- touch("climate-generator-baseline.txt")
  species_csv <- touch("NECN_Spp_Table.csv")

  ## Soil / initial SOM / dead-wood map filenames from the upstream reference.
  soil_map_names <- list(
    SoilDepthMapName              = "random110.tif",
    SoilDrainMapName              = "constantpoint75.tif",
    SoilBaseFlowMapName           = "constantpoint4.tif",
    SoilStormFlowMapName          = "constantpoint4.tif",
    SoilFieldCapacityMapName      = "random_point1_point2.tif",
    SoilWiltingPointMapName       = "random_point05_point099.tif",
    SoilPercentClayMapName        = "random_point01_point5.tif",
    SoilPercentSandMapName        = "random_point01_point5.tif",
    InitialSOM1CsurfMapName       = "random110.tif",
    InitialSOM1NsurfMapName       = "random6.tif",
    InitialSOM1CsoilMapName       = "random110.tif",
    InitialSOM1NsoilMapName       = "random9.tif",
    InitialSOM2CMapName           = "random4500.tif",
    InitialSOM2NMapName           = "random145.tif",
    InitialSOM3CMapName           = "random1294.tif",
    InitialSOM3NMapName           = "random50.tif",
    InitialDeadWoodSurfaceMapName = "random110.tif",
    InitialDeadWoodRootsMapName   = "random50.tif" ## upstream: "InitialDeadCoarseRootsMapName"
  )
  soil_map_files <- lapply(soil_map_names, touch)

  ## FireReductionParameters from the upstream reference (3 severity classes).
  fire_params <- tibble::tribble(
    ~FireSeverity , ~CoarseDebrisReduction , ~FineLitterReduction , ~CohortWoodReduction , ~CohortLeafReduction , ~SOMReduction ,
    1L            , 0.0                    , 0.5                  , 0.05                 , 0.85                 , 0.1           ,
    2L            , 0.5                    , 0.75                 , 0.15                 , 0.95                 , 0.5           ,
    3L            , 1.0                    , 1.0                  , 0.35                 , 1.0                  , 0.75
  )

  ## HarvestReductionParameters from the upstream reference. The upstream test
  ## file has a 5-numeric-column variant (Name, WoodLitterReduct, LitterReduct,
  ## SOMReduction, CohortWoodRemoval, CohortLeafRemoval); the user guide
  ## (section 2.35) documents only 4 numeric columns (DeadWoodReduction,
  ## DeadLitterReduction, CohortWoodRemoval, CohortLeafRemoval), so the
  ## upstream `SOMReduction` value (0.2 / 1.0) is dropped here.
  harvest_params <- tibble::tribble(
    ~PrescriptionName , ~DeadWoodReduction , ~DeadLitterReduction , ~CohortWoodRemoval , ~CohortLeafRemoval ,
    "MaxAgeClearcut"  , 0.5                , 0.15                 , 0.8                , 0.15               ,
    "PatchCutting"    , 1.0                , 1.0                  , 1.0                , 1.0
  )

  ext_necn <- NECNSuccession$new(
    path = tmp_pth,
    Timestep = 5L,
    SeedingAlgorithm = "WardSeedDispersal",
    InitialCommunitiesCSV = ic_csv,
    InitialCommunitiesMap = ic_map,
    ClimateConfigFile = climate_cfg,
    SoilMaps = soil_map_files,
    CalibrateMode = FALSE,
    SmokeModelOutputs = FALSE,
    WaterDecayFunction = "Ratio",
    ProbabilityEstablishAdjust = 1.0,
    InitialMineralN = 2.0,
    InitialFineFuels = 0.99,
    AtmosphericNSlope = 0.05,
    AtmosphericNIntercept = 0.05,
    Latitude = 44.0,
    DenitrificationRate = 0.001,
    DecayRateSurf = 0.01,
    DecayRateSOM1 = 0.01,
    DecayRateSOM2 = 0.2,
    DecayRateSOM3 = 0.001,
    SpeciesParameters = species_csv,
    FireReductionParameters = fire_params,
    HarvestReductionParameters = harvest_params
  )

  ext_necn$write()

  testthat::expect_true(all(file.exists(file.path(tmp_pth, ext_necn$files))))

  contents <- readLines(file.path(tmp_pth, ext_necn$files[1]))
  testthat::expect_true(any(grepl("^LandisData\\s+\"NECN Succession\"", contents)))
  testthat::expect_true(any(grepl("^Timestep\\s+5", contents)))
  testthat::expect_true(any(grepl("^SeedingAlgorithm\\s+\"?WardSeedDispersal", contents)))
  testthat::expect_true(any(grepl("^InitialCommunitiesCSV", contents)))
  testthat::expect_true(any(grepl("^InitialCommunitiesMap", contents)))
  testthat::expect_true(any(grepl("^ClimateConfigFile", contents)))
  testthat::expect_true(any(grepl("^SoilDepthMapName", contents)))
  testthat::expect_true(any(grepl("^InitialSOM1CsurfMapName", contents)))
  testthat::expect_true(any(grepl("^InitialDeadWoodRootsMapName", contents)))
  testthat::expect_true(any(grepl("^WaterDecayFunction\\s+\"?Ratio", contents)))
  testthat::expect_true(any(grepl("^InitialMineralN\\s+2", contents)))
  testthat::expect_true(any(grepl("^AtmosphericNSlope\\s+0\\.05", contents)))
  testthat::expect_true(any(grepl("^Latitude\\s+44", contents)))
  testthat::expect_true(any(grepl("^DenitrificationRate\\s+0\\.001", contents)))
  testthat::expect_true(any(grepl("^DecayRateSurf\\s+0\\.01", contents)))
  testthat::expect_true(any(grepl("^DecayRateSOM3\\s+0\\.001", contents)))
  testthat::expect_true(any(grepl("^SpeciesParameters", contents)))
  testthat::expect_true(any(grepl("^FireReductionParameters", contents)))
  testthat::expect_true(any(grepl("^HarvestReductionParameters", contents)))

  withr::deferred_run()
})

testthat::test_that("NECN Succession OutputMaps require `{timestep}` placeholder", {
  ## Per user guide section 2.29, optional output map filenames must contain
  ## the literal `{timestep}` placeholder so LANDIS-II writes one map per
  ## simulation year.
  tmp_pth <- withr::local_tempdir("test_NECNSuccession_OutputMaps_")

  ext <- NECNSuccession$new(path = tmp_pth)

  ## Missing placeholder -> should error.
  testthat::expect_error(
    ext$OutputMaps <- list(ANPPMapName = "NECN/ANPP.tif"),
    "`\\{timestep\\}` placeholder"
  )

  ## With placeholder -> should succeed and round-trip.
  valid_maps <- list(
    ANPPMapName = "NECN/ANPP-{timestep}.tif",
    ANPPMapFrequency = 10L,
    SoilCarbonMapName = "NECN/SoilC-{timestep}.tif"
  )
  ext$OutputMaps <- valid_maps
  testthat::expect_identical(ext$OutputMaps, valid_maps)

  withr::deferred_run()
})
