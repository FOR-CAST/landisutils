testthat::test_that("DGS Succession inputs are properly created", {
  ## NOTE: reference values from the LANDIS-II Core v8 / DGS Succession v2.0 test input:
  ## testing/Core8-DGS_version2.0/DGS_Succession_AKinput_020725.txt
  ## <https://github.com/LANDIS-II-Foundation/Extension-DGS-Succession/tree/master/testing/Core8-DGS_version2.0>

  tmp_pth <- withr::local_tempdir("test_DGSSuccession_")

  ## stub input files -- the scaffolding only checks that referenced paths exist,
  ## not that their contents are valid LANDIS-II files.
  touch <- function(name) {
    f <- file.path(tmp_pth, name)
    writeLines("", f)
    f
  }

  ic_txt <- touch("initial-communities.txt")
  ic_map <- touch("initial-communities.tif")
  climate_cfg <- touch("climate-generator-baseline.txt")
  shaw_gipl_cfg <- touch("ShawGiplConfig.txt")
  species_csv <- touch("DGS_Spp_Table.csv")

  shaw_gipl_files <- vapply(
    c(
      "ListThus.csv",
      "ShawGeneralInputs.csv",
      "ShawPlantTypes.csv",
      "ShawSoilTypes.csv",
      "GiplProperties.csv",
      "Unfrozen.txt"
    ),
    touch,
    character(1),
    USE.NAMES = FALSE
  )

  soil_map_names <- list(
    SoilDepthMapName = "soil_depth.tif",
    SoilDrainMapName = "soil_drain.tif",
    SoilBaseFlowMapName = "soil_baseflow.tif",
    SoilStormFlowMapName = "soil_stormflow.tif",
    SoilFieldCapacityMapName = "soil_fieldcap.tif",
    SoilWiltingPointMapName = "soil_wiltpt.tif",
    SoilPercentSandMapName = "soil_sand.tif",
    SoilPercentClayMapName = "soil_clay.tif",
    SoilBulkDensityMapName = "soil_bulkdens.tif",
    SoilParticleDensityMapName = "soil_partdens.tif",
    InitialSOC_PrimaryMapName = "initial_soc.tif",
    InitialSON_PrimaryMapName = "initial_son.tif",
    InitialDeadWoodSurfaceMapName = "initial_deadwood_surf.tif",
    InitialDeadCoarseRootsMapName = "initial_deadwood_roots.tif"
  )
  soil_map_files <- lapply(soil_map_names, touch)

  fire_params <- prepDGSFireReductionParameters()

  ## HarvestReductionParameters from the upstream Core8 single-cell test input
  ## (Clearcut prescription); the parser requires 6 columns including SOMReduction.
  harvest_params <- tibble::tribble(
    ~PrescriptionName , ~DeadWoodReduction , ~DeadLitterReduction , ~SOMReduction , ~CohortWoodRemoval , ~CohortLeafRemoval ,
    "Clearcut"        , 0.0                , 0.01                 , 0.02          , 0.85               , 0.15
  )

  ext_dgs <- DGSSuccession$new(
    path = tmp_pth,
    Timestep = 1L,
    CalibrateMode = FALSE,
    ClimateConfigFile = climate_cfg,
    AtmosphericNSlope = 0.0008,
    AtmosphericNIntercept = 0.003,
    InitialCommunities = ic_txt,
    InitialCommunitiesMap = ic_map,
    Latitude = 65.6,
    ShawGiplConfigFile = shaw_gipl_cfg,
    ShawGiplFiles = shaw_gipl_files,
    SoilMaps = soil_map_files,
    InitialFineFuels = 0.1,
    InitialMineralN = 1.697,
    DenitrificationRate = 0.001,
    WaterDecayFunction = "Linear",
    DammMcNIPParameters = defaultDGSDammMcNIPParameters(),
    SeedingAlgorithm = "WardSeedDispersal",
    ProbabilityEstablishAdjust = 0.4,
    SpeciesParameters = species_csv,
    FireReductionParameters = fire_params,
    HarvestReductionParameters = harvest_params
  )

  ext_dgs$write()

  testthat::expect_true(all(file.exists(file.path(tmp_pth, ext_dgs$files))))

  contents <- readLines(file.path(tmp_pth, ext_dgs$files[1]))
  testthat::expect_true(any(grepl("^LandisData\\s+\"DGS Succession\"", contents)))
  testthat::expect_true(any(grepl("^Timestep\\s+1", contents)))
  testthat::expect_true(any(grepl("^CalibrateMode\\s+\"?no", contents)))
  testthat::expect_true(any(grepl("^ClimateConfigFile", contents)))
  testthat::expect_true(any(grepl("^AtmosphericNSlope\\s+0\\.0008", contents)))
  testthat::expect_true(any(grepl("^AtmosphericNIntercept\\s+0\\.003", contents)))
  testthat::expect_true(any(grepl("^InitialCommunities\\s", contents)))
  testthat::expect_true(any(grepl("^InitialCommunitiesMap", contents)))
  testthat::expect_true(any(grepl("^Latitude\\s+65", contents)))
  testthat::expect_true(any(grepl("^ShawGiplConfigFile", contents)))
  testthat::expect_true(any(grepl("^SoilDepthMapName", contents)))
  testthat::expect_true(any(grepl("^SoilBulkDensityMapName", contents)))
  testthat::expect_true(any(grepl("^SoilParticleDensityMapName", contents)))
  testthat::expect_true(any(grepl("^InitialSOC_PrimaryMapName", contents)))
  testthat::expect_true(any(grepl("^InitialSON_PrimaryMapName", contents)))
  testthat::expect_true(any(grepl("^InitialDeadWoodSurfaceMapName", contents)))
  testthat::expect_true(any(grepl("^InitialDeadCoarseRootsMapName", contents)))
  testthat::expect_true(any(grepl("^InitialFineFuels\\s+0\\.1", contents)))
  testthat::expect_true(any(grepl("^InitialMineralN\\s+1\\.697", contents)))
  testthat::expect_true(any(grepl("^DenitrificationRate\\s+0\\.001", contents)))
  testthat::expect_true(any(grepl("^WaterDecayFunction\\s+\"?Linear", contents)))
  testthat::expect_true(any(grepl("^InitialMicrobialC", contents)))
  testthat::expect_true(any(grepl("^ActEnergyDOCUptake", contents)))
  testthat::expect_true(any(grepl("^CarbonUseEfficiency", contents)))
  testthat::expect_true(any(grepl("^FractionLitterToDOC", contents)))
  ## SoilMoistureA/B intentionally not written (commented out in DGS parser)
  testthat::expect_false(any(grepl("^SoilMoistureA", contents)))
  testthat::expect_false(any(grepl("^SoilMoistureB", contents)))
  testthat::expect_true(any(grepl("^SeedingAlgorithm\\s+\"?WardSeedDispersal", contents)))
  testthat::expect_true(any(grepl("^ProbabilityEstablishAdjust\\s+0\\.4", contents)))
  testthat::expect_true(any(grepl("^SpeciesParameters", contents)))
  testthat::expect_true(any(grepl("^FireReductionParameters", contents)))
  testthat::expect_true(any(grepl("^HarvestReductionParameters", contents)))

  ## SHAW/GIPL companion files were registered with the extension
  testthat::expect_true(all(basename(shaw_gipl_files) %in% basename(ext_dgs$files)))

  withr::deferred_run()
})

testthat::test_that("DGSSuccession$new validates inputs", {
  tmp_pth <- withr::local_tempdir("test_DGSSuccession_validate_")

  ## Bad seeding algorithm
  testthat::expect_error(
    DGSSuccession$new(path = tmp_pth, SeedingAlgorithm = "BogusDispersal"),
    regexp = "WardSeedDispersal"
  )

  ## Bad WaterDecayFunction
  testthat::expect_error(DGSSuccession$new(path = tmp_pth, WaterDecayFunction = "Quadratic"))

  ## SoilMaps must use only allowed keys
  testthat::expect_error(DGSSuccession$new(
    path = tmp_pth,
    SoilMaps = list(NotARealMap = "foo.tif")
  ))

  ## DammMcNIPParameters must use only allowed keys
  testthat::expect_error(DGSSuccession$new(
    path = tmp_pth,
    DammMcNIPParameters = list(NotAParam = 1)
  ))

  ## write() requires all DAMM-McNiP keys; missing one should fail
  ext <- DGSSuccession$new(
    path = tmp_pth,
    DammMcNIPParameters = defaultDGSDammMcNIPParameters()[-1]
  )
  testthat::expect_error(ext$write())

  withr::deferred_run()
})

testthat::test_that("prepDGSFireReductionParameters returns valid defaults", {
  df <- prepDGSFireReductionParameters()

  testthat::expect_s3_class(df, "data.frame")
  testthat::expect_named(
    df,
    c(
      "FireSeverity",
      "CoarseDebrisReduction",
      "FineLitterReduction",
      "CohortWoodReduction",
      "CohortLeafReduction",
      "OrganicHorizonReduction"
    )
  )
  testthat::expect_type(df$FireSeverity, "integer")
  testthat::expect_true(all(df$FireSeverity >= 1L & df$FireSeverity <= 10L))
})
