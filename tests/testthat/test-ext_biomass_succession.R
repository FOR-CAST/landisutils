testthat::test_that("Biomass Succession inputs are properly created", {
  testthat::skip_if_not_installed("climateR")
  testthat::skip_if_not_installed("future")
  testthat::skip_if_not_installed("furrr")
  testthat::skip_if_not_installed("purrr")
  testthat::skip_if_not_installed("withr")
  testthat::skip_if_not_installed("zonal")

  studyAreaName <- "Chine"

  ## initial communities
  cohortData <- landisutils::Chine_cohortData
  pixelGroupMap <- terra::unwrap(landisutils::Chine_pixelGroupMap)

  ## ecoregion
  ecoregion <- landisutils::Chine_ecoregion
  ecoregionMap <- terra::unwrap(landisutils::Chine_ecoregionMap)
  ecoregionPolys <- landisutils::Chine_ecoregionPolys

  ## fireRegimePolys
  fireRegimePolys <- landisutils::Chine_fireRegimePolys

  ## other
  minRelativeB <- landisutils::Chine_minRelativeB
  species <- landisutils::Chine_species
  speciesEcoregion <- landisutils::Chine_speciesEcoregion
  speciesLayers <- terra::unwrap(landisutils::Chine_speciesLayers)
  standAgeMap <- terra::unwrap(landisutils::Chine_standAgeMap)
  sufficientLight <- landisutils::Chine_sufficientLight

  ## prepare landis input files ----------------------------------------------------------------
  tmp_pth <- withr::local_tempdir("test_Biomass_Succession_")

  ## climate data
  clim_file <- file.path(tmp_pth, "climate-data-daily.csv")
  clim_vars <- c("prcp", "tmax", "tmin")
  clim_years <- 2011:2012 ## availability is 1980 to 2023

  ## TODO: set future plan for test
  daily_weather <- prep_daily_weather(
    vars = clim_vars,
    years = clim_years,
    studyArea = ecoregionPolys,
    id = "ecoregion"
  )

  writeClimateData(daily_weather, clim_file)

  cc <- prepClimateConfig(
    path = tmp_pth,
    ClimateTimeSeries = "Daily_RandomYears",
    ClimateFile = clim_file,
    SpinUpClimateTimeSeries = "Daily_RandomYears",
    SpinUpClimateFile = clim_file,
    GenerateClimateOutputFiles = "yes",
    UsingFireClimate = "no" ## TODO: allow 'yes' (need springstart/winterstart)
  )

  testthat::expect_true(all(file.exists(file.path(cc$path, cc$files))))

  aet_df <- prep_monthly_weather(
    vars = "aet",
    years = clim_years,
    studyArea = ecoregionPolys,
    id = "ecoregion"
  )

  erp_df <- prepEcoregionParameters(aet_df)

  frp_df <- prepFireReductionParameters(NULL) ## NULL uses dummy defaults

  hrp_df <- prepHarvestReductionParameters(NULL) ## ## NULL uses dummy defaults

  ic_objs <- simplifyCohorts(cohortData, pixelGroupMap, ageBin = 20)

  ic_files <- prepInitialCommunities(
    cohortData = ic_objs[[1]],
    pixelGroupMap = ic_objs[[2]],
    path = tmp_pth
  )

  testthat::expect_true(all(file.exists(ic_files)))

  mrb_df <- prepMinRelativeBiomass(minRelativeB)

  spp_file <- prepSpeciesData(species, tmp_pth, type = "succession")

  testthat::expect_true(file.exists(spp_file))

  spperd_file <- prepSpeciesEcoregionDataFile(speciesEcoregion, tmp_pth)

  testthat::expect_true(all(file.exists(spperd_file)))

  sfl_df <- sufficientLight

  ext_bs <- BiomassSuccession$new(
    path = tmp_pth,
    CalibrateMode = FALSE,
    ClimateConfigFile = file.path(cc$path, cc$files[1]),
    EcoregionParameters = erp_df,
    FireReductionParameters = frp_df,
    HarvestReductionParameters = hrp_df,
    InitialCommunitiesFiles = ic_files,
    MinRelativeBiomass = mrb_df,
    SeedingAlgorithm = "WardSeedDispersal",
    SpeciesDataFile = spp_file,
    SpeciesEcoregionDataFile = spperd_file,
    SufficientLight = sfl_df,
    Timestep = 10
  )

  ## prepare landis scenario file --------------------------------------------------------------

  er_files <- prepEcoregionsFiles(
    ecoregion = ecoregion,
    ecoregionMap = ecoregionMap,
    path = tmp_pth
  )

  testthat::expect_true(all(file.exists(er_files)))

  core_spp_file <- prepSpeciesData(species, tmp_pth, type = "core")

  testthat::expect_true(file.exists(core_spp_file))

  scenario_name <- glue::glue("scenario_{studyAreaName}")
  scenario <- scenario(
    name = scenario_name,
    extensions = list(ext_bs),
    path = tmp_pth,
    climate_config = cc,

    ## additional arguments
    CellLength = terra::res(ecoregionMap)[1],
    DisturbancesRandomOrder = FALSE,
    Duration = 20,
    EcoregionsFiles = er_files,
    RandomNumberSeed = NULL, ## optional
    SpeciesInputFile = core_spp_file
  )

  testthat::expect_true(all(file.exists(file.path(scenario$path, scenario$files))))

  scenario$replicate(n = 3)

  testthat::expect_true(dir.exists(scenario$path))
  testthat::expect_true(dir.exists(paste0(scenario$path, "_rep01")))
  testthat::expect_true(dir.exists(paste0(scenario$path, "_rep02")))
  testthat::expect_true(dir.exists(paste0(scenario$path, "_rep03")))

  scenario$replicate(n = 1)

  testthat::expect_true(dir.exists(paste0(scenario$path, "_rep04")))
  testthat::expect_true(all(file.exists(file.path(
    paste0(scenario$path, "_rep04"),
    scenario$files
  ))))

  ## run the landis scenario -------------------------------------------------------------------
  testthat::skip_if_not(nzchar(landis_find()))
  ## TODO
  landis_run(scenario_file)

  withr::deferred_run()
})
