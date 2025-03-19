testthat::test_that("Biomass Succession inputs are properly created", {
  testthat::skip_if_not_installed("SpaDES.core")
  testthat::skip_if_not_installed("withr")

  fireModel <- "scfm" # "landis"
  frpType <- "FRT" # "ECODISTRICT"
  d <- file.path(
    "~/GitHub/BC_HRV/outputs",
    glue::glue("NRD_Quesnel_{fireModel}_LH_hrv_NDTBEC_{frpType}_res125")
  )

  f1 <- file.path(d, "simOutPreamble_NRD_Quesnel_.rds")
  f2 <- file.path(d, "simOutDataPrep_NRD_Quesnel.rds")

  testthat::skip_if_not(all(file.exists(f1, f2)))

  sim1 <- SpaDES.core::loadSimList(f1)
  sim2 <- SpaDES.core::loadSimList(f2)

  ## initial communities
  cohortData <- sim2[["cohortData"]]
  pixelGroupMap <- sim2[["pixelGroupMap"]]

  ## ecoregion
  ecoregion <- sim2[["ecoregion"]]
  ecoregionMap <- sim2[["ecoregionMap"]]

  ## fireRegimePolys
  fireRegimePolys <- sim1[["fireRegimePolys"]]

  ## other
  minRelativeB <- sim2[["minRelativeB"]]
  species <- sim2[["species"]]
  speciesEcoregion <- sim2[["speciesEcoregion"]]
  sufficientLight <- sim2[["sufficientLight"]]

  ## TODO: unused below:
  speciesLayers <- sim2[["speciesLayers"]]
  standAgeMap <- sim2[["standAgeMap"]] |> terra::crop(speciesLayers)
  studyArea <- sim1[["studyArea"]]
  sppEquiv <- sim2[["sppEquiv"]]

  rm(sim1)
  rm(sim2)

  ## prepare landis input files ----------------------------------------------------------------

  tmp_pth <- withr::local_tempdir("test_Biomass_Succession_")

  ## climate data
  clim_file <- file.path(tmp_pth, "climate-data-daily.csv")
  clim_vars <- c("prcp", "tmax", "tmin")

  daily_weather <- purrr::map(
    .x = clim_vars,
    .f = prep_daily_weather,
    studyArea = fireRegimePolys,
    id = "FRT",
    start = "2011-01-01",
    end = "2015-12-31"
  ) |>
    purrr::list_rbind()

  write.csv(daily_weather, clim_file)

  cc_file <- prepClimateConfigFile(
    path = tmp_pth,
    ClimateTimeSeries = "Daily_RandomYears",
    ClimateFile = basename(clim_file),
    SpinUpClimateTimeSeries = "Daily_RandomYears",
    SpinUpClimateFile = basename(clim_file),
    GenerateClimateOutputFiles = "yes",
    UsingFireClimate = "",
    FineFuelMoistureCode = "",
    DuffMoistureCode = "",
    DroughtCode = "",
    FirstDayFire = "",
    LastDayFire = ""
  ) ## TODO

  testthat::expect_true(all(file.exists(cc_file)))

  erp_df <- prepEcoregionParameters() ## TODO

  frp_df <- prepFireReductionParameters() ## TODO

  hrp_df <- prepHarvestReductionParameters() ## TODO

  ic_objs <- simplifyCohorts(cohortData, pixelGroupMap, ageBin = 20) ## TODO: fix!

  ic_files <- prepInitialCommunities(
    cohortData = ic_objs[[1]],
    pixelGroupMap = ic_objs[[2]],
    path = tmp_pth
  )

  testthat::expect_true(all(file.exists(ic_files)))

  mrb_df <- prepMinRelativeBiomass(minRelativeB) ## TODO

  spp_file <- prepSpeciesData(species, tmp_pth) ## TODO

  testthat::expect_true(file.exists(spp_file))

  spperd_file <- prepSpeciesEcoregionDataFile(speciesEcoregion, tmp_pth)

  testthat::expect_true(all(file.exists(spperd_file)))

  sfl_df <- sufficientLight ## TODO

  bse_file <- BiomassSuccessionInput(
    path = tmp_path,

    ## TODO: these should be the prepped objects/filenames
    CalibrateMode = FALSE,
    ClimateConfigFile = cc_file,
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

  scenario_file <- scenario(
    name = "test_biomass_succession",
    extensions = list(succession = "Biomass Succession"),
    path = tmp_pth,

    ## additional arguments
    CellLength = terra::res(ecoregionMap)[1],
    DisturbancesRandomOrder = FALSE,
    Duration = 20,
    EcoregionsFiles = er_files,
    RandomNumberSeed = NULL, ## optional
    SpeciesDataFile = spp_file
  )

  testthat::expect_true(file.exists(scenario_file))

  ## run the landis scenario -------------------------------------------------------------------

  ## TODO
  landis(scenario_file)

  withr::deferred_run()
})
