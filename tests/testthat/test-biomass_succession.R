testthat::test_that("Biomass Succession inputs are properly created", {
  testthat::skip_if_not_installed("SpaDES.core")
  testthat::skip_if_not_installed("withr")

  fireModel <- "scfm" # "landis"
  frpType <- "FRT" # "ECODISTRICT"
  d <- file.path("~/GitHub/BC_HRV/outputs",
                 glue::glue("NRD_Quesnel_{fireModel}_LH_hrv_NDTBEC_{frpType}_res125"))
  f <- file.path(d, "simOutDataPrep_NRD_Quesnel.rds")

  testthat::skip_if_not(file.exists(f))

  sim <- SpaDES.core::loadSimList(f)

  ## initial communities
  cohortData <- sim$cohortData
  pixelGroupMap <- sim$pixelGroupMap

  ## ecoregion
  ecoregion <- sim$ecoregion
  ecoregionMap <- sim$ecoregionMap

  speciesLayers <- sim$speciesLayers

  standAgeMap <- sim$standAgeMap |> terra::crop(speciesLayers)

  minRelativeB <- sim$minRelativeB
  species <- sim$species ## csv
  speciesEcoregion <- sim$speciesEcoregion ## csv
  sufficientLight <- sim$sufficientLight

  sppEquiv <- sim$sppEquiv

  rm(sim)

  ## prepare landis input files ----------------------------------------------------------------

  tmp_pth <- withr::local_tempdir("test_Biomass_Succession")

  erp_df <- prepEcoregionParameters(
    speciesEcoregion = speciesEcoregion
  )

  frp_df <- prepFireReductionParameters() ## TODO

  hrp_df <- prepHarvestReductionParameters() ## TODO

  ic_files <- prepInitialCommunities(
    cohortData = cohortData,
    pixelGroupMap = pixelGroupMap,
    path = tmp_pth
  )

  testthat::expect_true(all(file.exists(ic_files)))

  spp_file <- prepSpeciesData( ## TODO
    species = species,
    path = tmp_pth
  )

  testthat::expect_true(file.exists(spp_file))

  bse_file <- BiomassSuccessionInput(
    path = tmp_path,

    ## TODO: these should be the prepped objects/filenames
    CalibrateMode = FALSE,
    ClimateConfigFile = NULL, ## can be NULL; optional for v7
    EcoregionParametersFiles = ,
    FireReductionParameters = frp_df,
    HarvestReductionParameters = hrp_df,
    InitialCommunitiesFiles = icc_files,
    MinRelativeBiomass = minRelativeB,
    SeedingAlgorithm = "WardSeedDispersal",
    SpeciesDataFile = ,
    SpeciesEcoregionDataFile = ,
    SufficientLight = ,
    Timestep
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
