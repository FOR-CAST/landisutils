testthat::test_that("Biomass Succession inputs are properly created", {
  testthat::skip_if_not_installed("SpaDES.core")
  testthat::skip_if_not_installed("withr")

  fireModel <- "scfm" # "landis"
  frpType <- "FRT" # "ECODISTRICT"
  d <- file.path(
    "~/GitHub/BC_HRV/outputs",
    glue::glue("NRD_Quesnel_{fireModel}_LH_hrv_NDTBEC_{frpType}_res125")
  )
  f1 <- file.path(d, "simOutPreamble_NRD_Quesnel.rds")
  f2 <- file.path(d, "simOutDataPrep_NRD_Quesnel.rds")

  testthat::skip_if_not(file.exists(f1))
  testthat::skip_if_not(file.exists(f1))

  sim1 <- SpaDES.core::loadSimList(f1)
  sim2 <- SpaDES.core::loadSimList(f2)

  ## fires
  frp_erni <- sim1[["fireRegimePolys"]] ## TODO: move to separate test

  ## TODO: use frp_erni to create Fire Region Parameters Table:
  ##       --> FireRegionName is e.g., glue("FRT{polyID}")
  ##       --> MapCode [0, 65535] is polyID
  ##       --> Mean Size [ha] from the attributes table
  ##       --> Min Size  [ha] from the attributes table
  ##       --> Max Size  [ha] from the attributes table
  ##       --> IgnitionProb [0, 1] from the attributes table?
  ##       --> k  [int >= 0] from the attributes table?? 'fire spread age'

  ## TODO: use frp_erni to create InitialFireRegionsMap (terra::rasterize)

  ## initial communities
  cohortData <- sim2[["cohortData"]]
  pixelGroupMap <- sim2[["pixelGroupMap"]]

  ## ecoregion
  ecoregion <- sim2[["ecoregion"]]
  ecoregionMap <- sim2[["ecoregionMap"]]

  ## other
  minRelativeB <- sim2[["minRelativeB"]]
  species <- sim2[["species"]]
  speciesEcoregion <- sim2[["speciesEcoregion"]]
  sufficientLight <- sim2[["sufficientLight"]]

  ## TODO: unused below:
  speciesLayers <- sim2[["speciesLayers"]]
  standAgeMap <- sim2[["standAgeMap"]] |> terra::crop(speciesLayers)
  sppEquiv <- sim2[["sppEquiv"]]

  rm(sim1, sim2)

  ## prepare landis input files ----------------------------------------------------------------

  tmp_pth <- withr::local_tempdir("test_Biomass_Succession_")

  cc_file <- prepClimateConfigFile() ## TODO

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
