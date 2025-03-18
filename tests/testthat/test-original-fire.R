testthat::test_that("Biomass Succession inputs are properly created", {
  testthat::skip_if_not_installed("map")
  testthat::skip_if_not_installed("SpaDES.core")
  testthat::skip_if_not_installed("withr")

  fireModel <- "scfm" # "landis"
  frpType <- "FRT" # "ECODISTRICT"
  d <- file.path(
    "~/GitHub/BC_HRV/outputs",
    glue::glue("NRD_Quesnel_{fireModel}_LH_hrv_NDTBEC_{frpType}_res125")
  )
  f1 <- file.path(d, "simOutPreamble_NRD_Quesnel_.rds")

  testthat::skip_if_not(file.exists(f1))

  ## fire objects -----------------------------------------

  sim1 <- SpaDES.core::loadSimList(f1)

  frp_erni <- sim[["fireRegimePolys"]]

  ## TODO: use frp_erni to create Fire Region Parameters Table:
  ##       --> FireRegionName is e.g., glue("FRT{polyID}")
  ##       --> MapCode [0, 65535] is polyID
  ##       --> Mean Size [ha] from the attributes table
  ##       --> Min Size  [ha] from the attributes table
  ##       --> Max Size  [ha] from the attributes table
  ##       --> IgnitionProb [0, 1] from the attributes table?
  ##       --> k  [int >= 0] from the attributes table?? 'fire spread age'

  ## TODO: use frp_erni to create InitialFireRegionsMap (terra::rasterize)

  rm(sim1)

  ## prepare landis input files ----------------------------------------------------------------

  tmp_pth <- withr::local_tempdir("test_Base_Fire_")

  ## TODO

  bf_file <- BaseFireInput(
    path = tmp_path,

    ## TODO: these should be the prepped objects/filenames
    DynamicFireRegionsTable = NULL,
    FireRegionParametersTable = TODO,
    FireDamageTable = TODO,
    FuelCurveTable = TODO,
    InitialFireRegionsMap = TODO,
    LogFile = TODO,
    MapNames = TODO,
    SummaryLogFile = TODO,
    Timestep = 1,
    WindCurveTable = TODO
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
    extensions = list(
      succession = "Biomass Succession",
      disturbance = "Base Fire" ## TODO
    ),
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
