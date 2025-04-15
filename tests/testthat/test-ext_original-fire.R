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

  ## TODO: use fireRegimePolys to create Fire Region Parameters Table:
  ##       --> FireRegionName is e.g., glue("FRT{polyID}")
  ##       --> MapCode [0, 65535] is polyID
  ##       --> Mean Size [ha] from the attributes table
  ##       --> Min Size  [ha] from the attributes table
  ##       --> Max Size  [ha] from the attributes table
  ##       --> IgnitionProb [0, 1] from the attributes table?
  ##       --> k  [int >= 0] from the attributes table?? 'fire spread age'

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

  tmp_pth <- withr::local_tempdir("test_Base_Fire_")

  ## TODO

  ifrm_file <- file.path(tmp_pth, "FireRegions.tif")
  InitialFireRegionsMap <- terra::rasterize(
    fireRegimePolys,
    standAgeMap,
    field = "FRT",
    filename = ifrm_file
  )

  bf_file <- OriginalFireInput(
    path = tmp_path,

    ## TODO: these should be the prepped objects/filenames
    DynamicFireRegionsTable = NULL,
    FireRegionParametersTable = TODO,
    FireDamageTable = TODO,
    FuelCurveTable = TODO,
    InitialFireRegionsMap = ifrm_file,
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
