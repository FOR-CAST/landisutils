testthat::test_that("Dynamic Fuel & Fire inputs are properly created", {
  skip("incomplete") ## TODO
  testthat::skip_if_not_installed("elevatr")
  testthat::skip_if_not_installed("purrr")
  testthat::skip_if_not_installed("withr")
  testthat::skip_if_not_installed("zonal")

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

  tmp_pth <- withr::local_tempdir("test_Dynamic_Fire_Fuels_")

  fs_table <- prepFireSizesTable() ## TODO

  ifrm_file <- terra::rasterize(fireRegimePolys, standAgeMap, field = "PolyID", background = 0) |>
    prepInitialFireRegionsMap(file = file.path(tmp_pth, "fire-regions-map.tif"))

  der_table <- prepDynamicEcoregionTable() ## TODO

  gs_file <- prepGroundSlopeFile(ecoregionMap, path = tmp_pth)
  usa_file <- prepUphillAzimuthMap(ecoregionMap, path = tmp_pth)

  szn_table <- TODO ## TODO

  iwdb_file <- prepInitialWeatherDatabase() ## TODO

  log_file <- file.path(tmp_pth, "fire/log.csv")
  sum_log_file <- file.path(tmp_pth, "fire/summary-log.csv")

  ext_dfire <- DynamicFire$new(
    path = tmp_pth,
    Timestep = 10,
    EventSizeType = NULL,
    BuildUpIndex = "yes",
    WeatherRandomizer = 0L,
    FireSizesTable = fs_table,
    InitialFireEcoregionsMap = ifrm_file,
    DynamicEcoregionTable = NULL, # not used
    GroundSlopeFile = gs_file,
    UphillSlopeAzimuthMap = usa_file,
    SeasonTable = szn_table,
    InitialWeatherDatabase = NULL,
    DynamicWeatherTable = NULL, # not used
    FuelTypeTable = defaultFuelTypeTable(),
    SeverityCalibrationFactor = 1.0,
    FireDamageTable = defaultFireDamageTable(),
    MapNames = NULL, # use default
    LogFile = log_file,
    SummaryLogFile = sum_log_file
  )

  ext_dfire$write()

  testthat::expect_true(all(file.exists(file.path(tmp_pth, ext_of$files))))

  spp_fuel_coeffs <- data.frame(Species = c(), FuelCoefficient = c()) ## TODO

  fuel_types <- data.frame() ## TODO

  disturb_conv <- data.frame() ## TODO

  ext_dfuel <- DynamicFuels$new(
    path = tmp_pth,
    Timestep = 10,
    SpeciesFuelCoefficients = spp_fuel_coeffs,
    HardwoodMaximum = 0L,
    DeadFirMaxAge = 15L, ## not needed w/o BDA extension
    FuelTypes = fuel_types,
    EcoregionTable = data.frame(FuelType = integer(0), Ecoregion = character(0)),
    DisturbanceConversionTable = disturb_conv,
    MapFileNames = NULL, ## use default
    PctConiferMapName = NULL, ## use default
    PctDeadFirMapName = NULL ## use default
  )

  withr::deferred_run()
})
