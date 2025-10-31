testthat::test_that("Original Fire (and Biomass Succession) inputs are properly created", {
  testthat::skip_if_not_installed("climateR")
  testthat::skip_if_not_installed("future")
  testthat::skip_if_not_installed("furrr")
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

  tmp_pth <- withr::local_tempdir("test_Original_Fire_")

  fire_spp_csv_file <- prepSpeciesData(species, tmp_pth, type = "fire")

  frp_table <- prepFireRegionParametersTable(fireRegimePolys)

  fire_dmg_df <- data.frame(
    CohortAgePercentLongevity = c(20, 50, 85, 100),
    FireSeverityMinusFireTolerance = c(-2, -1, 0, 1)
  )

  ## below is e.g., boreal (stand replacing fires)
  fuel_crv_df <- data.frame(
    FireRegionName = frp_table$FireRegionName,
    S1 = rep(-1, nrow(frp_table)),
    S2 = rep(-1, nrow(frp_table)),
    S3 = rep(-1, nrow(frp_table)),
    S4 = rep(-1, nrow(frp_table)),
    S5 = rep(15, nrow(frp_table))
  )

  ifrm_file <- terra::rasterize(fireRegimePolys, standAgeMap, field = "PolyID", background = 0) |>
    prepInitialFireRegionsMap(file = file.path(tmp_pth, "fire-regions-map.tif"))

  log_file <- file.path(tmp_pth, "fire/log.csv")
  sum_log_file <- file.path(tmp_pth, "fire/summary-log.csv")

  ext_of <- OriginalFire$new(
    path = tmp_pth,

    DynamicFireRegionsTable = NULL,
    FireRegionParametersTable = frp_table,
    FireDamageTable = fire_dmg_df,
    FuelCurveTable = fuel_crv_df,
    InitialFireRegionsMap = ifrm_file,
    LogFile = log_file,
    Species_CSV_File = fire_spp_csv_file,
    SummaryLogFile = sum_log_file,
    Timestep = 1,
    WindCurveTable = NULL
  )

  ext_of$write()

  testthat::expect_true(all(file.exists(file.path(tmp_pth, ext_of$files))))

  withr::deferred_run()
})
