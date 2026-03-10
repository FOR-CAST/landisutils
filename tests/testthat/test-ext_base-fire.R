testthat::test_that("Base Fire (and Biomass Succession) inputs are properly created", {
  skip("incomplete")

  ## prepare landis input files ----------------------------------------------------------------

  tmp_pth <- withr::local_tempdir("test_Original_Fire_")

  fire_spp_csv_file <- prepSpeciesData(species, tmp_pth, type = "fire")

  frp_table <- prepFireRegionParametersTable(fireRegimePolys)

  fire_dmg_df <- defaultFireDamageTable()

  ## below is e.g., boreal (stand replacing fires)
  fuel_crv_df <- defaultFuelCurveTable(frp_table)

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
