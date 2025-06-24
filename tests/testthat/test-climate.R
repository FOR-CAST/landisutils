testthat::test_that("Climate inputs are properly created", {
  testthat::skip_if_not_installed("climateR")
  testthat::skip_if_not_installed("withr")
  testthat::skip_if_not_installed("zonal")

  tmp_pth <- withr::local_tempdir("test_climate_")

  ## ecoregion polygons
  ecoregionPolys <- landisutils::test_ecoregionPolys

  ## climate data -------------------------------------------------------
  clim_years <- 2011:2012 ## availability is 1980 to last year

  ## Daymet
  clim_vars_daily <- c("prcp", "tmax", "tmin")

  daily_weather <- prep_daily_weather(
    vars = clim_vars_daily,
    years = clim_years,
    studyArea = ecoregionPolys,
    id = "PolyID"
  )

  clim_file <- file.path(tmp_pth, "climate-data-daily.csv")
  writeClimateData(daily_weather, clim_file)

  testthat::expect_true(file.exists(clim_file))

  ## Terra Climate
  aet_df <- prep_monthly_weather(
    vars = "aet",
    years = clim_years,
    studyArea = ecoregionPolys,
    id = "PolyID"
  )

  clim_file <- file.path(tmp_pth, "climate-data-monthly.csv")
  clim_vars_monthly <- c("ppt", "tmax", "tmin")

  monthly_weather <- prep_monthly_weather(
    vars = clim_vars_monthly,
    years = clim_years,
    studyArea = ecoregionPolys,
    id = "PolyID"
  )

  writeClimateData(monthly_weather, clim_file)

  testthat::expect_true(file.exists(clim_file))

  withr::deferred_run()
})
