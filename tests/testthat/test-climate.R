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
  clim_vars <- c("prcp", "tmax", "tmin")

  daily_weather <- purrr::map(
    .x = clim_vars,
    .f = prep_daily_weather,
    studyArea = ecoregionPolys,
    id = "PolyID",
    start = glue::glue("{head(clim_years, 1)}-01-01"),
    end = glue::glue("{tail(clim_years, 1)}-12-31")
  ) |>
    purrr::list_rbind()

  clim_file <- file.path(tmp_pth, "climate-data-daily.csv")
  writeClimateData(daily_weather, clim_file)

  testthat::expect_true(file.exists(clim_file))

  ## Terra Climate
  aet_df <- purrr::map(
    .x = "aet",
    .f = prep_monthly_weather,
    studyArea = ecoregionPolys,
    id = "PolyID",
    start = glue::glue("{head(clim_years, 1)}-01-01"),
    end = glue::glue("{tail(clim_years, 1)}-12-31")
  ) |>
    purrr::list_rbind() |>
    dplyr::filter(Year <= tail(clim_years, 1)) ## match end year

  clim_file <- file.path(tmp_pth, "climate-data-monthly.csv")
  climvars <- c("ppt", "tmax", "tmin")

  monthly_weather <- purrr::map(
    .x = climvars,
    .f = prep_monthly_weather,
    studyArea = ecoregionPolys,
    id = "PolyID",
    start = glue::glue("{head(clim_years, 1)}-01-01"),
    end = glue::glue("{tail(clim_years, 1)}-12-31")
  ) |>
    purrr::list_rbind() |>
    dplyr::filter(Year <= tail(clim_years, 1)) ## match end year

  writeClimateData(monthly_weather, clim_file)

  testthat::expect_true(file.exists(clim_file))
})
