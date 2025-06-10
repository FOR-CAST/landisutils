testthat::test_that("Climate inputs are properly created", {
  testthat::skip_if_not_installed("SpaDES.tools")
  testthat::skip_if_not_installed("scfmutils")
  testthat::skip_if_not_installed("withr")

  studyAreaBC <- terra::vect(cbind(-122.14, 52.14), crs = "epsg:4326") |>
    terra::project(paste("+proj=lcc +lat_1=49 +lat_2=77 +lat_0=0 +lon_0=-95",
                         "+x_0=0 +y_0=0 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0")) |>
    SpaDES.tools::randomStudyArea(seed = 60, size = 1e10) |>
    scfmutils::prepInputsFireRegimePolys(studyArea = _, type = "FRT")

  tmp_pth <- withr::local_tempdir("test_Biomass_Succession_")

  ## climate data
  clim_file <- file.path(tmp_pth, "climate-data-daily.csv")
  clim_vars <- c("prcp", "tmax", "tmin")
  clim_years <- 2011:2012 ## availability is 1980 to last-year

  daily_weather <- purrr::map(
    .x = clim_vars,
    .f = prep_daily_weather,
    studyArea = ecoregionPolys,
    id = "ecoregion",
    start = glue::glue("{head(clim_years, 1)}-01-01"),
    end = glue::glue("{tail(clim_years, 1)}-12-31")
  ) |>
    purrr::list_rbind()

  writeClimateData(daily_weather, clim_file)

  testthat::expect_true(file.exists(clim_file))

  ## Daymet
  climvars <- c("prcp", "tmax", "tmin")

  daily_weather <- purrr::map(
    .x = climvars,
    .f = prep_daily_weather,
    studyArea = studyAreaBC,
    id = "FRT", ## TODO: do this for ecoregions too
    start = "2018-01-01",
    end = "2019-12-31"
  ) |>
    purrr::list_rbind()

  writeCl

  testthat::expect_true()

  ## Terra Climate
  climvars <- c(
    "aet",
    # "def", "PDSI", "pet",
    "ppt",
    # "q", "soil",
    "srad",
    # "swe",
    "tmax", "tmin",
    # "vap", "vpd",
    "ws"
  )

  monthly_weather <- purrr::map(
    .x = climvars,
    .f = prep_monthly_weather,
    studyArea = studyAreaBC,
    id = "FRT",
    start = "2018-01-01",
    end = "2019-12-31"
  ) |>
    purrr::list_rbind()

  testthat::expect_true()
})
