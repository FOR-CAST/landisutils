## Real BioSIM / climr fetches can take many hours against a fine elevation
## raster (BioSIM batches scale with raster cell count; climr also downloads
## multi-GB GCM rasters in projection mode), which causes R CMD check to
## hang. Tests that hit those services skip by default and opt in via
## `LANDISUTILS_RUN_NETWORK_TESTS=true`. To keep opt-in runs reasonable we
## also pass a coarse `z = 6` to the prep_*() functions so the elevation
## raster shrinks to ~1 BioSIM batch over `test_ecoregionPolys`. Pure-
## validation tests further down do not call this helper and always run.
##
## climr's projection mode still pulls multi-GB GCM rasters regardless of
## bbox / `z`, so even with the env var set that one test takes longer than
## the BioSIM ones. If it becomes a problem, mock `climr::downscale()` or
## move it to an out-of-band integration job.

.skip_unless_network_tests <- function() {
  testthat::skip_on_cran()
  if (!isTRUE(as.logical(Sys.getenv("LANDISUTILS_RUN_NETWORK_TESTS", "false")))) {
    testthat::skip("Set LANDISUTILS_RUN_NETWORK_TESTS=true to run BioSIM/climr fetch tests.")
  }
}

testthat::test_that("BioSIM daily-climate pipeline produces wide LANDIS-II table", {
  .skip_unless_network_tests()
  testthat::skip_if_not_installed("BioSIM")
  testthat::skip_if_not_installed("arrow")
  testthat::skip_if_not_installed("elevatr")
  testthat::skip_if_not_installed("furrr")
  testthat::skip_if_not_installed("sf")
  testthat::skip_if_offline()

  tmp_pth <- withr::local_tempdir("test_climate_biosim_")
  withr::local_options(landisutils.cache.path = tmp_pth)

  ecoregionPolys <- landisutils::test_ecoregionPolys
  clim_years <- 2018L

  daily_weather <- prep_daily_weather(
    vars = c("prcp", "tmax", "tmin"),
    years = clim_years,
    studyArea = ecoregionPolys,
    id = "PolyID",
    z = 6
  )

  testthat::expect_s3_class(daily_weather, "tbl_df")
  testthat::expect_true(all(c("Year", "Month", "Day", "Variable") %in% names(daily_weather)))
  testthat::expect_setequal(unique(daily_weather$Variable), c("precip", "Tmin", "Tmax"))
  testthat::expect_true(all(daily_weather$Year == clim_years))
})

testthat::test_that("assemble_climate_library_file_scf() produces SCF-shaped Variable column", {
  .skip_unless_network_tests()
  testthat::skip_if_not_installed("BioSIM")
  testthat::skip_if_not_installed("arrow")
  testthat::skip_if_not_installed("elevatr")
  testthat::skip_if_not_installed("furrr")
  testthat::skip_if_not_installed("sf")
  testthat::skip_if_offline()

  tmp_pth <- withr::local_tempdir("test_climate_biosim_scf_")
  withr::local_options(landisutils.cache.path = tmp_pth)

  ecoregionPolys <- landisutils::test_ecoregionPolys
  clim_years <- 2018L

  ## populate the BioSIM cache with the 5 variables Social Climate Fire needs
  prep_daily_weather(
    vars = c("prcp", "tmax", "tmin", "ws", "wnddir"),
    years = clim_years,
    studyArea = ecoregionPolys,
    id = "PolyID",
    z = 6
  )

  sa_hash <- landisutils:::.studyArea_hash(ecoregionPolys)
  scenario_tag <- landisutils:::.biosim_scenario_tag("RCP45", "RCM4")
  dataset_path <- file.path(tmp_pth, "ClimaticEx_Daily", sa_hash, scenario_tag)

  scf_df <- assemble_climate_library_file_scf(
    dataset_path = dataset_path,
    vars = c("Prcp", "Tmin", "Tmax", "WndS", "WndD"),
    id_col = "EcoID"
  )

  testthat::expect_s3_class(scf_df, "tbl_df")
  testthat::expect_true(all(c("Year", "Month", "Day", "Variable") %in% names(scf_df)))
  testthat::expect_setequal(
    unique(scf_df$Variable),
    c("precip", "mintemp", "maxtemp", "windspeed", "winddirection")
  )
  testthat::expect_true(all(scf_df$Year == clim_years))

  ## SCF helper must not perturb the LANDIS-flavoured assembly
  landis_df <- assemble_climate_library_file(
    dataset_path = dataset_path,
    vars = c("Prcp", "Tmin", "Tmax", "WndS", "WndD"),
    id_col = "EcoID"
  )
  testthat::expect_setequal(
    unique(landis_df$Variable),
    c("precip", "Tmin", "Tmax", "windSpeed", "windDirection")
  )
})

testthat::test_that("BioSIM monthly-climate pipeline produces wide LANDIS-II table", {
  .skip_unless_network_tests()
  testthat::skip_if_not_installed("BioSIM")
  testthat::skip_if_not_installed("arrow")
  testthat::skip_if_not_installed("elevatr")
  testthat::skip_if_not_installed("furrr")
  testthat::skip_if_not_installed("sf")
  testthat::skip_if_offline()

  tmp_pth <- withr::local_tempdir("test_climate_biosim_monthly_")
  withr::local_options(landisutils.cache.path = tmp_pth)

  ecoregionPolys <- landisutils::test_ecoregionPolys
  clim_years <- 2018L

  monthly_weather <- prep_monthly_weather_biosim(
    vars = c("prcp", "tmax", "tmin", "ws", "wnddir"),
    years = clim_years,
    studyArea = ecoregionPolys,
    id = "PolyID",
    z = 6
  )

  testthat::expect_s3_class(monthly_weather, "tbl_df")
  testthat::expect_true(all(c("Year", "Month", "Variable") %in% names(monthly_weather)))
  testthat::expect_false("Day" %in% names(monthly_weather))
  testthat::expect_setequal(
    unique(monthly_weather$Variable),
    c("precip", "Tmin", "Tmax", "windSpeed", "windDirection")
  )
  testthat::expect_true(all(monthly_weather$Year == clim_years))
  testthat::expect_setequal(unique(monthly_weather$Month), 1:12)

  ## wind direction must stay within [0, 360) after circular-mean summarisation
  eco_cols <- setdiff(names(monthly_weather), c("Year", "Month", "Variable"))
  wnddir_vals <- monthly_weather[monthly_weather$Variable == "windDirection", eco_cols] |>
    unlist(use.names = FALSE)
  testthat::expect_true(all(wnddir_vals >= 0 & wnddir_vals < 360, na.rm = TRUE))
})

testthat::test_that("climr monthly-climate pipeline produces wide LANDIS-II table", {
  .skip_unless_network_tests()
  testthat::skip_if_not_installed("arrow")
  testthat::skip_if_not_installed("climr")
  testthat::skip_if_not_installed("digest")
  testthat::skip_if_not_installed("elevatr")
  testthat::skip_if_not_installed("furrr")
  testthat::skip_if_not_installed("sf")
  testthat::skip_if_not_installed("withr")
  testthat::skip_if_offline()

  tmp_pth <- withr::local_tempdir("test_climate_climr_")
  withr::local_options(landisutils.cache.path = tmp_pth)

  ecoregionPolys <- landisutils::test_ecoregionPolys
  clim_years <- 2018L

  monthly_weather <- prep_monthly_weather_climr(
    vars = c("prcp", "tmax", "tmin"),
    years = clim_years,
    studyArea = ecoregionPolys,
    id = "PolyID",
    z = 6
  )

  testthat::expect_s3_class(monthly_weather, "tbl_df")
  testthat::expect_true(all(c("Year", "Month", "Variable") %in% names(monthly_weather)))
  testthat::expect_false("Day" %in% names(monthly_weather))
  testthat::expect_setequal(unique(monthly_weather$Variable), c("precip", "Tmin", "Tmax"))
  testthat::expect_true(all(monthly_weather$Year == clim_years))
  testthat::expect_setequal(unique(monthly_weather$Month), 1:12)
})

testthat::test_that("BioSIM daily future-scenario pipeline namespaces cache and runs", {
  .skip_unless_network_tests()
  testthat::skip_if_not_installed("BioSIM")
  testthat::skip_if_not_installed("arrow")
  testthat::skip_if_not_installed("elevatr")
  testthat::skip_if_not_installed("furrr")
  testthat::skip_if_not_installed("sf")
  testthat::skip_if_offline()
  testthat::skip_if_not(exists("test_ecoregionPolys", envir = asNamespace("landisutils")))

  tmp_pth <- withr::local_tempdir("test_climate_biosim_future_")
  withr::local_options(landisutils.cache.path = tmp_pth)

  ecoregionPolys <- landisutils::test_ecoregionPolys
  clim_years <- 2041L

  daily_weather <- prep_daily_weather(
    vars = c("prcp", "tmax", "tmin"),
    years = clim_years,
    studyArea = ecoregionPolys,
    id = "PolyID",
    z = 6,
    rcp = "RCP45",
    clim_model = "GCM4"
  )

  testthat::expect_s3_class(daily_weather, "tbl_df")
  testthat::expect_true(all(daily_weather$Year == clim_years))
  testthat::expect_setequal(unique(daily_weather$Variable), c("precip", "Tmin", "Tmax"))

  ## cache should be namespaced by RCP45_GCM4
  scenario_dirs <- list.files(
    file.path(tmp_pth, "ClimaticEx_Daily"),
    recursive = FALSE,
    include.dirs = TRUE,
    full.names = FALSE
  )
  expected_tag <- "RCP45_GCM4"
  hits <- list.files(
    file.path(tmp_pth, "ClimaticEx_Daily"),
    pattern = paste0("^", expected_tag, "$"),
    recursive = TRUE,
    include.dirs = TRUE,
    full.names = FALSE
  )
  testthat::expect_true(length(hits) >= 1L)
})

testthat::test_that("BioSIM args validate: bad rcp / clim_model rejected", {
  testthat::skip_if_not(exists("test_ecoregionPolys", envir = asNamespace("landisutils")))

  testthat::expect_error(
    prep_daily_weather(
      vars = "prcp",
      years = 2018L,
      studyArea = landisutils::test_ecoregionPolys,
      id = "PolyID",
      rcp = "BOGUS"
    ),
    "should be one of"
  )
  testthat::expect_error(
    prep_daily_weather(
      vars = "prcp",
      years = 2018L,
      studyArea = landisutils::test_ecoregionPolys,
      id = "PolyID",
      clim_model = "BOGUS"
    ),
    "should be one of"
  )
})

testthat::test_that("climr_ensemble_8 names match climr::list_gcms()", {
  testthat::skip_if_not_installed("climr")

  testthat::expect_length(landisutils::climr_ensemble_8, 8L)
  testthat::expect_true(all(landisutils::climr_ensemble_8 %in% climr::list_gcms()))
})

testthat::test_that("climr projection mode validates gcms/ssps/years", {
  testthat::skip_if_not_installed("climr")
  testthat::skip_if_not(exists("test_ecoregionPolys", envir = asNamespace("landisutils")))

  ## gcms without ssps -> error
  testthat::expect_error(
    prep_monthly_weather_climr(
      vars = "prcp",
      years = 2041L,
      studyArea = landisutils::test_ecoregionPolys,
      id = "PolyID",
      gcms = "MIROC6"
    ),
    "ssps"
  )
  ## bogus GCM
  testthat::expect_error(
    prep_monthly_weather_climr(
      vars = "prcp",
      years = 2041L,
      studyArea = landisutils::test_ecoregionPolys,
      id = "PolyID",
      gcms = "BOGUS-GCM",
      ssps = "ssp245"
    ),
    "unknown GCM"
  )
  ## bogus SSP
  testthat::expect_error(
    prep_monthly_weather_climr(
      vars = "prcp",
      years = 2041L,
      studyArea = landisutils::test_ecoregionPolys,
      id = "PolyID",
      gcms = "MIROC6",
      ssps = "ssp999"
    ),
    "unknown SSP"
  )
  ## year outside list_gcm_ssp_years()
  testthat::expect_error(
    prep_monthly_weather_climr(
      vars = "prcp",
      years = 1900L,
      studyArea = landisutils::test_ecoregionPolys,
      id = "PolyID",
      gcms = "MIROC6",
      ssps = "ssp245"
    ),
    "outside climr GCM SSP timeseries range"
  )
})

testthat::test_that("climr projection-mode pipeline produces wide LANDIS-II table", {
  .skip_unless_network_tests()
  testthat::skip_if_not_installed("arrow")
  testthat::skip_if_not_installed("climr")
  testthat::skip_if_not_installed("digest")
  testthat::skip_if_not_installed("elevatr")
  testthat::skip_if_not_installed("furrr")
  testthat::skip_if_not_installed("sf")
  testthat::skip_if_not_installed("withr")
  testthat::skip_if_offline()

  tmp_pth <- withr::local_tempdir("test_climate_climr_future_")
  withr::local_options(landisutils.cache.path = tmp_pth)

  ecoregionPolys <- landisutils::test_ecoregionPolys
  clim_years <- 2041L

  ## small 2-GCM subset to keep runtime down; uses the same code path as the
  ## bcgov 8-member ensemble.
  monthly_weather <- prep_monthly_weather_climr(
    vars = c("prcp", "tmax", "tmin"),
    years = clim_years,
    studyArea = ecoregionPolys,
    id = "PolyID",
    z = 6,
    gcms = c("MIROC6", "MPI-ESM1-2-HR"),
    ssps = "ssp245",
    max_run = 0L
  )

  testthat::expect_s3_class(monthly_weather, "tbl_df")
  testthat::expect_true(all(c("Year", "Month", "Variable") %in% names(monthly_weather)))
  testthat::expect_setequal(unique(monthly_weather$Variable), c("precip", "Tmin", "Tmax"))
  testthat::expect_true(all(monthly_weather$Year == clim_years))
  testthat::expect_setequal(unique(monthly_weather$Month), 1:12)

  ## cache should be namespaced by gcm_ssp245_run0
  hits <- list.files(
    file.path(tmp_pth, "Climr_Monthly"),
    pattern = "^gcm_ssp245_run0$",
    recursive = TRUE,
    include.dirs = TRUE,
    full.names = FALSE
  )
  testthat::expect_true(length(hits) >= 1L)
})

testthat::test_that("climr wrapper rejects wind variables with a clear error", {
  testthat::skip_if_not_installed("climr")

  testthat::expect_error(
    prep_monthly_weather_climr(
      vars = "ws",
      years = 2018L,
      studyArea = landisutils::test_ecoregionPolys,
      id = "PolyID"
    ),
    "climr does not provide wind"
  )
})

testthat::test_that("Climate inputs are properly created", {
  skip("incomplete") ## legacy AppEEARS workflow; not exercised by the BioSIM refactor

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

  # -----------------------------------------------------
  Renviron_file <- "~/GitHub/BC_HRV/BC_HRV.Renviron"
  skip_if_not(file.exists(Renviron_file))

  readRenviron(Renviron_file)

  library(future)

  options(keyring_backend = "file")

  keyring::keyring_unlock("appeears", "")

  studyAreaName <- "NRR_Cariboo"
  tmp_pth <- glue::glue("~/GitHub/BC_HRV/LANDIS-II/{studyAreaName}_landis_LH_hrv_NDTBEC_FRT_res125")
  studyArea <- file.path(tmp_pth, "ecoregions.shp") |> sf::st_read(quiet = TRUE)

  tmp_data_path <- file.path(tmp_pth, "climate", studyAreaName) |> fs::dir_create()
  var <- clim_vars_daily
  # clim_years <- 1980:1999 ## done
  # clim_years <- 2000:2023 ## done
  clim_years <- 1993 ## in progress

  tasks <- lapply(clim_years, function(year) {
    appeears::rs_build_task(
      df = data.frame(
        task = rep("DAYMET", length(var)), ## folder name
        subtask = var, ## subfolder name
        start = rep(glue::glue("{year}-01-01"), length(var)),
        end = rep(glue::glue("{year}-12-31"), length(var)),
        product = rep("DAYMET.004", length(var)),
        layer = var
      ),
      roi = sf::st_convex_hull(studyArea) |> sf::st_as_sf(),
      format = "geotiff"
    )
  })

  token <- appeears::rs_login(user = Sys.getenv("EARTH_DATA_USER"))

  reqs <- lapply(tasks, function(task) {
    appeears::rs_request(
      request = task,
      user = Sys.getenv("EARTH_DATA_USER"),
      transfer = FALSE, ## TODO: try TRUE, but use purrr::insistently()
      path = tmp_data_path,
      time_out = 14400,
      verbose = TRUE
    )
  })

  task_ids <- lapply(reqs, function(req) {
    req$get_task_id()
  }) |>
    setNames(clim_years)

  ## record here in case session is lost / disconnected
  # dput(task_ids)

  ## takes ~30 mins from pending ==> processing; another ~20 mins from processing ==> done
  ll <- lapply(
    reqs,
    purrr::insistently(function(req) {
      req$update_status()$get_status()
    })
  ) |>
    setNames(clim_years)
  data.frame(year = names(ll), status = unlist(ll, use.names = FALSE))

  # plan(sequential)
  plan(multisession, workers = length(task_ids))

  ## once it's done, can transfer
  future.apply::future_lapply(
    reqs,
    purrr::insistently(
      function(req) {
        readRenviron("~/GitHub/BC_HRV/BC_HRV.Renviron")

        options(keyring_backend = "file")

        keyring::keyring_unlock("appeears", "")

        req_id <- req$get_task_id()

        if (req$update_status()$get_status() == "done") {
          message(glue::glue("Starting download for id {req_id} ..."))

          ## NOTE: does not actually update req status to indicate completed download
          appeears::rs_transfer(
            task_id = req_id,
            user = Sys.getenv("EARTH_DATA_USER"),
            path = tmp_data_path,
            verbose = TRUE
          )
        } else {
          ## trigger error after delay, to force retry
          Sys.sleep(60 * 30)
          stop(glue::glue("Request {req_id} not yet ready for download."))
        }
      },
      rate = purrr::rate_delay(60 * 10)
    ),
    future.seed = TRUE
  )

  task_ids <- list(
    # "2000" = "bd55dcd2-ae94-4c0e-943b-e6044c9b9231",
    # "2001" = "c1a6e382-661f-427f-b76b-0fe119d17867",
    # "2002" = "79b74e9e-d958-4822-819f-844b77408144",
    # "2003" = "7603653e-05a6-411b-9c5d-bb43e25b41b5",
    # "2004" = "402f979d-f797-489f-a44a-7ae1ea4e1656",
    # "2005" = "7326ae37-8b60-4518-8abf-acb497315c6c",
    # "2006" = "b3ae61cc-e651-46ae-8aca-f46484c31931",
    # "2007" = "6818a4d6-57dd-42e0-aa5c-19f112c15b4b",
    # "2008" = "4d0535f1-c77b-4e1f-84b0-350664951aed",
    # "2009" = "20212699-1bb8-4bca-afe1-ffe28c814fc6",
    # "2010" = "64fb0201-b327-41fb-8a30-5b69fb17f0f4",
    # "2011" = "7347cfd2-d5a5-44c7-902d-511d42f802b8",
    # "2012" = "a4136357-0903-4237-9b6a-08d70efb1418",
    # "2013" = "b1c790f0-2d0f-43d9-8157-f4b9bb80806e",
    # "2014" = "ab522b03-dd16-41d9-bf10-3fc1b942b2d2",
    # "2015" = "67c1c911-b0b1-4cf5-8107-d3efbf3dc097",
    # "2016" = "e80e2dcb-85db-423d-8055-345aed8bf140",
    # "2017" = "5aa9142c-8d32-4a35-b4c4-b5c0163e7b6c",
    # "2018" = "4942b827-1a1f-49c2-a7d4-2580bce6f63c",
    # "2019" = "c01cadeb-ab7a-49f5-80d7-010e728afdd1",
    # "2020" = "188ee63f-b9ac-4126-8121-0f3f4db5d6bc",
    # "2021" = "61cf2bb1-eead-4857-a195-5ed961056f3b",
    # "2022" = "dac1414b-781c-49c1-a24b-263d2330e41f",
    # "2023" = "68e8b156-3328-4636-8231-277c3ed1b0c0"
  )

  task_ids <- list(
    # "1980" = "086cfc6f-e149-4331-bbdb-d84e9e63d443",
    # "1981" = "769a86df-23c6-4c66-a745-59e90e49b50a",
    # "1982" = "058b9001-c7ce-4edd-8ca1-26cedf1795a0",
    # "1983" = "65ae8811-34c6-40b7-a101-47e97824ea1e",
    # "1984" = "ddc5b848-79aa-424b-befe-a398bd58e2f6",
    # "1985" = "fd59abff-18db-46a4-8eee-e3ef8a52f04e",
    # "1986" = "2db57b3e-d883-4920-a282-823027557910",
    # "1987" = "4baabc08-2662-4d6d-832f-41753693fa08",
    # "1988" = "c6ea87ac-2050-440e-85a4-b379b569df5f",
    # "1989" = "d1c19aa5-8f6b-43e8-90bf-4f02c983dac0",
    # "1990" = "4217437b-0d89-42e9-8b06-dc770ed3c125",
    # "1991" = "2ad4fa32-a0f8-4cdc-ab20-3e785096ba14",
    # "1992" = "04fad1cd-4adc-4acf-b306-18ac87a87e70",
    # "1993" = "1fc9c820-f211-4a25-add6-52bbaedb457f",
    "1993" = "c3404b84-7597-4b0a-a87c-e27e71ebbe20"
    # "1994" = "7b025120-bf9d-4802-9577-edaee30d773a",
    # "1995" = "756e8185-1338-4c9f-b776-f82289576fe5",
    # "1996" = "148912f9-8514-4864-8cc3-27634b459e11",
    # "1997" = "f6f77d55-fee4-4f91-9c26-f44ef2f53067",
    # "1998" = "50022ddb-58e0-4cbe-b203-ff83601c6216",
    # "1999" = "5965db5f-3eb0-4504-a49d-2cbe0f7f8746"
  )

  future.apply::future_lapply(
    task_ids,
    purrr::insistently(function(id) {
      readRenviron("~/GitHub/BC_HRV/BC_HRV.Renviron")

      options(keyring_backend = "file")

      keyring::keyring_unlock("appeears", "")

      message(glue::glue("Starting download for id {id} ..."))
      appeears::rs_transfer(
        id,
        user = Sys.getenv("EARTH_DATA_USER"),
        path = tmp_data_path,
        verbose = TRUE
      )
    })
  )

  # r <- terra::rast(list.files(tmp_pth, pattern = "[.]tif$")[1])

  ## rename these to be more sensible; convert DOY ==> YYYY-MM-DD
  r.name <- names(r)
  r.name <- "DAYMET.004_prcp_doy2011016_aid0001.tif"
  r.year <- stringr::str_extract(r.name, "doy.{4}") |> substr(4, 7) |> as.integer()
  r.doy <- stringr::str_extract(r.name, "doy.{7}") |> substr(8, 10) |> as.integer()
  r.date <- as.Date(r.doy - 1, origin = paste0(r.year, "-01-01"))
  r.month <- strftime(r.date, "%m")
  r.day <- strftime(r.date, "%d")

  names(r) <- sub("doy.{7}", r.date, r.name) |>
    sub("DAYMET[.]004_", "", x = _) |>
    sub("_aid0001", "", x = _)

  # -----------------------------------------------------

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
