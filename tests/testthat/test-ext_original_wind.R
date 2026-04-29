testthat::test_that("Original Wind inputs are properly created", {
  ## NOTE: reference values from the LANDIS-II Core v8 / Original Wind v4.0 test input:
  ## testings/Core8-OriginalWind4.0/original-wind.txt
  ## <https://github.com/LANDIS-II-Foundation/Extension-Base-Wind/tree/master/testings/Core8-OriginalWind4.0>

  tmp_pth <- withr::local_tempdir("test_OriginalWind_")

  wep <- tibble::tribble(
    ~Ecoregion , ~MaxSize , ~MeanSize , ~MinSize , ~WindRotationPeriod ,
    "101"      , 400      , 100       , 4        , 100L                ,
    "102"      , 600      , 200       , 6        , 50L
  )

  ext_wind <- OriginalWind$new(
    path = tmp_pth,
    Timestep = 5L,
    WindEventParametersTable = wep,
    MapNames = "outputs-wind/severity-{timestep}.tif",
    SummaryLogFile = "outputs-wind/wind-summary-log.csv",
    EventLogFile = "outputs-wind/wind-events-log.csv"
  )

  ext_wind$write()

  testthat::expect_true(all(file.exists(file.path(tmp_pth, ext_wind$files))))

  contents <- readLines(file.path(tmp_pth, ext_wind$files[1]))
  testthat::expect_true(any(grepl("^LandisData\\s+\"Original Wind\"", contents)))
  testthat::expect_true(any(grepl("^Timestep\\s+5", contents)))
  testthat::expect_true(any(grepl("\\b101\\b", contents)))
  testthat::expect_true(any(grepl("\\b102\\b", contents)))
  testthat::expect_true(any(grepl("^WindSeverities", contents)))
  testthat::expect_true(any(grepl("0% to 20%", contents)))
  testthat::expect_true(any(grepl("outputs-wind/severity-\\{timestep\\}", contents)))
  testthat::expect_true(any(grepl("wind-summary-log\\.csv", contents)))
  testthat::expect_true(any(grepl("wind-events-log\\.csv", contents)))

  withr::deferred_run()
})

testthat::test_that("Original Wind rejects invalid WindEventParametersTable", {
  tmp_pth <- withr::local_tempdir("test_OriginalWind_")

  bad_min_gt_max <- data.frame(
    Ecoregion = "Eco1",
    MaxSize = 10,
    MeanSize = 5,
    MinSize = 20,
    WindRotationPeriod = 50L
  )

  testthat::expect_error(
    OriginalWind$new(path = tmp_pth, Timestep = 10L, WindEventParametersTable = bad_min_gt_max)
  )

  withr::deferred_run()
})

testthat::test_that("Original Wind rejects MapNames without {timestep}", {
  tmp_pth <- withr::local_tempdir("test_OriginalWind_")

  wep <- data.frame(
    Ecoregion = "Eco1",
    MaxSize = 100,
    MeanSize = 10,
    MinSize = 1,
    WindRotationPeriod = 50L
  )

  testthat::expect_error(
    OriginalWind$new(
      path = tmp_pth,
      Timestep = 10L,
      WindEventParametersTable = wep,
      MapNames = "outputs-wind/severity.tif" ## missing {timestep}
    )
  )

  withr::deferred_run()
})

testthat::test_that("Original Wind rejects WindSeverities not in decreasing order", {
  tmp_pth <- withr::local_tempdir("test_OriginalWind_")

  wep <- data.frame(
    Ecoregion = "Eco1",
    MaxSize = 100,
    MeanSize = 10,
    MinSize = 1,
    WindRotationPeriod = 50L
  )

  bad_severities <- data.frame(
    Severity = c(1L, 2L, 3L), ## increasing, not decreasing
    LowerAge = c(0L, 50L, 85L),
    UpperAge = c(50L, 85L, 100L),
    MortalityProbability = c(0.1, 0.5, 0.9)
  )

  testthat::expect_error(
    OriginalWind$new(
      path = tmp_pth,
      Timestep = 10L,
      WindEventParametersTable = wep,
      WindSeverities = bad_severities
    )
  )

  withr::deferred_run()
})
