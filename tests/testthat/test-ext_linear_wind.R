testthat::test_that("Linear Wind inputs are properly created", {
  ## NOTE: reference values from the LANDIS-II Core v8 / Linear Wind v3 test input:
  ## testing/Core8-LinearWind3.0/LinearWind_Input.txt
  ## <https://github.com/LANDIS-II-Foundation/Extension-LinearWind/tree/master/testing/Core8-LinearWind3.0>

  tmp_pth <- withr::local_tempdir("test_LinearWind_")

  eco_mods <- data.frame(Ecoregion = "101", Modifier = -0.5)

  ext_wind <- LinearWind$new(
    path = tmp_pth,
    Timestep = 10L,
    NumEventsMean = 8.4,
    NumEventsStDev = 0.08,
    TornadoLengthLambda = 25.75,
    TornadoLengthAlpha = 26.5,
    TornadoWidth = 0.420,
    TornadoIntensityTable = c(0, 5, 20, 50, 25),
    TornadoProp = 0.76,
    DerechoLengthLambda = 178.0,
    DerechoLengthAlpha = 3.6,
    DerechoWidth = 40.000,
    DerechoIntensityTable = c(5, 15, 50, 25, 5),
    PropIntensityVar = 0.65,
    WindDirectionTable = c(0, 30, 60, 10),
    EcoregionModifiers = eco_mods,
    IntensityMapNames = "linearwind/intensity-{timestep}.tif",
    SeverityMapNames = "linearwind/severity-{timestep}.tif",
    LogFile = "linearwind/linearwind-event-log.csv"
  )

  ext_wind$write()

  testthat::expect_true(all(file.exists(file.path(tmp_pth, ext_wind$files))))

  contents <- readLines(file.path(tmp_pth, ext_wind$files[1]))
  testthat::expect_true(any(grepl("^LandisData\\s+\"Linear Wind\"", contents)))
  testthat::expect_true(any(grepl("^Timestep\\s+10", contents)))
  testthat::expect_true(any(grepl("^NumEventsMean\\s+8\\.4", contents)))
  testthat::expect_true(any(grepl("^NumEventsStDev\\s+0\\.08", contents)))
  testthat::expect_true(any(grepl("^TornadoLengthLambda\\s+25\\.75", contents)))
  testthat::expect_true(any(grepl("^TornadoLengthAlpha\\s+26\\.5", contents)))
  testthat::expect_true(any(grepl("^TornadoWidth\\s+0\\.42", contents)))
  testthat::expect_true(any(grepl("^TornadoIntensityTable", contents)))
  testthat::expect_true(any(grepl("^TornadoProp\\s+0\\.76", contents)))
  testthat::expect_true(any(grepl("^DerechoLengthLambda\\s+178", contents)))
  testthat::expect_true(any(grepl("^DerechoLengthAlpha\\s+3\\.6", contents)))
  testthat::expect_true(any(grepl("^DerechoWidth\\s+40", contents)))
  testthat::expect_true(any(grepl("^DerechoIntensityTable", contents)))
  testthat::expect_true(any(grepl("^PropIntensityVar\\s+0\\.65", contents)))
  testthat::expect_true(any(grepl("^WindDirectionTable", contents)))
  testthat::expect_true(any(grepl("N-S", contents)))
  testthat::expect_true(any(grepl("^EcoregionModifiers", contents)))
  testthat::expect_true(any(grepl("\\b101\\b", contents)))
  testthat::expect_true(any(grepl("^WindSeverities", contents)))
  testthat::expect_true(any(grepl("0% to 20%", contents)))
  testthat::expect_true(any(grepl("linearwind/intensity-\\{timestep\\}", contents)))
  testthat::expect_true(any(grepl("linearwind/severity-\\{timestep\\}", contents)))
  testthat::expect_true(any(grepl("linearwind-event-log\\.csv", contents)))

  withr::deferred_run()
})

testthat::test_that("Linear Wind rejects intensity tables that do not sum to 100", {
  tmp_pth <- withr::local_tempdir("test_LinearWind_")

  testthat::expect_error(LinearWind$new(
    path = tmp_pth,
    Timestep = 10L,
    NumEventsMean = 0.5,
    NumEventsStDev = 0.1,
    TornadoLengthLambda = 25,
    TornadoLengthAlpha = 1.2,
    TornadoWidth = 0.4,
    TornadoIntensityTable = c(10, 10, 10, 10, 10), ## sums to 50, not 100
    TornadoProp = 0.7,
    DerechoLengthLambda = 160,
    DerechoLengthAlpha = 50,
    DerechoWidth = 40,
    DerechoIntensityTable = c(5, 15, 50, 25, 5),
    PropIntensityVar = 0.5,
    WindDirectionTable = c(25, 25, 25, 25)
  ))

  withr::deferred_run()
})

testthat::test_that("Linear Wind rejects WindDirectionTable that does not sum to 100", {
  tmp_pth <- withr::local_tempdir("test_LinearWind_")

  testthat::expect_error(LinearWind$new(
    path = tmp_pth,
    Timestep = 10L,
    NumEventsMean = 0.5,
    NumEventsStDev = 0.1,
    TornadoLengthLambda = 25,
    TornadoLengthAlpha = 1.2,
    TornadoWidth = 0.4,
    TornadoIntensityTable = c(0, 5, 20, 50, 25),
    TornadoProp = 0.7,
    DerechoLengthLambda = 160,
    DerechoLengthAlpha = 50,
    DerechoWidth = 40,
    DerechoIntensityTable = c(5, 15, 50, 25, 5),
    PropIntensityVar = 0.5,
    WindDirectionTable = c(10, 20, 30, 20) ## sums to 80
  ))

  withr::deferred_run()
})

testthat::test_that("Linear Wind rejects WindSeverities not in decreasing order", {
  tmp_pth <- withr::local_tempdir("test_LinearWind_")

  bad_severities <- data.frame(
    Severity = c(1L, 2L, 3L), ## increasing, not decreasing
    LowerAge = c(0L, 50L, 85L),
    UpperAge = c(50L, 85L, 100L),
    WindspeedMortalityThreshold = c(0.1, 0.5, 0.9)
  )

  testthat::expect_error(LinearWind$new(
    path = tmp_pth,
    Timestep = 10L,
    NumEventsMean = 0.5,
    NumEventsStDev = 0.1,
    TornadoLengthLambda = 25,
    TornadoLengthAlpha = 1.2,
    TornadoWidth = 0.4,
    TornadoIntensityTable = c(0, 5, 20, 50, 25),
    TornadoProp = 0.7,
    DerechoLengthLambda = 160,
    DerechoLengthAlpha = 50,
    DerechoWidth = 40,
    DerechoIntensityTable = c(5, 15, 50, 25, 5),
    PropIntensityVar = 0.5,
    WindDirectionTable = c(0, 30, 60, 10),
    WindSeverities = bad_severities
  ))

  withr::deferred_run()
})
