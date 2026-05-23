testthat::test_that("Hurricane extension is properly created", {
  ## NOTE: reference values from the LANDIS-II Core8 / Biomass Hurricane v3 test input:
  ## testing/Core8-Hurricane3.0/Hurricane_v3.txt
  ## <https://github.com/LANDIS-II-Foundation/Extension-Biomass-Hurricane/tree/master/testing/Core8-Hurricane3.0>

  tmp_pth <- withr::local_tempdir("test_Hurricane_")

  occurrence <- data.frame(
    Storms = 0:8,
    Probability = c(0.05, 0.15, 0.22, 0.22, 0.17, 0.10, 0.05, 0.03, 0.01)
  )

  exposure <- data.frame(
    Degree = c(135L, 180L, 225L),
    MapName = c("test_135_wind_fix.tif", "test_180_wind_fix.tif", "test_225_wind_fix.tif")
  )

  default_curve <- defaultHurricaneMortalityCurve()
  vulns <- lapply(c("MountainHemlock", "SitkaSpruce", "WesternHemlock"), function(sp) {
    windSpeedVulnerability(species = sp, maxAge = 800, mortality = default_curve)
  })

  ext <- Hurricane$new(
    path = tmp_pth,
    Timestep = 1L,
    InputUnitsEnglish = FALSE,
    HurricaneRandomNumberSeed = 1974L,
    StormOccurrenceProbabilities = occurrence,
    LowBoundLandfallWindSpeed = 51,
    ModeLandfallWindSpeed = 165,
    HighBoundLandfallWindSpeed = 161,
    CoastalSlope = -0.903,
    MeanStormIntersectionX = 417.618,
    MeanStormIntersectionY = 105.420,
    LandfallSigma = 1,
    StormDirectionMu = 204.4397,
    StormDirectionSigma = 71.06188,
    MinimumWindSpeedforDamage = 60,
    ExposureMaps = exposure,
    WindSpeedVulnerabilities = vulns,
    LogFile = "hurricane-log.csv",
    WindReductionTableCSV = "EvennessWindReductions.csv"
  )
  ext$write()

  testthat::expect_true(all(file.exists(file.path(tmp_pth, ext$files))))

  contents <- readLines(file.path(tmp_pth, ext$files[1]))
  testthat::expect_true(any(grepl("^LandisData\\s+\"Hurricane\"", contents)))
  testthat::expect_true(any(grepl("^Timestep\\s+1", contents)))
  testthat::expect_true(any(grepl("^InputUnitsEnglish\\s+N$", contents)))
  testthat::expect_true(any(grepl("^HurricaneRandomNumberSeed\\s+1974", contents)))
  testthat::expect_true(any(grepl("^StormOccurrenceProbabilities", contents)))
  testthat::expect_true(any(grepl("^CoastalSlope\\s+-0\\.903", contents)))
  testthat::expect_true(any(grepl("^ExposureMaps", contents)))
  testthat::expect_true(any(grepl("test_135_wind_fix\\.tif", contents)))
  testthat::expect_true(any(grepl("^WindSpeedVulnerabilities", contents)))
  testthat::expect_true(any(grepl("MountainHemlock\\s+800\\s+60:0\\.05", contents)))
  testthat::expect_true(any(grepl("^MapNames\\s+hurricane/max-windspeed-", contents)))

  withr::deferred_run()
})

testthat::test_that("Hurricane rejects bad StormOccurrenceProbabilities (sum != 1)", {
  tmp_pth <- withr::local_tempdir("test_Hurricane_")

  bad_occ <- data.frame(Storms = 0:1, Probability = c(0.5, 0.3))
  testthat::expect_error(Hurricane$new(
    path = tmp_pth,
    Timestep = 1L,
    StormOccurrenceProbabilities = bad_occ
  ))

  withr::deferred_run()
})

testthat::test_that("windSpeedVulnerability rejects malformed mortality vector", {
  testthat::expect_error(windSpeedVulnerability(
    species = "abiebals",
    maxAge = 100,
    mortality = c(0.1, 0.5)
  ))
  testthat::expect_error(windSpeedVulnerability(
    species = "abiebals",
    maxAge = 100,
    mortality = c("60" = 1.5)
  ))
})
