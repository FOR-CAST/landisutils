testthat::test_that("Root Rot extension is properly created", {
  ## NOTE: parameter list and example values from the LANDIS-II Root Rot v1.0
  ## User Guide (Section 2 + Example File in Section 4):
  ## Docs/LANDIS-II Root Rot v1.0 User Guide.pdf
  ## <https://github.com/LANDIS-II-Foundation/Extension-Root-Rot/tree/master/Docs>

  tmp_pth <- withr::local_tempdir("test_RootRot_")

  susceptibility <- data.frame(
    Species = c("pinustro", "pinubank", "pinuresi"),
    Index1 = c(0.5, 0.5, 0.5),
    Index2 = c(0.05, 0.05, 0.05)
  )

  suppressWarnings({
    ext <- RootRot$new(
      path = tmp_pth,
      Timestep = 5L,
      Pathogen = "Phytophthora cinnamomi",
      SpeciesSusceptibility = susceptibility,
      LethalTemp = -30,
      MinSoilTemp = 10,
      PhWet = 51,
      PhDry = 102,
      PhMax = 250,
      MinProbID = 0.10,
      MaxProbDI = 0.85
    )
  })
  ext$write()

  testthat::expect_true(all(file.exists(file.path(tmp_pth, ext$files))))

  contents <- readLines(file.path(tmp_pth, ext$files[1]))
  testthat::expect_true(any(grepl(
    "^LandisData\\s+\"Root Rot\"\\s+<<Phytophthora cinnamomi",
    contents
  )))
  testthat::expect_true(any(grepl("^Timestep\\s+5", contents)))
  testthat::expect_true(any(grepl("^SpeciesSusceptibility", contents)))
  testthat::expect_true(any(grepl("^pinustro", contents)))
  testthat::expect_true(any(grepl("^LethalTemp\\s+-30", contents)))
  testthat::expect_true(any(grepl("^MinSoilTemp\\s+10", contents)))
  testthat::expect_true(any(grepl("^PhWet\\s+51", contents)))
  testthat::expect_true(any(grepl("^PhDry\\s+102", contents)))
  testthat::expect_true(any(grepl("^PhMax\\s+250", contents)))
  testthat::expect_true(any(grepl("^MinProbID\\s+0\\.1", contents)))
  testthat::expect_true(any(grepl("^MaxProbDI\\s+0\\.85", contents)))
  testthat::expect_true(any(grepl("^OutputMapName\\s+rootrot/RootRot-", contents)))
  testthat::expect_true(any(grepl("^TOLDMapName\\s+rootrot/TOLD-", contents)))
  testthat::expect_true(any(grepl("^SpeciesBiomassRemovedMapName", contents)))
  ## InputMap and LethalTempMapName optional and not provided -> should not appear
  testthat::expect_false(any(grepl("^InputMap", contents)))
  testthat::expect_false(any(grepl("^LethalTempMapName", contents)))

  withr::deferred_run()
})

testthat::test_that("RootRot rejects bad SpeciesSusceptibility", {
  tmp_pth <- withr::local_tempdir("test_RootRot_")

  ## missing Index2
  bad <- data.frame(Species = "abiebals", Index1 = 0.1)
  suppressWarnings(
    testthat::expect_error(
      RootRot$new(path = tmp_pth, Timestep = 5L, SpeciesSusceptibility = bad)
    )
  )

  ## Index out of range
  bad2 <- data.frame(Species = "abiebals", Index1 = 1.5, Index2 = 0)
  suppressWarnings(
    testthat::expect_error(
      RootRot$new(path = tmp_pth, Timestep = 5L, SpeciesSusceptibility = bad2)
    )
  )

  withr::deferred_run()
})

testthat::test_that("RootRot$new() warns about missing LANDIS-II v8 compatibility", {
  tmp_pth <- withr::local_tempdir("test_RootRot_")

  testthat::expect_warning(
    RootRot$new(path = tmp_pth, Timestep = 5L),
    regexp = "LANDIS-II v8"
  )

  withr::deferred_run()
})

testthat::test_that("RootRot LethalTemp must be <= 0", {
  tmp_pth <- withr::local_tempdir("test_RootRot_")

  suppressWarnings(
    testthat::expect_error(
      RootRot$new(path = tmp_pth, Timestep = 5L, LethalTemp = 5)
    )
  )

  withr::deferred_run()
})
