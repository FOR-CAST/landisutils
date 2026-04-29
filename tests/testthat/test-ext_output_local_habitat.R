testthat::test_that("Local Habitat Output extension is properly created", {
  ## NOTE: reference values from the LANDIS-II Core8 / Local Habitat v3 test input:
  ## test/Core8-LocalHabitat3.0/Habitat_input.txt
  ## <https://github.com/LANDIS-II-Foundation/Extension-Local-Habitat-Suitability-Output/tree/master/test/Core8-LocalHabitat3.0>

  tmp_pth <- withr::local_tempdir("test_OutputLocalHabitat_")

  ext <- OutputLocalHabitat$new(
    path = tmp_pth,
    Timestep = 1L,
    OutputTimestep = 10L,
    MapFileNames = "output/habitat/{HabitatName}-{timestep}.tif",
    SuitabilityFiles = list(
      suitabilityFile("AgeClass_ForestType_example.txt"),
      suitabilityFile("AgeClass_TimeSinceFire_example.txt"),
      suitabilityFile("ForestType_TimeSinceFire_example.txt"),
      suitabilityFile("ForestType_TimeSinceHarvest_example.txt")
    )
  )
  ext$write()

  testthat::expect_true(all(file.exists(file.path(tmp_pth, ext$files))))

  contents <- readLines(file.path(tmp_pth, ext$files[1]))
  testthat::expect_true(any(grepl("^LandisData\\s+\"Local Habitat Output\"", contents)))
  testthat::expect_true(any(grepl("^Timestep\\s+1", contents)))
  testthat::expect_true(any(grepl("^OutputTimestep\\s+10", contents)))
  testthat::expect_true(any(grepl("^MapFileNames\\s+output/habitat/\\{HabitatName\\}", contents)))
  testthat::expect_true(any(grepl("^SuitabilityFiles\\s+AgeClass_ForestType_example\\.txt", contents)))
  testthat::expect_true(any(grepl("ForestType_TimeSinceHarvest_example\\.txt", contents)))

  withr::deferred_run()
})

testthat::test_that("OutputLocalHabitat accepts a character vector of suitability paths", {
  tmp_pth <- withr::local_tempdir("test_OutputLocalHabitat_")

  ext <- OutputLocalHabitat$new(
    path = tmp_pth,
    Timestep = 1L,
    OutputTimestep = 5L,
    SuitabilityFiles = c("a.txt", "b.txt")
  )
  testthat::expect_length(ext$SuitabilityFiles, 2L)
  testthat::expect_true(all(vapply(ext$SuitabilityFiles, inherits, logical(1), "SuitabilityFile")))

  withr::deferred_run()
})

testthat::test_that("OutputLocalHabitat rejects invalid MapFileNames pattern", {
  tmp_pth <- withr::local_tempdir("test_OutputLocalHabitat_")

  testthat::expect_error(
    OutputLocalHabitat$new(
      path = tmp_pth,
      Timestep = 1L,
      MapFileNames = "no-placeholders.tif",
      SuitabilityFiles = list(suitabilityFile("a.txt"))
    )
  )

  withr::deferred_run()
})

testthat::test_that("OutputLocalHabitat write() requires at least one suitability file", {
  tmp_pth <- withr::local_tempdir("test_OutputLocalHabitat_")

  ext <- OutputLocalHabitat$new(path = tmp_pth, Timestep = 1L, OutputTimestep = 5L)
  testthat::expect_error(ext$write())

  withr::deferred_run()
})
