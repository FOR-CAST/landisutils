testthat::test_that("Wildlife Habitat Output extension is properly created", {
  ## NOTE: reference values from the LANDIS-II Core8 / Wildlife Habitat v3 test input:
  ## test/Core8-Wildlife3.0/NESands_Habitat.txt
  ## <https://github.com/LANDIS-II-Foundation/Extension-Output-Wildlife-Habitat/tree/master/test/Core8-Wildlife3.0>

  tmp_pth <- withr::local_tempdir("test_OutputWildlifeHabitat_")

  ext <- OutputWildlifeHabitat$new(
    path = tmp_pth,
    Timestep = 1L,
    OutputTimestep = 10L,
    MapFileNames = "output/habitat/{wildlifeName}-{timestep}.tif",
    SuitabilityFiles = c(
      "BBWOAgeForestType.txt",
      "BBWOAgeTSH.txt",
      "BTBW.txt",
      "KIWA.txt",
      "STGR.txt"
    )
  )
  ext$write()

  testthat::expect_true(all(file.exists(file.path(tmp_pth, ext$files))))

  contents <- readLines(file.path(tmp_pth, ext$files[1]))
  testthat::expect_true(any(grepl("^LandisData\\s+\"Wildlife Habitat Output\"", contents)))
  testthat::expect_true(any(grepl("^Timestep\\s+1", contents)))
  testthat::expect_true(any(grepl("^OutputTimestep\\s+10", contents)))
  testthat::expect_true(any(grepl("^MapFileNames\\s+output/habitat/\\{wildlifeName\\}", contents)))
  testthat::expect_true(any(grepl("^SuitabilityFiles\\s+BBWOAgeForestType\\.txt", contents)))
  testthat::expect_true(any(grepl("STGR\\.txt", contents)))

  withr::deferred_run()
})

testthat::test_that("OutputWildlifeHabitat rejects MapFileNames missing wildlifeName", {
  tmp_pth <- withr::local_tempdir("test_OutputWildlifeHabitat_")

  testthat::expect_error(OutputWildlifeHabitat$new(
    path = tmp_pth,
    Timestep = 1L,
    MapFileNames = "output/habitat/file-{timestep}.tif",
    SuitabilityFiles = list(suitabilityFile("a.txt"))
  ))

  withr::deferred_run()
})
