testthat::test_that("Output Biomass Community extension is properly created", {
  ## NOTE: reference values from the LANDIS-II Core8 / Output Biomass Community v3 test input:
  ## testing/Core8-BioCommunity3.0/output_Biomass_Community.txt
  ## <https://github.com/LANDIS-II-Foundation/Extension-Output-Biomass-Community/tree/master/testing/Core8-BioCommunity3.0>

  tmp_pth <- withr::local_tempdir("test_OutputBiomassCommunity_")

  ext <- OutputBiomassCommunity$new(path = tmp_pth, Timestep = 10L)
  ext$write()

  testthat::expect_true(all(file.exists(file.path(tmp_pth, ext$files))))

  contents <- readLines(file.path(tmp_pth, ext$files[1]))
  testthat::expect_true(any(grepl("^LandisData\\s+\"Output Biomass Community\"", contents)))
  testthat::expect_true(any(grepl("^Timestep\\s+10", contents)))

  withr::deferred_run()
})

testthat::test_that("OutputBiomassCommunity rejects invalid Timestep", {
  tmp_pth <- withr::local_tempdir("test_OutputBiomassCommunity_")

  testthat::expect_error(OutputBiomassCommunity$new(path = tmp_pth, Timestep = -1L))
  testthat::expect_error(OutputBiomassCommunity$new(path = tmp_pth, Timestep = 0L))

  withr::deferred_run()
})
