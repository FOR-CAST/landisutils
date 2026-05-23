testthat::test_that("PnET Biomass Output extension is properly created", {
  ## NOTE: reference values from the LANDIS-II PnET Output v4 example input:
  ## deploy/examples/OneCellSimulation/biomass.output.txt
  ## <https://github.com/LANDIS-II-Foundation/Extension-Output-Biomass-PnET/tree/master/deploy/examples/OneCellSimulation>
  ## (no `testings/Core8-*` directory exists for this extension upstream).

  tmp_pth <- withr::local_tempdir("test_OutputBiomassPnET_")

  ext <- OutputBiomassPnET$new(
    path = tmp_pth,
    Timestep = 1L,
    Species = "all",
    Outputs = list(
      Biomass = "output/biomass/{species}/biomass-{timestep}.img",
      LeafAreaIndex = "output/lai-{timestep}.img",
      Water = "output/water-{timestep}.img"
    )
  )
  ext$write()

  testthat::expect_true(all(file.exists(file.path(tmp_pth, ext$files))))

  contents <- readLines(file.path(tmp_pth, ext$files[1]))
  testthat::expect_true(any(grepl("^LandisData\\s+\"Output-PnET\"", contents)))
  testthat::expect_true(any(grepl("^Timestep\\s+1", contents)))
  testthat::expect_true(any(grepl("^Species\\s+ALL", contents)))
  testthat::expect_true(any(grepl("^Biomass\\s+output/biomass/", contents)))
  testthat::expect_true(any(grepl("^LeafAreaIndex\\s+output/lai-", contents)))
  testthat::expect_true(any(grepl("^Water\\s+output/water-", contents)))

  withr::deferred_run()
})

testthat::test_that("OutputBiomassPnET defaults populate the standard set", {
  defaults <- defaultPnETOutputFiles()
  testthat::expect_true(is.list(defaults))
  testthat::expect_true(all(names(defaults) %in% landisutils:::.pnetOutputKeywords))
  testthat::expect_true(all(c("Biomass", "LeafAreaIndex", "Water") %in% names(defaults)))
})

testthat::test_that("OutputBiomassPnET rejects unrecognized output keywords", {
  tmp_pth <- withr::local_tempdir("test_OutputBiomassPnET_")

  testthat::expect_error(OutputBiomassPnET$new(
    path = tmp_pth,
    Timestep = 1L,
    Outputs = list(NotARealField = "foo/{timestep}.img")
  ))

  withr::deferred_run()
})
