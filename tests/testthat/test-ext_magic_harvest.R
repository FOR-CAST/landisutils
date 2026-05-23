testthat::test_that("Magic Harvest extension is properly created", {
  ## NOTE: reference values from the LANDIS-II Magic Harvest v2 example input:
  ## Examples/Core-v8/Biomass Harvest/magic_harvest.txt
  ## <https://github.com/Klemet/LANDIS-II-Magic-Harvest/tree/main/Examples/Core-v8/Biomass%20Harvest>
  ## (this extension is by Klemet; no Foundation `testings/Core8-*` directory exists.)

  tmp_pth <- withr::local_tempdir("test_MagicHarvest_")

  ext <- MagicHarvest$new(
    path = tmp_pth,
    Timestep = 10L,
    HarvestExtensionParameterFile = "biomass-harvest_SetUp_s2e1.txt",
    ProcessToLaunch = "python",
    ProcessArguments = "./switchManagementRasterNames.py {timestep}",
    NoHarvestReInitialization = FALSE
  )
  ext$write()

  testthat::expect_true(all(file.exists(file.path(tmp_pth, ext$files))))

  contents <- readLines(file.path(tmp_pth, ext$files[1]))
  testthat::expect_true(any(grepl("^LandisData\\s+\"Magic Harvest\"", contents)))
  testthat::expect_true(any(grepl("^Timestep\\s+10", contents)))
  testthat::expect_true(any(grepl(
    "^HarvestExtensionParameterFile\\s+biomass-harvest_SetUp_s2e1\\.txt$",
    contents
  )))
  testthat::expect_true(any(grepl("^ProcessToLaunch\\s+python$", contents)))
  ## ProcessArguments contains whitespace, so it must remain quoted in the
  ## generated file (LANDIS-II requires quotes only for values with spaces).
  testthat::expect_true(any(grepl(
    "^ProcessArguments\\s+\".*switchManagementRasterNames\\.py \\{timestep\\}\"",
    contents
  )))
  testthat::expect_true(any(grepl("^NoHarvestReInitialization\\s+false$", contents)))

  withr::deferred_run()
})

testthat::test_that("MagicHarvest accepts logical or string for NoHarvestReInitialization", {
  tmp_pth <- withr::local_tempdir("test_MagicHarvest_")

  ext1 <- MagicHarvest$new(
    path = tmp_pth,
    Timestep = 5L,
    HarvestExtensionParameterFile = "h.txt",
    ProcessToLaunch = "python",
    ProcessArguments = "{none}",
    NoHarvestReInitialization = TRUE
  )
  testthat::expect_identical(ext1$NoHarvestReInitialization, "true")

  ext2 <- MagicHarvest$new(
    path = tmp_pth,
    Timestep = 5L,
    HarvestExtensionParameterFile = "h.txt",
    ProcessToLaunch = "python",
    ProcessArguments = "{none}",
    NoHarvestReInitialization = "yes"
  )
  testthat::expect_identical(ext2$NoHarvestReInitialization, "true")

  withr::deferred_run()
})

testthat::test_that("MagicHarvest write() requires required fields", {
  tmp_pth <- withr::local_tempdir("test_MagicHarvest_")

  ext <- MagicHarvest$new(path = tmp_pth, Timestep = 10L)
  testthat::expect_error(ext$write())

  withr::deferred_run()
})
