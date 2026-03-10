testthat::test_that("Landis scenarios are properly created", {
  tmp_pth <- withr::local_tempdir("test_LandisScenario_")

  ## prepare landis scenario file --------------------------------------------------------------

  skip("incomplete") ## TODO

  er_files <- prepEcoregionsFiles(
    ecoregion = ecoregion,
    ecoregionMap = ecoregionMap,
    path = tmp_pth
  )

  testthat::expect_true(all(file.exists(er_files)))

  core_spp_file <- prepSpeciesData(species, tmp_pth, type = "core")

  testthat::expect_true(file.exists(core_spp_file))

  scenario_name <- glue::glue("scenario_{studyAreaName}")
  scenario <- scenario(
    name = scenario_name,
    extensions = list(ext_biomass_succession),
    path = tmp_pth,
    climate_config = cc,

    ## additional arguments
    CellLength = terra::res(ecoregionMap)[1],
    DisturbancesRandomOrder = FALSE,
    Duration = 20,
    EcoregionsFiles = er_files,
    RandomNumberSeed = NULL, ## optional
    SpeciesInputFile = core_spp_file
  )

  testthat::expect_true(all(file.exists(file.path(scenario$path, scenario$files))))

  scenario$replicate(n = 3)

  testthat::expect_true(dir.exists(scenario$path))
  testthat::expect_true(dir.exists(paste0(scenario$path, "_rep01")))
  testthat::expect_true(dir.exists(paste0(scenario$path, "_rep02")))
  testthat::expect_true(dir.exists(paste0(scenario$path, "_rep03")))

  scenario$replicate(n = 1)

  testthat::expect_true(dir.exists(paste0(scenario$path, "_rep04")))
  testthat::expect_true(all(file.exists(file.path(
    paste0(scenario$path, "_rep04"),
    scenario$files
  ))))

  ## run the landis scenario -------------------------------------------------------------------
  testthat::skip_if_not(nzchar(landis_find()))
  ## TODO
  landis_run(scenario_file)

  withr::deferred_run()
})
