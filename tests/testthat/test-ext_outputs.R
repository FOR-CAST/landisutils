testthat::test_that("Output extensions are properly created", {
  testthat::skip_if_not_installed("withr")

  ## prepare landis input files ----------------------------------------------------------------

  tmp_pth <- withr::local_tempdir("test_output_exts_")

  output_biomass <- OutputBiomass$new(path = tmp_pth, Timestep = 10, Species = "all")

  output_biomass$write()

  testthat::expect_true(file.exists(file.path(tmp_pth, output_biomass$files)))

  ## TODO: test OutputBiomassByAge
  # biomass_by_age <- OutputBiomassByAge(
  #   path = tmp_pth,
  #   Timestep = 10,
  #   MapNames = NULL, # use default
  #   Species = c(
  #     "pinubank ageclass1(10-40) ageclass2(15-100)",
  #     "pinuresi ageclass(>200)",
  #     "pinustro ageclass(>250)",
  #     "poputrem ageclass1(<50)"
  #   )
  # )
  #
  # biomass_by_age$write()
  #
  # testthat::expect_true(file.exists(file.path(tmp_pth, biomass_by_age$files)))

  ## TODO: test OutputCohortStats
  # cohort_statistics <- OutputCohortStats$new(
  #   path = tmp_pth,
  #   Timestep = 10,
  #   SpeciesAgeStats = c(
  #     species = c(),
  #     stats = c()
  #   ),
  #   SiteAgeStats = c(
  #     stats = c()
  #   ),
  #   SiteSpeciesStats = c(
  #     stats = c()
  #   )
  # )
  #
  # cohort_satistics$write()
  #
  # testthat::expect_true(file.exists(file.path(tmp_pth, cohort_statistics$files)))

  max_species_age <- OutputMaxSpeciesAge$new(path = tmp_pth, Timestep = 10, Species = "all")

  max_species_age$write()

  testthat::expect_true(file.exists(file.path(tmp_pth, max_species_age$files)))

  withr::deferred_run()
})
