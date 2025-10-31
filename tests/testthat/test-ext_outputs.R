testthat::test_that("Output Biomass extension is properly created", {
  testthat::skip_if_not_installed("withr")

  tmp_pth <- withr::local_tempdir("test_output_exts_")

  output_biomass <- OutputBiomass$new(path = tmp_pth, Timestep = 10, Species = "all")

  output_biomass$write()

  testthat::expect_true(file.exists(file.path(tmp_pth, output_biomass$files)))

  withr::deferred_run()
})

testthat::test_that("Output Max Species Age extension is properly created", {
  testthat::skip_if_not_installed("withr")

  tmp_pth <- withr::local_tempdir("test_output_exts_")

  max_species_age <- OutputMaxSpeciesAge$new(path = tmp_pth, Timestep = 10, Species = "all")

  max_species_age$write()

  testthat::expect_true(file.exists(file.path(tmp_pth, max_species_age$files)))

  withr::deferred_run()
})

testthat::test_that("Output Biomass By Age extension is properly created", {
  testthat::skip_if_not_installed("withr")

  tmp_pth <- withr::local_tempdir("test_output_exts_")

  biomass_by_age <- OutputBiomassByAge$new(
    path = tmp_pth,
    Timestep = 10,
    MapNames = NULL, # use default
    Species = c(
      "pinubank ageclass1(10-40) ageclass2(15-100)",
      "pinuresi ageclass(>200)",
      "pinustro ageclass(>250)",
      "poputrem ageclass1(<50)"
    )
  )

  biomass_by_age$write()

  testthat::expect_true(file.exists(file.path(tmp_pth, biomass_by_age$files)))

  withr::deferred_run()
})

testthat::test_that("Output Cohort Statistics extension is properly created", {
  testthat::skip_if_not_installed("withr")

  tmp_pth <- withr::local_tempdir("test_output_exts_")

  cohort_statistics <- OutputCohortStats$new(
    path = tmp_pth,
    Timestep = 10,
    SpeciesAgeStats = list(species = c("querrub", "pinustro"), stats = c("MAX", "MIN")),
    SiteAgeStats = list(stats = c("MAX", "MED", "SD", "RICH", "EVEN")),
    SiteSpeciesStats = list(stats = c("RICH"))
  )

  cohort_statistics$write()

  testthat::expect_true(file.exists(file.path(tmp_pth, cohort_statistics$files)))

  withr::deferred_run()
})
