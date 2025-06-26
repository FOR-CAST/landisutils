testthat::test_that("Original Fire (and Biomass Succession) inputs are properly created", {
  testthat::skip_if_not_installed("withr")

  ## prepare landis input files ----------------------------------------------------------------

  tmp_pth <- withr::local_tempdir("test_output_exts_")

  out_b <- OutputBiomassInput(
    path = tmp_pth,
    Timestep = 10,
    Species = "all"
  )

  testthat::expect_true(file.exists(file.path(tmp_pth, out_b$files)))

  # out_bage <- OutputBiomassAgeInput(
  #   path = tmp_pth,
  #   Species = c(
  #     ## TODO
  #   )
  # )
  #
  # testthat::expect_true(file.exists(file.path(tmp_pth, out_bage$files)))

  # out_cohortstats <- OutputCohortStatsInput(
  #   path = tmp_pth,
  #   Timestep = 10
  # )
  #
  # testthat::expect_true(file.exists(file.path(tmp_pth, out_cohortstats$files)))

  out_maxsppage <- OutputMaxSppAgeInput(
    path = tmp_pth,
    Timestep = 10,
    Species = "all"
  )

  testthat::expect_true(file.exists(file.path(tmp_pth, out_maxsppage$files)))

  withr::deferred_run()
})
