testthat::test_that("Original Fire (and Biomass Succession) inputs are properly created", {
  testthat::skip_if_not_installed("withr")

  ## prepare landis input files ----------------------------------------------------------------

  tmp_pth <- withr::local_tempdir("test_output_exts_")

  out_b_file <- OutputBiomassInput(
    path = tmp_pth,
    Timestep = 10,
    Species = "all"
  )

  testthat::expect_true(file.exists(out_b_file))

  # out_bage_file <- OutputBiomassAgeInput(
  #   path = tmp_pth,
  #   Species = c(
  #     ## TODO
  #   )
  # )
  #
  # testthat::expect_true(file.exists(out_bage_file))

  # out_cohortstats_file <- OutputCohortStatsInput(
  #   path = tmp_pth,
  #   Timestep = 10
  # )
  #
  # testthat::expect_true(file.exists(out_cohortstats_file))

  out_maxsppage_file <- OutputMaxSppAgeInput(
    path = tmp_pth,
    Timestep = 10,
    Species = "all"
  )

  testthat::expect_true(file.exists(out_maxsppage_file))

  withr::deferred_run()
})
