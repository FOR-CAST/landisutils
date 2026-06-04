testthat::test_that(".biosim_generate_weather retries on failure then returns the result", {
  testthat::skip_if_not_installed("BioSIM")

  withr::local_options(
    landisutils.biosim.request_delay = c(0, 0), ## no stagger in tests
    landisutils.biosim.backoff_base = 0.001, ## near-instant backoff
    landisutils.biosim.max_attempts = 3L,
    landisutils.biosim.timeout = Inf
  )

  calls <- 0L
  testthat::local_mocked_bindings(
    generateWeather = function(...) {
      calls <<- calls + 1L
      if (calls < 3L) {
        stop("transient BioSIM failure")
      }
      list(ok = TRUE)
    },
    .package = "BioSIM"
  )

  res <- suppressMessages(.biosim_generate_weather(modelNames = "X", fromYr = 2000, toYr = 2000))
  testthat::expect_equal(res, list(ok = TRUE))
  testthat::expect_equal(calls, 3L)
})

testthat::test_that(".biosim_generate_weather errors after exhausting max_attempts", {
  testthat::skip_if_not_installed("BioSIM")

  withr::local_options(
    landisutils.biosim.request_delay = c(0, 0),
    landisutils.biosim.backoff_base = 0.001,
    landisutils.biosim.max_attempts = 2L,
    landisutils.biosim.timeout = Inf
  )

  testthat::local_mocked_bindings(
    generateWeather = function(...) stop("BioSIM web API unavailable"),
    .package = "BioSIM"
  )

  testthat::expect_snapshot(
    suppressMessages(.biosim_generate_weather(modelNames = "X", fromYr = 2000, toYr = 2000)),
    error = TRUE
  )
})
