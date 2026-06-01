## host_cpu_info() is OS-agnostic. CI runs this file on Linux, macOS, and
## Windows, so any platform-specific branch that quietly fails to populate
## a field will surface here. Each test is structural (shape and types) +
## "value present and not implausible" rather than asserting an exact CPU model.

testthat::test_that("host_cpu_info() returns a list with the documented shape", {
  info <- landisutils::host_cpu_info()

  testthat::expect_named(info, c("model", "n_logical", "ram_bytes"), ignore.order = TRUE)
  testthat::expect_true(is.character(info$model) && length(info$model) == 1L)
  testthat::expect_true(is.integer(info$n_logical) && length(info$n_logical) == 1L)
  testthat::expect_true(is.numeric(info$ram_bytes) && length(info$ram_bytes) == 1L)
})

testthat::test_that("host_cpu_info() populates n_logical with a sane value on every supported OS", {
  ## parallel::detectCores() always returns >=1 on machines that can run R.
  info <- landisutils::host_cpu_info()
  testthat::expect_false(is.na(info$n_logical))
  testthat::expect_gte(info$n_logical, 1L)
  testthat::expect_lte(info$n_logical, 4096L) ## sanity cap
})

testthat::test_that("host_cpu_info() populates model + ram_bytes on Linux", {
  testthat::skip_if_not(identical(Sys.info()[["sysname"]], "Linux"))
  info <- landisutils::host_cpu_info()
  testthat::expect_false(is.na(info$model))
  testthat::expect_true(nzchar(info$model))
  testthat::expect_false(is.na(info$ram_bytes))
  testthat::expect_gt(info$ram_bytes, 0)
  ## sanity: more than 256 MiB and less than 64 TiB
  testthat::expect_gt(info$ram_bytes, 256 * 1024^2)
  testthat::expect_lt(info$ram_bytes, 64 * 1024^4)
})

testthat::test_that("host_cpu_info() populates model + ram_bytes on macOS", {
  testthat::skip_if_not(identical(Sys.info()[["sysname"]], "Darwin"))
  info <- landisutils::host_cpu_info()
  testthat::expect_false(is.na(info$model))
  testthat::expect_true(nzchar(info$model))
  testthat::expect_false(is.na(info$ram_bytes))
  testthat::expect_gt(info$ram_bytes, 256 * 1024^2)
})

testthat::test_that("host_cpu_info() populates model on Windows", {
  testthat::skip_if_not(identical(Sys.info()[["sysname"]], "Windows"))
  info <- landisutils::host_cpu_info()
  ## PROCESSOR_IDENTIFIER is always set on Windows CI runners.
  testthat::expect_false(is.na(info$model))
  testthat::expect_true(nzchar(info$model))
  ## RAM via wmic; on newer Windows runners wmic may be missing -- allow NA.
  testthat::expect_true(is.na(info$ram_bytes) || info$ram_bytes > 256 * 1024^2)
})
