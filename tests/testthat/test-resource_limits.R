## Resource-cap helpers added in v0.0.23:
##   * .parse_mem_limit() accepts numeric bytes, NULL/Inf, or strings like
##     "8g" / "512m" / "1024k".
##   * .resolve_mem_limit() resolves the effective cap given a baseline + an
##     optional prior resource log. First-time runs (no prior log) get no cap;
##     subsequent runs honour the baseline but raise it to peak * margin when
##     the prior peak would exceed the baseline.
##
## Both functions are internal; tests reach them via `:::` to lock in the
## semantics that landis_run_docker() depends on.

## ---- .parse_mem_limit --------------------------------------------------------------------------

testthat::test_that(".parse_mem_limit() accepts numeric bytes and NULL/Inf", {
  testthat::expect_equal(landisutils:::.parse_mem_limit(1024^3), 1024^3)
  testthat::expect_equal(landisutils:::.parse_mem_limit(NULL), Inf)
  testthat::expect_equal(landisutils:::.parse_mem_limit(Inf), Inf)
  testthat::expect_equal(landisutils:::.parse_mem_limit(NA_real_), Inf)
})

testthat::test_that(".parse_mem_limit() parses k/m/g/t suffixes, case-insensitive", {
  testthat::expect_equal(landisutils:::.parse_mem_limit("8g"), 8 * 1024^3)
  testthat::expect_equal(landisutils:::.parse_mem_limit("8G"), 8 * 1024^3)
  testthat::expect_equal(landisutils:::.parse_mem_limit("8GB"), 8 * 1024^3)
  testthat::expect_equal(landisutils:::.parse_mem_limit("512m"), 512 * 1024^2)
  testthat::expect_equal(landisutils:::.parse_mem_limit("1024k"), 1024^2)
  testthat::expect_equal(landisutils:::.parse_mem_limit("2t"), 2 * 1024^4)
  testthat::expect_equal(landisutils:::.parse_mem_limit("4096"), 4096)
})

testthat::test_that(".parse_mem_limit() errors on garbage", {
  testthat::expect_error(landisutils:::.parse_mem_limit("8 gigs"), "Could not parse")
  testthat::expect_error(landisutils:::.parse_mem_limit("eight"), "Could not parse")
})

## ---- .resolve_mem_limit ------------------------------------------------------------------------

write_prior_log <- function(rep_dir, peak_mem_bytes, source = "docker") {
  log_dir <- file.path(rep_dir, "log")
  dir.create(log_dir, recursive = TRUE, showWarnings = FALSE)
  writeLines(
    c("elapsed_sec: 1000.0", sprintf("peak_mem_bytes: %.0f", peak_mem_bytes)),
    file.path(log_dir, paste0(source, "_resources.log"))
  )
  rep_dir
}

testthat::test_that(".resolve_mem_limit() returns Inf when no prior log exists (first run)", {
  td <- withr::local_tempdir()
  res <- landisutils:::.resolve_mem_limit(td, mem_limit = "8g", mem_margin = 1.5)
  testthat::expect_true(is.infinite(res))
})

testthat::test_that(".resolve_mem_limit() honours baseline when prior peak fits within it", {
  td <- withr::local_tempdir()
  write_prior_log(td, peak_mem_bytes = 4 * 1024^3) ## 4 GiB peak
  res <- landisutils:::.resolve_mem_limit(td, mem_limit = "8g", mem_margin = 1.5)
  ## 4 GiB * 1.5 = 6 GiB; baseline 8 GiB > 6 GiB so baseline wins.
  testthat::expect_equal(res, 8 * 1024^3)
})

testthat::test_that(".resolve_mem_limit() raises cap when prior peak * margin exceeds baseline", {
  td <- withr::local_tempdir()
  write_prior_log(td, peak_mem_bytes = 10 * 1024^3) ## 10 GiB peak
  res <- landisutils:::.resolve_mem_limit(td, mem_limit = "8g", mem_margin = 1.5)
  ## 10 GiB * 1.5 = 15 GiB > baseline 8 GiB so the higher value wins.
  testthat::expect_equal(res, 15 * 1024^3)
})

testthat::test_that(".resolve_mem_limit() honours numeric baseline (bytes)", {
  td <- withr::local_tempdir()
  write_prior_log(td, peak_mem_bytes = 1 * 1024^3)
  res <- landisutils:::.resolve_mem_limit(td, mem_limit = 4 * 1024^3, mem_margin = 2)
  ## 1 GiB * 2 = 2 GiB < 4 GiB so baseline wins.
  testthat::expect_equal(res, 4 * 1024^3)
})

testthat::test_that(".resolve_mem_limit() reads local_resources.log too (non-docker path)", {
  td <- withr::local_tempdir()
  write_prior_log(td, peak_mem_bytes = 12 * 1024^3, source = "local")
  res <- landisutils:::.resolve_mem_limit(td, mem_limit = "8g", mem_margin = 1.5)
  testthat::expect_equal(res, 18 * 1024^3)
})

testthat::test_that(".resolve_mem_limit() falls back to baseline if prior log lacks peak_mem_bytes", {
  td <- withr::local_tempdir()
  log_dir <- file.path(td, "log")
  dir.create(log_dir, recursive = TRUE, showWarnings = FALSE)
  ## elapsed only -- no peak_mem_bytes
  writeLines("elapsed_sec: 60.0", file.path(log_dir, "docker_resources.log"))
  res <- landisutils:::.resolve_mem_limit(td, mem_limit = "8g", mem_margin = 1.5)
  testthat::expect_equal(res, 8 * 1024^3)
})

testthat::test_that(".resolve_mem_limit() with mem_limit = Inf stays Inf", {
  td <- withr::local_tempdir()
  write_prior_log(td, peak_mem_bytes = 4 * 1024^3)
  res <- landisutils:::.resolve_mem_limit(td, mem_limit = Inf, mem_margin = 1.5)
  ## max(Inf, 6 GiB) == Inf
  testthat::expect_true(is.infinite(res))
})

## ---- .resolve_startup_jitter (v0.0.43) ---------------------------------------------------------

testthat::test_that(".resolve_startup_jitter() honours an explicit numeric", {
  testthat::expect_equal(landisutils:::.resolve_startup_jitter(30), 30)
  testthat::expect_equal(landisutils:::.resolve_startup_jitter(0), 0)
  testthat::expect_equal(landisutils:::.resolve_startup_jitter(2.5), 2.5)
})

testthat::test_that(".resolve_startup_jitter() clamps invalid input to 0", {
  testthat::expect_equal(landisutils:::.resolve_startup_jitter(-5), 0)
  testthat::expect_equal(landisutils:::.resolve_startup_jitter(NA_real_), 0)
})

testthat::test_that(".resolve_startup_jitter() reads LANDIS_STARTUP_JITTER when NULL", {
  withr::local_envvar(LANDIS_STARTUP_JITTER = "45")
  testthat::expect_equal(landisutils:::.resolve_startup_jitter(NULL), 45)
})

testthat::test_that(".resolve_startup_jitter() is 0 when env unset or non-numeric", {
  withr::local_envvar(LANDIS_STARTUP_JITTER = "")
  testthat::expect_equal(landisutils:::.resolve_startup_jitter(NULL), 0)
  withr::local_envvar(LANDIS_STARTUP_JITTER = "abc")
  testthat::expect_equal(landisutils:::.resolve_startup_jitter(NULL), 0)
})

testthat::test_that(".resolve_startup_jitter() explicit arg overrides the env var", {
  withr::local_envvar(LANDIS_STARTUP_JITTER = "60")
  testthat::expect_equal(landisutils:::.resolve_startup_jitter(10), 10)
})
