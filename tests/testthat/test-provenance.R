## parse_landis_log_versions() parses the header of a LANDIS-II Landis-log.txt.
## The header format is identical across runs (the console writes the same
## banner every time): a console-version line, a user-supplied seed line, and
## "Succession:" / "Disturbance Extensions:" / "Other Extensions:" sections
## whose body lines have the form "<Name> Version: X.Y, Input: foo.txt".
##
## Tests write a tiny hand-built log to a tempfile so they are hermetic.

## A minimal but representative Landis-log.txt header.
fake_log_lines <- c(
  "2026-06-17 12:00:00,000 - LANDIS-II 8.0 (8)",
  "2026-06-17 12:00:00,001 - ",
  "2026-06-17 12:00:00,002 - Loading scenario from file scenario.txt",
  "RandomNumberSeed: using user-supplied seed = 12,345",
  "",
  "Succession:",
  "   Biomass Succession Version: 7.0, Input: biomass-succession.txt",
  "",
  "Disturbance Extensions:",
  "   Dynamic Fire System Version: 4.0, Input: dynamic-fire.txt",
  "   Dynamic Fuel System Version: 3.0, Input: dynamic-fuel.txt",
  "",
  "Other Extensions:",
  "   Output Biomass-by-Age Version: 3.0, Input: output-biomass-age.txt",
  ""
)

testthat::test_that("parse_landis_log_versions() returns empty defaults for a missing file", {
  res <- landisutils::parse_landis_log_versions(file.path(tempdir(), "no-such-log-xyz.txt"))
  testthat::expect_identical(res$console, NA_character_)
  testthat::expect_identical(res$seed, NA_real_)
  testthat::expect_identical(res$succession, character(0))
  testthat::expect_identical(res$disturbance, character(0))
  testthat::expect_identical(res$other, character(0))
})

testthat::test_that("parse_landis_log_versions() extracts console, seed, and extension blocks", {
  log <- withr::local_tempfile(fileext = ".txt")
  writeLines(fake_log_lines, log)
  res <- landisutils::parse_landis_log_versions(log)

  testthat::expect_match(res$console, "^LANDIS-II 8\\.0 \\(8\\)$")
  testthat::expect_equal(res$seed, 12345)
  testthat::expect_equal(trimws(res$succession), "Biomass Succession v7.0")
  testthat::expect_equal(
    trimws(res$disturbance),
    c("Dynamic Fire System v4.0", "Dynamic Fuel System v3.0")
  )
  testthat::expect_equal(trimws(res$other), "Output Biomass-by-Age v3.0")
})

testthat::test_that("parse_landis_log_versions() handles a header with no extension sections", {
  log <- withr::local_tempfile(fileext = ".txt")
  writeLines(c("2026-06-17 12:00:00,000 - LANDIS-II 8.0 (8)", ""), log)
  res <- landisutils::parse_landis_log_versions(log)
  testthat::expect_match(res$console, "LANDIS-II")
  testthat::expect_identical(res$succession, character(0))
  testthat::expect_identical(res$disturbance, character(0))
  testthat::expect_identical(res$other, character(0))
})
