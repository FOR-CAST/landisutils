## Version detection is parsed from the banner the console prints on its first line,
## verified against ghcr.io/landis-ii-foundation/landis-ii-v8-uclv2-release:ubuntu-24.04:
##
##   $ dotnet $LANDIS_CONSOLE
##   LANDIS-II 8.0 (8)
##
##   Error: No scenario file specified.

test_that(".parse_landis_version() reads the console banner", {
  got <- .parse_landis_version("LANDIS-II 8.0 (8)")
  expect_identical(got$version, "8.0")
  expect_identical(got$major, 8L)

  ## The same banner as it appears in Landis-log.txt, behind a log4net timestamp.
  logged <- .parse_landis_version("2026-07-10 05:29:06,989 - LANDIS-II 8.0 (8)")
  expect_identical(logged$major, 8L)

  ## Found anywhere in multi-line output, not just on line 1.
  multi <- .parse_landis_version(c("", "LANDIS-II 7.0 (7)", "Error: No scenario file specified."))
  expect_identical(multi$major, 7L)
})

test_that(".parse_landis_version() returns NULL rather than guessing", {
  expect_null(.parse_landis_version(character(0)))
  expect_null(.parse_landis_version(""))
  expect_null(.parse_landis_version("bash: dotnet: command not found"))
  ## A banner with no parseable number must not yield a bogus major.
  expect_null(.parse_landis_version("LANDIS-II vNext"))
})

test_that("landis_assert_version() accepts v8 and rejects anything else", {
  local_mocked_bindings(.probe_landis_version = function(...) list(version = "8.0", major = 8L))
  expect_identical(landis_assert_version(image = "fake-v8-image"), "8.0")
})

test_that("landis_assert_version() rejects a non-v8 core with the version in the message", {
  local_mocked_bindings(.probe_landis_version = function(...) list(version = "7.0", major = 7L))
  expect_error(landis_assert_version(image = "fake-v7-image"), "v8 is required")
  expect_error(landis_assert_version(image = "fake-v7-image"), "7\\.0")
})

test_that("landis_assert_version() BLOCKS when the version cannot be determined", {
  ## The point of the guard: an unreadable probe is not evidence of a v8 core.
  local_mocked_bindings(.probe_landis_version = function(...) NULL)
  expect_error(landis_assert_version(image = "fake-silent-image"), "could not determine")
})

test_that("landis_assert_version() honours the explicit opt-out", {
  local_mocked_bindings(.probe_landis_version = function(...) NULL)
  withr::local_options(landisutils.skip_version_check = TRUE)
  expect_silent(landis_assert_version(image = "fake-silent-image"))
})

test_that("landis_assert_version() memoises per key, so a pool probes once", {
  calls <- 0L
  local_mocked_bindings(.probe_landis_version = function(...) {
    calls <<- calls + 1L
    list(version = "8.0", major = 8L)
  })
  key <- "memo-test-image"
  landis_assert_version(image = key)
  landis_assert_version(image = key)
  landis_assert_version(image = key)
  expect_identical(calls, 1L)

  ## A different target is probed on its own.
  landis_assert_version(image = "memo-test-other")
  expect_identical(calls, 2L)
})

test_that("landis_assert_version() requires something to probe", {
  expect_error(landis_assert_version(), "supply exactly one of")
})

test_that("the target version is a single switch, not a hardcoded 8", {
  ## The point of the option: when v9 supersedes v8, flipping one value has to move
  ## the guard, not leave a v8 assertion behind.
  expect_identical(landis_target_version(), .landis_version_default)

  withr::local_options(landisutils.landis.version = 9L)
  expect_identical(landis_target_version(), 9L)

  local_mocked_bindings(.probe_landis_version = function(...) list(version = "8.0", major = 8L))
  ## A v8 console must now FAIL, because the package is being told it targets v9.
  expect_error(landis_assert_version(image = "switch-test"), "v9 is required")
  expect_error(landis_assert_version(image = "switch-test"), "reports version 8\\.0")
})

test_that("the memo cache is keyed by required version, not just by target", {
  ## A cached PASS for one generation must not answer for another.
  local_mocked_bindings(.probe_landis_version = function(...) list(version = "8.0", major = 8L))
  img <- "cache-key-test"
  expect_identical(landis_assert_version(8L, image = img), "8.0")
  expect_error(landis_assert_version(9L, image = img), "v9 is required")
})

test_that("the docker image and console defaults derive from the same switch", {
  ## Flipping the constant must not leave a stale v8 in the image tag or console path.
  v <- .landis_version_default
  expect_match(getOption("landisutils.docker.image"), sprintf("landis-ii-v%d-", v), fixed = FALSE)
  expect_match(getOption("landisutils.docker.console"), sprintf("Core-Model-v%d-", v))
})

test_that("landis_assert_version() detects v8 in the real image", {
  skip_if_not(.docker_available(), "docker CLI not available")
  image <- "ghcr.io/landis-ii-foundation/landis-ii-v8-uclv2-release:ubuntu-24.04"
  have <- processx::run(
    "docker",
    c("image", "inspect", image),
    error_on_status = FALSE,
    echo = FALSE
  )
  skip_if(have$status != 0L, "v8 image not present locally")
  expect_identical(landis_assert_version(image = image), "8.0")
})
