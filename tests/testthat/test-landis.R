testthat::test_that("landis_replicate creates rep subdirs inside scenario_dir", {
  tmp <- withr::local_tempdir("test_landis_replicate_")
  file.create(file.path(tmp, c("scenario.txt", "species.txt")))

  rep_dirs <- landis_replicate(tmp, 2L, files = file.path(tmp, c("scenario.txt", "species.txt")))

  testthat::expect_length(rep_dirs, 2L)
  testthat::expect_match(rep_dirs[1], "rep01$")
  testthat::expect_match(rep_dirs[2], "rep02$")
  testthat::expect_true(all(fs::dir_exists(rep_dirs)))
  testthat::expect_true(fs::file_exists(fs::path(rep_dirs[1], "scenario.txt")))
  testthat::expect_true(fs::file_exists(fs::path(rep_dirs[2], "species.txt")))
})

testthat::test_that("landis_replicate is idempotent", {
  tmp <- withr::local_tempdir("test_landis_replicate_idem_")
  file.create(file.path(tmp, "scenario.txt"))

  landis_replicate(tmp, 2L)
  writeLines("sentinel", file.path(tmp, "rep01", "sentinel.txt"))

  landis_replicate(tmp, 3L)

  testthat::expect_true(fs::file_exists(fs::path(tmp, "rep01", "sentinel.txt")))
  testthat::expect_true(fs::dir_exists(fs::path(tmp, "rep03")))
})

testthat::test_that("landis_replicate sets per-rep seeds from base_seed", {
  tmp <- withr::local_tempdir("test_landis_seeds_")
  writeLines(
    c("LandisData  \"Scenario\"", ">> RandomNumberSeed    4357  << optional parameter", ""),
    file.path(tmp, "scenario.txt")
  )

  landis_replicate(tmp, 3L, files = file.path(tmp, "scenario.txt"), base_seed = 100L)

  seed_line <- function(rep) {
    grep("RandomNumberSeed", readLines(file.path(tmp, rep, "scenario.txt")), value = TRUE)
  }

  testthat::expect_match(seed_line("rep01"), "RandomNumberSeed\\s+100\\b")
  testthat::expect_match(seed_line("rep02"), "RandomNumberSeed\\s+101\\b")
  testthat::expect_match(seed_line("rep03"), "RandomNumberSeed\\s+102\\b")
})

testthat::test_that("landis_replicate seeds are stable when more reps are added", {
  tmp <- withr::local_tempdir("test_landis_seeds_stable_")
  writeLines(
    c("LandisData  \"Scenario\"", ">> RandomNumberSeed    4357  << optional parameter", ""),
    file.path(tmp, "scenario.txt")
  )

  landis_replicate(tmp, 2L, files = file.path(tmp, "scenario.txt"), base_seed = 999L)

  seed_rep01_before <- grep(
    "RandomNumberSeed",
    readLines(file.path(tmp, "rep01", "scenario.txt")),
    value = TRUE
  )

  ## add two more reps
  landis_replicate(tmp, 4L, files = file.path(tmp, "scenario.txt"), base_seed = 999L)

  seed_rep01_after <- grep(
    "RandomNumberSeed",
    readLines(file.path(tmp, "rep01", "scenario.txt")),
    value = TRUE
  )

  testthat::expect_identical(seed_rep01_before, seed_rep01_after)
  testthat::expect_match(
    grep("RandomNumberSeed", readLines(file.path(tmp, "rep04", "scenario.txt")), value = TRUE),
    "RandomNumberSeed\\s+1002\\b"
  )
})

testthat::test_that("landis_replicate auto-includes GDAL sidecars for .tif files", {
  tmp <- withr::local_tempdir("test_landis_sidecars_")
  file.create(file.path(tmp, c("ecoregions.tif", "ecoregions.tif.aux.xml")))

  landis_replicate(tmp, 1L)

  testthat::expect_true(fs::file_exists(fs::path(tmp, "rep01", "ecoregions.tif")))
  testthat::expect_true(fs::file_exists(fs::path(tmp, "rep01", "ecoregions.tif.aux.xml")))
})

testthat::test_that("landis_find_docker reads the option or returns default", {
  default_path <- "/opt/landis-ii/Core-Model-v8-LINUX/build/Release/Landis.Console.dll"

  ## default (option not set)
  withr::with_options(list(landisutils.docker.console = NULL), {
    testthat::expect_equal(landis_find_docker(), default_path)
  })

  ## option override
  withr::with_options(list(landisutils.docker.console = "/custom/path/Landis.Console.dll"), {
    testthat::expect_equal(landis_find_docker(), "/custom/path/Landis.Console.dll")
  })
})
