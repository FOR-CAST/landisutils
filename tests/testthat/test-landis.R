testthat::test_that("landis_replicate creates rep subdirs inside scenario_dir", {
  tmp <- withr::local_tempdir("test_landis_replicate_")
  file.create(file.path(tmp, c("scenario.txt", "species.txt")))

  rep_dirs <- landis_replicate(
    tmp,
    n_reps = 2L,
    files = file.path(tmp, c("scenario.txt", "species.txt"))
  )

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

  landis_replicate(tmp, n_reps = 2L)
  writeLines("sentinel", file.path(tmp, "rep01", "sentinel.txt"))

  landis_replicate(tmp, n_reps = 3L)

  testthat::expect_true(fs::file_exists(fs::path(tmp, "rep01", "sentinel.txt")))
  testthat::expect_true(fs::dir_exists(fs::path(tmp, "rep03")))
})

testthat::test_that("landis_replicate re-stages updated input files into existing rep dirs", {
  ## Regression for v0.0.55 staging-skip bug: when a rep dir already exists
  ## from a prior (e.g. failed) run, the guard `if (!fs::dir_exists(rep_dir))`
  ## skipped the `fs::file_copy()` block entirely -- so stale input files
  ## from the prior staging persisted forever. A subsequent LANDIS-II run
  ## would then run against a stale `dynamic-fire.txt` / `scenario.txt`
  ## even though the project pipeline had rebuilt those files in the
  ## scenario template. v0.0.56 removed the guard; new behaviour is:
  ## always re-create the rep dir + always overwrite its input files.
  tmp <- withr::local_tempdir("test_landis_restage_")
  scen_txt <- file.path(tmp, "scenario.txt")
  cfg_txt <- file.path(tmp, "dynamic-fire.txt")
  writeLines("v1-scenario", scen_txt)
  writeLines("v1-fire", cfg_txt)

  ## First staging: rep01 gets v1 content
  landis_replicate(tmp, rep_index = 1L)
  testthat::expect_equal(readLines(fs::path(tmp, "rep01", "dynamic-fire.txt")), "v1-fire")

  ## Update the source files (simulating tar_make rebuild)
  writeLines("v2-scenario", scen_txt)
  writeLines("v2-fire", cfg_txt)

  ## Second staging: rep01 already exists; must pick up the v2 content
  landis_replicate(tmp, rep_index = 1L)
  testthat::expect_equal(readLines(fs::path(tmp, "rep01", "dynamic-fire.txt")), "v2-fire")
  testthat::expect_equal(readLines(fs::path(tmp, "rep01", "scenario.txt")), "v2-scenario")
})

testthat::test_that("landis_replicate re-stage preserves LANDIS-II output files in the rep dir", {
  ## Companion to the re-stage regression: user-added (or LANDIS-II
  ## output) files that aren't in `src_files` must survive a re-stage.
  ## landis_replicate() only copies the listed input files, so output
  ## artefacts like Landis-log.txt + log_*.csv stay in place.
  tmp <- withr::local_tempdir("test_landis_restage_outputs_")
  scen_txt <- file.path(tmp, "scenario.txt")
  writeLines("v1", scen_txt)
  landis_replicate(tmp, rep_index = 1L)
  ## simulate a partial LANDIS-II output left in the rep dir
  writeLines("partial-output", fs::path(tmp, "rep01", "Landis-log.txt"))

  writeLines("v2", scen_txt)
  landis_replicate(tmp, rep_index = 1L)

  ## Updated input
  testthat::expect_equal(readLines(fs::path(tmp, "rep01", "scenario.txt")), "v2")
  ## Output artefact preserved
  testthat::expect_true(fs::file_exists(fs::path(tmp, "rep01", "Landis-log.txt")))
  testthat::expect_equal(readLines(fs::path(tmp, "rep01", "Landis-log.txt")), "partial-output")
})

testthat::test_that("landis_replicate sets per-rep seeds from base_seed", {
  tmp <- withr::local_tempdir("test_landis_seeds_")
  writeLines(
    c("LandisData  \"Scenario\"", ">> RandomNumberSeed    4357  << optional parameter", ""),
    file.path(tmp, "scenario.txt")
  )

  landis_replicate(tmp, n_reps = 3L, files = file.path(tmp, "scenario.txt"), base_seed = 100L)

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

  landis_replicate(tmp, n_reps = 2L, files = file.path(tmp, "scenario.txt"), base_seed = 999L)

  seed_rep01_before <- grep(
    "RandomNumberSeed",
    readLines(file.path(tmp, "rep01", "scenario.txt")),
    value = TRUE
  )

  ## add two more reps
  landis_replicate(tmp, n_reps = 4L, files = file.path(tmp, "scenario.txt"), base_seed = 999L)

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

  landis_replicate(tmp, n_reps = 1L)

  testthat::expect_true(fs::file_exists(fs::path(tmp, "rep01", "ecoregions.tif")))
  testthat::expect_true(fs::file_exists(fs::path(tmp, "rep01", "ecoregions.tif.aux.xml")))
})

## ---- landis_replicate rep_index (single-rep / per-branch) mode ---------------------------------

testthat::test_that("landis_replicate rep_index creates exactly one directory", {
  tmp <- withr::local_tempdir("test_lr_rep_index_")
  file.create(file.path(tmp, c("scenario.txt", "species.txt")))

  result <- landis_replicate(
    tmp,
    rep_index = 3L,
    files = file.path(tmp, c("scenario.txt", "species.txt"))
  )

  testthat::expect_length(result, 1L)
  testthat::expect_match(result, "rep03$")
  testthat::expect_true(fs::dir_exists(result))
  testthat::expect_true(fs::file_exists(file.path(result, "scenario.txt")))
  ## rep01 and rep02 must NOT have been created
  testthat::expect_false(fs::dir_exists(file.path(tmp, "rep01")))
  testthat::expect_false(fs::dir_exists(file.path(tmp, "rep02")))
})

testthat::test_that("landis_replicate rep_index sets the correct seed", {
  tmp <- withr::local_tempdir("test_lr_rep_index_seed_")
  writeLines(
    c("LandisData  \"Scenario\"", ">> RandomNumberSeed    4357  << optional parameter", ""),
    file.path(tmp, "scenario.txt")
  )

  ## rep_index = 3 -> seed = base_seed + 2
  landis_replicate(tmp, rep_index = 3L, files = file.path(tmp, "scenario.txt"), base_seed = 100L)

  seed_line <- grep(
    "RandomNumberSeed",
    readLines(file.path(tmp, "rep03", "scenario.txt")),
    value = TRUE
  )
  testthat::expect_match(seed_line, "RandomNumberSeed\\s+102\\b")
})

testthat::test_that("landis_replicate rep_index seed matches n_reps mode for the same index", {
  tmp_a <- withr::local_tempdir("test_lr_seed_match_a_")
  tmp_b <- withr::local_tempdir("test_lr_seed_match_b_")
  writeLines(
    c("LandisData  \"Scenario\"", ">> RandomNumberSeed    1  << optional parameter", ""),
    file.path(tmp_a, "scenario.txt")
  )
  file.copy(file.path(tmp_a, "scenario.txt"), file.path(tmp_b, "scenario.txt"))

  landis_replicate(tmp_a, n_reps = 5L, files = file.path(tmp_a, "scenario.txt"), base_seed = 12345L)
  landis_replicate(
    tmp_b,
    rep_index = 4L,
    files = file.path(tmp_b, "scenario.txt"),
    base_seed = 12345L
  )

  seed_a <- grep(
    "RandomNumberSeed",
    readLines(file.path(tmp_a, "rep04", "scenario.txt")),
    value = TRUE
  )
  seed_b <- grep(
    "RandomNumberSeed",
    readLines(file.path(tmp_b, "rep04", "scenario.txt")),
    value = TRUE
  )
  testthat::expect_identical(seed_a, seed_b)
})

testthat::test_that("landis_replicate rejects both or neither of n_reps / rep_index", {
  tmp <- withr::local_tempdir("test_lr_arg_check_")
  testthat::expect_error(landis_replicate(tmp), "Exactly one")
  testthat::expect_error(landis_replicate(tmp, n_reps = 2L, rep_index = 1L), "Exactly one")
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
