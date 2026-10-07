## tar_landis() skips a replicate whose log/input_hash.json records the hash of its current inputs.
## The hash must not depend on the collation locale of the R process computing it -- R collates
## with ICU in UTF-8 locales and by bytes under C/POSIX -- and hashes written by earlier versions,
## which sorted in the writer's locale, must still be recognised, or upgrading re-simulates every
## finished replicate.

## The expression earlier versions inlined into every tar_landis() command, verbatim.
legacy_hash <- function(dep_files, base_seed, rep_index, scenario_file) {
  digest::digest(
    list(
      files = vapply(sort(dep_files), tools::md5sum, character(1L)),
      base_seed = base_seed,
      rep_index = rep_index,
      scenario_file = scenario_file
    ),
    algo = "sha1"
  )
}

skip_if_no_collation <- function(locale) {
  old <- Sys.getlocale("LC_COLLATE")
  on.exit(Sys.setlocale("LC_COLLATE", old), add = TRUE)
  ok <- suppressWarnings(Sys.setlocale("LC_COLLATE", locale))
  testthat::skip_if(identical(ok, ""), paste("collation locale", locale, "is not available"))
}

## Three dependency files, two of which the collations order differently, and a completed replicate.
local_rep <- function(env = parent.frame()) {
  root <- withr::local_tempdir(.local_envir = env)
  deps <- file.path(root, c("output_manifest.txt", "output-biomass-community.txt", "scenario.txt"))
  for (i in seq_along(deps)) {
    writeLines(paste("input", i), deps[[i]])
  }
  rep_dir <- file.path(root, "rep01")
  dir.create(file.path(rep_dir, "log"), recursive = TRUE)
  writeLines("Model run is complete.", file.path(rep_dir, "Landis-log.txt"))
  list(deps = deps, rep_dir = rep_dir)
}

write_saved_hash <- function(rep_dir, hash) {
  jsonlite::write_json(
    list(input_hash = hash),
    file.path(rep_dir, "log", "input_hash.json"),
    auto_unbox = TRUE
  )
}

testthat::test_that("the two collations order the fixture's dependency files differently", {
  ## R on Windows collates with ICU only when R_ICU_LOCALE is set, so there is nothing to tell apart.
  testthat::skip_on_os("windows")
  skip_if_no_collation("C")
  skip_if_no_collation("C.UTF-8")
  x <- local_rep()
  testthat::expect_false(identical(
    withr::with_collate("C", legacy_hash(x$deps, 12345L, 1L, "scenario.txt")),
    withr::with_collate("C.UTF-8", legacy_hash(x$deps, 12345L, 1L, "scenario.txt"))
  ))
})

testthat::test_that("landis_input_hash() is the same in byte and ICU collation", {
  skip_if_no_collation("C")
  skip_if_no_collation("C.UTF-8")
  x <- local_rep()
  testthat::expect_identical(
    withr::with_collate("C", landis_input_hash(x$deps, 12345L, 1L, "scenario.txt")),
    withr::with_collate("C.UTF-8", landis_input_hash(x$deps, 12345L, 1L, "scenario.txt"))
  )
})

testthat::test_that("landis_input_hash() reproduces the legacy hash for a given collation", {
  skip_if_no_collation("C.UTF-8")
  x <- local_rep()
  testthat::expect_identical(
    landis_input_hash(x$deps, 12345L, 1L, "scenario.txt", collation = "C.UTF-8"),
    withr::with_collate("C.UTF-8", legacy_hash(x$deps, 12345L, 1L, "scenario.txt"))
  )
  testthat::expect_identical(
    landis_input_hash(x$deps, 12345L, 1L, "scenario.txt", collation = "no_SUCH.locale"),
    NA_character_
  )
})

for (writer in c("C", "C.UTF-8")) {
  testthat::test_that(paste("a legacy hash written under", writer, "is current in either locale"), {
    skip_if_no_collation("C")
    skip_if_no_collation("C.UTF-8")
    x <- local_rep()
    write_saved_hash(
      x$rep_dir,
      withr::with_collate(writer, legacy_hash(x$deps, 12345L, 1L, "scenario.txt"))
    )
    for (reader in c("C", "C.UTF-8")) {
      testthat::expect_true(withr::with_collate(
        reader,
        landis_rep_is_current(x$rep_dir, x$deps, 12345L, 1L, "scenario.txt")
      ))
    }
  })
}

testthat::test_that("a legacy ICU hash is current where the environment pins C collation", {
  ## R collates by bytes while LC_ALL is C, whatever Sys.setlocale() says.
  skip_if_no_collation("C")
  skip_if_no_collation("C.UTF-8")
  x <- local_rep()
  write_saved_hash(
    x$rep_dir,
    withr::with_collate("C.UTF-8", legacy_hash(x$deps, 12345L, 1L, "scenario.txt"))
  )
  withr::local_collate("C")
  withr::local_envvar(LC_ALL = "C")
  testthat::expect_true(landis_rep_is_current(x$rep_dir, x$deps, 12345L, 1L, "scenario.txt"))

  ## and the process is left as it was found
  testthat::expect_identical(
    Sys.getenv(c("LC_ALL", "LC_COLLATE")),
    c(LC_ALL = "C", LC_COLLATE = "C")
  )
  testthat::expect_identical(Sys.getlocale("LC_COLLATE"), "C")
  testthat::expect_identical(sort(c("a_b", "a-b")), c("a-b", "a_b"))
})

testthat::test_that("a hash from landis_input_hash() is current", {
  x <- local_rep()
  write_saved_hash(x$rep_dir, landis_input_hash(x$deps, 12345L, 1L, "scenario.txt"))
  testthat::expect_true(landis_rep_is_current(x$rep_dir, x$deps, 12345L, 1L, "scenario.txt"))
})

testthat::test_that("changed inputs, seed or replicate make a replicate not current", {
  x <- local_rep()
  write_saved_hash(x$rep_dir, landis_input_hash(x$deps, 12345L, 1L, "scenario.txt"))
  testthat::expect_false(landis_rep_is_current(x$rep_dir, x$deps, 12345L, 2L, "scenario.txt"))
  testthat::expect_false(landis_rep_is_current(x$rep_dir, x$deps, 1L, 1L, "scenario.txt"))
  writeLines("changed", x$deps[[3]])
  testthat::expect_false(landis_rep_is_current(x$rep_dir, x$deps, 12345L, 1L, "scenario.txt"))
})

testthat::test_that("an incomplete run, a missing or corrupt sidecar, or force is not current", {
  x <- local_rep()
  hash <- landis_input_hash(x$deps, 12345L, 1L, "scenario.txt")
  testthat::expect_false(landis_rep_is_current(x$rep_dir, x$deps, 12345L, 1L, "scenario.txt"))

  write_saved_hash(x$rep_dir, hash)
  testthat::expect_false(landis_rep_is_current(
    x$rep_dir,
    x$deps,
    12345L,
    1L,
    "scenario.txt",
    force = TRUE
  ))

  writeLines("{ not json", file.path(x$rep_dir, "log", "input_hash.json"))
  testthat::expect_false(landis_rep_is_current(x$rep_dir, x$deps, 12345L, 1L, "scenario.txt"))

  write_saved_hash(x$rep_dir, hash)
  writeLines("still running", file.path(x$rep_dir, "Landis-log.txt"))
  testthat::expect_false(landis_rep_is_current(x$rep_dir, x$deps, 12345L, 1L, "scenario.txt"))
})

## landis_dep_files() as landisutils 0.0.168 had it: one copy of every basename found in any
## scenario's dependency files, this scenario's own first.
dep_files_0168 <- function(deps, scenario_dir) {
  files <- unlist(Filter(is.character, deps))
  files <- unique(fs::path_abs(files))
  files <- as.character(fs::path_real(files[fs::file_exists(files)]))
  sd_real <- paste0(fs::path_real(scenario_dir), "/")
  files <- c(files[startsWith(files, sd_real)], files[!startsWith(files, sd_real)])
  files[!duplicated(basename(files))]
}

local_scenario <- function(root, name, files) {
  d <- fs::dir_create(fs::path(root, name))
  for (f in files) {
    writeLines(paste(name, f), fs::path(d, f))
  }
  as.character(fs::path(d, files))
}

## A no-wind scenario beside a wind scenario, and a completed replicate of the first that
## landisutils 0.0.168 staged and hashed with the wind scenario's original-wind.txt in it.
local_legacy_rep <- function(env = parent.frame()) {
  root <- withr::local_tempdir(.local_envir = env)
  only <- local_scenario(root, "ForCS_only", c("scenario.txt", "species.txt"))
  wind <- local_scenario(root, "ForCS_wind", c("scenario.txt", "species.txt", "original-wind.txt"))
  sd <- fs::path(root, "ForCS_only")
  deps <- list(c(only, wind))
  staged <- dep_files_0168(deps, sd)
  rep_dir <- landis_replicate(sd, rep_index = 1L, files = staged)[[1L]]
  dir.create(file.path(rep_dir, "log"))
  writeLines("Model run is complete.", file.path(rep_dir, "Landis-log.txt"))
  write_saved_hash(rep_dir, landis_input_hash(staged, 12345L, 1L, "scenario.txt"))
  list(root = root, sd = sd, deps = deps, rep_dir = rep_dir, only = only, wind = wind)
}

testthat::test_that("a replicate whose hash covered another scenario's file is current", {
  x <- local_legacy_rep()
  testthat::expect_true(file.exists(file.path(x$rep_dir, "original-wind.txt")))
  dep <- landis_dep_files(x$deps, x$sd)
  testthat::expect_false("original-wind.txt" %in% basename(dep))

  testthat::expect_true(landis_rep_is_current(x$rep_dir, dep, 12345L, 1L, "scenario.txt"))
  ## the "foreign" attribute is what recognises it
  testthat::expect_false(landis_rep_is_current(
    x$rep_dir,
    as.character(dep),
    12345L,
    1L,
    "scenario.txt"
  ))
})

testthat::test_that("it stays current when the other scenario's file changes or the fleet grows", {
  x <- local_legacy_rep()
  writeLines("refitted", fs::path(x$root, "ForCS_wind", "original-wind.txt"))
  ## listed first and also holding original-wind.txt, so 0.0.168 would now stage this one
  fire_wind <- local_scenario(
    x$root,
    "ForCS_fire_wind",
    c("scenario.txt", "dynamic-fire.txt", "original-wind.txt")
  )
  dep <- landis_dep_files(list(c(fire_wind, x$deps[[1L]])), x$sd)
  testthat::expect_true(landis_rep_is_current(x$rep_dir, dep, 12345L, 1L, "scenario.txt"))
})

testthat::test_that("it stays current after the scenario it was staged from leaves the fleet", {
  x <- local_legacy_rep()
  fire_wind <- local_scenario(x$root, "ForCS_fire_wind", c("scenario.txt", "original-wind.txt"))
  dep <- landis_dep_files(list(c(x$only, fire_wind)), x$sd)
  testthat::expect_false(any(as.character(fs::path_real(x$wind)) %in% attr(dep, "foreign")))
  testthat::expect_true(landis_rep_is_current(x$rep_dir, dep, 12345L, 1L, "scenario.txt"))
})

testthat::test_that("a change to its own files, seed or replicate still makes it stale", {
  x <- local_legacy_rep()
  dep <- landis_dep_files(x$deps, x$sd)
  testthat::expect_false(landis_rep_is_current(x$rep_dir, dep, 12345L, 2L, "scenario.txt"))
  testthat::expect_false(landis_rep_is_current(x$rep_dir, dep, 1L, 1L, "scenario.txt"))
  writeLines("changed", fs::path(x$sd, "species.txt"))
  testthat::expect_false(landis_rep_is_current(x$rep_dir, dep, 12345L, 1L, "scenario.txt"))
})

testthat::test_that("a replicate that ran another scenario's scenario file is not vouched for", {
  ## This scenario's own inputs are missing from deps, so 0.0.168 staged another scenario's
  ## scenario.txt and species.txt into its replicate, which then ran that scenario.
  root <- withr::local_tempdir()
  fs::dir_create(fs::path(root, "A"))
  b <- local_scenario(root, "B", c("scenario.txt", "species.txt"))
  sd <- fs::path(root, "A")
  staged <- dep_files_0168(list(b), sd)
  rep_dir <- landis_replicate(sd, rep_index = 1L, files = staged)[[1L]]
  dir.create(file.path(rep_dir, "log"))
  writeLines("Model run is complete.", file.path(rep_dir, "Landis-log.txt"))
  write_saved_hash(rep_dir, landis_input_hash(staged, 12345L, 1L, "scenario.txt"))
  writeLines("changed", b[[2L]])

  dep <- landis_dep_files(list(b), sd)
  testthat::expect_length(.foreign_staged_hashes(rep_dir, dep, 12345L, 1L, "scenario.txt"), 0L)
  testthat::expect_false(landis_rep_is_current(rep_dir, dep, 12345L, 1L, "scenario.txt"))
})

testthat::test_that("the source paths tried are capped, keeping the current order's choice", {
  x <- local_legacy_rep()
  fire_wind <- local_scenario(x$root, "ForCS_fire_wind", c("scenario.txt", "original-wind.txt"))
  saved <- jsonlite::fromJSON(file.path(x$rep_dir, "log", "input_hash.json"))$input_hash
  wind_first <- landis_dep_files(list(c(x$deps[[1L]], fire_wind)), x$sd)
  fire_wind_first <- landis_dep_files(list(c(fire_wind, x$deps[[1L]])), x$sd)

  capped <- .foreign_staged_hashes(x$rep_dir, wind_first, 12345L, 1L, "scenario.txt", 1L)
  testthat::expect_identical(capped, saved)
  testthat::expect_length(
    .foreign_staged_hashes(x$rep_dir, fire_wind_first, 12345L, 1L, "scenario.txt"),
    2L
  )
  testthat::expect_false(
    saved %in% .foreign_staged_hashes(x$rep_dir, fire_wind_first, 12345L, 1L, "scenario.txt", 1L)
  )
})
