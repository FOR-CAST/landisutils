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
