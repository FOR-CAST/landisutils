## tar_landis() produces a tar_target_raw whose command embeds the
## input-hash idempotency machinery. These tests check the *structure* of
## the generated command (parse-time properties); end-to-end behaviour with
## an actual LANDIS-II run is covered by the integration suite in
## inst/integration-tests/.

testthat::test_that("tar_landis() embeds hash-based skip check in command", {
  testthat::skip_if_not_installed("targets")

  ## a minimal tar_landis() invocation that returns a tar_target_raw
  tgt <- landisutils::tar_landis(
    name = test_run,
    scenario_dir = "fake/scenario",
    rep_index = 1L,
    deps = list("fake/scenario/scenario.txt"),
    method = "local",
    base_seed = 12345L
  )

  cmd_chr <- deparse(tgt$command$expr, width.cutoff = 500L) |> paste(collapse = "\n")

  ## Hash sidecar path is named
  testthat::expect_match(cmd_chr, "input_hash\\.json", fixed = FALSE)
  ## Per-file md5 + sha1 digest are computed
  testthat::expect_match(cmd_chr, "tools::md5sum", fixed = TRUE)
  testthat::expect_match(cmd_chr, 'algo = "sha1"', fixed = TRUE)
  ## Skip check actually compares saved vs current hash
  testthat::expect_match(cmd_chr, "identical(.saved_hash, .input_hash)", fixed = TRUE)
  ## Hash is written via jsonlite after a successful run
  testthat::expect_match(cmd_chr, "jsonlite::write_json", fixed = TRUE)
})

testthat::test_that("tar_landis(force = TRUE) bakes the force flag into the skip check", {
  testthat::skip_if_not_installed("targets")

  forced <- landisutils::tar_landis(
    name = test_run_forced,
    scenario_dir = "fake/scenario",
    rep_index = 1L,
    deps = list("fake/scenario/scenario.txt"),
    method = "local",
    force = TRUE
  )
  cmd_chr <- deparse(forced$command$expr, width.cutoff = 500L) |> paste(collapse = "\n")
  ## When force = TRUE the literal value TRUE is baked into the
  ## "!isTRUE(...)" guard, short-circuiting the skip check.
  testthat::expect_match(cmd_chr, "isTRUE(TRUE)", fixed = TRUE)

  ## And the unforced default is the opposite
  unforced <- landisutils::tar_landis(
    name = test_run_unforced,
    scenario_dir = "fake/scenario",
    rep_index = 1L,
    deps = list("fake/scenario/scenario.txt"),
    method = "local"
  )
  cmd_chr2 <- deparse(unforced$command$expr, width.cutoff = 500L) |> paste(collapse = "\n")
  testthat::expect_match(cmd_chr2, "isTRUE(FALSE)", fixed = TRUE)
})
