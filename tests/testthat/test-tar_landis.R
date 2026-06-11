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

testthat::test_that("tar_landis(method = 'docker') forwards post_completion_timeout_sec", {
  testthat::skip_if_not_installed("targets")

  ## Default: 300 s grace period baked into the landis_run_docker() call.
  default_tgt <- landisutils::tar_landis(
    name = test_run_docker,
    scenario_dir = "fake/scenario",
    rep_index = 1L,
    deps = list("fake/scenario/scenario.txt"),
    method = "docker"
  )
  cmd_default <- deparse(default_tgt$command$expr, width.cutoff = 500L) |> paste(collapse = "\n")
  testthat::expect_match(cmd_default, "landisutils::landis_run_docker", fixed = TRUE)
  testthat::expect_match(cmd_default, "post_completion_timeout_sec = 300", fixed = TRUE)

  ## Inf disables the watchdog and must be reachable from the factory.
  disabled_tgt <- landisutils::tar_landis(
    name = test_run_docker_nowatchdog,
    scenario_dir = "fake/scenario",
    rep_index = 1L,
    deps = list("fake/scenario/scenario.txt"),
    method = "docker",
    post_completion_timeout_sec = Inf
  )
  cmd_disabled <- deparse(disabled_tgt$command$expr, width.cutoff = 500L) |> paste(collapse = "\n")
  testthat::expect_match(cmd_disabled, "post_completion_timeout_sec = Inf", fixed = TRUE)
})

testthat::test_that(".watchdog_should_stop() fires only after the grace period (Inf disables)", {
  should_stop <- landisutils:::.watchdog_should_stop

  ## Marker not yet seen: never stop (and tolerate an NA arming time).
  testthat::expect_false(should_stop(
    completion_seen = FALSE,
    seen_at = NA_real_,
    now = 100,
    timeout = 300
  ))
  ## Seen, but still inside the grace period.
  testthat::expect_false(should_stop(
    completion_seen = TRUE,
    seen_at = 100,
    now = 350,
    timeout = 300
  ))
  ## Seen, grace period exceeded -> stop.
  testthat::expect_true(should_stop(
    completion_seen = TRUE,
    seen_at = 100,
    now = 401,
    timeout = 300
  ))
  ## Inf timeout disables the watchdog even long after the marker.
  testthat::expect_false(should_stop(
    completion_seen = TRUE,
    seen_at = 100,
    now = 1e6,
    timeout = Inf
  ))
})

testthat::test_that(".landis_run_complete() detects the completion marker in Landis-log.txt", {
  is_complete <- landisutils:::.landis_run_complete
  d <- withr::local_tempdir()

  ## No Landis-log.txt yet.
  testthat::expect_identical(is_complete(d), FALSE)

  ## Log present but no completion marker.
  writeLines(c("Starting LANDIS-II", "timestep 1"), file.path(d, "Landis-log.txt"))
  testthat::expect_identical(is_complete(d), FALSE)

  ## Completion marker present.
  writeLines(
    c("timestep 50", "Model run is complete.", "Shutting down"),
    file.path(d, "Landis-log.txt")
  )
  testthat::expect_identical(is_complete(d), TRUE)
})

testthat::test_that(".docker_container_running() is FALSE (not an error) for an unknown container", {
  testthat::skip_if(unname(Sys.which("docker")) == "", "docker not available")
  running <- landisutils:::.docker_container_running
  testthat::expect_identical(running("landis-run-pkgtest-does-not-exist-0000"), FALSE)
})
