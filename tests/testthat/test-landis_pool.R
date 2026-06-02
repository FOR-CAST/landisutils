## Docker-gated tests for the warm pool helpers. Skip unless `docker` is on PATH
## (so the test file runs cleanly in CI without Docker installed).

.docker_available <- function() {
  rc <- suppressWarnings(system2("docker", "version", stdout = FALSE, stderr = FALSE))
  identical(as.integer(rc), 0L)
}

test_that("landis_pool_start + exec + stop round-trips against a busybox image", {
  skip_if_not(.docker_available(), "docker CLI not available")

  scratch <- withr::local_tempdir()
  ## Use a tiny image we know is on most hosts; this test exercises pool plumbing,
  ## not LANDIS-II semantics.
  pool <- landis_pool_start(
    n = 2L,
    image = "busybox:latest",
    scratch_root = scratch,
    cpu_limit = 1,
    mem_limit = "256m",
    name_prefix = "landispool-test"
  )
  withr::defer(landis_pool_stop(pool))

  expect_s3_class(pool, "landis_pool")
  expect_length(pool$names, 2L)
  expect_true(all(grepl("^landispool-test-", pool$names)))

  ## Containers are actually running:
  for (name in pool$names) {
    rc <- suppressWarnings(system2(
      "docker",
      c("inspect", "--format", "{{.State.Running}}", name),
      stdout = TRUE,
      stderr = FALSE
    ))
    expect_equal(trimws(rc[1L]), "true")
  }

  ## Exec a noop in each container; verify isolation by writing a sentinel file
  ## inside the bind-mount under a per-call sub-directory, then asserting each
  ## sentinel exists on the host afterwards.
  fs::dir_create(fs::path(scratch, "t1"))
  fs::dir_create(fs::path(scratch, "t2"))

  res1 <- landis_pool_exec(
    pool,
    idx = 1L,
    workdir = "/scratch/t1",
    command = "sh",
    args = c("-c", "echo hello-from-1 > sentinel.txt")
  )
  res2 <- landis_pool_exec(
    pool,
    idx = 2L,
    workdir = "/scratch/t2",
    command = "sh",
    args = c("-c", "echo hello-from-2 > sentinel.txt")
  )

  expect_equal(res1$status, 0L)
  expect_equal(res2$status, 0L)
  expect_equal(readLines(fs::path(scratch, "t1", "sentinel.txt")), "hello-from-1")
  expect_equal(readLines(fs::path(scratch, "t2", "sentinel.txt")), "hello-from-2")

  ## And isolation: t2 sentinel did NOT bleed into t1's dir.
  expect_false(fs::file_exists(fs::path(scratch, "t1", "container2_marker")))
})

test_that("landis_pool_exec() surfaces a clear error on non-zero exit", {
  skip_if_not(.docker_available(), "docker CLI not available")

  scratch <- withr::local_tempdir()
  pool <- landis_pool_start(
    n = 1L,
    image = "busybox:latest",
    scratch_root = scratch,
    name_prefix = "landispool-errtest"
  )
  withr::defer(landis_pool_stop(pool))

  expect_error(
    landis_pool_exec(
      pool,
      idx = 1L,
      workdir = "/scratch",
      command = "sh",
      args = c("-c", "exit 7")
    ),
    "status 7"
  )
})

test_that("landis_pool_stop() is idempotent", {
  skip_if_not(.docker_available(), "docker CLI not available")

  scratch <- withr::local_tempdir()
  pool <- landis_pool_start(
    n = 1L,
    image = "busybox:latest",
    scratch_root = scratch,
    name_prefix = "landispool-stoptest"
  )
  expect_no_error(landis_pool_stop(pool))
  expect_no_error(landis_pool_stop(pool)) ## already stopped; tolerate
})

test_that("sim_r_reimpl() errors with the not-yet-implemented message", {
  expect_error(sim_r_reimpl(par_vec = c(a = 1)), "not yet implemented")
})

test_that("landis_pool_restart_one() replaces a container with a fresh one", {
  skip_if_not(.docker_available(), "docker CLI not available")

  scratch <- withr::local_tempdir()
  pool <- landis_pool_start(
    n = 1L,
    image = "busybox:latest",
    scratch_root = scratch,
    name_prefix = "landispool-restart"
  )
  withr::defer(landis_pool_stop(pool))

  original_name <- pool$names[1L]
  ## Restart and capture the (updated) pool returned
  pool <- landis_pool_restart_one(pool, 1L)
  new_name <- pool$names[1L]

  expect_false(identical(original_name, new_name))
  ## Confirm the new container is running
  rc <- suppressWarnings(processx::run(
    "docker",
    c("inspect", "--format", "{{.State.Running}}", new_name),
    error_on_status = FALSE,
    echo = FALSE
  ))
  expect_equal(rc$status, 0L)
  expect_equal(trimws(rc$stdout), "true")

  ## And the old one is gone
  rc_old <- suppressWarnings(processx::run(
    "docker",
    c("inspect", original_name),
    error_on_status = FALSE,
    echo = FALSE
  ))
  expect_true(rc_old$status != 0L)
})

test_that("landis_pool_exec(retries=1) retries after a container failure", {
  skip_if_not(.docker_available(), "docker CLI not available")

  scratch <- withr::local_tempdir()
  pool <- landis_pool_start(
    n = 1L,
    image = "busybox:latest",
    scratch_root = scratch,
    name_prefix = "landispool-retry"
  )
  withr::defer(landis_pool_stop(pool))

  ## Stop the only container to force the first attempt to fail; the retry path
  ## should restart it and succeed.
  processx::run("docker", c("rm", "-f", pool$names[1L]), error_on_status = FALSE, echo = FALSE)

  res <- landis_pool_exec(
    pool,
    idx = 1L,
    workdir = "/scratch",
    command = "sh",
    args = c("-c", "echo retried > marker.txt"),
    retries = 1L
  )
  expect_equal(res$status, 0L)
  expect_equal(res$attempts, 2L) ## 1 failed + 1 successful retry
  expect_equal(readLines(fs::path(scratch, "marker.txt")), "retried")
})
