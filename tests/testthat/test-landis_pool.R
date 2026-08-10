## Docker-gated tests for the warm pool helpers. The `.docker_available()` gate lives in
## helper-docker.R so every docker-backed test file shares one definition.
##
## These exercise pool PLUMBING against a tiny stand-in image (busybox), not LANDIS-II
## semantics -- so the core-version guard in `landis_pool_start()` has nothing to find
## and would (correctly) refuse to start every pool here. Opting out is the honest
## response: the alternative is pulling a multi-GB LANDIS image to test `docker exec`
## argument handling. `landis_assert_version()` has its own coverage in
## test-landis_version.R, including against the real image.
## Scoped per TEST, not per file. A file-level `teardown_env()` opt-out is not restored until the
## whole `test_dir()` run finishes, so it silently disabled the version guard for every test file
## that ran afterwards -- including test-landis_version.R, whose entire purpose is to prove the
## guard fires. That produced 13 failures in a serial run (`TESTTHAT_PARALLEL=false`) and none
## under the parallel config this package uses by default, where each file gets its own process.
local_skip_version_check <- function(.local_envir = parent.frame()) {
  withr::local_options(landisutils.skip_version_check = TRUE, .local_envir = .local_envir)
}

test_that("landis_pool_start + exec + stop round-trips against a busybox image", {
  skip_if_not(.docker_available(), "docker CLI not available")
  local_skip_version_check()

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
  local_skip_version_check()

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
  local_skip_version_check()

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

## Container ID behind a name; NA_character_ if no such container.
.container_id <- function(name) {
  rc <- suppressWarnings(processx::run(
    "docker",
    c("inspect", "--format", "{{.Id}}", name),
    error_on_status = FALSE,
    echo = FALSE
  ))
  if (rc$status != 0L) NA_character_ else trimws(rc$stdout)
}

test_that("landis_pool_restart_one() replaces the container but keeps its name", {
  skip_if_not(.docker_available(), "docker CLI not available")
  local_skip_version_check()

  scratch <- withr::local_tempdir()
  pool <- landis_pool_start(
    n = 1L,
    image = "busybox:latest",
    scratch_root = scratch,
    name_prefix = "landispool-restart"
  )
  withr::defer(landis_pool_stop(pool))

  original_name <- pool$names[1L]
  original_id <- .container_id(original_name)
  expect_false(is.na(original_id))

  pool <- landis_pool_restart_one(pool, 1L)

  ## The name is a stable identity -- callers that hold a copy of the pool (and
  ## R's copy-on-modify guarantees some do) must still address the container.
  expect_identical(pool$names[1L], original_name)

  ## ... but it is genuinely a new container, and it is running.
  new_id <- .container_id(original_name)
  expect_false(is.na(new_id))
  expect_false(identical(original_id, new_id))

  rc <- suppressWarnings(processx::run(
    "docker",
    c("inspect", "--format", "{{.State.Running}}", original_name),
    error_on_status = FALSE,
    echo = FALSE
  ))
  expect_equal(trimws(rc$stdout), "true")
})

test_that("repeated restarts do not leak containers", {
  skip_if_not(.docker_available(), "docker CLI not available")
  local_skip_version_check()

  scratch <- withr::local_tempdir()
  prefix <- "landispool-leak"
  pool <- landis_pool_start(
    n = 1L,
    image = "busybox:latest",
    scratch_root = scratch,
    name_prefix = prefix
  )
  withr::defer(landis_pool_stop(pool))

  ## Regression guard: restarts once generated a fresh `-r<rand>` name each time,
  ## so the previous container was abandoned rather than replaced and the pool's
  ## owner kept exec'ing a removed one. Three restarts must still leave exactly
  ## one container belonging to this pool.
  for (i in seq_len(3L)) {
    ## Deliberately discard the return value: the owner must not have to
    ## reassign for the pool to stay valid.
    landis_pool_restart_one(pool, 1L)
  }

  live <- processx::run(
    "docker",
    c("ps", "--filter", paste0("name=", pool$pool_id), "--format", "{{.Names}}"),
    error_on_status = FALSE,
    echo = FALSE
  )
  live_names <- Filter(nzchar, trimws(strsplit(live$stdout, "\n")[[1L]]))
  expect_identical(live_names, pool$names[1L])
})

test_that("landis_pool_exec(retries=1) retries after a container failure", {
  skip_if_not(.docker_available(), "docker CLI not available")
  local_skip_version_check()

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

test_that("landis_pool_exec() converts a timeout into a retry, then a clear error", {
  ## The regression: the calibration called landis_pool_exec() with no `timeout_sec`, so processx
  ## waited forever on a `docker exec` that wedged instead of exiting. The coordinator, parked in a
  ## blocking read on that worker's socket, waited with it -- one run hung ~16 h before anything
  ## noticed. `retries` was no help: it only fires when the simulator EXITS.
  ##
  ## The second half matters as much as the first: processx signals a timeout as a CONDITION, not a
  ## non-zero status, so `error_on_status = FALSE` does not cover it. Letting it propagate would
  ## unwind parApply and DEoptim and discard every generation since the last checkpoint -- strictly
  ## worse than the hang it is meant to cure.
  pool <- structure(
    list(
      names = "landis-cal-fake",
      image = "busybox",
      scratch_root = tempdir(),
      n = 1L,
      user_args = NULL,
      cpu_args = NULL,
      mem_args = NULL
    ),
    class = "landis_pool"
  )
  attempts <- 0L
  local_mocked_bindings(landis_pool_restart_one = function(pool, idx) invisible(pool))
  local_mocked_bindings(
    run = function(command, args, timeout = Inf, ...) {
      attempts <<- attempts + 1L
      stop(structure(
        class = c("system_command_timeout_error", "system_command_error", "error", "condition"),
        list(message = "process timed out", call = NULL, stdout = "", stderr = "")
      ))
    },
    .package = "processx"
  )

  expect_error(
    suppressMessages(landis_pool_exec(
      pool,
      idx = 1L,
      workdir = "/scratch",
      command = "true",
      timeout_sec = 1,
      retries = 1L
    )),
    "timed out"
  )
  ## 1 initial attempt + 1 retry: a timeout must CONSUME the retry budget, not bypass it.
  expect_equal(attempts, 2L)
})

test_that("landis_pool_exec() passes timeout_sec through to processx", {
  pool <- structure(
    list(
      names = "landis-cal-fake",
      image = "busybox",
      scratch_root = tempdir(),
      n = 1L,
      user_args = NULL,
      cpu_args = NULL,
      mem_args = NULL
    ),
    class = "landis_pool"
  )
  seen <- NULL
  local_mocked_bindings(
    run = function(command, args, timeout = Inf, ...) {
      seen <<- timeout
      list(status = 0L, stdout = "", stderr = "")
    },
    .package = "processx"
  )

  landis_pool_exec(pool, idx = 1L, workdir = "/scratch", command = "true", timeout_sec = 42)
  expect_equal(seen, 42)

  ## NULL must remain wait-forever, so the single-node and interactive paths are unchanged.
  landis_pool_exec(pool, idx = 1L, workdir = "/scratch", command = "true", timeout_sec = NULL)
  expect_equal(seen, Inf)
})
