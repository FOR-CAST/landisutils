test_that(".parse_nodes normalises and validates", {
  expect_null(.parse_nodes(NULL))
  expect_null(.parse_nodes(integer(0)))
  expect_equal(.parse_nodes(c(a = 2, b = 3)), c(a = 2L, b = 3L))
  expect_error(.parse_nodes(c(2, 3)), "NAMED")
  expect_error(.parse_nodes(c(a = 0, b = 3)), ">= 1")
})

test_that(".cap_nodes_by_ram caps each host against its OWN budget", {
  ## 9 GiB per container, 0.85 fraction: a 100 GiB host admits floor(85/9) = 9,
  ## a 1000 GiB host admits floor(850/9) = 94 so its request of 30 stands.
  capped <- .cap_nodes_by_ram(
    nodes = c(small = 30L, big = 30L),
    avail_gb = list(small = 100, big = 1000),
    mem_per_container = 9,
    mem_fraction = 0.85
  )
  expect_equal(capped[["small"]], 9L)
  expect_equal(capped[["big"]], 30L)
})

test_that(".cap_nodes_by_ram leaves a host alone when its RAM is unknown", {
  capped <- .cap_nodes_by_ram(
    nodes = c(a = 12L),
    avail_gb = list(a = NA_real_),
    mem_per_container = 9,
    mem_fraction = 0.85
  )
  expect_equal(capped[["a"]], 12L)
})

test_that(".trim_nodes_to_max trims from the last host and drops emptied ones", {
  expect_equal(.trim_nodes_to_max(c(a = 30L, b = 30L, c = 30L), 90), c(a = 30L, b = 30L, c = 30L))
  expect_equal(.trim_nodes_to_max(c(a = 30L, b = 30L, c = 30L), 75), c(a = 30L, b = 30L, c = 15L))
  ## trimming past a host's whole allocation drops it entirely rather than leaving a 0
  expect_equal(.trim_nodes_to_max(c(a = 30L, b = 30L, c = 30L), 40), c(a = 30L, b = 10L))
  expect_equal(.trim_nodes_to_max(c(a = 30L, b = 30L), Inf), c(a = 30L, b = 30L))
})

test_that(".resolve_pool prefers the worker-local pool over the shared one", {
  on.exit(suppressWarnings(rm(".worker_pool", envir = .worker_pool_env)), add = TRUE)

  shared <- structure(list(names = "shared-01", n = 1L), class = "landis_pool")
  ## no worker pool set -> shared pool + env-var index (the single-node path)
  withr::with_envvar(c(LANDIS_POOL_CONTAINER_IDX = "7"), {
    got <- .resolve_pool(shared)
    expect_identical(got$pool, shared)
    expect_equal(got$idx, 7L)
  })

  ## worker-local pool set -> it wins, always at index 1
  local <- structure(list(names = "worker-01", n = 1L), class = "landis_pool")
  assign(".worker_pool", local, envir = .worker_pool_env)
  withr::with_envvar(c(LANDIS_POOL_CONTAINER_IDX = "7"), {
    got <- .resolve_pool(shared)
    expect_identical(got$pool, local)
    expect_equal(got$idx, 1L)
  })
})

test_that(".resolve_pool returns NULLs when there is no pool at all", {
  got <- .resolve_pool(NULL)
  expect_null(got$pool)
  expect_null(got$idx)
})

## Regression: the sidecar lives on shared storage and PIDs are unique per HOST only. Keying by pid
## alone lets two workers on different nodes append interleaved rows to one file, corrupting the
## trace and the memoization cache built from it.
test_that("trial-trace sidecars are keyed by host as well as pid", {
  dir <- withr::local_tempdir()
  .write_trial_trace_row(
    dir = dir,
    par_vec = c(a = 1, b = 2),
    par_names = c("a", "b"),
    total = 0.5,
    components = c(count = 0.25, size = 0.25),
    weights = c(count = 1, size = 1),
    eval_fp = "fp1"
  )
  f <- list.files(dir, pattern = "\\.csv$")
  expect_length(f, 1L)
  host <- gsub("[^A-Za-z0-9]+", "-", as.character(Sys.info()[["nodename"]]))
  expect_match(f, paste0("^worker_", host, "_[0-9]+\\.csv$"))
})

## The cache scanner must keep finding sidecars after the rename, including ones written by an older
## landisutils (plain worker_<pid>.csv), or a resumed run silently loses every memoized evaluation.
test_that("the eval-cache scan matches both legacy and host-qualified sidecars", {
  dir <- withr::local_tempdir()
  hdr <- "wall_clock_iso,pid,par_a,total,eval_fp\n"
  writeLines(paste0(hdr, "2026-01-01T00:00:00,1,1.5,0.25,fp1"), file.path(dir, "worker_123.csv"))
  writeLines(
    paste0(hdr, "2026-01-01T00:00:01,2,2.5,0.75,fp1"),
    file.path(dir, "worker_nodeB_456.csv")
  )

  cache <- new.env(parent = emptyenv())
  .augment_eval_cache(cache, dir, par_names = "a", eval_fp = "fp1")
  expect_equal(sort(unname(unlist(as.list(cache)))), c(0.25, 0.75))
})

## Exercises the real PSOCK transport (localhost, so no remote host needed) plus teardown. Pools are
## skipped: this asserts the cluster plumbing, not Docker.
test_that(".start_calibration_cluster builds and tears down a PSOCK cluster", {
  skip_on_cran()
  mc <- .start_calibration_cluster(
    nodes = c(localhost = 2L),
    max_workers = 2L,
    image = NULL,
    scratch_root = withr::local_tempdir(),
    cpu_limit = 1,
    mem_limit = "1g",
    mem_fraction = 0.85,
    pull = FALSE,
    name_prefix = "landis-cal-test",
    start_pools = FALSE
  )
  expect_equal(mc$total, 2L)
  expect_s3_class(mc$cl, "cluster")
  ## workers must have landisutils attached -- PSOCK inherits nothing, unlike FORK
  loaded <- parallel::clusterEvalQ(mc$cl, "landisutils" %in% loadedNamespaces())
  expect_true(all(unlist(loaded)))
  .stop_calibration_cluster(mc)
  ## a stopped cluster's connections are closed, so any further use must error
  expect_error(parallel::clusterEvalQ(mc$cl, 1))
})

test_that(".start_calibration_cluster returns NULL when no nodes are configured", {
  expect_null(.start_calibration_cluster(
    nodes = NULL,
    max_workers = 4L,
    image = NULL,
    scratch_root = tempdir(),
    cpu_limit = 1,
    mem_limit = "1g",
    mem_fraction = 0.85,
    pull = FALSE,
    name_prefix = "x"
  ))
})

test_that(".stop_calibration_cluster is a no-op for NULL", {
  expect_null(.stop_calibration_cluster(NULL))
})

## The image guard protects two silent failures: a host missing the image (pools start with
## pull = FALSE, so it fails late and only on that host) and hosts holding DIFFERENT digests behind
## the same mutable tag, which would evaluate trials against different LANDIS-II builds without
## erroring at all. Exercised against a fake 2-"host" cluster on localhost.
test_that(".verify_node_images accepts a consistent image and rejects a missing one", {
  skip_if(unname(Sys.which("docker")) == "", "docker not available")
  cl <- parallel::makeCluster(2L, type = "PSOCK")
  on.exit(try(parallel::stopCluster(cl), silent = TRUE), add = TRUE)

  ## NULL image = nothing to check (mock / local-method runs)
  expect_true(is.na(.verify_node_images(cl, NULL)))

  ## an image that certainly does not exist must name the offending hosts
  expect_error(
    .verify_node_images(cl, "landisutils/definitely-not-a-real-image:nope"),
    "is missing on"
  )
})
