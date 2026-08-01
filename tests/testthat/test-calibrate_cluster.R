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
  ## NOT `== 2`: the per-host caps are RAM- and physical-core-aware, so a small CI runner can
  ## legitimately be trimmed to one worker. What must hold regardless of hardware is that the
  ## reported total matches the cluster actually built.
  expect_gte(mc$total, 1L)
  expect_lte(mc$total, 2L)
  expect_length(mc$cl, mc$total)
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
  skip_if_not(.docker_available(), "docker (with Linux containers) not available")
  cl <- parallel::makeCluster(2L, type = "PSOCK")
  on.exit(try(parallel::stopCluster(cl), silent = TRUE), add = TRUE)

  ## NULL image = nothing to check (mock / local-method runs)
  expect_true(is.na(.verify_node_images(cl, NULL)))

  ## an image that certainly does not exist must name the offending hosts
  expect_error(
    .verify_node_images(cl, "landisutils/definitely-not-a-real-image:nope"),
    "is missing on"
  )

  ## Probe EXACTLY what .verify_node_images() probes: guarding on {{.Id}} while the function reads
  ## {{index .RepoDigests 0}} lets the two disagree -- an image built locally but never pulled has
  ## an Id and no RepoDigests, so the guard passes and the function then errors.
  ## POSITIVE case. Without this, the test above passes even when the docker call is malformed --
  ## a usage error and a genuinely absent image both come back non-zero, so "missing" is reported
  ## either way. That is exactly how an unquoted --format string (which system2() splits into
  ## separate shell words) shipped as a guard that failed every host it checked.
  ## Probe the image ON THE WORKERS, which is where .verify_node_images() looks. Probing the main
  ## process instead lets the two disagree -- exactly what happened in CI, where the image satisfied
  ## a main-process guard and the function then reported it missing from the workers. Pull first
  ## (~2 MB): testthat runs files in PARALLEL here, so relying on another file's docker test to have
  ## pulled busybox makes this test's coverage depend on file ordering.
  suppressWarnings(system2(
    "docker",
    c("pull", "-q", "busybox:latest"),
    stdout = FALSE,
    stderr = FALSE
  ))
  on_workers <- unlist(parallel::clusterCall(cl, function() {
    length(suppressWarnings(system2(
      "docker",
      c(
        "image",
        "inspect",
        "--format",
        shQuote("{{index .RepoDigests 0}}"),
        shQuote("busybox:latest")
      ),
      stdout = TRUE,
      stderr = FALSE
    ))) >
      0L
  }))
  skip_if(!all(on_workers), "busybox:latest not visible to the cluster workers")
  digest <- .verify_node_images(cl, "busybox:latest")
  expect_false(is.na(digest))
  expect_match(digest, "@sha256:|busybox")
})

## Regression: the coordinator runs ON one of the fleet hosts, so the fleet almost always contains the
## local machine. Naming it explicitly makes parallelly SSH from the host to itself, which fails when
## the machine's own name does not resolve to a listening sshd (127.0.1.1 here) -- cluster setup then
## stalls with no diagnostic. Localised hosts are launched directly, without SSH.
test_that(".localise_hosts rewrites this machine to localhost, in any spelling", {
  expect_equal(.localise_hosts(c("nodeA", "nodeB"), me = "nodeA"), c("localhost", "nodeB"))
  ## FQDN coordinator vs short name in the fleet, and vice versa
  expect_equal(
    .localise_hosts(c("nodeA", "nodeB"), me = "nodeA.example.ca"),
    c("localhost", "nodeB")
  )
  expect_equal(
    .localise_hosts(c("nodeA.example.ca", "nodeB"), me = "nodeA"),
    c("localhost", "nodeB")
  )
  ## already-local entries stay local; unrelated hosts are untouched
  expect_equal(.localise_hosts("localhost", me = "nodeA"), "localhost")
  expect_equal(.localise_hosts(c("nodeB", "nodeC"), me = "nodeA"), c("nodeB", "nodeC"))
  ## repeats (one entry per worker) are all rewritten
  expect_equal(.localise_hosts(rep("nodeA", 3L), me = "nodeA"), rep("localhost", 3L))
})


## Regression: RAM alone is not a sufficient cap on a heterogeneous cluster. Two hosts can hold the
## same ~1 TB while differing 2.7x in physical cores, so a RAM-only cap books the smaller host to
## ~94% of its cores and the larger to ~35%. DEoptim waits for every population member, so the whole
## generation then runs at the saturated host's pace (measured: 68 vs 40 min per rep).
test_that(".cap_nodes_by_cpu caps each host by its OWN physical core count", {
  capped <- .cap_nodes_by_cpu(
    nodes = c(big = 45L, small = 45L),
    cores = list(big = 128, small = 48),
    cores_per_worker = 1,
    cpu_fraction = 0.85
  )
  expect_equal(capped[["big"]], 45L) ## floor(128 * 0.85) = 108, so the request stands
  expect_equal(capped[["small"]], 40L) ## floor(48 * 0.85) = 40, so it binds
})

test_that(".cap_nodes_by_cpu leaves a host alone when its core count is unknown", {
  capped <- .cap_nodes_by_cpu(c(a = 12L), list(a = NA_real_), 1, 0.85)
  expect_equal(capped[["a"]], 12L)
})

test_that(".cap_nodes_by_cpu scales with cores_per_worker", {
  ## a worker needing 2 cores halves what a host can hold
  capped <- .cap_nodes_by_cpu(c(a = 100L), list(a = 48), cores_per_worker = 2, cpu_fraction = 1)
  expect_equal(capped[["a"]], 24L)
})

test_that("the tighter of the RAM and CPU caps wins, per host", {
  nodes <- c(ramBound = 60L, cpuBound = 60L)
  by_ram <- .cap_nodes_by_ram(nodes, list(ramBound = 100, cpuBound = 1000), 9, 0.85)
  by_cpu <- .cap_nodes_by_cpu(nodes, list(ramBound = 128, cpuBound = 48), 1, 0.85)
  capped <- pmin(by_ram, by_cpu)
  expect_equal(unname(capped[["ramBound"]]), 9L) ## RAM binds here
  expect_equal(unname(capped[["cpuBound"]]), 40L) ## cores bind here
})
