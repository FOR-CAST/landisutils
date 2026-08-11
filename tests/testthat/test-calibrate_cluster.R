test_that(".stop_calibration_cluster() stops healthy pools when one worker is unreachable", {
  skip_on_cran()
  ## The regression: teardown used one cluster-wide `clusterCall()`, which is all-or-nothing, so a
  ## single unreachable worker left EVERY container running. Breaking one connection must still
  ## leave the healthy worker torn down.
  local_mocked_bindings(.worker_pool_stop = function() TRUE)

  cl <- parallel::makePSOCKcluster(2L)
  on.exit(try(parallel::stopCluster(cl), silent = TRUE), add = TRUE)
  close(cl[[2L]]$con)

  res <- suppressWarnings(.stop_calibration_cluster(list(cl = cl)))

  expect_equal(res$stopped, 1L)
  expect_equal(res$failed, 1L)
})

test_that(".stop_calibration_cluster() warns rather than orphaning silently", {
  skip_on_cran()
  cl <- parallel::makePSOCKcluster(2L)
  on.exit(try(parallel::stopCluster(cl), silent = TRUE), add = TRUE)
  close(cl[[1L]]$con)
  close(cl[[2L]]$con)

  expect_snapshot(invisible(.stop_calibration_cluster(list(cl = cl))))
})

test_that(".stop_calibration_cluster() tolerates a NULL cluster", {
  expect_null(.stop_calibration_cluster(NULL))
})

test_that(".verify_worker_pools() aborts, naming the hosts, when workers have no container", {
  skip_on_cran()
  ## The regression this guards: `clusterCall(cl, .worker_pool_start)` reports only hard errors, so
  ## containers that started and then died left the fleet silently short. A 90-worker fleet came up
  ## with 56 and the search ran degraded for days, because DEoptim scores a failed trial as a
  ## penalty rather than an error.
  cl <- parallel::makePSOCKcluster(2L)
  on.exit(try(parallel::stopCluster(cl), silent = TRUE), add = TRUE)
  ## Mark ONE worker unhealthy. The flag has to live on the worker: the probe closure is serialised
  ## per call, so a counter in this frame would never see the worker's increment.
  parallel::clusterCall(cl[1], function() Sys.setenv(FAKE_POOL_RUNNING = "true"))
  parallel::clusterCall(cl[2], function() Sys.setenv(FAKE_POOL_RUNNING = "false"))
  local_mocked_bindings(.worker_pool_probe = function() {
    list(
      host = "hostA",
      container = "landis-cal-fake",
      running = identical(Sys.getenv("FAKE_POOL_RUNNING"), "true")
    )
  })

  expect_error(.verify_worker_pools(cl, heal = FALSE), "1 of 2 calibration worker")
  expect_error(.verify_worker_pools(cl, heal = FALSE), "hostA=1")
})

test_that(".verify_worker_pools() heals a restartable worker instead of aborting", {
  skip_on_cran()
  cl <- parallel::makePSOCKcluster(2L)
  on.exit(try(parallel::stopCluster(cl), silent = TRUE), add = TRUE)
  parallel::clusterCall(cl[1], function() Sys.setenv(FAKE_POOL_RUNNING = "true"))
  parallel::clusterCall(cl[2], function() Sys.setenv(FAKE_POOL_RUNNING = "false"))
  local_mocked_bindings(
    .worker_pool_probe = function() {
      list(
        host = "hostA",
        container = "landis-cal-fake",
        running = identical(Sys.getenv("FAKE_POOL_RUNNING"), "true")
      )
    },
    .worker_pool_heal = function() {
      Sys.setenv(FAKE_POOL_RUNNING = "true")
      TRUE
    }
  )

  expect_no_error(suppressMessages(.verify_worker_pools(cl)))
})

test_that(".verify_worker_pools() passes a fully healthy fleet", {
  skip_on_cran()
  cl <- parallel::makePSOCKcluster(2L)
  on.exit(try(parallel::stopCluster(cl), silent = TRUE), add = TRUE)
  local_mocked_bindings(.worker_pool_probe = function() {
    list(host = "hostA", container = "landis-cal-fake", running = TRUE)
  })

  res <- expect_no_error(suppressMessages(.verify_worker_pools(cl)))
  expect_length(res, 2L)
})

## Helper: drive .start_calibration_cluster() as far as the fleet-sizing warning, then stop before
## any cluster is opened. The capacity probe is mocked so the cap is decided by `nodes` alone.
local_fleet_sizing <- function(ram = 4000, cores = 512, .local_envir = parent.frame()) {
  testthat::local_mocked_bindings(
    .probe_node_capacity = function(hosts, rscript, outfile = nullfile()) {
      list(
        ram = stats::setNames(rep(ram, length(hosts)), hosts),
        cores = stats::setNames(rep(cores, length(hosts)), hosts)
      )
    },
    .psock_cluster = function(...) stop("STOP_BEFORE_CLUSTER", call. = FALSE),
    .env = .local_envir
  )
}

size_fleet <- function(nodes, max_workers) {
  suppressMessages(try(
    .start_calibration_cluster(
      nodes = nodes,
      max_workers = max_workers,
      image = "img",
      scratch_root = tempdir(),
      cpu_limit = 1,
      mem_limit = "16g",
      mem_fraction = 0.9,
      pull = FALSE,
      name_prefix = "test",
      start_pools = FALSE
    ),
    silent = TRUE
  ))
}

test_that(".start_calibration_cluster() warns when the fleet is smaller than NP", {
  local_fleet_sizing()
  expect_warning(size_fleet(c(hostA = 10L), max_workers = 20L), "2 evaluation wave")
  expect_warning(size_fleet(c(hostA = 10L), max_workers = 20L), "10 worker\\(s\\) against NP = 20")
})

test_that(".start_calibration_cluster() flags the expensive one-short-of-NP case", {
  local_fleet_sizing()
  ## The regression: 89 workers against NP = 90 doubled every generation while 88 of them sat idle
  ## in the second wave. The cost is a step function, so ONE missing container is not a 1/90 loss.
  expect_warning(size_fleet(c(hostA = 19L), max_workers = 20L), "2 evaluation wave")
  expect_warning(size_fleet(c(hostA = 19L), max_workers = 20L), "18 worker\\(s\\) idle")
})

test_that(".start_calibration_cluster() is silent when the fleet meets NP", {
  local_fleet_sizing()
  ## Trimming DOWN to NP is the normal path and must not warn.
  expect_no_warning(size_fleet(c(hostA = 20L), max_workers = 20L))
  expect_no_warning(size_fleet(c(hostA = 30L), max_workers = 20L))
})

test_that(".start_calibration_cluster() reports the per-host split in the shortfall warning", {
  local_fleet_sizing()
  expect_warning(size_fleet(c(hostA = 6L, hostB = 4L), max_workers = 20L), "hostA=6, hostB=4")
})
