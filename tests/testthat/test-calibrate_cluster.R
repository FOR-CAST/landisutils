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
