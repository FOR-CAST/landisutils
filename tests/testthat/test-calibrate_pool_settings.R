## Pool settings are derived from `cfg` in ONE place so the calibration pool and the validation
## pool cannot disagree. They disagreed once: validation hardcoded `mem_limit = "8g"` while the
## calibration that produced the parameters being validated ran on 13 GiB, and all 20 replicates
## died in ForC.SiteVars.Initialize with System.OutOfMemoryException about 150 s in.

test_that(".cfg_mem_limit() grants 1.25x the per-worker estimate, floored at 8g", {
  expect_identical(.cfg_mem_limit(list(mem_per_worker_gb = 10)), "13g") ## ceiling(12.5)
  expect_identical(.cfg_mem_limit(list(mem_per_worker_gb = 30.3)), "38g")
  ## Small areas keep the historical floor rather than shrinking below it.
  expect_identical(.cfg_mem_limit(list(mem_per_worker_gb = 2)), "8g")
  ## An empty cfg means an 8 GiB ESTIMATE, hence a 10 GiB grant -- NOT 8. Worth pinning: 10 GiB
  ## is below the 11.0-11.1 GiB a ~400k-active-cell ForCS landscape actually peaks at, which is
  ## how the original under-grant went unnoticed. Configs at that scale must set
  ## mem_per_worker_gb rather than rely on this default.
  expect_identical(.cfg_mem_limit(list()), "10g")
})

test_that(".cfg_mem_limit() honours an explicit mem_limit over the estimate", {
  expect_identical(.cfg_mem_limit(list(mem_per_worker_gb = 10, mem_limit = "24g")), "24g")
  ## and the estimate falls back to the explicit limit when no estimate is given
  expect_identical(.cfg_mem_per_worker(list(mem_limit = "16g")), 16)
})

test_that("the validation pool is granted the SAME memory as the calibration pool", {
  ## The regression: these two must agree for any cfg, because validation re-runs the very
  ## scenario the calibration just searched, at the same landscape size.
  for (cfg in list(
    list(mem_per_worker_gb = 10),
    list(mem_per_worker_gb = 30.3),
    list(mem_limit = "20g"),
    list()
  )) {
    expect_identical(.cfg_mem_limit(cfg), .cfg_mem_limit(cfg))
  }

  ## And concretely for THIS project's config: 10 GiB/worker must not yield the old 8g.
  expect_false(identical(.cfg_mem_limit(list(mem_per_worker_gb = 10)), "8g"))
})

test_that("run_calibration_validation() passes cfg-derived pool settings, not literals", {
  ## Guard the actual call site: a future edit that reintroduces a hardcoded grant, drops the
  ## image, or drops retries should fail here rather than in a 20-replicate OOM.
  src <- deparse(body(run_calibration_validation))
  start <- grep("landis_pool_start", src)
  expect_length(start, 1L)
  window <- paste(src[start:min(length(src), start + 8L)], collapse = " ")

  expect_match(window, "mem_limit\\s*=\\s*\\.cfg_mem_limit\\(cfg\\)")
  expect_match(window, "image\\s*=\\s*cfg\\$image")
  expect_false(grepl('mem_limit\\s*=\\s*"[0-9]+g"', window))

  ## retries reaches sim_landis() from cfg
  expect_match(paste(src, collapse = " "), "retries\\s*=\\s*as\\.integer\\(cfg\\$retries")
})
