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

## The block schedule the checkpointed DEoptim loop walks. Mirrors the `k <- ...` computation in
## .run_deoptim_checkpointed(); kept here so the boundaries can be asserted without running a
## multi-hour search.
.block_schedule <- function(itermax, steptol, K) {
  gens <- 0L
  out <- integer(0)
  repeat {
    k <- min(K, itermax - gens)
    if (k <= 0L) {
      break
    }
    if (gens + k > steptol) {
      k <- max(1L, min(k, steptol + 1L - gens))
    }
    gens <- gens + k
    out <- c(out, gens)
  }
  out
}

test_that("the block schedule checks convergence every generation past steptol", {
  ## Regression: with checkpoint_every = 5 and steptol = 25 the boundaries were 5,10,...,25,30,
  ## so the earliest CHECKABLE generation (26) was skipped and a converged run went to 30.
  b <- .block_schedule(itermax = 100L, steptol = 25L, K = 5L)
  expect_identical(b[1:6], c(5L, 10L, 15L, 20L, 25L, 26L))
  expect_identical(b[6:9], 26:29)
})

test_that("the block schedule never exceeds itermax", {
  ## The shrink has a max(1L, ...) floor, so an exhausted budget must be caught before it applies.
  for (itermax in c(1L, 5L, 26L, 30L, 100L)) {
    for (K in c(1L, 5L, 10L)) {
      b <- .block_schedule(itermax, steptol = 25L, K = K)
      expect_identical(max(b), itermax)
      expect_identical(b, sort(unique(b)))
    }
  }
})

test_that("a budget no longer than steptol yields no checkable generation", {
  ## itermax <= steptol can never satisfy the length(history) > steptol test; the loop must still
  ## terminate rather than spin.
  b <- .block_schedule(itermax = 25L, steptol = 25L, K = 5L)
  expect_identical(max(b), 25L)
  expect_length(b, 5L)
})
