## read_landis_resource_logs() parses *_resources.log sidecars written by
## landis_run_docker() / landis_run_local(). The parser must:
##   - find logs recursively under <run_dir>/<rep>/log/{docker,local}_resources.log
##   - return one row per log with replicate + source columns
##   - parse the documented keys (elapsed_sec, peak_mem_bytes, host_cpu_*)
##   - keep numeric fields numeric and string fields character
##   - gracefully handle missing dirs and missing keys (return NA, not error)
##
## Tests use a tempdir + hand-written log files so they are hermetic and run
## identically on Linux, macOS, and Windows.

## ---- helpers ------------------------------------------------------------------------------------

make_rep_log <- function(root, scenario, rep, source, content) {
  log_dir <- file.path(root, scenario, rep, "log")
  dir.create(log_dir, recursive = TRUE, showWarnings = FALSE)
  writeLines(content, file.path(log_dir, paste0(source, "_resources.log")))
  log_dir
}

full_log_content <- c(
  "elapsed_sec: 1234.5",
  "peak_mem_bytes: 4294967296",
  "host_cpu_model: AMD EPYC 7702 64-Core Processor",
  "host_cpu_cores: 256",
  "host_ram_bytes: 1056596520960"
)

legacy_log_content <- c(
  ## landisutils <= 0.0.21 only wrote elapsed + memory.
  "elapsed_sec: 600.0",
  "peak_mem_bytes: 2147483648"
)

## ---- tests --------------------------------------------------------------------------------------

testthat::test_that("read_landis_resource_logs() returns empty df when dir is missing", {
  res <- landisutils::read_landis_resource_logs(file.path(tempdir(), "does-not-exist-xyz"))
  testthat::expect_s3_class(res, "data.frame")
  testthat::expect_equal(nrow(res), 0L)
})

testthat::test_that("read_landis_resource_logs() returns empty df when no logs exist", {
  td <- withr::local_tempdir()
  res <- landisutils::read_landis_resource_logs(td)
  testthat::expect_s3_class(res, "data.frame")
  testthat::expect_equal(nrow(res), 0L)
})

testthat::test_that("read_landis_resource_logs() parses a single full v0.0.22 log", {
  td <- withr::local_tempdir()
  make_rep_log(td, "scen", "rep01", "docker", full_log_content)

  res <- landisutils::read_landis_resource_logs(td)
  testthat::expect_equal(nrow(res), 1L)
  testthat::expect_equal(res$replicate, "rep01")
  testthat::expect_equal(res$source, "docker")
  testthat::expect_equal(res$elapsed_sec, 1234.5)
  testthat::expect_equal(res$peak_mem_bytes, 4294967296)
  testthat::expect_equal(res$host_cpu_model, "AMD EPYC 7702 64-Core Processor")
  testthat::expect_equal(res$host_cpu_cores, 256)
  testthat::expect_equal(res$host_ram_bytes, 1056596520960)
})

testthat::test_that("read_landis_resource_logs() distinguishes docker vs local source", {
  td <- withr::local_tempdir()
  make_rep_log(td, "scen", "rep01", "docker", full_log_content)
  make_rep_log(td, "scen", "rep02", "local", full_log_content)

  res <- landisutils::read_landis_resource_logs(td)
  res <- res[order(res$replicate), ]
  testthat::expect_equal(res$replicate, c("rep01", "rep02"))
  testthat::expect_equal(res$source, c("docker", "local"))
})

testthat::test_that("read_landis_resource_logs() aggregates multiple reps", {
  td <- withr::local_tempdir()
  for (i in 1:5) {
    make_rep_log(
      td,
      "scen",
      sprintf("rep%02d", i),
      "docker",
      sub("1234.5", as.character(1000 + i), full_log_content)
    )
  }
  res <- landisutils::read_landis_resource_logs(td)
  testthat::expect_equal(nrow(res), 5L)
  testthat::expect_setequal(res$elapsed_sec, 1001:1005)
})

testthat::test_that("read_landis_resource_logs() returns NA for missing keys (legacy v<=0.0.21 logs)", {
  td <- withr::local_tempdir()
  make_rep_log(td, "scen", "rep01", "docker", legacy_log_content)

  res <- landisutils::read_landis_resource_logs(td)
  testthat::expect_equal(res$elapsed_sec, 600.0)
  testthat::expect_equal(res$peak_mem_bytes, 2147483648)
  ## host_* columns either absent or NA depending on union-of-keys logic.
  if ("host_cpu_model" %in% names(res)) {
    testthat::expect_true(is.na(res$host_cpu_model))
  }
  if ("host_ram_bytes" %in% names(res)) {
    testthat::expect_true(is.na(res$host_ram_bytes))
  }
})

testthat::test_that("read_landis_resource_logs() merges legacy + new logs into one frame", {
  td <- withr::local_tempdir()
  make_rep_log(td, "scen", "rep01", "docker", legacy_log_content) ## no host fields
  make_rep_log(td, "scen", "rep02", "docker", full_log_content) ## full

  res <- landisutils::read_landis_resource_logs(td)
  testthat::expect_equal(nrow(res), 2L)
  ## Both rows have elapsed/memory numeric.
  testthat::expect_true(all(!is.na(res$elapsed_sec)))
  testthat::expect_true(all(!is.na(res$peak_mem_bytes)))
  ## host_* should be NA for the legacy row, populated for the v0.0.22 row.
  legacy_row <- res[res$replicate == "rep01", ]
  new_row <- res[res$replicate == "rep02", ]
  testthat::expect_true(is.na(legacy_row$host_cpu_model))
  testthat::expect_false(is.na(new_row$host_cpu_model))
})

testthat::test_that("read_landis_resource_logs() writes by run helpers round-trip cleanly", {
  ## Integration-ish check: the resource log written by landis_run_*() should
  ## be parseable back into the same fields. We simulate the write side here
  ## (since landis_run_*() needs a real LANDIS binary) and verify the read.
  td <- withr::local_tempdir()
  log_dir <- file.path(td, "scen", "rep01", "log")
  dir.create(log_dir, recursive = TRUE)
  host <- landisutils::host_cpu_info()
  writeLines(
    c(
      sprintf("elapsed_sec: %.1f", 42.5),
      sprintf("peak_mem_bytes: %.0f", 4096),
      sprintf("host_cpu_model: %s", host$model %||% "NA"),
      sprintf("host_cpu_cores: %s", host$n_logical %||% "NA"),
      sprintf(
        "host_ram_bytes: %s",
        if (is.na(host$ram_bytes)) "NA" else sprintf("%.0f", host$ram_bytes)
      )
    ),
    file.path(log_dir, "docker_resources.log")
  )
  res <- landisutils::read_landis_resource_logs(td)
  testthat::expect_equal(res$elapsed_sec, 42.5)
  testthat::expect_equal(res$peak_mem_bytes, 4096)
  if (!is.na(host$model)) {
    testthat::expect_equal(res$host_cpu_model, host$model)
  }
})
