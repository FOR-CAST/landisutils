## Build a minimal ForCS log_Summary.csv (per-cell, per-time ecosystem carbon).
make_forcs_summary_csv <- function(path, times = c(0L, 100L)) {
  rows <- data.frame(
    Time = rep(times, each = 2L),
    row = rep(c(1L, 2L), length(times)),
    column = 1L,
    ABio = c(100, 80, 200, 160)[seq_len(2L * length(times))],
    BBio = 50,
    TotalDOM = 30
  )
  utils::write.csv(rows, path, row.names = FALSE)
  invisible(path)
}

testthat::test_that("read_forcs_log_summary attaches scenario/replicate, masks, empty-safe", {
  scen <- withr::local_tempdir()
  rep_dir <- file.path(scen, "rep01")
  dir.create(rep_dir)
  csv <- file.path(rep_dir, "log_Summary.csv")
  make_forcs_summary_csv(csv)

  df <- read_forcs_log_summary(csv)
  testthat::expect_true(all(c("scenario", "replicate", "Time", "row", "column") %in% names(df)))
  testthat::expect_equal(unique(df$replicate), "rep01")
  testthat::expect_equal(unique(df$scenario), basename(scen))

  testthat::expect_equal(nrow(read_forcs_log_summary(character(0))), 0L)

  masked <- read_forcs_log_summary(csv, cell_mask = data.frame(row = 1L, column = 1L))
  testthat::expect_equal(unique(masked$row), 1L)
})

testthat::test_that("write_forcs_log_summary_parquet round-trips + unions multiple scenarios", {
  base <- withr::local_tempdir()
  roots <- character(0)
  for (s in c("s1", "s2")) {
    rep_dir <- file.path(base, s, "rep01")
    dir.create(rep_dir, recursive = TRUE)
    csv <- file.path(rep_dir, "log_Summary.csv")
    make_forcs_summary_csv(csv)
    dst <- write_forcs_log_summary_parquet(csv)
    testthat::expect_match(dst, "_aggregates/forcs_log_summary/replicate=rep01/part-0\\.parquet$")
    roots <- c(roots, file.path(base, s, "_aggregates", "forcs_log_summary"))
  }
  ## atomic publish: one final parquet per scenario, no temp left
  testthat::expect_length(list.files(base, pattern = "\\.parquet$", recursive = TRUE), 2L)

  back <- dplyr::collect(open_forcs_log_summary_dataset(roots))
  testthat::expect_setequal(unique(back$scenario), c("s1", "s2"))
  testthat::expect_null(open_forcs_log_summary_dataset(file.path(base, "nope")))
})

testthat::test_that("write_forcs_log_summary_parquet errors on empty input", {
  scen <- withr::local_tempdir()
  withr::local_dir(scen) ## relative src_path -> stable snapshot (no tempdir in the message)
  dir.create("rep01")
  writeLines("Time,row,column,ABio,BBio,TotalDOM", "rep01/log_Summary.csv")
  testthat::expect_snapshot(write_forcs_log_summary_parquet("rep01/log_Summary.csv"), error = TRUE)
})
