## NOTE: FPSM is not a LANDIS-II extension, so there is no upstream Core8 test
## input. The fixture below reproduces the row semantics observed in the shipped
## example outputs (deploy/examples/{SimpleTier1Ex,ComplexTier3Ex}/FPS_raw_out.csv
## of LANDIS-II-Foundation/Extension-Forest-Product-Sector): types 4 and 5 carry
## the annual end-of-year stocks, types 1-3 carry decay and combustion, and only
## destinations 2006/2007 are atmospheric. Types 1/2 additionally emit a partial
## residual set in the terminal year, which is what `drop_terminal_year` guards.

.write_raw <- function(dir, rows) {
  fs::dir_create(dir)
  p <- fs::path(dir, "FPS_raw_out.csv")
  writeLines(
    c(
      "Type, YearCreated, YearReported,  Market, FromPool, To_Gas/Pool, AmountEmitted, AmountRetained",
      rows
    ),
    p
  )
  p
}

## Years 1-2 are complete (types 4/5); year 3 is terminal residue (types 1/2).
.fixture_rows <- c(
  "5, 1, 1, 300, 204, 204, 0, 10",
  "5, 1, 1, 300, 204, 1000, 2, 0",
  "4, 1, 1, 0, 1000, 1000, 0, 5",
  "3, 1, 1, 0, 1002, 2006, 7, 0",
  "3, 1, 1, 0, 1002, 2007, 0.5, 0",
  "5, 1, 2, 300, 204, 204, 0, 8",
  "4, 1, 2, 0, 1000, 1000, 0, 6",
  "1, 1, 2, 0, 1000, 1009, 1, 0",
  "1, 1, 3, 0, 1009, 1009, 0, 99",
  "2, 1, 3, 0, 1001, 1001, 0, 99"
)

test_that("scenario and replicate are derived for both run-directory layouts", {
  d <- withr::local_tempdir()
  flat <- .write_raw(fs::path(d, "ScenA", "rep01"), .fixture_rows)
  nested <- .write_raw(fs::path(d, "ScenA", "rep02", "fps"), .fixture_rows)
  out <- read_fps_raw_out(c(flat, nested))
  expect_identical(unique(out$scenario), "ScenA")
  expect_setequal(unique(out$replicate), c("rep01", "rep02"))
  expect_named(
    out,
    c(
      "scenario",
      "replicate",
      "Type",
      "YearCreated",
      "YearReported",
      "Market",
      "FromPool",
      "ToPool",
      "AmountEmitted",
      "AmountRetained"
    )
  )
})

test_that("empty input gives an empty tibble rather than an error", {
  expect_identical(nrow(read_fps_raw_out(character(0))), 0L)
  expect_identical(nrow(fps_pools(tibble::tibble())), 0L)
  expect_identical(nrow(fps_stocks_by_pool(tibble::tibble())), 0L)
})

test_that("fps_pools() separates stocks from ATMOSPHERIC emissions", {
  d <- withr::local_tempdir()
  raw <- read_fps_raw_out(.write_raw(fs::path(d, "ScenA", "rep01"), .fixture_rows))
  pl <- fps_pools(raw)
  y1 <- pl[pl$year == 1L, ]
  expect_equal(y1$products_tC, 10)
  expect_equal(y1$special_pools_tC, 5)
  ## 7 to 2006 and 0.5 to 2007 are atmospheric; the 2 leaving the product pool
  ## for 1000 and the 1 transferred to 1009 are NOT.
  expect_equal(y1$emitted_co2_tC, 7)
  expect_equal(y1$emitted_ch4_tC, 0.5)
})

test_that("the terminal year is dropped by default and kept on request", {
  d <- withr::local_tempdir()
  raw <- read_fps_raw_out(.write_raw(fs::path(d, "ScenA", "rep01"), .fixture_rows))
  ## Year 3 has only types 1/2, so it is residue, not an annual report.
  expect_identical(max(fps_pools(raw)$year), 2L)
  expect_identical(max(fps_pools(raw, drop_terminal_year = FALSE)$year), 3L)
  ## Kept, it reads as a total collapse -- the cliff the default prevents.
  kept <- fps_pools(raw, drop_terminal_year = FALSE)
  expect_equal(kept$products_tC[kept$year == 3L], 0)
  expect_equal(kept$special_pools_tC[kept$year == 3L], 0)
})

test_that("fps_stocks_by_pool() labels product and special pools", {
  d <- withr::local_tempdir()
  raw <- read_fps_raw_out(.write_raw(fs::path(d, "ScenA", "rep01"), .fixture_rows))
  sp <- fps_stocks_by_pool(raw)
  expect_setequal(unique(sp$kind), c("product", "special"))
  expect_equal(sp$stock_tC[sp$year == 1L & sp$pool == 204L], 10)
  expect_equal(sp$stock_tC[sp$year == 1L & sp$pool == 1000L], 5)
  expect_false(3L %in% sp$year)
})
