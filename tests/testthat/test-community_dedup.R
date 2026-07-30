## Build a snapshot pair: `codes` is the per-cell map code laid out row-wise on an n x n grid, and
## `comms` maps each code to its cohort rows. Mirrors the Output Biomass Community layout: one map
## code per active cell, 0 = inactive.
.mk_snapshot <- function(dir, codes, comms, nrow_grid = NULL) {
  csv <- file.path(dir, "community-input-file-0.csv")
  tif <- file.path(dir, "output-community-0.tif")
  d <- data.table::rbindlist(lapply(names(comms), function(k) {
    x <- data.table::as.data.table(comms[[k]])
    x[, MapCode := as.integer(k)]
    data.table::setcolorder(x, "MapCode")
    x
  }))
  data.table::fwrite(d, csv)
  n <- nrow_grid %||% sqrt(length(codes))
  r <- terra::rast(nrows = n, ncols = length(codes) / n, crs = "EPSG:3005")
  terra::values(r) <- codes
  terra::writeRaster(r, tif, datatype = "INT4U", overwrite = TRUE)
  list(csv = csv, tif = tif)
}

.coh <- function(sp, age, bio) data.frame(SpeciesName = sp, CohortAge = age, CohortBiomass = bio)

test_that("identical communities collapse and the raster is remapped consistently", {
  dir <- withr::local_tempdir()
  ## codes 1 and 3 are identical; 2 differs. 0 = inactive.
  comms <- list(
    "1" = .coh(c("Pinu_con", "Pice_gla"), c(50L, 80L), c(1000, 2000)),
    "2" = .coh("Popu_tre", 30L, 500),
    "3" = .coh(c("Pinu_con", "Pice_gla"), c(50L, 80L), c(1000, 2000))
  )
  s <- .mk_snapshot(dir, c(1L, 2L, 3L, 0L), comms, nrow_grid = 2)

  res <- dedup_community_snapshot(s$csv, s$tif, quiet = TRUE)
  expect_equal(res$map_codes_before, 3L)
  expect_equal(res$map_codes_after, 2L)

  out <- data.table::fread(s$csv)
  expect_setequal(unique(out$MapCode), c(1L, 2L))

  ## cells that shared a community now share a code; the distinct one stays distinct
  v <- terra::values(terra::rast(s$tif), mat = FALSE)
  expect_equal(v[1], v[3])
  expect_true(v[1] != v[2])
  expect_equal(v[4], 0L) ## inactive preserved

  ## every raster code still resolves in the CSV -- the "Unknown map code" invariant
  present <- setdiff(unique(v), c(0L, NA))
  expect_true(all(present %in% out$MapCode))
})

test_that("each cell's cohort list is preserved exactly through the remap", {
  dir <- withr::local_tempdir()
  comms <- list(
    "7" = .coh(c("Abie_las", "Pinu_con"), c(20L, 60L), c(10, 20)),
    "9" = .coh("Betu_pap", 40L, 33),
    "11" = .coh(c("Abie_las", "Pinu_con"), c(20L, 60L), c(10, 20))
  )
  s <- .mk_snapshot(dir, c(7L, 9L, 11L, 9L), comms, nrow_grid = 2)

  before_v <- terra::values(terra::rast(s$tif), mat = FALSE)
  before <- data.table::fread(s$csv)
  cohorts_of <- function(d, code) {
    x <- d[d$MapCode == code, c("SpeciesName", "CohortAge", "CohortBiomass")]
    data.table::setorderv(x, names(x))
    x
  }
  expected <- lapply(before_v, function(cd) if (cd == 0L) NULL else cohorts_of(before, cd))

  dedup_community_snapshot(s$csv, s$tif, quiet = TRUE)

  after_v <- terra::values(terra::rast(s$tif), mat = FALSE)
  after <- data.table::fread(s$csv)
  for (i in seq_along(after_v)) {
    if (after_v[i] == 0L) {
      expect_null(expected[[i]])
      next
    }
    expect_equal(cohorts_of(after, after_v[i]), expected[[i]], info = paste("cell", i))
  }
})

test_that("cohort ORDER within a community does not prevent collapsing", {
  dir <- withr::local_tempdir()
  comms <- list(
    "1" = .coh(c("Pinu_con", "Pice_gla"), c(50L, 80L), c(1, 2)),
    "2" = .coh(c("Pice_gla", "Pinu_con"), c(80L, 50L), c(2, 1)) ## same set, listed in reverse
  )
  s <- .mk_snapshot(dir, c(1L, 2L, 1L, 2L), comms, nrow_grid = 2)
  res <- dedup_community_snapshot(s$csv, s$tif, quiet = TRUE)
  expect_equal(res$map_codes_after, 1L)
})

test_that("communities differing only in biomass are NOT merged", {
  dir <- withr::local_tempdir()
  comms <- list(
    "1" = .coh("Pinu_con", 50L, 1000),
    "2" = .coh("Pinu_con", 50L, 1000.5) ## a real difference, however small
  )
  s <- .mk_snapshot(dir, c(1L, 2L, 1L, 2L), comms, nrow_grid = 2)
  res <- dedup_community_snapshot(s$csv, s$tif, quiet = TRUE)
  expect_equal(res$map_codes_after, 2L)
})

test_that("an already-minimal snapshot is left semantically unchanged", {
  dir <- withr::local_tempdir()
  comms <- list("1" = .coh("Pinu_con", 50L, 1), "2" = .coh("Popu_tre", 20L, 2))
  s <- .mk_snapshot(dir, c(1L, 2L, 1L, 2L), comms, nrow_grid = 2)
  res <- dedup_community_snapshot(s$csv, s$tif, quiet = TRUE)
  expect_equal(res$map_codes_before, res$map_codes_after)
  expect_equal(res$rows_before, res$rows_after)
})

test_that("writing to separate output paths leaves the inputs untouched", {
  dir <- withr::local_tempdir()
  comms <- list("1" = .coh("Pinu_con", 50L, 1), "2" = .coh("Pinu_con", 50L, 1))
  s <- .mk_snapshot(dir, c(1L, 2L, 1L, 2L), comms, nrow_grid = 2)
  before <- data.table::fread(s$csv)

  o_csv <- file.path(dir, "out.csv")
  o_tif <- file.path(dir, "out.tif")
  res <- dedup_community_snapshot(s$csv, s$tif, out_csv = o_csv, out_tif = o_tif, quiet = TRUE)

  expect_equal(res$map_codes_after, 1L)
  expect_equal(data.table::fread(s$csv), before) ## input CSV untouched
  expect_equal(data.table::uniqueN(data.table::fread(o_csv)$MapCode), 1L)
})

test_that("a MapCode-less CSV errors rather than writing a broken pair", {
  dir <- withr::local_tempdir()
  csv <- file.path(dir, "bad.csv")
  data.table::fwrite(data.frame(SpeciesName = "Pinu_con", CohortAge = 1L), csv)
  tif <- file.path(dir, "r.tif")
  r <- terra::rast(nrows = 2, ncols = 2, crs = "EPSG:3005")
  terra::values(r) <- c(1L, 1L, 1L, 1L)
  terra::writeRaster(r, tif, datatype = "INT4U")
  ## not expect_snapshot(): the message embeds the caller-supplied path, which is a random
  ## tempdir here, so a snapshot would differ on every run.
  expect_error(dedup_community_snapshot(csv, tif, quiet = TRUE), "has no MapCode column")
})

test_that("empty communities stay ACTIVE and collapse to one shared code", {
  ## Regression: active cells whose map code has no CSV rows are legitimate (a cell with no cohorts).
  ## Zeroing them silently shrinks the simulated landscape -- 95,063 cells / 3.4% on NRD_Quesnel.
  dir <- withr::local_tempdir()
  comms <- list("1" = .coh("Pinu_con", 50L, 1), "2" = .coh("Pinu_con", 50L, 1))
  ## codes 5 and 6 appear in the raster but NOT in the CSV; 0 is genuinely inactive
  s <- .mk_snapshot(dir, c(1L, 2L, 5L, 6L, 0L, 1L), comms, nrow_grid = 2)

  before <- terra::values(terra::rast(s$tif), mat = FALSE)
  res <- dedup_community_snapshot(s$csv, s$tif, quiet = TRUE)
  after <- terra::values(terra::rast(s$tif), mat = FALSE)

  expect_equal(sum(before > 0), sum(after > 0)) ## active-cell count unchanged
  expect_equal(res$empty_cells, 2)
  expect_false(is.na(res$empty_code))
  expect_equal(after[3], after[4]) ## both empty cells share one code
  expect_true(after[3] > 0) ## and remain ACTIVE
  expect_equal(after[5], 0L) ## genuinely inactive cell untouched
  ## the shared code is deliberately absent from the CSV, as the codes it replaced were
  expect_false(res$empty_code %in% data.table::fread(s$csv)$MapCode)
})

test_that("a snapshot with no empty communities reports empty_code = NA", {
  dir <- withr::local_tempdir()
  comms <- list("1" = .coh("Pinu_con", 50L, 1), "2" = .coh("Popu_tre", 20L, 2))
  s <- .mk_snapshot(dir, c(1L, 2L, 1L, 0L), comms, nrow_grid = 2)
  res <- dedup_community_snapshot(s$csv, s$tif, quiet = TRUE)
  expect_true(is.na(res$empty_code))
  expect_equal(res$empty_cells, 0)
})

## LANDIS-II's GDAL reader accepts only Byte / Int16 / Int32 / Float32 / Float64. Writing the remapped
## raster as UInt32 -- terra's natural choice for positive-only map codes, and what this function did
## before 0.0.70 -- aborts the run with "Raster band is not byte, short, int, float, double" the moment
## Biomass Succession opens it, so the pixel type is part of the contract, not an implementation detail.
## Note INT1U is UNSIGNED: GDAL calls it Byte, which is on the accepted list. INT1S is not a substitute
## (GDAL 3.7+ writes it as Int8, which is not).
test_that("the remapped raster uses a pixel type LANDIS-II can read", {
  dir <- withr::local_tempdir()
  comms <- list("1" = .coh("Pinu_con", 50L, 1), "2" = .coh("Popu_tre", 20L, 2))
  s <- .mk_snapshot(dir, c(1L, 2L, 1L, 0L), comms, nrow_grid = 2)
  res <- dedup_community_snapshot(s$csv, s$tif, quiet = TRUE)

  expect_equal(res$datatype, "INT1U")
  expect_match(terra::datatype(terra::rast(s$tif)), "^INT1U$")
})

test_that("pixel type widens with the code range, staying within the accepted set", {
  dir <- withr::local_tempdir()
  ## 300 distinct communities forces something wider than a byte; still well inside Int16.
  comms <- stats::setNames(
    lapply(seq_len(300L), function(i) .coh("Pinu_con", 50L, i)),
    as.character(seq_len(300L))
  )
  s <- .mk_snapshot(dir, c(seq_len(300L), 0L, 0L, 0L, 0L), comms, nrow_grid = 8)
  res <- dedup_community_snapshot(s$csv, s$tif, quiet = TRUE)

  expect_equal(res$map_codes_after, 300L)
  expect_equal(res$datatype, "INT2S")
  expect_match(terra::datatype(terra::rast(s$tif)), "^INT2S$")
})
