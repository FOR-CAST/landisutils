test_that("landis_datatype() picks the smallest readable type", {
  expect_equal(landis_datatype(0), "INT1U")
  expect_equal(landis_datatype(255), "INT1U")
  expect_equal(landis_datatype(256), "INT2S")
  expect_equal(landis_datatype(32767), "INT2S")
  expect_equal(landis_datatype(32768), "INT4S")
  expect_equal(landis_datatype(1e6), "INT4S")
})

test_that("landis_datatype() never returns a type LANDIS-II rejects", {
  types <- vapply(c(0, 1, 255, 256, 32767, 32768, 1e6, 2e9), landis_datatype, character(1L))
  expect_true(all(types %in% .LANDIS_DATATYPES))
  expect_false(any(types %in% c("INT1S", "INT2U", "INT4U", "INT8U", "INT8S")))
})

test_that("landis_datatype() rejects values it cannot encode", {
  expect_error(landis_datatype(-1), "non-negative")
  expect_error(landis_datatype(3e9), "exceeds Int32")
  expect_error(landis_datatype(NA_real_), "single finite number")
  expect_error(landis_datatype(c(1, 2)), "single finite number")
})

test_that("the chosen type survives a terra write/read round trip", {
  dir <- withr::local_tempdir()
  for (mx in c(200, 5000, 1e5)) {
    f <- file.path(dir, paste0("r", mx, ".tif"))
    r <- terra::rast(nrows = 2, ncols = 2, crs = "EPSG:3005")
    terra::values(r) <- c(0L, 1L, 2L, as.integer(mx))
    terra::writeRaster(r, f, datatype = landis_datatype(mx), overwrite = TRUE)
    expect_equal(terra::datatype(terra::rast(f)), landis_datatype(mx))
    expect_equal(max(terra::values(terra::rast(f), mat = FALSE)), mx)
  }
})

## Static guard. An unreadable pixel type is invisible at write time -- LANDIS-II only rejects it when
## the extension initialises, seconds into a run, and the R side sees a bare non-zero exit with empty
## stderr. Nothing in R CMD check or the unit tests would catch a `datatype = "INT4U"` typed into a new
## writeRaster() call, so scan the sources instead. This is what shipped the 0.0.68/0.0.69 bug.
test_that("no writeRaster() call in the package uses a datatype LANDIS-II rejects", {
  src <- list.files(test_path("..", "..", "R"), pattern = "[.][Rr]$", full.names = TRUE)
  skip_if(length(src) == 0L, "package sources not available (installed-package test run)")

  found <- unlist(lapply(src, function(f) {
    lines <- readLines(f, warn = FALSE)
    ## drop comments so commented-out counter-examples and cautionary notes don't trip the guard
    code <- sub("#.*$", "", lines)
    hits <- regmatches(code, gregexpr('datatype\\s*=\\s*"[A-Z0-9]+"', code))
    idx <- which(lengths(hits) > 0L)
    if (!length(idx)) {
      return(NULL)
    }
    paste0(basename(f), ":", idx, " ", unlist(hits[idx]))
  }))

  bad <- found[!grepl(paste0('"(', paste(.LANDIS_DATATYPES, collapse = "|"), ')"'), found)]
  expect_equal(bad, character(0L))
})
