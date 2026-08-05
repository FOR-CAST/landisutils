## Static guard: every call into a Suggests-only package must sit in a function that checks for it.
##
## This is the same shape as the pixel-type scan in test-landis_datatype.R, and for the same reason:
## the defect it prevents is invisible to R CMD check and to every unit test run on a machine that
## happens to have the package installed. It only surfaces for a user who does not have it, as
## "there is no package called 'arrow'" thrown from somewhere deep in the call stack.
##
## arrow (99 MB), ggplot2 and ggalluvial are Suggests because most consumers of this package write
## LANDIS-II configs and run the model without ever reading a parquet dataset or drawing a figure.

## Private helpers that call into a Suggests package without guarding, because each is reached from
## exactly one exported function and that function guards. Listed explicitly rather than exempting
## all dot-prefixed functions, so a new unguarded helper still fails.
.guard_exempt <- c(
  ".cached_cellids_global", ## <- prep_monthly_weather_biosim()
  ".append_clim_monthly_global", ## <- prep_monthly_weather_biosim()
  ".fread_biomass_c_times" ## <- read_biomass_c_snapshots()
)

testthat::test_that("every Suggests-package call sits inside a guarded function", {
  pkgs <- c("arrow", "ggplot2", "ggalluvial")
  src <- list.files(testthat::test_path("..", "..", "R"), pattern = "[.][Rr]$", full.names = TRUE)
  testthat::skip_if(length(src) == 0L, "package sources not available (installed-package test run)")

  unguarded <- character(0)
  for (f in src) {
    lines <- readLines(f, warn = FALSE)
    ## strip comments so prose and roxygen references to `arrow::open_dataset()` do not count
    code <- sub("#.*$", "", lines)
    defs <- grep("^[^ #].*<- *function", code)
    for (pkg in pkgs) {
      hits <- grep(paste0("\\b", pkg, "::"), code)
      for (h in hits) {
        before <- defs[defs < h]
        if (length(before) == 0L) {
          unguarded <- c(unguarded, sprintf("%s:%d <top level> %s", basename(f), h, pkg))
          next
        }
        start <- max(before)
        after <- defs[defs > start]
        end <- if (length(after)) min(after) - 1L else length(code)
        fn <- sub(" *<-.*", "", code[start])
        if (fn %in% .guard_exempt) {
          next
        }
        body <- code[start:end]
        guarded <- any(grepl(paste0("\\.need\\((c\\()?[^)]*[\"']", pkg, "[\"']"), body)) ||
          any(grepl(paste0("requireNamespace\\([\"']", pkg, "[\"']"), body))
        if (!guarded) {
          unguarded <- c(unguarded, sprintf("%s:%d %s() calls %s::", basename(f), h, fn, pkg))
        }
      }
    }
  }

  testthat::expect_equal(unguarded, character(0))
})

testthat::test_that("the Suggests-guarded packages are not Imports", {
  desc <- testthat::test_path("..", "..", "DESCRIPTION")
  testthat::skip_if_not(file.exists(desc), "DESCRIPTION not available")
  d <- read.dcf(desc)
  imports <- trimws(strsplit(d[1L, "Imports"], ",")[[1L]])
  imports <- sub("[ ]*\\(.*", "", imports)

  ## Guarding costs nothing if the package quietly returns to Imports: the guard passes, the
  ## dependency is back, and the 153 MB this bought is silently spent again.
  testthat::expect_false(any(c("arrow", "ggplot2", "ggalluvial", "cffdrs") %in% imports))
})
