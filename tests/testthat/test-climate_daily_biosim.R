## NOTE: the reference values below were captured from a live BioSIM FWI_Daily call
## (RNCan/BioSimClient_R @ 7d8c28d, 2026-08-11) for a single cell at
## lat 56.34408, lon -129.1691, elev 1284.474 m, year 1991. `returned` is what BioSIM
## reported; `expected` is the FWI System's own ISI equation evaluated on the FFMC and
## WS in the same record. They differ only in the sign of the exponent.

test_that("repair_fwi_exponent recovers BioSIM's sign-dropped exponents", {
  ffmc <- c(17.16240, 5.85462, 1.79614)
  ws <- c(8.96132, 8.76132, 13.86130)
  returned <- c(649936, 18141300, 149181000)

  ## the FWI System's ISI equation, evaluated independently of cffdrs
  m <- 147.2 * (101 - ffmc) / (59.5 + ffmc)
  expected <- 0.208 * exp(0.05039 * ws) * 91.9 * exp(-0.1386 * m) * (1 + m^5.31 / 4.93e7)

  expect_equal(repair_fwi_exponent(returned), expected, tolerance = 1e-5)
})

test_that("repair_fwi_exponent leaves valid values untouched", {
  ## spans the full legitimate range of every FWI System code and index
  valid <- c(0, 1e-4, 0.5, 19.84, 95.08, 101, 144.8, 772, 1932.9, 9999.9)
  expect_identical(repair_fwi_exponent(valid), valid)

  ## NA propagates rather than erroring or being repaired
  expect_identical(repair_fwi_exponent(c(1, NA, 3)), c(1, NA, 3))
  expect_identical(repair_fwi_exponent(NA_real_), NA_real_)
  expect_identical(repair_fwi_exponent(numeric(0)), numeric(0))
})

test_that("repair_fwi_exponent lands every repaired value below the artifact threshold", {
  ## The artifact only arises for values BioSIM formats in scientific notation, i.e.
  ## below 1e-4, so a correct repair can never produce a value at or above that. This
  ## makes the repair idempotent: a second application changes nothing.
  artifacts <- c(1e5, 6.49936e5, 1.8141e7, 9.99992e9)
  repaired <- repair_fwi_exponent(artifacts)

  expect_true(all(repaired < 1e-4))
  expect_identical(repair_fwi_exponent(repaired), repaired)
})

test_that("repair_fwi_exponent rejects non-numeric input", {
  expect_error(repair_fwi_exponent("649936"))
})

test_that("repair_fwi_daily repairs the codes and re-derives the indices", {
  ## row 1-2: real corrupt ISI/FWI records from a live fetch (FFMC/DMC/DC clean)
  ## row 3:   clean record
  ## row 4:   real corrupt DMC (true 9.14847e-05), which must not reach buildup_index()
  x <- data.frame(
    FFMC = c(17.16240, 1.79614, 60, 17.16240),
    DMC = c(1.14745, 0, 40, 914847),
    DC = c(96.4630, 91.8443, 300, 200),
    ISI = c(649936, 149181000, 3, 5),
    BUI = c(2.2286250, 0, 30, 25),
    FWI = c(3026.811, 21822.165, 8, 9),
    WS = c(8.96132, 13.86130, 12, 9)
  )
  out <- repair_fwi_daily(x)

  expect_equal(out$DMC[[4]], 9.14847e-05, tolerance = 1e-8)
  expect_true(all(out$ISI < 100))
  expect_true(all(out$FWI < 200))
  ## the clean record keeps BioSIM's own BUI, which recomputing reproduces
  expect_equal(out$BUI[[3]], 60, tolerance = 1e-6)
  ## unrelated columns are carried through untouched
  expect_identical(out$WS, x$WS)
})

test_that("repair_fwi_daily validates and reports out-of-range values", {
  x <- data.frame(FFMC = 60, DMC = 40, DC = 300, WS = 12)
  expect_silent(repair_fwi_daily(x))

  ## too large to be valid, too small to be the exponent artifact
  bad <- data.frame(FFMC = 500, DMC = 40, DC = 300, WS = 12)
  expect_error(repair_fwi_daily(bad), "outside physical bounds")
  expect_no_error(repair_fwi_daily(bad, validate = FALSE))
})

test_that("repair_fwi_daily requires the columns it repairs from", {
  expect_error(repair_fwi_daily(data.frame(FFMC = 60)), "needs column")
})
