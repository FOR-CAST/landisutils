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
