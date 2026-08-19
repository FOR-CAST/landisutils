test_that("succession species.csv writes double columns with a decimal point", {
  ## Biomass Succession infers each CSV column's type from the FIRST data row, so a whole number
  ## written bare types the column Int32 and later decimals abort the run at extension load.
  df <- data.frame(
    species = c("a", "b"),
    leaflongevity = c(3, 3),
    wooddecayrate = c(0.062, 0.062),
    mortalityshape = c(5, 7.5),
    growthcurve = c(0, 0.1),
    leafLignin = c(0.2, 0.2),
    shadetolerance = c(5L, 3L),
    firetolerance = c(1L, 2L),
    stringsAsFactors = FALSE
  )
  path <- withr::local_tempdir()
  out <- prepSpeciesData(df, type = "succession", path = path)
  lines <- readLines(out)

  expect_equal(lines[[2]], '"a",3.0,0.062,5.0,0.0,0.2,5,1')
  expect_equal(lines[[3]], '"b",3.0,0.062,7.5,0.1,0.2,3,2')
})

test_that("integer-typed tolerance columns are not given a decimal point", {
  df <- data.frame(
    species = "a",
    leaflongevity = 3,
    wooddecayrate = 0.062,
    mortalityshape = 15,
    growthcurve = 1,
    leafLignin = 0.2,
    shadetolerance = 5L,
    firetolerance = 2L,
    stringsAsFactors = FALSE
  )
  path <- withr::local_tempdir()
  lines <- readLines(prepSpeciesData(df, type = "succession", path = path))

  expect_match(lines[[2]], ",5,2$")
})
