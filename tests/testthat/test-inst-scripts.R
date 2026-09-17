test_that("verify_candidate_loss.R refuses a missing or abbreviated candidate_ref", {
  script <- system.file("scripts", "verify_candidate_loss.R", package = "landisutils")
  expect_snapshot(error = TRUE, source(script, local = new.env(parent = globalenv())))

  env <- new.env(parent = globalenv())
  env$candidate_ref <- "e7d98f72"
  expect_snapshot(error = TRUE, source(script, local = env))
})
