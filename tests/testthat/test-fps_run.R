## NOTE: FPSM is not a LANDIS-II extension (its `PlugIn.Run()` is a stub), so there
## is no upstream Core8 test input to reference. The fixtures below are derived from
## the ForCS log_FluxBio.csv / log_FluxDOM.csv column layouts documented in the FPSM
## user guide (Dymond et al. 2025, Tables 2 and 3), which are the layouts
## `ForestProps.cs::ReadHarvestFile()` indexes by position.

.bio_header <- paste(
  "Time",
  "row",
  "column",
  "ecoregion",
  "species",
  "Dist",
  "MERCH_ToDOM",
  "MERCH_ToAir",
  "FOL_ToDOM",
  "FOL_ToAir",
  "OtherWoody_ToDOM",
  "OtherWoody_ToAir",
  "CrsRt_ToDOM",
  "CrsRt_ToAir",
  "FRt_ToDOM",
  "FRt_ToAir",
  "BioToFPS",
  sep = ","
)
.dom_header <- paste(
  "Time",
  "row",
  "column",
  "ecoregion",
  "species",
  "Dist",
  "VF_A_toAir",
  "VF_B_toAir",
  "Fast_A_toAir",
  "Fast_B_toAir",
  "MED_toAir",
  "Slow_A_toAir",
  "Slow_B_toAir",
  "Sng_Stem_toAir",
  "SngStemToMed",
  "Sng_Oth_toAir",
  "SngOthToFast",
  "Extra_toAir",
  "SnagsToFPS",
  "DOMtoFPS",
  sep = ","
)

test_that("fps_output_files() names the three files FPSM writes", {
  expect_identical(fps_output_files(), c("FPS_log.txt", "FPS_raw_out.csv", "FPS_test_out.csv"))
})

test_that("declared input filenames are read from the config, ignoring comments", {
  d <- withr::local_tempdir()
  writeLines(
    c(
      'LandisData  "FPS"',
      ">> HarvestFileLive \"commented-out.csv\"",
      'HarvestFileLive  "log_FluxBio.csv"',
      'HarvestFileDOM   "log_FluxDOM.csv"'
    ),
    file.path(d, "fps.txt")
  )
  expect_identical(
    landisutils:::.fps_declared_inputs(file.path(d, "fps.txt")),
    c(live = "log_FluxBio.csv", dom = "log_FluxDOM.csv")
  )
})

test_that("the flux-log header assertion accepts the documented ForCS layout", {
  d <- withr::local_tempdir()
  writeLines(.bio_header, bio <- file.path(d, "log_FluxBio.csv"))
  writeLines(.dom_header, dom <- file.path(d, "log_FluxDOM.csv"))
  expect_true(landisutils:::.fps_check_flux_header(bio, "live"))
  expect_true(landisutils:::.fps_check_flux_header(dom, "dom"))
})

test_that("the flux-log header assertion catches a reordered column", {
  ## FPSM indexes these columns by position with no header validation, so a ForCS
  ## release that reordered them would otherwise be read as the wrong quantity.
  d <- withr::local_tempdir()
  p <- strsplit(.bio_header, ",", fixed = TRUE)[[1L]]
  p[c(11L, 17L)] <- p[c(17L, 11L)]
  writeLines(paste(p, collapse = ","), bad <- file.path(d, "bad.csv"))
  expect_error(
    landisutils:::.fps_check_flux_header(bad, "live"),
    "does not match the column layout FPSM reads by position"
  )
})

test_that("a filename differing only in case is caught before the container starts", {
  ## FPSM's own shipped examples name `log_fluxDOM.csv` beside `log_FluxDOM.csv`:
  ## harmless on Windows, fatal on a case-sensitive filesystem.
  d <- withr::local_tempdir()
  writeLines(
    c(
      'LandisData  "FPS"',
      'HarvestFileLive  "log_FluxBio.csv"',
      'HarvestFileDOM   "log_FluxDOM.csv"'
    ),
    file.path(d, "fps.txt")
  )
  writeLines(.bio_header, file.path(d, "log_FluxBio.csv"))
  writeLines(.dom_header, file.path(d, "log_fluxDOM.csv")) ## lower-case f
  expect_error(
    fps_run_docker(d, image = "unused-because-preflight-fails"),
    "differing only in case"
  )
})

test_that("a missing config file is reported before anything else", {
  d <- withr::local_tempdir()
  expect_error(fps_run_docker(d, image = "unused"), "configuration file not found")
})

test_that("no image and no option is an error", {
  d <- withr::local_tempdir()
  withr::local_options(landisutils.fps.image = NULL)
  expect_error(fps_run_docker(d), "no FPSM image")
})
