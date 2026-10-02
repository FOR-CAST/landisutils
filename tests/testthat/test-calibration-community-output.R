## build_calibration_scenario_template(community_output_timestep =)
##
## The calibration scores trials from the severity maps and the fire logs, so the cohort snapshot is
## left out unless it is asked for: a per-year snapshot for every trial of every generation is far
## more disk than the search needs. It is asked for when post-fire cohort state has to be inspected,
## because mortality derived from the severity maps and the damage table describes what the damage
## table specifies rather than what the extension killed.
##
## Building a template needs a real one to copy (a succession config, species, ecoregions, the fire
## configs), so these skip without the fixture. Point LANDISUTILS_CALIBRATION_TEMPLATE at one to run
## them; CI has no such directory.

template_fixture <- function() {
  dir <- Sys.getenv("LANDISUTILS_CALIBRATION_TEMPLATE", "")
  skip_if(!nzchar(dir) || !fs::dir_exists(dir), "no calibration template fixture")
  dir
}

built <- function(template, ...) {
  out <- fs::path(withr::local_tempdir(.local_envir = parent.frame()), "tpl")
  ic_csv <- fs::path(template, "initial-communities.csv")
  ic_tif <- fs::path(template, "initial-communities.tif")
  skip_if(!fs::file_exists(ic_csv) || !fs::file_exists(ic_tif), "fixture has no IC snapshot")
  build_calibration_scenario_template(
    out_dir = out,
    template_dir = template,
    snapshot_ic_csv = ic_csv,
    snapshot_ic_tif = ic_tif,
    sim_years = 3L,
    cell_length = 120,
    ...
  )
  out
}

other_extensions <- function(dir) {
  txt <- readLines(fs::path(dir, "scenario.txt"), warn = FALSE)
  from <- grep("Other Extensions", txt)
  if (!length(from)) {
    return(character())
  }
  rest <- txt[seq(from[1] + 1L, length(txt))]
  rest <- rest[!grepl("^\\s*>>", rest)]
  trimws(rest[nzchar(trimws(rest))])
}

test_that("cohort output is left out by default", {
  out <- built(template_fixture())

  expect_false(any(grepl("Output Biomass Community", other_extensions(out))))
})

test_that("a timestep registers the extension and emits every year it covers", {
  out <- built(template_fixture(), community_output_timestep = 1L)

  expect_true(any(grepl("Output Biomass Community", other_extensions(out))))
  expect_match(
    paste(readLines(fs::path(out, "output-biomass-community.txt"), warn = FALSE), collapse = "\n"),
    "Timestep\\s+1"
  )
  ## Every manifest entry becomes a tracked file, so it must name what the extension writes and
  ## nothing else. Measured on a 3-year run at Timestep 1: a CSV at year 0 and at each year after,
  ## and ONE map-code raster, at year 0.
  manifest <- readLines(fs::path(out, "output_manifest.txt"), warn = FALSE)
  expect_true(all(sprintf("community-input-file-%d.csv", 0:3) %in% manifest))
  expect_true("output-community-0.tif" %in% manifest)
  expect_false(any(sprintf("output-community-%d.tif", 1:3) %in% manifest))
})

test_that("a timestep coarser than the run emits only its multiples, and year 0", {
  out <- built(template_fixture(), community_output_timestep = 3L)
  manifest <- readLines(fs::path(out, "output_manifest.txt"), warn = FALSE)

  expect_true(all(c("community-input-file-0.csv", "community-input-file-3.csv") %in% manifest))
  expect_false(any(sprintf("community-input-file-%d.csv", c(1L, 2L)) %in% manifest))
})

test_that("a timestep longer than the whole run still asks for year 0", {
  ## seq(from = 5, to = 3, by = 5) errors, so this path has to be built from multiples
  out <- built(template_fixture(), community_output_timestep = 5L)
  manifest <- readLines(fs::path(out, "output_manifest.txt"), warn = FALSE)

  expect_true("community-input-file-0.csv" %in% manifest)
  expect_false(any(sprintf("community-input-file-%d.csv", 1:5) %in% manifest))
})

test_that("a timestep below one is refused", {
  expect_error(built(template_fixture(), community_output_timestep = 0))
})
