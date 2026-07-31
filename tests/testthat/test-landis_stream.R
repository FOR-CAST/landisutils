## Build a fake in-progress run directory: a log reporting `Current time: t`, plus output maps for
## timesteps 1..t under fire/.
.mk_run <- function(dir, current_t, steps = seq_len(current_t)) {
  fs::dir_create(fs::path(dir, "fire"))
  writeLines(
    c("start", sprintf("Current time: %d", steps), sprintf("Current time: %d", current_t)),
    fs::path(dir, "Landis-log.txt")
  )
  for (t in steps) {
    for (nm in c("severity", "FuelType", "PctConifer", "PctDeadFir")) {
      writeLines("x", fs::path(dir, "fire", sprintf("%s-%d.tif", nm, t)))
    }
  }
  ## non-output files that must never be touched
  writeLines("scenario", fs::path(dir, "scenario.txt"))
  writeLines("appending", fs::path(dir, "fire", "log.csv"))
  invisible(dir)
}

test_that(".landis_current_timestep reads the LAST reported timestep", {
  dir <- withr::local_tempdir()
  .mk_run(dir, current_t = 7L)
  expect_equal(.landis_current_timestep(fs::path(dir, "Landis-log.txt")), 7L)
})

test_that(".landis_current_timestep is NA when the log is absent or has no timestep", {
  dir <- withr::local_tempdir()
  expect_true(is.na(.landis_current_timestep(fs::path(dir, "nope.txt"))))
  writeLines("no timesteps here", fs::path(dir, "Landis-log.txt"))
  expect_true(is.na(.landis_current_timestep(fs::path(dir, "Landis-log.txt"))))
})

test_that(".stream_file_timestep parses the trailing timestep, NA when absent", {
  expect_equal(.stream_file_timestep("fire/severity-12.tif"), 12L)
  expect_equal(.stream_file_timestep("fire/PctDeadFir-3.tif"), 3L)
  expect_true(is.na(.stream_file_timestep("fire/log.csv")))
  expect_true(is.na(.stream_file_timestep("scenario.txt")))
})

## The lag is the whole safety story: LANDIS-II writes several rasters per timestep, so a file for
## step t can still be OPEN when the log already reports `Current time: t`. Anything at or above
## (current - lag) must be left alone.
test_that("only timesteps at or below current-lag are moved", {
  skip_if(unname(Sys.which("rsync")) == "", "rsync not available")
  root <- withr::local_tempdir()
  run <- fs::dir_create(fs::path(root, "run"))
  dest <- fs::path(root, "dest")
  .mk_run(run, current_t = 10L)

  n <- .stream_completed_outputs(run, dest, lag_steps = 2L)
  expect_equal(n, 8L * 4L) ## timesteps 1..8, four maps each

  ## moved: present at destination, gone from scratch
  expect_true(fs::file_exists(fs::path(dest, "fire", "severity-8.tif")))
  expect_false(fs::file_exists(fs::path(run, "fire", "severity-8.tif")))
  ## within the lag window: untouched on scratch, absent at destination
  expect_true(fs::file_exists(fs::path(run, "fire", "severity-9.tif")))
  expect_true(fs::file_exists(fs::path(run, "fire", "severity-10.tif")))
  expect_false(fs::file_exists(fs::path(dest, "fire", "severity-9.tif")))
})

test_that("non-output files are never streamed or deleted", {
  skip_if(unname(Sys.which("rsync")) == "", "rsync not available")
  root <- withr::local_tempdir()
  run <- fs::dir_create(fs::path(root, "run"))
  dest <- fs::path(root, "dest")
  .mk_run(run, current_t = 10L)

  .stream_completed_outputs(run, dest, lag_steps = 2L)
  ## the scenario file and the APPEND-mode log must survive: deleting a log the sim is still
  ## writing to would truncate the run's own record
  expect_true(fs::file_exists(fs::path(run, "scenario.txt")))
  expect_true(fs::file_exists(fs::path(run, "fire", "log.csv")))
  expect_true(fs::file_exists(fs::path(run, "Landis-log.txt")))
  expect_false(fs::file_exists(fs::path(dest, "scenario.txt")))
})

test_that("TimeOfLastFire is NOT streamed by default (may be simulation state)", {
  skip_if(unname(Sys.which("rsync")) == "", "rsync not available")
  root <- withr::local_tempdir()
  run <- fs::dir_create(fs::path(root, "run"))
  dest <- fs::path(root, "dest")
  .mk_run(run, current_t = 10L)
  fs::dir_create(fs::path(run, "DFFS-output"))
  writeLines("x", fs::path(run, "DFFS-output", "TimeOfLastFire-3.tif"))

  .stream_completed_outputs(run, dest, lag_steps = 2L)
  expect_true(fs::file_exists(fs::path(run, "DFFS-output", "TimeOfLastFire-3.tif")))
})

test_that("nothing is moved before the sim has reported enough timesteps", {
  skip_if(unname(Sys.which("rsync")) == "", "rsync not available")
  root <- withr::local_tempdir()
  run <- fs::dir_create(fs::path(root, "run"))
  .mk_run(run, current_t = 1L)
  expect_equal(.stream_completed_outputs(run, fs::path(root, "dest"), lag_steps = 2L), 0L)
})

test_that("a failed sync leaves scratch untouched", {
  root <- withr::local_tempdir()
  run <- fs::dir_create(fs::path(root, "run"))
  .mk_run(run, current_t = 10L)
  ## an unwritable destination makes rsync fail; the scratch copy must survive so the next
  ## interval can retry rather than the outputs being lost
  bad <- fs::path(root, "nope", "deeper")
  fs::dir_create(fs::path(root, "nope"))
  fs::file_chmod(fs::path(root, "nope"), "a-w")
  on.exit(try(fs::file_chmod(fs::path(root, "nope"), "u+w"), silent = TRUE), add = TRUE)

  n <- suppressWarnings(.stream_completed_outputs(run, bad, lag_steps = 2L))
  expect_equal(n, 0L)
  expect_true(fs::file_exists(fs::path(run, "fire", "severity-1.tif")))
})

test_that(".next_stream_at jitters around the interval", {
  set.seed(1)
  v <- vapply(1:200, function(i) .next_stream_at(0, 600, 0.25), numeric(1))
  expect_true(all(v >= 450 & v <= 750)) ## 600 +/- 25%
  expect_gt(stats::sd(v), 0) ## actually varies, so replicates cannot stay in phase
  ## zero jitter is exact, for deterministic use
  expect_equal(.next_stream_at(100, 600, 0), 700)
})

## The allow-list must cover EVERY extension, not just the ones one project uses -- a sibling project
## running ForCS/NECN gets no benefit from a fire-only list. Scoping by output DIRECTORY rather than
## by map name achieves that without chasing each new extension, because output maps share the
## `<dir>/<name>-{timestep}.<ext>` convention.
test_that("the allow-list covers output maps from other extensions", {
  streamable <- function(f) {
    p <- .default_stream_patterns()
    ex <- .default_stream_exclude()
    any(vapply(p, function(x) grepl(x, f), logical(1))) &&
      !any(vapply(ex, function(x) grepl(x, basename(f)), logical(1)))
  }
  for (f in c(
    "fire/severity-12.tif",
    "ForCS/biomass-30.tif",
    "NECN/ANPP-5.tif",
    "outputs/biomass/biomass-Pinu_con-40.tif",
    "output/leaf-area-index/lai-7.img",
    "bda/agent1-9.tif",
    "eda/agent-MORT-2.tif",
    "harvest/biomass-removed-3.tif",
    "rootrot/RootRot-4.img",
    "hurricane/max-windspeed-3-1.tif",
    "wind/severity-6.tif"
  )) {
    expect_true(streamable(f), info = f)
  }
})

## Root-level per-timestep files are NOT outputs. `landuse-{timestep}.tif` is read by Land Use Plus
## as INPUT, so a bare `-{timestep}.tif` pattern would move a file the simulation still needs.
## Directory scoping excludes it structurally rather than by memory.
test_that("root-level timestep files and stateful maps are never streamed", {
  streamable <- function(f) {
    p <- .default_stream_patterns()
    ex <- .default_stream_exclude()
    any(vapply(p, function(x) grepl(x, f), logical(1))) &&
      !any(vapply(ex, function(x) grepl(x, basename(f)), logical(1)))
  }
  for (f in c(
    "landuse-8.tif", # Land Use Plus INPUT at scenario root
    "initial-communities.tif",
    "scenario.txt",
    "fire/log.csv",
    "DFFS-output/TimeOfLastFire-3.tif", # may be simulation state
    "rootrot/TOLD-4.img" # time of last disturbance: state
  )) {
    expect_false(streamable(f), info = f)
  }
})
