## Smoke-test the Dynamic Fire calibration end-to-end at a tiny scale.
##
## Purpose: verify that all five layers of the calibration plumbing work --
## observed-target build, spinup, scenario template, sim_landis trial, DEoptim
## loop -- without committing to a multi-hour production calibration. Useful
## when:
##
##   * setting up the calibration on a new project for the first time;
##   * after upgrading landisutils, LANDIS-II, or DEoptim to confirm nothing
##     regressed;
##   * confirming Docker / dotnet / DEoptim are all installed correctly.
##
## Usage (run from a calibrating project's root, with the project's renv
## activated; expects the listed `tar_read()` targets to exist in the project's
## `_targets/objects/`):
##
##   source(system.file("scripts/calibration_smoke_test.R", package = "landisutils"))
##
## or, to inspect / customise first:
##
##   src <- system.file("scripts/calibration_smoke_test.R", package = "landisutils")
##   file.edit(src)
##
## Cost (with the warm pool, 2 cores, NP=4 itermax=2 n_reps=1):
##   ~5 min for spinup + ~20 min for the smoke-scale DEoptim run.
##
## Prerequisites:
##   * `renv::install("DEoptim")` (DEoptim is in landisutils' Suggests).
##   * `docker` on PATH; LANDIS-II v8 image cached locally
##     (ghcr.io/landis-ii-foundation/landis-ii-v8-release:main by default).
##   * The project's targets pipeline must already have these targets built:
##     `nfdb_point_fru59`, `nfdb_poly_fru59`, `fire_years`, `fuel_types_rast`,
##     `fire_size_table`, `fuel_type_table`, `fire_damage_table`,
##     `seasons_sim_table` plus the LANDIS scenario directories under
##     `LANDIS-II/`.

suppressPackageStartupMessages({
  library(landisutils)
  library(targets)
  library(fs)
})

## ---- 0. Pre-flight ----------------------------------------------------------
if (!requireNamespace("DEoptim", quietly = TRUE)) {
  stop("DEoptim is required for the smoke test; install via renv::install('DEoptim').")
}
docker_rc <- suppressWarnings(system2("docker", "version", stdout = FALSE, stderr = FALSE))
if (!identical(as.integer(docker_rc), 0L)) {
  stop("docker CLI not available; the smoke test runs LANDIS-II via Docker.")
}

scratch_root <- fs::dir_create(fs::path(tempdir(), "calib_smoke"))
out_dir <- fs::dir_create(fs::path(scratch_root, "out"))

## ---- 1. Observed fire targets ----------------------------------------------
## Adapt the secondary inputs / labels if your project uses different names.
message("[1/5] Building observed fire targets ...")
obs_path <- save_observed_fire_targets(
  primary_points = tar_read(nfdb_point_fru59),
  primary_polys = tar_read(nfdb_poly_fru59),
  secondary_points = tryCatch(tar_read(nfdb_point_frt12), error = function(e) NULL),
  secondary_polys = tryCatch(tar_read(nfdb_poly_frt12), error = function(e) NULL),
  fire_years = tar_read(fire_years),
  fuel_types_rast = tar_read(fuel_types_rast),
  path = fs::path(out_dir, "observed_fire_targets.rds"),
  primary_label = "FRU59",
  secondary_label = "FRT12",
  severity_dist = default_severity_prior_sturtevant2009()
)

## ---- 2. Pre-calibration spinup ----------------------------------------------
## Builds and runs one LANDIS-II sim with both ForCS spinup flags ON; emits
## a community snapshot we'll reuse as the calibration IC. Adapt
## `template_dir` to any non-fire production scenario in your project.
message("[2/5] Running the pre-calibration spinup (one LANDIS-II run) ...")
spinup_dir <- fs::path(scratch_root, "spinup")
build_calibration_spinup_scenario(
  out_dir = spinup_dir,
  template_dir = "LANDIS-II/phase_2_ICH", ## non-fire production scenario
  duration = 1L,
  cell_length = 100L
)
snapshot_csv <- run_calibration_spinup(
  scenario_dir = spinup_dir,
  base_seed = 12345L,
  method = "docker"
)
snapshot_tif <- fs::path(dirname(snapshot_csv), "output-community-0.tif")

## ---- 3. Calibration scenario template --------------------------------------
message("[3/5] Building the calibration scenario template ...")
calib_dir <- fs::path(scratch_root, "calib")
build_calibration_scenario_template(
  out_dir = calib_dir,
  template_dir = "LANDIS-II/phase_2_ICH_fire", ## production fire scenario
  snapshot_ic_csv = snapshot_csv,
  snapshot_ic_tif = snapshot_tif,
  baseline_fire_size_table = tar_read(fire_size_table),
  baseline_fuel_type_table = tar_read(fuel_type_table),
  baseline_fire_damage_table = tar_read(fire_damage_table),
  baseline_seasons_sim_table = tar_read(seasons_sim_table),
  sim_years = 10L,
  cell_length = 100L
)

## ---- 4. Tiny DEoptim run (smoke scale; NOT a production calibration) -------
## Default smoke-test scale: NP=4 itermax=2 n_reps=1 n_cores=2. Per-trial cost
## is dominated by the LANDIS-II Docker sim (~1-3 min); total ~5-20 min.
message("[4/5] Smoke DEoptim run (NP=4, itermax=2, n_reps=1, n_cores=2) ...")
cfg <- list(
  lower = c(
    SeverityCalibrationFactor = 0.5,
    SpHiProp = 0,
    SumHiProp = 0,
    FallHiProp = 0,
    IgnProb_Conifer = 0,
    IgnProb_ConiferPlantation = 0,
    IgnProb_Deciduous = 0,
    IgnProb_Slash = 0,
    IgnProb_Open = 0
  ),
  upper = c(
    SeverityCalibrationFactor = 2.5,
    SpHiProp = 1,
    SumHiProp = 1,
    FallHiProp = 1,
    IgnProb_Conifer = 1.5,
    IgnProb_ConiferPlantation = 1.5,
    IgnProb_Deciduous = 1.5,
    IgnProb_Slash = 1.5,
    IgnProb_Open = 1.5
  ),
  NP = 4L,
  itermax = 2L,
  n_reps = 1L,
  sim_years = 10L,
  weights = c(count = 1, size = 1, area_fuel = 0, severity = 0),
  simulator = "landis",
  method = "docker",
  n_cores = 2L,
  parallel = TRUE,
  cpu_limit = 2,
  mem_limit = "8g",
  base_seed = 12345L,
  trace = TRUE
)
result <- calibrate_dynamic_fire(
  observed_targets_path = obs_path,
  scenario_template = fs::path(calib_dir, "scenario.txt"),
  cfg = cfg,
  out_dir = out_dir
)

## ---- 5. Report ---------------------------------------------------------------
message("[5/5] Smoke test complete.")
message("  best_params (9 entries):")
print(result$best_params)
message("  objective: ", signif(result$objective, 4))
message("  trace CSV: ", result$trace_path)
message("  pool image: ", result$pool_image %||% "(no pool)")
message("  pool digest: ", result$pool_digest %||% "(n/a)")

invisible(result)
