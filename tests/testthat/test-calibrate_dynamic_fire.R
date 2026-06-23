## Tests for the Dynamic Fire calibration pure-data helpers (Phase 8a).
## Fixtures under inst/testdata/ are sampled from real LANDIS-II rep01 outputs.

test_that("calibration_par_names() is the canonical 9-entry vector", {
  nm <- calibration_par_names()
  expect_type(nm, "character")
  expect_length(nm, 9L)
  expect_setequal(
    nm,
    c(
      "SeverityCalibrationFactor",
      "SpHiProp",
      "SumHiProp",
      "FallHiProp",
      "IgnProb_Conifer",
      "IgnProb_ConiferPlantation",
      "IgnProb_Deciduous",
      "IgnProb_Slash",
      "IgnProb_Open"
    )
  )
})

test_that("parse_dynamic_fire_logs() reads sample event + summary logs", {
  rep_dir <- withr::local_tempdir()
  fs::dir_create(fs::path(rep_dir, "fire"))
  fs::file_copy(
    system.file("testdata", "dynamic-fire-event-log-sample.csv", package = "landisutils"),
    fs::path(rep_dir, "fire", "dynamic-fire-event-log.csv")
  )
  fs::file_copy(
    system.file("testdata", "dynamic-fire-summary-log-sample.csv", package = "landisutils"),
    fs::path(rep_dir, "fire", "dynamic-fire-summary-log.csv")
  )

  parsed <- parse_dynamic_fire_logs(rep_dir, pixel_area_ha = 1)

  expect_named(
    parsed,
    c("n_fires_by_year", "fire_sizes_ha", "events", "total_sites_burned", "n_events")
  )
  expect_s3_class(parsed$n_fires_by_year, "tbl_df")
  expect_named(parsed$n_fires_by_year, c("year", "n_fires"))
  ## sample has 4 events; sizes column is DamagedSites
  expect_equal(parsed$n_events, 4L)
  expect_equal(parsed$total_sites_burned, 538L + 4L + 50L + 4L)
  expect_true(all(parsed$fire_sizes_ha > 0))
  expect_true(is.numeric(parsed$fire_sizes_ha))
  ## eco column trimmed of leading/trailing whitespace
  expect_equal(unique(parsed$events$eco), "FRU59")
})

test_that("parse_dynamic_fire_logs() errors clearly when logs are missing", {
  rep_dir <- withr::local_tempdir()
  expect_error(parse_dynamic_fire_logs(rep_dir), "Dynamic Fire logs not found")
})

test_that("patch_fire_config() rewrites SeverityCalibrationFactor / HiProp / IgnProb", {
  scenario_dir <- withr::local_tempdir()
  fs::file_copy(
    system.file("testdata", "dynamic-fire-sample.txt", package = "landisutils"),
    fs::path(scenario_dir, "dynamic-fire.txt")
  )

  cand <- c(
    SeverityCalibrationFactor = 1.7,
    SpHiProp = 0.42,
    SumHiProp = 0.63,
    FallHiProp = 0.17,
    IgnProb_Conifer = 0.8,
    IgnProb_ConiferPlantation = 1.2,
    IgnProb_Deciduous = 1.0,
    IgnProb_Slash = 0.5,
    IgnProb_Open = 1.5
  )

  out_path <- patch_fire_config(scenario_dir, cand)
  expect_true(fs::file_exists(out_path))
  patched <- readLines(out_path)

  ## (1) SeverityCalibrationFactor scalar line replaced
  sev_line <- grep("^SeverityCalibrationFactor", patched, value = TRUE)
  expect_length(sev_line, 1L)
  expect_match(sev_line, "SeverityCalibrationFactor\\s+1\\.7")

  ## (2) FireSizesTable: first data row should have SpHiProp/SumHiProp/FallHiProp
  ##     in columns 8/11/14
  fs_hdr <- grep(">>\\s+Fire Sizes", patched)
  i <- fs_hdr + 1L
  while (grepl("^[[:space:]]*>>", patched[i]) || !nzchar(trimws(patched[i]))) {
    i <- i + 1L
  }
  parts <- strsplit(trimws(patched[i]), "\\s+")[[1]]
  expect_equal(as.numeric(parts[8L]), 0.42)
  expect_equal(as.numeric(parts[11L]), 0.63)
  expect_equal(as.numeric(parts[14L]), 0.17)

  ## (3) FuelTypeTable: IgnProb (col 4) = clamp(default * multiplier, [0, 1]).
  ## Conifer rows had IgnProb = 1.0 -> 0.8 (cand for Conifer = 0.8; no clamp).
  ## ConiferPlantation row (C6) had 1.0 -> 1.2, clamped to 1.0.
  ## Deciduous row (D1) had 0.5 -> 0.5 (cand = 1.0; no clamp).
  ## Open rows had 1.0 -> 1.5, clamped to 1.0.
  ftt_hdr <- grep("^FuelTypeTable[[:space:]]*$", patched)
  j <- ftt_hdr + 1L
  while (grepl("^[[:space:]]*>>", patched[j]) || !nzchar(trimws(patched[j]))) {
    j <- j + 1L
  }
  ## row 1: Conifer C1 -> IgnProb = 0.8
  row1 <- strsplit(trimws(patched[j]), "\\s+")[[1]]
  expect_equal(row1[2L], "Conifer")
  expect_equal(as.numeric(row1[4L]), 0.8)
  ## look ahead for the ConiferPlantation, Deciduous, and Open rows
  saw_cp <- FALSE
  saw_open <- FALSE
  for (k in seq(j, j + 16L)) {
    if (k > length(patched) || !nzchar(trimws(patched[k]))) {
      break
    }
    pk <- strsplit(trimws(patched[k]), "\\s+")[[1]]
    if (length(pk) >= 4L && identical(pk[2L], "ConiferPlantation")) {
      expect_equal(as.numeric(pk[4L]), 1.0) ## 1.0 * 1.2 -> clamped
      saw_cp <- TRUE
    }
    if (length(pk) >= 4L && identical(pk[2L], "Deciduous")) {
      expect_equal(as.numeric(pk[4L]), 0.5)
    }
    if (length(pk) >= 4L && identical(pk[2L], "Open")) {
      expect_equal(as.numeric(pk[4L]), 1.0) ## 1.0 * 1.5 -> clamped
      saw_open <- TRUE
    }
  }
  expect_true(saw_cp)
  expect_true(saw_open)
  ## All emitted IgnProb values must be parseable and within [0, 1]
  emitted <- numeric()
  for (k in seq(j, j + 16L)) {
    if (k > length(patched) || !nzchar(trimws(patched[k]))) {
      break
    }
    pk <- strsplit(trimws(patched[k]), "\\s+")[[1]]
    if (length(pk) >= 4L) {
      emitted <- c(emitted, as.numeric(pk[4L]))
    }
  }
  expect_true(all(emitted >= 0 & emitted <= 1))
})

test_that("patch_fire_config() rejects par_vec with wrong names", {
  scenario_dir <- withr::local_tempdir()
  fs::file_copy(
    system.file("testdata", "dynamic-fire-sample.txt", package = "landisutils"),
    fs::path(scenario_dir, "dynamic-fire.txt")
  )
  bad <- c(SeverityCalibrationFactor = 1, NotARealParam = 2)
  expect_error(patch_fire_config(scenario_dir, bad))
})

test_that("apply_calibrated_ignprob() multiplies by base-type multipliers", {
  ft <- defaultFuelTypeTable()
  cand <- c(
    SeverityCalibrationFactor = 1,
    SpHiProp = 0,
    SumHiProp = 0,
    FallHiProp = 0,
    IgnProb_Conifer = 0.8,
    IgnProb_ConiferPlantation = 1.2,
    IgnProb_Deciduous = 0.5,
    IgnProb_Slash = 1.5,
    IgnProb_Open = 0.0
  )
  out <- apply_calibrated_ignprob(ft, cand)

  ## row-wise: out$IgnProb == pmin(pmax(ft$IgnProb * multiplier[ft$Base], 0), 1)
  ## Clamping kicks in for ConiferPlantation (1.0 * 1.2 -> 1.0) and Slash
  ## (1.0 * 1.5 -> 1.0); others stay within [0, 1] and clamp is a no-op.
  expect_equal(out$IgnProb[ft$Base == "Conifer"], ft$IgnProb[ft$Base == "Conifer"] * 0.8)
  expect_equal(
    out$IgnProb[ft$Base == "ConiferPlantation"],
    pmin(ft$IgnProb[ft$Base == "ConiferPlantation"] * 1.2, 1)
  )
  expect_equal(out$IgnProb[ft$Base == "Deciduous"], ft$IgnProb[ft$Base == "Deciduous"] * 0.5)
  expect_equal(out$IgnProb[ft$Base == "Slash"], pmin(ft$IgnProb[ft$Base == "Slash"] * 1.5, 1))
  expect_equal(out$IgnProb[ft$Base == "Open"], rep(0, sum(ft$Base == "Open")))
  ## non-IgnProb columns untouched
  expect_equal(out$a, ft$a)
  expect_equal(out$Base, ft$Base)
})

test_that("apply_calibrated_ignprob() clamps to LANDIS-II's [0, 1] range", {
  ## LANDIS-II Dynamic Fire's parser rejects IgnProb outside [0, 1] with
  ## "Value must be between 0 and 1.0", so the helper must clamp.
  ft <- defaultFuelTypeTable()
  cand <- c(
    SeverityCalibrationFactor = 1,
    SpHiProp = 0,
    SumHiProp = 0,
    FallHiProp = 0,
    IgnProb_Conifer = 5, ## way above 1/default_IgnProb
    IgnProb_ConiferPlantation = 5,
    IgnProb_Deciduous = 5,
    IgnProb_Slash = 5,
    IgnProb_Open = 5
  )
  out <- apply_calibrated_ignprob(ft, cand)
  expect_true(all(out$IgnProb >= 0))
  expect_true(all(out$IgnProb <= 1))
  ## upper boundary actually reached
  expect_true(any(out$IgnProb == 1))
})

test_that("apply_calibrated_hi_prop() overwrites the three HiProp columns", {
  fst <- tibble::tibble(
    EcoCode = 1:2,
    EcoName = c("A", "B"),
    SpHiProp = c(0.5, 0.5),
    SumHiProp = c(0.5, 0.5),
    FallHiProp = c(0.5, 0.5),
    NumFires = c(10, 5)
  )
  cand <- c(
    SeverityCalibrationFactor = 1,
    SpHiProp = 0.42,
    SumHiProp = 0.63,
    FallHiProp = 0.17,
    IgnProb_Conifer = 1,
    IgnProb_ConiferPlantation = 1,
    IgnProb_Deciduous = 1,
    IgnProb_Slash = 1,
    IgnProb_Open = 1
  )
  out <- apply_calibrated_hi_prop(fst, cand)
  expect_equal(out$SpHiProp, c(0.42, 0.42))
  expect_equal(out$SumHiProp, c(0.63, 0.63))
  expect_equal(out$FallHiProp, c(0.17, 0.17))
  ## non-HiProp columns untouched
  expect_equal(out$EcoCode, fst$EcoCode)
  expect_equal(out$NumFires, fst$NumFires)
})

test_that("loss_from_stats() Tier 1 returns finite count + size components", {
  ## one fake replicate: 3 sim fires of various sizes over 10 years
  rep1 <- list(
    n_fires_by_year = tibble::tibble(year = 1:10, n_fires = c(0, 1, 0, 0, 1, 0, 1, 0, 0, 0)),
    fire_sizes_ha = c(10, 50, 200)
  )
  observed <- list(
    fru59 = list(
      lambda_obs = 0.3,
      n_fires_by_year = tibble::tibble(year = 1:74, n = sample.int(5, 74, replace = TRUE)),
      fire_sizes_ha = c(1, 5, 20, 80, 300, 1000)
    )
  )
  loss <- loss_from_stats(list(rep1), observed)
  expect_named(loss, c("total", "components", "weights"))
  expect_named(loss$components, c("count", "size", "area_fuel", "severity"))
  expect_true(is.finite(loss$total))
  expect_true(loss$components[["count"]] >= 0)
  expect_true(loss$components[["size"]] >= 0 && loss$components[["size"]] <= 1)
  expect_equal(loss$components[["area_fuel"]], 0)
  expect_equal(loss$components[["severity"]], 0)
})

test_that("bc_fuel_code_to_base() maps the 13 BC FUEL_TYPE_CD levels", {
  m <- bc_fuel_code_to_base()
  expect_length(m, 13L)
  expect_setequal(names(m), as.character(1:13))
  ## Conifers are codes 1-5, 7, 9
  expect_true(all(m[as.character(c(1, 2, 3, 4, 5, 7, 9))] == "Conifer"))
  expect_equal(m[["6"]], "ConiferPlantation")
  expect_equal(m[["8"]], "Deciduous")
  expect_true(is.na(m[["10"]])) ## non-fuel
  expect_equal(m[["11"]], "Open")
  expect_true(all(m[as.character(c(12, 13))] == "Slash"))
})

test_that("save_observed_fire_targets() writes a payload with expected shape", {
  ## Build a tiny synthetic landscape + NFDB.
  ## Raster: 10x10 grid, fuel codes 2 (Conifer C-2) on left half, 8 (Deciduous) on right.
  r <- terra::rast(nrows = 10, ncols = 10, xmin = 0, xmax = 1000, ymin = 0, ymax = 1000)
  terra::values(r) <- rep(c(rep(2L, 5), rep(8L, 5)), 10)

  ## Two synthetic ignition points: one in conifer, one in deciduous.
  pts <- terra::vect(
    data.frame(
      lon = c(250, 750, 300),
      lat = c(500, 500, 200),
      YEAR = c(2010L, 2015L, 2020L),
      SIZE_HA = c(5.0, 100.0, 0.5)
    ),
    geom = c("lon", "lat"),
    crs = terra::crs(r)
  )
  ## One synthetic fire polygon (matches the 100ha fire, deciduous side).
  poly_df <- data.frame(
    geom = "POLYGON ((600 400, 900 400, 900 600, 600 600, 600 400))",
    YEAR = 2015L,
    SIZE_HA = 100.0
  )
  polys <- terra::vect(poly_df$geom, crs = terra::crs(r))
  polys$YEAR <- 2015L
  polys$SIZE_HA <- 100.0

  out_path <- withr::local_tempfile(fileext = ".rds")
  result_path <- save_observed_fire_targets(
    primary_points = pts,
    primary_polys = polys,
    fire_years = 2010L:2020L,
    fuel_types_rast = r,
    path = out_path,
    primary_label = "TEST_REGION"
  )
  expect_true(fs::file_exists(result_path))

  payload <- readRDS(result_path)
  expect_named(
    payload,
    c(
      "primary",
      "secondary",
      "fru59",
      "frt12",
      "fuel_code_to_base",
      "fire_years_range",
      "fire_years",
      "pixel_area_ha",
      "computed_at",
      "notes"
    )
  )
  ## Primary summary checks
  p <- payload$primary
  expect_equal(p$ecoregion, "TEST_REGION")
  expect_equal(p$n_ignitions, 3L)
  expect_equal(p$n_polys, 1L)
  expect_equal(p$lambda_obs, 3 / 11) ## 3 fires over 11 years
  ## Sizes come from the polygons' SIZE_HA when polys are supplied (NBAC's
  ## ADJ_HA is more accurate than NFDB's agency-reported point sizes); the
  ## points' SIZE_HA (5, 100, 0.5) is only used in the no-polys fallback
  ## case -- covered by the dedicated test below.
  expect_equal(p$fire_sizes_ha, sort(c(100.0)))
  ## area_by_fuel_ha: polygon overlaps deciduous cells only (codes 8)
  expect_s3_class(p$area_by_fuel_ha, "tbl_df")
  expect_true("Deciduous" %in% p$area_by_fuel_ha$base)

  ## Secondary stays NULL when no secondary inputs supplied
  expect_null(payload$secondary)
  expect_null(payload$frt12)
  ## Aliases populated
  expect_identical(payload$fru59, payload$primary)
})

test_that("save_observed_fire_targets() falls back to points' SIZE_HA when polys are not supplied", {
  ## Same synthetic landscape as the main payload-shape test, but with NO
  ## polygon input -- exercises the fallback branch in `.summarise()`.
  r <- terra::rast(nrows = 10, ncols = 10, xmin = 0, xmax = 1000, ymin = 0, ymax = 1000)
  terra::values(r) <- rep(c(rep(2L, 5), rep(8L, 5)), 10)
  pts <- terra::vect(
    data.frame(
      lon = c(250, 750, 300),
      lat = c(500, 500, 200),
      YEAR = c(2010L, 2015L, 2020L),
      SIZE_HA = c(5.0, 100.0, 0.5)
    ),
    geom = c("lon", "lat"),
    crs = terra::crs(r)
  )

  out_path <- withr::local_tempfile(fileext = ".rds")
  result_path <- save_observed_fire_targets(
    primary_points = pts,
    primary_polys = NULL,
    fire_years = 2010L:2020L,
    fuel_types_rast = r,
    path = out_path,
    primary_label = "TEST_REGION"
  )
  payload <- readRDS(result_path)
  p <- payload$primary
  expect_equal(p$n_polys, 0L)
  expect_equal(p$fire_sizes_ha, sort(c(0.5, 5.0, 100.0)))
  expect_null(p$area_by_fuel_ha) ## skipped when no polys are supplied
})

test_that("save_observed_fire_targets() accepts a custom fuel_code_to_base mapping", {
  r <- terra::rast(nrows = 5, ncols = 5, xmin = 0, xmax = 500, ymin = 0, ymax = 500)
  terra::values(r) <- 1L ## all the same custom code
  pts <- terra::vect(
    data.frame(lon = 250, lat = 250, YEAR = 2010L, SIZE_HA = 1.0),
    geom = c("lon", "lat"),
    crs = terra::crs(r)
  )
  polys <- terra::vect(
    "POLYGON ((100 100, 400 100, 400 400, 100 400, 100 100))",
    crs = terra::crs(r)
  )
  polys$YEAR <- 2010L
  polys$SIZE_HA <- 9.0
  custom_map <- c("1" = "Open") ## custom: code 1 -> Open
  out_path <- withr::local_tempfile(fileext = ".rds")
  save_observed_fire_targets(
    primary_points = pts,
    primary_polys = polys,
    fire_years = 2010L:2015L,
    fuel_types_rast = r,
    path = out_path,
    fuel_code_to_base = custom_map
  )
  payload <- readRDS(out_path)
  expect_equal(payload$primary$area_by_fuel_ha$base, "Open")
  expect_equal(payload$fuel_code_to_base, custom_map)
})

test_that(".patch_forcs_for_calibration() rewrites Timestep + SpinUp", {
  fixture <- system.file("testdata", "forc-succession-sample.txt", package = "landisutils")
  tmp <- withr::local_tempfile(fileext = ".txt")
  fs::file_copy(fixture, tmp)
  landisutils:::.patch_forcs_for_calibration(tmp, sim_years = 10L)
  patched <- readLines(tmp)

  ts_line <- grep("^Timestep[[:space:]]", patched, value = TRUE)
  expect_length(ts_line, 1L)
  ## sim_years + 1 = 11
  expect_match(ts_line, "Timestep\\s+11")

  sp_idx <- grep("^SpinUp[[:space:]]*$", patched)
  data_idx <- sp_idx + 1L
  while (grepl("^[[:space:]]*>>", patched[data_idx])) {
    data_idx <- data_idx + 1L
  }
  ## SpinUp row patched to enable DOM spinup (Flag = 1) while keeping
  ## biomass spinup OFF (BiomassSpinUpFlag = 0): the snapshot IC has the
  ## cohort biomass already, but DOM pools must be equilibrated each run
  ## or Dynamic Fire's CohortMortality handler hits an NRE in ForCS's
  ## Soil.cs:DisturbanceImpactsBiomass (v0.0.28).
  expect_equal(trimws(patched[data_idx]), "1  0  1  20")
})

test_that(".patch_forcs_for_calibration() errors when expected sections are missing", {
  tmp <- withr::local_tempfile(fileext = ".txt")
  writeLines(c("LandisData  \"ForC Succession\"", ""), tmp)
  expect_error(landisutils:::.patch_forcs_for_calibration(tmp, sim_years = 10L), "Timestep")
})

test_that("write_landis_scenario_file() writes a syntactically clean scenario.txt", {
  dir <- withr::local_tempdir()
  ## minimum-viable fake inputs (paths only -- the writer doesn't validate file
  ## contents, just relativises and references them)
  species <- fs::path(dir, "species.txt")
  fs::file_create(species)
  eco_txt <- fs::path(dir, "ecoregions.txt")
  eco_tif <- fs::path(dir, "ecoregions.tif")
  fs::file_create(eco_txt)
  fs::file_create(eco_tif)
  forcs <- fs::path(dir, "forc-succession.txt")
  fs::file_create(forcs)
  fire <- fs::path(dir, "dynamic-fire.txt")
  fs::file_create(fire)

  out <- write_landis_scenario_file(
    path = dir,
    duration = 5L,
    cell_length = 100L,
    species_file = species,
    ecoregions_files = c(eco_txt, eco_tif),
    succession_ext_files = c("ForC Succession" = forcs),
    disturbance_ext_files = c("Dynamic Fire System" = fire),
    other_ext_files = NULL,
    output_manifest = c("fire/dynamic-fire-event-log.csv")
  )
  expect_true(fs::file_exists(out))
  lines <- readLines(out)
  ## Sections we expect
  expect_true(any(grepl('LandisData\\s+"Scenario"', lines)))
  expect_true(any(grepl("^Duration\\s+5", lines)))
  expect_true(any(grepl("^CellLength\\s+100", lines)))
  expect_true(any(grepl('"ForC Succession"\\s+forc-succession.txt', lines)))
  expect_true(any(grepl('"Dynamic Fire System"\\s+dynamic-fire.txt', lines)))
  ## RandomNumberSeed is commented (allows per-rep seed override at run time)
  expect_true(any(grepl("^>>\\s+RandomNumberSeed", lines)))

  ## output_manifest.txt is written alongside
  manifest_path <- fs::path(dir, "output_manifest.txt")
  expect_true(fs::file_exists(manifest_path))
  expect_equal(readLines(manifest_path), "fire/dynamic-fire-event-log.csv")
})

test_that("sim_mock() returns parse_dynamic_fire_logs()-shaped output", {
  cand <- c(
    SeverityCalibrationFactor = 1.2,
    SpHiProp = 0.4,
    SumHiProp = 0.6,
    FallHiProp = 0.2,
    IgnProb_Conifer = 1,
    IgnProb_ConiferPlantation = 1,
    IgnProb_Deciduous = 1,
    IgnProb_Slash = 1,
    IgnProb_Open = 1
  )
  out <- sim_mock(par_vec = cand, sim_years = 5L, base_seed = 42L)
  expect_named(
    out,
    c("n_fires_by_year", "fire_sizes_ha", "events", "total_sites_burned", "n_events")
  )
  expect_s3_class(out$n_fires_by_year, "tbl_df")
  expect_equal(nrow(out$n_fires_by_year), 5L)
  expect_true(is.numeric(out$fire_sizes_ha))
  ## deterministic given the seed
  out2 <- sim_mock(par_vec = cand, sim_years = 5L, base_seed = 42L)
  expect_equal(out$fire_sizes_ha, out2$fire_sizes_ha)
})

test_that("calibrate_dynamic_fire() runs end-to-end with sim_mock (no Docker)", {
  skip_if_not_installed("DEoptim")
  observed <- list(
    primary = list(
      lambda_obs = 4,
      n_fires_by_year = tibble::tibble(year = 1:20, n = sample.int(8, 20, replace = TRUE)),
      fire_sizes_ha = sort(stats::rlnorm(50, 3, 2))
    ),
    secondary = NULL
  )
  observed$fru59 <- observed$primary ## back-compat alias for loss_from_stats()
  obs_path <- withr::local_tempfile(fileext = ".rds")
  saveRDS(observed, obs_path)

  ## Minimal scenario.txt fixture for the path check inside calibrate_dynamic_fire()
  scen_dir <- withr::local_tempdir()
  scen_txt <- fs::path(scen_dir, "scenario.txt")
  writeLines(c("LandisData  \"Scenario\""), scen_txt)
  out_dir <- withr::local_tempdir()

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
    NP = 6L,
    itermax = 2L,
    n_reps = 1L,
    sim_years = 5L,
    weights = c(count = 1, size = 1, area_fuel = 0, severity = 0),
    n_cores = 1L,
    parallel = FALSE,
    simulator = "mock",
    base_seed = 12345L,
    trace = FALSE
  )

  ## NP = 6 (set small above to keep this test fast) is below DEoptim's recommended 10 * npar, so
  ## DEoptim emits its standard advisory warning. Assert it explicitly so the expectation is encoded
  ## (and the suite stays warning-clean) rather than letting it bubble up as an uncaught test warning.
  expect_warning(
    res <- calibrate_dynamic_fire(
      observed_targets_path = obs_path,
      scenario_template = scen_txt,
      cfg = cfg,
      out_dir = out_dir
    ),
    regexp = "ten times the length of the parameter vector"
  )

  expect_named(
    res,
    c(
      "best_params",
      "objective",
      "deoptim",
      "trace_path",
      "trial_trace_path",
      "cfg",
      "pool_image",
      "pool_digest"
    )
  )
  expect_equal(length(res$best_params), 9L)
  expect_setequal(names(res$best_params), calibration_par_names())
  expect_true(is.finite(res$objective))
  expect_true(fs::file_exists(res$trace_path))
  ## No pool was started (mock simulator)
  expect_true(is.na(res$pool_image))
})

test_that("calibrate_dynamic_fire() forwards reltol/steptol to DEoptim.control", {
  skip_if_not_installed("DEoptim")
  observed <- list(
    primary = list(
      lambda_obs = 4,
      n_fires_by_year = tibble::tibble(year = 1:20, n = sample.int(8, 20, replace = TRUE)),
      fire_sizes_ha = sort(stats::rlnorm(50, 3, 2))
    ),
    secondary = NULL
  )
  observed$fru59 <- observed$primary
  obs_path <- withr::local_tempfile(fileext = ".rds")
  saveRDS(observed, obs_path)

  scen_dir <- withr::local_tempdir()
  scen_txt <- fs::path(scen_dir, "scenario.txt")
  writeLines(c("LandisData  \"Scenario\""), scen_txt)

  base_cfg <- list(
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
    NP = 6L,
    itermax = 2L,
    n_reps = 1L,
    sim_years = 5L,
    weights = c(count = 1, size = 1, area_fuel = 0, severity = 0),
    n_cores = 1L,
    parallel = FALSE,
    simulator = "mock",
    base_seed = 12345L,
    trace = FALSE
  )

  ## Default path: cfg omits reltol/steptol -> defaults applied (reltol = 1e-3,
  ## steptol = 25). Verify via the startup-message tag rather than try to coax
  ## a stochastic mock objective into triggering steptol.
  expect_message(
    suppressWarnings(calibrate_dynamic_fire(
      observed_targets_path = obs_path,
      scenario_template = scen_txt,
      cfg = base_cfg,
      out_dir = withr::local_tempdir()
    )),
    regexp = "reltol=0\\.001.*steptol=25"
  )

  ## Caller overrides flow through verbatim.
  cfg_custom <- c(base_cfg, list(reltol = 0.05, steptol = 7L))
  expect_message(
    suppressWarnings(calibrate_dynamic_fire(
      observed_targets_path = obs_path,
      scenario_template = scen_txt,
      cfg = cfg_custom,
      out_dir = withr::local_tempdir()
    )),
    regexp = "reltol=0\\.05.*steptol=7"
  )
})

test_that("calibrate_dynamic_fire() rejects unknown simulator names", {
  skip_if_not_installed("DEoptim")
  observed <- list(
    primary = list(
      lambda_obs = 1,
      n_fires_by_year = tibble::tibble(year = 1:5, n = 1L:5L),
      fire_sizes_ha = c(1, 2, 3)
    ),
    fru59 = list(
      lambda_obs = 1,
      n_fires_by_year = tibble::tibble(year = 1:5, n = 1L:5L),
      fire_sizes_ha = c(1, 2, 3)
    )
  )
  obs_path <- withr::local_tempfile(fileext = ".rds")
  saveRDS(observed, obs_path)
  scen_dir <- withr::local_tempdir()
  scen_txt <- fs::path(scen_dir, "scenario.txt")
  writeLines("x", scen_txt)
  out_dir <- withr::local_tempdir()
  cfg <- list(
    lower = setNames(rep(0, 9), calibration_par_names()),
    upper = setNames(rep(1, 9), calibration_par_names()),
    NP = 4L,
    itermax = 1L,
    n_reps = 1L,
    sim_years = 2L,
    n_cores = 1L,
    parallel = FALSE,
    simulator = "not-a-simulator",
    base_seed = 1L
  )
  expect_error(calibrate_dynamic_fire(obs_path, scen_txt, cfg, out_dir), "Unknown simulator")
})

test_that("loss_from_stats() handles empty-fires reps with a finite penalty", {
  rep_empty <- list(
    n_fires_by_year = tibble::tibble(year = 1:10, n_fires = rep(0L, 10L)),
    fire_sizes_ha = numeric(0)
  )
  observed <- list(
    fru59 = list(
      lambda_obs = 8.23,
      n_fires_by_year = tibble::tibble(year = 1:74, n = sample.int(20, 74, replace = TRUE)),
      fire_sizes_ha = c(1, 5, 20, 80, 300)
    )
  )
  loss <- loss_from_stats(list(rep_empty), observed)
  expect_true(is.finite(loss$total))
  expect_equal(loss$components[["size"]], 1.0)
})

## ---- Tier 2 ----------------------------------------------------------------

test_that("default_severity_prior_sturtevant2009() returns a 5-element vector that sums to 1", {
  p <- default_severity_prior_sturtevant2009()
  expect_named(p, c("1", "2", "3", "4", "5"))
  expect_equal(sum(p), 1, tolerance = 1e-9)
  expect_true(all(p > 0))
})

test_that("L_area_fuel activates when observed has area_by_fuel_ha + fuel_code_to_base", {
  rep1 <- list(
    n_fires_by_year = tibble::tibble(year = 1:10, n_fires = c(0, 1, 1, 0, 1, 0, 0, 1, 0, 0)),
    fire_sizes_ha = c(10, 50, 200, 100),
    events = tibble::tibble(
      year = c(2L, 3L, 5L, 8L),
      eco = "MOCK",
      init_fuel = c(2L, 2L, 8L, 2L), ## 3x Conifer (C-2), 1x Deciduous (D-1/2)
      sites = c(10L, 50L, 200L, 100L),
      mean_severity = c(2.0, 3.0, 4.0, 1.0)
    )
  )
  observed <- list(
    primary = list(
      lambda_obs = 4,
      n_fires_by_year = tibble::tibble(year = 1:74, n = sample.int(8, 74, replace = TRUE)),
      fire_sizes_ha = sort(stats::rlnorm(50, 3, 2)),
      area_by_fuel_ha = tibble::tibble(
        base = c("Conifer", "Deciduous"),
        area_ha = c(150, 50),
        cells = c(150L, 50L)
      ),
      severity_dist = NULL
    ),
    fuel_code_to_base = bc_fuel_code_to_base(),
    pixel_area_ha = 1.0
  )
  loss <- loss_from_stats(list(rep1), observed, weights = c(area_fuel = 1))
  expect_true(is.finite(loss$components[["area_fuel"]]))
  expect_true(loss$components[["area_fuel"]] >= 0)
  expect_equal(loss$total, loss$components[["area_fuel"]])
})

test_that("L_area_fuel contributes 0 when fuel_code_to_base is missing", {
  rep1 <- list(
    n_fires_by_year = tibble::tibble(year = 1:5, n_fires = c(0, 1, 0, 0, 0)),
    fire_sizes_ha = 10,
    events = tibble::tibble(year = 2L, eco = "X", init_fuel = 2L, sites = 10L, mean_severity = 2.0)
  )
  observed <- list(
    primary = list(
      lambda_obs = 1,
      n_fires_by_year = tibble::tibble(year = 1:5, n = 1L:5L),
      fire_sizes_ha = c(1, 5),
      area_by_fuel_ha = tibble::tibble(base = "Conifer", area_ha = 10, cells = 10L)
    )
    ## fuel_code_to_base intentionally absent
  )
  loss <- loss_from_stats(list(rep1), observed, weights = c(area_fuel = 1))
  expect_equal(loss$components[["area_fuel"]], 0)
})

test_that("L_severity activates when observed$primary$severity_dist is non-NULL", {
  rep1 <- list(
    n_fires_by_year = tibble::tibble(year = 1:5, n_fires = c(0, 2, 0, 1, 0)),
    fire_sizes_ha = c(20, 30, 80),
    events = tibble::tibble(
      year = c(2L, 2L, 4L),
      eco = "MOCK",
      init_fuel = 2L,
      sites = c(20L, 30L, 80L),
      mean_severity = c(1.0, 3.0, 5.0)
    )
  )
  observed <- list(
    primary = list(
      lambda_obs = 1,
      n_fires_by_year = tibble::tibble(year = 1:74, n = sample.int(5, 74, replace = TRUE)),
      fire_sizes_ha = c(5, 10, 30, 60, 120),
      severity_dist = default_severity_prior_sturtevant2009()
    )
  )
  loss <- loss_from_stats(list(rep1), observed, weights = c(severity = 1))
  expect_true(is.finite(loss$components[["severity"]]))
  expect_true(loss$components[["severity"]] >= 0)
})

test_that("L_severity contributes 0 when severity_dist is NULL", {
  rep1 <- list(
    n_fires_by_year = tibble::tibble(year = 1:3, n_fires = c(0, 1, 0)),
    fire_sizes_ha = 50,
    events = tibble::tibble(year = 2L, eco = "X", init_fuel = 2L, sites = 50L, mean_severity = 3.0)
  )
  observed <- list(
    primary = list(
      lambda_obs = 0.3,
      n_fires_by_year = tibble::tibble(year = 1:3, n = c(0L, 1L, 0L)),
      fire_sizes_ha = c(5, 10, 30)
      ## severity_dist absent (effectively NULL)
    )
  )
  loss <- loss_from_stats(list(rep1), observed, weights = c(severity = 1))
  expect_equal(loss$components[["severity"]], 0)
})

## ---- 9c: pre-flight checks -------------------------------------------------

## Helper: build a minimal valid template_dir for pre-flight tests.
.make_min_template_dir <- function() {
  dir <- withr::local_tempdir(.local_envir = parent.frame())
  files <- c(
    "scenario.txt",
    "forc-succession.txt",
    "dynamic-fire.txt",
    "dynamic-fuels.txt",
    "species.txt",
    "ecoregions.txt",
    "ecoregions.tif",
    "initial-communities.csv",
    "initial-communities.tif",
    "ground_slope.tif",
    "uphill_slope_azimuth.tif",
    "fire-ecoregions.tif",
    "initial_weather_database.csv",
    "DynamicFire_Spp_Table.csv"
  )
  for (f in files) {
    fs::file_create(fs::path(dir, f))
  }
  dir
}

.make_min_observed <- function() {
  list(
    primary = list(
      lambda_obs = 8.23,
      n_fires_by_year = tibble::tibble(year = 1:74, n = sample.int(20, 74, replace = TRUE)),
      fire_sizes_ha = sort(stats::rlnorm(50, 3, 2))
    ),
    fru59 = NULL ## set below
  )
}

.make_default_cfg <- function() {
  list(
    lower = stats::setNames(rep(0, 9), calibration_par_names()),
    upper = stats::setNames(rep(1, 9), calibration_par_names()),
    NP = 90L, ## = 10 * 9 to avoid the NP-advisory message in expect_silent()
    itermax = 10L,
    n_reps = 1L,
    sim_years = 5L,
    weights = c(count = 1, size = 1, area_fuel = 0, severity = 0),
    simulator = "mock",
    method = "local",
    n_cores = 1L,
    parallel = FALSE,
    base_seed = 1L,
    trace = FALSE
  )
}

test_that("pre-flight: lower >= upper errors with the offending parameter names", {
  cfg <- .make_default_cfg()
  cfg$lower[["SpHiProp"]] <- 0.8
  cfg$upper[["SpHiProp"]] <- 0.5
  expect_error(
    landisutils:::.preflight_calibrate(
      cfg = cfg,
      par_names = calibration_par_names(),
      template_dir = .make_min_template_dir(),
      observed = .make_min_observed(),
      scratch_root = withr::local_tempdir()
    ),
    "SpHiProp"
  )
})

test_that("pre-flight: NP < 4 errors", {
  cfg <- .make_default_cfg()
  cfg$NP <- 3L
  expect_error(
    landisutils:::.preflight_calibrate(
      cfg = cfg,
      par_names = calibration_par_names(),
      template_dir = .make_min_template_dir(),
      observed = .make_min_observed(),
      scratch_root = withr::local_tempdir()
    ),
    "NP must be >= 4"
  )
})

test_that("pre-flight: all-zero weights errors", {
  cfg <- .make_default_cfg()
  cfg$weights <- c(count = 0, size = 0, area_fuel = 0, severity = 0)
  expect_error(
    landisutils:::.preflight_calibrate(
      cfg = cfg,
      par_names = calibration_par_names(),
      template_dir = .make_min_template_dir(),
      observed = .make_min_observed(),
      scratch_root = withr::local_tempdir()
    ),
    "all zero"
  )
})

test_that("pre-flight: missing scenario template files surfaces a clear error", {
  cfg <- .make_default_cfg()
  bad_dir <- withr::local_tempdir()
  ## intentionally don't populate with the required files
  expect_error(
    landisutils:::.preflight_calibrate(
      cfg = cfg,
      par_names = calibration_par_names(),
      template_dir = bad_dir,
      observed = .make_min_observed(),
      scratch_root = withr::local_tempdir()
    ),
    "missing required files"
  )
})

test_that("pre-flight: observed payload missing $primary errors", {
  cfg <- .make_default_cfg()
  bad_obs <- list(some_other_key = TRUE)
  expect_error(
    landisutils:::.preflight_calibrate(
      cfg = cfg,
      par_names = calibration_par_names(),
      template_dir = .make_min_template_dir(),
      observed = bad_obs,
      scratch_root = withr::local_tempdir()
    ),
    "primary"
  )
})

test_that("pre-flight: severity weight > 0 with NULL severity_dist warns", {
  cfg <- .make_default_cfg()
  cfg$weights <- c(count = 1, size = 1, area_fuel = 0, severity = 1)
  expect_warning(
    landisutils:::.preflight_calibrate(
      cfg = cfg,
      par_names = calibration_par_names(),
      template_dir = .make_min_template_dir(),
      observed = .make_min_observed(),
      scratch_root = withr::local_tempdir()
    ),
    "severity_dist is NULL"
  )
})

test_that("pre-flight: passes cleanly with a fully-populated minimal config", {
  cfg <- .make_default_cfg()
  expect_silent(landisutils:::.preflight_calibrate(
    cfg = cfg,
    par_names = calibration_par_names(),
    template_dir = .make_min_template_dir(),
    observed = .make_min_observed(),
    scratch_root = withr::local_tempdir()
  ))
})

## ---- 9e: override slots on build_calibration_scenario_template -------------

test_that("build_calibration_scenario_template() overrides replace specific template files", {
  template_dir <- withr::local_tempdir()
  template_files <- c(
    "scenario.txt",
    "forc-succession.txt",
    "dynamic-fire.txt",
    "dynamic-fuels.txt",
    "species.txt",
    "ecoregions.txt",
    "ecoregions.tif",
    "initial-communities.csv",
    "initial-communities.tif",
    "ground_slope.tif",
    "uphill_slope_azimuth.tif",
    "fire-ecoregions.tif",
    "initial_weather_database.csv",
    "DynamicFire_Spp_Table.csv"
  )
  for (f in template_files) {
    writeLines("template-content", fs::path(template_dir, f))
  }
  ## forc-succession.txt needs SpinUp + Timestep lines for the patcher
  writeLines(
    c(
      "LandisData  \"ForC Succession\"",
      "Timestep    1",
      "SpinUp",
      ">>  On/Off  Biomass    Tolerance  Max",
      ">>  Flag    Spin-up    %          Iterations",
      ">>          Flag",
      ">>  ----------------------------------------",
      "1  1  1  20"
    ),
    fs::path(template_dir, "forc-succession.txt")
  )

  snap_csv <- withr::local_tempfile(fileext = ".csv")
  writeLines("snap-csv-content", snap_csv)
  snap_tif <- withr::local_tempfile(fileext = ".tif")
  writeLines("snap-tif-content", snap_tif)

  override_slope <- withr::local_tempfile(fileext = ".tif")
  writeLines("CUSTOM-SLOPE-CONTENT", override_slope)

  out_dir <- withr::local_tempdir()
  build_calibration_scenario_template(
    out_dir = out_dir,
    template_dir = template_dir,
    snapshot_ic_csv = snap_csv,
    snapshot_ic_tif = snap_tif,
    sim_years = 5L,
    cell_length = 100L,
    overrides = list(ground_slope.tif = override_slope)
  )

  expect_equal(readLines(fs::path(out_dir, "ground_slope.tif")), "CUSTOM-SLOPE-CONTENT")
  expect_equal(readLines(fs::path(out_dir, "uphill_slope_azimuth.tif")), "template-content")
})

test_that("build_calibration_scenario_template() rejects unknown override keys", {
  template_dir <- withr::local_tempdir()
  for (f in c("scenario.txt", "forc-succession.txt", "ground_slope.tif")) {
    writeLines("x", fs::path(template_dir, f))
  }
  snap_csv <- withr::local_tempfile(fileext = ".csv")
  writeLines("x", snap_csv)
  snap_tif <- withr::local_tempfile(fileext = ".tif")
  writeLines("x", snap_tif)
  expect_error(
    build_calibration_scenario_template(
      out_dir = withr::local_tempdir(),
      template_dir = template_dir,
      snapshot_ic_csv = snap_csv,
      snapshot_ic_tif = snap_tif,
      cell_length = 100L,
      overrides = list(notarealfile.tif = "/tmp/whatever")
    ),
    "Unknown override target"
  )
})

test_that("build_calibration_scenario_template() rejects missing override source files", {
  template_dir <- withr::local_tempdir()
  for (f in c("scenario.txt", "forc-succession.txt", "ground_slope.tif")) {
    writeLines("x", fs::path(template_dir, f))
  }
  snap_csv <- withr::local_tempfile(fileext = ".csv")
  writeLines("x", snap_csv)
  snap_tif <- withr::local_tempfile(fileext = ".tif")
  writeLines("x", snap_tif)
  expect_error(
    build_calibration_scenario_template(
      out_dir = withr::local_tempdir(),
      template_dir = template_dir,
      snapshot_ic_csv = snap_csv,
      snapshot_ic_tif = snap_tif,
      cell_length = 100L,
      overrides = list(ground_slope.tif = "/no/such/file.tif")
    ),
    "Override for"
  )
})

test_that("save_observed_fire_targets() stores severity_dist on primary when supplied", {
  r <- terra::rast(nrows = 5, ncols = 5, xmin = 0, xmax = 500, ymin = 0, ymax = 500)
  terra::values(r) <- 2L ## Conifer C-2
  pts <- terra::vect(
    data.frame(lon = 250, lat = 250, YEAR = 2010L, SIZE_HA = 1.0),
    geom = c("lon", "lat"),
    crs = terra::crs(r)
  )
  polys <- terra::vect(
    "POLYGON ((100 100, 400 100, 400 400, 100 400, 100 100))",
    crs = terra::crs(r)
  )
  polys$YEAR <- 2010L
  polys$SIZE_HA <- 9.0
  out_path <- withr::local_tempfile(fileext = ".rds")
  save_observed_fire_targets(
    primary_points = pts,
    primary_polys = polys,
    fire_years = 2010L:2015L,
    fuel_types_rast = r,
    path = out_path,
    severity_dist = default_severity_prior_sturtevant2009()
  )
  payload <- readRDS(out_path)
  expect_equal(payload$primary$severity_dist, default_severity_prior_sturtevant2009())
})

test_that(".calibration_succession_backend detects ForCS vs Biomass Succession", {
  d <- withr::local_tempdir()
  ## Biomass Succession backend
  writeLines("LandisData  \"Biomass Succession\"", fs::path(d, "biomass-succession.txt"))
  bk <- .calibration_succession_backend(d)
  expect_equal(bk$name, "Biomass Succession")
  expect_equal(bk$file, "biomass-succession.txt")
  expect_length(bk$logs, 0L) ## no fixed-name succession logs needed for Biomass

  ## ForCS backend (takes precedence + carries the ForCS logs)
  writeLines("LandisData  \"ForC Succession\"", fs::path(d, "forc-succession.txt"))
  bk2 <- .calibration_succession_backend(d)
  expect_equal(bk2$name, "ForC Succession")
  expect_true("log_Summary.csv" %in% bk2$logs)
})

test_that(".calibration_succession_backend errors when no succession config is present", {
  ## regexp (not snapshot): the error message embeds the temp dir path, which is non-deterministic.
  d <- withr::local_tempdir()
  expect_error(.calibration_succession_backend(d), "no recognised succession config")
})

test_that(".patch_biomass_for_calibration freezes the succession Timestep past sim_years", {
  f <- fs::path(withr::local_tempdir(), "biomass-succession.txt")
  writeLines(
    c(
      "LandisData  \"Biomass Succession\"",
      "",
      "Timestep    20",
      "",
      "SeedingAlgorithm  WardSeedDispersal"
    ),
    f
  )
  .patch_biomass_for_calibration(f, sim_years = 10L)
  ts <- grep("^Timestep", readLines(f), value = TRUE)
  expect_equal(ts, "Timestep    11") ## sim_years + 1 -> succession never executes during calibration
})

test_that(".calibration_species_file() reads the scenario's Species directive", {
  dir <- withr::local_tempdir()
  writeLines(
    c(
      "LandisData Scenario",
      "Species    species-core.txt  >> the core species file",
      "Duration 10"
    ),
    fs::path(dir, "scenario.txt")
  )
  expect_equal(.calibration_species_file(dir), fs::path(dir, "species-core.txt"))
})

test_that(".calibration_species_file() falls back to species.txt when the directive/file is absent", {
  dir <- withr::local_tempdir()
  writeLines(c("LandisData Scenario", "Duration 10"), fs::path(dir, "scenario.txt"))
  expect_equal(.calibration_species_file(dir), fs::path(dir, "species.txt"))
  dir2 <- withr::local_tempdir() ## no scenario.txt at all
  expect_equal(.calibration_species_file(dir2), fs::path(dir2, "species.txt"))
})

test_that(".calibration_directive_file() reads a directive from any config + strips comments", {
  dir <- withr::local_tempdir()
  writeLines(
    c(
      "LandisData \"Dynamic Fire System\"",
      "InitialWeatherDatabase    initial-weather-database.csv  >> hyphenated names",
      "Species_CSV_File    dynamic-fire-species.csv"
    ),
    fs::path(dir, "dynamic-fire.txt")
  )
  expect_equal(
    .calibration_directive_file(
      dir,
      "dynamic-fire.txt",
      "InitialWeatherDatabase",
      "initial_weather_database.csv"
    ),
    fs::path(dir, "initial-weather-database.csv")
  )
  expect_equal(
    .calibration_directive_file(
      dir,
      "dynamic-fire.txt",
      "Species_CSV_File",
      "DynamicFire_Spp_Table.csv"
    ),
    fs::path(dir, "dynamic-fire-species.csv")
  )
})

test_that(".calibration_directive_file() falls back to the default when directive/config absent", {
  dir <- withr::local_tempdir()
  writeLines("LandisData \"Dynamic Fire System\"", fs::path(dir, "dynamic-fire.txt"))
  expect_equal(
    .calibration_directive_file(
      dir,
      "dynamic-fire.txt",
      "InitialWeatherDatabase",
      "initial_weather_database.csv"
    ),
    fs::path(dir, "initial_weather_database.csv")
  )
  dir2 <- withr::local_tempdir() ## no config file at all
  expect_equal(
    .calibration_directive_file(
      dir2,
      "dynamic-fire.txt",
      "InitialWeatherDatabase",
      "initial_weather_database.csv"
    ),
    fs::path(dir2, "initial_weather_database.csv")
  )
})

test_that(".calibration_required_files() resolves backend/scenario names (Biomass Succession)", {
  dir <- withr::local_tempdir()
  writeLines(
    c("LandisData Scenario", "   Species   species-core.txt"),
    fs::path(dir, "scenario.txt")
  )
  writeLines("LandisData \"Biomass Succession\"", fs::path(dir, "biomass-succession.txt"))
  writeLines(
    c(
      "LandisData \"Dynamic Fire System\"",
      "InitialWeatherDatabase   initial-weather-database.csv",
      "Species_CSV_File   dynamic-fire-species.csv"
    ),
    fs::path(dir, "dynamic-fire.txt")
  )
  expect_setequal(
    .calibration_required_files(dir, "landis"),
    c(
      "scenario.txt",
      "biomass-succession.txt",
      "dynamic-fire.txt",
      "dynamic-fuels.txt",
      "species-core.txt",
      "ecoregions.txt",
      "ecoregions.tif",
      "initial-communities.csv",
      "initial-communities.tif",
      "ground_slope.tif",
      "uphill_slope_azimuth.tif",
      "fire-ecoregions.tif",
      "initial-weather-database.csv",
      "dynamic-fire-species.csv"
    )
  )
})

test_that(".calibration_required_files() keeps the legacy names (ForC Succession, defaults)", {
  dir <- withr::local_tempdir()
  writeLines(c("LandisData Scenario", "   Species   species.txt"), fs::path(dir, "scenario.txt"))
  writeLines("LandisData \"ForC Succession\"", fs::path(dir, "forc-succession.txt"))
  writeLines("LandisData \"Dynamic Fire System\"", fs::path(dir, "dynamic-fire.txt"))
  expect_setequal(
    .calibration_required_files(dir, "landis"),
    c(
      "scenario.txt",
      "forc-succession.txt",
      "dynamic-fire.txt",
      "dynamic-fuels.txt",
      "species.txt",
      "ecoregions.txt",
      "ecoregions.tif",
      "initial-communities.csv",
      "initial-communities.tif",
      "ground_slope.tif",
      "uphill_slope_azimuth.tif",
      "fire-ecoregions.tif",
      "initial_weather_database.csv",
      "DynamicFire_Spp_Table.csv"
    )
  )
})

test_that(".calibration_required_files() needs only scenario.txt for non-landis simulators", {
  expect_equal(.calibration_required_files(withr::local_tempdir(), "mock"), "scenario.txt")
})

test_that(".mem_limit_to_gb() parses docker --memory strings", {
  expect_equal(.mem_limit_to_gb("8g"), 8)
  expect_equal(.mem_limit_to_gb("16gib"), 16)
  expect_equal(.mem_limit_to_gb("512m"), 0.5)
  expect_identical(.mem_limit_to_gb(NULL), NA_real_)
  expect_identical(.mem_limit_to_gb(""), NA_real_)
})

test_that(".ram_pool_cap() caps the pool by the RAM budget", {
  ## district-scale worker (22 GiB) on a 1007 GiB host: floor(1007 * 0.85 / 22) = 38
  expect_equal(.ram_pool_cap(90, mem_per_worker_gb = 22, mem_fraction = 0.85, avail_gb = 1007), 38L)
  ## small worker (8 GiB) leaves headroom -> requested count unchanged
  expect_equal(.ram_pool_cap(90, mem_per_worker_gb = 8, mem_fraction = 0.85, avail_gb = 1007), 90L)
  ## the floor never drops below one container
  expect_equal(.ram_pool_cap(90, mem_per_worker_gb = 2000, mem_fraction = 0.85, avail_gb = 100), 1L)
})

test_that(".ram_pool_cap() is a no-op when RAM or the per-worker estimate is unknown", {
  expect_equal(.ram_pool_cap(90, mem_per_worker_gb = NA_real_, avail_gb = 1007), 90L)
  expect_equal(.ram_pool_cap(90, mem_per_worker_gb = 22, avail_gb = NA_real_), 90L)
  expect_equal(.ram_pool_cap(90, mem_per_worker_gb = 0, avail_gb = 1007), 90L)
})
