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
    test_path("..", "..", "inst", "testdata", "dynamic-fire-event-log-sample.csv"),
    fs::path(rep_dir, "fire", "dynamic-fire-event-log.csv")
  )
  fs::file_copy(
    test_path("..", "..", "inst", "testdata", "dynamic-fire-summary-log-sample.csv"),
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
    test_path("..", "..", "inst", "testdata", "dynamic-fire-sample.txt"),
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

  ## (3) FuelTypeTable: IgnProb (col 4) multiplied by base-type-specific candidate.
  ## Conifer rows had IgnProb = 1.0 -> 0.8 (cand for Conifer = 0.8).
  ## ConiferPlantation row (C6) had 1.0 -> 1.2.
  ## Deciduous row (D1) had 0.5 -> 0.5 (cand = 1.0).
  ftt_hdr <- grep("^FuelTypeTable[[:space:]]*$", patched)
  j <- ftt_hdr + 1L
  while (grepl("^[[:space:]]*>>", patched[j]) || !nzchar(trimws(patched[j]))) {
    j <- j + 1L
  }
  ## row 1: Conifer C1 -> IgnProb = 0.8
  row1 <- strsplit(trimws(patched[j]), "\\s+")[[1]]
  expect_equal(row1[2L], "Conifer")
  expect_equal(as.numeric(row1[4L]), 0.8)
  ## look ahead for the ConiferPlantation row (Index 6)
  for (k in seq(j, j + 16L)) {
    if (k > length(patched) || !nzchar(trimws(patched[k]))) {
      break
    }
    pk <- strsplit(trimws(patched[k]), "\\s+")[[1]]
    if (length(pk) >= 4L && identical(pk[2L], "ConiferPlantation")) {
      expect_equal(as.numeric(pk[4L]), 1.2)
    }
    if (length(pk) >= 4L && identical(pk[2L], "Deciduous")) {
      expect_equal(as.numeric(pk[4L]), 0.5)
    }
  }
})

test_that("patch_fire_config() rejects par_vec with wrong names", {
  scenario_dir <- withr::local_tempdir()
  fs::file_copy(
    test_path("..", "..", "inst", "testdata", "dynamic-fire-sample.txt"),
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

  ## row-wise: out$IgnProb == ft$IgnProb * multiplier[ft$Base]
  expect_equal(out$IgnProb[ft$Base == "Conifer"], ft$IgnProb[ft$Base == "Conifer"] * 0.8)
  expect_equal(
    out$IgnProb[ft$Base == "ConiferPlantation"],
    ft$IgnProb[ft$Base == "ConiferPlantation"] * 1.2
  )
  expect_equal(out$IgnProb[ft$Base == "Deciduous"], ft$IgnProb[ft$Base == "Deciduous"] * 0.5)
  expect_equal(out$IgnProb[ft$Base == "Slash"], ft$IgnProb[ft$Base == "Slash"] * 1.5)
  expect_equal(out$IgnProb[ft$Base == "Open"], rep(0, sum(ft$Base == "Open")))
  ## non-IgnProb columns untouched
  expect_equal(out$a, ft$a)
  expect_equal(out$Base, ft$Base)
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
  r <- terra::rast(nrow = 10, ncol = 10, xmin = 0, xmax = 1000, ymin = 0, ymax = 1000)
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
  ## Sizes: SIZE_HA = 0.5 dropped (would be zero-ish + need positive); 5 and 100 kept
  expect_equal(p$fire_sizes_ha, sort(c(5.0, 100.0, 0.5)))
  ## area_by_fuel_ha: polygon overlaps deciduous cells only (codes 8)
  expect_s3_class(p$area_by_fuel_ha, "tbl_df")
  expect_true("Deciduous" %in% p$area_by_fuel_ha$base)

  ## Secondary stays NULL when no secondary inputs supplied
  expect_null(payload$secondary)
  expect_null(payload$frt12)
  ## Aliases populated
  expect_identical(payload$fru59, payload$primary)
})

test_that("save_observed_fire_targets() accepts a custom fuel_code_to_base mapping", {
  r <- terra::rast(nrow = 5, ncol = 5, xmin = 0, xmax = 500, ymin = 0, ymax = 500)
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
  fixture <- test_path("..", "..", "inst", "testdata", "forc-succession-sample.txt")
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
  expect_equal(trimws(patched[data_idx]), "0  0  1  20")
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
