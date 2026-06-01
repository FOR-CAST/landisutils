## Dynamic Fire calibration ----------------------------------------------------------------
##
## Pure-data helpers (no LANDIS-II invocation, no project-target assumptions).
## Subsequent tranches add: observed-target builder (8b), scenario builders (8c),
## simulator orchestrator + spinup runner (8d), DEoptim driver (8e), vignette (8f).
##
## See also `calibrate_original_fire.R` (stub; a future calibration target for
## the LANDIS-II Original Fire extension would mirror this file's structure).

#' Canonical parameter names for the Dynamic Fire calibration vector
#'
#' The order of names in this vector is the canonical order used by
#' [calibrate_dynamic_fire()] (Phase 8e) and [patch_fire_config()] (this file).
#' Callers building `lower` / `upper` bounds, or passing candidate vectors to
#' [patch_fire_config()], must match this exact set.
#'
#' @returns Character vector of length 9.
#'
#' @family Dynamic Fire calibration helpers
#'
#' @export
calibration_par_names <- function() {
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
}

## LANDIS-II writes CSVs with a trailing comma -> empty trailing column.
## Strip it so downstream column references aren't misaligned.
.read_landis_csv <- function(p) {
  df <- utils::read.csv(p, header = TRUE, check.names = FALSE, stringsAsFactors = FALSE)
  blank <- vapply(df, function(col) all(is.na(col) | col == ""), logical(1))
  df[, !blank, drop = FALSE]
}

#' Parse a Dynamic Fire run's event and summary logs
#'
#' Reads `<rep_dir>/fire/dynamic-fire-event-log.csv` (one row per fire event)
#' and `<rep_dir>/fire/dynamic-fire-summary-log.csv` (one row per simulation
#' year per fire ecoregion), returning a small list of summary statistics
#' suitable for loss-function comparison.
#'
#' Columns parsed (Dynamic Fire System v4):
#' \itemize{
#'   \item event-log: `Time`, `InitFireRegion`, `InitFuel`, `DamagedSites`, `MeanSeverity`.
#'   \item summary-log: `Time`, `NumberFires`, `TotalSitesBurned`.
#' }
#'
#' Cells -> hectares uses `pixel_area_ha` (1 ha for a 100 m x 100 m grid).
#'
#' @param rep_dir Character. Path to the per-rep directory (the `rep01/` under
#'   the scenario directory). Must contain `fire/dynamic-fire-event-log.csv`
#'   and `fire/dynamic-fire-summary-log.csv`.
#' @param pixel_area_ha Numeric. Hectares per cell. Default `1.0`.
#'
#' @returns Named list with `n_fires_by_year` (tibble: `year`, `n_fires`),
#'   `fire_sizes_ha` (sorted numeric vector), `events` (per-event tibble),
#'   `total_sites_burned` (integer), `n_events` (integer).
#'
#' @family Dynamic Fire calibration helpers
#'
#' @export
parse_dynamic_fire_logs <- function(rep_dir, pixel_area_ha = 1.0) {
  stopifnot(fs::dir_exists(rep_dir), is.numeric(pixel_area_ha), pixel_area_ha > 0)
  event_path <- fs::path(rep_dir, "fire", "dynamic-fire-event-log.csv")
  summary_path <- fs::path(rep_dir, "fire", "dynamic-fire-summary-log.csv")
  if (!fs::file_exists(event_path) || !fs::file_exists(summary_path)) {
    stop("Dynamic Fire logs not found under ", fs::path(rep_dir, "fire"), call. = FALSE)
  }

  events <- .read_landis_csv(event_path)
  summary_df <- .read_landis_csv(summary_path)

  n_fires_by_year <- summary_df |>
    dplyr::group_by(year = as.integer(.data$Time)) |>
    dplyr::summarise(n_fires = sum(.data$NumberFires, na.rm = TRUE), .groups = "drop") |>
    dplyr::arrange(.data$year)

  if (nrow(events) > 0L) {
    events_tbl <- tibble::tibble(
      year = as.integer(events$Time),
      eco = trimws(as.character(events$InitFireRegion)),
      init_fuel = as.integer(events$InitFuel),
      sites = as.integer(events$DamagedSites),
      mean_severity = as.numeric(events$MeanSeverity)
    )
    fire_sizes_ha <- sort(as.numeric(events_tbl$sites) * pixel_area_ha)
  } else {
    events_tbl <- tibble::tibble(
      year = integer(0),
      eco = character(0),
      init_fuel = integer(0),
      sites = integer(0),
      mean_severity = numeric(0)
    )
    fire_sizes_ha <- numeric(0)
  }

  list(
    n_fires_by_year = n_fires_by_year,
    fire_sizes_ha = fire_sizes_ha,
    events = events_tbl,
    total_sites_burned = sum(events_tbl$sites),
    n_events = nrow(events_tbl)
  )
}

#' Patch a `dynamic-fire.txt` in place with candidate calibration parameters
#'
#' Surgical text replacements:
#' \itemize{
#'   \item `SeverityCalibrationFactor <x>` (single scalar line).
#'   \item `FireSizesTable` data rows: columns 8 (`SpHiProp`), 11 (`SumHiProp`),
#'         14 (`FallHiProp`) replaced. Shared across all ecoregion rows --
#'         per-ecoregion HiProp calibration would require 6 params not 3.
#'   \item `FuelTypeTable` data rows: column 4 (`IgnProb`) is multiplied by the
#'         base-type-specific candidate (e.g., `IgnProb_Conifer` for `Base == "Conifer"`).
#'         Default IgnProbs are mostly 1.0 (D1 = 0.5), so candidate range `[0, 1.5]`
#'         directly scales the relative-weighting.
#' }
#'
#' The file is patched in place; callers are expected to pass a per-trial copy
#' of the template so trials don't collide.
#'
#' @param scenario_dir Character. Directory containing `dynamic-fire.txt`.
#' @param par_vec Numeric. Named vector keyed by [calibration_par_names()].
#'
#' @returns Character scalar: absolute path to the patched `dynamic-fire.txt`.
#'
#' @family Dynamic Fire calibration helpers
#'
#' @export
patch_fire_config <- function(scenario_dir, par_vec) {
  stopifnot(
    fs::dir_exists(scenario_dir),
    is.numeric(par_vec),
    !is.null(names(par_vec)),
    setequal(names(par_vec), calibration_par_names())
  )
  fire_txt <- fs::path(scenario_dir, "dynamic-fire.txt")
  if (!fs::file_exists(fire_txt)) {
    stop("dynamic-fire.txt not found in ", scenario_dir, call. = FALSE)
  }
  lines <- readLines(fire_txt)

  ## 1. SeverityCalibrationFactor scalar
  sev_idx <- grep("^SeverityCalibrationFactor[[:space:]]", lines)
  if (length(sev_idx) != 1L) {
    stop(
      "Expected exactly one SeverityCalibrationFactor line in ",
      fire_txt,
      " (found ",
      length(sev_idx),
      ")",
      call. = FALSE
    )
  }
  lines[sev_idx] <- sprintf(
    "SeverityCalibrationFactor    %g",
    par_vec[["SeverityCalibrationFactor"]]
  )

  ## 2. FireSizesTable HiProp columns (8 / 11 / 14)
  fs_hdr <- grep(">>\\s+Fire Sizes", lines)
  if (length(fs_hdr) != 1L) {
    stop("Could not locate FireSizesTable header in ", fire_txt, call. = FALSE)
  }
  i <- fs_hdr + 1L
  while (i <= length(lines) && (grepl("^[[:space:]]*>>", lines[i]) || !nzchar(trimws(lines[i])))) {
    i <- i + 1L
  }
  while (i <= length(lines) && nzchar(trimws(lines[i])) && !grepl("^[A-Za-z]", lines[i])) {
    parts <- strsplit(trimws(lines[i]), "\\s+")[[1]]
    if (length(parts) >= 14L) {
      parts[8L] <- sprintf("%g", par_vec[["SpHiProp"]])
      parts[11L] <- sprintf("%g", par_vec[["SumHiProp"]])
      parts[14L] <- sprintf("%g", par_vec[["FallHiProp"]])
      lines[i] <- paste(parts, collapse = "    ")
    }
    i <- i + 1L
  }

  ## 3. FuelTypeTable IgnProb column (4)
  ftt_hdr <- grep("^FuelTypeTable[[:space:]]*$", lines)
  if (length(ftt_hdr) != 1L) {
    stop("Could not locate FuelTypeTable header in ", fire_txt, call. = FALSE)
  }
  base_multipliers <- c(
    Conifer = par_vec[["IgnProb_Conifer"]],
    ConiferPlantation = par_vec[["IgnProb_ConiferPlantation"]],
    Deciduous = par_vec[["IgnProb_Deciduous"]],
    Slash = par_vec[["IgnProb_Slash"]],
    Open = par_vec[["IgnProb_Open"]]
  )
  j <- ftt_hdr + 1L
  while (j <= length(lines) && (grepl("^[[:space:]]*>>", lines[j]) || !nzchar(trimws(lines[j])))) {
    j <- j + 1L
  }
  while (j <= length(lines) && nzchar(trimws(lines[j])) && !grepl("^[A-Za-z]", lines[j])) {
    parts <- strsplit(trimws(lines[j]), "\\s+")[[1]]
    if (length(parts) >= 11L) {
      base <- parts[2L]
      default_ignprob <- suppressWarnings(as.numeric(parts[4L]))
      mult <- base_multipliers[[base]]
      if (!is.null(mult) && !is.na(default_ignprob)) {
        parts[4L] <- sprintf("%g", default_ignprob * mult)
        lines[j] <- paste(parts, collapse = "    ")
      }
    }
    j <- j + 1L
  }

  writeLines(lines, fire_txt)
  fs::path_real(fire_txt)
}

#' Compute the calibration loss from N replicate trial outputs
#'
#' Combines per-replicate [parse_dynamic_fire_logs()] outputs into the multi-
#' component weighted loss against observed targets from `save_observed_fire_targets()`
#' (Phase 8b).
#'
#' Tier 1 implementation: `L_count` + `L_size`.
#' \itemize{
#'   \item `L_count = |mean(n_fires_sim) - lambda_obs| / sd(n_fires_obs)` --
#'         annual-rate match against the primary ecoregion target (default `fru59`).
#'   \item `L_size = KS_D(empirical CDF of sim sizes, empirical CDF of obs sizes)` --
#'         shape match between simulated and observed fire-size distributions.
#' }
#'
#' Tier 2 (future): `L_area_fuel` and `L_severity`. Stubbed here as zeros when
#' the corresponding observed component is NULL; weights default to 0 so they
#' contribute nothing until implemented.
#'
#' @param reps List. Each element is the return value of
#'   [parse_dynamic_fire_logs()] for one replicate.
#' @param observed List. Output of `save_observed_fire_targets()` (Phase 8b).
#'   Must contain `$fru59` with `$lambda_obs`, `$n_fires_by_year`, `$fire_sizes_ha`.
#' @param weights Named numeric vector. Components: `count`, `size`, `area_fuel`,
#'   `severity`. Missing components default to 0.
#'
#' @returns Named list with `total` (the scalar minimised by DEoptim), `components`
#'   (per-component contributions), and `weights` (echoed weight vector).
#'
#' @family Dynamic Fire calibration helpers
#'
#' @export
loss_from_stats <- function(
  reps,
  observed,
  weights = c(count = 1, size = 1, area_fuel = 0, severity = 0)
) {
  stopifnot(is.list(reps), length(reps) >= 1L, is.list(observed), !is.null(observed$fru59))

  ## L_count: pool simulated annual counts per rep, compare mean to observed lambda
  n_fires_per_year_per_rep <- vapply(
    reps,
    function(r) sum(r$n_fires_by_year$n_fires) / max(1L, nrow(r$n_fires_by_year)),
    numeric(1)
  )
  obs_n <- observed$fru59$n_fires_by_year$n
  obs_sd <- stats::sd(obs_n)
  if (!is.finite(obs_sd) || obs_sd <= 0) {
    obs_sd <- 1
  }
  L_count <- abs(mean(n_fires_per_year_per_rep) - observed$fru59$lambda_obs) / obs_sd

  ## L_size: pool simulated sizes across reps, compute KS distance to observed
  sim_sizes <- unlist(lapply(reps, function(r) r$fire_sizes_ha), use.names = FALSE)
  obs_sizes <- observed$fru59$fire_sizes_ha
  if (length(sim_sizes) == 0L || length(obs_sizes) == 0L) {
    L_size <- 1.0
  } else {
    L_size <- suppressWarnings(stats::ks.test(sim_sizes, obs_sizes)$statistic |> as.numeric())
  }

  ## Tier 2 stubs
  L_area_fuel <- 0.0
  L_severity <- 0.0

  components <- c(count = L_count, size = L_size, area_fuel = L_area_fuel, severity = L_severity)
  w <- setNames(rep(0, length(components)), names(components))
  w[names(weights)] <- weights
  total <- sum(w * components)

  list(total = total, components = components, weights = w)
}

#' Apply per-base-fuel-type IgnProb multipliers to a FuelTypeTable
#'
#' Each row of `fuel_type_table` carries a `Base` column (one of `"Conifer"`,
#' `"ConiferPlantation"`, `"Deciduous"`, `"Slash"`, `"Open"`) and an `IgnProb`
#' column. This multiplies `IgnProb` row-wise by the matching `IgnProb_<base>`
#' entry in the calibrated parameter vector. Defaults in [defaultFuelTypeTable()]
#' are mostly 1.0 (with `D1 = 0.5`), so a candidate range of `[0, 1.5]` directly
#' scales the relative ignition weighting.
#'
#' @param fuel_type_table data.frame from [defaultFuelTypeTable()]. Must have
#'   `Base` and `IgnProb` columns.
#' @param calibrated_fire_params Named numeric vector. Must include the five
#'   `IgnProb_<base>` entries from [calibration_par_names()].
#'
#' @returns A copy of `fuel_type_table` with `IgnProb` updated.
#'
#' @family Dynamic Fire calibration helpers
#' @family Dynamic Fire helpers
#'
#' @export
apply_calibrated_ignprob <- function(fuel_type_table, calibrated_fire_params) {
  stopifnot(
    is.data.frame(fuel_type_table),
    all(c("Base", "IgnProb") %in% names(fuel_type_table)),
    is.numeric(calibrated_fire_params),
    !is.null(names(calibrated_fire_params))
  )
  multipliers <- c(
    Conifer = calibrated_fire_params[["IgnProb_Conifer"]],
    ConiferPlantation = calibrated_fire_params[["IgnProb_ConiferPlantation"]],
    Deciduous = calibrated_fire_params[["IgnProb_Deciduous"]],
    Slash = calibrated_fire_params[["IgnProb_Slash"]],
    Open = calibrated_fire_params[["IgnProb_Open"]]
  )
  m <- multipliers[fuel_type_table$Base]
  m[is.na(m)] <- 1.0
  fuel_type_table$IgnProb <- fuel_type_table$IgnProb * m
  fuel_type_table
}

#' Overwrite FireSizesTable Sp/Sum/Fall HiProp columns with calibrated values
#'
#' Replaces `SpHiProp`, `SumHiProp`, `FallHiProp` in every row of `fire_size_table`
#' with the calibrated triple (shared across all ecoregion rows).
#'
#' @param fire_size_table data.frame as produced by a project's
#'   `make_fire_size_table()`-equivalent. Must have columns `SpHiProp`,
#'   `SumHiProp`, `FallHiProp`.
#' @param calibrated_fire_params Named numeric vector. Must include `SpHiProp`,
#'   `SumHiProp`, `FallHiProp` entries.
#'
#' @returns A copy of `fire_size_table` with the three HiProp columns updated.
#'
#' @family Dynamic Fire calibration helpers
#' @family Dynamic Fire helpers
#'
#' @export
apply_calibrated_hi_prop <- function(fire_size_table, calibrated_fire_params) {
  stopifnot(
    is.data.frame(fire_size_table),
    all(c("SpHiProp", "SumHiProp", "FallHiProp") %in% names(fire_size_table)),
    is.numeric(calibrated_fire_params),
    !is.null(names(calibrated_fire_params))
  )
  fire_size_table$SpHiProp <- calibrated_fire_params[["SpHiProp"]]
  fire_size_table$SumHiProp <- calibrated_fire_params[["SumHiProp"]]
  fire_size_table$FallHiProp <- calibrated_fire_params[["FallHiProp"]]
  fire_size_table
}


## Phase 8b: observed-target builder ------------------------------------------------------------

#' Default fuel-code -> base-fuel-type mapping (BC FUEL_TYPE_CD factor levels)
#'
#' Returns the mapping used by gitanyow-partial-harvest and other projects that
#' use the BC `FUEL_TYPE_CD` factor encoding for `fuel_types_rast`. Levels
#' correspond to: 1=B71_S-2, 2=C-2, 3=C-3, 4=C-4, 5=C-5, 6=C-6, 7=C-7, 8=D-1/2,
#' 9=M-1/2, 10=N (non-fuel), 11=O-1a/b, 12=S-1, 13=S-3. Mapped to the five base
#' types accepted by [defaultFuelTypeTable()] / [calibration_par_names()].
#'
#' Downstream projects with a different fuel-classification raster should pass
#' their own mapping vector to [save_observed_fire_targets()] via the
#' `fuel_code_to_base` argument.
#'
#' @returns Character vector of length 13, names "1".."13", values
#'   `"Conifer"` / `"ConiferPlantation"` / `"Deciduous"` / `"Slash"` /
#'   `"Open"` / `NA_character_`.
#'
#' @family Dynamic Fire calibration helpers
#'
#' @export
bc_fuel_code_to_base <- function() {
  c(
    "1" = "Conifer", ## B71_S-2 burned regen, classified as Conifer per pipeline
    "2" = "Conifer", ## C-2
    "3" = "Conifer", ## C-3
    "4" = "Conifer", ## C-4
    "5" = "Conifer", ## C-5
    "6" = "ConiferPlantation", ## C-6
    "7" = "Conifer", ## C-7
    "8" = "Deciduous", ## D-1/2
    "9" = "Conifer", ## M-1/2 mixedwood
    "10" = NA_character_, ## N (non-fuel)
    "11" = "Open", ## O-1a/b
    "12" = "Slash", ## S-1
    "13" = "Slash" ## S-3
  )
}

#' Save observed fire-regime targets (NFDB-derived) for calibration loss
#'
#' Pre-computes per-ecoregion observed summaries that downstream calibration
#' loss components ([loss_from_stats()]) compare simulated fires against.
#' Saves a single small `.rds` payload of base R types -- so DEoptim workers
#' can read it from disk without terra/sf in their environments.
#'
#' Loss-component consumers:
#' \itemize{
#'   \item `L_count` uses `n_fires_by_year` (mean + sd for normalisation).
#'   \item `L_size` uses `fire_sizes_ha` (sorted vector; KS test against sim).
#'   \item `L_area_fuel` uses primary-ecoregion `area_by_fuel_ha` (Tier 2;
#'         weight 0 in Tier 1).
#'   \item `L_severity` stays NULL; populate from literature priors when Tier 2
#'         severity matching is implemented.
#' }
#'
#' Per ecoregion (`primary_ecoregion`, `secondary_ecoregion`):
#' \itemize{
#'   \item Fire counts come from NFDB IGNITION POINTS (one row = one ignition).
#'         NFDB polygons are sparser (only mapped for larger fires).
#'   \item Fire sizes come from NFDB points' `SIZE_HA` column (zeros dropped to
#'         keep the lognormal-flavoured size distribution positive).
#'   \item `area_by_fuel_ha` is computed for the PRIMARY ecoregion only via
#'         polygon overlay on `fuel_types_rast`. `fuel_types_rast` covers the
#'         LANDIS simulation domain; secondary-ecoregion polygons typically
#'         extend well beyond that extent, making a secondary computation
#'         misleading (it would just be the primary value over again).
#' }
#'
#' @param primary_points,primary_polys SpatVector. NFDB ignition points and
#'   fire polygons for the primary ecoregion (the LANDIS simulation extent).
#' @param secondary_points,secondary_polys SpatVector or NULL. Same, for an
#'   optional regional-context ecoregion. `area_by_fuel_ha` is NOT computed
#'   for the secondary (see Details).
#' @param fire_years Integer vector. Years over which counts are normalised
#'   (denominator for `lambda_obs`).
#' @param fuel_types_rast SpatRaster. Integer-coded fuel-type raster covering
#'   the LANDIS simulation extent.
#' @param primary_label,secondary_label Character. Labels for the two ecoregions
#'   (e.g., `"FRU59"` / `"FRT12"`). Stored in the payload for reproducibility.
#' @param fuel_code_to_base Named character vector. Mapping from
#'   `fuel_types_rast` integer codes (as character names) to the five base
#'   fuel types from [defaultFuelTypeTable()]. NA values mark non-fuel codes
#'   to be excluded. Default: [bc_fuel_code_to_base()].
#' @param path Character. Output `.rds` path. Parent dir created if missing.
#'
#' @returns Character. Absolute path to the written file.
#'
#' @family Dynamic Fire calibration helpers
#'
#' @export
save_observed_fire_targets <- function(
  primary_points,
  primary_polys,
  fire_years,
  fuel_types_rast,
  path,
  secondary_points = NULL,
  secondary_polys = NULL,
  primary_label = "primary",
  secondary_label = "secondary",
  fuel_code_to_base = bc_fuel_code_to_base()
) {
  stopifnot(
    inherits(primary_points, "SpatVector"),
    inherits(primary_polys, "SpatVector"),
    is.null(secondary_points) || inherits(secondary_points, "SpatVector"),
    is.null(secondary_polys) || inherits(secondary_polys, "SpatVector"),
    is.numeric(fire_years),
    length(fire_years) >= 1L,
    inherits(fuel_types_rast, "SpatRaster"),
    is.character(path),
    length(path) == 1L,
    is.character(fuel_code_to_base),
    !is.null(names(fuel_code_to_base))
  )

  fs::dir_create(dirname(path))
  pixel_area_ha <- prod(terra::res(fuel_types_rast)) / 10000

  .summarise <- function(points_sv, polys_sv, label, compute_area_by_fuel) {
    pts <- as.data.frame(points_sv)
    plys <- if (is.null(polys_sv)) data.frame() else as.data.frame(polys_sv)

    pts_year <- pts[["YEAR"]]
    n_fires_by_year <- tibble::tibble(
      year = as.integer(fire_years),
      n = vapply(as.integer(fire_years), function(y) sum(pts_year == y, na.rm = TRUE), integer(1))
    )

    sizes_raw <- pts[["SIZE_HA"]]
    fire_sizes_ha <- sort(sizes_raw[!is.na(sizes_raw) & sizes_raw > 0])

    if (isTRUE(compute_area_by_fuel) && !is.null(polys_sv) && nrow(plys) > 0L) {
      poly_mask <- terra::rasterize(polys_sv, fuel_types_rast, background = NA, field = 1)
      burned <- terra::mask(fuel_types_rast, poly_mask)
      freq_df <- as.data.frame(terra::freq(burned))
      val_col <- if ("value" %in% names(freq_df)) "value" else "label"
      area_by_fuel_ha <- tibble::tibble(
        fuel_code = as.integer(freq_df[[val_col]]),
        cells = as.integer(freq_df[["count"]]),
        area_ha = as.numeric(freq_df[["count"]]) * pixel_area_ha,
        base = unname(fuel_code_to_base[as.character(freq_df[[val_col]])])
      ) |>
        dplyr::filter(!is.na(.data$base)) |>
        dplyr::group_by(.data$base) |>
        dplyr::summarise(
          area_ha = sum(.data$area_ha),
          cells = sum(.data$cells),
          .groups = "drop"
        ) |>
        dplyr::arrange(.data$base)
    } else {
      area_by_fuel_ha <- NULL
    }

    list(
      ecoregion = label,
      n_years = length(fire_years),
      n_ignitions = nrow(pts),
      n_polys = nrow(plys),
      lambda_obs = nrow(pts) / length(fire_years),
      n_fires_by_year = n_fires_by_year,
      fire_sizes_ha = fire_sizes_ha,
      area_by_fuel_ha = area_by_fuel_ha,
      severity_dist = NULL
    )
  }

  primary <- .summarise(primary_points, primary_polys, primary_label, compute_area_by_fuel = TRUE)
  secondary <- if (!is.null(secondary_points)) {
    .summarise(secondary_points, secondary_polys, secondary_label, compute_area_by_fuel = FALSE)
  } else {
    NULL
  }

  ## Keep the named accessors that loss_from_stats() expects ($fru59 in legacy
  ## clients, $primary going forward). Set BOTH so projects can use either name.
  payload <- list(
    primary = primary,
    secondary = secondary,
    fru59 = primary, ## back-compat alias for the gitanyow project's loss_from_stats() refs
    frt12 = secondary, ## back-compat alias
    fuel_code_to_base = fuel_code_to_base,
    fire_years_range = c(min = min(fire_years), max = max(fire_years)),
    fire_years = as.integer(fire_years),
    pixel_area_ha = pixel_area_ha,
    computed_at = Sys.time(),
    notes = c(
      "Fire counts come from NFDB ignition points (one row = one ignition).",
      "Fire sizes are NFDB point SIZE_HA values (zeros dropped).",
      "area_by_fuel_ha is computed for the primary ecoregion only (LANDIS sim extent).",
      "severity_dist is NULL; Tier 2 will populate from literature priors."
    )
  )

  saveRDS(payload, path)
  fs::path_real(path)
}
