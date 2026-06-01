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
    stop(
      "Dynamic Fire logs not found under ",
      fs::path(rep_dir, "fire"),
      call. = FALSE
    )
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

## LANDIS-II writes CSVs with a trailing comma -> empty trailing column.
## Strip it so downstream column references aren't misaligned.
.read_landis_csv <- function(p) {
  df <- utils::read.csv(p, header = TRUE, check.names = FALSE, stringsAsFactors = FALSE)
  blank <- vapply(df, function(col) all(is.na(col) | col == ""), logical(1))
  df[, !blank, drop = FALSE]
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
  stopifnot(
    is.list(reps),
    length(reps) >= 1L,
    is.list(observed),
    !is.null(observed$fru59)
  )

  ## L_count: pool simulated annual counts per rep, compare mean to observed lambda
  n_fires_per_year_per_rep <- vapply(
    reps,
    function(r) sum(r$n_fires_by_year$n_fires) / max(1L, nrow(r$n_fires_by_year)),
    numeric(1)
  )
  obs_n <- observed$fru59$n_fires_by_year$n
  obs_sd <- stats::sd(obs_n)
  if (!is.finite(obs_sd) || obs_sd <= 0) obs_sd <- 1
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
