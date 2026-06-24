## Dynamic Fire calibration ----------------------------------------------------------------
##
## Pure-data helpers (no LANDIS-II invocation, no project-target assumptions).
## Subsequent tranches add: observed-target builder (8b), scenario builders (8c),
## simulator orchestrator + spinup runner (8d), DEoptim driver (8e), vignette (8f).
##
## See also `calibrate_original_fire.R` (stub; a future calibration target for
## the LANDIS-II Original Fire extension would mirror this file's structure).

#' @include landis_pool.R
NULL

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
#'   `total_sites_burned` (integer), `n_events` (integer), and
#'   `area_by_fuel_ha` (tibble with `fuel_code`, `cells`, `area_ha` columns,
#'   or NULL when the per-timestep severity x fuel-type rasters aren't on
#'   disk -- e.g. mock-simulator trials).
#'
#' @details
#' The `area_by_fuel_ha` summary is computed by intersecting the per-timestep
#' Dynamic Fire severity rasters (cells with severity > 1 = burned; values 0
#' and 1 are inactive and active-but-unburned respectively) with the matching
#' per-timestep Dynamic Fuels `FuelType` rasters from the same
#' `<rep_dir>/fire/` subdirectory. Cell-fuel-timestep is the unit of
#' accounting: a cell that burns in two distinct timesteps contributes twice
#' (consistent with NBAC's per-fire-perimeter accounting on the observed side).
#' This replaces the earlier convention of attributing each event's
#' entire `DamagedSites` count to its `InitFuel`, which biased simulated
#' burn area toward the dominant-cover fuel since fires ignite where there's
#' igniteable fuel and then spread anywhere.
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
    n_events = nrow(events_tbl),
    area_by_fuel_ha = .read_burned_area_by_fuel(rep_dir, pixel_area_ha)
  )
}

#' Per-rep cell-based burn area by fuel code (internal)
#'
#' Walks `<rep_dir>/fire/severity-{t}.tif` and the matching
#' `<rep_dir>/fire/FuelType-{t}.tif` files, masks the fuel raster to cells
#' with severity > 1 (the Dynamic Fire encoding is 0 = inactive,
#' 1 = active-but-unburned, >= 2 = burned with the value as severity class),
#' and accumulates cell counts per fuel code across timesteps. Each
#' cell-timestep is counted once, so a cell that burns in two distinct
#' timesteps contributes twice -- matching NBAC's per-fire-perimeter
#' accounting on the observed side.
#'
#' Returns NULL when:
#'   * `<rep_dir>/fire/` does not exist (caller didn't run LANDIS),
#'   * no `severity-*.tif` files were emitted (no fires in this rep), or
#'   * no matching `FuelType-*.tif` companion is on disk (Dynamic Fuels
#'     either not enabled or wrote to a different output dir than expected).
#'
#' Downstream `loss_from_stats()` ->`.chi_sq_area_by_fuel()` prefers this
#' tibble when present and falls back to the legacy `events$init_fuel`
#' attribution when it's NULL (e.g. from mock-simulator trials or
#' payloads written by older landisutils versions).
#'
#' @keywords internal
.read_burned_area_by_fuel <- function(rep_dir, pixel_area_ha) {
  fire_dir <- fs::path(rep_dir, "fire")
  if (!fs::dir_exists(fire_dir)) {
    return(NULL)
  }
  sev_files <- fs::dir_ls(fire_dir, regexp = "severity-\\d+\\.tif$", type = "file")
  if (length(sev_files) == 0L) {
    return(NULL)
  }
  steps <- as.integer(sub(".*severity-(\\d+)\\.tif$", "\\1", as.character(sev_files)))
  per_step <- list()
  for (i in seq_along(sev_files)) {
    fuel_path <- fs::path(fire_dir, sprintf("FuelType-%d.tif", steps[i]))
    if (!fs::file_exists(fuel_path)) {
      next
    }
    sev <- terra::rast(as.character(sev_files[i]))
    fuel <- terra::rast(as.character(fuel_path))
    ## Dynamic Fire severity encoding:
    ##   0  = inactive (non-flammable / off-landscape)
    ##   1  = active but UNBURNED this timestep (still in scope; just didn't burn)
    ##   >= 2 = burned, with the value as the fire severity class
    ## So burned cells are `sev > 1`, NOT `sev > 0`. Counting `> 0` would
    ## select the whole active landscape and accumulate the landscape's fuel
    ## composition instead of the burned area -- silently mis-training
    ## `L_area_fuel` and mis-rendering the report's area-by-fuel panel.
    ## See R/landis_results_fire.R / R/targets_landis_results.R in the
    ## consuming projects, which apply the same `> 1` convention.
    burned_fuel <- terra::ifel(sev > 1, fuel, NA)
    freq_df <- terra::freq(burned_fuel)
    if (nrow(freq_df) == 0L) {
      next
    }
    per_step[[length(per_step) + 1L]] <- as.data.frame(freq_df)
  }
  if (length(per_step) == 0L) {
    return(NULL)
  }
  all_counts <- do.call(rbind, per_step)
  val_col <- if ("value" %in% colnames(all_counts)) "value" else "label"
  agg <- stats::aggregate(
    all_counts$count,
    by = list(fuel_code = as.integer(all_counts[[val_col]])),
    FUN = sum
  )
  names(agg)[2L] <- "cells"
  tibble::tibble(
    fuel_code = as.integer(agg$fuel_code),
    cells = as.integer(agg$cells),
    area_ha = as.numeric(agg$cells) * pixel_area_ha
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
        ## LANDIS-II Dynamic Fire requires IgnProb in [0, 1.0]; clamp so a
        ## DEoptim trial whose multiplier pushes the product above 1.0 (e.g.,
        ## `IgnProb_Conifer = 1.5` against a default IgnProb of 1.0) does not
        ## abort the run with a parser error.
        parts[4L] <- sprintf("%g", min(max(default_ignprob * mult, 0), 1))
        lines[j] <- paste(parts, collapse = "    ")
      }
    }
    j <- j + 1L
  }

  writeLines(lines, fire_txt)
  fs::path_real(fire_txt)
}

#' Default severity-class prior (Sturtevant et al. 2009)
#'
#' Returns a named 5-element vector of expected proportions across the
#' integer severity classes (1 = low, 5 = high) produced by the
#' Dynamic Fire System. Default values are illustrative starting points
#' derived from the modelled distribution in the original Dynamic Fire
#' extension paper; callers should override with empirical priors when
#' available for their specific fire regime.
#'
#' @returns Named numeric vector of length 5, summing to 1.
#'
#' @references Sturtevant, B.R., Scheller, R.M., Miranda, B.R., Shinneman, D.,
#'   and Syphard, A. 2009. Simulating dynamic and mixed-severity fire regimes:
#'   A process-based fire extension for LANDIS-II. Ecological Modelling
#'   220(23): 3380-3393. \doi{10.1016/j.ecolmodel.2009.07.030}
#'
#' @family Dynamic Fire calibration helpers
#'
#' @export
default_severity_prior_sturtevant2009 <- function() {
  c("1" = 0.30, "2" = 0.25, "3" = 0.20, "4" = 0.15, "5" = 0.10)
}

#' Compute the calibration loss from N replicate trial outputs
#'
#' Combines per-replicate [parse_dynamic_fire_logs()] outputs into the multi-
#' component weighted loss against observed targets from
#' [save_observed_fire_targets()].
#'
#' Components:
#' \itemize{
#'   \item `L_count = |mean(n_fires_sim) - lambda_obs| / sd(n_fires_obs)` --
#'         annual-rate match against the primary ecoregion target.
#'   \item `L_size = KS_D(empirical CDF of sim sizes, empirical CDF of obs
#'         sizes)` -- shape match for the fire-size distribution.
#'   \item `L_area_fuel`: chi-squared distance between simulated and observed
#'         burn-area-by-base-fuel-type *proportions*. Simulated area-by-fuel
#'         comes from each event's ignition fuel code times its `DamagedSites`,
#'         mapped to base fuel types via `observed$fuel_code_to_base`. Skipped
#'         (contributes 0) when either `observed$primary$area_by_fuel_ha` is
#'         NULL or `observed$fuel_code_to_base` is missing.
#'   \item `L_severity`: chi-squared distance between simulated and observed
#'         severity-class proportions. Simulated severities come from each
#'         event's `MeanSeverity` binned into integer classes 1..5; observed
#'         comes from `observed$primary$severity_dist` (a 5-element named
#'         numeric vector summing to 1). Skipped when observed is NULL.
#' }
#'
#' All component values are unitless and non-negative; chi-squared components
#' use a small epsilon in the denominator to avoid division by zero on empty
#' observed bins.
#'
#' @param reps List. Each element is the return value of
#'   [parse_dynamic_fire_logs()] for one replicate.
#' @param observed List. Output of [save_observed_fire_targets()]. Must contain
#'   `$primary` (or `$fru59` back-compat alias) with `$lambda_obs`,
#'   `$n_fires_by_year`, `$fire_sizes_ha`. May contain
#'   `$primary$area_by_fuel_ha`, `$primary$severity_dist`,
#'   `$fuel_code_to_base`, and `$pixel_area_ha` to activate Tier 2 components.
#' @param weights Named numeric vector. Components: `count`, `size`,
#'   `area_fuel`, `severity`. Missing components default to 0.
#'
#' @returns Named list with `total` (the scalar minimised by DEoptim),
#'   `components` (per-component contributions), and `weights` (echoed weights).
#'
#' @family Dynamic Fire calibration helpers
#'
#' @export
loss_from_stats <- function(
  reps,
  observed,
  weights = c(count = 1, size = 1, size_tail = 1, area_fuel = 0, severity = 0)
) {
  stopifnot(is.list(reps), length(reps) >= 1L, is.list(observed))
  primary <- observed$primary %||% observed$fru59
  stopifnot(!is.null(primary))

  ## L_count: pool simulated annual counts per rep, compare mean to observed lambda
  n_fires_per_year_per_rep <- vapply(
    reps,
    function(r) sum(r$n_fires_by_year$n_fires) / max(1L, nrow(r$n_fires_by_year)),
    numeric(1)
  )
  obs_n <- primary$n_fires_by_year$n
  obs_sd <- stats::sd(obs_n)
  if (!is.finite(obs_sd) || obs_sd <= 0) {
    obs_sd <- 1
  }
  L_count <- abs(mean(n_fires_per_year_per_rep) - primary$lambda_obs) / obs_sd

  ## L_size: pool simulated sizes across reps, compute KS distance to observed.
  ##
  ## Two corrections vs. the naive KS:
  ##
  ##   1. SYMMETRIC LEFT-TRUNCATION at `observed$min_size_ha` (default 1.0).
  ##      The observed `fire_sizes_ha` was already left-truncated in
  ##      `save_observed_fire_targets()` (NFDB / NBAC under-report sub-1-ha
  ##      fires); apply the same floor to sim_sizes so we don't compare an
  ##      effectively complete simulator distribution against a truncated
  ##      observed sample. Set `observed$min_size_ha = 0` to disable.
  ##
  ##   2. SAMPLE-SIZE CAP: sim is typically n_reps * sim_years = O(thousands
  ##      to tens of thousands) of events while obs is O(tens to a few
  ##      hundred). Even when the underlying distributions are identical,
  ##      the much-better-sampled sim CDF extends further into the tails,
  ##      so KS picks up sampling-frequency mismatch as a "shape" gap.
  ##      Subsample sim down to obs length before the KS test. Done with a
  ##      deterministic seed so the loss is reproducible across DEoptim
  ##      trials at the same parameter vector. Override the seed via
  ##      `getOption("landisutils.calibration.subsample_seed", 12345L)`.
  ##
  ## Also computes `L_size_tail = |log10(sim_q95) - log10(obs_q95)|` -- a
  ## direct upper-tail check that KS systematically under-weights. Weighted
  ## separately via `weights["size_tail"]`.
  sim_sizes <- unlist(lapply(reps, function(r) r$fire_sizes_ha), use.names = FALSE)
  obs_sizes <- primary$fire_sizes_ha
  min_size_ha <- observed$min_size_ha %||% 0
  if (min_size_ha > 0) {
    sim_sizes <- sim_sizes[sim_sizes >= min_size_ha]
    ## obs_sizes was already filtered upstream; do it again as a safety net for
    ## payloads written by older save_observed_fire_targets() (no min_size_ha).
    obs_sizes <- obs_sizes[obs_sizes >= min_size_ha]
  }
  L_size_tail <- 0.0
  if (length(sim_sizes) == 0L || length(obs_sizes) == 0L) {
    L_size <- 1.0
  } else {
    ## Subsample sim if it has materially more events than obs (>= 2x). The
    ## 2x guard avoids unnecessary subsampling when sim and obs are already
    ## comparable -- subsampling small samples just adds variance.
    sim_for_ks <- if (length(sim_sizes) >= 2L * length(obs_sizes)) {
      .seed <- getOption("landisutils.calibration.subsample_seed", 12345L)
      withr::with_seed(.seed, sample(sim_sizes, size = length(obs_sizes), replace = FALSE))
    } else {
      sim_sizes
    }
    L_size <- suppressWarnings(
      stats::ks.test(sim_for_ks, obs_sizes, exact = FALSE)$statistic |> as.numeric()
    )
    ## Tail-aware companion to L_size: log-q95 absolute difference. log10 so
    ## a 10x scale gap reads as ~1 -- comparable in magnitude to the KS [0,1]
    ## statistic. Use length thresholds to avoid taking quantiles of tiny
    ## samples (degenerate).
    if (length(sim_sizes) >= 20L && length(obs_sizes) >= 20L) {
      sim_q95 <- stats::quantile(sim_sizes, 0.95, names = FALSE)
      obs_q95 <- stats::quantile(obs_sizes, 0.95, names = FALSE)
      if (is.finite(sim_q95) && is.finite(obs_q95) && sim_q95 > 0 && obs_q95 > 0) {
        L_size_tail <- abs(log10(sim_q95) - log10(obs_q95))
      }
    }
  }

  ## L_area_fuel: chi-squared on burn-area-by-base-fuel-type proportions.
  ## Active when observed has area_by_fuel_ha AND fuel_code_to_base is supplied.
  L_area_fuel <- if (!is.null(primary$area_by_fuel_ha) && !is.null(observed$fuel_code_to_base)) {
    .chi_sq_area_by_fuel(reps, primary, observed)
  } else {
    0.0
  }

  ## L_severity: chi-squared on severity-class proportions.
  ## Active when observed$primary$severity_dist is non-NULL.
  L_severity <- if (!is.null(primary$severity_dist)) {
    .chi_sq_severity(reps, primary$severity_dist)
  } else {
    0.0
  }

  components <- c(
    count = L_count,
    size = L_size,
    size_tail = L_size_tail,
    area_fuel = L_area_fuel,
    severity = L_severity
  )
  w <- stats::setNames(rep(0, length(components)), names(components))
  w[names(weights)] <- weights
  total <- sum(w * components)

  list(total = total, components = components, weights = w)
}

## Chi-squared on burn-area-by-base-fuel-type proportions (internal).
## Returns a finite scalar; degenerates to a small value when either side has
## no data (the L_count / L_size components handle the no-fires case more
## meaningfully).
.chi_sq_area_by_fuel <- function(reps, primary, observed) {
  fuel_to_base <- observed$fuel_code_to_base
  pixel_area_ha <- observed$pixel_area_ha %||% 1.0

  ## Cell-based attribution: prefer the per-rep `area_by_fuel_ha` summary
  ## emitted by parse_dynamic_fire_logs() from the severity x FuelType
  ## raster intersection. This matches the observed side (NBAC perimeters
  ## rasterised against `fuel_types_rast` -- each burned cell attributed
  ## to its actual fuel). Reps without an `area_by_fuel_ha` field (mock
  ## simulators, payloads from landisutils < 0.0.52) fall back to the
  ## legacy event-based attribution (each event's full `DamagedSites`
  ## counted toward its `InitFuel` only), which biases simulated burn area
  ## toward the dominant-cover fuel.
  has_cell_attr <- vapply(reps, function(r) !is.null(r$area_by_fuel_ha), logical(1))
  if (all(has_cell_attr)) {
    sim_area_df <- do.call(rbind, lapply(reps, function(r) r$area_by_fuel_ha))
    sim_area_df$base <- unname(fuel_to_base[as.character(sim_area_df$fuel_code)])
    sim_area_df <- sim_area_df[!is.na(sim_area_df$base), , drop = FALSE]
    if (nrow(sim_area_df) == 0L) {
      return(1.0)
    }
    sim_area_by_base <- tapply(sim_area_df$area_ha, sim_area_df$base, sum)
  } else {
    sim_events <- do.call(
      rbind,
      lapply(reps, function(r) {
        if (nrow(r$events) == 0L) {
          return(NULL)
        }
        r$events[, c("init_fuel", "sites"), drop = FALSE]
      })
    )
    if (is.null(sim_events) || nrow(sim_events) == 0L) {
      return(1.0) ## penalty for no simulated fires
    }
    sim_events$base <- unname(fuel_to_base[as.character(sim_events$init_fuel)])
    sim_events <- sim_events[!is.na(sim_events$base), , drop = FALSE]
    if (nrow(sim_events) == 0L) {
      return(1.0)
    }
    sim_area_by_base <- tapply(sim_events$sites, sim_events$base, sum) * pixel_area_ha
  }

  obs_area_by_base <- stats::setNames(primary$area_by_fuel_ha$area_ha, primary$area_by_fuel_ha$base)

  ## Pool both distributions over the union of base types, with 0 padding.
  bases <- union(names(sim_area_by_base), names(obs_area_by_base))
  sim_v <- as.numeric(sim_area_by_base[bases])
  sim_v[is.na(sim_v)] <- 0
  obs_v <- as.numeric(obs_area_by_base[bases])
  obs_v[is.na(obs_v)] <- 0

  sim_p <- if (sum(sim_v) > 0) sim_v / sum(sim_v) else sim_v
  obs_p <- if (sum(obs_v) > 0) obs_v / sum(obs_v) else obs_v

  ## Laplace-smooth obs_p to keep the chi-sq finite when sim puts mass in
  ## an empty observed bin (same `alpha = 0.01` default + option override
  ## as `.chi_sq_severity()` for consistency).
  alpha <- getOption("landisutils.calibration.area_fuel_smoothing_alpha", 0.01)
  nbins <- length(obs_p)
  obs_p_smoothed <- (obs_p + alpha) / (1 + nbins * alpha)
  sum((sim_p - obs_p_smoothed)^2 / obs_p_smoothed)
}

## Chi-squared on severity-class proportions (internal).
.chi_sq_severity <- function(reps, severity_dist) {
  sim_sev <- unlist(
    lapply(reps, function(r) {
      if (nrow(r$events) == 0L) {
        return(numeric(0))
      }
      r$events$mean_severity
    }),
    use.names = FALSE
  )
  if (length(sim_sev) == 0L) {
    return(1.0)
  }
  ## Bin MeanSeverity (continuous, ~0-5) into integer classes 1-5 using
  ## half-integer boundaries.
  sim_bins <- cut(
    sim_sev,
    breaks = c(-Inf, 1.5, 2.5, 3.5, 4.5, Inf),
    labels = c("1", "2", "3", "4", "5"),
    right = TRUE
  )
  sim_counts <- table(sim_bins)
  sim_p <- as.numeric(sim_counts) / sum(sim_counts)
  obs_p <- as.numeric(severity_dist[names(sim_counts)])
  obs_p[is.na(obs_p)] <- 0

  ## Laplace smoothing (additive smoothing) on obs_p. The previous
  ## `pmax(obs_p, 1e-6)` in the denominator gave any empty observed class a
  ## ~1e6 multiplier on its (sim_p)^2 chi-sq contribution -- a single percent
  ## of simulated mass in an empty observed bin produced a ~10 chi-sq value,
  ## dominating every other component. Replace with proper smoothing:
  ##
  ##   obs_smoothed = (obs + alpha) / (1 + nbins * alpha)
  ##
  ## with `alpha = 0.01` (1% per bin). This bounds an empty-obs bin's
  ## contribution at ~ (sim_p^2 / 0.01) <= 100 for sim_p in [0, 1] -- still
  ## significant if sim puts all mass in an empty bin, but no longer
  ## drowns the rest of the loss. Override via
  ## `getOption("landisutils.calibration.severity_smoothing_alpha", 0.01)`.
  alpha <- getOption("landisutils.calibration.severity_smoothing_alpha", 0.01)
  nbins <- length(sim_p)
  obs_p_smoothed <- (obs_p + alpha) / (1 + nbins * alpha)
  sum((sim_p - obs_p_smoothed)^2 / obs_p_smoothed)
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
  ## LANDIS-II Dynamic Fire requires IgnProb in [0, 1.0]; clamp the product so
  ## production scenarios assembled from a calibrated parameter vector do not
  ## emit out-of-range values into `dynamic-fire.txt`.
  fuel_type_table$IgnProb <- pmin(pmax(fuel_type_table$IgnProb * m, 0), 1)
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
#' Returns the mapping used by downstream projects that use the BC
#' `FUEL_TYPE_CD` factor encoding for `fuel_types_rast`. Levels
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
#' @param primary_points SpatVector. NFDB ignition points for the primary
#'   ecoregion (the LANDIS simulation extent). Required.
#' @param primary_polys SpatVector or NULL. Fire perimeter polygons for the
#'   primary ecoregion. When supplied, `fire_sizes_ha` is drawn from the
#'   polys' `SIZE_HA` (e.g. NBAC's `ADJ_HA`) and `area_by_fuel_ha` is computed
#'   by rasterising the polys against `fuel_types_rast`. When NULL,
#'   `fire_sizes_ha` falls back to the points' `SIZE_HA` (NFDB agency-reported
#'   sizes) and `area_by_fuel_ha` is NULL on the primary summary.
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
#' @param severity_dist Named numeric vector or NULL. Expected proportions
#'   across the 5 Dynamic Fire severity classes (names `"1"`..`"5"`). Stored
#'   on the primary-ecoregion summary; consumed by `loss_from_stats()`'s
#'   `L_severity` component. NULL = skip severity calibration (the loss
#'   contributes 0). For a literature-prior default, see
#'   [default_severity_prior_sturtevant2009()].
#' @param min_size_ha Numeric scalar. Minimum fire size (ha) retained in
#'   `fire_sizes_ha`. Defaults to `1.0`: NFDB systematically under-reports
#'   sub-1-ha fires (agencies don't document every spot fire) and NBAC's
#'   mapped polygons are also typically truncated below ~1 ha, so the
#'   observed distribution is effectively left-censored at this threshold.
#'   `loss_from_stats()` reads `observed$min_size_ha` and applies the same
#'   truncation symmetrically to `sim_sizes` so the KS distance compares
#'   like with like. Set to `0` to disable the floor.
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
  fuel_code_to_base = bc_fuel_code_to_base(),
  severity_dist = NULL,
  min_size_ha = 1.0
) {
  stopifnot(
    inherits(primary_points, "SpatVector"),
    is.numeric(min_size_ha),
    length(min_size_ha) == 1L,
    min_size_ha >= 0,
    ## primary_polys is optional: when NULL, fire_sizes_ha falls back to the
    ## points' SIZE_HA and area_by_fuel_ha is skipped. Callers that pass NBAC
    ## perimeters get sizes from the polys; callers with only NFDB-style
    ## ignition points still work, just without the area-by-fuel target.
    is.null(primary_polys) || inherits(primary_polys, "SpatVector"),
    is.null(secondary_points) || inherits(secondary_points, "SpatVector"),
    is.null(secondary_polys) || inherits(secondary_polys, "SpatVector"),
    is.numeric(fire_years),
    length(fire_years) >= 1L,
    inherits(fuel_types_rast, "SpatRaster"),
    is.character(path),
    length(path) == 1L,
    is.character(fuel_code_to_base),
    !is.null(names(fuel_code_to_base)),
    is.null(severity_dist) || (is.numeric(severity_dist) && !is.null(names(severity_dist)))
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

    ## fire_sizes_ha source preference:
    ## 1. polygons' SIZE_HA when polys are supplied and non-empty (NBAC's
    ##    ADJ_HA is the mapped burn perimeter minus unburned islands -- the
    ##    most accurate per-fire area). Only polygons with SIZE_HA set are
    ##    used; polys without an area attribute are skipped.
    ## 2. points' SIZE_HA otherwise (NFDB's agency-reported size; available
    ##    even for very small fires that NBAC does not map).
    sizes_raw <- if (nrow(plys) > 0L && "SIZE_HA" %in% colnames(plys)) {
      plys[["SIZE_HA"]]
    } else {
      pts[["SIZE_HA"]]
    }
    ## Drop sub-`min_size_ha` fires: NFDB/NBAC are effectively left-censored
    ## at ~1 ha (small fires systematically under-reported); without this
    ## floor the KS comparison compares the sim's full distribution against
    ## a truncated observed distribution. `loss_from_stats()` applies the
    ## same truncation to `sim_sizes` symmetrically via `observed$min_size_ha`.
    fire_sizes_ha <- sort(sizes_raw[!is.na(sizes_raw) & sizes_raw >= min_size_ha])

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
      severity_dist = NULL ## set on the primary summary below if a prior was passed
    )
  }

  primary <- .summarise(primary_points, primary_polys, primary_label, compute_area_by_fuel = TRUE)
  primary$severity_dist <- severity_dist
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
    fru59 = primary, ## back-compat alias kept for downstream loss_from_stats() refs
    frt12 = secondary, ## back-compat alias
    fuel_code_to_base = fuel_code_to_base,
    fire_years_range = c(min = min(fire_years), max = max(fire_years)),
    fire_years = as.integer(fire_years),
    pixel_area_ha = pixel_area_ha,
    ## Min size threshold used to truncate `fire_sizes_ha`. Loss function
    ## reads this back to apply the same truncation symmetrically to
    ## `sim_sizes` (so KS compares like-with-like, not sim-with-floor vs
    ## obs-without-floor).
    min_size_ha = min_size_ha,
    computed_at = Sys.time(),
    notes = c(
      "Fire counts come from NFDB ignition points (one row = one ignition).",
      "Fire sizes are NFDB point SIZE_HA values (zeros dropped).",
      "area_by_fuel_ha is computed for the primary ecoregion only (LANDIS sim extent).",
      paste(
        "severity_dist on primary is",
        if (is.null(severity_dist)) "NULL (L_severity will contribute 0);" else "set from caller;",
        "see default_severity_prior_sturtevant2009() for a Sturtevant 2009 prior."
      )
    )
  )

  saveRDS(payload, path)
  fs::path_real(path)
}


## Phase 8c: scenario builders for calibration runs ----------------------------------------------

#' Patch a ForC Succession config for calibration use (internal)
#'
#' Two surgical replacements in `forc-succession.txt`:
#' \itemize{
#'   \item `Timestep N` -> `Timestep <sim_years + 1>`. Makes ForCS skip its
#'         succession step for the duration of the calibration run (no growth,
#'         no establishment, no mortality) so each DEoptim trial is purely a
#'         fire-on-fixed-landscape experiment.
#'   \item `SpinUp` data row -> `0  0  1  20`. The calibration IC is already
#'         post-spinup (from the snapshot of [build_calibration_spinup_scenario()]),
#'         so per-trial spinup would be wasted compute. DOM equilibration is
#'         also skipped -- fire severity depends on cohort biomass, not DOM.
#' }
#'
#' Patches the file in place. Caller is expected to pass the calibration
#' scenario's own copy of `forc-succession.txt` (not the production one).
#'
#' @param path Character. Absolute path to `forc-succession.txt`.
#' @param sim_years Integer. Calibration sim duration (years).
#'
#' @returns The patched lines, invisibly (also written to `path`).
#'
#' @keywords internal
.patch_forcs_for_calibration <- function(path, sim_years) {
  stopifnot(fs::file_exists(path), is.numeric(sim_years), sim_years >= 1L)
  lines <- readLines(path)

  ts_idx <- grep("^Timestep[[:space:]]", lines)
  if (length(ts_idx) != 1L) {
    stop(
      "Expected exactly one `Timestep` line in ",
      path,
      " (found ",
      length(ts_idx),
      ")",
      call. = FALSE
    )
  }
  lines[ts_idx] <- sprintf("Timestep    %d", as.integer(sim_years) + 1L)

  spinup_hdr <- grep("^SpinUp[[:space:]]*$", lines)
  if (length(spinup_hdr) != 1L) {
    stop(
      "Expected exactly one `SpinUp` section header in ",
      path,
      " (found ",
      length(spinup_hdr),
      ")",
      call. = FALSE
    )
  }
  data_idx <- spinup_hdr + 1L
  while (data_idx <= length(lines) && grepl("^[[:space:]]*>>", lines[data_idx])) {
    data_idx <- data_idx + 1L
  }
  if (data_idx > length(lines)) {
    stop("SpinUp data row not found in ", path, call. = FALSE)
  }
  ## SpinUp flags for calibration:
  ##   * `Flag` = 1: enable DOM spinup (SpinupSoils iteratively equilibrates
  ##     each ecoregion x species DOM pool). Required for fires to actually
  ##     damage cohorts -- otherwise ForCS's DisturbFireFromBiomassPools is
  ##     left in a partly-initialised state and Dynamic Fire's CohortMortality
  ##     handler hits a NullReferenceException in
  ##     Extension-ForCS-Succession/src/Soil.cs:DisturbanceImpactsBiomass.
  ##   * `BiomassSpinUpFlag` = 0: keep biomass-cohort spinup OFF so the
  ##     snapshot IC's CohortBiomass values are preserved verbatim (the whole
  ##     point of the pre-calibration spinup pipeline). Biomass spinup would
  ##     overwrite the snapshot by walking ANPP from age 0 to each cohort's
  ##     age, which we explicitly DON'T want here.
  ## Cost: ~30-60s startup per LANDIS-II trial for DOM equilibration; one-time
  ## per simulation, so calibration wall-time bumps marginally (and only on
  ## trials that wouldn't have started fires anyway).
  lines[data_idx] <- "1  0  1  20"

  writeLines(lines, path)
  invisible(lines)
}

#' Freeze Biomass Succession for the calibration scenario (the Biomass-Succession analog of
#' [.patch_forcs_for_calibration()]).
#'
#' Biomass Succession has no `SpinUp` section and (unlike ForCS) no `Soil.cs` DisturbFireFromBiomassPools
#' path, so there is no DOM-spinup / NullReferenceException workaround to apply. The only requirement is
#' that succession does NOT change the (spun-up) fuel landscape during the short calibration sims, so the
#' fire behaviour reflects the candidate Dynamic Fire parameters rather than vegetation change. We freeze
#' it by setting the succession `Timestep` greater than the calibration `sim_years`: the extension
#' initialises but its first scheduled succession event falls beyond the run Duration, so it never
#' executes and the initial-communities biomass is held static.
#' @keywords internal
.patch_biomass_for_calibration <- function(path, sim_years) {
  stopifnot(fs::file_exists(path), is.numeric(sim_years), sim_years >= 1L)
  lines <- readLines(path)
  ts_idx <- grep("^Timestep[[:space:]]", lines)
  if (length(ts_idx) != 1L) {
    stop(
      "Expected exactly one `Timestep` line in ",
      path,
      " (found ",
      length(ts_idx),
      ")",
      call. = FALSE
    )
  }
  lines[ts_idx] <- sprintf("Timestep    %d", as.integer(sim_years) + 1L)
  writeLines(lines, path)
  invisible(lines)
}

#' Detect the succession backend of a (calibration) scenario directory and return the per-backend
#' bits the calibration spinup/template need: the LandisData extension name, the config filename, the
#' calibration freeze/spinup patcher, and any fixed-name succession logs to track. ForCS and Biomass
#' Succession are supported; the rest of the calibration setup (Output Biomass Community snapshot,
#' Dynamic Fire/Fuels, fire logs) is backend-independent.
#' @keywords internal
.calibration_succession_backend <- function(dir) {
  if (fs::file_exists(fs::path(dir, "forc-succession.txt"))) {
    list(
      name = "ForC Succession",
      file = "forc-succession.txt",
      patch = .patch_forcs_for_calibration,
      logs = c(
        "log_BiomassC.csv",
        "log_FluxBio.csv",
        "log_Flux.csv",
        "log_FluxDOM.csv",
        "log_Pools.csv",
        "log_Summary.csv"
      )
    )
  } else if (fs::file_exists(fs::path(dir, "biomass-succession.txt"))) {
    list(
      name = "Biomass Succession",
      file = "biomass-succession.txt",
      patch = .patch_biomass_for_calibration,
      logs = character(0) ## no fixed-name Biomass Succession logs are needed by the calibration loss
    )
  } else {
    stop(
      "no recognised succession config (forc-succession.txt or biomass-succession.txt) in ",
      dir,
      call. = FALSE
    )
  }
}

## The LANDIS-II input files a calibration scenario template must contain, with the filenames that vary
## by succession backend / scenario resolved from the template itself rather than assuming one project's
## convention: the succession config (forc-succession.txt vs biomass-succession.txt), the species file
## (scenario.txt `Species` directive), and the Dynamic Fire inputs (dynamic-fire.txt directives). These
## are the same names build_calibration_scenario_template() writes, so its output always validates here.
## simulator_name other than "landis" (mock / r_reimpl) only needs scenario.txt.
.calibration_required_files <- function(template_dir, simulator_name) {
  if (simulator_name != "landis") {
    return("scenario.txt")
  }
  df_input <- function(directive, default) {
    fs::path_file(.calibration_directive_file(template_dir, "dynamic-fire.txt", directive, default))
  }
  c(
    "scenario.txt",
    .calibration_succession_backend(template_dir)$file,
    "dynamic-fire.txt",
    "dynamic-fuels.txt",
    fs::path_file(.calibration_species_file(template_dir)),
    "ecoregions.txt",
    "ecoregions.tif",
    "initial-communities.csv",
    "initial-communities.tif",
    df_input("GroundSlopeFile", "ground_slope.tif"),
    df_input("UphillSlopeAzimuthMap", "uphill_slope_azimuth.tif"),
    df_input("InitialFireEcoregionsMap", "fire-ecoregions.tif"),
    df_input("InitialWeatherDatabase", "initial_weather_database.csv"),
    df_input("Species_CSV_File", "DynamicFire_Spp_Table.csv")
  )
}

## Resolve an input file the template scenario actually references, rather than assuming a fixed name:
## reads the `<directive> <file>` line from `dir/config` (stripping any trailing `>>` comment) and
## returns `dir/<file>`; falls back to `dir/default` when the config or directive is absent. This lets
## the calibration builders work across scenarios that name the same input differently -- e.g. the
## Dynamic Fire weather DB is `initial_weather_database.csv` in some scenarios and
## `initial-weather-database.csv` in others -- without hard-coding either convention.
.calibration_directive_file <- function(dir, config, directive, default) {
  nm <- default
  cf <- fs::path(dir, config)
  if (fs::file_exists(cf)) {
    hit <- grep(
      paste0("^[[:space:]]*", directive, "[[:space:]]"),
      readLines(cf, warn = FALSE),
      value = TRUE
    )
    if (length(hit) > 0L) {
      nm <- trimws(sub(
        ">>.*$",
        "",
        sub(paste0("^[[:space:]]*", directive, "[[:space:]]+"), "", hit[[1L]])
      ))
    }
  }
  fs::path(dir, nm)
}

## The species-definitions file the template scenario references (the scenario.txt `Species` directive);
## falls back to "species.txt" (some scenarios name it "species-core.txt").
.calibration_species_file <- function(dir) {
  .calibration_directive_file(dir, "scenario.txt", "Species", "species.txt")
}

#' Build a calibration spinup scenario directory
#'
#' Materialises a self-contained LANDIS-II scenario whose only purpose is to run
#' the succession backend (ForC Succession or Biomass Succession, auto-detected
#' from the template's config file) for `duration` years, emit a snapshot of the
#' spun-up cohort community via the Output Biomass Community extension, and exit.
#'
#' The Output Biomass Community extension emits at multiples of its Timestep
#' starting from **year 0** (post-init, pre-step), so with Timestep = 1 and
#' Duration = 1 we get two snapshot CSVs: `community-input-file-0.csv`
#' (post-spinup state -- this is the one we want) and `community-input-file-1.csv`
#' (after one year of ANPP). The TIF (`output-community-0.tif`) is emitted only
#' once at year 0 -- cohort communities don't repartition in a no-disturbance
#' run, so one raster suffices for both years.
#'
#' Note on CSV schema: LANDIS-II Output Biomass Community v3 writes a 5-column
#' file (`MapCode, SpeciesName, CohortAge, CohortBiomass, CohortANPP`). LANDIS-II's
#' initial-communities parser tolerates the extra `CohortANPP` column, so the
#' file is drop-in usable as `InitialCommunitiesFiles` without post-processing.
#'
#' Implementation: copy every top-level file from `template_dir` (a production
#' scenario directory), strip the disturbance stack from the copied scenario.txt,
#' add an Output Biomass Community extension, and rewrite scenario.txt with
#' Duration = `duration`. Rep subdirectories are NOT copied.
#'
#' @param out_dir Character. Destination directory (created or overwritten).
#' @param template_dir Character. Existing production scenario directory to copy
#'   from. Must contain `forc-succession.txt`, `species.txt`, `ecoregions.txt`,
#'   `ecoregions.tif`, `climate.txt`, `ForCS_DM.txt`, `initial-communities.csv`,
#'   `initial-communities.tif`, and the ForCS data CSVs.
#' @param duration Integer. Simulation duration in years. LANDIS-II minimum 1.
#' @param community_output_year Integer. Year at which the snapshot is consumed
#'   downstream (caller chooses 0 for post-spinup state; default 0).
#' @param cell_length Integer. Raster cell size in metres.
#'
#' @returns Character scalar: absolute path to the written `scenario.txt`.
#'
#' @family Dynamic Fire calibration helpers
#'
#' @export
build_calibration_spinup_scenario <- function(
  out_dir,
  template_dir,
  duration = 1L,
  community_output_year = 0L,
  cell_length
) {
  stopifnot(
    fs::dir_exists(template_dir),
    is.numeric(duration),
    duration >= 1L,
    is.numeric(community_output_year),
    community_output_year >= 0L,
    community_output_year <= duration,
    is.numeric(cell_length),
    cell_length > 0
  )

  if (fs::dir_exists(out_dir)) {
    fs::dir_delete(out_dir)
  }
  fs::dir_create(out_dir)

  for (f in fs::dir_ls(template_dir, type = "file")) {
    fs::file_copy(f, fs::path(out_dir, basename(f)))
  }

  ## Output Biomass Community: Timestep = 1 emits at every year (we consume year 0).
  ## Timestep = 0 would skip emission entirely (LANDIS treats 0 as "never").
  obc <- OutputBiomassCommunity$new(path = out_dir, Timestep = 1L)
  obc$write()

  backend <- .calibration_succession_backend(out_dir) ## ForCS or Biomass Succession
  succession_file <- fs::path(out_dir, backend$file)
  species_file <- .calibration_species_file(out_dir)
  eco_files <- c(fs::path(out_dir, "ecoregions.txt"), fs::path(out_dir, "ecoregions.tif"))
  obc_file <- fs::path(out_dir, "output-biomass-community.txt")
  stopifnot(
    fs::file_exists(succession_file),
    fs::file_exists(species_file),
    all(fs::file_exists(eco_files)),
    fs::file_exists(obc_file)
  )

  write_landis_scenario_file(
    path = out_dir,
    duration = as.integer(duration),
    cell_length = as.integer(cell_length),
    species_file = species_file,
    ecoregions_files = eco_files,
    succession_ext_files = stats::setNames(succession_file, backend$name),
    disturbance_ext_files = NULL,
    other_ext_files = c("Output Biomass Community" = obc_file),
    output_manifest = c(
      backend$logs,
      sprintf("community-input-file-%d.csv", as.integer(community_output_year)),
      sprintf("output-community-%d.tif", as.integer(community_output_year))
    )
  )
}

#' Build the calibration scenario template directory
#'
#' Materialises a self-contained LANDIS-II scenario directory that DEoptim
#' workers copy from. Each per-trial worker copies this template into a scratch
#' dir, patches just `dynamic-fire.txt` with candidate parameters, and runs
#' LANDIS-II. Anything that does NOT vary across trials (ForCS config, fire
#' ecoregions map, ground slope, weather DB, species file, ...) lives in this
#' template so it's built once.
#'
#' Composition:
#' \itemize{
#'   \item The template's succession backend (ForC Succession or Biomass
#'         Succession, auto-detected), frozen for the calibration: ForCS gets a
#'         frozen Timestep + DOM-spinup-on/biomass-spinup-off flags; Biomass
#'         Succession just gets a frozen Timestep (no SpinUp section). Either way
#'         succession is effectively a no-op so fire behaviour reflects the
#'         candidate parameters, not vegetation change.
#'   \item Dynamic Fire System + Dynamic Fuel System as the only disturbances.
#'   \item Initial communities point at the spun-up snapshot from
#'         [build_calibration_spinup_scenario()] (renamed to the standard
#'         `initial-communities.csv` + `.tif` so the existing ForCS config
#'         references work without further modification).
#'   \item Duration = `sim_years`.
#' }
#'
#' When the baseline fire-config tables are supplied (recommended), the function
#' overwrites the copied `dynamic-fire.txt` with a fresh uncalibrated config
#' built from these tables. This breaks the otherwise-circular dependency
#' between the production fire config and the calibration loop (production fire
#' config -> calibrated_fire_params -> calibration -> production fire config).
#'
#' @param out_dir Character. Destination directory (created or overwritten).
#' @param template_dir Character. Existing production fire scenario directory
#'   to copy from.
#' @param snapshot_ic_csv,snapshot_ic_tif Character. Paths to the spun-up
#'   community CSV / TIF (return of [build_calibration_spinup_scenario()]).
#' @param baseline_fire_size_table,baseline_fuel_type_table,baseline_fire_damage_table,baseline_seasons_sim_table
#'   data.frame or NULL. Baseline (uncalibrated) tables. When all four are
#'   supplied, the function writes a fresh `dynamic-fire.txt` from them.
#' @param sim_years Integer. Calibration sim duration (years). Default 10.
#' @param cell_length Integer. Raster cell size in metres.
#' @param overrides Named list. Optional per-file overrides applied AFTER the
#'   bulk template-dir copy. Keys are output filenames (relative to `out_dir`);
#'   values are paths to source files to copy in place of whatever was copied
#'   from `template_dir`. Useful for swapping in a coarser fuel raster, a
#'   cropped slope/aspect, alternative weather, etc. for calibration without
#'   touching the production scenario. Accepted keys: `"ground_slope.tif"`,
#'   `"uphill_slope_azimuth.tif"`, `"fire-ecoregions.tif"`,
#'   `"initial_weather_database.csv"`, `"DynamicFire_Spp_Table.csv"`,
#'   `"species.txt"`, `"ecoregions.txt"`, `"ecoregions.tif"`, `"climate.txt"`.
#'   `.tif` overrides also copy their `.aux.xml` / `.tfw` sidecars if present
#'   alongside the source.
#'
#' @returns Character scalar: absolute path to the written `scenario.txt`.
#'
#' @family Dynamic Fire calibration helpers
#'
#' @export
build_calibration_scenario_template <- function(
  out_dir,
  template_dir,
  snapshot_ic_csv,
  snapshot_ic_tif,
  baseline_fire_size_table = NULL,
  baseline_fuel_type_table = NULL,
  baseline_fire_damage_table = NULL,
  baseline_seasons_sim_table = NULL,
  sim_years = 10L,
  cell_length,
  overrides = list()
) {
  stopifnot(
    fs::dir_exists(template_dir),
    fs::file_exists(snapshot_ic_csv),
    fs::file_exists(snapshot_ic_tif),
    is.numeric(sim_years),
    sim_years >= 1L,
    is.numeric(cell_length),
    cell_length > 0,
    is.list(overrides),
    is.null(names(overrides)) || all(nzchar(names(overrides)))
  )
  ## Validate override target filenames against the set of files we know how to
  ## replace post-copy. Catches typos like `overrides = list(ground_slope.tif = ...)`
  ## (would silently fail if we let unknown names through).
  allowed_overrides <- c(
    "ground_slope.tif",
    "uphill_slope_azimuth.tif",
    "fire-ecoregions.tif",
    "initial_weather_database.csv",
    "DynamicFire_Spp_Table.csv",
    "species.txt",
    "ecoregions.txt",
    "ecoregions.tif",
    "climate.txt"
  )
  bad_overrides <- setdiff(names(overrides), allowed_overrides)
  if (length(bad_overrides) > 0L) {
    stop(
      "Unknown override target(s): ",
      paste(bad_overrides, collapse = ", "),
      ". Allowed: ",
      paste(allowed_overrides, collapse = ", "),
      call. = FALSE
    )
  }
  ## Confirm each override path exists before we begin copying.
  for (nm in names(overrides)) {
    if (!fs::file_exists(overrides[[nm]])) {
      stop("Override for `", nm, "` not found: ", overrides[[nm]], call. = FALSE)
    }
  }
  write_baseline_fire_config <- !is.null(baseline_fire_size_table) &&
    !is.null(baseline_fuel_type_table) &&
    !is.null(baseline_fire_damage_table) &&
    !is.null(baseline_seasons_sim_table)

  if (fs::dir_exists(out_dir)) {
    fs::dir_delete(out_dir)
  }
  fs::dir_create(out_dir)

  for (f in fs::dir_ls(template_dir, type = "file")) {
    fs::file_copy(f, fs::path(out_dir, basename(f)))
  }

  ## Apply per-file overrides AFTER the bulk copy, so they win.
  for (nm in names(overrides)) {
    fs::file_copy(overrides[[nm]], fs::path(out_dir, nm), overwrite = TRUE)
    ## Carry GDAL sidecars (.tif.aux.xml / .tfw) alongside any overridden .tif.
    if (grepl("\\.tif$", nm, ignore.case = TRUE)) {
      for (sidecar_ext in c(".aux.xml", ".tfw")) {
        src_side <- paste0(overrides[[nm]], sidecar_ext)
        if (fs::file_exists(src_side)) {
          fs::file_copy(src_side, fs::path(out_dir, paste0(nm, sidecar_ext)), overwrite = TRUE)
        }
      }
    }
  }

  ## Replace production IC with the post-spinup snapshot, renaming to the
  ## standard filenames so the existing ForCS config refs work as-is.
  ic_csv_dst <- fs::path(out_dir, "initial-communities.csv")
  ic_tif_dst <- fs::path(out_dir, "initial-communities.tif")
  fs::file_copy(snapshot_ic_csv, ic_csv_dst, overwrite = TRUE)
  fs::file_copy(snapshot_ic_tif, ic_tif_dst, overwrite = TRUE)
  for (sidecar_ext in c(".aux.xml", ".tfw")) {
    src_side <- paste0(snapshot_ic_tif, sidecar_ext)
    if (fs::file_exists(src_side)) {
      fs::file_copy(src_side, paste0(ic_tif_dst, sidecar_ext), overwrite = TRUE)
    }
  }

  ## Patch the succession config for calibration (ForCS: spinup flags + freeze; Biomass: freeze only).
  backend <- .calibration_succession_backend(out_dir)
  backend$patch(fs::path(out_dir, backend$file), sim_years = sim_years)

  ## Overwrite dynamic-fire.txt with a fresh uncalibrated config. Relative
  ## file-path references inside (fire-ecoregions.tif, ground_slope.tif, ...)
  ## resolve against out_dir, where production copies of those files were just
  ## placed by the dir_ls() loop above.
  if (isTRUE(write_baseline_fire_config)) {
    ext_fire <- DynamicFire$new(
      path = out_dir,
      Timestep = 1L,
      EventSizeType = "size_based",
      BuildUpIndex = "yes",
      WeatherRandomizer = 0L,
      FireSizesTable = baseline_fire_size_table,
      InitialFireEcoregionsMap = .calibration_directive_file(
        out_dir,
        "dynamic-fire.txt",
        "InitialFireEcoregionsMap",
        "fire-ecoregions.tif"
      ),
      DynamicEcoregionTable = prepDynamicEcoregionTable(),
      GroundSlopeFile = .calibration_directive_file(
        out_dir,
        "dynamic-fire.txt",
        "GroundSlopeFile",
        "ground_slope.tif"
      ),
      UphillSlopeAzimuthMap = .calibration_directive_file(
        out_dir,
        "dynamic-fire.txt",
        "UphillSlopeAzimuthMap",
        "uphill_slope_azimuth.tif"
      ),
      SeasonTable = baseline_seasons_sim_table,
      InitialWeatherDatabase = .calibration_directive_file(
        out_dir,
        "dynamic-fire.txt",
        "InitialWeatherDatabase",
        "initial_weather_database.csv"
      ),
      DynamicWeatherTable = NULL,
      FuelTypeTable = baseline_fuel_type_table,
      SeverityCalibrationFactor = 1.0, ## baseline; calibrated factor is applied production-side
      FireDamageTable = baseline_fire_damage_table,
      Species_CSV_File = .calibration_directive_file(
        out_dir,
        "dynamic-fire.txt",
        "Species_CSV_File",
        "DynamicFire_Spp_Table.csv"
      ),
      MapNames = NULL,
      LogFile = file.path(out_dir, "fire/dynamic-fire-event-log.csv"),
      SummaryLogFile = file.path(out_dir, "fire/dynamic-fire-summary-log.csv")
    )
    ext_fire$write()
  }

  succession_file <- fs::path(out_dir, backend$file)
  fuels_file <- fs::path(out_dir, "dynamic-fuels.txt")
  fire_file <- fs::path(out_dir, "dynamic-fire.txt")
  species_file <- .calibration_species_file(out_dir)
  eco_files <- c(fs::path(out_dir, "ecoregions.txt"), fs::path(out_dir, "ecoregions.tif"))
  stopifnot(
    fs::file_exists(succession_file),
    fs::file_exists(fuels_file),
    fs::file_exists(fire_file),
    fs::file_exists(species_file),
    all(fs::file_exists(eco_files))
  )

  write_landis_scenario_file(
    path = out_dir,
    duration = as.integer(sim_years),
    cell_length = as.integer(cell_length),
    species_file = species_file,
    ecoregions_files = eco_files,
    succession_ext_files = stats::setNames(succession_file, backend$name),
    disturbance_ext_files = c(
      "Dynamic Fuel System" = fuels_file,
      "Dynamic Fire System" = fire_file
    ),
    other_ext_files = NULL,
    output_manifest = c(
      backend$logs,
      "fire/dynamic-fire-event-log.csv",
      "fire/dynamic-fire-summary-log.csv"
    )
  )
}
## Phase 8d: simulator orchestrator + spinup runner ----------------------------------------------

#' Run the calibration spinup scenario (blocking)
#'
#' Invokes LANDIS-II once against the scenario in `scenario_dir`, blocks until
#' completion, and verifies that the year-0 snapshot files emitted by the
#' Output Biomass Community extension landed on disk.
#'
#' Dispatches to [landis_run_local()] or [landis_run_docker()] based on
#' `method` -- both are synchronous and stop on a non-zero exit, so this
#' wrapper only has to verify the expected files appeared.
#'
#' Per LANDIS-II convention, scenarios are invoked from a numbered replicate
#' sub-directory (`rep01/`); [landis_replicate()] materialises that with a
#' `base_seed`-derived `RandomNumberSeed`, then the run happens inside it.
#' Top-level `scenario_dir` stays clean (output files land under `rep01/`).
#'
#' @param scenario_dir Character. Spinup scenario directory (containing
#'   `scenario.txt`), typically the return of
#'   [build_calibration_spinup_scenario()].
#' @param base_seed Integer. Random seed passed to LANDIS-II via the per-rep
#'   `RandomNumberSeed` rewrite.
#' @param method Character. `"docker"` or `"local"`. Default from
#'   `getOption("landisutils.run.method")`.
#' @param image Character or NULL. Docker image (Docker only).
#' @param pull Logical. `docker pull` before running (Docker only). Default FALSE.
#'
#' @returns Character scalar: absolute path to the year-0 snapshot CSV
#'   (`<scenario_dir>/rep01/community-input-file-0.csv`). The TIF
#'   (`output-community-0.tif`) lives alongside.
#'
#' @family Dynamic Fire calibration helpers
#'
#' @export
run_calibration_spinup <- function(
  scenario_dir,
  base_seed = 12345L,
  method = NULL,
  image = NULL,
  pull = FALSE
) {
  stopifnot(fs::dir_exists(scenario_dir), is.numeric(base_seed), base_seed > 0)
  scenario_dir <- fs::path_real(scenario_dir)

  method <- method %||%
    getOption(
      "landisutils.run.method",
      default = if (.Platform$OS.type == "windows") "local" else "docker"
    )
  stopifnot(method %in% c("local", "docker"))

  rep_dir <- fs::path(scenario_dir, "rep01")
  if (fs::dir_exists(rep_dir)) {
    fs::dir_delete(rep_dir)
  }
  landis_replicate(
    scenario_dir = scenario_dir,
    rep_index = 1L,
    files = NULL,
    base_seed = as.integer(base_seed)
  )

  if (method == "docker") {
    landis_run_docker(
      scenario_dir = rep_dir,
      scenario_file = "scenario.txt",
      image = image,
      pull = isTRUE(pull)
    )
  } else {
    landis_run_local(scenario_dir = rep_dir, scenario_file = "scenario.txt")
  }

  csv_path <- fs::path(rep_dir, "community-input-file-0.csv")
  tif_path <- fs::path(rep_dir, "output-community-0.tif")
  if (!fs::file_exists(csv_path) || !fs::file_exists(tif_path)) {
    stop(
      "LANDIS-II ran but did not produce expected Output Biomass Community files:\n  ",
      csv_path,
      "\n  ",
      tif_path,
      call. = FALSE
    )
  }
  as.character(csv_path)
}

#' Run one DEoptim trial for a candidate parameter vector (blocking)
#'
#' One calibration trial: copy `paths$scenario_template` into a scratch dir
#' under `paths$scratch_root`, patch `dynamic-fire.txt` with `par_vec`, run
#' LANDIS-II via the warm Docker pool (or a one-off Docker/local invocation if
#' no pool is supplied), parse the resulting Dynamic Fire logs.
#'
#' `paths` carries only file PATHS so the function is FORK-safe (no terra/sf
#' objects in the worker's environment).
#'
#' Isolation between trials in the same pool container: each trial uses a
#' unique scratch directory; `landis_pool_exec()` sets per-call env vars to
#' redirect dotnet caches; the trial directory is deleted after parsing unless
#' `keep_scratch = TRUE`.
#'
#' @param par_vec Numeric. Named candidate parameter vector.
#' @param par_names Character or NULL. Names in canonical order
#'   ([calibration_par_names()]). Used to re-attach names if DEoptim strips
#'   them when calling the objective function with positional args.
#' @param paths Named list of strings. Required entries:
#'   \describe{
#'     \item{scenario_template}{Directory built by
#'       [build_calibration_scenario_template()].}
#'     \item{scratch_root}{Where per-trial dirs are created. Must equal the
#'       pool's `scratch_root` when `pool` is supplied. NULL = `tempdir()`.}
#'   }
#' @param sim_years Integer. Calibration sim duration in years (informational;
#'   the actual Duration comes from the template's scenario.txt).
#' @param base_seed Integer. Random seed for this trial.
#' @param pool A `landis_pool` from [landis_pool_start()], or NULL for one-off.
#' @param pool_idx Integer. 1-based container index in `pool`. Required when
#'   `pool` is non-NULL.
#' @param method Character. `"docker"` or `"local"`. Used only when `pool` is
#'   NULL. Default from `getOption("landisutils.run.method")`.
#' @param pixel_area_ha Numeric. Hectares per cell. Default 1.
#' @param keep_scratch Logical. Leave the per-trial scratch dir in place for
#'   debugging. Default FALSE.
#'
#' @returns The output of [parse_dynamic_fire_logs()] for the trial's `rep01/`.
#'
#' @family Dynamic Fire calibration helpers
#'
#' @export
sim_landis <- function(
  par_vec,
  par_names = NULL,
  paths,
  sim_years,
  base_seed,
  pool = NULL,
  pool_idx = NULL,
  method = NULL,
  pixel_area_ha = 1.0,
  keep_scratch = FALSE
) {
  if (is.null(names(par_vec)) && !is.null(par_names)) {
    names(par_vec) <- par_names
  }
  stopifnot(
    is.numeric(par_vec),
    !is.null(names(par_vec)),
    is.list(paths),
    !is.null(paths$scenario_template),
    fs::dir_exists(paths$scenario_template)
  )
  scratch_root <- paths$scratch_root %||% tempdir()
  fs::dir_create(scratch_root)

  if (!is.null(pool)) {
    stopifnot(
      inherits(pool, "landis_pool"),
      !is.null(pool_idx),
      ## scratch_root must be inside the pool's bind-mount root so the container
      ## sees the trial dir.
      identical(fs::path_real(scratch_root), fs::path_real(pool$scratch_root))
    )
  }

  trial_dir <- fs::file_temp(pattern = "dynfire_trial_", tmp_dir = scratch_root)
  fs::dir_create(trial_dir)
  ## Default: clean up the trial scratch dir only on a CLEAN exit (so post-mortem
  ## LANDIS-II stdout/stderr stays available when a trial fails). `keep_scratch =
  ## TRUE` retains it unconditionally. Cleanup is gated on this local flag, which
  ## is set TRUE just before the function returns successfully.
  trial_succeeded <- FALSE
  if (!isTRUE(keep_scratch)) {
    on.exit(
      {
        if (isTRUE(trial_succeeded)) {
          try(fs::dir_delete(trial_dir), silent = TRUE)
        } else {
          message("sim_landis: trial scratch retained for diagnostics: ", trial_dir)
        }
      },
      add = TRUE
    )
  }

  ## Copy template -> scratch dir
  for (f in fs::dir_ls(paths$scenario_template, type = "file")) {
    fs::file_copy(f, fs::path(trial_dir, basename(f)))
  }
  ## Patch only the fire config
  patch_fire_config(trial_dir, par_vec)
  ## Per-rep dir with seed
  rep_dir <- fs::path(trial_dir, "rep01")
  if (fs::dir_exists(rep_dir)) {
    fs::dir_delete(rep_dir)
  }
  landis_replicate(
    scenario_dir = trial_dir,
    rep_index = 1L,
    files = NULL,
    base_seed = as.integer(base_seed)
  )

  if (!is.null(pool)) {
    ## Warm-pool path: docker exec into the assigned container, in the container-side
    ## path corresponding to rep_dir.
    rel_rep <- fs::path_rel(rep_dir, start = pool$scratch_root)
    container_workdir <- fs::path("/scratch", rel_rep)
    console <- getOption(
      "landisutils.docker.console",
      default = "/opt/landis-ii/Core-Model-v8-LINUX/build/Release/Landis.Console.dll"
    )
    log_dir <- fs::dir_create(fs::path(rep_dir, "log"))
    landis_pool_exec(
      pool = pool,
      idx = pool_idx,
      workdir = container_workdir,
      command = "dotnet",
      args = c(console, "scenario.txt"),
      stdout_log = fs::path(log_dir, "pool_stdout.log"),
      stderr_log = fs::path(log_dir, "pool_stderr.log")
    )
  } else {
    method <- method %||%
      getOption(
        "landisutils.run.method",
        default = if (.Platform$OS.type == "windows") "local" else "docker"
      )
    if (method == "docker") {
      landis_run_docker(scenario_dir = rep_dir, scenario_file = "scenario.txt")
    } else {
      landis_run_local(scenario_dir = rep_dir, scenario_file = "scenario.txt")
    }
  }

  result <- parse_dynamic_fire_logs(rep_dir, pixel_area_ha = pixel_area_ha)
  trial_succeeded <- TRUE ## triggers scratch cleanup in the on.exit handler
  result
}

#' Standalone-R Dynamic Fire reimplementation (stub)
#'
#' Reserved slot for a future pure-R reimplementation of LANDIS-II Dynamic Fire,
#' usable as a faster simulator backend for calibration. Signature matches
#' [sim_landis()] so it can be swapped in via the calibration driver's
#' `simulator` argument without touching the loss function or observed-target
#' contract.
#'
#' Currently raises; see the comparison-of-approaches discussion in the
#' Dynamic Fire calibration design notes for the rationale.
#'
#' @param ... Same shape as [sim_landis()].
#'
#' @family Dynamic Fire calibration helpers
#'
#' @export
sim_r_reimpl <- function(...) {
  stop("sim_r_reimpl() not yet implemented; use sim_landis() for now.", call. = FALSE)
}


## Phase 8e: DEoptim driver + mock simulator for tests ------------------------------------------

#' Mock simulator backend for testing the calibration driver without LANDIS-II
#'
#' Returns plausibly-shaped [parse_dynamic_fire_logs()] output without invoking
#' the real simulator. The output varies with `par_vec` so DEoptim sees a
#' non-trivial loss surface (a few of the calibrated parameters bias the mock's
#' fire count and size distribution; this is illustrative, not biophysical).
#'
#' Use this in unit tests of [calibrate_dynamic_fire()] when Docker is not
#' available; do NOT use for actual calibration.
#'
#' @param par_vec Numeric. Named candidate parameter vector.
#' @param par_names Character. Names in canonical order ([calibration_par_names()]).
#' @param paths Named list. Currently unused; accepted for [sim_landis()] signature parity.
#' @param sim_years Integer. Number of simulated years.
#' @param base_seed Integer. RNG seed for deterministic mock output.
#' @param ... Ignored. Lets callers pass `pool`, `pool_idx`, `method`, etc.
#'
#' @returns A list matching the shape of [parse_dynamic_fire_logs()] output.
#'
#' @family Dynamic Fire calibration helpers
#'
#' @export
sim_mock <- function(
  par_vec,
  par_names = NULL,
  paths = NULL,
  sim_years = 10L,
  base_seed = 1L,
  ...
) {
  if (is.null(names(par_vec)) && !is.null(par_names)) {
    names(par_vec) <- par_names
  }
  set.seed(base_seed)
  ## Couple fire count to SeverityCalibrationFactor + Sp/SumHiProp so the loss
  ## surface has detectable gradient.
  lambda <- max(
    0.5,
    8 *
      (par_vec[["SeverityCalibrationFactor"]] %||% 1) *
      (1 + (par_vec[["SpHiProp"]] %||% 0)) *
      (1 + (par_vec[["SumHiProp"]] %||% 0)) /
      4
  )
  n_fires_per_year <- as.integer(stats::rpois(sim_years, lambda = lambda))
  total_fires <- sum(n_fires_per_year)
  fire_sizes_ha <- if (total_fires > 0L) {
    sort(stats::rlnorm(
      total_fires,
      meanlog = 3 + 1.5 * (par_vec[["IgnProb_Conifer"]] %||% 1),
      sdlog = 2
    ))
  } else {
    numeric(0)
  }
  list(
    n_fires_by_year = tibble::tibble(
      year = as.integer(seq_len(sim_years)),
      n_fires = n_fires_per_year
    ),
    fire_sizes_ha = fire_sizes_ha,
    events = tibble::tibble(
      year = if (total_fires > 0L) rep.int(seq_len(sim_years), n_fires_per_year) else integer(0),
      eco = if (total_fires > 0L) rep("MOCK", total_fires) else character(0),
      init_fuel = if (total_fires > 0L) rep(2L, total_fires) else integer(0),
      sites = as.integer(fire_sizes_ha),
      mean_severity = if (total_fires > 0L) {
        stats::runif(total_fires, min = 0, max = 5)
      } else {
        numeric(0)
      }
    ),
    total_sites_burned = sum(as.integer(fire_sizes_ha)),
    n_events = length(fire_sizes_ha),
    ## sim_mock is not running LANDIS, so there are no severity x FuelType
    ## rasters to intersect. Loss function falls back to event-based
    ## attribution for any rep without `area_by_fuel_ha`.
    area_by_fuel_ha = NULL
  )
}

## Internal pre-flight validation. Catches common cfg / scenario / payload
## errors before any expensive resource setup (pool / cluster / first trial).
## Errors are fail-fast; soft issues (e.g., NP < 10 * npar advisory, missing
## severity_dist with non-zero severity weight) are warnings.
.preflight_calibrate <- function(cfg, par_names, template_dir, observed, scratch_root) {
  npar <- length(par_names)

  ## ---- cfg shape -----------------------------------------------------------
  bad_bounds <- par_names[cfg$lower[par_names] >= cfg$upper[par_names]]
  if (length(bad_bounds) > 0L) {
    stop(
      "cfg$lower must be strictly less than cfg$upper for every parameter; ",
      "violations: ",
      paste(bad_bounds, collapse = ", "),
      call. = FALSE
    )
  }
  NP <- as.integer(cfg$NP %||% 60L)
  itermax <- as.integer(cfg$itermax %||% 100L)
  n_reps <- as.integer(cfg$n_reps %||% 5L)
  if (NP < 4L) {
    stop("cfg$NP must be >= 4 (DEoptim minimum); got ", NP, call. = FALSE)
  }
  if (itermax < 1L) {
    stop("cfg$itermax must be >= 1; got ", itermax, call. = FALSE)
  }
  if (n_reps < 1L) {
    stop("cfg$n_reps must be >= 1; got ", n_reps, call. = FALSE)
  }
  if (NP < 10L * npar) {
    message(
      sprintf("calibrate_dynamic_fire: NP (%d) < 10 * length(par_names) (= %d); ", NP, 10L * npar),
      "DEoptim will issue an advisory warning. Bump NP to ~",
      10L * npar,
      " for production calibration runs."
    )
  }

  ## weights: at least one component must be non-zero, otherwise DEoptim has
  ## nothing to optimise
  w <- cfg$weights %||% c(count = 1, size = 1, area_fuel = 0, severity = 0)
  if (all(w == 0)) {
    stop(
      "cfg$weights are all zero; DEoptim has nothing to optimise. ",
      "Set at least one of count / size / area_fuel / severity to > 0.",
      call. = FALSE
    )
  }
  unknown_w <- setdiff(names(w), c("count", "size", "area_fuel", "severity"))
  if (length(unknown_w) > 0L) {
    warning(
      "cfg$weights has unrecognised components (ignored): ",
      paste(unknown_w, collapse = ", "),
      call. = FALSE
    )
  }

  ## ---- simulator name (cheap enum check, do early) ------------------------
  simulator_name <- cfg$simulator %||% "landis"
  if (!simulator_name %in% c("landis", "r_reimpl", "mock")) {
    stop("Unknown simulator: ", simulator_name, call. = FALSE)
  }

  ## ---- scenario template ---------------------------------------------------
  ## sim_mock / sim_r_reimpl don't actually invoke LANDIS-II, so the full
  ## set of LANDIS-II input files isn't needed. Only sim_landis requires it.
  ## Even for mock / r_reimpl we still expect scenario.txt to exist (caller
  ## already passed its path to calibrate_dynamic_fire) -- skip the rest.
  required_files <- .calibration_required_files(template_dir, simulator_name)
  missing_files <- required_files[!fs::file_exists(fs::path(template_dir, required_files))]
  if (length(missing_files) > 0L) {
    stop(
      "calibration scenario template at ",
      template_dir,
      " is missing required files: ",
      paste(missing_files, collapse = ", "),
      ". Did you call build_calibration_scenario_template() first?",
      call. = FALSE
    )
  }

  ## ---- observed payload shape ---------------------------------------------
  primary <- observed$primary %||% observed$fru59
  if (is.null(primary)) {
    stop(
      "observed_targets payload is missing $primary (or back-compat $fru59); ",
      "did save_observed_fire_targets() complete successfully?",
      call. = FALSE
    )
  }
  required_obs <- c("lambda_obs", "n_fires_by_year", "fire_sizes_ha")
  missing_obs <- required_obs[!required_obs %in% names(primary)]
  if (length(missing_obs) > 0L) {
    stop(
      "observed$primary is missing required fields: ",
      paste(missing_obs, collapse = ", "),
      call. = FALSE
    )
  }
  if (!is.numeric(primary$lambda_obs) || length(primary$lambda_obs) != 1L) {
    stop("observed$primary$lambda_obs must be a numeric scalar.", call. = FALSE)
  }

  ## ---- weight / observed coherence (warnings only) ------------------------
  if (
    (w["area_fuel"] %||% 0) > 0 &&
      (is.null(primary$area_by_fuel_ha) || is.null(observed$fuel_code_to_base))
  ) {
    warning(
      "cfg$weights['area_fuel'] > 0 but the observed payload is missing ",
      "area_by_fuel_ha or fuel_code_to_base; L_area_fuel will contribute 0. ",
      "Either set the weight to 0 or populate the payload via ",
      "save_observed_fire_targets() with a fuel-code mapping.",
      call. = FALSE
    )
  }
  if ((w["severity"] %||% 0) > 0 && is.null(primary$severity_dist)) {
    warning(
      "cfg$weights['severity'] > 0 but observed$primary$severity_dist is NULL; ",
      "L_severity will contribute 0. Pass `severity_dist = ",
      "default_severity_prior_sturtevant2009()` to save_observed_fire_targets().",
      call. = FALSE
    )
  }

  ## ---- method coherence (simulator name was validated earlier) ------------
  method <- cfg$method %||%
    getOption(
      "landisutils.run.method",
      default = if (.Platform$OS.type == "windows") "local" else "docker"
    )
  if (simulator_name == "landis") {
    if (method == "docker") {
      docker_rc <- suppressWarnings(system2("docker", "version", stdout = FALSE, stderr = FALSE))
      if (!identical(as.integer(docker_rc), 0L)) {
        stop(
          "simulator = 'landis' + method = 'docker' but `docker version` failed. ",
          "Either install Docker, point cfg$method = 'local', or use ",
          "simulator = 'mock' for testing.",
          call. = FALSE
        )
      }
    } else if (method == "local") {
      console <- landis_find()
      if (is.null(console) || is.na(console) || !nzchar(console)) {
        stop(
          "simulator = 'landis' + method = 'local' but landis_find() did not ",
          "return a usable Landis.Console.dll path. Set the LANDIS_CONSOLE env ",
          "var or use method = 'docker'.",
          call. = FALSE
        )
      }
    }
  }

  ## ---- scratch root writability -------------------------------------------
  if (!fs::dir_exists(scratch_root)) {
    stop("scratch_root does not exist: ", scratch_root, call. = FALSE)
  }
  test_file <- fs::file_temp(pattern = "preflight_", tmp_dir = scratch_root, ext = ".test")
  ok <- tryCatch(
    {
      writeLines("ok", test_file)
      fs::file_delete(test_file)
      TRUE
    },
    error = function(e) FALSE
  )
  if (!isTRUE(ok)) {
    stop("scratch_root is not writable: ", scratch_root, call. = FALSE)
  }

  invisible(TRUE)
}

## Internal: available host RAM in GiB. Reads /proc/meminfo MemAvailable (falling back to MemTotal) on
## Linux; returns NA elsewhere so the caller skips RAM-capping rather than guessing.
.available_ram_gb <- function() {
  mi <- tryCatch(readLines("/proc/meminfo", n = 50L), error = function(e) character(0))
  for (key in c("^MemAvailable:", "^MemTotal:")) {
    ln <- grep(key, mi, value = TRUE)
    if (length(ln) > 0L) {
      return(as.numeric(sub("\\D*(\\d+).*", "\\1", ln[1])) / 1024^2) ## kB -> GiB
    }
  }
  NA_real_
}

## Internal: parse a docker `--memory` string ("8g", "512m", "16gib") to GiB. NULL/empty -> NA.
.mem_limit_to_gb <- function(x) {
  if (is.null(x) || !nzchar(x)) {
    return(NA_real_)
  }
  num <- as.numeric(sub("([0-9.]+).*", "\\1", x))
  unit <- tolower(sub("[0-9.]+\\s*", "", x))
  switch(unit, g = , gb = , gib = num, m = , mb = , mib = num / 1024, num)
}

## Internal: cap a requested warm-pool size `n` by the RAM budget. Each container holds a full LANDIS
## landscape in memory, so `n` containers can exceed host RAM and OOM. Returns
## min(n, floor(avail_gb * mem_fraction / mem_per_worker_gb)), or `n` unchanged when RAM or the
## per-worker estimate is unknown (so capping is opt-in via cfg$mem_per_worker_gb).
.ram_pool_cap <- function(
  n,
  mem_per_worker_gb,
  mem_fraction = 0.85,
  avail_gb = .available_ram_gb()
) {
  if (!isTRUE(is.finite(avail_gb) && is.finite(mem_per_worker_gb) && mem_per_worker_gb > 0)) {
    return(as.integer(n))
  }
  cap <- max(1L, as.integer(floor(avail_gb * mem_fraction / mem_per_worker_gb)))
  min(as.integer(n), cap)
}

#' DEoptim driver for Dynamic Fire calibration
#'
#' Sets up a warm Docker pool (for the `landis` simulator on Docker) and a FORK
#' cluster of `n_cores` workers, then invokes [DEoptim::DEoptim()] with the
#' multi-component loss as the objective. Pool + cluster are torn down via
#' `on.exit()` regardless of success / error / interrupt.
#'
#' Designed to be called from a `tar_target` with `deployment = "main"` so the
#' outer `targets` crew doesn't try to dispatch this as a single worker while
#' it manages its own internal cluster.
#'
#' Per-worker container assignment: each FORK worker sets its
#' `LANDIS_POOL_CONTAINER_IDX` env var to its 1-based pool index.
#' [sim_landis()] reads this when running inside the worker.
#'
#' DEoptim is gated on `requireNamespace("DEoptim")`; install via
#' `renv::install("DEoptim")` before calling.
#'
#' @param observed_targets_path Character. Path to the `.rds` from
#'   [save_observed_fire_targets()].
#' @param scenario_template Character. Path to the calibration scenario's
#'   `scenario.txt` (the return of [build_calibration_scenario_template()]).
#' @param cfg List. Calibration config. Expected keys:
#'   \describe{
#'     \item{lower, upper}{Named numeric vectors keyed by [calibration_par_names()].}
#'     \item{NP, itermax, strategy}{DEoptim control args.}
#'     \item{reltol, steptol}{Optional DEoptim early-stopping controls. When set,
#'       DEoptim halts before `itermax` if the best-of-population objective fails
#'       to improve by more than `reltol` for `steptol` consecutive generations.
#'       Defaults: `reltol = 1e-3` (0.1% relative improvement) and
#'       `steptol = 25` generations. Pass `steptol = itermax` (or any value `>=
#'       itermax`) to disable early stopping and always run the full schedule.
#'       Pass `cfg$steptol = NULL` to fall back to the upstream DEoptim default
#'       (`steptol = itermax`).}
#'     \item{n_reps, sim_years, weights, base_seed}{Per-trial settings.}
#'     \item{n_cores, parallel}{Parallelism settings.}
#'     \item{simulator}{`"landis"` (default), `"r_reimpl"`, or `"mock"`.}
#'     \item{method}{`"docker"` (default) or `"local"`.}
#'     \item{image, cpu_limit, mem_limit, pull}{Pool settings (Docker only).}
#'   }
#' @param out_dir Character. Where to write the DEoptim trace + scratch
#'   sub-directory. Created if missing.
#'
#' @returns List with `best_params` (named numeric), `objective` (scalar),
#'   `deoptim` (full DEoptim return), `trace_path` (per-iter best-value CSV
#'   path), `trial_trace_path` (per-trial loss-decomposition CSV path, with
#'   one row per `objfn` evaluation; columns: `wall_clock_iso`, `pid`,
#'   `par_<name>...`, `total`, `comp_<name>...`, `w_<name>...`,
#'   `weighted_<name>...`. Useful for plotting how DEoptim trades off the
#'   four loss components over iterations.), `cfg` (echo), `pool_image` /
#'   `pool_digest` (provenance; NA when no pool was started).
#'
#' @family Dynamic Fire calibration helpers
#'
#' @export
calibrate_dynamic_fire <- function(observed_targets_path, scenario_template, cfg, out_dir) {
  if (!requireNamespace("DEoptim", quietly = TRUE)) {
    stop(
      "Package `DEoptim` is required for calibrate_dynamic_fire() but is not installed. ",
      "Install via `renv::install('DEoptim')`.",
      call. = FALSE
    )
  }
  stopifnot(
    is.character(observed_targets_path),
    length(observed_targets_path) == 1L,
    fs::file_exists(observed_targets_path),
    is.character(scenario_template),
    length(scenario_template) == 1L,
    fs::file_exists(scenario_template),
    is.list(cfg)
  )
  fs::dir_create(out_dir)

  template_dir <- fs::path_real(dirname(scenario_template))
  ## Per-trial scratch dir for the warm Docker pool. Defaults to
  ## `<out_dir>/scratch`, but `cfg$scratch_root` lets callers route the bind
  ## mount to docker-visible storage when `out_dir` lives on a filesystem the
  ## Docker daemon cannot see (e.g. user-space autofs / sshfs / NFS mounts).
  scratch_root <- fs::path_real(fs::dir_create(cfg$scratch_root %||% fs::path(out_dir, "scratch")))
  observed <- readRDS(observed_targets_path)
  par_names <- calibration_par_names()
  stopifnot(setequal(names(cfg$lower), par_names), setequal(names(cfg$upper), par_names))
  cfg$lower <- cfg$lower[par_names]
  cfg$upper <- cfg$upper[par_names]

  ## Pre-flight checks: fail fast on common config / scenario / payload errors
  ## BEFORE starting the warm pool or FORK cluster.
  .preflight_calibrate(
    cfg = cfg,
    par_names = par_names,
    template_dir = template_dir,
    observed = observed,
    scratch_root = scratch_root
  )

  paths <- list(scenario_template = template_dir, scratch_root = scratch_root)
  n_reps <- as.integer(cfg$n_reps %||% 5L)
  weights <- cfg$weights %||% c(count = 1, size = 1, area_fuel = 0, severity = 0)
  base_seed <- as.integer(cfg$base_seed %||% 12345L)
  sim_years <- as.integer(cfg$sim_years %||% 10L)
  simulator_name <- cfg$simulator %||% "landis"
  simulator <- switch(
    simulator_name,
    landis = sim_landis,
    r_reimpl = sim_r_reimpl,
    mock = sim_mock,
    stop("Unknown simulator: ", simulator_name, call. = FALSE)
  )
  method <- cfg$method %||%
    getOption(
      "landisutils.run.method",
      default = if (.Platform$OS.type == "windows") "local" else "docker"
    )

  ## Default core count: prefer `parallelly::availableCores(constraints =
  ## "connections", omit = 2)` if available, since `parallel::detectCores()`
  ## ignores R's per-session connection cap (~125) and over-provisions FORK
  ## clusters on very large hosts (e.g. 256 logical cores). `omit = 2`
  ## reserves two cores for the main session and shell. Fall back to
  ## detectCores() so parallelly remains an optional dependency.
  default_cores <- if (requireNamespace("parallelly", quietly = TRUE)) {
    as.integer(parallelly::availableCores(constraints = "connections", omit = 2L))
  } else {
    max(1L, parallel::detectCores() - 2L)
  }
  n_cores <- as.integer(cfg$n_cores %||% default_cores)

  ## RAM-aware pool cap: each warm container holds a full LANDIS landscape in memory, so per-container
  ## RAM scales with the study-area cell count (~3 GiB for a ~300k-active-cell LU; ~8x for a district).
  ## A core-based count can exceed host RAM and OOM (90 x 22 GiB ~ 2 TB on a 1 TB host). Cap the pool at
  ## the RAM budget; opt-in via cfg$mem_per_worker_gb (else falls back to the mem_limit cap, so existing
  ## small-area configs are unchanged). cfg$mem_fraction (default 0.85) reserves RAM for the OS + the
  ## FORK workers + the main R process.
  mem_per_worker <- as.numeric(cfg$mem_per_worker_gb %||% .mem_limit_to_gb(cfg$mem_limit %||% "8g"))
  avail_gb <- .available_ram_gb()
  capped_cores <- .ram_pool_cap(n_cores, mem_per_worker, cfg$mem_fraction %||% 0.85, avail_gb)
  if (capped_cores < n_cores) {
    message(glue::glue(
      "calibrate_dynamic_fire: RAM-capping warm pool {n_cores} -> {capped_cores} container(s) ",
      "({round(avail_gb)} GiB avail x {cfg$mem_fraction %||% 0.85} / {round(mem_per_worker, 1)} ",
      "GiB/worker). Set cfg$mem_per_worker_gb to tune."
    ))
    n_cores <- capped_cores
  }
  use_parallel <- isTRUE(cfg$parallel %||% TRUE) && n_cores > 1L

  ## Pool lifecycle: only LANDIS-II Docker + parallel needs a pool. Mock /
  ## r_reimpl / local-method runs don't touch Docker.
  pool <- NULL
  if (simulator_name == "landis" && method == "docker" && use_parallel) {
    pool <- landis_pool_start(
      n = n_cores,
      image = cfg$image,
      scratch_root = scratch_root,
      cpu_limit = cfg$cpu_limit %||% 2,
      ## --memory must be >= expected per-container usage or docker OOM-kills the container; derive from
      ## the RAM estimate (+25% headroom) when not set explicitly. Default 8g preserves small-area configs.
      mem_limit = cfg$mem_limit %||% sprintf("%dg", max(8L, ceiling(mem_per_worker * 1.25))),
      pull = isTRUE(cfg$pull %||% FALSE),
      name_prefix = paste0("landis-cal-", Sys.getpid())
    )
    ## Tear down the pool before the cluster (FORK children inherit the pool's
    ## state but don't own its containers; clean up containers first).
    on.exit(landis_pool_stop(pool), add = TRUE)
  }

  ## FORK cluster -- workers inherit the parent's environment including `pool`.
  cl <- NULL
  if (use_parallel && .Platform$OS.type != "windows") {
    cl <- parallel::makeCluster(n_cores, type = "FORK")
    on.exit(parallel::stopCluster(cl), add = TRUE, after = FALSE)
    if (!is.null(pool)) {
      parallel::clusterApply(cl, seq_len(n_cores), function(i) {
        Sys.setenv(LANDIS_POOL_CONTAINER_IDX = as.character(i))
      })
    }
  }

  ## Per-trial loss-component CSV. Each worker (FORK child + main) appends to
  ## its own file keyed by PID so concurrent writes don't collide; the files
  ## are concatenated into `trial_trace.csv` after DEoptim returns. The trace
  ## captures (par_vec, total, components) for every objfn evaluation, which
  ## downstream visualisations can use to plot per-component loss evolution
  ## (not just the per-iter best total that DEoptim already records).
  trial_trace_dir <- fs::dir_create(fs::path(
    out_dir,
    sprintf("trial_trace_%s", format(Sys.time(), "%Y%m%d_%H%M%S"))
  ))

  objfn <- function(par_vec) {
    names(par_vec) <- par_names
    pool_idx <- if (!is.null(pool)) {
      as.integer(Sys.getenv("LANDIS_POOL_CONTAINER_IDX", "1"))
    } else {
      NULL
    }
    reps <- lapply(seq_len(n_reps), function(i) {
      simulator(
        par_vec = par_vec,
        par_names = par_names,
        paths = paths,
        sim_years = sim_years,
        base_seed = base_seed + i,
        pool = pool,
        pool_idx = pool_idx,
        method = method
      )
    })
    .loss <- loss_from_stats(reps, observed, weights)
    ## Append a row to this worker's trial-trace CSV. Header is written
    ## lazily on the first write of each PID.
    .write_trial_trace_row(
      dir = trial_trace_dir,
      par_vec = par_vec,
      par_names = par_names,
      total = .loss$total,
      components = .loss$components,
      weights = .loss$weights
    )
    .loss$total
  }

  control_args <- list(
    NP = as.integer(cfg$NP %||% 60L),
    itermax = as.integer(cfg$itermax %||% 100L),
    strategy = as.integer(cfg$strategy %||% 3L),
    trace = isTRUE(cfg$trace %||% TRUE),
    storepopfrom = 1L,
    storepopfreq = 5L,
    ## Early-stopping (DEoptim halts when bestvalit fails to improve by more
    ## than `reltol` for `steptol` consecutive generations). Caller can disable
    ## by setting cfg$steptol >= cfg$itermax or by passing cfg$steptol = NULL
    ## (the latter falls through to DEoptim's upstream default of steptol =
    ## itermax, i.e. never stop early).
    reltol = as.numeric(cfg$reltol %||% 1e-3),
    steptol = as.integer(cfg$steptol %||% 25L)
  )
  if (!is.null(cl)) {
    ## DEoptim 2.2.8: the `ctrl$cluster` branch uses the supplied cluster
    ## without binding a local `cl` variable, but the post-loop cleanup runs
    ## `parallel::stopCluster(cl)` whenever `parallelType == "parallel"`,
    ## which errors with `object 'cl' not found`. Leave parallelType at its
    ## default ("none") so DEoptim skips that cleanup path -- we still get
    ## the parallel objfn evaluation because `parApply(cl = ctrl$cluster, ...)`
    ## fires from the `!is.null(ctrl$cluster)` branch -- and our on.exit
    ## handler stops the FORK cluster.
    control_args$cluster <- cl
  }
  control <- do.call(DEoptim::DEoptim.control, control_args)

  message(glue::glue(
    "calibrate_dynamic_fire: simulator={simulator_name}, NP={control_args$NP}, ",
    "itermax={control_args$itermax}, reltol={control_args$reltol}, ",
    "steptol={control_args$steptol}, n_reps={n_reps}, sim_years={sim_years}, ",
    "n_cores={if (is.null(cl)) 1L else n_cores}, pool={!is.null(pool)}"
  ))

  res <- DEoptim::DEoptim(
    fn = objfn,
    lower = unname(cfg$lower),
    upper = unname(cfg$upper),
    control = control
  )

  best_params <- stats::setNames(as.numeric(res$optim$bestmem), par_names)
  trace_path <- fs::path(
    out_dir,
    sprintf("deoptim_trace_%s.csv", format(Sys.time(), "%Y%m%d_%H%M%S"))
  )
  utils::write.csv(
    data.frame(iter = seq_along(res$member$bestvalit), best_value = res$member$bestvalit),
    trace_path,
    row.names = FALSE
  )

  ## Merge per-worker trial traces into a single CSV. Workers may have produced
  ## zero rows (mock simulator, FORK initialisation) which we tolerate.
  trial_trace_path <- fs::path(
    out_dir,
    sprintf("trial_trace_%s.csv", format(Sys.time(), "%Y%m%d_%H%M%S"))
  )
  worker_files <- fs::dir_ls(trial_trace_dir, glob = "*.csv")
  if (length(worker_files) > 0L) {
    .merge_trial_trace(files = worker_files, out_path = trial_trace_path)
  } else {
    trial_trace_path <- NA_character_
  }
  ## Best-effort cleanup of the per-worker scratch dir.
  tryCatch(fs::dir_delete(trial_trace_dir), error = function(e) invisible(NULL))

  list(
    best_params = best_params,
    objective = as.numeric(res$optim$bestval),
    deoptim = res,
    trace_path = as.character(trace_path),
    trial_trace_path = as.character(trial_trace_path),
    cfg = cfg,
    pool_image = if (!is.null(pool)) pool$image else NA_character_,
    pool_digest = if (!is.null(pool)) pool$digest else NA_character_
  )
}

## Append a single row of per-trial loss-decomposition data to this worker's
## sidecar CSV. Header is written lazily on first write per PID so concurrent
## FORK workers don't collide.
.write_trial_trace_row <- function(dir, par_vec, par_names, total, components, weights) {
  pid <- Sys.getpid()
  f <- fs::path(dir, sprintf("worker_%d.csv", pid))
  comp_names <- names(components)
  weight_vals <- as.numeric(weights[comp_names])
  weighted <- as.numeric(components) * weight_vals
  ## Row schema: wall_clock_iso, pid, par_<name>..., total, comp_<name>..., w_<name>..., weighted_<name>...
  row <- c(
    list(wall_clock_iso = format(Sys.time(), "%Y-%m-%dT%H:%M:%OS3%z"), pid = pid),
    stats::setNames(as.list(as.numeric(par_vec[par_names])), paste0("par_", par_names)),
    list(total = total),
    stats::setNames(as.list(as.numeric(components)), paste0("comp_", comp_names)),
    stats::setNames(as.list(weight_vals), paste0("w_", comp_names)),
    stats::setNames(as.list(weighted), paste0("weighted_", comp_names))
  )
  is_new <- !fs::file_exists(f)
  utils::write.table(
    as.data.frame(row, stringsAsFactors = FALSE),
    file = f,
    sep = ",",
    row.names = FALSE,
    col.names = is_new,
    append = !is_new,
    quote = FALSE
  )
  invisible(NULL)
}

## Merge per-worker trial-trace CSVs into a single CSV, in wall-clock order
## (approximate DEoptim evaluation order). Each file keeps its own header; we
## use the first non-empty file's header as the reference.
.merge_trial_trace <- function(files, out_path) {
  dfs <- lapply(files, function(f) {
    tryCatch(utils::read.csv(f, stringsAsFactors = FALSE), error = function(e) NULL)
  })
  dfs <- dfs[!vapply(dfs, is.null, logical(1))]
  if (length(dfs) == 0L) {
    return(invisible(NULL))
  }
  ref_cols <- colnames(dfs[[1]])
  dfs <- lapply(dfs, function(d) d[, ref_cols, drop = FALSE])
  merged <- do.call(rbind, dfs)
  ## Sort by wall-clock if column present; otherwise leave as worker-ordered.
  if ("wall_clock_iso" %in% colnames(merged)) {
    merged <- merged[order(merged$wall_clock_iso), , drop = FALSE]
  }
  utils::write.csv(merged, out_path, row.names = FALSE)
  invisible(out_path)
}

## ---- post-calibration validation ----------------------------------------------------------------
##
## Re-simulate at the calibrated parameter vector to recover the per-trial fire
## statistics (sizes, severity, area-by-fuel) that DEoptim does not retain, for
## the goodness-of-fit section of a model-calibration report. Reuses the same
## scenario template, scratch root, and Docker pool plumbing as the main
## calibration loop ([sim_landis()] / [landis_pool_start()]). De-duplicated
## from the BC_HRV / gitanyow-partial-harvest report-pipeline templates;
## succession-backend-agnostic.

#' Re-simulate at the calibrated parameter vector for goodness-of-fit plots
#'
#' Runs `n_reps` replicate Dynamic Fire simulations at the calibrated
#' parameter vector and returns their per-replicate fire statistics plus the
#' loss against the observed targets, so a calibration report can plot the
#' goodness-of-fit that [calibrate_dynamic_fire()] does not retain.
#'
#' @param scenario_template Character path to `scenario.txt` inside the
#'   calibration scenario directory.
#' @param best_params Named numeric vector of calibrated parameters (the
#'   `calibrate_dynamic_fire()` result's `best_params`).
#' @param observed_targets_path Character path to the saved observed-targets
#'   `.rds` (the [save_observed_fire_targets()] output); reattached to the
#'   return value so the report has a single self-contained input.
#' @param cfg The `calibration_config` list (for `sim_years`, `weights`).
#' @param n_reps Integer number of replicate simulations at `best_params`.
#' @param scratch_root Docker-visible host directory for the warm pool
#'   bind-mount (distinct from the main calibration scratch so concurrent runs
#'   do not collide).
#' @param base_seed Base seed for the validation reps (`base_seed + i` per
#'   rep).
#'
#' @return A list with `reps`, `best_params`, `observed`, `pixel_area_ha`,
#'   `sim_years`, `n_reps`, `loss`, `pool_image`, and `pool_digest`.
#'
#' @family Dynamic Fire calibration helpers
#'
#' @export
run_calibration_validation <- function(
  scenario_template,
  best_params,
  observed_targets_path,
  cfg,
  n_reps = 20L,
  scratch_root,
  base_seed = 99999L
) {
  stopifnot(
    is.character(scenario_template),
    is.numeric(best_params),
    !is.null(names(best_params)),
    is.character(observed_targets_path),
    fs::file_exists(observed_targets_path),
    is.list(cfg),
    is.numeric(n_reps),
    n_reps >= 1L
  )
  fs::dir_create(scratch_root)

  pool <- landis_pool_start(
    n = as.integer(n_reps),
    scratch_root = scratch_root,
    cpu_limit = 2, ## LANDIS-II is single-threaded (~1 core/container); 2 packs more reps per node
    mem_limit = "8g",
    name_prefix = "landis-validate"
  )
  on.exit(landis_pool_stop(pool), add = TRUE)

  template_dir <- fs::path_real(dirname(scenario_template))
  paths <- list(scenario_template = template_dir, scratch_root = pool$scratch_root)

  reps <- lapply(seq_len(as.integer(n_reps)), function(i) {
    sim_landis(
      par_vec = best_params,
      paths = paths,
      sim_years = as.integer(cfg$sim_years %||% 10L),
      base_seed = as.integer(base_seed) + i,
      pool = pool,
      pool_idx = i,
      method = "docker"
    )
  })

  observed <- readRDS(observed_targets_path)
  loss <- loss_from_stats(reps = reps, observed = observed, weights = cfg$weights)

  list(
    reps = reps,
    best_params = best_params,
    observed = observed,
    pixel_area_ha = observed$pixel_area_ha %||% NA_real_,
    sim_years = as.integer(cfg$sim_years %||% 10L),
    n_reps = as.integer(n_reps),
    loss = loss,
    pool_image = pool$image,
    pool_digest = pool$digest
  )
}
