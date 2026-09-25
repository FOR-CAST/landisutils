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
#' @section A catalogue, not a parameter set:
#' Two entries are alternative ways to express the same quantity: `NumFires` gives every fire
#' ecoregion one ignition rate, `NumFiresMultiplier` scales each ecoregion's own. A calibration
#' carrying both would have two parameters for one degree of freedom, so it is refused. Anything
#' building "the full set" from this vector should drop one of the two.
#'
#' @returns Character vector of length 12.
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
    "IgnProb_Open",
    "NumFires",
    "NumFiresMultiplier",
    "DamageAgeMultiplier"
  )
}

## The two ignition-rate parameters are alternatives, not companions (internal).
##
## `NumFires` sets every fire ecoregion's rate to one value; `NumFiresMultiplier` scales each
## ecoregion's own rate by a common factor. A landscape whose ecoregions carry markedly different
## rates -- 0.57/yr in one and 17/yr in another is a real case -- loses that structure entirely
## under the first and keeps it under the second. Carrying both is ambiguous rather than additive,
## so it is refused where a caller first supplies them.
.check_num_fires_params <- function(nms) {
  if (all(c("NumFires", "NumFiresMultiplier") %in% nms)) {
    stop(
      "`NumFires` and `NumFiresMultiplier` cannot both be calibrated: the first sets every fire ",
      "ecoregion's ignition rate to one value, the second scales each ecoregion's own rate.",
      call. = FALSE
    )
  }
  invisible(nms)
}

## Scale ignition rates (internal).
##
## Shared by `patch_fire_config()`, which rewrites a calibration trial's config text, and
## `apply_calibrated_num_fires()`, which rewrites a production table, so the two cannot drift.
## The rate is a Poisson mean, so it is not rounded.
.scale_num_fires <- function(rates, mult) {
  if (!is.numeric(mult) || length(mult) != 1L || !is.finite(mult) || mult < 0) {
    stop(
      "NumFiresMultiplier must be a non-negative finite number, not ",
      paste(format(mult), collapse = ", "),
      call. = FALSE
    )
  }
  if (anyNA(rates)) {
    stop(
      "cannot scale an ignition rate that is not a number; the FireSizesTable's NumFires column ",
      "holds a missing or unparseable value",
      call. = FALSE
    )
  }
  rates * mult
}

## Safe read of one calibrated parameter, with a default when it is not being calibrated.
##
## `par_vec[["name"]]` on an ATOMIC vector raises a subscript error for a missing name -- it does
## NOT return NULL -- so the idiom `par_vec[["x"]] %||% default` does not protect anything: the
## error fires before `%||%` ever sees a value. That mattered once the calibrated set became a
## subset of `calibration_par_names()`.
.par <- function(par_vec, name, default = NULL) {
  if (!is.null(names(par_vec)) && name %in% names(par_vec)) par_vec[[name]] else default
}

## LANDIS-II writes CSVs with a trailing comma -> empty trailing column.
## Strip it so downstream column references aren't misaligned.
.read_landis_csv <- function(p) {
  df <- utils::read.csv(p, header = TRUE, check.names = FALSE, stringsAsFactors = FALSE)
  blank <- vapply(df, function(col) all(is.na(col) | col == ""), logical(1))
  df[, !blank, drop = FALSE]
}

## Hectares per cell from a scenario directory's `scenario.txt` `CellLength` (metres) (internal).
.scenario_pixel_area_ha <- function(scenario_dir) {
  f <- fs::path(scenario_dir, "scenario.txt")
  if (!fs::file_exists(f)) {
    stop("scenario.txt not found in ", scenario_dir, "; cannot read CellLength", call. = FALSE)
  }
  ln <- grep("^\\s*CellLength\\b", readLines(f, warn = FALSE), value = TRUE)
  len <- suppressWarnings(as.numeric(strsplit(trimws(ln[1L]), "\\s+")[[1]][2L]))
  if (length(ln) != 1L || !is.finite(len) || len <= 0) {
    stop("Expected one numeric CellLength in ", f, call. = FALSE)
  }
  len^2 / 10000
}

## The cell area a trial's sizes are converted with: the scenario's own, unless a caller supplied
## one, which must then agree with it (internal).
.resolve_pixel_area_ha <- function(scenario_dir, pixel_area_ha = NULL) {
  from_scenario <- .scenario_pixel_area_ha(scenario_dir)
  if (!is.null(pixel_area_ha) && !isTRUE(all.equal(pixel_area_ha, from_scenario))) {
    stop(
      "pixel_area_ha = ",
      pixel_area_ha,
      " disagrees with the scenario's CellLength, which gives ",
      from_scenario,
      " ha per cell",
      call. = FALSE
    )
  }
  from_scenario
}

## The observed targets carry the cell area of the fuel raster they were built on. If it differs
## from the simulated landscape's, simulated and observed areas are on different scales (internal).
.check_observed_pixel_area <- function(observed, scenario_dir) {
  ## A mock-simulator template need not be a real scenario: without a CellLength there is no
  ## simulated grid to disagree with. `sim_landis()` still requires one.
  f <- fs::path(scenario_dir, "scenario.txt")
  if (!fs::file_exists(f) || !any(grepl("^\\s*CellLength\\b", readLines(f, warn = FALSE)))) {
    return(invisible(NULL))
  }
  obs <- observed$pixel_area_ha
  sim <- .scenario_pixel_area_ha(scenario_dir)
  if (!is.null(obs) && is.finite(obs) && !isTRUE(all.equal(obs, sim))) {
    stop(
      "The observed targets were built on ",
      obs,
      " ha cells but the scenario's CellLength gives ",
      sim,
      " ha; rebuild the targets on the simulation grid",
      call. = FALSE
    )
  }
  invisible(sim)
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
#'   \item event-log: `Time`, `InitFireRegion`, `InitFuel`, `SitesChecked`, `DamagedSites`,
#'         `MeanSeverity`.
#'   \item summary-log: `Time`, `NumberFires`, `TotalSitesBurned`.
#' }
#'
#' A fire's size is taken from `SitesChecked`. The extension logs `DamagedSites` as one more
#' than the cells the fire burned, on every event, while `SitesChecked` equals the burned cells
#' on the timestep's severity map. `MeanSeverity` is divided by that inflated count, so the
#' returned `mean_severity` is rescaled to `MeanSeverity * DamagedSites / SitesChecked`, the mean
#' over the burned cells.
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
    dplyr::arrange(.data$year) |>
    .drop_initial_timestep()

  if (nrow(events) > 0L) {
    if (!all(c("SitesChecked", "DamagedSites") %in% names(events))) {
      stop(
        "Dynamic Fire event log ",
        event_path,
        " lacks SitesChecked or DamagedSites; the burned area and mean severity cannot be recovered",
        call. = FALSE
      )
    }
    ## A fire's burned area is `SitesChecked`, not `DamagedSites`. The extension logs
    ## `DamagedSites` as one more than the cells the fire burned, on every event: a fire that
    ## burns only its ignition cell logs 2, and the burned cells on that timestep's severity map
    ## equal `SitesChecked` exactly. `MeanSeverity` is the summed severity divided by that same
    ## inflated count, so it is rescaled onto the burned cells; left alone it reads 0.5 for a
    ## one-cell fire at severity 1 and 1.5 for one at severity 3.
    checked <- as.integer(events$SitesChecked)
    damaged <- as.integer(events$DamagedSites)
    ## The reading below rests on `DamagedSites == SitesChecked + 1`, verified against the
    ## severity maps on one landscape. Say so if a log breaks it, rather than silently scoring
    ## sizes on an assumption that no longer holds. A warning, not an error, so it cannot strand
    ## a long calibration.
    off <- which(damaged - checked != 1L)
    if (length(off) > 0L) {
      warning(
        "Dynamic Fire event log ",
        event_path,
        ": DamagedSites is not SitesChecked + 1 on ",
        length(off),
        " of ",
        length(damaged),
        " events (first at row ",
        off[1L],
        "); fire sizes are taken from SitesChecked, which matched the severity maps only where ",
        "that relation held",
        call. = FALSE
      )
    }
    events_tbl <- tibble::tibble(
      year = as.integer(events$Time),
      eco = trimws(as.character(events$InitFireRegion)),
      init_fuel = as.integer(events$InitFuel),
      sites = checked,
      mean_severity = ifelse(
        checked > 0L,
        as.numeric(events$MeanSeverity) * damaged / checked,
        NA_real_
      )
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
    area_by_fuel_ha = .read_burned_area_by_fuel(rep_dir, pixel_area_ha),
    ## NULL from a mock simulator, or from a rep that kept no severity maps.
    overstory_mortality = landis_overstory_mortality_share(rep_dir)
  )
}

#' Share of burned area that lost its dominant cohort
#'
#' The proportion of a replicate's burned cells in which the cohort holding the most biomass was
#' killed. This is the model-side counterpart of a field burn-severity class defined by mortality
#' of the structurally dominant vegetation, and unlike the extension's own severity classes -- which
#' are crown fraction burned -- it is comparable with an observed mortality measure. In a forest
#' that burns hot at ground level without crowning, the two disagree by construction.
#'
#' Everything needed is in the replicate directory, because a run is a copy of its scenario:
#' \itemize{
#'   \item `fire/severity-{t}.tif` -- the severity map per timestep. The Dynamic Fire encoding is
#'         0 inactive, 1 active and unburned, 2 burned with no cohort damaged, and severity + 2 for
#'         a damaged cell, so a cell's severity class is its map value MINUS TWO.
#'   \item `initial-communities.csv` / `.tif` -- the cohorts on each cell.
#'   \item `species.txt` -- longevity, which the damage table's age thresholds are shares of.
#'   \item `DynamicFire_Spp_Table.csv` -- each species' fire tolerance.
#'   \item `dynamic-fire.txt` -- the `FireDamageTable`, mapping severity minus tolerance to the
#'         oldest cohort killed, as a percentage of longevity.
#' }
#'
#' A cohort dies when severity is 5 (the extension kills everything at that severity, whatever the
#' tolerance) or when its age is at or below that percentage of its species' longevity. Composition
#' is read at time zero, so a cell that burns twice in one replicate is scored on its original
#' cohorts; in a calibration run, where succession is frozen, only reburns are affected.
#'
#' @param rep_dir Character. Path to the replicate directory.
#'
#' @returns A list with `burned_cells`, `high_cells` and `share` (NA when nothing burned), or NULL
#'   when the replicate holds no severity maps or is missing one of the files above.
#'
#' @family Dynamic Fire calibration helpers
#'
#' @export
landis_overstory_mortality_share <- function(rep_dir) {
  stopifnot(fs::dir_exists(rep_dir))
  fire_dir <- fs::path(rep_dir, "fire")
  if (!fs::dir_exists(fire_dir)) {
    return(NULL)
  }
  sev_files <- fs::dir_ls(fire_dir, regexp = "severity-\\d+\\.tif$", type = "file")
  ic_csv <- fs::path(rep_dir, "initial-communities.csv")
  ic_tif <- fs::path(rep_dir, "initial-communities.tif")
  spp_csv <- fs::path(rep_dir, "DynamicFire_Spp_Table.csv")
  spp_txt <- fs::path(rep_dir, "species.txt")
  fire_txt <- fs::path(rep_dir, "dynamic-fire.txt")
  if (
    length(sev_files) == 0L || !all(fs::file_exists(c(ic_csv, ic_tif, spp_csv, spp_txt, fire_txt)))
  ) {
    return(NULL)
  }

  tol <- .read_fire_tolerance(spp_csv)
  longevity <- .read_species_longevity(spp_txt)
  damage <- .read_fire_damage_table(fire_txt)

  ## Per map code, the age of the dominant cohort as a share of its species' longevity, and that
  ## species' tolerance -- all the severity-independent part of the decision, computed once.
  ic <- utils::read.csv(ic_csv)
  stopifnot(all(c("MapCode", "SpeciesName", "CohortAge", "CohortBiomass") %in% names(ic)))
  ic <- ic[ic$SpeciesName %in% names(tol) & ic$SpeciesName %in% names(longevity), , drop = FALSE]
  if (nrow(ic) == 0L) {
    return(NULL)
  }
  ic <- ic[order(ic$MapCode, -ic$CohortBiomass), , drop = FALSE]
  dom <- ic[!duplicated(ic$MapCode), , drop = FALSE]
  dom$age_share <- 100 * dom$CohortAge / longevity[dom$SpeciesName]
  dom$tol <- tol[dom$SpeciesName]

  ic_r <- read_landis_raster(as.character(ic_tif))
  codes <- terra::values(ic_r, mat = FALSE)

  burned_cells <- 0L
  high_cells <- 0L
  for (f in as.character(sev_files)) {
    sev <- terra::values(read_landis_raster(f), mat = FALSE)
    burned <- which(!is.na(sev) & sev >= 2)
    if (length(burned) == 0L) {
      next
    }
    burned_cells <- burned_cells + length(burned)
    ## Severity class from the map value, and the oldest cohort that class kills on this cell.
    class <- sev[burned] - 2L
    idx <- match(codes[burned], dom$MapCode)
    ok <- !is.na(idx)
    if (!any(ok)) {
      next
    }
    ## A class of 0 is the map's value 2: the extension burned the cell and damaged NO cohort
    ## (user guide 3.1). Its severity is not recorded, so re-deriving mortality from the damage
    ## table there would invent a severity of 0 and, at any tolerance the table reaches, score a
    ## share of those cells as having lost their dominant cohort -- against the extension's own
    ## statement that nothing was killed. They stay in the denominator, because the observed
    ## reference counts every assessed pixel inside a fire perimeter including the unburned ones.
    damaged <- class[ok] >= 1L
    diff <- class[ok] - dom$tol[idx[ok]]
    killed_to <- .damage_age_pct(damage, diff)
    high_cells <- high_cells + sum(damaged & (class[ok] >= 5 | dom$age_share[idx[ok]] <= killed_to))
  }
  list(
    burned_cells = burned_cells,
    high_cells = high_cells,
    share = if (burned_cells > 0L) high_cells / burned_cells else NA_real_
  )
}

## Species fire tolerances from a Dynamic Fire `Species_CSV_File` (internal).
.read_fire_tolerance <- function(path) {
  df <- utils::read.csv(path)
  stopifnot(all(c("SpeciesCode", "FireTolerance") %in% names(df)))
  stats::setNames(as.numeric(df$FireTolerance), trimws(as.character(df$SpeciesCode)))
}

## Longevity per species from a LANDIS-II core `species.txt` (internal). Data rows are the
## non-comment lines after the LandisData header; the first two fields are the code and longevity.
.read_species_longevity <- function(path) {
  lines <- readLines(path, warn = FALSE)
  lines <- trimws(lines)
  lines <- lines[nzchar(lines) & !startsWith(lines, ">>") & !startsWith(lines, "LandisData")]
  parts <- strsplit(lines, "[[:space:]]+")
  keep <- vapply(
    parts,
    function(x) length(x) >= 2L && !is.na(suppressWarnings(as.numeric(x[2L]))),
    logical(1)
  )
  parts <- parts[keep]
  stats::setNames(
    vapply(parts, function(x) as.numeric(x[2L]), numeric(1)),
    vapply(parts, function(x) x[1L], character(1))
  )
}

## The FireDamageTable of a `dynamic-fire.txt` (internal): the oldest cohort killed, as a percent
## of longevity, for each severity-minus-tolerance difference.
.read_fire_damage_table <- function(path) {
  lines <- readLines(path, warn = FALSE)
  hdr <- grep("^FireDamageTable", trimws(lines))
  stopifnot(length(hdr) == 1L)
  out <- list(pct = numeric(0), diff = numeric(0))
  i <- hdr + 1L
  while (i <= length(lines)) {
    ln <- trimws(lines[i])
    i <- i + 1L
    if (startsWith(ln, ">>") || !nzchar(ln)) {
      if (length(out$pct) > 0L && !nzchar(ln)) {
        break
      }
      next
    }
    if (grepl("^[A-Za-z]", ln)) {
      break
    }
    parts <- strsplit(ln, "[[:space:]]+")[[1]]
    if (length(parts) < 2L) {
      next
    }
    pct <- suppressWarnings(as.numeric(sub("%$", "", parts[1L])))
    dif <- suppressWarnings(as.numeric(parts[2L]))
    if (is.na(pct) || is.na(dif)) {
      next
    }
    out$pct <- c(out$pct, pct)
    out$diff <- c(out$diff, dif)
  }
  stopifnot(length(out$pct) >= 1L)
  ord <- order(out$diff)
  list(pct = out$pct[ord], diff = out$diff[ord])
}

## The oldest cohort killed (percent of longevity) at each severity-minus-tolerance difference
## (internal). Below the table's smallest difference nothing is killed; at or above its largest,
## everything is.
.damage_age_pct <- function(damage, diff) {
  out <- numeric(length(diff))
  hit <- findInterval(diff, damage$diff)
  out[hit == 0L] <- 0
  keep <- hit > 0L
  out[keep] <- damage$pct[hit[keep]]
  out
}

#' Per-rep cell-based burn area by fuel code (internal)
#'
#' Walks `<rep_dir>/fire/severity-{t}.tif` and the matching
#' `<rep_dir>/fire/FuelType-{t}.tif` files, masks the fuel raster to cells
#' with severity > 1 (the Dynamic Fire encoding is 0 = inactive,
#' 1 = active-but-unburned, 2 = burned with no cohort damaged, and severity + 2
#' for a damaged cell, so `> 1` is every burned cell and the value is NOT the
#' severity class),
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
    sev <- read_landis_raster(as.character(sev_files[i]))
    fuel <- read_landis_raster(as.character(fuel_path))
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
#'   \item `FireSizesTable` column 16 (`NumFires`): replaced by `NumFires`, or
#'         multiplied by `NumFiresMultiplier`. The two are alternatives -- the
#'         first gives every ecoregion the same rate, the second preserves the
#'         rates' relative structure -- and supplying both is an error.
#'   \item `FuelTypeTable` data rows: column 4 (`IgnProb`) is multiplied by the
#'         base-type-specific candidate (e.g., `IgnProb_Conifer` for `Base == "Conifer"`).
#'         Default IgnProbs are mostly 1.0 (D1 = 0.5), so candidate range `[0, 1.5]`
#'         directly scales the relative-weighting.
#'   \item `FireDamageTable` cohort-age column multiplied by `DamageAgeMultiplier`,
#'         rounded to whole percentages and forced strictly increasing. The paired
#'         severity-minus-tolerance column must be an integer, so it is left alone:
#'         shifting it offers only a few reachable outcomes and cannot be fitted,
#'         whereas the age column is a percentage of longevity and scales smoothly.
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
    length(par_vec) > 0L,
    all(names(par_vec) %in% calibration_par_names())
  )
  .check_num_fires_params(names(par_vec))
  ## A SUBSET is allowed, and an absent name means "leave the template's value alone". Requiring
  ## all nine forced every calibration to search every dimension, including ones a project sets
  ## from data instead -- a HiProp is the share of a season's fires in its high-FMC part, which the
  ## fire record gives directly -- and ones that are degenerate for its fire regime: where a
  ## season's FMCLo equals its FMCHi, its HiProp cannot change any outcome, and candidates
  ## differing only in it score byte-identical losses.
  fire_txt <- fs::path(scenario_dir, "dynamic-fire.txt")
  if (!fs::file_exists(fire_txt)) {
    stop("dynamic-fire.txt not found in ", scenario_dir, call. = FALSE)
  }
  lines <- readLines(fire_txt)

  ## 1. SeverityCalibrationFactor scalar (skipped entirely when not calibrated)
  if ("SeverityCalibrationFactor" %in% names(par_vec)) {
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
  }

  ## 2. FireSizesTable HiProp columns (8 / 11 / 14) and NumFires (16)
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
      if ("SpHiProp" %in% names(par_vec)) {
        parts[8L] <- sprintf("%g", par_vec[["SpHiProp"]])
      }
      if ("SumHiProp" %in% names(par_vec)) {
        parts[11L] <- sprintf("%g", par_vec[["SumHiProp"]])
      }
      if ("FallHiProp" %in% names(par_vec)) {
        parts[14L] <- sprintf("%g", par_vec[["FallHiProp"]])
      }
      ## NumFires is the LAST column, and the ecoregion row has 16 fields once
      ## OpenFuelIndex is counted -- patched only when the row is that long, so a table
      ## written without it is left alone rather than gaining a stray field.
      ##
      ## The multiplier scales the row's OWN rate, so a landscape whose ecoregions differ in
      ## ignition rate keeps that structure; the absolute form replaces every row alike. Both are
      ## applied to a fresh copy of the template (see the note above), so scaling is deterministic
      ## across trials rather than compounding.
      if ("NumFires" %in% names(par_vec) && length(parts) >= 16L) {
        parts[16L] <- sprintf("%g", par_vec[["NumFires"]])
      } else if ("NumFiresMultiplier" %in% names(par_vec) && length(parts) >= 16L) {
        parts[16L] <- sprintf(
          "%g",
          .scale_num_fires(
            suppressWarnings(as.numeric(parts[16L])),
            par_vec[["NumFiresMultiplier"]]
          )
        )
      }
      lines[i] <- paste(parts, collapse = "    ")
    }
    i <- i + 1L
  }

  ## 3. FuelTypeTable IgnProb column (4)
  ftt_hdr <- grep("^FuelTypeTable[[:space:]]*$", lines)
  if (length(ftt_hdr) != 1L) {
    stop("Could not locate FuelTypeTable header in ", fire_txt, call. = FALSE)
  }
  ## Only the fuel bases whose multiplier is being calibrated; the rest keep the template value.
  .ign <- c(
    Conifer = "IgnProb_Conifer",
    ConiferPlantation = "IgnProb_ConiferPlantation",
    Deciduous = "IgnProb_Deciduous",
    Slash = "IgnProb_Slash",
    Open = "IgnProb_Open"
  )
  .ign <- .ign[.ign %in% names(par_vec)]
  base_multipliers <- stats::setNames(as.numeric(par_vec[unname(.ign)]), names(.ign))
  j <- ftt_hdr + 1L
  while (j <= length(lines) && (grepl("^[[:space:]]*>>", lines[j]) || !nzchar(trimws(lines[j])))) {
    j <- j + 1L
  }
  while (j <= length(lines) && nzchar(trimws(lines[j])) && !grepl("^[A-Za-z]", lines[j])) {
    parts <- strsplit(trimws(lines[j]), "\\s+")[[1]]
    if (length(parts) >= 11L) {
      base <- parts[2L]
      default_ignprob <- suppressWarnings(as.numeric(parts[4L]))
      ## `[[` on an atomic vector ERRORS for a missing name -- it does not return NULL -- so the
      ## former `!is.null(mult)` guard was dead code that only ever held because all five bases
      ## were always present. With a calibrated SUBSET they are not, and a fuel row whose base is
      ## not being calibrated must simply keep its template IgnProb.
      mult <- if (base %in% names(base_multipliers)) base_multipliers[[base]] else NA_real_
      if (!is.na(mult) && !is.na(default_ignprob)) {
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

  ## 4. FireDamageTable cohort-age column, scaled by DamageAgeMultiplier
  ##
  ## The table's severity-minus-tolerance column must be an integer (user guide 2.16.3), so
  ## shifting it gives only a handful of reachable outcomes and cannot be fitted. The age
  ## column is a percentage of longevity (2.16.2), which scales continuously, so that is what
  ## a calibration can move: the multiplier asks how much younger than the guide's example
  ## table a cohort must be before a fire of a given severity kills it.
  ##
  ## Scaling the FILE's own values rather than an assumed 20/50/85/100 keeps this honest for a
  ## template carrying a different baseline table, and does not compound across trials because
  ## every trial patches a fresh copy of the template (see `sim_landis()`).
  if ("DamageAgeMultiplier" %in% names(par_vec)) {
    mult <- par_vec[["DamageAgeMultiplier"]]
    dt_hdr <- grep("^FireDamageTable[[:space:]]*$", lines)
    if (length(dt_hdr) != 1L) {
      stop(
        "Expected exactly one FireDamageTable header in ",
        fire_txt,
        " (found ",
        length(dt_hdr),
        ")",
        call. = FALSE
      )
    }
    k <- dt_hdr + 1L
    while (
      k <= length(lines) && (grepl("^[[:space:]]*>>", lines[k]) || !nzchar(trimws(lines[k])))
    ) {
      k <- k + 1L
    }
    ## Row indices and the percentages they carry, so the whole column can be rescaled at once
    ## and kept strictly increasing -- a row that did not exceed its predecessor would be
    ## unreachable, since the extension takes the first row whose bound the cohort falls under.
    rows <- integer(0)
    pcts <- numeric(0)
    while (k <= length(lines) && nzchar(trimws(lines[k])) && !grepl("^[A-Za-z]", lines[k])) {
      parts <- strsplit(trimws(lines[k]), "\\s+")[[1]]
      pct <- suppressWarnings(as.numeric(sub("%$", "", parts[1L])))
      if (length(parts) >= 2L && !is.na(pct)) {
        rows <- c(rows, k)
        pcts <- c(pcts, pct)
      }
      k <- k + 1L
    }
    if (length(rows) == 0L) {
      stop("FireDamageTable in ", fire_txt, " has no data rows", call. = FALSE)
    }
    scaled <- .scale_damage_age(pcts, mult)
    for (n in seq_along(rows)) {
      parts <- strsplit(trimws(lines[rows[n]]), "\\s+")[[1]]
      parts[1L] <- sprintf("%d%%", as.integer(scaled[n]))
      lines[rows[n]] <- paste(parts, collapse = "    ")
    }
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
#'         comes from each event's ignition fuel code times its burned cells,
#'         mapped to base fuel types via `observed$fuel_code_to_base`. Skipped
#'         (contributes 0) when either `observed$primary$area_by_fuel_ha` is
#'         NULL or `observed$fuel_code_to_base` is missing.
#'   \item `L_severity`: chi-squared distance between simulated and observed
#'         severity-class proportions. Simulated severities come from each
#'         event's `MeanSeverity` binned into integer classes 1..5; observed
#'         comes from `observed$primary$severity_dist` (a 5-element named
#'         numeric vector summing to 1). Skipped when observed is NULL.
#'   \item `L_mortality = |mean(share_sim) - share_obs| / share_obs` -- the
#'         share of burned area that lost its dominant cohort, against the same
#'         share observed. Contributes 0 when
#'         `observed$primary$mortality_share` is NULL or NA, or when no
#'         replicate kept the severity maps it is measured from.
#'   \item `L_area_burned = |log10(area_sim / area_obs)|` -- annual area burned,
#'         simulated against observed. No other component scores how much area
#'         burns: `L_area_fuel` scores how burned area is distributed across
#'         base fuel types, not how much there is. A log10 ratio keeps it
#'         scale-free, so the same weight means the same thing on study areas
#'         whose burn rates differ by orders of magnitude. Simulated area is
#'         summed from each replicate's events over the years `L_count` scores,
#'         converted with `observed$pixel_area_ha`. Observed area is
#'         `lambda_obs * mean(fire_sizes_ha)` -- the count target's own annual
#'         rate times the size sample's mean fire size, NOT
#'         `sum(fire_sizes_ha) / n_years`. The two agree only when the size
#'         sample is every fire in the area and years the counts cover; a
#'         payload that borrows per-fire sizes from a wider region than it
#'         counts ignitions in would otherwise be scored against that wider
#'         region's annual area. Contributes 0 when `fire_sizes_ha` is empty or
#'         `lambda_obs` is not positive, and `.AREA_BURNED_NO_FIRE` (3.0) when a
#'         replicate set burns nothing, since `log10(0)` would be infinite and
#'         DEoptim cannot rank an infinite objective.
#'
#'         This term and `count` both move with the number of fires, so they
#'         compete for the same lever wherever a calibration scales ignition
#'         rates. `count` is normalised by the observed year-to-year standard
#'         deviation, making it steeper by about
#'         `lambda_obs / sd(n_fires_obs) * ln(10)`; keep
#'         `weights["area_burned"]` well below `weights["count"]` times that
#'         factor, or the fitted fire count is pulled off its own target to
#'         compensate for a fire-size distribution the search cannot change.
#'         **Compute that factor for your own record rather than assuming it is
#'         large.** It is roughly 3.4 on a record averaging 27.8 fires per year
#'         with a standard deviation of 18.9, but only 1.6 on a sparse record
#'         averaging 0.87 fires per year, because a record whose counts are
#'         nearly Poisson has a small standard deviation to divide by. The
#'         sparser the fire record, the less headroom this component has.
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
#'   `size_tail`, `area_fuel`, `severity`, `mortality`, `area_burned`. Missing
#'   components default to 0, so an existing caller's weights keep their
#'   meaning when a component is added.
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
  ## `is.list(reps)` is satisfied by a list holding NULLs, and that is not a theoretical gap:
  ## `parallel::mclapply()` returns NULL for a replicate whose child process died, so a caller
  ## that only tested for `try-error` hands one straight to us. Scoring it fails deep in the
  ## component arithmetic with "missing value where TRUE/FALSE needed", which points at this
  ## function rather than at the dead replicate. Same family as the `weights` and `lambda_obs`
  ## checks below: cheap to test here, expensive to diagnose later.
  bad_reps <- which(!vapply(reps, is.list, logical(1)))
  if (length(bad_reps) > 0L) {
    stop(
      "`reps` must be a list of per-replicate summary lists; element(s) ",
      paste(bad_reps, collapse = ", "),
      " are not (",
      paste(
        unique(vapply(reps[bad_reps], function(x) class(x)[[1L]], character(1))),
        collapse = ", "
      ),
      "). A NULL element usually means a parallel replicate's process died without returning.",
      call. = FALSE
    )
  }
  primary <- observed$primary %||% observed$fru59
  stopifnot(!is.null(primary))
  ## `weights` must be a NAMED numeric vector over a subset of the known components. Neither half is
  ## paranoia. Passing NULL leaves `w` at its zero initialisation, since `w[names(NULL)] <- NULL` is
  ## a no-op, so the total collapses to 0 while every component is computed correctly -- a wrong
  ## answer with no signal at all. An unknown name is worse: `w[names(weights)] <- weights` GROWS
  ## `w` past `components`, and the multiply then recycles across mismatched pairs. Both are the
  ## same family as the `lambda_obs` case below.
  if (
    !is.numeric(weights) ||
      length(weights) == 0L ||
      is.null(names(weights)) ||
      !all(nzchar(names(weights))) ||
      anyNA(weights)
  ) {
    stop(
      "`weights` must be a non-empty, fully named, non-NA numeric vector over: ",
      paste(.LOSS_COMPONENTS, collapse = ", "),
      ".",
      call. = FALSE
    )
  }
  unknown_w <- setdiff(names(weights), .LOSS_COMPONENTS)
  if (length(unknown_w) > 0L) {
    stop(
      "unknown loss weight(s): ",
      paste(unknown_w, collapse = ", "),
      ". Known components: ",
      paste(.LOSS_COMPONENTS, collapse = ", "),
      ".",
      call. = FALSE
    )
  }

  ## `lambda_obs` must be a finite scalar, and this has to be checked rather than assumed. If it is
  ## absent, `L_count` below evaluates to numeric(0); `c(count = numeric(0), size = ...)` then DROPS
  ## the element instead of erroring, so `components` comes back one short, its names shift, and
  ## `w * components` multiplies mismatched pairs -- a silently wrong total behind nothing louder
  ## than a recycling warning.
  if (
    !is.numeric(primary$lambda_obs) ||
      length(primary$lambda_obs) != 1L ||
      !is.finite(primary$lambda_obs)
  ) {
    stop(
      "`observed$primary$lambda_obs` must be a single finite number; ",
      "loss_from_stats() cannot score the count component without it.",
      call. = FALSE
    )
  }

  ## L_count: pool simulated annual counts per rep, compare mean to observed lambda
  n_fires_per_year_per_rep <- vapply(
    reps,
    function(r) {
      d <- .drop_initial_timestep(r$n_fires_by_year)
      sum(d$n_fires) / max(1L, nrow(d))
    },
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

  ## Mortality: the share of burned area that lost its dominant cohort, against the same share
  ## observed. Unlike `severity`, both sides measure mortality, so they are comparable in a forest
  ## where fire kills the canopy from the ground without crowning. Relative absolute difference,
  ## which keeps it on the same scale as `count`.
  L_mortality <- if (is.null(primary$mortality_share) || is.na(primary$mortality_share)) {
    0.0
  } else {
    sim <- vapply(
      reps,
      function(r) {
        m <- r$overstory_mortality
        if (is.null(m) || is.na(m$share)) NA_real_ else as.numeric(m$share)
      },
      numeric(1)
    )
    if (all(is.na(sim))) {
      0.0
    } else {
      obs <- as.numeric(primary$mortality_share)
      abs(mean(sim, na.rm = TRUE) - obs) / max(obs, .Machine$double.eps)
    }
  }

  ## L_area_burned: annual area burned, as a log10 ratio of simulated to observed.
  ##
  ## Nothing else in the loss scores how much area burns. `area_fuel` scores how burned area is
  ## DISTRIBUTED across base fuel types, not how much of it there is, and `count` and `size` score
  ## the number and the shape. A parameter set can satisfy every one of them while burning several
  ## times too much or too little, which for a range-of-variation study is the quantity the whole
  ## exercise turns on: annual area burned is what drives the seral-stage distribution.
  ##
  ## A log10 ratio rather than a normalised difference, because annual area burned spans orders of
  ## magnitude between study areas and a scale-free term transfers between them unchanged.
  ##
  ## Summed from `events` rather than from `fire_sizes_ha`: the latter is a bare sorted vector with
  ## no year attached, so it cannot be restricted to the scored years. `.drop_initial_timestep()`
  ## defines those years exactly as `L_count` does, so the two components always divide by the same
  ## denominator.
  ##
  ## NOTE ON WEIGHTING. This term and `count` both move with the number of fires, so they compete
  ## for the same lever where a calibration scales ignition rates. `count` is normalised by the
  ## observed year-to-year standard deviation, which makes it the steeper of the two by roughly
  ## `lambda_obs / sd(n_fires_obs) * ln(10)` -- about 3x on a record with 27.8 fires per year and an
  ## sd of 18.9. Below that ratio of weights the count target still decides the fire number and this
  ## term decides only the size of each fire; above it, the two swap roles and the fitted fire count
  ## is pulled off its target to compensate for a fire-size distribution the search cannot change.
  ## Keep `weights["area_burned"]` well under `weights["count"]` times that factor.
  L_area_burned <- {
    ## The observed rate is the COUNT TARGET's own rate times the SIZE SAMPLE's mean fire size --
    ## NOT `sum(fire_sizes_ha) / n_years`. Those two agree only when the size sample is every fire
    ## in the area and years the counts were taken over, and a payload may deliberately break that:
    ## per-fire sizes are scarce, fire counts are not, so a small study area may borrow its size
    ## sample from a wider region while counting ignitions only within itself. The summed form then
    ## returns the WIDER region's annual area. On one such payload it overstated the target by 8.1x,
    ## which would have dragged the fitted ignition rate up to match an area the landscape does not
    ## contain. The product form is also the honest statement of what this component assumes: that
    ## the size sample is representative of fires in the counted area.
    obs_sizes <- primary$fire_sizes_ha
    obs_mean_size <- if (is.null(obs_sizes) || length(obs_sizes) == 0L) {
      NA_real_
    } else {
      mean(as.numeric(obs_sizes))
    }
    obs_aab <- if (
      !is.numeric(primary$lambda_obs) ||
        length(primary$lambda_obs) != 1L ||
        !is.finite(primary$lambda_obs) ||
        primary$lambda_obs <= 0 ||
        !is.finite(obs_mean_size) ||
        obs_mean_size <= 0
    ) {
      NA_real_
    } else {
      primary$lambda_obs * obs_mean_size
    }
    if (!is.finite(obs_aab) || obs_aab <= 0) {
      0.0
    } else {
      cell_ha <- observed$pixel_area_ha %||% 1.0
      sim_aab <- mean(vapply(
        reps,
        function(r) {
          d <- .drop_initial_timestep(r$n_fires_by_year)
          n_yr <- max(1L, nrow(d))
          ev <- r$events
          if (is.null(ev) || nrow(ev) == 0L) {
            return(0)
          }
          sum(as.numeric(ev$sites[ev$year %in% d$year])) * cell_ha / n_yr
        },
        numeric(1)
      ))
      if (!is.finite(sim_aab) || sim_aab <= 0) {
        .AREA_BURNED_NO_FIRE
      } else {
        abs(log10(sim_aab / obs_aab))
      }
    }
  }

  components <- c(
    count = L_count,
    size = L_size,
    size_tail = L_size_tail,
    area_fuel = L_area_fuel,
    severity = L_severity,
    mortality = L_mortality,
    area_burned = L_area_burned
  )
  stopifnot(identical(names(components), .LOSS_COMPONENTS))
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
  ## to its actual fuel).
  ##
  ## Gate logic: a rep is "cell-attribution capable" if it either has a
  ## populated `area_by_fuel_ha` tibble OR has no events at all (in which
  ## case there's nothing to attribute and `parse_dynamic_fire_logs()`
  ## correctly returns NULL -- no severity/FuelType tifs are written for
  ## a no-fire rep). We only fall back to legacy event-`InitFuel`
  ## attribution if some rep HAS events but is MISSING `area_by_fuel_ha`
  ## (mock simulator, Dynamic Fuels disabled, or payloads from landisutils
  ## < 0.0.52). Treating "no fires" reps as cell-capable lets the cell
  ## path engage during low-fire-rate calibrations where a fraction of
  ## reps land on zero events -- the regime in which the old gate
  ## (`all(has_cell_attr)`) silently fell back to legacy and produced a
  ## non-smooth optimization surface (chi-sq jumped depending on whether
  ## EVERY rep happened to fire in the current trial).
  has_cell_attr <- vapply(
    reps,
    function(r) {
      !is.null(r$area_by_fuel_ha) || nrow(r$events) == 0L
    },
    logical(1)
  )
  if (all(has_cell_attr)) {
    ## Drop NULL elements (zero-fire reps) before binding; they contribute
    ## nothing to sim_area_by_base, which is what we want.
    rep_dfs <- Filter(Negate(is.null), lapply(reps, function(r) r$area_by_fuel_ha))
    if (length(rep_dfs) == 0L) {
      return(1.0) ## no fires in ANY rep across the trial -- penalty
    }
    sim_area_df <- do.call(rbind, rep_dfs)
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

  ## Score on the THREE categories the observation actually distinguishes, not five.
  ##
  ## Every observed reference here is 3-class at source -- CanLaBS applies two dNBR thresholds,
  ## and the BC layer is Low/Medium/High -- and is spread onto LANDIS 1-5 by a trapezoid kernel
  ## that splits low across classes 1|2 and high across 4|5. So an observed vector always has the
  ## form (a, a, b, c, c): the 1-vs-2 and 4-vs-5 equality is an artifact of that projection, not a
  ## measurement. Scoring on 5 therefore charges the simulator for its within-low and within-high
  ## shape, about which the observation says nothing. Measured on this landscape: of the 1.02 total
  ## absolute error against the study-area reference, 0.79 -- 78% -- came from that split alone,
  ## while the aggregate low/medium/high proportions very nearly matched (0.888 vs 0.810 low).
  ##
  ## Collapsing both sides keeps every bit of real signal (a genuine low/med/high disagreement
  ## still scores) and drops only the fabricated part. Opt back into the 5-class form with
  ## `options(landisutils.calibration.severity_classes = 5L)` for backwards comparison.
  if (identical(as.integer(getOption("landisutils.calibration.severity_classes", 3L)), 3L)) {
    collapse3 <- function(p) c(sum(p[1:2]), p[[3]], sum(p[4:5]))
    if (length(sim_p) == 5L && length(obs_p) == 5L) {
      sim_p <- collapse3(sim_p)
      obs_p <- collapse3(obs_p)
    }
  }

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
#' entry in the calibrated parameter vector.
#'
#' @section Multipliers above `1 / default` are inert:
#' LANDIS-II requires `IgnProb` in `[0, 1]`, so the product is clamped to that
#' range. The defaults in [defaultFuelTypeTable()] are 1.0 for every base except
#' `Deciduous` (`D1`), which is 0.5. A `Conifer` multiplier above 1.0 is
#' therefore clamped away entirely, and a `Deciduous` multiplier of 2.0 maps to
#' exactly the ceiling. Useful search bounds are `[0, 1]` for the 1.0 defaults
#' and `[0, 2]` for `Deciduous`; anything wider searches a flat region.
#'
#' This matters when reading a finished calibration. A multiplier that comes
#' back pinned at such a bound is **not** an estimate that wanted more room --
#' it is saturation, meaning the objective wanted more fire than the maximum
#' ignition probability can deliver. Widening the bound is a no-op. The
#' lever to reach for instead is the ignition rate itself: an ignition becomes a
#' fire only if the initiation probability of the fuel on its cell allows it, so
#' a rate taken from a count of observed FIRES is systematically low as a count
#' of ignitions. Search `NumFires`, or `NumFiresMultiplier` where the rate varies
#' by ecoregion and that variation should survive the fit, applying the result
#' with [apply_calibrated_num_fires()], and check the count target too --
#' starting with whether the simulated annual rate is computed over the right
#' number of years.
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
  ## Default 1.0 = leave that base's template IgnProb alone. An uncalibrated base must be neutral,
  ## not an error: a fuel table can legitimately omit bases the landscape cannot produce, and their
  ## multipliers are then dropped from the search (they are inert -- candidates differing only in
  ## them score identically). `.par()` is used because `[[` on an atomic vector raises a subscript
  ## error for a missing name; the pre-existing `m[is.na(m)] <- 1.0` below already handled a base
  ## absent from THIS vector, but the vector itself could not be built in the first place.
  multipliers <- c(
    Conifer = .par(calibrated_fire_params, "IgnProb_Conifer", 1.0),
    ConiferPlantation = .par(calibrated_fire_params, "IgnProb_ConiferPlantation", 1.0),
    Deciduous = .par(calibrated_fire_params, "IgnProb_Deciduous", 1.0),
    Slash = .par(calibrated_fire_params, "IgnProb_Slash", 1.0),
    Open = .par(calibrated_fire_params, "IgnProb_Open", 1.0)
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
  ## Only overwrite the seasons that were actually CALIBRATED, leaving the rest at their template
  ## value -- the same contract patch_fire_config() follows. `[[` on an atomic vector raises a
  ## subscript error for a missing name rather than returning NULL, so indexing unconditionally
  ## aborts the moment a calibration searches a subset. That is not hypothetical: a run that dropped
  ## the degenerate FallHiProp produced an 8-parameter best_params.rds, and the next production
  ## scenario build died here with "subscript out of bounds" before the calibration could start.
  for (col in c("SpHiProp", "SumHiProp", "FallHiProp")) {
    if (col %in% names(calibrated_fire_params)) {
      fire_size_table[[col]] <- calibrated_fire_params[[col]]
    }
  }
  fire_size_table
}


#' Apply a calibrated ignition rate to a FireSizesTable
#'
#' `NumFires` is the Poisson mean number of IGNITIONS per year for the ecoregion, each of which
#' becomes a fire only if the initiation probability of the fuel on its cell says so, so it is not
#' the same quantity as an observed count of fires.
#'
#' Two calibrated forms, and they are alternatives:
#' \itemize{
#'   \item `NumFires` replaces the rate in every row with one value. Right where the rate is a
#'         property of the landscape as a whole.
#'   \item `NumFiresMultiplier` scales each row's own rate by a common factor, so ecoregions that
#'         differ in ignition rate keep their relative structure. Right where the rate is measured
#'         per ecoregion and the correction being fitted -- the gap between a count of observed
#'         fires and a count of ignitions -- applies to all of them alike.
#' }
#'
#' Supplying both is an error rather than a composition, and a vector carrying neither leaves the
#' table alone.
#'
#' @param fire_size_table data.frame with a `NumFires` column, as a project's
#'   `make_fire_size_table()`-equivalent produces.
#' @param calibrated_fire_params Named numeric vector. Used only if it holds `NumFires` or
#'   `NumFiresMultiplier`.
#'
#' @returns A copy of `fire_size_table`, with `NumFires` replaced or scaled where calibrated.
#'
#' @family Dynamic Fire calibration helpers
#' @family Dynamic Fire helpers
#'
#' @export
apply_calibrated_num_fires <- function(fire_size_table, calibrated_fire_params) {
  stopifnot(
    is.data.frame(fire_size_table),
    "NumFires" %in% names(fire_size_table),
    is.numeric(calibrated_fire_params),
    !is.null(names(calibrated_fire_params))
  )
  .check_num_fires_params(names(calibrated_fire_params))
  if ("NumFires" %in% names(calibrated_fire_params)) {
    fire_size_table$NumFires <- calibrated_fire_params[["NumFires"]]
  } else if ("NumFiresMultiplier" %in% names(calibrated_fire_params)) {
    fire_size_table$NumFires <- .scale_num_fires(
      fire_size_table$NumFires,
      calibrated_fire_params[["NumFiresMultiplier"]]
    )
  }
  fire_size_table
}


## Scale a FireDamageTable cohort-age column (internal).
##
## Shared by `patch_fire_config()`, which rewrites a calibration trial's config text, and
## `apply_calibrated_damage_age()`, which rewrites a production table, so the two cannot drift.
## Percentages are whole numbers (user guide 2.16.2), and each row must exceed the one above it:
## the extension takes the first row whose bound the cohort falls under, so a row that did not
## increase could never be reached.
.scale_damage_age <- function(pcts, mult) {
  if (!is.numeric(mult) || length(mult) != 1L || !is.finite(mult) || mult < 0) {
    stop(
      "DamageAgeMultiplier must be a non-negative finite number, not ",
      paste(format(mult), collapse = ", "),
      call. = FALSE
    )
  }
  scaled <- pmax(round(pcts * mult), 1)
  for (n in seq_along(scaled)[-1L]) {
    scaled[n] <- max(scaled[n], scaled[n - 1L] + 1L)
  }
  if (max(scaled) > 100) {
    stop(
      "DamageAgeMultiplier of ",
      mult,
      " scales the fire damage table past 100 % of longevity, which the user guide (2.16.2) ",
      "does not allow",
      call. = FALSE
    )
  }
  as.integer(scaled)
}

#' Scale a fire damage table's cohort ages by a calibrated multiplier
#'
#' Multiplies the cohort-age column of `fire_damage_table` by `DamageAgeMultiplier` when the
#' calibrated vector carries it, and leaves the table alone when it does not. This is the
#' production-side counterpart of the same scaling [patch_fire_config()] applies to a
#' calibration trial, so a calibrated table reaches the simulations it was fitted for.
#'
#' The paired severity-minus-tolerance column is never touched: the user guide (2.16.3) requires
#' an integer there, so it offers only a few reachable outcomes and is not a fittable quantity,
#' whereas the age column is a percentage of longevity (2.16.2) and scales continuously.
#'
#' @param fire_damage_table data.frame whose FIRST column is the cohort age as a percentage of
#'   longevity, as [defaultFireDamageTable()] returns.
#' @param calibrated_fire_params Named numeric vector. Used only if it holds
#'   `DamageAgeMultiplier`.
#'
#' @returns A copy of `fire_damage_table` with the age column scaled where calibrated.
#'
#' @family Dynamic Fire calibration helpers
#' @family Dynamic Fire helpers
#'
#' @export
apply_calibrated_damage_age <- function(fire_damage_table, calibrated_fire_params) {
  stopifnot(
    is.data.frame(fire_damage_table),
    ncol(fire_damage_table) >= 2L,
    nrow(fire_damage_table) > 0L,
    is.numeric(calibrated_fire_params),
    !is.null(names(calibrated_fire_params))
  )
  if ("DamageAgeMultiplier" %in% names(calibrated_fire_params)) {
    fire_damage_table[[1L]] <- .scale_damage_age(
      fire_damage_table[[1L]],
      calibrated_fire_params[["DamageAgeMultiplier"]]
    )
  }
  fire_damage_table
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

#' Observed per-fire sizes: one per ignition point, upgraded to mapped area
#'
#' One size per ignition point. Each point keeps its own `SIZE_HA` unless a
#' perimeter polygon from the SAME calendar year contains it, in which case the
#' polygon's `SIZE_HA` replaces it. For NFDB points and NBAC perimeters, this
#' keeps NFDB's full sample (pre-1972 fires, and small fires NBAC does not map)
#' while taking NBAC's satellite-derived area wherever a fire was mapped.
#'
#' This is the fire-size rule behind [save_observed_fire_targets()]'s
#' `fire_sizes_ha`. It is exported so that anything else derived from the same
#' fire record -- such as a fitted fire-size distribution -- uses the same sizes
#' as the calibration's size target, rather than a second rule that can drift
#' from it. Binding points and polygons as separate rows instead would count
#' every mapped fire twice.
#'
#' @param points SpatVector. Ignition points with `SIZE_HA` and `YEAR` columns.
#' @param polys SpatVector or NULL. Perimeter polygons with `SIZE_HA` and `YEAR`
#'   columns. NULL keeps every point's own size.
#' @param min_size_ha Numeric scalar. Sizes below this, and missing sizes, are
#'   dropped. Default `0` keeps every positive and zero size.
#'
#' @returns Numeric vector of sizes (ha), sorted ascending; at most one element
#'   per point.
#'
#' @family Dynamic Fire calibration helpers
#'
#' @export
observed_fire_sizes <- function(points, polys = NULL, min_size_ha = 0) {
  stopifnot(
    inherits(points, "SpatVector"),
    is.null(polys) || inherits(polys, "SpatVector"),
    is.numeric(min_size_ha),
    length(min_size_ha) == 1L,
    min_size_ha >= 0
  )
  pts <- as.data.frame(points)
  plys <- if (is.null(polys)) data.frame() else as.data.frame(polys)
  pts_year <- pts[["YEAR"]]

  sizes_raw <- pts[["SIZE_HA"]]
  if (
    nrow(plys) > 0L &&
      "SIZE_HA" %in% colnames(plys) &&
      "YEAR" %in% colnames(plys) &&
      "YEAR" %in% colnames(pts)
  ) {
    ## terra::extract(<polys>, <points>) returns one row per point with the
    ## intersecting polygon's attributes; ID = point index, NA where no
    ## polygon contains the point. When a point intersects multiple
    ## polygons (rare), extract returns multiple rows -- we accept the
    ## last-write-wins assignment because all matches are valid year-aligned
    ## polygons for that point.
    poly_attrs <- tryCatch(terra::extract(polys, points), error = function(e) NULL)
    if (
      !is.null(poly_attrs) &&
        nrow(poly_attrs) > 0L &&
        "SIZE_HA" %in% colnames(poly_attrs) &&
        "YEAR" %in% colnames(poly_attrs) &&
        "id.y" %in% colnames(poly_attrs)
    ) {
      ## terra >= 1.7-29 uses `id.y` for the point row index; older versions
      ## used `ID`. Support both.
      pt_idx_col <- "id.y"
    } else if (
      !is.null(poly_attrs) &&
        nrow(poly_attrs) > 0L &&
        "SIZE_HA" %in% colnames(poly_attrs) &&
        "YEAR" %in% colnames(poly_attrs) &&
        "ID" %in% colnames(poly_attrs)
    ) {
      pt_idx_col <- "ID"
    } else {
      pt_idx_col <- NA_character_
    }
    if (!is.na(pt_idx_col)) {
      pt_idx <- as.integer(poly_attrs[[pt_idx_col]])
      poly_yr <- poly_attrs[["YEAR"]]
      poly_size <- poly_attrs[["SIZE_HA"]]
      valid <- !is.na(pt_idx) &
        pt_idx >= 1L &
        pt_idx <= nrow(pts) &
        !is.na(poly_yr) &
        !is.na(poly_size) &
        !is.na(pts_year[pt_idx]) &
        poly_yr == pts_year[pt_idx]
      if (any(valid)) {
        sizes_raw[pt_idx[valid]] <- poly_size[valid]
      }
    }
  }
  sort(sizes_raw[!is.na(sizes_raw) & sizes_raw >= min_size_ha])
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
#'   \item Fire sizes come from [observed_fire_sizes()]: one per ignition
#'         point, its own `SIZE_HA` unless a same-year perimeter polygon
#'         contains it, then the polygon's `SIZE_HA`.
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
#'   primary ecoregion. When supplied, a point's size is replaced by the
#'   `SIZE_HA` (e.g. NBAC's `ADJ_HA`) of a same-year polygon containing it (see
#'   [observed_fire_sizes()]), and `area_by_fuel_ha` is computed by rasterising
#'   the polys against `fuel_types_rast`. When NULL, `fire_sizes_ha` is the
#'   points' own `SIZE_HA` (NFDB agency-reported sizes) and `area_by_fuel_ha` is
#'   NULL on the primary summary.
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
#' @param mortality_share Numeric scalar or NULL. Observed share of burned area that lost its
#'   dominant cohorts -- a high-mortality burn-severity class expressed as a proportion of assessed
#'   burned area. Stored on the primary summary and consumed by `loss_from_stats()`'s `mortality`
#'   component, which compares it with [landis_overstory_mortality_share()] per replicate. NULL
#'   leaves that component at 0.
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
  mortality_share = NULL,
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
    is.null(severity_dist) || (is.numeric(severity_dist) && !is.null(names(severity_dist))),
    is.null(mortality_share) ||
      (is.numeric(mortality_share) &&
        length(mortality_share) == 1L &&
        mortality_share >= 0 &&
        mortality_share <= 1)
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

    ## fire_sizes_ha source preference (NBAC-backfilled-with-NFDB):
    ##   1. Start from NFDB points' `SIZE_HA` (full coverage -- 1950+, every
    ##      detected fire including small ones NBAC does not map).
    ##   2. Where an NBAC polygon (in the SAME calendar year) contains the
    ##      NFDB ignition point, swap in NBAC's `SIZE_HA` -- NBAC's
    ##      satellite-derived ADJ_HA is the more accurate per-fire area.
    ## This preserves NFDB's full sample size while upgrading any matched
    ## fire to NBAC's better measurement. Earlier implementations used
    ## "polys IF supplied, else points" which silently dropped:
    ##   * pre-1972 NFDB fires (NBAC's coverage starts 1972);
    ##   * small fires that NFDB recorded but NBAC did not map
    ##     (NBAC's MAFM pipeline has a threshold around its
    ##     30-m Landsat detection floor);
    ## both of which biased the obs size distribution toward larger fires.
    ## Drop sub-`min_size_ha` fires: NFDB/NBAC are effectively left-censored
    ## at ~1 ha (small fires systematically under-reported); without this
    ## floor the KS comparison compares the sim's full distribution against
    ## a truncated observed distribution. `loss_from_stats()` applies the
    ## same truncation to `sim_sizes` symmetrically via `observed$min_size_ha`.
    fire_sizes_ha <- observed_fire_sizes(points_sv, polys_sv, min_size_ha = min_size_ha)

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
      severity_dist = NULL, ## set on the primary summary below if a prior was passed
      mortality_share = NULL ## likewise
    )
  }

  primary <- .summarise(primary_points, primary_polys, primary_label, compute_area_by_fuel = TRUE)
  primary$severity_dist <- severity_dist
  primary$mortality_share <- mortality_share
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
      "Fire sizes: one per ignition point, upgraded to a same-year containing polygon's SIZE_HA.",
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
  fs::path(dir, landis_directive(fs::path(dir, config), directive, default = default))
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
#' @param dedup Logical. Collapse duplicate communities in the resulting
#'   snapshot via [dedup_community_snapshot()] before returning. Default TRUE.
#'   Biomass Succession emits one map code per PIXEL, so on a large landscape the
#'   snapshot is overwhelmingly duplicate rows and is liable to OOM the LANDIS-II
#'   initial-communities parser when read back (see that function). The collapse
#'   is state-preserving -- every pixel keeps exactly its cohort list -- so this
#'   is on by default; set FALSE only to inspect the raw writer output.
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
  pull = FALSE,
  dedup = TRUE
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
  ## Collapse the per-pixel map codes the writer emits. Done HERE rather than at the point of use so
  ## every consumer of the snapshot -- and every downstream copy of it -- gets the small form: the raw
  ## file is what OOM-kills the LANDIS-II initial-communities parser on a large landscape.
  if (isTRUE(dedup)) {
    dedup_community_snapshot(as.character(csv_path), as.character(tif_path))
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
#' @param pixel_area_ha Numeric or NULL. Hectares per cell. Default NULL derives it from the
#'   template's `scenario.txt` `CellLength`, so fire sizes are in hectares on any grid. A value
#'   that disagrees with `CellLength` is an error: the calibration drivers never passed one, so
#'   every trial on a non-100 m grid was scored in cells against observed hectares.
#' @param keep_scratch Logical. Leave the per-trial scratch dir in place for
#'   debugging. Default FALSE.
#' @param retries Integer >= 0. Extra attempts if the simulator exits non-zero,
#'   passed to [landis_pool_exec()]. Default 0 preserves fail-fast. Set it (via
#'   `cfg$retries`) for long searches: one failed exec aborts the entire
#'   calibration, and a production run makes tens of thousands of container
#'   executions, so a rare transient becomes near-certain. Only the pooled path
#'   honours this; a genuine input fault fails on every attempt regardless.
#' @param trial_timeout_sec Numeric or NULL. Wall-clock ceiling for one
#'   simulator execution, passed to [landis_pool_exec()]. NULL (the default)
#'   waits indefinitely. `retries` only helps when the simulator *exits*; a
#'   process that wedges instead never returns, and the coordinator -- parked in
#'   a blocking read on that worker's socket -- waits with it, so the whole
#'   generation stalls behind one container with nothing logged. Set this to a
#'   generous multiple of a healthy trial's runtime (it is a deadlock breaker,
#'   not a scheduler) so a wedged trial is killed, retried and, if it keeps
#'   wedging, surfaced as an error the search can act on. Only the pooled path
#'   honours it.
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
  pixel_area_ha = NULL,
  keep_scratch = FALSE,
  retries = 0L,
  trial_timeout_sec = NULL
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
  pixel_area_ha <- .resolve_pixel_area_ha(paths$scenario_template, pixel_area_ha)
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
      stderr_log = fs::path(log_dir, "pool_stderr.log"),
      ## A failed exec aborts the WHOLE calibration: the error propagates through parApply, unwinds
      ## DEoptim and errors the target, discarding every generation since the last checkpoint. A
      ## production search is ~NP x n_reps x itermax container executions (90 x 10 x 100 = 90,000),
      ## so even a very rare transient is near-certain to hit once. Observed twice in ~27 h: LANDIS-II
      ## exited 139 (SIGSEGV) about one second in, right after "Sites: N active" and before the
      ## succession extension loaded, with no managed exception and a staged trial directory verified
      ## byte-identical to the template -- i.e. nothing wrong with the inputs.
      ##
      ## Retrying costs nothing diagnostically: a genuine input fault (unreadable pixel type, unknown
      ## map code, a grant below the landscape's peak RSS) fails identically on every attempt and
      ## still surfaces, only later by the duration of the retries.
      retries = retries,
      timeout_sec = trial_timeout_sec
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
      .par(par_vec, "SeverityCalibrationFactor", 1) *
      (1 + .par(par_vec, "SpHiProp", 0)) *
      (1 + .par(par_vec, "SumHiProp", 0)) /
      4
  )
  n_fires_per_year <- as.integer(stats::rpois(sim_years, lambda = lambda))
  total_fires <- sum(n_fires_per_year)
  fire_sizes_ha <- if (total_fires > 0L) {
    sort(stats::rlnorm(
      total_fires,
      meanlog = 3 + 1.5 * .par(par_vec, "IgnProb_Conifer", 1),
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
  ## nothing to optimise. The recognised set must stay in sync with the
  ## components emitted by `loss_from_stats()` -- size_tail was added in
  ## v0.0.51 but missing from this whitelist until v0.0.55, which silently
  ## stripped it from every cfg$weights and ran calibrations with the tail
  ## term effectively disabled.
  .known_weights <- .LOSS_COMPONENTS
  w <- cfg$weights %||% c(count = 1, size = 1, size_tail = 1, area_fuel = 0, severity = 0)
  if (all(w == 0)) {
    stop(
      "cfg$weights are all zero; DEoptim has nothing to optimise. ",
      "Set at least one of ",
      paste(.known_weights, collapse = " / "),
      " to > 0.",
      call. = FALSE
    )
  }
  unknown_w <- setdiff(names(w), .known_weights)
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
    .weight_gt0(w, "area_fuel") &&
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
  if (.weight_gt0(w, "severity") && is.null(primary$severity_dist)) {
    warning(
      "cfg$weights['severity'] > 0 but observed$primary$severity_dist is NULL; ",
      "L_severity will contribute 0. Pass `severity_dist = ",
      "default_severity_prior_sturtevant2009()` to save_observed_fire_targets().",
      call. = FALSE
    )
  }
  if (
    .weight_gt0(w, "area_burned") &&
      (is.null(primary$fire_sizes_ha) || length(primary$fire_sizes_ha) == 0L)
  ) {
    warning(
      "cfg$weights['area_burned'] > 0 but observed$primary$fire_sizes_ha is empty, so the ",
      "observed annual area burned (lambda_obs x mean fire size) cannot be formed; ",
      "L_area_burned will contribute 0. Either set the weight to 0 or rebuild the payload with ",
      "save_observed_fire_targets().",
      call. = FALSE
    )
  }
  if (.weight_gt0(w, "area_burned") && is.null(observed$pixel_area_ha)) {
    warning(
      "cfg$weights['area_burned'] > 0 but observed$pixel_area_ha is NULL, so simulated burned ",
      "cells will be converted at 1 ha each. On any grid other than 100 m that scores simulated ",
      "area against observed area in different units. Rebuild the payload with ",
      "save_observed_fire_targets(), which records the cell area of the fuel raster.",
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
## Per-container RAM ESTIMATE implied by `cfg`, in GiB.
##
## Kept beside .cfg_mem_limit() and used by BOTH the calibration pool and the validation pool.
## They used to compute this independently, and validation's copy was a bare `mem_limit = "8g"`
## that ignored `cfg` entirely: on a 397k-active-cell landscape whose measured ForCS peak is
## 11.0-11.1 GiB, every validation replicate died in `ForC.SiteVars.Initialize` with
## System.OutOfMemoryException about 150 s in, while the calibration that produced the very
## parameters being validated ran fine on its 13 GiB grant. One source, so they cannot drift again.
.cfg_mem_per_worker <- function(cfg) {
  as.numeric(cfg$mem_per_worker_gb %||% .mem_limit_to_gb(cfg$mem_limit %||% "8g"))
}

## Per-container `--memory` GRANT implied by `cfg`. 1.25x the estimate as headroom, floored at 8g
## so existing small-area configs are unchanged.
.cfg_mem_limit <- function(cfg) {
  cfg$mem_limit %||% sprintf("%dg", max(8L, ceiling(.cfg_mem_per_worker(cfg) * 1.25)))
}

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
#' @details
#' ## Checkpoint / resume (opt-in)
#'
#' Set `cfg$checkpoint_every = K` to make the search resumable. The DEoptim run
#' is then executed in blocks of `K` generations; after each block the full
#' population, best-so-far parameters, and best-value history are written
#' atomically to `out_dir` (`checkpoint.rds` and `best_params_so_far.rds`) and
#' the next block reseeds `DEoptim.control(initialpop=)` from the saved
#' population. If the run is interrupted (crash, node reboot, kill), the next
#' call resumes from the last checkpoint instead of restarting from generation 1.
#' Resume is a warm restart: it restores the population and best-so-far, not
#' DEoptim's internal RNG stream or generation counter, so it is not bit-for-bit
#' identical to an uninterrupted run (acceptable for calibration). Previously
#' evaluated parameter vectors are memoized via the trial-trace CSVs, so resumed
#' points and per-block re-evaluations skip their (expensive) simulator runs.
#' Point `out_dir` at storage that survives a reboot (the pipeline passes the NFS
#' `outputs/calibration/`). When `checkpoint_every` is `NULL` (the default),
#' behaviour is unchanged: a single monolithic `DEoptim()` call, no checkpoint
#' files.
#'
#' Resume and the memoization cache are both scoped to a *loss-config
#' fingerprint* -- a hash of the weights, per-trial sim settings (`n_reps`,
#' `sim_years`, `base_seed`, `simulator`, `method`, Docker `image`), and the
#' observed targets -- in addition to the population fingerprint (par names,
#' bounds, `NP`). Change any loss-affecting input and a checkpoint/cache left in
#' the same `out_dir` is silently ignored rather than resumed or folded in, so
#' reusing a single `out_dir` (e.g. the persistent `outputs/calibration/`) across
#' successive calibrations with different weights or observations does NOT poison
#' the new run's objective. You therefore do not need to clear `out_dir` by hand
#' after a config change; only the population geometry (par count / bounds / `NP`)
#' and the loss config must be stable for a resume to take effect.
#'
#' What the fingerprint does NOT cover is the loss COMPUTATION. It digests the
#' calibration's inputs, not the code that turns them into a number, and not the
#' package version. So a change to how a component is calculated leaves the
#' fingerprint byte-identical, the cache is accepted rather than rejected, and a
#' post-change run is served pre-change losses for every parameter vector it has
#' seen before -- silently, and mixed in with correctly computed ones.
#'
#' Deleting `checkpoint.rds` is NOT sufficient to get a clean slate: the memoized
#' losses live in the trial-trace and `worker_*.csv` files, which
#' `.augment_eval_cache()` folds in separately and RECURSIVELY from `out_dir`.
#' After any release that changes a loss component, start the next calibration
#' with `resume = "never"`, which skips that step entirely.
#'
#' The honest framing is that the fingerprint is a cheap guard against obviously
#' mismatched reuse, not a correctness guarantee in either direction. It has been
#' reported as too SENSITIVE (rejecting a valid resume after a cosmetic template
#' rebuild) and, as above, as not sensitive ENOUGH. Both follow from digesting
#' inputs rather than the computation.
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
#'       itermax`) to disable early stopping and always run the full schedule;
#'       a run configured that way says so in a startup message. Omitting
#'       `steptol` (or setting it to `NULL`) gives the 25-generation default, not
#'       DEoptim's own `steptol = itermax`.}
#'     \item{n_reps, sim_years, weights, base_seed}{Per-trial settings.}
#'     \item{n_cores, parallel}{Parallelism settings.}
#'     \item{simulator}{`"landis"` (default), `"r_reimpl"`, or `"mock"`.}
#'     \item{method}{`"docker"` (default) or `"local"`.}
#'     \item{image, cpu_limit, mem_limit, pull}{Pool settings (Docker only).}
#'     \item{nodes}{Optional named vector of workers per host, e.g.
#'       `c(host1 = 30, host2 = 30)`, spreading the search across machines via a
#'       PSOCK cluster. Each worker runs its own single container on its own
#'       host. Per-host counts are capped against that host's available RAM and
#'       the total is trimmed to `NP` (workers beyond `NP` never receive a task
#'       but still hold a container). Requires the `parallelly` package, the
#'       Docker image on every host, and -- under renv -- the project at the same
#'       path everywhere. Unset (default) uses a local FORK cluster and one
#'       shared pool, which is unchanged. Note the workload is
#'       memory-bandwidth-bound, so one host saturates well before its cores are
#'       busy; spreading a fixed `NP` over more hosts therefore helps more than
#'       the host count suggests, because each host also returns to its
#'       unsaturated regime.}
#'     \item{rscript}{Path to `Rscript` on the worker hosts. Defaults to the
#'       coordinator's own (`file.path(R.home("bin"), "Rscript")`), which keeps
#'       the workers on a matching R version.}
#'     \item{checkpoint_every}{Optional integer >= 1. When set, run the search in
#'       blocks of this many generations and persist a resumable checkpoint to
#'       `out_dir` between blocks (see Details). `NULL` (default) = single
#'       monolithic `DEoptim()` call.}
#'     \item{resume}{`"auto"` (default), `"never"`, or `"force"`. Only used when
#'       `checkpoint_every` is set. `"auto"` resumes from `out_dir/checkpoint.rds`
#'       iff BOTH its population fingerprint (par names + bounds + NP) AND its
#'       loss-config fingerprint (weights + sim settings + observed + image)
#'       match; `"force"` resumes regardless of either; `"never"` ignores any
#'       checkpoint and starts from an empty memoization cache (a clean slate).}
#'     \item{trial_timeout_sec}{Optional numeric. Wall-clock ceiling on ONE
#'       simulator execution (see [sim_landis()]). `retries` only rescues a
#'       simulator that exits; one that wedges never returns, and the whole
#'       generation waits behind it. Recommended for any unattended search.
#'       Deliberately excluded from both fingerprints, so it can be added to or
#'       changed on an in-flight search without invalidating its checkpoint.}
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
  .check_observed_pixel_area(observed, scenario_template)
  ## The SEARCHED parameters are whichever `cfg$lower` / `cfg$upper` name, in a stable order -- not
  ## necessarily all of `calibration_par_names()`. Requiring the full set (the former
  ## `setequal(...)`) forced every calibration to search dimensions that a project sets from data,
  ## or that are degenerate for its fire regime (a season whose FMCLo equals its FMCHi leaves its
  ## HiProp unable to move any outcome). `patch_fire_config()` leaves an unnamed field at its
  ## template value, so a subset is well defined end to end.
  stopifnot(
    !is.null(names(cfg$lower)),
    !is.null(names(cfg$upper)),
    length(cfg$lower) > 0L,
    setequal(names(cfg$lower), names(cfg$upper)),
    all(names(cfg$lower) %in% calibration_par_names())
  )
  .check_num_fires_params(names(cfg$lower))
  ## Order by calibration_par_names() so the vector layout is stable across runs regardless of the
  ## order the caller happened to write the bounds in.
  par_names <- calibration_par_names()[calibration_par_names() %in% names(cfg$lower)]
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
  weights <- cfg$weights %||% c(count = 1, size = 1, size_tail = 1, area_fuel = 0, severity = 0)
  base_seed <- as.integer(cfg$base_seed %||% 12345L)
  sim_years <- as.integer(cfg$sim_years %||% 10L)

  ## Optional checkpoint/resume (opt-in via cfg$checkpoint_every). When set, the
  ## DEoptim search runs in blocks of `checkpoint_every` generations, persisting
  ## the population + best-so-far to `out_dir` between blocks so an interrupted
  ## run (crash / node reboot / kill) resumes instead of restarting from
  ## generation 1 (see .run_deoptim_checkpointed()). Validate early so a bad
  ## value fails before the expensive warm pool / FORK cluster is set up.
  use_checkpoint <- !is.null(cfg$checkpoint_every)
  if (use_checkpoint) {
    stopifnot(
      is.numeric(cfg$checkpoint_every),
      length(cfg$checkpoint_every) == 1L,
      cfg$checkpoint_every >= 1L
    )
    cfg$resume <- match.arg(cfg$resume %||% "auto", c("auto", "never", "force"))
  }

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
  mem_per_worker <- .cfg_mem_per_worker(cfg)
  avail_gb <- .available_ram_gb()
  ## Cap against what each container is ALLOWED to consume, not the estimate that allowance is derived
  ## from. `mem_limit` grants 1.25x the estimate as headroom, so capping on the estimate over-subscribes
  ## the host by that same 25%: at 30.3 GiB/worker the cap admitted 27 containers, each granted 38 GiB
  ## -- 1026 GiB on a 1007 GiB node. A container is entitled to use its full --memory, so the invariant
  ## that must hold is `n * mem_limit <= mem_fraction * avail`, and only capping on the limit gives it.
  mem_limit <- .cfg_mem_limit(cfg)
  mem_per_container <- .mem_limit_to_gb(mem_limit)
  capped_cores <- .ram_pool_cap(n_cores, mem_per_container, cfg$mem_fraction %||% 0.85, avail_gb)
  if (capped_cores < n_cores) {
    message(glue::glue(
      "calibrate_dynamic_fire: RAM-capping warm pool {n_cores} -> {capped_cores} container(s) ",
      "({round(avail_gb)} GiB avail x {cfg$mem_fraction %||% 0.85} / {round(mem_per_container, 1)} ",
      "GiB per-container limit '{mem_limit}'). Set cfg$mem_per_worker_gb or cfg$mem_limit to tune."
    ))
    n_cores <- capped_cores
  }
  use_parallel <- isTRUE(cfg$parallel %||% TRUE) && n_cores > 1L

  ## ---- multi-node (PSOCK) path ---------------------------------------------------------------
  ## cfg$nodes = c(host1 = n1, host2 = n2, ...) spreads the search across machines. Each worker owns
  ## a single container on its own host, so no shared pool and no cross-host container index exist.
  ## Unset (the default) leaves the single-node FORK path below untouched.
  multi <- NULL
  pool <- NULL
  cl <- NULL
  if (!is.null(cfg$nodes) && use_parallel) {
    needs_docker <- simulator_name == "landis" && method == "docker"
    multi <- .start_calibration_cluster(
      nodes = cfg$nodes,
      ## DEoptim dispatches exactly NP tasks per generation; workers beyond that idle while holding
      ## a container, so cap the fleet at NP.
      ## NB: must read cfg$NP, not control_args$NP -- control_args is not built until further down.
      ## Mirror its default (60L) so the cap matches what DEoptim will actually dispatch.
      max_workers = as.integer(cfg$NP %||% 60L),
      image = cfg$image,
      scratch_root = scratch_root,
      cpu_limit = cfg$cpu_limit %||% 2,
      mem_limit = mem_limit,
      mem_fraction = cfg$mem_fraction %||% 0.85,
      pull = isTRUE(cfg$pull %||% FALSE),
      name_prefix = paste0("landis-cal-", Sys.getpid()),
      rscript = cfg$rscript %||% file.path(R.home("bin"), "Rscript"),
      start_pools = needs_docker,
      ## LANDIS-II is effectively single-threaded, so one container occupies ~one PHYSICAL core.
      cores_per_worker = as.numeric(cfg$cores_per_worker %||% 1),
      cpu_fraction = as.numeric(cfg$cpu_fraction %||% 0.85)
    )
  }
  if (!is.null(multi)) {
    cl <- multi$cl
    n_cores <- multi$total
    ## Pools live on the workers, so they must be stopped through the cluster and BEFORE it.
    on.exit(.stop_calibration_cluster(multi), add = TRUE, after = FALSE)
  }

  ## Pool lifecycle: only LANDIS-II Docker + parallel needs a pool. Mock /
  ## r_reimpl / local-method runs don't touch Docker.
  if (is.null(multi) && simulator_name == "landis" && method == "docker" && use_parallel) {
    pool <- landis_pool_start(
      n = n_cores,
      image = cfg$image,
      scratch_root = scratch_root,
      cpu_limit = cfg$cpu_limit %||% 2,
      ## --memory must be >= expected per-container usage or docker OOM-kills the container; derived
      ## above (RAM estimate + 25% headroom) when not set explicitly, and reused verbatim here so the
      ## value the pool is CAPPED against is the value each container is GRANTED.
      mem_limit = mem_limit,
      pull = isTRUE(cfg$pull %||% FALSE),
      name_prefix = paste0("landis-cal-", Sys.getpid())
    )
    ## Tear down the pool before the cluster (FORK children inherit the pool's
    ## state but don't own its containers; clean up containers first).
    on.exit(landis_pool_stop(pool), add = TRUE)
  }

  ## FORK cluster -- workers inherit the parent's environment including `pool`. Skipped entirely when
  ## a multi-node PSOCK cluster is already up.
  if (is.null(multi) && use_parallel && .Platform$OS.type != "windows") {
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

  ## Loss-config fingerprint: hashes everything BESIDES the parameter vector that
  ## determines objfn's return value (weights, per-trial sim settings, observed
  ## targets, Docker image, and the scenario template's contents). Stamped into
  ## every trial-trace row and the checkpoint so a reused out_dir seeds the
  ## memoization cache / resumes the population ONLY from evaluations produced
  ## under the identical loss config -- change any of these and the stale state
  ## self-invalidates instead of poisoning the new run's objective. (The narrower
  ## population fingerprint in .run_deoptim_checkpointed() additionally guards the
  ## population geometry: par names + bounds + NP.)
  eval_fp <- .eval_fingerprint(
    par_names = par_names,
    weights = weights,
    n_reps = n_reps,
    sim_years = sim_years,
    base_seed = base_seed,
    simulator_name = simulator_name,
    method = method,
    image = cfg$image %||% getOption("landisutils.docker.image"),
    observed = observed,
    template_digest = .scenario_template_digest(scenario_template)
  )

  ## Memoization cache (strategy #2; only when checkpointing). Keyed by a stable
  ## hash of the parameter vector -> loss total, seeded from the trial-trace CSVs
  ## under out_dir whose eval fingerprint matches `eval_fp` (a prior interrupted
  ## run of THIS config). This makes resumed points and each block's initialpop
  ## re-evaluations cache hits instead of fresh (and expensive) LANDIS runs.
  ## `resume = "never"` starts from an empty cache (a genuine clean slate). FORK
  ## children receive a copy of this env via objfn's serialized closure; their
  ## in-memory writes stay local, but the durable channel back to the parent is
  ## the per-worker trial-trace CSV, re-read via .augment_eval_cache() between
  ## blocks.
  eval_cache <- if (use_checkpoint) {
    .load_eval_cache(out_dir, par_names, eval_fp, resume = cfg$resume)
  } else {
    NULL
  }

  objfn <- function(par_vec) {
    names(par_vec) <- par_names
    cache_key <- if (!is.null(eval_cache)) .par_key(par_vec) else NULL
    if (!is.null(cache_key)) {
      cached <- eval_cache[[cache_key]]
      if (!is.null(cached)) {
        return(cached)
      }
    }
    ## On a PSOCK worker this returns that worker's OWN single-container pool (idx 1); on a FORK child
    ## or the coordinator it falls through to the shared pool + LANDIS_POOL_CONTAINER_IDX. Either way
    ## `landis_pool_exec()` ends up naming a container on the machine it is executing on.
    .pi <- .resolve_pool(pool)
    reps <- lapply(seq_len(n_reps), function(i) {
      simulator(
        par_vec = par_vec,
        par_names = par_names,
        paths = paths,
        sim_years = sim_years,
        base_seed = base_seed + i,
        pool = .pi$pool,
        pool_idx = .pi$idx,
        method = method,
        ## Retry a failed simulator exec rather than aborting the search; see sim_landis()'s
        ## `retries` docs. `sim_mock` ignores extra arguments via its `...`.
        retries = as.integer(cfg$retries %||% 0L),
        ## Bound a single execution so a wedged container cannot stall the generation behind it;
        ## see sim_landis()'s `trial_timeout_sec` docs. NULL keeps the historical wait-forever.
        trial_timeout_sec = cfg$trial_timeout_sec
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
      weights = .loss$weights,
      eval_fp = eval_fp
    )
    if (!is.null(cache_key)) {
      eval_cache[[cache_key]] <- .loss$total
    }
    .loss$total
  }

  control_args <- list(
    NP = as.integer(cfg$NP %||% 60L),
    itermax = as.integer(cfg$itermax %||% 100L),
    strategy = as.integer(cfg$strategy %||% 3L),
    trace = isTRUE(cfg$trace %||% TRUE),
    storepopfrom = 1L,
    storepopfreq = 5L,
    ## Early-stopping (the OUTER block loop halts when bestvalit fails to improve by more than
    ## `reltol` over `steptol` generations; DEoptim's own in-block check is disabled). Disable it
    ## by setting cfg$steptol >= cfg$itermax. Note `cfg$steptol = NULL` does NOT fall through to
    ## DEoptim's upstream default -- `%||%` treats NULL as absent, so it yields the 25 below.
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
  message(glue::glue(
    "calibrate_dynamic_fire: simulator={simulator_name}, NP={control_args$NP}, ",
    "itermax={control_args$itermax}, reltol={control_args$reltol}, ",
    "steptol={control_args$steptol}, n_reps={n_reps}, sim_years={sim_years}, ",
    "n_cores={if (is.null(cl)) 1L else n_cores}, pool={!is.null(pool)}, ",
    "checkpoint_every={cfg$checkpoint_every %||% NA}"
  ))

  if (use_checkpoint) {
    ## Block-restart with population checkpointing + memoization + anytime write.
    res <- .run_deoptim_checkpointed(
      objfn = objfn,
      cfg = cfg,
      par_names = par_names,
      control_args = control_args,
      eval_cache = eval_cache,
      eval_fp = eval_fp,
      out_dir = out_dir
    )
  } else {
    ## Unchanged monolithic path: a single DEoptim call over the whole schedule.
    control <- do.call(DEoptim::DEoptim.control, control_args)
    res <- DEoptim::DEoptim(
      fn = objfn,
      lower = unname(cfg$lower),
      upper = unname(cfg$upper),
      control = control
    )
  }

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
.write_trial_trace_row <- function(
  dir,
  par_vec,
  par_names,
  total,
  components,
  weights,
  eval_fp = NA_character_
) {
  pid <- Sys.getpid()
  ## Key the sidecar by HOST + pid, not pid alone. `dir` is on shared storage (the caller passes the
  ## NFS out_dir), and PIDs are unique per host only -- once workers span nodes, two of them can hold
  ## the same pid and append interleaved rows to one file, corrupting both the trace and the
  ## memoization cache built from it. The host segment is sanitised because it lands in a filename.
  host <- tryCatch(as.character(Sys.info()[["nodename"]]), error = function(e) "")
  if (!length(host) || is.na(host) || !nzchar(host)) {
    host <- "localhost"
  }
  host <- gsub("[^A-Za-z0-9]+", "-", host)
  f <- fs::path(dir, sprintf("worker_%s_%d.csv", host, pid))
  comp_names <- names(components)
  weight_vals <- as.numeric(weights[comp_names])
  weighted <- as.numeric(components) * weight_vals
  ## Row schema: wall_clock_iso, pid, par_<name>..., total, eval_fp, comp_<name>..., w_<name>..., weighted_<name>...
  ## `par_*` and `total` are written at full (round-trippable) precision via
  ## .fmt_par() so the memoization cache (.load_eval_cache() / .par_key()) keys a
  ## value identically whether it comes straight from DEoptim or is re-read from
  ## this CSV. `eval_fp` is the loss-config fingerprint (weights + sim settings +
  ## observed targets + image); .augment_eval_cache() folds a row into the cache
  ## only when it matches the current run's fingerprint, so a reused out_dir never
  ## seeds a stale (wrong-config) loss. The diagnostic component columns keep
  ## default formatting.
  row <- c(
    list(wall_clock_iso = format(Sys.time(), "%Y-%m-%dT%H:%M:%OS3%z"), pid = pid),
    stats::setNames(as.list(.fmt_par(par_vec[par_names])), paste0("par_", par_names)),
    list(total = .fmt_par(total), eval_fp = eval_fp),
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

## ---- checkpoint / resume / memoization helpers (opt-in via cfg$checkpoint_every) -----------------
##
## Together these turn the monolithic DEoptim call into a resumable, block-restart
## search: the population is persisted between blocks (strategy #1), previously
## evaluated points are cached so resumes and per-block initialpop re-evaluations
## skip their LANDIS runs (strategy #2), and the best-so-far is written every block
## as a usable fallback (strategy #3). All state lives under `out_dir` (the caller
## passes the NFS `outputs/calibration/`, which survives a node reboot).

## Format a numeric vector at full, round-trippable precision. `%.17g` is enough
## to serialise then parse an IEEE double without loss, so a value keys to the
## same hash whether it comes straight from DEoptim or is re-read from a
## trial-trace CSV. Used by both .par_key() and .write_trial_trace_row().
.fmt_par <- function(v) {
  sprintf("%.17g", as.numeric(v))
}

## Stable cache key for a parameter vector (house style: digest + xxhash64).
.par_key <- function(par_vec) {
  digest::digest(.fmt_par(par_vec), algo = "xxhash64")
}

## Internal: content digest of a calibration scenario template directory. Hashes each file's name
## and contents (sorted, so directory-listing order cannot perturb it), giving one value that changes
## whenever ANY simulated input changes. NULL / a missing directory digests to NULL, which keeps the
## mock and non-Docker simulators -- which have no template -- working unchanged.
##
## Content, not mtime: `build_calibration_scenario_template()` rewrites the whole directory on every
## build, so mtimes change even when nothing simulated does, and a timestamp-based digest would
## discard a valid cache on every rebuild.
.scenario_template_digest <- function(scenario_template) {
  if (is.null(scenario_template) || !length(scenario_template)) {
    return(NULL)
  }
  ## `scenario_template` is the scenario FILE (".../scenario.txt"), not the directory holding it --
  ## that is how every caller passes it, and run_calibration_validation() takes `dirname()` of it
  ## for exactly this reason. Requiring a directory made this return NULL on every real call, so the
  ## template contributed NOTHING to the eval fingerprint and a rebuilt template's losses were
  ## served straight from the memoization cache. That is precisely the failure 0.0.75 added this
  ## function to prevent, and it went unnoticed because a silent NULL is indistinguishable from a
  ## template that genuinely has not changed. Accept either form, and say so when neither resolves.
  dir <- if (fs::dir_exists(scenario_template)) {
    scenario_template
  } else if (fs::file_exists(scenario_template)) {
    dirname(scenario_template)
  } else {
    warning(
      "scenario template '",
      scenario_template,
      "' is neither a directory nor a file; the evaluation fingerprint cannot see the template, ",
      "so cached losses will survive a template change.",
      call. = FALSE
    )
    return(NULL)
  }
  files <- sort(as.character(fs::dir_ls(dir, type = "file")))
  if (!length(files)) {
    warning(
      "scenario template directory '",
      dir,
      "' contains no files; the evaluation fingerprint cannot see the template.",
      call. = FALSE
    )
    return(NULL)
  }
  digest::digest(
    list(
      names = basename(files),
      ## unname(): vapply carries `files` (ABSOLUTE paths) through as names, and digest() hashes
      ## names as well as values -- so without this, two byte-identical templates staged under
      ## different directories digest differently and every rebuild discards a valid cache.
      contents = unname(vapply(
        files,
        digest::digest,
        character(1L),
        file = TRUE,
        algo = "xxhash64"
      ))
    ),
    algo = "xxhash64"
  )
}

## Loss-config fingerprint: a stable hash of everything BESIDES the parameter
## vector that determines objfn's return value for a given par_vec -- the loss
## `weights`, the per-trial sim settings (`n_reps`, `sim_years`, `base_seed`,
## `simulator`, `method`, Docker `image`), the `observed` targets, the scenario
## template's contents, and the parameter ordering. Written into each trial-trace
## row and the checkpoint so a resumed run / seeded memoization cache reuses ONLY
## evaluations produced under the identical loss config: change any input and the
## stale cache is silently dropped instead of poisoning the new run's objective
## (so the caller need not clear out_dir on a config change). Weights are
## name-sorted so input order does not perturb the hash. DEoptim search knobs (NP,
## bounds, strategy, core count) are deliberately excluded -- they steer the search
## but do not change the loss VALUE at a given par_vec; population geometry is
## guarded separately by the checkpoint's `fingerprint`.
.eval_fingerprint <- function(
  par_names,
  weights,
  n_reps,
  sim_years,
  base_seed,
  simulator_name,
  method,
  image,
  observed,
  template_digest = NULL
) {
  digest::digest(
    list(
      par_names = par_names,
      weights = weights[order(names(weights))],
      n_reps = as.integer(n_reps),
      sim_years = as.integer(sim_years),
      base_seed = as.integer(base_seed),
      simulator = simulator_name,
      method = method,
      image = image,
      observed = observed,
      ## The scenario template is the largest determinant of a trial's loss and was absent here until
      ## 0.0.75. `sim_years` above does NOT stand in for it: that value is informational (sim_landis()
      ## documents the run Duration as coming from the template's scenario.txt), so a template rebuilt
      ## at a different Duration hashed IDENTICALLY to the old one and its losses were served straight
      ## out of the memoization cache. Initial communities, ecoregions, the weather database, the fuel
      ## and fire tables and the landscape extent were all equally invisible.
      template = template_digest
    ),
    algo = "xxhash64"
  )
}

## Write an .rds atomically: serialise to a per-process temp sibling then rename
## (atomic within one filesystem), so an interrupted write never leaves a
## half-written, unreadable checkpoint behind.
.atomic_saveRDS <- function(obj, path) {
  tmp <- fs::path(paste0(path, ".tmp-", Sys.getpid()))
  saveRDS(obj, tmp)
  fs::file_move(tmp, path)
  invisible(path)
}

## Build a memoization environment (`.par_key(par) -> total`) from the trial-trace
## CSVs under `out_dir` whose eval fingerprint matches `eval_fp`: the per-worker
## sidecars (`trial_trace_*/worker_*.csv`) and any merged `trial_trace_*.csv` left
## by a prior (possibly interrupted) run of the SAME config. Reuses the schema
## written by .write_trial_trace_row() (`par_<name>...`, `total`, `eval_fp`).
## `resume = "never"` returns an empty cache (a genuine clean slate); within-run
## evaluations still accrue into it during the run.
.load_eval_cache <- function(out_dir, par_names, eval_fp, resume = "auto") {
  cache <- new.env(parent = emptyenv())
  if (!identical(resume, "never")) {
    .augment_eval_cache(cache, out_dir, par_names, eval_fp)
  }
  cache
}

## Fold any trial-trace rows not yet in `cache` into it (idempotent; first value
## for a key wins). Only rows whose `eval_fp` column matches the current run's
## fingerprint are folded, so a reused out_dir never seeds the cache with a stale
## (different weights / observed / image) loss; rows from a pre-fingerprint CSV
## format (no `eval_fp` column) are skipped for the same reason. Called between
## blocks so evaluations written by FORK children during a block become cache hits
## for the next block's initialpop re-evaluation.
.augment_eval_cache <- function(cache, out_dir, par_names, eval_fp) {
  files <- c(
    fs::dir_ls(
      out_dir,
      recurse = TRUE,
      type = "file",
      ## Matches BOTH the legacy `worker_<pid>.csv` and the host-qualified
      ## `worker_<host>_<pid>.csv` written since multi-node support, so a resumed run still folds
      ## sidecars written by an older version into its cache.
      regexp = "worker_.*[0-9]+\\.csv$",
      fail = FALSE
    ),
    fs::dir_ls(
      out_dir,
      recurse = FALSE,
      type = "file",
      regexp = "trial_trace_[0-9_]+\\.csv$",
      fail = FALSE
    )
  )
  par_cols <- paste0("par_", par_names)
  for (f in files) {
    df <- tryCatch(utils::read.csv(f, stringsAsFactors = FALSE), error = function(e) NULL)
    if (is.null(df) || nrow(df) == 0L || !all(c(par_cols, "total", "eval_fp") %in% colnames(df))) {
      next
    }
    keep <- !is.na(df$eval_fp) & as.character(df$eval_fp) == eval_fp
    df <- df[keep, , drop = FALSE]
    for (i in seq_len(nrow(df))) {
      key <- .par_key(as.numeric(df[i, par_cols]))
      if (is.null(cache[[key]])) {
        cache[[key]] <- as.numeric(df[i, "total"])
      }
    }
  }
  invisible(cache)
}

## DEoptim's own early-stop rule, applied across blocks: converged when the best
## value has failed to improve by more than `reltol` (relative) over the last
## `steptol` generations. Mirrors DEoptim.control(reltol=, steptol=).
.deoptim_converged <- function(bestval_history, reltol, steptol) {
  n <- length(bestval_history)
  if (n <= steptol) {
    return(FALSE)
  }
  prev <- bestval_history[n - steptol]
  cur <- bestval_history[n]
  (prev - cur) <= reltol * abs(cur)
}

## Block-restart DEoptim with population checkpointing. Runs the search in blocks
## of cfg$checkpoint_every generations; after each block it atomically persists
## the population + best-so-far + full best-value history to `out_dir` and reseeds
## control$initialpop from the population, so an interrupted run resumes from the
## last checkpoint instead of restarting from generation 1. Resume (in "auto"
## mode) requires BOTH the population fingerprint (par names + bounds + NP, which
## keeps the saved population geometrically valid) AND the loss-config `eval_fp`
## (weights + sim settings + observed + image, which keeps the carried best_val /
## history meaningful) to match; any mismatch archives the stale checkpoint and
## starts fresh (cfg$resume = "force" overrides both; "never" ignores any
## checkpoint). Early-stopping (reltol/steptol) is enforced here across blocks
## -- each block runs its full K generations. Returns a DEoptim-shaped result
## whose member$bestvalit / optim$bestmem / optim$bestval span the entire run.
.run_deoptim_checkpointed <- function(
  objfn,
  cfg,
  par_names,
  control_args,
  eval_cache,
  eval_fp,
  out_dir
) {
  K <- as.integer(cfg$checkpoint_every)
  itermax_total <- as.integer(cfg$itermax %||% 100L)
  reltol <- as.numeric(cfg$reltol %||% 1e-3)
  steptol <- as.integer(cfg$steptol %||% 25L)
  resume_mode <- cfg$resume %||% "auto"
  lower <- unname(cfg$lower)
  upper <- unname(cfg$upper)

  fingerprint <- digest::digest(
    list(par_names, as.numeric(cfg$lower), as.numeric(cfg$upper), as.integer(cfg$NP %||% 60L)),
    algo = "xxhash64"
  )
  ckpt_path <- fs::path(out_dir, "checkpoint.rds")

  ## `.deoptim_converged()` needs a history LONGER than `steptol`, so a run whose whole budget is
  ## no longer than `steptol` can never satisfy it and will always run to `itermax` -- which looks
  ## exactly like early stopping being broken. Setting `steptol >= itermax` is the documented way
  ## to DISABLE early stopping, so this is only worth saying out loud, not correcting.
  if (itermax_total <= steptol) {
    message(glue::glue(
      "calibrate_dynamic_fire: early stopping is disabled (steptol = {steptol} >= ",
      "itermax = {itermax_total}); the search will run all {itermax_total} generation(s)."
    ))
  }

  pop <- NULL
  gens_done <- 0L
  best_hist <- numeric(0)
  best_mem <- NULL
  best_val <- Inf

  if (resume_mode != "never" && fs::file_exists(ckpt_path)) {
    st <- tryCatch(readRDS(ckpt_path), error = function(e) NULL)
    resume_ok <- !is.null(st) &&
      (resume_mode == "force" ||
        (identical(st$fingerprint, fingerprint) && identical(st$eval_fingerprint, eval_fp)))
    if (resume_ok) {
      pop <- st$pop
      gens_done <- as.integer(st$gens_done)
      best_hist <- as.numeric(st$bestval_history)
      best_mem <- st$best_mem
      best_val <- as.numeric(st$best_val)
      message(glue::glue(
        "calibrate_dynamic_fire: RESUMING from generation {gens_done} ",
        "(population {nrow(pop)}x{ncol(pop)}, best_val={signif(best_val, 4)})"
      ))
    } else if (!is.null(st)) {
      stale <- fs::path(
        out_dir,
        sprintf("checkpoint_stale_%s.rds", format(Sys.time(), "%Y%m%d_%H%M%S"))
      )
      fs::file_move(ckpt_path, stale)
      message(glue::glue(
        "calibrate_dynamic_fire: checkpoint config fingerprint mismatch; ",
        "archived to {fs::path_file(stale)} and starting fresh"
      ))
    }
  }

  res <- NULL
  repeat {
    k <- min(K, itermax_total - gens_done)
    ## Budget check FIRST: the shrink below has a `max(1L, ...)` floor, so applying it to an
    ## exhausted budget would resurrect a 1-generation block and run past `itermax` forever.
    if (k <= 0L) {
      break
    }
    ## The convergence test below can only be applied at a block boundary, and it needs a history
    ## longer than `steptol` before it says anything. A full-size block therefore overshoots the
    ## earliest generation that satisfies it: at checkpoint_every = 5 and steptol = 25 the first
    ## checkable generation is 26, but boundaries fall on 25 and 30, so a run that had converged by
    ## 26 kept going to 30 -- four wasted generations, and more as checkpoint_every grows. Shrink
    ## the block to land exactly on generation `steptol + 1`, then advance one generation at a time
    ## so the test is applied every generation. The extra cost is one small checkpoint write per
    ## generation, against a generation that takes hours. `k` only ever shrinks, so the itermax
    ## budget established above still holds.
    if (gens_done + k > steptol) {
      k <- max(1L, min(k, steptol + 1L - gens_done))
    }
    blk_args <- control_args
    blk_args$itermax <- as.integer(k)
    ## The outer loop owns global early-stopping, so disable the in-block check
    ## (steptol >= itermax => a block always runs its full K generations).
    blk_args$steptol <- as.integer(k)
    ## Disable DEoptim's intermediate population storage per block: it is unused
    ## here (the checkpoint uses res$member$pop) and its indexing errors when a
    ## block's itermax is smaller than storepopfreq (e.g. checkpoint_every = 1).
    blk_args$storepopfrom <- as.integer(k) + 1L
    if (!is.null(pop)) {
      blk_args$initialpop <- pop
    }
    res <- DEoptim::DEoptim(
      fn = objfn,
      lower = lower,
      upper = upper,
      control = do.call(DEoptim::DEoptim.control, blk_args)
    )
    pop <- res$member$pop
    gens_done <- gens_done + k
    best_hist <- c(best_hist, as.numeric(res$member$bestvalit))
    if (as.numeric(res$optim$bestval) < best_val) {
      best_val <- as.numeric(res$optim$bestval)
      best_mem <- as.numeric(res$optim$bestmem)
    }
    ## strategy #3: anytime best-params (usable even if the run never completes)
    .atomic_saveRDS(
      list(
        best_params = stats::setNames(best_mem, par_names),
        objective = best_val,
        gens_done = gens_done,
        computed_at = Sys.time()
      ),
      fs::path(out_dir, "best_params_so_far.rds")
    )
    ## strategy #1: population checkpoint (fingerprint-guarded on resume). Both the
    ## population `fingerprint` (geometry) and the loss-config `eval_fingerprint`
    ## are stored so an "auto" resume can require both to match.
    .atomic_saveRDS(
      list(
        pop = pop,
        fingerprint = fingerprint,
        eval_fingerprint = eval_fp,
        gens_done = gens_done,
        bestval_history = best_hist,
        best_mem = best_mem,
        best_val = best_val,
        par_names = par_names,
        saved_at = Sys.time()
      ),
      ckpt_path
    )
    ## strategy #2: fold this block's evaluations (written to the trial-trace CSVs
    ## by the FORK children) into the parent cache so next block's initialpop
    ## re-evaluations are cache hits.
    .augment_eval_cache(eval_cache, out_dir, par_names, eval_fp)
    message(glue::glue(
      "calibrate_dynamic_fire: checkpoint at generation {gens_done}/{itermax_total} ",
      "(best_val={signif(best_val, 4)}, cache_size={length(ls(eval_cache))})"
    ))
    if (.deoptim_converged(best_hist, reltol, steptol)) {
      message(glue::glue(
        "calibrate_dynamic_fire: early stop at generation {gens_done} ",
        "(no > {reltol} relative improvement over {steptol} generations)"
      ))
      break
    }
  }

  ## Synthesize a result when zero blocks ran (resumed a checkpoint already at
  ## itermax), then overwrite the run-spanning fields so the caller's trace/return
  ## code sees the whole (possibly resumed) run rather than just the last block.
  if (is.null(res)) {
    res <- structure(list(optim = list(), member = list()), class = "DEoptim")
  }
  res$member$pop <- pop
  res$member$bestvalit <- best_hist
  res$optim$bestmem <- best_mem
  res$optim$bestval <- best_val

  ## Clean completion: drop the checkpoint so a later deliberate re-run (same
  ## config fingerprint) starts fresh instead of silently resuming a finished run.
  tryCatch(fs::file_delete(ckpt_path), error = function(e) invisible(NULL))

  res
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

## Fail loudly on any replicate a parallel map did not return properly.
##
## `parallel::mclapply()` reports a failed replicate in one of TWO ways, and testing for only
## one of them is how a failure gets past its caller. A replicate whose R code raises comes back
## as a `try-error`. A replicate whose CHILD PROCESS DIES -- OOM-killed, segfault, a container
## fault taking the worker with it -- comes back as NULL, accompanied by nothing but a
## "parallel jobs did not deliver results" warning, which is easy to lose in a long log.
##
## Measured: of one killed child and one erroring child, a `try-error`-only test caught one.
## The surviving NULL reached `loss_from_stats()` and failed deep in the component arithmetic
## with "missing value where TRUE/FALSE needed", naming neither the replicate nor the cause.
## Kept separate from `run_calibration_validation()` so both branches can be tested without
## Docker, since that function starts a container pool before it maps anything.
.stop_on_failed_reps <- function(reps, n_reps, fn = "run_calibration_validation") {
  errored <- vapply(reps, inherits, logical(1), "try-error")
  died <- vapply(reps, is.null, logical(1))
  if (!any(errored) && !any(died)) {
    return(invisible(NULL))
  }
  parts <- character(0)
  if (any(errored)) {
    first <- which(errored)[1L]
    parts <- c(
      parts,
      sprintf(
        "%d errored (first error, replicate %d: %s)",
        sum(errored),
        first,
        conditionMessage(attr(reps[[first]], "condition"))
      )
    )
  }
  if (any(died)) {
    parts <- c(
      parts,
      sprintf(
        paste0(
          "%d returned no result at all (replicate(s) %s): the forked child process died ",
          "rather than raising, so look for an OOM kill, a segfault or a container fault ",
          "rather than an R error"
        ),
        sum(died),
        paste(which(died), collapse = ", ")
      )
    )
  }
  stop(
    sprintf(
      "%s: %d of %d validation replicate(s) failed; %s",
      fn,
      sum(errored | died),
      n_reps,
      paste(parts, collapse = "; ")
    ),
    call. = FALSE
  )
}


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

  ## Take the pool settings from `cfg`, exactly as the calibration pool does. Validation runs the
  ## SAME scenario at the SAME landscape size as the search that produced `best_params`, so a
  ## grant that differs from the calibration's is wrong by construction: a hardcoded "8g" here
  ## OOM'd all 20 replicates in `ForC.SiteVars.Initialize` on a landscape whose calibration had
  ## just completed 30 generations on 13 GiB. `image` matters for the same reason -- it was
  ## previously resolved from a global option that a long-lived crew worker can hold stale.
  pool <- landis_pool_start(
    n = as.integer(n_reps),
    image = cfg$image,
    scratch_root = scratch_root,
    cpu_limit = cfg$cpu_limit %||% 2, ## LANDIS-II is single-threaded (~1 core/container)
    mem_limit = .cfg_mem_limit(cfg),
    name_prefix = "landis-validate"
  )
  on.exit(landis_pool_stop(pool), add = TRUE)

  template_dir <- fs::path_real(dirname(scenario_template))
  paths <- list(scenario_template = template_dir, scratch_root = pool$scratch_root)

  ## Run the replicates in PARALLEL across the warm pool. `landis_pool_start()`
  ## above allocates one container per replicate, so map each replicate onto its
  ## own container concurrently rather than serially -- a plain `lapply()` left
  ## all but one container idle and made validation O(n_reps) slower than the
  ## pool was sized for. `sim_landis()` is FORK-safe (it carries only file paths,
  ## no terra/sf handles; see its `@details`) and each replicate uses a distinct
  ## `pool_idx`, so FORK children never share a container. `mc.preschedule =
  ## FALSE` forks one child per replicate to match the one-container-per-replicate
  ## pool.
  n_reps_i <- as.integer(n_reps)
  sim_years_i <- as.integer(cfg$sim_years %||% 10L)
  run_rep <- function(i) {
    sim_landis(
      par_vec = best_params,
      paths = paths,
      sim_years = sim_years_i,
      base_seed = as.integer(base_seed) + i,
      pool = pool,
      pool_idx = i,
      method = "docker",
      ## Same rationale as the search: one transient container fault should not
      ## discard the whole validation. `cfg$retries` is what the calibration used.
      retries = as.integer(cfg$retries %||% 0L)
    )
  }
  reps <- if (n_reps_i > 1L) {
    parallel::mclapply(seq_len(n_reps_i), run_rep, mc.cores = n_reps_i, mc.preschedule = FALSE)
  } else {
    lapply(seq_len(n_reps_i), run_rep)
  }
  .stop_on_failed_reps(reps, n_reps_i)

  observed <- readRDS(observed_targets_path)
  .check_observed_pixel_area(observed, scenario_template)
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
