#' Build the calibration design table
#'
#' One row per landscape cell. `landis_species` is the (pseudo-)species name
#' written into `species.txt`; `species` is the real modelled species it stands
#' for.
#'
#' With `grid = NULL` this returns the Phase A verification design: one cell per
#' species at its current calibrated parameters, which reproduces the original
#' single-cell runs. Supply `grid` to expand a factorial.
#'
#' @param growth_params A data frame of the parameters currently in use, with
#'   columns `species`, `growth_shp`, `mort_shp`, `anpp_max` and `biomass_max`.
#'   Restrict it to the species being calibrated.
#' @param grid Optional tibble with `species` and any of `growth_shp`,
#'   `mort_shp`, `anpp_max`, `biomass_max` giving the candidate values to sweep.
#'   One row per candidate value per parameter per species.
#' @param cohort_age Integer. Age of the single planted cohort.
#' @param max_cells Integer. Guard against an accidentally enormous factorial.
#'
#' @return A tibble with one row per cell.
#' @family growth calibration helpers
#' @export
growth_calibration_design <- function(
  growth_params,
  grid = NULL,
  cohort_age = 1L,
  max_cells = 4000L
) {
  stopifnot(all(
    c("species", "growth_shp", "mort_shp", "anpp_max", "biomass_max") %in% names(growth_params)
  ))

  ## Candidate values per parameter: the sweep values where supplied, otherwise
  ## the current calibrated value.
  candidates <- function(param) {
    base <- dplyr::select(growth_params, species, value = dplyr::all_of(param))
    if (is.null(grid) || !param %in% names(grid)) {
      return(dplyr::filter(base, !is.na(.data$value)))
    }
    swept <- grid |>
      dplyr::select(species, value = dplyr::all_of(param)) |>
      dplyr::filter(!is.na(.data$value))
    ## species not swept for this parameter keep their calibrated value
    dplyr::bind_rows(swept, dplyr::anti_join(base, swept, by = "species")) |> dplyr::distinct()
  }

  ## Species-level dimension: growth and mortality shape.
  spp_dim <- dplyr::inner_join(
    dplyr::rename(candidates("growth_shp"), growth_shp = value),
    dplyr::rename(candidates("mort_shp"), mort_shp = value),
    by = "species",
    relationship = "many-to-many"
  ) |>
    dplyr::arrange(.data$species, .data$growth_shp, .data$mort_shp) |>
    dplyr::mutate(
      landis_species = growth_pseudo_species_name(.data$species, dplyr::row_number()),
      .by = "species"
    )

  ## Ecoregion-level dimension: max ANPP and max biomass. Ecoregions are shared
  ## across species, so the candidate sets are crossed per species and then
  ## indexed to a common ecoregion sequence.
  eco_dim <- dplyr::inner_join(
    dplyr::rename(candidates("anpp_max"), anpp_max = value),
    dplyr::rename(candidates("biomass_max"), biomass_max = value),
    by = "species",
    relationship = "many-to-many"
  ) |>
    dplyr::arrange(.data$species, .data$anpp_max, .data$biomass_max) |>
    dplyr::mutate(eco_index = dplyr::row_number(), .by = "species")

  ## LANDIS-II ecoregion NAMES are the integer `ecoregionGroup` values that
  ## `landisutils::prepEcoregionsFiles()` writes into ecoregions.txt, and every
  ## ForCS table plus climate.txt must use the same identifier. Keep the integer
  ## as `ecoregion` and carry a readable label separately for reporting.
  design <- dplyr::inner_join(spp_dim, eco_dim, by = "species", relationship = "many-to-many") |>
    dplyr::mutate(
      ecoregion = as.integer(.data$eco_index),
      ecoregion_label = sprintf("E%02d", .data$eco_index)
    ) |>
    dplyr::arrange(.data$species, .data$landis_species, .data$eco_index)

  n_spp <- dplyr::n_distinct(design$landis_species)
  n_eco <- dplyr::n_distinct(design$ecoregion)
  if (n_spp * n_eco > max_cells) {
    stop(
      sprintf(
        paste0(
          "growth_calibration_design(): %d pseudo-species x %d ecoregions = %d cells ",
          "exceeds max_cells = %d. Narrow the sweep in the design CSV, or raise max_cells ",
          "deliberately."
        ),
        n_spp,
        n_eco,
        n_spp * n_eco,
        max_cells
      ),
      call. = FALSE
    )
  }

  ## Lay pseudo-species down the rows, ecoregions across the columns, so every
  ## (pseudo-species, ecoregion) pair gets exactly one cell.
  spp_levels <- unique(design$landis_species)
  eco_levels <- sort(unique(design$ecoregion))
  eco_labels <- stats::setNames(
    design$ecoregion_label[match(eco_levels, design$ecoregion)],
    as.character(eco_levels)
  )

  tidyr::expand_grid(landis_species = spp_levels, ecoregion = eco_levels) |>
    dplyr::left_join(
      dplyr::distinct(design, landis_species, species, growth_shp, mort_shp),
      by = "landis_species"
    ) |>
    dplyr::left_join(
      dplyr::distinct(design, species, ecoregion, anpp_max, biomass_max),
      by = c("species", "ecoregion")
    ) |>
    dplyr::mutate(
      row = match(.data$landis_species, spp_levels),
      col = match(.data$ecoregion, eco_levels),
      map_code = dplyr::row_number(),
      cohort_age = as.integer(cohort_age),
      ecoregion_label = unname(eco_labels[as.character(.data$ecoregion)])
    ) |>
    dplyr::select(
      map_code,
      row,
      col,
      landis_species,
      species,
      ecoregion,
      ecoregion_label,
      cohort_age,
      growth_shp,
      mort_shp,
      anpp_max,
      biomass_max
    )
}

#' Expand a shape-and-ratio sweep into absolute ForCS parameters
#'
#' The sweep is specified as growth shape, mortality shape, and `anpp_prop` --
#' `anpp_max` as a percentage of `biomass_max`, the same ratio that
#' PredictiveEcology/Biomass_speciesFactorial sweeps as `mANPPproportion`.
#'
#' `biomass_max` is NOT swept. It is pinned to each species' current calibrated
#' value and the level a candidate implies is recovered arithmetically instead;
#' see [growth_inflation_factor()]. Sweeping it alongside the shapes made the two
#' inseparable: many (shape, level) pairs produce near-identical curves, so the
#' score traded them off arbitrarily and settled wherever the reference cloud's
#' centre happened to sit.
#'
#' Pinning per species rather than at one global constant -- LandR uses 5000 for
#' every species -- keeps every simulation inside the range over which the
#' achieved-fraction invariance was actually checked here. `biomass_max` spans
#' 18200 to 48028 across these six species, so a single constant would put some
#' of them far outside it.
#'
#' @param grid A tibble with `species` and any of `growth_shp`, `mort_shp`,
#'   `anpp_prop`. One row per candidate value per parameter per species.
#' @param growth_params A data frame of the parameters currently in use; see
#'   [growth_calibration_design()].
#'
#' @return A grid in the absolute form [growth_calibration_design()] expects.
#' @family growth calibration helpers
#' @export
growth_factorial_ratio_grid <- function(grid, growth_params) {
  fixed <- dplyr::select(growth_params, species, biomass_max)

  props <- if (!is.null(grid) && "anpp_prop" %in% names(grid)) {
    grid |> dplyr::select(species, anpp_prop) |> dplyr::filter(!is.na(.data$anpp_prop))
  } else {
    growth_params[0L, c("species")] |> dplyr::mutate(anpp_prop = numeric(0))
  }

  ## Species with no swept ratio keep the ratio they currently use.
  current <- growth_params |>
    dplyr::transmute(species, anpp_prop = 100 * .data$anpp_max / .data$biomass_max)
  props <- dplyr::bind_rows(props, dplyr::anti_join(current, props, by = "species"))

  anpp <- props |>
    dplyr::left_join(fixed, by = "species") |>
    ## ForCS parses both as integers; a fractional value dies deep in its CSV
    ## parser rather than at the point of use.
    dplyr::transmute(
      species,
      anpp_max = round(.data$biomass_max * .data$anpp_prop / 100),
      biomass_max = round(.data$biomass_max)
    ) |>
    dplyr::distinct()

  shapes <- if (is.null(grid)) {
    NULL
  } else {
    dplyr::select(grid, dplyr::any_of(c("species", "growth_shp", "mort_shp")))
  }

  if (is.null(shapes)) {
    return(anpp)
  }
  dplyr::bind_rows(shapes, anpp)
}

#' Name a pseudo-species
#'
#' Distinct, stable, and LANDIS-II-safe (no spaces or punctuation). A single
#' combination per species collapses to the plain species code, so the Phase A
#' verification landscape uses the real species names.
#'
#' @param species Character vector of real species codes.
#' @param index Integer vector of combination indices within each species.
#'
#' @return Character vector of species names for `species.txt`.
#' @family growth calibration helpers
#' @export
growth_pseudo_species_name <- function(species, index) {
  ## NOT ifelse(): its result takes the length of the TEST, so a scalar test
  ## silently collapses the whole vector to one name.
  if (max(index) == 1L) {
    species
  } else {
    sprintf("%s_c%02d", species, index)
  }
}

## ---- calibration scenario writer ----------------------------------------------------------------

#' Expand a per-species parameter table over pseudo-species
#'
#' The ForCS tables are keyed on the species names in `species.txt`, which for a
#' factorial are pseudo-species. This maps each pseudo-species back to its real
#' species, joins the real species' values, and renames.
#'
#' @param df A tibble with a `species` column of REAL species codes.
#' @param design A design table from [growth_calibration_design()].
#'
#' @return `df` expanded to one row per pseudo-species, `species` replaced.
#' @family growth calibration helpers
#' @export
growth_expand_over_pseudo_species <- function(df, design) {
  dplyr::distinct(design, landis_species, species) |>
    dplyr::left_join(df, by = "species", relationship = "many-to-many") |>
    dplyr::select(-species) |>
    dplyr::rename(species = landis_species)
}

#' Partition a design into batches that fit in memory
#'
#' Runtime is near-linear in cells but memory is not: per-cell cost rises from
#' about 0.29 MB at 4k cells to 0.34 MB at 39k, and a full structure design runs
#' to hundreds of thousands of cells. Splitting it into batches keeps each run in
#' the linear regime, bounds peak memory, and lets the batches run concurrently
#' or be resumed after an interruption.
#'
#' Batches are cut on CELL boundaries, never within a cell, so a mixed cell's
#' cohorts always stay together.
#'
#' @param design A cohort table from [growth_structure_design()].
#' @param max_cells_per_batch Integer. Cell budget per batch. The default of
#'   12000 corresponds to roughly 4 GB peak, which fits a 16 GB laptop.
#'
#' @return `design` with `map_code` renumbered within each batch, plus `batch`,
#'   `row`, and `col`.
#' @family growth calibration helpers
#' @export
growth_calibration_partition <- function(design, max_cells_per_batch = 12000L) {
  stopifnot(max_cells_per_batch >= 1L)

  cells <- design |>
    dplyr::distinct(.data$map_code, .data$ecoregion) |>
    dplyr::arrange(.data$map_code) |>
    dplyr::mutate(batch = ((dplyr::row_number() - 1L) %/% as.integer(max_cells_per_batch)) + 1L)

  ## Within a batch, lay cells out with ecoregions across the columns so each
  ## cell sits in the column its ecoregion code names, matching the layout
  ## write_growth_calibration_inputs() expects.
  cells <- cells |>
    dplyr::mutate(
      col = as.integer(.data$ecoregion),
      row = dplyr::row_number(),
      .by = c("batch", "ecoregion")
    ) |>
    dplyr::mutate(batch_map_code = dplyr::row_number(), .by = "batch")

  design |>
    dplyr::select(-dplyr::any_of(c("row", "col", "batch"))) |>
    dplyr::inner_join(
      dplyr::select(cells, map_code, batch, row, col, batch_map_code),
      by = "map_code"
    ) |>
    dplyr::mutate(map_code = .data$batch_map_code, batch_map_code = NULL) |>
    dplyr::arrange(.data$batch, .data$map_code, .data$species, .data$cohort_age)
}

## ---- standalone review artifacts -----------------------------------------------------------------
##
## Written to a plain directory, so a parameter set can be reviewed and iterated
## on BEFORE anything is promoted into a project's hand-maintained parameter
## table.

#' Build a design crossing landscape cohort structures with a parameter sweep
#'
#' For each structure and each species in it, generates one cell per
#' (shape combination, ecoregion), with the chosen species represented by a
#' swept pseudo-species and every other species by a held-fixed pseudo-species
#' carrying its calibrated values in every ecoregion.
#'
#' @param structures A data frame of landscape cohort structures, with one row
#'   per cohort and a `structure_id` grouping the cohorts of a cell.
#' @param growth_params The parameters currently in use, restricted to the
#'   calibrated species; see [growth_calibration_design()].
#' @param grid Optional sweep grid, as for [growth_calibration_design()].
#' @param max_structures Integer or `NULL`. Keep only the N structures covering
#'   the most communities. `NULL` keeps all of them.
#' @param max_cells Integer. Refuse to build a design larger than this.
#'
#' @return A cohort table: one row per cohort, several per mixed cell.
#' @family growth calibration helpers
#' @export
growth_structure_design <- function(
  structures,
  growth_params,
  grid = NULL,
  max_structures = NULL,
  max_cells = 400000L
) {
  calibrated <- growth_params$species

  ## Structures are only usable if every species in them is calibrated.
  usable <- structures |>
    dplyr::summarise(ok = all(.data$species %in% calibrated), .by = "structure_id") |>
    dplyr::filter(.data$ok)
  structures <- dplyr::semi_join(structures, usable, by = "structure_id")

  if (!is.null(max_structures)) {
    keep <- structures |>
      dplyr::distinct(.data$structure_id, .data$n_communities) |>
      dplyr::slice_max(.data$n_communities, n = max_structures, with_ties = FALSE)
    structures <- dplyr::semi_join(structures, keep, by = "structure_id")
  }

  ## Reuse the single-cohort builder to enumerate the sweep: its one-cell-per
  ## (shape combo, ecoregion) layout is exactly the per-species sweep needed
  ## here, and reusing it keeps the two designs in step.
  sweep <- growth_calibration_design(growth_params, grid = grid, max_cells = max_cells) |>
    dplyr::select(
      species,
      swept_landis_species = "landis_species",
      ecoregion,
      ecoregion_label,
      growth_shp,
      mort_shp,
      anpp_max,
      biomass_max
    )

  ## Held-fixed partners: calibrated values, identical in every ecoregion.
  fixed <- growth_params |>
    dplyr::transmute(
      species,
      fixed_landis_species = paste0(.data$species, "_fix"),
      fx_growth_shp = .data$growth_shp,
      fx_mort_shp = .data$mort_shp,
      fx_anpp_max = .data$anpp_max,
      fx_biomass_max = .data$biomass_max
    )

  ## One design branch per (structure, swept species).
  branches <- structures |> dplyr::distinct(.data$structure_id, swept_species = .data$species)

  cohorts <- branches |>
    dplyr::inner_join(structures, by = "structure_id", relationship = "many-to-many") |>
    dplyr::inner_join(sweep, by = c("swept_species" = "species"), relationship = "many-to-many") |>
    dplyr::inner_join(fixed, by = "species") |>
    dplyr::mutate(
      is_swept = .data$species == .data$swept_species,
      landis_species = dplyr::if_else(
        .data$is_swept,
        .data$swept_landis_species,
        .data$fixed_landis_species
      ),
      growth_shp = dplyr::if_else(.data$is_swept, .data$growth_shp, .data$fx_growth_shp),
      mort_shp = dplyr::if_else(.data$is_swept, .data$mort_shp, .data$fx_mort_shp),
      anpp_max = dplyr::if_else(.data$is_swept, .data$anpp_max, .data$fx_anpp_max),
      biomass_max = dplyr::if_else(.data$is_swept, .data$biomass_max, .data$fx_biomass_max)
    )

  ## One cell per (structure, swept species, swept pseudo-species, ecoregion).
  cell_keys <- cohorts |>
    dplyr::distinct(
      .data$structure_id,
      .data$swept_species,
      .data$swept_landis_species,
      .data$ecoregion,
      .data$ecoregion_label
    ) |>
    dplyr::arrange(
      .data$structure_id,
      .data$swept_species,
      .data$swept_landis_species,
      .data$ecoregion
    ) |>
    dplyr::mutate(map_code = dplyr::row_number())

  n_cells <- nrow(cell_keys)
  if (n_cells > max_cells) {
    stop(
      sprintf(
        paste0(
          "growth_structure_design(): %d cells exceeds max_cells = %d. Narrow the sweep, ",
          "reduce max_structures, or raise max_cells deliberately."
        ),
        n_cells,
        max_cells
      ),
      call. = FALSE
    )
  }

  cohorts |>
    dplyr::inner_join(
      cell_keys,
      by = c(
        "structure_id",
        "swept_species",
        "swept_landis_species",
        "ecoregion",
        "ecoregion_label"
      )
    ) |>
    dplyr::select(
      map_code,
      structure_id,
      n_communities,
      swept_species,
      species,
      landis_species,
      is_swept,
      cohort_age,
      ecoregion,
      ecoregion_label,
      growth_shp,
      mort_shp,
      anpp_max,
      biomass_max
    ) |>
    dplyr::arrange(.data$map_code, .data$species, .data$cohort_age)
}

#' Extract distinct cohort structures from the initial communities
#'
#' Reduces the landscape's initial communities to the distinct **structures**
#' they contain -- a structure being the set of (species, age) cohorts in a
#' community, irrespective of which map codes carry it. On this landscape 444k
#' communities collapse to roughly 1.7k structures with one or two cohorts,
#' which is what makes a structure-aware calibration tractable at all.
#'
#' `n_communities` is carried so downstream work can weight or subset structures
#' by how much of the landscape they actually represent.
#'
#' @param ic A data frame of initial communities with `MapCode`, `SpeciesCode`,
#'   and `Age` (the `landis_ic_data_list` target).
#' @param max_cohorts Integer. Largest community size to retain.
#'
#' @return A tibble with one row per cohort: `structure_id`, `n_cohorts`,
#'   `species`, `cohort_age`, `n_communities`.
#' @family growth calibration helpers
#' @export
extract_landscape_cohort_structures <- function(ic, max_cohorts = 2L) {
  ic <- tibble::as_tibble(ic) |>
    dplyr::select(MapCode, SpeciesCode, Age) |>
    dplyr::mutate(Age = as.integer(.data$Age))

  keep <- ic |>
    dplyr::count(.data$MapCode, name = "n_cohorts") |>
    dplyr::filter(.data$n_cohorts <= max_cohorts)

  ## A structure key that is invariant to cohort order within a community.
  per_community <- ic |>
    dplyr::inner_join(keep, by = "MapCode") |>
    dplyr::arrange(.data$MapCode, .data$SpeciesCode, .data$Age) |>
    dplyr::summarise(
      structure_key = paste(paste0(.data$SpeciesCode, ":", .data$Age), collapse = "|"),
      n_cohorts = dplyr::first(.data$n_cohorts),
      .by = "MapCode"
    )

  weights <- per_community |>
    dplyr::count(.data$structure_key, .data$n_cohorts, name = "n_communities") |>
    dplyr::arrange(dplyr::desc(.data$n_communities), .data$structure_key) |>
    dplyr::mutate(structure_id = sprintf("S%05d", dplyr::row_number()))

  weights |>
    dplyr::mutate(cohort = strsplit(.data$structure_key, "|", fixed = TRUE)) |>
    tidyr::unnest("cohort") |>
    tidyr::separate_wider_delim("cohort", delim = ":", names = c("species", "cohort_age")) |>
    dplyr::transmute(
      structure_id,
      n_cohorts = as.integer(.data$n_cohorts),
      species = .data$species,
      cohort_age = as.integer(.data$cohort_age),
      n_communities = as.integer(.data$n_communities)
    ) |>
    dplyr::arrange(.data$structure_id, .data$species, .data$cohort_age)
}

#' Resolve a writable, Docker-bind-mountable run root for calibration runs
#'
#' Mirrors the fire-calibration scratch root, with one extra constraint: the value is
#' baked into the `{targets}` command at pipeline-definition time, so it must be
#' STABLE across sessions. `tempdir()` is therefore not usable as the fallback --
#' it would change every session and invalidate the run every time.
#'
#' Resolution order:
#' 1. `LANDIS_GROWTH_SCRATCH`, if set
#' 2. `/mnt/scratch/<user>/<project>/landis-growth-calibration`, when
#'    `/mnt/scratch/<user>` exists and is writable (the cluster convention)
#' 3. `~/.cache/landis-growth-calibration`
#'
#' The default of running in place is deliberately NOT used: in many deployments
#' `./LANDIS-II/` is commonly a root-squashed NFS symlink, which Docker cannot
#' bind-mount (the run dies with exit code 126). Staging on a local filesystem
#' and letting `tar_landis()` archive the finished replicate back to the tracked
#' scenario directory works on the workstation, the cluster, and a laptop alike.
#'
#' @return Character path.
#' @family growth calibration helpers
#' @export
growth_calibration_work_root <- function() {
  env <- Sys.getenv("LANDIS_GROWTH_SCRATCH", unset = "")
  if (nzchar(env)) {
    return(env)
  }

  user <- Sys.info()[["user"]]
  project <- basename(normalizePath(".", mustWork = FALSE))
  cluster <- file.path("/mnt/scratch", user)

  if (dir.exists(cluster) && file.access(cluster, mode = 2L) == 0L) {
    file.path(cluster, project, "landis-growth-calibration")
  } else {
    file.path(path.expand("~/.cache"), "landis-growth-calibration")
  }
}
