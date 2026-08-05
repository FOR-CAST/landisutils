## Vegetation dynamics: species biomass and transition plots ------------------------------------------
##
## Two data sources are supported; both produce identical output suitable for all
## downstream functions (leading_species, community_label, transition_data, plot_transitions):
##
##   read_biomass_output_rasters() -- general-purpose; reads Output.Biomass-v4 rasters
##       (one .tif per species per timestep).  Works with any LANDIS-II succession
##       extension.  Requires Output.Biomass to be configured in the scenario; note that
##       LANDIS-II can be slow when writing many raster outputs.
##
##   read_biomass_c_snapshots() -- ForCS-specific fast path; reads log_BiomassC.csv
##       (per-cohort, per-cell, every timestep, already written by ForCS with no extra
##       config needed).  Uses awk pre-filtering to keep memory use manageable even for
##       multi-GB files.  Not applicable to non-ForCS succession extensions.
##
## Two classification schemes are provided:
##   - leading_species()  -- cell label = species with highest total biomass
##   - community_label()  -- cell label = top-n species joined with "-" (e.g. "Hw-Sx")
##
## Transition plots use ggalluvial (Sankey/alluvial diagrams) to show how the fraction
## of cells in each type changes between user-specified snapshot years.

utils::globalVariables(c(
  "Wood", "Leaf", "CrsRoot", "FineRoot", "Age",
  "ecoregion", "row", "column", "species", "Time", "replicate", "scenario",
  "biomass", "total_biomass", "label", "n", "alluvium",
  "stratum", "x", "y"
))

## ---- data reading -------------------------------------------------------------------------------

#' Read ForCS log_BiomassC snapshots
#'
#' **ForCS-specific fast path.** Reads one or more `log_BiomassC.csv` files
#' (one per replicate) and returns per-cell, per-species total biomass at the
#' requested snapshot years.
#' Uses `awk` to pre-filter the files before parsing, keeping memory usage
#' manageable for large (multi-GB) CSV files.
#'
#' This function avoids the LANDIS-II raster-output overhead because ForCS writes
#' `log_BiomassC.csv` unconditionally; no additional output extension is required.
#' For scenarios using a succession extension other than ForCS, use
#' [read_biomass_output_rasters()] instead.
#'
#' Biomass columns (`Wood`, `Leaf`, `CrsRoot`, `FineRoot`) are summed over all
#' age cohorts of each species per cell, then converted from g C m^-2 to Mg C ha^-1
#' (multiply by 0.01).
#'
#' @param paths Character vector of paths to `log_BiomassC.csv` files (one per replicate).
#'   Each file must contain the ForCS per-cohort columns:
#'   `Time, row, column, ecoregion, species, Age, Wood, Leaf, CrsRoot, FineRoot`.
#' @param times Integer vector of snapshot years to extract.
#' @param run_name Character. Scenario directory label attached as a `scenario` column.
#'   If `NULL` (default), `scenario` is set to `basename(dirname(dirname(paths[1])))`.
#' @param cell_mask `data.frame` or `NULL`. If provided, must have integer columns `row`
#'   and `column` identifying the cells to **retain** (all others are dropped).
#'   Use this to restrict results to the core study area when the simulation was
#'   run over a larger buffered extent.  The `row`/`column` values must correspond
#'   to 1-indexed raster coordinates within the **buffered** simulation grid, not a
#'   separately-indexed core grid (the two grids index the same physical cells
#'   differently).  Derive the mask via spatial intersection of the buffered Initial Communities
#'   raster with the core study area boundary, then extract raster row/column.
#'
#' @returns A `data.table` with columns
#'   `scenario, replicate, Time, row, column, ecoregion, species, biomass`
#'   where `biomass` is total live biomass in Mg C ha^-1.
#'   This is the canonical format accepted by all downstream functions
#'   ([leading_species()], [community_label()], [biomass_landscape_summary()]).
#'
#' @seealso [read_biomass_output_rasters()] for the succession-agnostic alternative.
#' @family Vegetation transition helpers
#'
#' @export
read_biomass_c_snapshots <- function(paths, times, run_name = NULL, cell_mask = NULL) {
  .need("arrow", "Reading biomass-community snapshots")
  stopifnot(is.character(paths), length(paths) >= 1L, is.numeric(times), length(times) >= 1L)
  if (!is.null(cell_mask)) {
    stopifnot(is.data.frame(cell_mask), all(c("row", "column") %in% names(cell_mask)))
    cell_mask <- data.table::as.data.table(cell_mask)[, .(row, column)]
  }

  times <- as.integer(times)

  cols <- c("Time", "row", "column", "ecoregion", "species", "Wood", "Leaf", "CrsRoot", "FineRoot")

  purrr::map(paths, function(path) {
    replicate_dir <- basename(dirname(path))
    scenario_label <- run_name %||% basename(dirname(dirname(path)))

    dt <- .fread_biomass_c_times(path, times, cols)

    ## restrict to core study area cells if a mask was provided
    if (!is.null(cell_mask)) {
      dt <- dt[cell_mask, on = .(row, column), nomatch = NULL]
    }

    ## sum over cohort age classes; convert g C/m?^2 -> Mg C/ha (x 0.01)
    dt <- dt[,
      .(biomass = sum(Wood + Leaf + CrsRoot + FineRoot) * 0.01),
      by = .(Time, row, column, ecoregion, species)
    ]

    dt[, `:=`(scenario = scenario_label, replicate = replicate_dir)]
    data.table::setcolorder(
      dt,
      c("scenario", "replicate", "Time", "row", "column", "ecoregion", "species", "biomass")
    )
    dt
  }) |>
    data.table::rbindlist()
}

## Read log_BiomassC.csv rows matching `times`, selecting only `cols`.
## arrow::open_dataset() creates a lazy Scanner that reads the file in chunks and
## applies the filter at the Arrow compute level; only matching rows are materialised
## in R, keeping memory use bounded regardless of file size or OS.
##
## ForCS writes headers with a space after each comma (e.g. "Time, row, column").
## Arrow does not strip these, so column names arrive as " row", " column", etc.
## dplyr::rename_with(trimws) normalises them before filtering/selecting.
.fread_biomass_c_times <- function(path, times, cols) {
  arrow::open_dataset(path, format = "csv") |>
    dplyr::rename_with(trimws) |>
    dplyr::filter(.data$Time %in% times) |>
    dplyr::select(dplyr::all_of(cols)) |>
    dplyr::collect() |>
    data.table::as.data.table()
}

#' Open + collect a per-scenario biomass_snapshots Arrow dataset
#'
#' Convenience wrapper that opens `<scenario_dir>/<subdir>` as an Arrow dataset
#' partitioned by `replicate`, collects the full contents into a tibble, and
#' returns `NULL` for missing directories or empty datasets so callers can
#' short-circuit gracefully.
#'
#' Isolated from the rest of the [`read_biomass_c_snapshots()`] pipeline so its
#' body stays stable across project-side refactors: `{targets}` does not track
#' installed-package function bodies, so consuming targets are not invalidated
#' by cosmetic changes to how the collect happens under the hood.  Callers that
#' want that invalidation guarantee should call this function from their target
#' commands rather than inlining the `arrow::open_dataset() |> collect()` chain.
#'
#' The default `subdir = "_aggregates/biomass_snapshots"` matches the layout
#' produced by [write_biomass_c_snapshots_parquet()]-style writers in the
#' FOR-CAST post-processing pipeline; override for other layouts.
#'
#' @param scenario_dir Path to a scenario's root directory (e.g. `"LANDIS-II/ForCS_only"`).
#' @param subdir Path within `scenario_dir` to the parquet dataset root
#'   (default `"_aggregates/biomass_snapshots"`).
#'
#' @return A tibble with columns
#'   `scenario, replicate, Time, row, column, ecoregion, species, biomass`
#'   (matching [`read_biomass_c_snapshots()`]'s output), or `NULL`.
#'
#' @family Vegetation transition helpers
#'
#' @export
read_biomass_c_snapshots_for_scenario <- function(
  scenario_dir,
  subdir = "_aggregates/biomass_snapshots"
) {
  .need("arrow", "Reading a scenario's biomass-community dataset")
  scen_root <- file.path(scenario_dir, subdir)
  if (!dir.exists(scen_root)) {
    return(NULL)
  }
  df <- arrow::open_dataset(scen_root, partitioning = "replicate") |>
    dplyr::collect() |>
    tibble::as_tibble()
  if (nrow(df) == 0L) {
    return(NULL)
  }
  df
}

#' Write one replicate's biomass_snapshots to a partitioned parquet
#'
#' Reads a single replicate's `log_BiomassC.csv` via [read_biomass_c_snapshots()]
#' (Arrow-filtered to `times`, masked to `cell_mask`, cohort ages summed) and
#' writes it to
#' `<scenario_dir>/<subdir>/replicate=<rep>/part-0.parquet`, where
#' `scenario_dir` is inferred as `dirname(dirname(src_path))` and `<rep>` is the
#' replicate directory. This is the writer counterpart to
#' [read_biomass_c_snapshots_for_scenario()]; colocating the parquet inside the
#' scenario directory means it is archived alongside the raw replicate outputs
#' and adding a replicate is a single write with no downstream ripple.
#'
#' The publish is atomic: the parquet is written to a temporary file and then
#' [fs::file_move()]d into place, so a concurrent reader or a retried write never
#' observes a partial file -- safe for many replicate writers running at once
#' against an NFS output directory. When `staging_dir` is supplied the temporary
#' is written there (e.g. per-host scratch, keeping the interim bytes off NFS)
#' and moved cross-filesystem; the default stages in the destination directory so
#' the move is a same-filesystem atomic rename.
#'
#' @param src_path Path to one replicate's `log_BiomassC.csv`.
#' @param times Integer vector of snapshot years (see [read_biomass_c_snapshots()]).
#' @param cell_mask Optional `data.frame` with `row`/`column` columns identifying
#'   the core-area cells to retain (see [read_biomass_c_snapshots()]).
#' @param subdir Path within the scenario directory for the dataset root (default
#'   `"_aggregates/biomass_snapshots"`, matching
#'   [read_biomass_c_snapshots_for_scenario()]).
#' @param staging_dir Optional directory for the temporary parquet before it is
#'   moved into place; `NULL` (default) stages in the destination directory
#'   (same-filesystem atomic rename).
#'
#' @return The written parquet path.
#'
#' @family Vegetation transition helpers
#'
#' @export
write_biomass_c_snapshots_parquet <- function(
  src_path,
  times,
  cell_mask = NULL,
  subdir = "_aggregates/biomass_snapshots",
  staging_dir = NULL
) {
  .need("arrow", "Writing a parquet snapshot")
  df <- read_biomass_c_snapshots(
    paths = src_path,
    times = times,
    run_name = NULL,
    cell_mask = cell_mask
  )
  if (nrow(df) == 0L) {
    stop("write_biomass_c_snapshots_parquet(): empty input for ", src_path, call. = FALSE)
  }
  rep <- unique(df$replicate)
  stopifnot(length(unique(df$scenario)) == 1L, length(rep) == 1L)
  dst_dir <- file.path(dirname(dirname(src_path)), subdir, paste0("replicate=", rep))
  fs::dir_create(dst_dir)
  dst <- file.path(dst_dir, "part-0.parquet")
  tmp_root <- staging_dir %||% dst_dir
  fs::dir_create(tmp_root)
  tmp <- tempfile("part-", tmpdir = tmp_root, fileext = ".parquet")
  arrow::write_parquet(df, tmp)
  fs::file_move(tmp, dst)
  dst
}

#' Read Output.Biomass raster snapshots
#'
#' **General-purpose reader.** Reads per-species biomass rasters written by the
#' LANDIS-II Output Biomass v4 extension (`Output.Biomass-v4`) and returns
#' per-cell, per-species total live biomass at the requested snapshot years.
#'
#' This reader works with any LANDIS-II succession extension (Biomass Succession,
#' ForCS, NECN, etc.) as long as Output.Biomass is included in the scenario.
#' Be aware that writing many raster outputs can slow down the simulation;
#' for ForCS scenarios, [read_biomass_c_snapshots()] avoids this overhead by
#' reading the per-cohort carbon log that ForCS writes unconditionally.
#'
#' Raster values are assumed to be in g C m^-2 and are converted to Mg C ha^-1
#' (multiply by 0.01).  This is correct for ForCS; for Biomass Succession the
#' rasters store g dry biomass m^-2, so apply an appropriate carbon fraction
#' before downstream use.
#'
#' @param dirs Character vector of replicate root directories (one per replicate),
#'   each containing output rasters at the path given by `live_map_pattern`.
#' @param times Integer vector of snapshot years to extract.
#' @param species Character vector of species names to include.
#' @param live_map_pattern Character.  File naming pattern for live-biomass rasters,
#'   using `{species}` and `{timestep}` as literal placeholders (same notation as
#'   the `MapNames` directive in the Output Biomass extension config file).
#'   Default: `"outputs/biomass/biomass-{species}-{timestep}.tif"`.
#' @param run_name Character.  Scenario label attached as the `scenario` column.
#'   If `NULL` (default), derived from `basename(dirname(dirs[1]))`.
#'
#' @returns A `data.table` with columns
#'   `scenario, replicate, Time, row, column, species, biomass`
#'   where `biomass` is total live biomass in Mg C ha^-1.
#'   The `ecoregion` column present in [read_biomass_c_snapshots()] output is
#'   absent here (rasters do not carry ecoregion attribution per cell), but all
#'   downstream functions treat `ecoregion` as optional.
#'   This is the canonical format accepted by all downstream functions
#'   ([leading_species()], [community_label()], [biomass_landscape_summary()]).
#'
#' @seealso [read_biomass_c_snapshots()] for the ForCS-specific fast path.
#' @family Vegetation transition helpers
#'
#' @export
read_biomass_output_rasters <- function(
  dirs,
  times,
  species,
  live_map_pattern = "outputs/biomass/biomass-{species}-{timestep}.tif",
  run_name = NULL
) {
  stopifnot(
    is.character(dirs),
    length(dirs) >= 1L,
    is.numeric(times),
    length(times) >= 1L,
    is.character(species),
    length(species) >= 1L
  )

  times <- as.integer(times)

  purrr::map(dirs, function(dir) {
    replicate_dir <- basename(dir)
    scenario_label <- run_name %||% basename(dirname(dir))

    ## one data.table per (species, time) combination; skip missing rasters
    tiles <- purrr::map(times, function(t) {
      purrr::map(species, function(spp) {
        path <- file.path(
          dir,
          gsub(
            "{timestep}",
            t,
            gsub("{species}", spp, live_map_pattern, fixed = TRUE),
            fixed = TRUE
          )
        )
        if (!file.exists(path)) {
          return(NULL)
        }
        ## read_landis_raster(), NOT terra::rast(): LANDIS-II writes these maps south-up, so terra
        ## would reverse their rows and the `row` reported below would be mirrored.
        r <- read_landis_raster(path)
        df <- terra::as.data.frame(r, cells = TRUE, na.rm = TRUE)
        if (nrow(df) == 0L) {
          return(NULL)
        }
        ## terra::as.data.frame returns (cell, <layer_name>); rename value column
        names(df)[2L] <- "raw"
        data.table::data.table(
          scenario = scenario_label,
          replicate = replicate_dir,
          Time = t,
          row = terra::rowFromCell(r, df$cell),
          column = terra::colFromCell(r, df$cell),
          species = spp,
          ## g C/m?^2 -> Mg C/ha; see note in ?read_biomass_output_rasters re: Biomass Succession
          biomass = df$raw * 0.01
        )
      })
    })

    data.table::rbindlist(unlist(tiles, recursive = FALSE), fill = TRUE)
  }) |>
    data.table::rbindlist(fill = TRUE)
}

## ---- landscape-level summary --------------------------------------------------------------------

#' Landscape biomass summary by species
#'
#' Aggregates per-cell snapshot data to landscape-mean biomass density
#' (Mg C ha^-1) per species per timestep per replicate, then computes
#' mean ?+/- SD across replicates.
#'
#' @param df `data.table` or `data.frame` as returned by [read_biomass_c_snapshots()].
#'
#' @returns A `tibble` with columns
#'   `Time, species, mean_biomass, sd_biomass`.
#'
#' @family Vegetation transition helpers
#'
#' @export
biomass_landscape_summary <- function(df) {
  dt <- data.table::as.data.table(df)

  ## mean per cell per species per replicate
  rep_summary <- dt[, .(biomass = mean(biomass)), by = .(replicate, Time, species)]

  ## mean ?+/- SD across replicates
  rep_summary[,
    .(mean_biomass = mean(biomass), sd_biomass = if (.N > 1L) stats::sd(biomass) else 0),
    by = .(Time, species)
  ] |>
    tibble::as_tibble()
}

## ---- per-cell type classification ---------------------------------------------------------------

#' Leading species per cell at each snapshot
#'
#' For each (replicate, Time, row, column), identifies the species with the
#' highest total biomass and labels that cell accordingly.
#' Ties are broken by alphabetical species name.
#' Cells where total biomass across all species is zero are labelled
#' `"Non-vegetated"` (rather than getting an arbitrary alphabetically-first
#' species via the tiebreaker), matching [community_label()]'s behaviour.
#'
#' @param df `data.table` or `data.frame` as returned by [read_biomass_c_snapshots()].
#'
#' @returns A `tibble` with columns
#'   `scenario, replicate, Time, row, column, label`
#'   where `label` is the leading species name (or `"Non-vegetated"`).
#'
#' @seealso [community_label()], [plot_transitions()]
#' @family Vegetation transition helpers
#'
#' @export
leading_species <- function(df) {
  dt <- data.table::as.data.table(df)
  ## Compute per-cell totals so "Non-vegetated" (total <= 0) can be assigned
  ## vectorised rather than via a per-group R eval.
  dt[, total_biomass := sum(biomass), by = .(scenario, replicate, Time, row, column)]
  data.table::setorder(dt, scenario, replicate, Time, row, column, -biomass, species)
  ## Sorted + unique(by=) is a C-level first-per-group selection with no R eval
  ## per group; scales linearly and drops runtime by ~100x on multi-million-row
  ## inputs compared to the previous per-group `{}` idiom.
  result <- unique(dt, by = c("scenario", "replicate", "Time", "row", "column"))
  result[, label := data.table::fifelse(total_biomass <= 0, "Non-vegetated", species)]
  tibble::as_tibble(result[, .(scenario, replicate, Time, row, column, label)])
}

#' Community label per cell at each snapshot
#'
#' Labels each cell with a community type defined by its top-`n_spp` species
#' by biomass, joined with `"-"` (e.g. `"Hw-Sx"` for a western hemlock --
#' spruce co-dominant cell).  Cells where all species biomass is zero are
#' labelled `"Non-vegetated"`.
#'
#' @param df `data.table` or `data.frame` as returned by [read_biomass_c_snapshots()].
#' @param n_spp Integer. Number of top species to include in the label (default `2`).
#' @param min_pct Numeric in (0, 1). Minimum fraction of total cell biomass that the
#'   top species must account for to be included; remaining species in the top-`n_spp`
#'   set that fall below this threshold are dropped from the label (default `0.1`,
#'   i.e. a species contributing < 10% of cell total is omitted).
#'
#' @returns A `tibble` with columns
#'   `scenario, replicate, Time, row, column, label`.
#'
#' @seealso [leading_species()], [plot_transitions()]
#' @family Vegetation transition helpers
#'
#' @export
community_label <- function(df, n_spp = 2L, min_pct = 0.1) {
  stopifnot(is.numeric(n_spp), n_spp >= 1L, is.numeric(min_pct), min_pct >= 0, min_pct < 1)
  n_spp <- as.integer(n_spp)

  dt <- data.table::as.data.table(df)
  ## Vectorised pipeline (~1200x faster than the previous per-group `{}` idiom
  ## on the ~8M-row biomass_snapshots inputs produced by the FOR-CAST pipeline):
  ##   1. per-cell totals via `sum(biomass), by = ...`  (C-level aggregation)
  ##   2. sort desc-biomass within each cell (setorder)
  ##   3. per-cell rank via `data.table::rowid()` (C-level per-group counter)
  ##   4. filter to top-`n_spp` species AND `biomass / total >= min_pct`
  ##   5. pivot species to wide by rank via `dcast`
  ##   6. build label as vectorised `paste()` of the rank columns with "-",
  ##      collapsing NA slots and empty-species gaps.
  dt[, total_biomass := sum(biomass), by = .(scenario, replicate, Time, row, column)]
  data.table::setorder(dt, scenario, replicate, Time, row, column, -biomass, species)
  dt[, rank := data.table::rowid(scenario, replicate, Time, row, column)]

  keep <- dt[rank <= n_spp]
  keep <- keep[total_biomass <= 0 | biomass / total_biomass >= min_pct]
  keep[, rank := as.character(rank)]

  wide <- data.table::dcast(
    keep,
    scenario + replicate + Time + row + column + total_biomass ~ rank,
    value.var = "species"
  )

  ## Vectorised label construction. Some rank columns may be absent if no cell
  ## had that many surviving species (e.g. `n_spp = 3` but every cell keeps at
  ## most 2); intersect() handles that. NA species per column become "" so
  ## `paste(..., sep = "-")` produces things like "Hw--" which the follow-up
  ## `gsub` calls trim back to "Hw".
  rank_cols <- intersect(as.character(seq_len(n_spp)), names(wide))
  sp_cols <- lapply(rank_cols, function(nm) {
    data.table::fifelse(is.na(wide[[nm]]), "", wide[[nm]])
  })
  raw_label <- do.call(paste, c(sp_cols, sep = "-"))
  raw_label <- gsub("^-+|-+$", "", raw_label)
  raw_label <- gsub("-+", "-", raw_label)

  wide[, label := data.table::fifelse(total_biomass <= 0, "Non-vegetated", raw_label)]
  tibble::as_tibble(wide[, .(scenario, replicate, Time, row, column, label)])
}

## ---- transition data frame ----------------------------------------------------------------------

#' Build a transition data frame for alluvial plots
#'
#' Takes per-cell type labels (from [leading_species()] or [community_label()]),
#' pivots to wide form, counts unique label-path combinations across all snapshots,
#' and converts to the lodes (long) form required by [ggalluvial::geom_alluvium()].
#'
#' Counts are averaged across replicates so that the diagram represents the mean
#' landscape behaviour.
#'
#' @param label_df `tibble` or `data.frame` with columns
#'   `scenario, replicate, Time, row, column, label` -- as returned by
#'   [leading_species()] or [community_label()].
#' @param times Integer vector of snapshot years to include, in order.
#'   All values must be present in `label_df$Time`.
#'
#' @returns A `tibble` in lodes form with columns
#'   `alluvium, x, stratum, y`
#'   compatible with [ggalluvial::geom_alluvium()]:
#'   * `alluvium` -- integer ID for the flow path
#'   * `x` -- year (from `times`)
#'   * `stratum` -- vegetation type label at that year
#'   * `y` -- mean number of cells (averaged across replicates) following this path
#'
#' @family Vegetation transition helpers
#'
#' @export
transition_data <- function(label_df, times) {
  times <- as.integer(sort(times))
  stopifnot(all(times %in% label_df$Time))

  label_df <- dplyr::filter(label_df, .data$Time %in% times)

  ## one row per replicate per cell, one column per snapshot year
  wide <- tidyr::pivot_wider(
    label_df,
    id_cols = c("scenario", "replicate", "row", "column"),
    names_from = "Time",
    values_from = "label",
    names_prefix = "t_"
  )

  time_cols <- paste0("t_", times)

  ## count cells per unique label-path combination within each replicate
  path_counts <- wide |>
    dplyr::count(dplyr::across(dplyr::all_of(c("replicate", time_cols))), name = "n")

  ## average across replicates
  mean_counts <- path_counts |>
    dplyr::group_by(dplyr::across(dplyr::all_of(time_cols))) |>
    dplyr::summarise(y = mean(n), .groups = "drop") |>
    dplyr::mutate(alluvium = dplyr::row_number())

  ## convert to lodes form
  tidyr::pivot_longer(
    mean_counts,
    cols = dplyr::all_of(time_cols),
    names_to = "x",
    values_to = "stratum"
  ) |>
    dplyr::mutate(x = as.integer(sub("t_", "", .data$x))) |>
    dplyr::select("alluvium", "x", "stratum", "y")
}

## ---- plots --------------------------------------------------------------------------------------

#' Plot species biomass over time (stacked area)
#'
#' Creates a stacked area chart of landscape-mean biomass density by species
#' over time.
#'
#' @param summary_df `tibble` as returned by [biomass_landscape_summary()].
#' @param colours Named character vector of hex colours, one per species.
#'   If `NULL` (default), a built-in qualitative palette is used.
#' @param title Character. Plot title (default: `NULL` -> no title).
#'
#' @returns A `ggplot` object.
#'
#' @seealso [read_biomass_c_snapshots()], [biomass_landscape_summary()]
#' @family Vegetation transition helpers
#'
#' @export
plot_species_biomass <- function(summary_df, colours = NULL, title = NULL) {
  .need("ggplot2", "Plotting species biomass")
  species_levels <- summary_df |>
    dplyr::group_by(.data$species) |>
    dplyr::summarise(total = sum(.data$mean_biomass), .groups = "drop") |>
    dplyr::arrange(-.data$total) |>
    dplyr::pull("species")

  df <- dplyr::mutate(summary_df, species = factor(.data$species, levels = species_levels))

  fill_scale <- if (!is.null(colours)) {
    ggplot2::scale_fill_manual(values = colours)
  } else {
    ggplot2::scale_fill_brewer(palette = "Set2")
  }

  ggplot2::ggplot(df, ggplot2::aes(x = .data$Time, y = .data$mean_biomass, fill = .data$species)) +
    ggplot2::geom_area(colour = "white", linewidth = 0.3) +
    fill_scale +
    ggplot2::labs(
      x = "Year",
      y = expression(
        "Biomass (Mg C ha"^{
          -1
        } *
          ")"
      ),
      fill = "Species",
      title = title
    ) +
    ggplot2::theme_bw()
}

#' Plot vegetation transitions (Sankey / alluvial diagram)
#'
#' Renders a Sankey-style alluvial diagram showing how cells move between
#' vegetation types (leading species or community labels) across user-specified
#' snapshot years.
#'
#' The width of each band is proportional to the mean number of cells that
#' follow that transition path (averaged across replicates).
#'
#' @param lodes_df `tibble` in lodes form as returned by [transition_data()].
#' @param colours Named character vector mapping label names to hex colours.
#'   If `NULL` (default), a built-in qualitative palette is used.
#' @param title Character. Plot title (default: `NULL` -> no title).
#'
#' @returns A `ggplot` object.
#'
#' @seealso [transition_data()], [leading_species()], [community_label()]
#' @family Vegetation transition helpers
#'
#' @export
plot_transitions <- function(lodes_df, colours = NULL, title = NULL) {
  .need("ggplot2", "Plotting transitions")
  labels <- sort(unique(lodes_df$stratum))

  fill_scale <- if (!is.null(colours)) {
    ggplot2::scale_fill_manual(values = colours, breaks = labels)
  } else {
    ggplot2::scale_fill_brewer(palette = "Set2", breaks = labels)
  }

  time_breaks <- sort(unique(lodes_df$x))

  ggplot2::ggplot(
    lodes_df,
    ggplot2::aes(
      x = .data$x,
      y = .data$y,
      stratum = .data$stratum,
      alluvium = .data$alluvium,
      fill = .data$stratum,
      label = .data$stratum
    )
  ) +
    ggalluvial::geom_alluvium(width = 1 / 10, alpha = 0.7, colour = NA) +
    ggalluvial::geom_stratum(width = 1 / 10, fill = "grey90", colour = "grey50") +
    ggplot2::geom_text(
      stat = ggalluvial::StatStratum,
      ggplot2::aes(label = ggplot2::after_stat(stratum)),
      size = 2.8,
      fontface = "italic"
    ) +
    fill_scale +
    ggplot2::scale_x_continuous(breaks = time_breaks, labels = time_breaks) +
    ggplot2::labs(x = "Year", y = "Cells (mean across replicates)", fill = "Type", title = title) +
    ggplot2::theme_bw() +
    ggplot2::theme(legend.position = "none")
}
