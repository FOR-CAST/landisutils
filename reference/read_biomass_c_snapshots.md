# Read ForCS log_BiomassC snapshots

**ForCS-specific fast path.** Reads one or more `log_BiomassC.csv` files
(one per replicate) and returns per-cell, per-species total biomass at
the requested snapshot years. Uses `awk` to pre-filter the files before
parsing, keeping memory usage manageable for large (multi-GB) CSV files.

## Usage

``` r
read_biomass_c_snapshots(paths, times, run_name = NULL, cell_mask = NULL)
```

## Arguments

- paths:

  Character vector of paths to `log_BiomassC.csv` files (one per
  replicate). Each file must contain the ForCS per-cohort columns:
  `Time, row, column, ecoregion, species, Age, Wood, Leaf, CrsRoot, FineRoot`.

- times:

  Integer vector of snapshot years to extract.

- run_name:

  Character. Scenario directory label attached as a `scenario` column.
  If `NULL` (default), `scenario` is set to
  `basename(dirname(dirname(paths[1])))`.

- cell_mask:

  `data.frame` or `NULL`. If provided, must have integer columns `row`
  and `column` identifying the cells to **retain** (all others are
  dropped). Use this to restrict results to the core study area when the
  simulation was run over a larger buffered extent. The `row`/`column`
  values must correspond to 1-indexed raster coordinates within the
  **buffered** simulation grid, not a separately-indexed core grid (the
  two grids index the same physical cells differently). Derive the mask
  via spatial intersection of the buffered Initial Communities raster
  with the core study area boundary, then extract raster row/column.

## Value

A `data.table` with columns
`scenario, replicate, Time, row, column, ecoregion, species, biomass`
where `biomass` is total live biomass in Mg C ha^-1. This is the
canonical format accepted by all downstream functions
([`leading_species()`](https://for-cast.github.io/landisutils/reference/leading_species.md),
[`community_label()`](https://for-cast.github.io/landisutils/reference/community_label.md),
[`biomass_landscape_summary()`](https://for-cast.github.io/landisutils/reference/biomass_landscape_summary.md)).

## Details

This function avoids the LANDIS-II raster-output overhead because ForCS
writes `log_BiomassC.csv` unconditionally; no additional output
extension is required. For scenarios using a succession extension other
than ForCS, use
[`read_biomass_output_rasters()`](https://for-cast.github.io/landisutils/reference/read_biomass_output_rasters.md)
instead.

Biomass columns (`Wood`, `Leaf`, `CrsRoot`, `FineRoot`) are summed over
all age cohorts of each species per cell, then converted from g C m^-2
to Mg C ha^-1 (multiply by 0.01).

## See also

[`read_biomass_output_rasters()`](https://for-cast.github.io/landisutils/reference/read_biomass_output_rasters.md)
for the succession-agnostic alternative.

Other Vegetation transition helpers:
[`biomass_landscape_summary()`](https://for-cast.github.io/landisutils/reference/biomass_landscape_summary.md),
[`community_label()`](https://for-cast.github.io/landisutils/reference/community_label.md),
[`leading_species()`](https://for-cast.github.io/landisutils/reference/leading_species.md),
[`plot_species_biomass()`](https://for-cast.github.io/landisutils/reference/plot_species_biomass.md),
[`plot_transitions()`](https://for-cast.github.io/landisutils/reference/plot_transitions.md),
[`read_biomass_output_rasters()`](https://for-cast.github.io/landisutils/reference/read_biomass_output_rasters.md),
[`transition_data()`](https://for-cast.github.io/landisutils/reference/transition_data.md)
