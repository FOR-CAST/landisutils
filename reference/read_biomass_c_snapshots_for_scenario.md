# Open + collect a per-scenario biomass_snapshots Arrow dataset

Convenience wrapper that opens `<scenario_dir>/<subdir>` as an Arrow
dataset partitioned by `replicate`, collects the full contents into a
tibble, and returns `NULL` for missing directories or empty datasets so
callers can short-circuit gracefully.

## Usage

``` r
read_biomass_c_snapshots_for_scenario(
  scenario_dir,
  subdir = "_aggregates/biomass_snapshots"
)
```

## Arguments

- scenario_dir:

  Path to a scenario's root directory (e.g. `"LANDIS-II/ForCS_only"`).

- subdir:

  Path within `scenario_dir` to the parquet dataset root (default
  `"_aggregates/biomass_snapshots"`).

## Value

A tibble with columns
`scenario, replicate, Time, row, column, ecoregion, species, biomass`
(matching
[`read_biomass_c_snapshots()`](https://for-cast.github.io/landisutils/reference/read_biomass_c_snapshots.md)'s
output), or `NULL`.

## Details

Isolated from the rest of the
[`read_biomass_c_snapshots()`](https://for-cast.github.io/landisutils/reference/read_biomass_c_snapshots.md)
pipeline so its body stays stable across project-side refactors:
`{targets}` does not track installed-package function bodies, so
consuming targets are not invalidated by cosmetic changes to how the
collect happens under the hood. Callers that want that invalidation
guarantee should call this function from their target commands rather
than inlining the `arrow::open_dataset() |> collect()` chain.

The default `subdir = "_aggregates/biomass_snapshots"` matches the
layout produced by `write_biomass_c_snapshots_parquet()`-style writers
in the FOR-CAST post-processing pipeline; override for other layouts.

## See also

Other Vegetation transition helpers:
[`biomass_landscape_summary()`](https://for-cast.github.io/landisutils/reference/biomass_landscape_summary.md),
[`community_label()`](https://for-cast.github.io/landisutils/reference/community_label.md),
[`leading_species()`](https://for-cast.github.io/landisutils/reference/leading_species.md),
[`plot_species_biomass()`](https://for-cast.github.io/landisutils/reference/plot_species_biomass.md),
[`plot_transitions()`](https://for-cast.github.io/landisutils/reference/plot_transitions.md),
[`read_biomass_c_snapshots()`](https://for-cast.github.io/landisutils/reference/read_biomass_c_snapshots.md),
[`read_biomass_output_rasters()`](https://for-cast.github.io/landisutils/reference/read_biomass_output_rasters.md),
[`transition_data()`](https://for-cast.github.io/landisutils/reference/transition_data.md)
