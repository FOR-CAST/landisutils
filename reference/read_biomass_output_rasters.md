# Read Output.Biomass raster snapshots

**General-purpose reader.** Reads per-species biomass rasters written by
the LANDIS-II Output Biomass v4 extension (`Output.Biomass-v4`) and
returns per-cell, per-species total live biomass at the requested
snapshot years.

## Usage

``` r
read_biomass_output_rasters(
  dirs,
  times,
  species,
  live_map_pattern = "outputs/biomass/biomass-{species}-{timestep}.tif",
  run_name = NULL
)
```

## Arguments

- dirs:

  Character vector of replicate root directories (one per replicate),
  each containing output rasters at the path given by
  `live_map_pattern`.

- times:

  Integer vector of snapshot years to extract.

- species:

  Character vector of species names to include.

- live_map_pattern:

  Character. File naming pattern for live-biomass rasters, using
  `{species}` and `{timestep}` as literal placeholders (same notation as
  the `MapNames` directive in the Output Biomass extension config file).
  Default: `"outputs/biomass/biomass-{species}-{timestep}.tif"`.

- run_name:

  Character. Scenario label attached as the `scenario` column. If `NULL`
  (default), derived from `basename(dirname(dirs[1]))`.

## Value

A `data.table` with columns
`scenario, replicate, Time, row, column, species, biomass` where
`biomass` is total live biomass in Mg C ha^-1. The `ecoregion` column
present in
[`read_biomass_c_snapshots()`](https://for-cast.github.io/landisutils/reference/read_biomass_c_snapshots.md)
output is absent here (rasters do not carry ecoregion attribution per
cell), but all downstream functions treat `ecoregion` as optional. This
is the canonical format accepted by all downstream functions
([`leading_species()`](https://for-cast.github.io/landisutils/reference/leading_species.md),
[`community_label()`](https://for-cast.github.io/landisutils/reference/community_label.md),
[`biomass_landscape_summary()`](https://for-cast.github.io/landisutils/reference/biomass_landscape_summary.md)).

## Details

This reader works with any LANDIS-II succession extension (Biomass
Succession, ForCS, NECN, etc.) as long as Output.Biomass is included in
the scenario. Be aware that writing many raster outputs can slow down
the simulation; for ForCS scenarios,
[`read_biomass_c_snapshots()`](https://for-cast.github.io/landisutils/reference/read_biomass_c_snapshots.md)
avoids this overhead by reading the per-cohort carbon log that ForCS
writes unconditionally.

Raster values are assumed to be in g C m^-2 and are converted to Mg C
ha^-1 (multiply by 0.01). This is correct for ForCS; for Biomass
Succession the rasters store g dry biomass m^-2, so apply an appropriate
carbon fraction before downstream use.

## See also

[`read_biomass_c_snapshots()`](https://for-cast.github.io/landisutils/reference/read_biomass_c_snapshots.md)
for the ForCS-specific fast path.

Other Vegetation transition helpers:
[`biomass_landscape_summary()`](https://for-cast.github.io/landisutils/reference/biomass_landscape_summary.md),
[`community_label()`](https://for-cast.github.io/landisutils/reference/community_label.md),
[`leading_species()`](https://for-cast.github.io/landisutils/reference/leading_species.md),
[`plot_species_biomass()`](https://for-cast.github.io/landisutils/reference/plot_species_biomass.md),
[`plot_transitions()`](https://for-cast.github.io/landisutils/reference/plot_transitions.md),
[`read_biomass_c_snapshots()`](https://for-cast.github.io/landisutils/reference/read_biomass_c_snapshots.md),
[`transition_data()`](https://for-cast.github.io/landisutils/reference/transition_data.md)
