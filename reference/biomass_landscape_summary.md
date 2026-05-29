# Landscape biomass summary by species

Aggregates per-cell snapshot data to landscape-mean biomass density (Mg
C ha^-1) per species per timestep per replicate, then computes mean ?+/-
SD across replicates.

## Usage

``` r
biomass_landscape_summary(df)
```

## Arguments

- df:

  `data.table` or `data.frame` as returned by
  [`read_biomass_c_snapshots()`](https://for-cast.github.io/landisutils/reference/read_biomass_c_snapshots.md).

## Value

A `tibble` with columns `Time, species, mean_biomass, sd_biomass`.

## See also

Other Vegetation transition helpers:
[`community_label()`](https://for-cast.github.io/landisutils/reference/community_label.md),
[`leading_species()`](https://for-cast.github.io/landisutils/reference/leading_species.md),
[`plot_species_biomass()`](https://for-cast.github.io/landisutils/reference/plot_species_biomass.md),
[`plot_transitions()`](https://for-cast.github.io/landisutils/reference/plot_transitions.md),
[`read_biomass_c_snapshots()`](https://for-cast.github.io/landisutils/reference/read_biomass_c_snapshots.md),
[`read_biomass_output_rasters()`](https://for-cast.github.io/landisutils/reference/read_biomass_output_rasters.md),
[`transition_data()`](https://for-cast.github.io/landisutils/reference/transition_data.md)
