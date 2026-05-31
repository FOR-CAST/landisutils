# Leading species per cell at each snapshot

For each (replicate, Time, row, column), identifies the species with the
highest total biomass and labels that cell accordingly. Ties are broken
by alphabetical species name. Cells where total biomass across all
species is zero are labelled `"Non-vegetated"` (rather than getting an
arbitrary alphabetically-first species via the tiebreaker), matching
[`community_label()`](https://for-cast.github.io/landisutils/reference/community_label.md)'s
behaviour.

## Usage

``` r
leading_species(df)
```

## Arguments

- df:

  `data.table` or `data.frame` as returned by
  [`read_biomass_c_snapshots()`](https://for-cast.github.io/landisutils/reference/read_biomass_c_snapshots.md).

## Value

A `tibble` with columns `scenario, replicate, Time, row, column, label`
where `label` is the leading species name (or `"Non-vegetated"`).

## See also

[`community_label()`](https://for-cast.github.io/landisutils/reference/community_label.md),
[`plot_transitions()`](https://for-cast.github.io/landisutils/reference/plot_transitions.md)

Other Vegetation transition helpers:
[`biomass_landscape_summary()`](https://for-cast.github.io/landisutils/reference/biomass_landscape_summary.md),
[`community_label()`](https://for-cast.github.io/landisutils/reference/community_label.md),
[`plot_species_biomass()`](https://for-cast.github.io/landisutils/reference/plot_species_biomass.md),
[`plot_transitions()`](https://for-cast.github.io/landisutils/reference/plot_transitions.md),
[`read_biomass_c_snapshots()`](https://for-cast.github.io/landisutils/reference/read_biomass_c_snapshots.md),
[`read_biomass_output_rasters()`](https://for-cast.github.io/landisutils/reference/read_biomass_output_rasters.md),
[`transition_data()`](https://for-cast.github.io/landisutils/reference/transition_data.md)
