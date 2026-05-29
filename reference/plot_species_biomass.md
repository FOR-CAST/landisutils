# Plot species biomass over time (stacked area)

Creates a stacked area chart of landscape-mean biomass density by
species over time.

## Usage

``` r
plot_species_biomass(summary_df, colours = NULL, title = NULL)
```

## Arguments

- summary_df:

  `tibble` as returned by
  [`biomass_landscape_summary()`](https://for-cast.github.io/landisutils/reference/biomass_landscape_summary.md).

- colours:

  Named character vector of hex colours, one per species. If `NULL`
  (default), a built-in qualitative palette is used.

- title:

  Character. Plot title (default: `NULL` -\> no title).

## Value

A `ggplot` object.

## See also

[`read_biomass_c_snapshots()`](https://for-cast.github.io/landisutils/reference/read_biomass_c_snapshots.md),
[`biomass_landscape_summary()`](https://for-cast.github.io/landisutils/reference/biomass_landscape_summary.md)

Other Vegetation transition helpers:
[`biomass_landscape_summary()`](https://for-cast.github.io/landisutils/reference/biomass_landscape_summary.md),
[`community_label()`](https://for-cast.github.io/landisutils/reference/community_label.md),
[`leading_species()`](https://for-cast.github.io/landisutils/reference/leading_species.md),
[`plot_transitions()`](https://for-cast.github.io/landisutils/reference/plot_transitions.md),
[`read_biomass_c_snapshots()`](https://for-cast.github.io/landisutils/reference/read_biomass_c_snapshots.md),
[`read_biomass_output_rasters()`](https://for-cast.github.io/landisutils/reference/read_biomass_output_rasters.md),
[`transition_data()`](https://for-cast.github.io/landisutils/reference/transition_data.md)
