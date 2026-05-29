# Plot vegetation transitions (Sankey / alluvial diagram)

Renders a Sankey-style alluvial diagram showing how cells move between
vegetation types (leading species or community labels) across
user-specified snapshot years.

## Usage

``` r
plot_transitions(lodes_df, colours = NULL, title = NULL)
```

## Arguments

- lodes_df:

  `tibble` in lodes form as returned by
  [`transition_data()`](https://for-cast.github.io/landisutils/reference/transition_data.md).

- colours:

  Named character vector mapping label names to hex colours. If `NULL`
  (default), a built-in qualitative palette is used.

- title:

  Character. Plot title (default: `NULL` -\> no title).

## Value

A `ggplot` object.

## Details

The width of each band is proportional to the mean number of cells that
follow that transition path (averaged across replicates).

## See also

[`transition_data()`](https://for-cast.github.io/landisutils/reference/transition_data.md),
[`leading_species()`](https://for-cast.github.io/landisutils/reference/leading_species.md),
[`community_label()`](https://for-cast.github.io/landisutils/reference/community_label.md)

Other Vegetation transition helpers:
[`biomass_landscape_summary()`](https://for-cast.github.io/landisutils/reference/biomass_landscape_summary.md),
[`community_label()`](https://for-cast.github.io/landisutils/reference/community_label.md),
[`leading_species()`](https://for-cast.github.io/landisutils/reference/leading_species.md),
[`plot_species_biomass()`](https://for-cast.github.io/landisutils/reference/plot_species_biomass.md),
[`read_biomass_c_snapshots()`](https://for-cast.github.io/landisutils/reference/read_biomass_c_snapshots.md),
[`read_biomass_output_rasters()`](https://for-cast.github.io/landisutils/reference/read_biomass_output_rasters.md),
[`transition_data()`](https://for-cast.github.io/landisutils/reference/transition_data.md)
