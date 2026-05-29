# Build a transition data frame for alluvial plots

Takes per-cell type labels (from
[`leading_species()`](https://for-cast.github.io/landisutils/reference/leading_species.md)
or
[`community_label()`](https://for-cast.github.io/landisutils/reference/community_label.md)),
pivots to wide form, counts unique label-path combinations across all
snapshots, and converts to the lodes (long) form required by
[`ggalluvial::geom_alluvium()`](http://corybrunson.github.io/ggalluvial/reference/geom_alluvium.md).

## Usage

``` r
transition_data(label_df, times)
```

## Arguments

- label_df:

  `tibble` or `data.frame` with columns
  `scenario, replicate, Time, row, column, label` – as returned by
  [`leading_species()`](https://for-cast.github.io/landisutils/reference/leading_species.md)
  or
  [`community_label()`](https://for-cast.github.io/landisutils/reference/community_label.md).

- times:

  Integer vector of snapshot years to include, in order. All values must
  be present in `label_df$Time`.

## Value

A `tibble` in lodes form with columns `alluvium, x, stratum, y`
compatible with
[`ggalluvial::geom_alluvium()`](http://corybrunson.github.io/ggalluvial/reference/geom_alluvium.md):

- `alluvium` – integer ID for the flow path

- `x` – year (from `times`)

- `stratum` – vegetation type label at that year

- `y` – mean number of cells (averaged across replicates) following this
  path

## Details

Counts are averaged across replicates so that the diagram represents the
mean landscape behaviour.

## See also

Other Vegetation transition helpers:
[`biomass_landscape_summary()`](https://for-cast.github.io/landisutils/reference/biomass_landscape_summary.md),
[`community_label()`](https://for-cast.github.io/landisutils/reference/community_label.md),
[`leading_species()`](https://for-cast.github.io/landisutils/reference/leading_species.md),
[`plot_species_biomass()`](https://for-cast.github.io/landisutils/reference/plot_species_biomass.md),
[`plot_transitions()`](https://for-cast.github.io/landisutils/reference/plot_transitions.md),
[`read_biomass_c_snapshots()`](https://for-cast.github.io/landisutils/reference/read_biomass_c_snapshots.md),
[`read_biomass_output_rasters()`](https://for-cast.github.io/landisutils/reference/read_biomass_output_rasters.md)
