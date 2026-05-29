# Community label per cell at each snapshot

Labels each cell with a community type defined by its top-`n_spp`
species by biomass, joined with `"-"` (e.g. `"Hw-Sx"` for a western
hemlock – spruce co-dominant cell). Cells where all species biomass is
zero are labelled `"Non-vegetated"`.

## Usage

``` r
community_label(df, n_spp = 2L, min_pct = 0.1)
```

## Arguments

- df:

  `data.table` or `data.frame` as returned by
  [`read_biomass_c_snapshots()`](https://for-cast.github.io/landisutils/reference/read_biomass_c_snapshots.md).

- n_spp:

  Integer. Number of top species to include in the label (default `2`).

- min_pct:

  Numeric in (0, 1). Minimum fraction of total cell biomass that the top
  species must account for to be included; remaining species in the
  top-`n_spp` set that fall below this threshold are dropped from the
  label (default `0.1`, i.e. a species contributing \< 10% of cell total
  is omitted).

## Value

A `tibble` with columns `scenario, replicate, Time, row, column, label`.

## See also

[`leading_species()`](https://for-cast.github.io/landisutils/reference/leading_species.md),
[`plot_transitions()`](https://for-cast.github.io/landisutils/reference/plot_transitions.md)

Other Vegetation transition helpers:
[`biomass_landscape_summary()`](https://for-cast.github.io/landisutils/reference/biomass_landscape_summary.md),
[`leading_species()`](https://for-cast.github.io/landisutils/reference/leading_species.md),
[`plot_species_biomass()`](https://for-cast.github.io/landisutils/reference/plot_species_biomass.md),
[`plot_transitions()`](https://for-cast.github.io/landisutils/reference/plot_transitions.md),
[`read_biomass_c_snapshots()`](https://for-cast.github.io/landisutils/reference/read_biomass_c_snapshots.md),
[`read_biomass_output_rasters()`](https://for-cast.github.io/landisutils/reference/read_biomass_output_rasters.md),
[`transition_data()`](https://for-cast.github.io/landisutils/reference/transition_data.md)
