# Specify the dynamic-browser-population rows for the Biomass Browse extension

Emits the keyword `DynamicPopulation` followed by `RMin`, `RMax`,
`MortalityMin`, `MortalityMax`, `PredationMin`, `PredationMax`,
`HarvestMin`, `HarvestMax` (one keyword per line). Returns
`character(0)` when `dyn` is `NULL`.

## Usage

``` r
insertBrowseDynamicPopulation(dyn)
```

## Arguments

- dyn:

  Named list of dynamic-population parameters.

## Value

Character string(s) to write to the suitable LANDIS-II input file.

## See also

Other Biomass Browse helpers:
[`BiomassBrowse`](https://for-cast.github.io/landisutils/reference/BiomassBrowse.md),
[`insertBrowseSpeciesTable()`](https://for-cast.github.io/landisutils/reference/insertBrowseSpeciesTable.md)
