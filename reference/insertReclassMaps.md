# Specify the `ReclassMaps` block for the Output Biomass Reclass extension

Specify the `ReclassMaps` block for the Output Biomass Reclass extension

## Usage

``` r
insertReclassMaps(x)
```

## Arguments

- x:

  Nested named list (see
  [OutputBiomassReclass](https://for-cast.github.io/landisutils/reference/OutputBiomassReclass.md))
  whose top-level names are reclassification map names and whose values
  are named lists mapping forest type -\> character vector of species
  codes.

## Value

Character string(s) to write to the suitable LANDIS-II input file.

## See also

Other Biomass Reclass Output helpers:
[`OutputBiomassReclass`](https://for-cast.github.io/landisutils/reference/OutputBiomassReclass.md)
