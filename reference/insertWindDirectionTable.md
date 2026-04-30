# Specify the `WindDirectionTable` for the Linear Wind extension

Specify the `WindDirectionTable` for the Linear Wind extension

## Usage

``` r
insertWindDirectionTable(pct)
```

## Arguments

- pct:

  Numeric vector of length 4 giving percentages for
  `N-S, NE-SW, E-W, SE-NW`; must sum to 100.

## Value

Character string(s) to write to the suitable LANDIS-II input file.

## See also

Other Linear Wind helpers:
[`LinearWind`](https://for-cast.github.io/landisutils/reference/LinearWind.md),
[`defaultLinearWindSeverities()`](https://for-cast.github.io/landisutils/reference/defaultLinearWindSeverities.md),
[`insertLinearWindEcoregionModifiers()`](https://for-cast.github.io/landisutils/reference/insertLinearWindEcoregionModifiers.md),
[`insertLinearWindIntensityTable()`](https://for-cast.github.io/landisutils/reference/insertLinearWindIntensityTable.md),
[`insertLinearWindSeverities()`](https://for-cast.github.io/landisutils/reference/insertLinearWindSeverities.md)
