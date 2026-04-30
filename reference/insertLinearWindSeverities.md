# Specify the `WindSeverities` table for the Linear Wind extension

Specify the `WindSeverities` table for the Linear Wind extension

## Usage

``` r
insertLinearWindSeverities(df)
```

## Arguments

- df:

  `data.frame` with columns `Severity`, `LowerAge`, `UpperAge`,
  `WindspeedMortalityThreshold`. Rows must be ordered by decreasing
  `Severity`.

## Value

Character string(s) to write to the suitable LANDIS-II input file.

## See also

Other Linear Wind helpers:
[`LinearWind`](https://for-cast.github.io/landisutils/reference/LinearWind.md),
[`defaultLinearWindSeverities()`](https://for-cast.github.io/landisutils/reference/defaultLinearWindSeverities.md),
[`insertLinearWindEcoregionModifiers()`](https://for-cast.github.io/landisutils/reference/insertLinearWindEcoregionModifiers.md),
[`insertLinearWindIntensityTable()`](https://for-cast.github.io/landisutils/reference/insertLinearWindIntensityTable.md),
[`insertWindDirectionTable()`](https://for-cast.github.io/landisutils/reference/insertWindDirectionTable.md)
