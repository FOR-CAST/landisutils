# Specify the `WindSeverities` table for the Original Wind extension

Specify the `WindSeverities` table for the Original Wind extension

## Usage

``` r
insertWindSeverities(df)
```

## Arguments

- df:

  `data.frame` with columns `Severity`, `LowerAge`, `UpperAge`,
  `MortalityProbability`. Rows must be ordered by decreasing `Severity`.

## Value

Character string(s) to write to the suitable LANDIS-II input file.

## See also

Other Original Wind helpers:
[`OriginalWind`](https://for-cast.github.io/landisutils/reference/OriginalWind.md),
[`defaultWindSeverities()`](https://for-cast.github.io/landisutils/reference/defaultWindSeverities.md),
[`insertWindEventParametersTable()`](https://for-cast.github.io/landisutils/reference/insertWindEventParametersTable.md),
[`prepWindEventParametersTable()`](https://for-cast.github.io/landisutils/reference/prepWindEventParametersTable.md)
