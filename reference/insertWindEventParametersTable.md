# Specify the `WindEventParameters` table for the Original Wind extension

Specify the `WindEventParameters` table for the Original Wind extension

## Usage

``` r
insertWindEventParametersTable(df)
```

## Arguments

- df:

  `data.frame` with columns `Ecoregion`, `MaxSize`, `MeanSize`,
  `MinSize`, `WindRotationPeriod`.

## Value

Character string(s) to write to the suitable LANDIS-II input file.

## See also

Other Original Wind helpers:
[`OriginalWind`](https://for-cast.github.io/landisutils/reference/OriginalWind.md),
[`defaultWindSeverities()`](https://for-cast.github.io/landisutils/reference/defaultWindSeverities.md),
[`insertWindSeverities()`](https://for-cast.github.io/landisutils/reference/insertWindSeverities.md),
[`prepWindEventParametersTable()`](https://for-cast.github.io/landisutils/reference/prepWindEventParametersTable.md)
