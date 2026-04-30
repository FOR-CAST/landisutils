# Prepare Original Wind `WindEventParametersTable`

Prepare Original Wind `WindEventParametersTable`

## Usage

``` r
prepWindEventParametersTable(ecoregion_params)
```

## Arguments

- ecoregion_params:

  `data.frame` with columns `Ecoregion`, `MaxSize`, `MeanSize`,
  `MinSize`, `WindRotationPeriod`. Any additional columns are dropped.

## Value

`data.frame` suitable for passing as `WindEventParametersTable`.

## See also

Other Original Wind helpers:
[`OriginalWind`](https://for-cast.github.io/landisutils/reference/OriginalWind.md),
[`defaultWindSeverities()`](https://for-cast.github.io/landisutils/reference/defaultWindSeverities.md),
[`insertWindEventParametersTable()`](https://for-cast.github.io/landisutils/reference/insertWindEventParametersTable.md),
[`insertWindSeverities()`](https://for-cast.github.io/landisutils/reference/insertWindSeverities.md)
