# Specify the `EcoregionModifiers` table for the Linear Wind extension

Ecoregions not in the table are assigned a modifier of 0 (no
modification).

## Usage

``` r
insertLinearWindEcoregionModifiers(df)
```

## Arguments

- df:

  `data.frame` with columns `Ecoregion`, `Modifier`. May be `NULL`.

## Value

Character string(s) to write to the suitable LANDIS-II input file.

## See also

Other Linear Wind helpers:
[`LinearWind`](https://for-cast.github.io/landisutils/reference/LinearWind.md),
[`defaultLinearWindSeverities()`](https://for-cast.github.io/landisutils/reference/defaultLinearWindSeverities.md),
[`insertLinearWindIntensityTable()`](https://for-cast.github.io/landisutils/reference/insertLinearWindIntensityTable.md),
[`insertLinearWindSeverities()`](https://for-cast.github.io/landisutils/reference/insertLinearWindSeverities.md),
[`insertWindDirectionTable()`](https://for-cast.github.io/landisutils/reference/insertWindDirectionTable.md)
