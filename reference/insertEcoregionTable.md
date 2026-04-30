# Specify Dynamic Fuel Extension's Ecoregion Table

Specify Dynamic Fuel Extension's Ecoregion Table

## Usage

``` r
insertEcoregionTable(df)
```

## Arguments

- df:

  data.frame corresponding to `EcoregionTable`, with columns: `FuelType`
  (int) and `Ecoregion` (char).

## Value

Character string(s) to write to the suitable LANDIS-II input file.

## See also

Other Dynamic Fuels helpers:
[`DynamicFuels`](https://for-cast.github.io/landisutils/reference/DynamicFuels.md),
[`insertDisturbanceConversionTable()`](https://for-cast.github.io/landisutils/reference/insertDisturbanceConversionTable.md),
[`insertFuelTypesTable()`](https://for-cast.github.io/landisutils/reference/insertFuelTypesTable.md),
[`insertSpeciesFuelCoefficients()`](https://for-cast.github.io/landisutils/reference/insertSpeciesFuelCoefficients.md),
[`prepDisturbanceConversionTable()`](https://for-cast.github.io/landisutils/reference/prepDisturbanceConversionTable.md),
[`prepFuelTypesTable()`](https://for-cast.github.io/landisutils/reference/prepFuelTypesTable.md)
