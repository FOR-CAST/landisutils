# Specify Dynamic Fuel Extension's Fuel Types Table

Specify Dynamic Fuel Extension's Fuel Types Table

## Usage

``` r
insertFuelTypesTable(df)
```

## Arguments

- df:

  data.frame corresponding to `FuelTypesTable`, with columns: `FuelType`
  (int), `BaseFuel` (char), `AgeMin` (int), `AgeMax` (int), `Species`
  (char).

## Value

Character string(s) to write to the suitable LANDIS-II input file.

## See also

Other Dynamic Fuels helpers:
[`DynamicFuels`](https://for-cast.github.io/landisutils/reference/DynamicFuels.md),
[`insertDisturbanceConversionTable()`](https://for-cast.github.io/landisutils/reference/insertDisturbanceConversionTable.md),
[`insertEcoregionTable()`](https://for-cast.github.io/landisutils/reference/insertEcoregionTable.md),
[`insertSpeciesFuelCoefficients()`](https://for-cast.github.io/landisutils/reference/insertSpeciesFuelCoefficients.md),
[`prepDisturbanceConversionTable()`](https://for-cast.github.io/landisutils/reference/prepDisturbanceConversionTable.md),
[`prepFuelTypesTable()`](https://for-cast.github.io/landisutils/reference/prepFuelTypesTable.md)
