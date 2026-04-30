# Specify Dynamic Fuel Extension's Disturbance Conversion Table

Specify Dynamic Fuel Extension's Disturbance Conversion Table

## Usage

``` r
insertDisturbanceConversionTable(df)
```

## Arguments

- df:

  data.frame corresponding to `DisturbanceConversionTable`, with
  columns: `Fuel` (int), `Type` (int), `Duration` (int), and
  `Prescription` (char).

## Value

Character string(s) to write to the suitable LANDIS-II input file.

## See also

Other Dynamic Fuels helpers:
[`DynamicFuels`](https://for-cast.github.io/landisutils/reference/DynamicFuels.md),
[`insertEcoregionTable()`](https://for-cast.github.io/landisutils/reference/insertEcoregionTable.md),
[`insertFuelTypesTable()`](https://for-cast.github.io/landisutils/reference/insertFuelTypesTable.md),
[`insertSpeciesFuelCoefficients()`](https://for-cast.github.io/landisutils/reference/insertSpeciesFuelCoefficients.md),
[`prepDisturbanceConversionTable()`](https://for-cast.github.io/landisutils/reference/prepDisturbanceConversionTable.md),
[`prepFuelTypesTable()`](https://for-cast.github.io/landisutils/reference/prepFuelTypesTable.md)
