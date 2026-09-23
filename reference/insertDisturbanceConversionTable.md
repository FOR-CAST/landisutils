# Specify Dynamic Fuel Extension's Disturbance Conversion Table

Specify Dynamic Fuel Extension's Disturbance Conversion Table

## Usage

``` r
insertDisturbanceConversionTable(df)
```

## Arguments

- df:

  data.frame corresponding to `DisturbanceConversionTable`, with
  columns: `Fuel` (int, the fuel type index), `Duration` (int, years)
  and `Prescription` (char, a harvest prescription name or a
  `FireSeverityN` / `WindSeverityN` keyword).

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
