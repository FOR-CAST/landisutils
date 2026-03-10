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
