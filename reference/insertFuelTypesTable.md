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
