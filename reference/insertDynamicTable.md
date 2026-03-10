# Specify Dynamic Tables

Specify Dynamic Tables

## Usage

``` r
insertDynamicTable(name = NULL, df = NULL)
```

## Arguments

- name:

  Character specifying the name of the table (e.g.,
  `DynamicEcoregionTable`, `DynamicFireRegionTable`,
  `DynamicWeatherTable`).

- df:

  data.frame corresponding to the dynamic table (optional). Must have
  two columns: `Year` and `FileName`.

## Value

Character string(s) to write to the suitable LANDIS-II input file.
