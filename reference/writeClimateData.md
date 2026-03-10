# Write LANDIS-II Climate Input File to disk

Simple wrapper around
[`utils::write.csv()`](https://rdrr.io/r/utils/write.table.html).

## Usage

``` r
writeClimateData(df, file)
```

## Arguments

- df:

  data.frame corresponding to Climate Input Data table

- file:

  Character, specifying the path to the file.

## Value

Character string(s) specifying suitable LANDIS-II input file(s), created
as a side effect of the function.
