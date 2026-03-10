# Prepare `Suppression_CSV_File` for Social-Climate-Fire extension

Prepare `Suppression_CSV_File` for Social-Climate-Fire extension

## Usage

``` r
prepSuppression_CSV_File(df, path, filename = "suppression.csv")
```

## Arguments

- df:

  data.frame corresponding to `Suppression_CSV_File` table

- path:

  Character. Path specifying a directory to use for the scenario runs.

- filename:

  Character, specifying a filename (will be appended to `path`).

## Value

Character string(s) specifying suitable LANDIS-II input file(s), created
as a side effect of the function.
