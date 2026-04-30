# Collapse one row of a labelled data frame into a separator-joined string

Drops the first (label) column of `df`, applies `fmt` to each remaining
cell, and collapses the result with `sep`. Used by succession-extension
table writers (e.g. `MinRelativeBiomass`, `SufficientLight`).

## Usage

``` r
.collapseRow(df, i, fmt = as.character, sep = "  ")
```

## Arguments

- df:

  data.frame whose first column is a row label and whose remaining
  columns hold the values to emit.

- i:

  integer row index.

- fmt:

  formatter applied to each cell; must return a length-1 character.
  Defaults to [`as.character()`](https://rdrr.io/r/base/character.html).

- sep:

  separator between cells. Defaults to two spaces.
