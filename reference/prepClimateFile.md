# Prepare `ClimateFile` for Forest Carbon Succession (ForCS) extension

Prepare `ClimateFile` for Forest Carbon Succession (ForCS) extension

## Usage

``` r
prepClimateFile(df = NULL, path, filename = "ForCS_climate.txt")
```

## Arguments

- df:

  `data.frame`

- path:

  Character. Path specifying a directory to use for the scenario runs.

- filename:

  Character, specifying a filename (will be appended to `path`).

## Value

Character string(s) specifying suitable LANDIS-II input file(s), created
as a side effect of the function.

data.frame
