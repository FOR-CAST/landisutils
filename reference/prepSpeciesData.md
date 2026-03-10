# Species Data File

Species Data File

## Usage

``` r
prepSpeciesData(df = NULL, type = NULL, path = NULL, filename = NULL)
```

## Arguments

- df:

  data.frame corresponding to the species data table

- type:

  character, corresponding to one of the following types: - "core":
  generates core species data (`.txt`) file; - "fire": generates `.csv`
  version for use with fire extensions; - "succession": generates `.csv`
  version for use with succession extensions;

- path:

  Character. Path specifying a directory to use for the scenario runs.

- filename:

  Character, specifying a filename (will be appended to `path`).

## Value

Character string(s) specifying suitable LANDIS-II input file(s), created
as a side effect of the function.
