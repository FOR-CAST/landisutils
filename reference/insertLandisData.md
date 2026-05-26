# Write a LANDIS-II `LandisData` header block

Emits the `landisutils` auto-generated file header followed by the
`LandisData "<x>"` line and a blank line. Every LANDIS-II input file
starts with this block.

## Usage

``` r
insertLandisData(x)
```

## Arguments

- x:

  Character. The `LandisData` value (extension or scenario name).

## Value

Character string(s) to write to the suitable LANDIS-II input file.
