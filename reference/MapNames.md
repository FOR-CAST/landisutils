# Create Map Names pattern for file outputs

Create Map Names pattern for file outputs

## Usage

``` r
MapNames(type, ext_type, path = ".")
```

## Arguments

- type:

  Character, specifying the output file type (e.g., 'severity').

- ext_type:

  Character, specifying the output extension type (e.g., 'fire').

- path:

  Character. Path specifying a directory to use for the scenario runs.

## Value

Character, specifying filename pattern for map outputs.

## Examples

``` r
SeverityMaps <- MapNames("severity", "fire")
PctConiferMaps <- MapNames("PctConifer", "fire")
PctDeadFirMaps <- MapNames("PctDeadFir", "fire")
```
