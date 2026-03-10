# Prepare `DisturbanceMatrixFile` for Forest Carbon Succession (ForCS) extension

Prepare `DisturbanceMatrixFile` for Forest Carbon Succession (ForCS)
extension

## Usage

``` r
prepDisturbanceMatrixFile(
  DisturbFireTransferDOM = NULL,
  DisturbOtherTransferDOM = NULL,
  DisturbFireTransferBiomass = NULL,
  DisturbOtherTransferBiomass = NULL,
  path,
  filename = "ForCS_DM.txt"
)
```

## Arguments

- DisturbFireTransferDOM:

  `data.frame`

- DisturbOtherTransferDOM:

  `data.frame`

- DisturbFireTransferBiomass:

  `data.frame`

- DisturbOtherTransferBiomass:

  `data.frame`

- path:

  Character. Path specifying a directory to use for the scenario runs.

- filename:

  Character, specifying a filename (will be appended to `path`).

## Value

Character string(s) specifying suitable LANDIS-II input file(s), created
as a side effect of the function.

data.frame
