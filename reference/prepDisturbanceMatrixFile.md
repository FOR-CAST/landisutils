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

## See also

Other ForCS helpers:
[`ForCS`](https://for-cast.github.io/landisutils/reference/ForCS.md),
[`insertANPPTimeSeries()`](https://for-cast.github.io/landisutils/reference/insertANPPTimeSeries.md),
[`insertAvailableLightBiomass()`](https://for-cast.github.io/landisutils/reference/insertAvailableLightBiomass.md),
[`insertDOMPools()`](https://for-cast.github.io/landisutils/reference/insertDOMPools.md),
[`insertEcoSppDOMParameters()`](https://for-cast.github.io/landisutils/reference/insertEcoSppDOMParameters.md),
[`insertEstablishProbabilities()`](https://for-cast.github.io/landisutils/reference/insertEstablishProbabilities.md),
[`insertForCSMapControl()`](https://for-cast.github.io/landisutils/reference/insertForCSMapControl.md),
[`insertForCSProportions()`](https://for-cast.github.io/landisutils/reference/insertForCSProportions.md),
[`insertLightEstablishmentTable()`](https://for-cast.github.io/landisutils/reference/insertLightEstablishmentTable.md),
[`insertMaxBiomassTimeSeries()`](https://for-cast.github.io/landisutils/reference/insertMaxBiomassTimeSeries.md),
[`insertOutputTables()`](https://for-cast.github.io/landisutils/reference/insertOutputTables.md),
[`insertRootDynamics()`](https://for-cast.github.io/landisutils/reference/insertRootDynamics.md),
[`insertSpeciesParameters()`](https://for-cast.github.io/landisutils/reference/insertSpeciesParameters.md),
[`insertSpinUp()`](https://for-cast.github.io/landisutils/reference/insertSpinUp.md),
[`prepClimateFile()`](https://for-cast.github.io/landisutils/reference/prepClimateFile.md),
[`prepSnagFile()`](https://for-cast.github.io/landisutils/reference/prepSnagFile.md)
