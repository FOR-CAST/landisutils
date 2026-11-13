# Specify `SpinUpLastDisturbanceMap` for Forest Carbon Succession (ForCS) extension

Optional map and code table, written after `SpinUpReturnIntervals`. A
cell's code overrides its ecoregion's `LastDisturbance`; code 0, or a
code not in the table, keeps the ecoregion's.

## Usage

``` r
insertSpinUpLastDisturbanceMap(path, codes)
```

## Arguments

- path:

  Character. Relative path to the GeoTIFF of codes.

- codes:

  data.frame with columns `Code` (unique whole numbers \>= 1) and
  `LastDisturbance` (`"fire 1"` to `"fire 5"`, or a single-word
  disturbance type).

## Value

character vector of lines

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
[`insertSpinUpReturnIntervals()`](https://for-cast.github.io/landisutils/reference/insertSpinUpReturnIntervals.md),
[`prepClimateFile()`](https://for-cast.github.io/landisutils/reference/prepClimateFile.md),
[`prepDisturbanceMatrixFile()`](https://for-cast.github.io/landisutils/reference/prepDisturbanceMatrixFile.md),
[`prepSnagFile()`](https://for-cast.github.io/landisutils/reference/prepSnagFile.md)
