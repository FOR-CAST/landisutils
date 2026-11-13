# Specify `SpinUpReturnIntervals` for Forest Carbon Succession (ForCS) extension

Optional table, written after `SpinUp`. The spin-up of a listed
ecoregion cycles a stand of the return interval's age instead of the
present stand's age.

## Usage

``` r
insertSpinUpReturnIntervals(df)
```

## Arguments

- df:

  data.frame with columns `Ecoregion` (character, unique),
  `ReturnInterval` (whole years \>= 1), `CycleDisturbance` and
  `LastDisturbance` (each `"fire 1"` to `"fire 5"`, or a single-word
  disturbance type; two-word types are not supported).

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
[`insertSpinUpLastDisturbanceMap()`](https://for-cast.github.io/landisutils/reference/insertSpinUpLastDisturbanceMap.md),
[`prepClimateFile()`](https://for-cast.github.io/landisutils/reference/prepClimateFile.md),
[`prepDisturbanceMatrixFile()`](https://for-cast.github.io/landisutils/reference/prepDisturbanceMatrixFile.md),
[`prepSnagFile()`](https://for-cast.github.io/landisutils/reference/prepSnagFile.md)
