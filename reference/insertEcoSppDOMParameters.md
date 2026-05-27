# Specify `EcoSppDOMParameters` for Forest Carbon Succession (ForCS) extension

ForCS v4 reads this table from a CSV file rather than inline in the main
config. This function writes `ForCS_EcoSppDOMParameters.csv` to `path`
and returns the keyword + filename reference for inclusion in the main
config.

## Usage

``` r
insertEcoSppDOMParameters(df, path)
```

## Arguments

- df:

  data.frame corresponding to `EcoSppDOMParameters` table

- path:

  Character. Directory where the CSV file will be written.

## Value

Character vector (keyword line for the main config).

## See also

Other ForCS helpers:
[`ForCS`](https://for-cast.github.io/landisutils/reference/ForCS.md),
[`insertANPPTimeSeries()`](https://for-cast.github.io/landisutils/reference/insertANPPTimeSeries.md),
[`insertAvailableLightBiomass()`](https://for-cast.github.io/landisutils/reference/insertAvailableLightBiomass.md),
[`insertDOMPools()`](https://for-cast.github.io/landisutils/reference/insertDOMPools.md),
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
[`prepDisturbanceMatrixFile()`](https://for-cast.github.io/landisutils/reference/prepDisturbanceMatrixFile.md),
[`prepSnagFile()`](https://for-cast.github.io/landisutils/reference/prepSnagFile.md)
