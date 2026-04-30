# Specify the optional `EcoregionModifiers` table

Specify the optional `EcoregionModifiers` table

## Usage

``` r
insertEcoregionModifiers(df)
```

## Arguments

- df:

  (Optional) `data.frame` with columns `Ecoregion`, `CapacityModifier`.
  When `NULL`, nothing is written.

## Value

Character string(s) to write to the suitable LANDIS-II input file.

## See also

Other Climate BDA helpers:
[`ClimateBDA`](https://for-cast.github.io/landisutils/reference/ClimateBDA.md),
[`bdaAgent()`](https://for-cast.github.io/landisutils/reference/bdaAgent.md),
[`insertAgentHeader()`](https://for-cast.github.io/landisutils/reference/insertAgentHeader.md),
[`insertBDAInputFiles()`](https://for-cast.github.io/landisutils/reference/insertBDAInputFiles.md),
[`insertBDASpeciesParameters()`](https://for-cast.github.io/landisutils/reference/insertBDASpeciesParameters.md),
[`insertDispersal()`](https://for-cast.github.io/landisutils/reference/insertDispersal.md),
[`insertDisturbanceModifiers()`](https://for-cast.github.io/landisutils/reference/insertDisturbanceModifiers.md),
[`insertIgnoredSpecies()`](https://for-cast.github.io/landisutils/reference/insertIgnoredSpecies.md),
[`insertIntensityClasses()`](https://for-cast.github.io/landisutils/reference/insertIntensityClasses.md),
[`insertNeighborhood()`](https://for-cast.github.io/landisutils/reference/insertNeighborhood.md),
[`insertOutbreakPattern()`](https://for-cast.github.io/landisutils/reference/insertOutbreakPattern.md),
[`writeAgentFile()`](https://for-cast.github.io/landisutils/reference/writeAgentFile.md)
