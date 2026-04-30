# Specify the optional `DisturbanceModifiers` table

Specify the optional `DisturbanceModifiers` table

## Usage

``` r
insertDisturbanceModifiers(df)
```

## Arguments

- df:

  (Optional) `data.frame` with columns `SRDModifier`, `Duration`,
  `Type`. When `NULL`, nothing is written.

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
[`insertEcoregionModifiers()`](https://for-cast.github.io/landisutils/reference/insertEcoregionModifiers.md),
[`insertIgnoredSpecies()`](https://for-cast.github.io/landisutils/reference/insertIgnoredSpecies.md),
[`insertIntensityClasses()`](https://for-cast.github.io/landisutils/reference/insertIntensityClasses.md),
[`insertNeighborhood()`](https://for-cast.github.io/landisutils/reference/insertNeighborhood.md),
[`insertOutbreakPattern()`](https://for-cast.github.io/landisutils/reference/insertOutbreakPattern.md),
[`writeAgentFile()`](https://for-cast.github.io/landisutils/reference/writeAgentFile.md)
