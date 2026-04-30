# Specify the optional `DisturbanceModifiers` table for an EDA agent

Specify the optional `DisturbanceModifiers` table for an EDA agent

## Usage

``` r
insertEDADisturbanceModifiers(df)
```

## Arguments

- df:

  (Optional) `data.frame` with columns `SHIModifier`, `Duration`,
  `Type`. When `NULL`, nothing is written.

## Value

Character string(s) to write to the suitable LANDIS-II input file.

## See also

Other EDA helpers:
[`EDA`](https://for-cast.github.io/landisutils/reference/EDA.md),
[`edaAgent()`](https://for-cast.github.io/landisutils/reference/edaAgent.md),
[`insertEDAIgnoredSpecies()`](https://for-cast.github.io/landisutils/reference/insertEDAIgnoredSpecies.md),
[`insertEDAInputFiles()`](https://for-cast.github.io/landisutils/reference/insertEDAInputFiles.md),
[`insertEDASpeciesParameters()`](https://for-cast.github.io/landisutils/reference/insertEDASpeciesParameters.md),
[`writeEDAAgentFile()`](https://for-cast.github.io/landisutils/reference/writeEDAAgentFile.md)
