# Specify the `EDASpeciesParameters` block for an EDA agent

Specify the `EDASpeciesParameters` block for an EDA agent

## Usage

``` r
insertEDASpeciesParameters(df)
```

## Arguments

- df:

  `data.frame` with required columns: `Species`, `LowAge`, `LowScore`,
  `MediumAge`, `MediumScore`, `HighAge`, `HighScore`, `VulnLowAge`,
  `VulnLowMortProb`, `VulnMediumAge`, `VulnMediumMortProb`,
  `VulnHighAge`, `VulnHighMortProb`, `CFSConifer`, `MortalityPlot`.

## Value

Character string(s) to write to the suitable LANDIS-II input file.

## See also

Other EDA helpers:
[`EDA`](https://for-cast.github.io/landisutils/reference/EDA.md),
[`edaAgent()`](https://for-cast.github.io/landisutils/reference/edaAgent.md),
[`insertEDADisturbanceModifiers()`](https://for-cast.github.io/landisutils/reference/insertEDADisturbanceModifiers.md),
[`insertEDAIgnoredSpecies()`](https://for-cast.github.io/landisutils/reference/insertEDAIgnoredSpecies.md),
[`insertEDAInputFiles()`](https://for-cast.github.io/landisutils/reference/insertEDAInputFiles.md),
[`writeEDAAgentFile()`](https://for-cast.github.io/landisutils/reference/writeEDAAgentFile.md)
