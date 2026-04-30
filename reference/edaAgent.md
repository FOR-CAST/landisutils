# Construct an EDA agent

Returns a validated agent specification used by
[EDA](https://for-cast.github.io/landisutils/reference/EDA.md). Each
agent is written to its own per-agent input file on disk and listed in
the main extension file's `EDAInputFiles` block.

## Usage

``` r
edaAgent(
  name,
  SHIMode = "mean",
  StartYear = NULL,
  EndYear = NULL,
  TransmissionRate = NULL,
  AcquisitionRate = NULL,
  InitialEpidemMap = NULL,
  DispersalType = "STATIC",
  DispersalKernel = "PowerLaw",
  DispersalMaxDist = NULL,
  AlphaCoef = NULL,
  EDASpeciesParameters = NULL,
  DisturbanceModifiers = NULL,
  IgnoredSpecies = NULL,
  extraClimateLines = character(0)
)
```

## Arguments

- name:

  Character. Agent name (no spaces).

- SHIMode:

  Character. One of `"mean"` or `"max"`.

- StartYear, EndYear:

  (Optional) Integer. Outbreak window.

- TransmissionRate:

  Numeric (`beta0`). Mean rate at which an infected cell infects another
  cell per timestep.

- AcquisitionRate:

  Numeric (`rD`). Rate of acquisition of detectable symptoms per
  timestep.

- InitialEpidemMap:

  (Optional) Character. Relative path to the initial-outbreak raster.

- DispersalType:

  Character. One of `"STATIC"` or `"DYNAMIC"`.

- DispersalKernel:

  Character. One of `"PowerLaw"` or `"NegExp"`.

- DispersalMaxDist:

  Numeric. Cut-off distance (m) for the dispersal kernel.

- AlphaCoef:

  Numeric. Exponent of the dispersal kernel.

- EDASpeciesParameters:

  `data.frame` with required columns: `Species`, `LowAge`, `LowScore`,
  `MediumAge`, `MediumScore`, `HighAge`, `HighScore`, `VulnLowAge`,
  `VulnLowMortProb`, `VulnMediumAge`, `VulnMediumMortProb`,
  `VulnHighAge`, `VulnHighMortProb`, `CFSConifer`, `MortalityPlot`.

- DisturbanceModifiers:

  (Optional) `data.frame` with columns `SHIModifier`, `Duration`,
  `Type`.

- IgnoredSpecies:

  (Optional) Character vector of species codes to exclude from the SHI
  calculation.

- extraClimateLines:

  (Optional) Character vector of raw lines for the climate-input section
  (e.g. `ClimateVariables` table). Inserted verbatim, before the
  transmission block.

## Value

A list of class `"EDAAgent"`.

## Details

Climate-related blocks (`ClimateVariables`, `DerivedClimateVariables`,
`WeatherIndexVariables`, `AnnualWeatherIndex`, formula blocks, etc.)
vary widely between pathogens; supply them verbatim via
`extraClimateLines` rather than modelling each one as an active binding.

## See also

Other EDA helpers:
[`EDA`](https://for-cast.github.io/landisutils/reference/EDA.md),
[`insertEDADisturbanceModifiers()`](https://for-cast.github.io/landisutils/reference/insertEDADisturbanceModifiers.md),
[`insertEDAIgnoredSpecies()`](https://for-cast.github.io/landisutils/reference/insertEDAIgnoredSpecies.md),
[`insertEDAInputFiles()`](https://for-cast.github.io/landisutils/reference/insertEDAInputFiles.md),
[`insertEDASpeciesParameters()`](https://for-cast.github.io/landisutils/reference/insertEDASpeciesParameters.md),
[`writeEDAAgentFile()`](https://for-cast.github.io/landisutils/reference/writeEDAAgentFile.md)
