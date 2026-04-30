# Construct a Climate BDA agent

Returns a validated agent specification. One or more of these is passed
to
[ClimateBDA](https://for-cast.github.io/landisutils/reference/ClimateBDA.md)
via `Agents`; each is written to its own per-agent input file on disk
and listed in the main extension file's `BDAInputFiles` block.

## Usage

``` r
bdaAgent(
  name,
  BDPCalibrator = NULL,
  SRDMode = NULL,
  StartYear = NULL,
  EndYear = NULL,
  OutbreakPattern = NULL,
  Mean = NULL,
  StDev = NULL,
  MaxInterval = NULL,
  MinInterval = NULL,
  TimeSinceLastEpidemic = NULL,
  TemporalType = NULL,
  MinROS = NULL,
  MaxROS = NULL,
  Dispersal = NULL,
  DispersalRate = NULL,
  EpidemicThresh = NULL,
  InitialEpicenterNum = NULL,
  OutbreakEpicenterCoeff = NULL,
  OutbreakEpicenterThresh = NULL,
  SeedEpicenter = NULL,
  SeedEpicenterMax = NULL,
  SeedEpicenterCoeff = NULL,
  DispersalTemplate = NULL,
  NeighborFlag = NULL,
  NeighborSpeedUp = NULL,
  NeighborRadius = NULL,
  NeighborShape = NULL,
  NeighborWeight = NULL,
  IntensityClass2_BDP = NULL,
  IntensityClass3_BDP = NULL,
  EcoregionModifiers = NULL,
  DisturbanceModifiers = NULL,
  BDASpeciesParameters = NULL,
  IgnoredSpecies = NULL
)
```

## Arguments

- name:

  Character. Agent name (no spaces); used as `BDAAgentName` and as the
  basename of the per-agent input file.

- BDPCalibrator:

  Numeric. BDP calibrator.

- SRDMode:

  Character. One of `"max"` or `"mean"`.

- StartYear, EndYear:

  (Optional) Integer. Outbreak window.

- OutbreakPattern:

  Character. One of `"CyclicNormal"` or `"CyclicUniform"`.

- Mean, StDev:

  Numeric. Required when `OutbreakPattern = "CyclicNormal"`.

- MaxInterval, MinInterval:

  Numeric. Required when `OutbreakPattern = "CyclicUniform"`.

- TimeSinceLastEpidemic:

  Integer. Years.

- TemporalType:

  Character. One of `"pulse"` or `"variablepulse"`.

- MinROS, MaxROS:

  Integer. Region-of-spread bounds.

- Dispersal:

  Logical / `"yes"` / `"no"`. Whether dispersal is enabled.

- DispersalRate:

  Numeric. Meters/year. Required when `Dispersal` is truthy.

- EpidemicThresh:

  Numeric.

- InitialEpicenterNum:

  Integer.

- OutbreakEpicenterCoeff, OutbreakEpicenterThresh:

  Numeric.

- SeedEpicenter:

  Logical / `"yes"` / `"no"`.

- SeedEpicenterMax:

  (Optional, v5+) Integer.

- SeedEpicenterCoeff:

  Numeric. Required when `SeedEpicenter` is truthy.

- DispersalTemplate:

  Character. One of `"MaxRadius"`, `"4N"`, `"8N"`, `"12N"`, or `"24N"`.

- NeighborFlag:

  Logical / `"yes"` / `"no"`. Whether the neighborhood resource
  calculation is enabled.

- NeighborSpeedUp:

  Character. One of `"none"`, `"2x"`, `"3x"`, or `"4x"`. Required when
  `NeighborFlag` is truthy.

- NeighborRadius:

  Numeric. Meters. Required when `NeighborFlag` is truthy.

- NeighborShape:

  Character. One of `"uniform"`, `"linear"`, or `"gaussian"`. Required
  when `NeighborFlag` is truthy.

- NeighborWeight:

  Numeric. Required when `NeighborFlag` is truthy.

- IntensityClass2_BDP, IntensityClass3_BDP:

  Numeric. BDP thresholds for intensity classes 2 and 3 (class 1 is
  hardwired to 0.0).

- EcoregionModifiers:

  (Optional) `data.frame` with columns `Ecoregion`, `CapacityModifier`.

- DisturbanceModifiers:

  (Optional) `data.frame` with columns `SRDModifier`, `Duration`,
  `Type`. The `Type` column may contain space-separated disturbance type
  names.

- BDASpeciesParameters:

  `data.frame` with columns `Species`, `MinorHostAge`,
  `MinorHostSRDProb`, `SecondHostAge`, `SecondHostSRDProb`,
  `MajorHostAge`, `MajorHostSRDProb`, `Class3Age`, `Class3VulnProb`,
  `Class2Age`, `Class2VulnProb`, `Class1Age`, `Class1VulnProb`,
  `CFSConifer`.

- IgnoredSpecies:

  (Optional) Character vector of species codes to exclude from BDA
  disturbance.

## Value

A list of class `"BDAAgent"`.

## See also

Other Climate BDA helpers:
[`ClimateBDA`](https://for-cast.github.io/landisutils/reference/ClimateBDA.md),
[`insertAgentHeader()`](https://for-cast.github.io/landisutils/reference/insertAgentHeader.md),
[`insertBDAInputFiles()`](https://for-cast.github.io/landisutils/reference/insertBDAInputFiles.md),
[`insertBDASpeciesParameters()`](https://for-cast.github.io/landisutils/reference/insertBDASpeciesParameters.md),
[`insertDispersal()`](https://for-cast.github.io/landisutils/reference/insertDispersal.md),
[`insertDisturbanceModifiers()`](https://for-cast.github.io/landisutils/reference/insertDisturbanceModifiers.md),
[`insertEcoregionModifiers()`](https://for-cast.github.io/landisutils/reference/insertEcoregionModifiers.md),
[`insertIgnoredSpecies()`](https://for-cast.github.io/landisutils/reference/insertIgnoredSpecies.md),
[`insertIntensityClasses()`](https://for-cast.github.io/landisutils/reference/insertIntensityClasses.md),
[`insertNeighborhood()`](https://for-cast.github.io/landisutils/reference/insertNeighborhood.md),
[`insertOutbreakPattern()`](https://for-cast.github.io/landisutils/reference/insertOutbreakPattern.md),
[`writeAgentFile()`](https://for-cast.github.io/landisutils/reference/writeAgentFile.md)
