# Biomass Hurricane Extension

Biomass Hurricane Extension

## References

LANDIS-II Biomass Hurricane v3 Extension User Guide
<https://github.com/LANDIS-II-Foundation/Extension-Biomass-Hurricane/blob/master/docs/LANDIS-II%20Biomass%20Hurricane%20v3%20User%20Guide.pdf>

## See also

Other Hurricane helpers:
[`defaultHurricaneMortalityCurve()`](https://for-cast.github.io/landisutils/reference/defaultHurricaneMortalityCurve.md),
[`insertExposureMaps()`](https://for-cast.github.io/landisutils/reference/insertExposureMaps.md),
[`insertStormOccurrenceProbabilities()`](https://for-cast.github.io/landisutils/reference/insertStormOccurrenceProbabilities.md),
[`insertWindSpeedVulnerabilities()`](https://for-cast.github.io/landisutils/reference/insertWindSpeedVulnerabilities.md),
[`windSpeedVulnerability()`](https://for-cast.github.io/landisutils/reference/windSpeedVulnerability.md)

## Super class

[`LandisExtension`](https://for-cast.github.io/landisutils/reference/LandisExtension.md)
-\> `Hurricane`

## Active bindings

- `InputUnitsEnglish`:

  Character `"yes"` / `"no"`.

- `HurricaneRandomNumberSeed`:

  Integer.

- `StormOccurrenceProbabilities`:

  `data.frame` with columns `Storms`, `Probability`.

- `LowBoundLandfallWindSpeed`:

  Numeric.

- `ModeLandfallWindSpeed`:

  Numeric.

- `HighBoundLandfallWindSpeed`:

  Numeric.

- `CoastalSlope`:

  Numeric.

- `MeanStormIntersectionX`:

  Numeric.

- `MeanStormIntersectionY`:

  Numeric.

- `LandfallSigma`:

  Numeric.

- `StormDirectionMu`:

  Numeric.

- `StormDirectionSigma`:

  Numeric.

- `MinimumWindSpeedforDamage`:

  Numeric.

- `ExposureMaps`:

  `data.frame` with columns `Degree`, `MapName`.

- `WindSpeedVulnerabilities`:

  List of `WindSpeedVulnerability` objects.

- `MapNames`:

  Character. Output filename pattern; must contain `{timestep}`.
  Including `{stormNumber}` is recommended.

- `LogFile`:

  Character. Relative path.

- `WindReductionTableCSV`:

  Character. Relative path.

## Methods

### Public methods

- [`Hurricane$new()`](#method-Hurricane-initialize)

- [`Hurricane$write()`](#method-Hurricane-write)

- [`Hurricane$clone()`](#method-Hurricane-clone)

Inherited methods

- [`LandisExtension$add_file()`](https://for-cast.github.io/landisutils/reference/LandisExtension.html#method-add_file)

------------------------------------------------------------------------

### `Hurricane$new()`

#### Usage

    Hurricane$new(
      path,
      Timestep = NULL,
      InputUnitsEnglish = FALSE,
      HurricaneRandomNumberSeed = NULL,
      StormOccurrenceProbabilities = NULL,
      LowBoundLandfallWindSpeed = NULL,
      ModeLandfallWindSpeed = NULL,
      HighBoundLandfallWindSpeed = NULL,
      CoastalSlope = NULL,
      MeanStormIntersectionX = NULL,
      MeanStormIntersectionY = NULL,
      LandfallSigma = NULL,
      StormDirectionMu = NULL,
      StormDirectionSigma = NULL,
      MinimumWindSpeedforDamage = NULL,
      ExposureMaps = NULL,
      WindSpeedVulnerabilities = list(),
      MapNames = NULL,
      LogFile = "hurricane/hurricane-log.csv",
      WindReductionTableCSV = NULL
    )

#### Arguments

- `path`:

  Character. Directory path.

- `Timestep`:

  Integer. Years.

- `InputUnitsEnglish`:

  Logical (or `"yes"`/`"no"`). When `TRUE`, wind speeds are in mph;
  otherwise km/h.

- `HurricaneRandomNumberSeed`:

  Optional integer. Seed for the hurricane-specific RNG; lets hurricanes
  vary independently of the Core RNG. Omit to fall back to the Core
  seed.

- `StormOccurrenceProbabilities`:

  `data.frame` with columns `Storms` (integer) and `Probability`
  (numeric); the probabilities must sum to `1.0`.

- `LowBoundLandfallWindSpeed, ModeLandfallWindSpeed, HighBoundLandfallWindSpeed`:

  Numeric. Landfall wind-speed distribution parameters (units per
  `InputUnitsEnglish`).

- `CoastalSlope`:

  Numeric. Slope factor relative to landscape.

- `MeanStormIntersectionX, MeanStormIntersectionY`:

  Numeric. Mean storm-centre coordinates.

- `LandfallSigma`:

  Numeric. Variance of landfalls around the mean.

- `StormDirectionMu, StormDirectionSigma`:

  Numeric. Mean and standard deviation of storm direction (degrees).

- `MinimumWindSpeedforDamage`:

  Numeric. Damage threshold; should match the smallest wind threshold in
  `WindSpeedVulnerabilities`.

- `ExposureMaps`:

  `data.frame` with columns `Degree` (integer) and `MapName`
  (character).

- `WindSpeedVulnerabilities`:

  List of `WindSpeedVulnerability` sub-objects (see
  [`windSpeedVulnerability()`](https://for-cast.github.io/landisutils/reference/windSpeedVulnerability.md)).

- `MapNames`:

  Character. Output filename pattern; must contain the literal
  `{timestep}`. Including `{stormNumber}` is recommended so per-storm
  maps within a timestep do not overwrite one another.

- `LogFile`:

  Character. Relative file path for the CSV log.

- `WindReductionTableCSV`:

  Optional character. Relative path to a wind-reduction-factor CSV
  (links to the Output Cohort Statistics `Evenness` calculation). Omit
  to disable structure-based wind-speed reduction.

------------------------------------------------------------------------

### `Hurricane$write()`

Write extension inputs to disk

#### Usage

    Hurricane$write()

------------------------------------------------------------------------

### `Hurricane$clone()`

The objects of this class are cloneable with this method.

#### Usage

    Hurricane$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
