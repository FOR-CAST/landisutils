# Linear Wind Disturbance Extension

Linear Wind Disturbance Extension

## References

LANDIS-II Linear Wind Disturbance v3 Extension User Guide
<https://github.com/LANDIS-II-Foundation/Extension-LinearWind/blob/master/docs/LANDIS-II%20Linear%20Wind%20v3%20User%20Guide.pdf>

## See also

Other Linear Wind helpers:
[`defaultLinearWindSeverities()`](https://for-cast.github.io/landisutils/reference/defaultLinearWindSeverities.md),
[`insertLinearWindEcoregionModifiers()`](https://for-cast.github.io/landisutils/reference/insertLinearWindEcoregionModifiers.md),
[`insertLinearWindIntensityTable()`](https://for-cast.github.io/landisutils/reference/insertLinearWindIntensityTable.md),
[`insertLinearWindSeverities()`](https://for-cast.github.io/landisutils/reference/insertLinearWindSeverities.md),
[`insertWindDirectionTable()`](https://for-cast.github.io/landisutils/reference/insertWindDirectionTable.md)

## Super class

[`LandisExtension`](https://for-cast.github.io/landisutils/reference/LandisExtension.md)
-\> `LinearWind`

## Active bindings

- `NumEventsMean`:

  Numeric. Mean number of events per 40,000 km^2 per year.

- `NumEventsStDev`:

  Numeric. Standard deviation of number of events.

- `TornadoLengthLambda`:

  Numeric. Weibull lambda for tornado length (km).

- `TornadoLengthAlpha`:

  Numeric. Weibull alpha for tornado length (km).

- `TornadoWidth`:

  Numeric. Mean width of tornado events (km).

- `TornadoIntensityTable`:

  Numeric vector of 5 percentages (sum 100) for intensity classes
  `0.2, 0.4, 0.6, 0.8, 1.0`.

- `TornadoProp`:

  Numeric. Proportion of events that are tornadoes (0-1).

- `DerechoLengthLambda`:

  Numeric. Weibull lambda for derecho length (km).

- `DerechoLengthAlpha`:

  Numeric. Weibull alpha for derecho length (km).

- `DerechoWidth`:

  Numeric. Mean width of derecho events (km).

- `DerechoIntensityTable`:

  Numeric vector of 5 percentages (sum 100) for intensity classes
  `0.2, 0.4, 0.6, 0.8, 1.0`.

- `PropIntensityVar`:

  Numeric. Variability in wind intensity within an event (0-1).

- `WindDirectionTable`:

  Numeric vector of 4 percentages (sum 100) for directions
  `N-S, NE-SW, E-W, SE-NW`.

- `EcoregionModifiers`:

  `data.frame` with columns `Ecoregion`, `Modifier`.

- `WindSeverities`:

  `data.frame` with columns `Severity`, `LowerAge`, `UpperAge`,
  `WindspeedMortalityThreshold`.

- `IntensityMapNames`:

  Character. File pattern for wind intensity output maps; must contain
  the literal `{timestep}` placeholder.

- `SeverityMapNames`:

  Character. File pattern for wind severity output maps; must contain
  the literal `{timestep}` placeholder.

- `LogFile`:

  Character. Relative file path.

## Methods

### Public methods

- [`LinearWind$new()`](#method-LinearWind-initialize)

- [`LinearWind$write()`](#method-LinearWind-write)

- [`LinearWind$clone()`](#method-LinearWind-clone)

Inherited methods

- [`LandisExtension$add_file()`](https://for-cast.github.io/landisutils/reference/LandisExtension.html#method-add_file)

------------------------------------------------------------------------

### `LinearWind$new()`

#### Usage

    LinearWind$new(
      path,
      Timestep = NULL,
      NumEventsMean = NULL,
      NumEventsStDev = NULL,
      TornadoLengthLambda = NULL,
      TornadoLengthAlpha = NULL,
      TornadoWidth = NULL,
      TornadoIntensityTable = NULL,
      TornadoProp = NULL,
      DerechoLengthLambda = NULL,
      DerechoLengthAlpha = NULL,
      DerechoWidth = NULL,
      DerechoIntensityTable = NULL,
      PropIntensityVar = NULL,
      WindDirectionTable = NULL,
      EcoregionModifiers = NULL,
      WindSeverities = NULL,
      IntensityMapNames = NULL,
      SeverityMapNames = NULL,
      LogFile = "linearwind/log.csv"
    )

#### Arguments

- `path`:

  Character. Directory path.

- `Timestep`:

  Integer. Years.

- `NumEventsMean`:

  Numeric. Mean number of wind events per 40,000 km^2 per year.

- `NumEventsStDev`:

  Numeric. Standard deviation of number of wind events.

- `TornadoLengthLambda`:

  Numeric. Weibull lambda for tornado event length (km).

- `TornadoLengthAlpha`:

  Numeric. Weibull alpha for tornado event length (km).

- `TornadoWidth`:

  Numeric. Mean width of tornado events (km).

- `TornadoIntensityTable`:

  Numeric vector of length 5 giving the percentage of tornado events
  with maximum intensity in each class `0.2, 0.4, 0.6, 0.8, 1.0`; must
  sum to 100.

- `TornadoProp`:

  Numeric. Proportion of events that are tornadoes (0-1); the remainder
  are derechos.

- `DerechoLengthLambda`:

  Numeric. Weibull lambda for derecho event length (km).

- `DerechoLengthAlpha`:

  Numeric. Weibull alpha for derecho event length (km).

- `DerechoWidth`:

  Numeric. Mean width of derecho events (km).

- `DerechoIntensityTable`:

  Numeric vector of length 5 giving the percentage of derecho events
  with maximum intensity in each class `0.2, 0.4, 0.6, 0.8, 1.0`; must
  sum to 100.

- `PropIntensityVar`:

  Numeric. Variability in wind intensity within an event (0-1).

- `WindDirectionTable`:

  Numeric vector of length 4 giving the percentage of events with
  primary direction `N-S, NE-SW, E-W, SE-NW`; must sum to 100.

- `EcoregionModifiers`:

  `data.frame` with columns `Ecoregion`, `Modifier` (`Modifier` in
  `[-1, 1]`); optional.

- `WindSeverities`:

  `data.frame` with columns `Severity`, `LowerAge`, `UpperAge`,
  `WindspeedMortalityThreshold`; rows must be ordered by decreasing
  `Severity`. Defaults to
  [`defaultLinearWindSeverities()`](https://for-cast.github.io/landisutils/reference/defaultLinearWindSeverities.md).

- `IntensityMapNames`:

  Character. File pattern for wind intensity output maps; must contain
  the literal `{timestep}` placeholder.

- `SeverityMapNames`:

  Character. File pattern for wind severity output maps; must contain
  the literal `{timestep}` placeholder.

- `LogFile`:

  Character. Relative file path.

------------------------------------------------------------------------

### `LinearWind$write()`

Write extension inputs to disk

#### Usage

    LinearWind$write()

------------------------------------------------------------------------

### `LinearWind$clone()`

The objects of this class are cloneable with this method.

#### Usage

    LinearWind$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
