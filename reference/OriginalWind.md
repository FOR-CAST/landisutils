# Original Wind Extension

Original Wind Extension

Original Wind Extension

## References

LANDIS-II Original Wind v4.1 Extension User Guide
<https://github.com/LANDIS-II-Foundation/Extension-Base-Wind/blob/master/docs/LANDIS-II%20Original%20Wind%20v4%20User%20Guide.pdf>

## See also

Helpers that prepare inputs for this extension:
[`prepWindEventParametersTable()`](https://for-cast.github.io/landisutils/reference/prepWindEventParametersTable.md).

Other Original Wind helpers:
[`defaultWindSeverities()`](https://for-cast.github.io/landisutils/reference/defaultWindSeverities.md),
[`insertWindEventParametersTable()`](https://for-cast.github.io/landisutils/reference/insertWindEventParametersTable.md),
[`insertWindSeverities()`](https://for-cast.github.io/landisutils/reference/insertWindSeverities.md),
[`prepWindEventParametersTable()`](https://for-cast.github.io/landisutils/reference/prepWindEventParametersTable.md)

## Super class

[`landisutils::LandisExtension`](https://for-cast.github.io/landisutils/reference/LandisExtension.md)
-\> `OriginalWind`

## Active bindings

- `WindEventParametersTable`:

  `data.frame` with columns `Ecoregion`, `MaxSize`, `MeanSize`,
  `MinSize`, `WindRotationPeriod`.

- `WindSeverities`:

  `data.frame` with columns `Severity`, `LowerAge`, `UpperAge`,
  `MortalityProbability`.

- `MapNames`:

  Character. File pattern for writing outputs to disk; must contain
  `{timestep}`.

- `SummaryLogFile`:

  Character. Relative file path.

- `EventLogFile`:

  Character. Relative file path.

## Methods

### Public methods

- [`OriginalWind$new()`](#method-OriginalWind-new)

- [`OriginalWind$write()`](#method-OriginalWind-write)

- [`OriginalWind$clone()`](#method-OriginalWind-clone)

Inherited methods

- [`landisutils::LandisExtension$add_file()`](https://for-cast.github.io/landisutils/reference/LandisExtension.html#method-add_file)

------------------------------------------------------------------------

### Method `new()`

#### Usage

    OriginalWind$new(
      path,
      Timestep = NULL,
      WindEventParametersTable = NULL,
      WindSeverities = NULL,
      MapNames = NULL,
      SummaryLogFile = "wind/summary-log.csv",
      EventLogFile = "wind/event-log.csv"
    )

#### Arguments

- `path`:

  Character. Directory path.

- `Timestep`:

  Integer.

- `WindEventParametersTable`:

  `data.frame` with columns `Ecoregion`, `MaxSize`, `MeanSize`,
  `MinSize`, `WindRotationPeriod` (see
  [`prepWindEventParametersTable()`](https://for-cast.github.io/landisutils/reference/prepWindEventParametersTable.md)).

- `WindSeverities`:

  `data.frame` with columns `Severity`, `LowerAge`, `UpperAge`,
  `MortalityProbability`; rows must be ordered by decreasing `Severity`.
  Defaults to
  [`defaultWindSeverities()`](https://for-cast.github.io/landisutils/reference/defaultWindSeverities.md).

- `MapNames`:

  Character. File pattern for writing outputs to disk; must contain
  `{timestep}`.

- `SummaryLogFile`:

  Character. Relative file path.

- `EventLogFile`:

  Character. Relative file path.

------------------------------------------------------------------------

### Method [`write()`](https://rdrr.io/r/base/write.html)

Write extension inputs to disk

#### Usage

    OriginalWind$write()

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    OriginalWind$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
