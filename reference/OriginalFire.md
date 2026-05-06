# Original Fire Extension

Original Fire Extension

## References

LANDIS-II Original Fire v5.0 Extension User Guide
<https://github.com/LANDIS-II-Foundation/Extension-Base-Fire/blob/master/docs/LANDIS-II%20Original%20Fire%20v5.0%20User%20Guide.pdf>

## See also

Helpers that prepare inputs for this extension:
[`prepFireRegionParametersTable()`](https://for-cast.github.io/landisutils/reference/prepFireRegionParametersTable.md),
[`prepInitialFireRegionsMap()`](https://for-cast.github.io/landisutils/reference/prepInitialFireRegionsMap.md).

Other Original Fire helpers:
[`defaultFuelCurveTable()`](https://for-cast.github.io/landisutils/reference/defaultFuelCurveTable.md),
[`insertFireRegionParametersTable()`](https://for-cast.github.io/landisutils/reference/insertFireRegionParametersTable.md),
[`insertFuelCurveTable()`](https://for-cast.github.io/landisutils/reference/insertFuelCurveTable.md),
[`insertWindCurveTable()`](https://for-cast.github.io/landisutils/reference/insertWindCurveTable.md),
[`prepFireRegionParametersTable()`](https://for-cast.github.io/landisutils/reference/prepFireRegionParametersTable.md),
[`prepInitialFireRegionsMap()`](https://for-cast.github.io/landisutils/reference/prepInitialFireRegionsMap.md)

## Super class

[`LandisExtension`](https://for-cast.github.io/landisutils/reference/LandisExtension.md)
-\> `OriginalFire`

## Active bindings

- `Species_CSV_File`:

  Character. Relative file path.

- `FireRegionParametersTable`:

  `data.frame`.

- `InitialFireRegionsMap`:

  Character. Relative file path.

- `DynamicFireRegionTable`:

  `data.frame`.

- `FuelCurveTable`:

  `data.frame`.

- `WindCurveTable`:

  `data.frame`.

- `FireDamageTable`:

  `data.frame`.

- `MapNames`:

  Character. File pattern for writing outputs to disk.

- `LogFile`:

  Character. Relative file path.

- `SummaryLogFile`:

  Character. Relative file path.

## Methods

### Public methods

- [`OriginalFire$new()`](#method-OriginalFire-initialize)

- [`OriginalFire$write()`](#method-OriginalFire-write)

- [`OriginalFire$clone()`](#method-OriginalFire-clone)

Inherited methods

- [`LandisExtension$add_file()`](https://for-cast.github.io/landisutils/reference/LandisExtension.html#method-add_file)

------------------------------------------------------------------------

### `OriginalFire$new()`

#### Usage

    OriginalFire$new(
      path,
      Timestep = NULL,
      Species_CSV_File = NULL,
      FireRegionParametersTable = NULL,
      InitialFireRegionsMap = NULL,
      DynamicFireRegionTable = NULL,
      FuelCurveTable = NULL,
      WindCurveTable = NULL,
      FireDamageTable = NULL,
      MapNames = NULL,
      LogFile = "fire/log.csv",
      SummaryLogFile = "fire/summary-log.csv"
    )

#### Arguments

- `path`:

  Character. Directory path.

- `Timestep`:

  Integer.

- `Species_CSV_File`:

  Character. Relative file path.

- `FireRegionParametersTable`:

  `data.frame`.

- `InitialFireRegionsMap`:

  `SpatRaster`.

- `DynamicFireRegionTable`:

  `data.frame`.

- `FuelCurveTable`:

  `data.frame`.

- `WindCurveTable`:

  `data.frame`.

- `FireDamageTable`:

  `data.frame`.

- `MapNames`:

  Character. File pattern for writing outputs to disk.

- `LogFile`:

  Character. Relative file path.

- `SummaryLogFile`:

  Character. Relative file path.

------------------------------------------------------------------------

### `OriginalFire$write()`

Write extension inputs to disk

#### Usage

    OriginalFire$write()

------------------------------------------------------------------------

### `OriginalFire$clone()`

The objects of this class are cloneable with this method.

#### Usage

    OriginalFire$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
