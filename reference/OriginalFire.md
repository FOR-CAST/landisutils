# Original Fire Extension

Original Fire Extension

Original Fire Extension

## References

LANDIS-II Original Fire v5.0 Extension User Guide
<https://github.com/LANDIS-II-Foundation/Extension-Base-Fire/blob/master/docs/LANDIS-II%20Original%20Fire%20v5.0%20User%20Guide.pdf>

## Super class

[`landisutils::LandisExtension`](https://for-cast.github.io/landisutils/reference/LandisExtension.md)
-\> `OriginalFire`

## Active bindings

- `Species_CSV_File`:

  Character. Relative file path.

- `FireRegionParametersTable`:

  `data.frame`.

- `InitialFireRegionsMap`:

  Character. Relative file path.

- `DynamicFireRegionsTable`:

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

- [`OriginalFire$new()`](#method-OriginalFire-new)

- [`OriginalFire$write()`](#method-OriginalFire-write)

- [`OriginalFire$clone()`](#method-OriginalFire-clone)

Inherited methods

- [`landisutils::LandisExtension$add_file()`](https://for-cast.github.io/landisutils/reference/LandisExtension.html#method-add_file)

------------------------------------------------------------------------

### Method `new()`

#### Usage

    OriginalFire$new(
      path,
      Timestep = NULL,
      Species_CSV_File = NULL,
      FireRegionParametersTable = NULL,
      InitialFireRegionsMap = NULL,
      DynamicFireRegionsTable = NULL,
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

- `DynamicFireRegionsTable`:

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

### Method [`write()`](https://rdrr.io/r/base/write.html)

Write extension inputs to disk

#### Usage

    OriginalFire$write()

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    OriginalFire$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
