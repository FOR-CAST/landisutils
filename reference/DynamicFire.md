# Dynamic Fire System Extension

Dynamic Fire System Extension

Dynamic Fire System Extension

## References

LANDIS-II Dynamic Fire System Extension v4 User Guide
<https://github.com/LANDIS-II-Foundation/Extension-Dynamic-Fire-System/blob/master/docs/LANDIS-II%20Dynamic%20Fire%20System%20v4%20User%20Guide.pdf>

## See also

Helpers that prepare inputs for this extension:
[`prepFireSizesTable()`](https://for-cast.github.io/landisutils/reference/prepFireSizesTable.md),
[`prepDynamicEcoregionTable()`](https://for-cast.github.io/landisutils/reference/prepDynamicEcoregionTable.md),
[`prepTopographyFile()`](https://for-cast.github.io/landisutils/reference/prepTopographyFile.md),
[`prepGroundSlopeFile()`](https://for-cast.github.io/landisutils/reference/prepTopographyFile.md),
[`prepUphillAzimuthMap()`](https://for-cast.github.io/landisutils/reference/prepTopographyFile.md),
[`prepInitialWeatherDatabase()`](https://for-cast.github.io/landisutils/reference/prepInitialWeatherDatabase.md),
[`prepDynamicWeatherTable()`](https://for-cast.github.io/landisutils/reference/prepDynamicWeatherTable.md).

Other Dynamic Fire helpers:
[`defaultFuelTypeTable()`](https://for-cast.github.io/landisutils/reference/defaultFuelTypeTable.md),
[`insertBuildUpIndex()`](https://for-cast.github.io/landisutils/reference/insertBuildUpIndex.md),
[`insertFireSizesTable()`](https://for-cast.github.io/landisutils/reference/insertFireSizesTable.md),
[`insertFuelTypeTable()`](https://for-cast.github.io/landisutils/reference/insertFuelTypeTable.md),
[`insertGroundSlopeFile()`](https://for-cast.github.io/landisutils/reference/insertGroundSlopeFile.md),
[`insertSeasonTable()`](https://for-cast.github.io/landisutils/reference/insertSeasonTable.md),
[`insertUphillSlopeAzimuthMap()`](https://for-cast.github.io/landisutils/reference/insertUphillSlopeAzimuthMap.md),
[`prepDynamicEcoregionTable()`](https://for-cast.github.io/landisutils/reference/prepDynamicEcoregionTable.md),
[`prepDynamicWeatherTable()`](https://for-cast.github.io/landisutils/reference/prepDynamicWeatherTable.md),
[`prepFireSizesTable()`](https://for-cast.github.io/landisutils/reference/prepFireSizesTable.md),
[`prepInitialWeatherDatabase()`](https://for-cast.github.io/landisutils/reference/prepInitialWeatherDatabase.md),
[`prepTopographyFile()`](https://for-cast.github.io/landisutils/reference/prepTopographyFile.md)

## Super class

[`landisutils::LandisExtension`](https://for-cast.github.io/landisutils/reference/LandisExtension.md)
-\> `DynamicFire`

## Active bindings

- `Species_CSV_File`:

  Character. Relative file path.

- `EventSizeType`:

  Character. One of "size_based" or "duration_based".

- `BuildUpIndex`:

  Logical, or character indicating "yes" or "no".

- `WeatherRandomizer`:

  (Optional) Integer `[0,4]` controlling the strength of the linkage
  between fire size/duration distribution and weather distribution.

- `FireSizesTable`:

  `data.frame`.

- `InitialFireEcoregionsMap`:

  Character. Relative file path.

- `DynamicEcoregionTable`:

  `data.frame`.

- `GroundSlopeFile`:

  Character. Relative file path.

- `UphillSlopeAzimuthMap`:

  Character. Relative file path.

- `SeasonTable`:

  `data.frame`.

- `InitialWeatherDatabase`:

  Character. Relative file path.

- `DynamicWeatherTable`:

  `data.frame`.

- `FuelTypeTable`:

  `data.frame`.

- `SeverityCalibrationFactor`:

  Numeric.

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

- [`DynamicFire$new()`](#method-DynamicFire-new)

- [`DynamicFire$write()`](#method-DynamicFire-write)

- [`DynamicFire$clone()`](#method-DynamicFire-clone)

Inherited methods

- [`landisutils::LandisExtension$add_file()`](https://for-cast.github.io/landisutils/reference/LandisExtension.html#method-add_file)

------------------------------------------------------------------------

### Method `new()`

#### Usage

    DynamicFire$new(
      path,
      Timestep = 10,
      Species_CSV_File = NULL,
      EventSizeType = NULL,
      BuildUpIndex = NULL,
      WeatherRandomizer = NULL,
      FireSizesTable = NULL,
      InitialFireEcoregionsMap = NULL,
      DynamicEcoregionTable = NULL,
      GroundSlopeFile = NULL,
      UphillSlopeAzimuthMap = NULL,
      SeasonTable = NULL,
      InitialWeatherDatabase = NULL,
      DynamicWeatherTable = NULL,
      FuelTypeTable = NULL,
      SeverityCalibrationFactor = NULL,
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

  Character. Relative file path to the per-species fire-coefficient CSV
  (must precede `EventSizeType` in the emitted config).

- `EventSizeType`:

  Character. One of "size_based" or "duration_based".

- `BuildUpIndex`:

  Logical, or character indicating "yes" or "no".

- `WeatherRandomizer`:

  Integer `[0, 4]`.

- `FireSizesTable`:

  `data.frame`.

- `InitialFireEcoregionsMap`:

  Character. Relative file path.

- `DynamicEcoregionTable`:

  `data.frame`.

- `GroundSlopeFile`:

  Character. Relative file path.

- `UphillSlopeAzimuthMap`:

  Character. Relative file path.

- `SeasonTable`:

  `data.frame`.

- `InitialWeatherDatabase`:

  Character. Relative file path.

- `DynamicWeatherTable`:

  `data.frame`.

- `FuelTypeTable`:

  `data.frame`.

- `SeverityCalibrationFactor`:

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

    DynamicFire$write()

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    DynamicFire$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.

## Examples

``` r
## see vignette for usage examples
```
