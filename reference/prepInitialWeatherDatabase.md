# Create Dynamic Fire Extension's Initial Weather Database

Create Dynamic Fire Extension's Initial Weather Database

## Usage

``` r
prepInitialWeatherDatabase(df, path, filename = "initial-weather-database.csv")
```

## Arguments

- df:

  data.frame

- path:

  Character. Path specifying a directory to use for the scenario runs.

- filename:

  Character specifying the output filename.

## Value

Character string(s) specifying suitable LANDIS-II input file(s), created
as a side effect of the function.

## See also

Other Dynamic Fire helpers:
[`DynamicFire`](https://for-cast.github.io/landisutils/reference/DynamicFire.md),
[`apply_calibrated_hi_prop()`](https://for-cast.github.io/landisutils/reference/apply_calibrated_hi_prop.md),
[`apply_calibrated_ignprob()`](https://for-cast.github.io/landisutils/reference/apply_calibrated_ignprob.md),
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
[`prepTopographyFile()`](https://for-cast.github.io/landisutils/reference/prepTopographyFile.md)
