# Prepare topographical raster files

- `prepGroundSlopeFile` produces a raster of slope (degrees);

- `prepUphillAzimuthMap` produces a raster of slope direction (degrees);

## Usage

``` r
prepTopographyFile(aoi, type, path = ".", filename = NULL)

prepGroundSlopeFile(aoi = NULL, path = ".", filename = "ground_slope.tif")

prepUphillAzimuthMap(
  aoi = NULL,
  path = ".",
  filename = "uphill_slope_azimuth.tif"
)
```

## Arguments

- aoi:

  `sf` polygon, `SpatVector` or `SpatRaster` object defining the area of
  interest.

- type:

  Character. One of the allowed types for `v` in
  [terra::terrain](https://rspatial.github.io/terra/reference/terrain.html),
  e.g., "slope" or "aspect".

- path:

  Character. Path specifying a directory to use for the scenario runs.

- filename:

  Character specifying the output filename.

## Value

Character file path.

## See also

Used by
[DynamicFire](https://for-cast.github.io/landisutils/reference/DynamicFire.md)
and
[SocialClimateFire](https://for-cast.github.io/landisutils/reference/SocialClimateFire.md).

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
[`prepInitialWeatherDatabase()`](https://for-cast.github.io/landisutils/reference/prepInitialWeatherDatabase.md)

Other Social Climate Fire helpers:
[`SocialClimateFire`](https://for-cast.github.io/landisutils/reference/SocialClimateFire.md),
[`assemble_climate_library_file_scf()`](https://for-cast.github.io/landisutils/reference/assemble_climate_library_file_scf.md),
[`insertDeadWoodTable()`](https://for-cast.github.io/landisutils/reference/insertDeadWoodTable.md),
[`insertLadderFuelSpeciesList()`](https://for-cast.github.io/landisutils/reference/insertLadderFuelSpeciesList.md),
[`prepSuppression_CSV_File()`](https://for-cast.github.io/landisutils/reference/prepSuppression_CSV_File.md)
