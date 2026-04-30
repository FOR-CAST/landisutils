# Create Original Fire `InitialFireRegionsMap`

Create Original Fire `InitialFireRegionsMap`

## Usage

``` r
prepInitialFireRegionsMap(r, file = "fire-regions-map.tif")
```

## Arguments

- r:

  `SpatRaster` corresponding to initial fire regions map

- file:

  Character, specifying the path to the file.

## Value

Character string(s) specifying suitable LANDIS-II input file(s), created
as a side effect of the function.

## See also

Other Original Fire helpers:
[`OriginalFire`](https://for-cast.github.io/landisutils/reference/OriginalFire.md),
[`defaultFuelCurveTable()`](https://for-cast.github.io/landisutils/reference/defaultFuelCurveTable.md),
[`insertFireRegionParametersTable()`](https://for-cast.github.io/landisutils/reference/insertFireRegionParametersTable.md),
[`insertFuelCurveTable()`](https://for-cast.github.io/landisutils/reference/insertFuelCurveTable.md),
[`insertWindCurveTable()`](https://for-cast.github.io/landisutils/reference/insertWindCurveTable.md),
[`prepFireRegionParametersTable()`](https://for-cast.github.io/landisutils/reference/prepFireRegionParametersTable.md)
