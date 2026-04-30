# Create sample Fuel Curve Table

Create sample Fuel Curve Table

## Usage

``` r
defaultFuelCurveTable(frp)
```

## Arguments

- frp:

  `data.frame` corresponding to `FireRegionParametersTable` (see
  [`prepFireRegionParametersTable()`](https://for-cast.github.io/landisutils/reference/prepFireRegionParametersTable.md)).

## Value

`data.frame` with 5 columns: `FireRegimeName`, `S1`, `S2`, `S3`, `S4`,
`S5`.

## See also

Other Original Fire helpers:
[`OriginalFire`](https://for-cast.github.io/landisutils/reference/OriginalFire.md),
[`insertFireRegionParametersTable()`](https://for-cast.github.io/landisutils/reference/insertFireRegionParametersTable.md),
[`insertFuelCurveTable()`](https://for-cast.github.io/landisutils/reference/insertFuelCurveTable.md),
[`insertWindCurveTable()`](https://for-cast.github.io/landisutils/reference/insertWindCurveTable.md),
[`prepFireRegionParametersTable()`](https://for-cast.github.io/landisutils/reference/prepFireRegionParametersTable.md),
[`prepInitialFireRegionsMap()`](https://for-cast.github.io/landisutils/reference/prepInitialFireRegionsMap.md)
