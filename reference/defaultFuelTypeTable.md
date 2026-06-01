# Create default Fuel Type Table

The Fuel Type Table specifies the rate of spread parameters for each
base surface type, following the Canadian Forest Fire Behavior
Prediction System (Forestry Canada, 1992). Default values from Tables 6,
7, 8 and Appendix 1.

## Usage

``` r
defaultFuelTypeTable()
```

## Value

`data.frame` with columns: `Index`, `Base`, `Surface`, `IgnProb`, `a`,
`b`, `c`, `q`, `BUI`, `maxBE` and `CBH`.

## References

Forestry Canada. 1992. Development and structure of the Canadian Forest
Fire Behavior Prediction System. Forestry Canada, Headquarters, Fire
Danger Group and Science and Sustainable Development Directorate,
Ottawa. Information Report ST-X-3. 64 p.
<https://ostrnrcan-dostrncan.canada.ca/handle/1845/235421>

Natural Resources Canada. 2019. Fire Behavior Prediction System Fuel
Type Descriptions.
<https://cwfis.cfs.nrcan.gc.ca/background/fueltypes/c1>

Wotton, B.M.; Alexander, M.E.; Taylor, S.W. 2009. Updates and revisions
to the 1992 Canadian Forest Fire Behavior Prediction System. Natural
Resources Canada, Canadian Forest Service, Great Lakes Forestry Centre,
Sault Ste. Marie, Ontario. Information Report GLC-X-10. 45 p.
<https://ostrnrcan-dostrncan.canada.ca/handle/1845/247839>

## See also

Other Dynamic Fire helpers:
[`DynamicFire`](https://for-cast.github.io/landisutils/reference/DynamicFire.md),
[`apply_calibrated_hi_prop()`](https://for-cast.github.io/landisutils/reference/apply_calibrated_hi_prop.md),
[`apply_calibrated_ignprob()`](https://for-cast.github.io/landisutils/reference/apply_calibrated_ignprob.md),
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
