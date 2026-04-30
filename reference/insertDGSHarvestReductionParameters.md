# Specify DGS Succession `HarvestReductionParameters` table

The DGS parser reads six columns per row: prescription name, coarse
(dead) wood reduction, fine (dead) litter reduction, SOM reduction,
cohort wood removal, and cohort leaf removal. The `SOMReduction` column
is required even though it is not listed in user-guide §2.56.

## Usage

``` r
insertDGSHarvestReductionParameters(df)
```

## Arguments

- df:

  `data.frame` with columns `PrescriptionName`, `DeadWoodReduction`,
  `DeadLitterReduction`, `SOMReduction`, `CohortWoodRemoval`,
  `CohortLeafRemoval`.

## Value

Character string(s) to write to the suitable LANDIS-II input file.

## See also

Other DGS Succession helpers:
[`DGSSuccession`](https://for-cast.github.io/landisutils/reference/DGSSuccession.md),
[`defaultDGSDammMcNIPParameters()`](https://for-cast.github.io/landisutils/reference/defaultDGSDammMcNIPParameters.md),
[`insertDGSFireReductionParameters()`](https://for-cast.github.io/landisutils/reference/insertDGSFireReductionParameters.md),
[`prepDGSFireReductionParameters()`](https://for-cast.github.io/landisutils/reference/prepDGSFireReductionParameters.md)
