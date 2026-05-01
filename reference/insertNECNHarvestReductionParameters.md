# Specify NECN Succession `HarvestReductionParameters` table

Specify NECN Succession `HarvestReductionParameters` table

## Usage

``` r
insertNECNHarvestReductionParameters(df)
```

## Arguments

- df:

  `data.frame` with columns `PrescriptionName`, `DeadWoodReduction`,
  `DeadLitterReduction`, `SOMReduction`, `CohortWoodRemoval`,
  `CohortLeafRemoval`. `SOMReduction` is required by the runtime NECN
  parser even though the v8 user guide §2.35 documents only the other
  four numeric columns.

## Value

Character string(s) to write to the suitable LANDIS-II input file.

## See also

Other NECN Succession helpers:
[`NECNSuccession`](https://for-cast.github.io/landisutils/reference/NECNSuccession.md),
[`insertNECNFireReductionParameters()`](https://for-cast.github.io/landisutils/reference/insertNECNFireReductionParameters.md),
[`prepNECNFireReductionParameters()`](https://for-cast.github.io/landisutils/reference/prepNECNFireReductionParameters.md)
