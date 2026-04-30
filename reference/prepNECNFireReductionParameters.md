# Prepare NECN Succession `FireReductionParameters` table

Defaults adapted from the LANDIS-II NECN Succession v8 user guide; users
should tune to their study system.

## Usage

``` r
prepNECNFireReductionParameters(df = NULL)
```

## Arguments

- df:

  (Optional) `data.frame` with columns `FireSeverity`,
  `CoarseDebrisReduction`, `FineLitterReduction`, `CohortWoodReduction`,
  `CohortLeafReduction`, and `SOMReduction`. When `NULL`, a sensible
  default table with 5 fire-severity classes is returned.

## Value

data.frame

## See also

Other NECN Succession helpers:
[`NECNSuccession`](https://for-cast.github.io/landisutils/reference/NECNSuccession.md),
[`insertNECNFireReductionParameters()`](https://for-cast.github.io/landisutils/reference/insertNECNFireReductionParameters.md),
[`insertNECNHarvestReductionParameters()`](https://for-cast.github.io/landisutils/reference/insertNECNHarvestReductionParameters.md)
