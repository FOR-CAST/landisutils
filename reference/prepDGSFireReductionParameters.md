# Prepare DGS Succession `FireReductionParameters` table

Defaults adapted from the LANDIS-II DGS Succession v2.0 user guide;
users should tune to their study system.

## Usage

``` r
prepDGSFireReductionParameters(df = NULL)
```

## Arguments

- df:

  (Optional) `data.frame` with columns `FireSeverity`,
  `CoarseDebrisReduction`, `FineLitterReduction`, `CohortWoodReduction`,
  `CohortLeafReduction`, and `OrganicHorizonReduction`. When `NULL`, a
  default table with 5 fire-severity classes is returned.

## Value

data.frame

## See also

Other DGS Succession helpers:
[`DGSSuccession`](https://for-cast.github.io/landisutils/reference/DGSSuccession.md),
[`defaultDGSDammMcNIPParameters()`](https://for-cast.github.io/landisutils/reference/defaultDGSDammMcNIPParameters.md),
[`insertDGSFireReductionParameters()`](https://for-cast.github.io/landisutils/reference/insertDGSFireReductionParameters.md),
[`insertDGSHarvestReductionParameters()`](https://for-cast.github.io/landisutils/reference/insertDGSHarvestReductionParameters.md)
