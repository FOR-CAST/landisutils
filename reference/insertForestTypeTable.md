# Specify a `ForestTypeTable` for a Biomass Harvest prescription

Specify a `ForestTypeTable` for a Biomass Harvest prescription

## Usage

``` r
insertForestTypeTable(df)
```

## Arguments

- df:

  (Optional) `data.frame` with columns `InclusionRule`, `AgeRange`,
  `PercentCells`, `Species`. When `NULL`, nothing is written.

## Value

Character string(s) to write to the suitable LANDIS-II input file.

## See also

Other Biomass Harvest helpers:
[`BiomassHarvest`](https://for-cast.github.io/landisutils/reference/BiomassHarvest.md),
[`harvestPrescription()`](https://for-cast.github.io/landisutils/reference/harvestPrescription.md),
[`insertCohortsRemoved()`](https://for-cast.github.io/landisutils/reference/insertCohortsRemoved.md),
[`insertEconomicRankTable()`](https://for-cast.github.io/landisutils/reference/insertEconomicRankTable.md),
[`insertFireHazardTable()`](https://for-cast.github.io/landisutils/reference/insertFireHazardTable.md),
[`insertHarvestImplementations()`](https://for-cast.github.io/landisutils/reference/insertHarvestImplementations.md),
[`insertMultipleRepeat()`](https://for-cast.github.io/landisutils/reference/insertMultipleRepeat.md),
[`insertPlant()`](https://for-cast.github.io/landisutils/reference/insertPlant.md),
[`insertPrescription()`](https://for-cast.github.io/landisutils/reference/insertPrescription.md),
[`insertSingleRepeat()`](https://for-cast.github.io/landisutils/reference/insertSingleRepeat.md),
[`insertSiteSelection()`](https://for-cast.github.io/landisutils/reference/insertSiteSelection.md),
[`insertStandRanking()`](https://for-cast.github.io/landisutils/reference/insertStandRanking.md)
