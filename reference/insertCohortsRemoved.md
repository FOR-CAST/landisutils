# Specify the `CohortsRemoved` portion of a Biomass Harvest prescription

Specify the `CohortsRemoved` portion of a Biomass Harvest prescription

## Usage

``` r
insertCohortsRemoved(method, species_list = NULL)
```

## Arguments

- method:

  Character. One of `"ClearCut"`, `"PlantOnly"`, or `"SpeciesList"`.

- species_list:

  (Optional) `data.frame` with columns `Species` and `Cohorts`. Required
  when `method = "SpeciesList"`.

## Value

Character string(s) to write to the suitable LANDIS-II input file.

## See also

Other Biomass Harvest helpers:
[`BiomassHarvest`](https://for-cast.github.io/landisutils/reference/BiomassHarvest.md),
[`harvestPrescription()`](https://for-cast.github.io/landisutils/reference/harvestPrescription.md),
[`insertEconomicRankTable()`](https://for-cast.github.io/landisutils/reference/insertEconomicRankTable.md),
[`insertFireHazardTable()`](https://for-cast.github.io/landisutils/reference/insertFireHazardTable.md),
[`insertForestTypeTable()`](https://for-cast.github.io/landisutils/reference/insertForestTypeTable.md),
[`insertHarvestImplementations()`](https://for-cast.github.io/landisutils/reference/insertHarvestImplementations.md),
[`insertMultipleRepeat()`](https://for-cast.github.io/landisutils/reference/insertMultipleRepeat.md),
[`insertPlant()`](https://for-cast.github.io/landisutils/reference/insertPlant.md),
[`insertPrescription()`](https://for-cast.github.io/landisutils/reference/insertPrescription.md),
[`insertSingleRepeat()`](https://for-cast.github.io/landisutils/reference/insertSingleRepeat.md),
[`insertSiteSelection()`](https://for-cast.github.io/landisutils/reference/insertSiteSelection.md),
[`insertStandRanking()`](https://for-cast.github.io/landisutils/reference/insertStandRanking.md)
