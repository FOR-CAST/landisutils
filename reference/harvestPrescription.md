# Construct a Biomass Harvest prescription

Returns a validated prescription specification. One or more of these is
passed to
[BiomassHarvest](https://for-cast.github.io/landisutils/reference/BiomassHarvest.md)
via `Prescriptions`.

## Usage

``` r
harvestPrescription(
  name,
  StandRanking = NULL,
  EconomicRankTable = NULL,
  FireHazardTable = NULL,
  TimeSinceLastFire = NULL,
  TimeSinceLastWind = NULL,
  MinimumAge = NULL,
  MaximumAge = NULL,
  StandAdjacency = NULL,
  AdjacencyType = NULL,
  AdjacencyNeighborSetAside = NULL,
  MinimumTimeSinceLastHarvest = NULL,
  ForestTypeTable = NULL,
  PresalvageYears = NULL,
  SiteSelection = NULL,
  MinTargetSize = NULL,
  MaxTargetSize = NULL,
  PatchPercentage = NULL,
  PatchSize = NULL,
  AllowOverlap = NULL,
  RepeatExactCells = NULL,
  MinTimeSinceDamage = NULL,
  PreventEstablishment = FALSE,
  CohortsRemoved = NULL,
  SpeciesList = NULL,
  Plant = NULL,
  SingleRepeat = NULL,
  SingleRepeatCohortsRemoved = NULL,
  SingleRepeatSpeciesList = NULL,
  SingleRepeatPlant = NULL,
  MultipleRepeat = NULL,
  TimesToRepeat = NULL
)
```

## Arguments

- name:

  Character. Prescription name (no spaces); referenced from
  `HarvestImplementations`.

- StandRanking:

  Character. One of `"Economic"`, `"MaxCohortAge"`, `"RegulateAges"`,
  `"Random"`, `"FireHazard"`, or `"TimeSinceDisturbance"`.

- EconomicRankTable:

  `data.frame` with columns `Species`, `EconomicRank`, `MinimumAge`.
  Required when `StandRanking = "Economic"`.

- FireHazardTable:

  `data.frame` with columns `FuelType`, `FuelTypeRank`. Required when
  `StandRanking = "FireHazard"`.

- TimeSinceLastFire, TimeSinceLastWind:

  Integer. Years. Exactly one is required when
  `StandRanking = "TimeSinceDisturbance"`.

- MinimumAge, MaximumAge:

  (Optional) Integer years.

- StandAdjacency:

  (Optional) Integer. Years.

- AdjacencyType:

  (Optional) Character. One of `"StandAge"` or
  `"TimeSinceLastHarvested"`.

- AdjacencyNeighborSetAside:

  (Optional) Integer. Years.

- MinimumTimeSinceLastHarvest:

  (Optional) Integer. Years.

- ForestTypeTable:

  (Optional) `data.frame` with columns `InclusionRule`, `AgeRange`,
  `PercentCells`, `Species`. Multiple species may be space-separated in
  the `Species` column.

- PresalvageYears:

  (Optional) Integer. Years.

- SiteSelection:

  Character. One of `"Complete"`, `"CompleteStandSpread"`,
  `"PartialStandSpread"`, or `"PatchCutting"`.

- MinTargetSize, MaxTargetSize:

  Numeric (hectares). Required when `SiteSelection` is
  `"CompleteStandSpread"` or `"PartialStandSpread"`.

- PatchPercentage:

  Numeric in (0, 100\]. Required when `SiteSelection = "PatchCutting"`.

- PatchSize:

  Numeric (hectares). Required when `SiteSelection = "PatchCutting"`.

- AllowOverlap, RepeatExactCells:

  (Optional) Logical. Only applies to `SiteSelection = "PatchCutting"`.

- MinTimeSinceDamage:

  (Optional) Integer.

- PreventEstablishment:

  Logical. If `TRUE`, emits the bare `PreventEstablishment` keyword.

- CohortsRemoved:

  Character. One of `"ClearCut"`, `"PlantOnly"`, or `"SpeciesList"`.

- SpeciesList:

  `data.frame` with columns `Species` and `Cohorts`. Required when
  `CohortsRemoved = "SpeciesList"`.

- Plant:

  (Optional) Character vector of species codes to plant after harvest.

- SingleRepeat:

  (Optional) Integer. Interval (years) for a single-repeat harvest.

- SingleRepeatCohortsRemoved:

  (Optional) Character. `CohortsRemoved` method to use for the repeat
  entry; required when `SingleRepeat` is set.

- SingleRepeatSpeciesList:

  (Optional) `data.frame` like `SpeciesList`, used when
  `SingleRepeatCohortsRemoved = "SpeciesList"`.

- SingleRepeatPlant:

  (Optional) Character vector of species to plant on the repeat entry.

- MultipleRepeat:

  (Optional) Integer. Interval (years) for a multiple-repeat harvest.

- TimesToRepeat:

  (Optional) Integer. Caps the number of multiple-repeat iterations.

## Value

A list of class `"HarvestPrescription"`.

## See also

Other Biomass Harvest helpers:
[`BiomassHarvest`](https://for-cast.github.io/landisutils/reference/BiomassHarvest.md),
[`insertCohortsRemoved()`](https://for-cast.github.io/landisutils/reference/insertCohortsRemoved.md),
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
