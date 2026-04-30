# Construct a Land Cover Change for a [`landUseType()`](https://for-cast.github.io/landisutils/reference/landUseType.md)

One `landCoverChange()` describes what happens to existing tree cohorts
on a site that transitions to (or remains in) a particular land use.

## Usage

``` r
landCoverChange(type, repeatHarvest = FALSE, cohorts = NULL)
```

## Arguments

- type:

  Character. One of `"NoChange"`, `"RemoveTrees"`,
  `"InsectDefoliation"`.

- repeatHarvest:

  Logical (or `"yes"`/`"no"`). When `TRUE`, the removal/defoliation is
  re-applied each timestep.

- cohorts:

  (Required for `"RemoveTrees"` / `"InsectDefoliation"`) List of
  `CohortSelector` objects (see
  [`cohortSelector()`](https://for-cast.github.io/landisutils/reference/cohortSelector.md)).

## Value

A list of class `"LandCoverChange"`.

## See also

Other Land Use Plus helpers:
[`LandUsePlus`](https://for-cast.github.io/landisutils/reference/LandUsePlus.md),
[`cohortSelector()`](https://for-cast.github.io/landisutils/reference/cohortSelector.md),
[`insertCohortSelector()`](https://for-cast.github.io/landisutils/reference/insertCohortSelector.md),
[`insertLandCoverChange()`](https://for-cast.github.io/landisutils/reference/insertLandCoverChange.md),
[`insertLandUseType()`](https://for-cast.github.io/landisutils/reference/insertLandUseType.md),
[`landUseType()`](https://for-cast.github.io/landisutils/reference/landUseType.md)
