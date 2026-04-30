# Construct a cohort selector for a Land Use Plus change block

Bundles a species code with one or more `(low, high, percent)` age-range
selections. Multiple ranges may be passed for the same species.

## Usage

``` r
cohortSelector(species, ranges)
```

## Arguments

- species:

  Character. Species code.

- ranges:

  `data.frame` with columns `low`, `high`, `percent`. `low` may equal
  `high` (single age) and `percent` is the proportion (0-100) of cohorts
  in that range that are affected.

## Value

A list of class `"CohortSelector"`.

## See also

Other Land Use Plus helpers:
[`LandUsePlus`](https://for-cast.github.io/landisutils/reference/LandUsePlus.md),
[`insertCohortSelector()`](https://for-cast.github.io/landisutils/reference/insertCohortSelector.md),
[`insertLandCoverChange()`](https://for-cast.github.io/landisutils/reference/insertLandCoverChange.md),
[`insertLandUseType()`](https://for-cast.github.io/landisutils/reference/insertLandUseType.md),
[`landCoverChange()`](https://for-cast.github.io/landisutils/reference/landCoverChange.md),
[`landUseType()`](https://for-cast.github.io/landisutils/reference/landUseType.md)

## Examples

``` r
cohortSelector(
  species = "querrubr",
  ranges = data.frame(low = c(1, 71), high = c(62, 200), percent = c(20, 25))
)
#> $species
#> [1] "querrubr"
#> 
#> $ranges
#>   low high percent
#> 1   1   62      20
#> 2  71  200      25
#> 
#> attr(,"class")
#> [1] "CohortSelector"
```
