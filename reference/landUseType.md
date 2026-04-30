# Construct a Land Use type

Defines one land use entry for
[LandUsePlus](https://for-cast.github.io/landisutils/reference/LandUsePlus.md).
Each land use has a unique `name` and `mapCode`, an `allowHarvest` flag,
an optional `preventEstablishment` flag, and one or more
[`landCoverChange()`](https://for-cast.github.io/landisutils/reference/landCoverChange.md)
blocks describing what happens to existing trees.

## Usage

``` r
landUseType(
  name,
  mapCode,
  allowHarvest = TRUE,
  preventEstablishment = FALSE,
  changes = list(),
  plant = NULL
)
```

## Arguments

- name:

  Character. Land-use name (no internal whitespace; quoted automatically
  when written).

- mapCode:

  Integer. Numeric code matching the input raster.

- allowHarvest:

  Logical (or `"yes"`/`"no"`). Whether harvesting is permitted. Defaults
  to `TRUE` to match the LU+ v4 user-guide default.

- preventEstablishment:

  Logical. When `TRUE`, the keyword `PreventEstablishment` is emitted
  (which blocks tree establishment).

- changes:

  List of `LandCoverChange` objects (see
  [`landCoverChange()`](https://for-cast.github.io/landisutils/reference/landCoverChange.md)).

- plant:

  (Optional) Character. Species code planted after change.

## Value

A list of class `"LandUseType"`.

## See also

Other Land Use Plus helpers:
[`LandUsePlus`](https://for-cast.github.io/landisutils/reference/LandUsePlus.md),
[`cohortSelector()`](https://for-cast.github.io/landisutils/reference/cohortSelector.md),
[`insertCohortSelector()`](https://for-cast.github.io/landisutils/reference/insertCohortSelector.md),
[`insertLandCoverChange()`](https://for-cast.github.io/landisutils/reference/insertLandCoverChange.md),
[`insertLandUseType()`](https://for-cast.github.io/landisutils/reference/insertLandUseType.md),
[`landCoverChange()`](https://for-cast.github.io/landisutils/reference/landCoverChange.md)
