# Specify a `*ElevationCosts` table for the Forest Roads extension

Specify a `*ElevationCosts` table for the Forest Roads extension

## Usage

``` r
insertElevationCosts(name, ranges, valueLabel)
```

## Arguments

- name:

  Character. Either `"CoarseElevationCosts"` or `"FineElevationCosts"`.

- ranges:

  List of `ElevationCostRange` objects.

- valueLabel:

  Character. Header label for the third column (`"Additional"` or
  `"Multiplication"`).

## Value

Character string(s) to write to the suitable LANDIS-II input file.

## See also

Other Forest Roads Simulation helpers:
[`ForestRoadsSimulation`](https://for-cast.github.io/landisutils/reference/ForestRoadsSimulation.md),
[`elevationCostRange()`](https://for-cast.github.io/landisutils/reference/elevationCostRange.md),
[`exitRoadType()`](https://for-cast.github.io/landisutils/reference/exitRoadType.md),
[`insertExitRoadTypes()`](https://for-cast.github.io/landisutils/reference/insertExitRoadTypes.md),
[`insertRoadTypes()`](https://for-cast.github.io/landisutils/reference/insertRoadTypes.md),
[`roadType()`](https://for-cast.github.io/landisutils/reference/roadType.md)
