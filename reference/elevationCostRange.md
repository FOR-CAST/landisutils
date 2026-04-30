# Construct an elevation cost-range entry

Construct an elevation cost-range entry

## Usage

``` r
elevationCostRange(lower, upper, value)
```

## Arguments

- lower, upper:

  Numeric. Inclusive elevation thresholds (m).

- value:

  Numeric. Additive (for `CoarseElevationCosts`) or multiplicative (for
  `FineElevationCosts`) cost.

## Value

A list of class `"ElevationCostRange"`.

## See also

Other Forest Roads Simulation helpers:
[`ForestRoadsSimulation`](https://for-cast.github.io/landisutils/reference/ForestRoadsSimulation.md),
[`exitRoadType()`](https://for-cast.github.io/landisutils/reference/exitRoadType.md),
[`insertElevationCosts()`](https://for-cast.github.io/landisutils/reference/insertElevationCosts.md),
[`insertExitRoadTypes()`](https://for-cast.github.io/landisutils/reference/insertExitRoadTypes.md),
[`insertRoadTypes()`](https://for-cast.github.io/landisutils/reference/insertRoadTypes.md),
[`roadType()`](https://for-cast.github.io/landisutils/reference/roadType.md)
