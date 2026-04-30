# Construct a non-exit road type

Construct a non-exit road type

## Usage

``` r
roadType(
  id,
  name,
  costMultiplier,
  fluxMin = NULL,
  fluxMax = NULL,
  maxAge = NULL
)
```

## Arguments

- id:

  Integer. Road type ID.

- name:

  Character. Road type name (no whitespace).

- costMultiplier:

  Numeric. Multiplicative cost value.

- fluxMin, fluxMax:

  Numeric. Required when the parent
  [ForestRoadsSimulation](https://for-cast.github.io/landisutils/reference/ForestRoadsSimulation.md)'s
  `SimulationOfWoodFlux` is enabled.

- maxAge:

  Integer. Required when the parent's `SimulationOfRoadAging` is
  enabled.

## Value

A list of class `"RoadType"`.

## See also

Other Forest Roads Simulation helpers:
[`ForestRoadsSimulation`](https://for-cast.github.io/landisutils/reference/ForestRoadsSimulation.md),
[`elevationCostRange()`](https://for-cast.github.io/landisutils/reference/elevationCostRange.md),
[`exitRoadType()`](https://for-cast.github.io/landisutils/reference/exitRoadType.md),
[`insertElevationCosts()`](https://for-cast.github.io/landisutils/reference/insertElevationCosts.md),
[`insertExitRoadTypes()`](https://for-cast.github.io/landisutils/reference/insertExitRoadTypes.md),
[`insertRoadTypes()`](https://for-cast.github.io/landisutils/reference/insertRoadTypes.md)
