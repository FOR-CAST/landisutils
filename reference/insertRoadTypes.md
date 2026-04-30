# Specify the `RoadTypes` table for the Forest Roads extension

Column emission depends on whether the parent extension's
`SimulationOfWoodFlux` and `SimulationOfRoadAging` are enabled.

## Usage

``` r
insertRoadTypes(roads, includeFlux, includeAge)
```

## Arguments

- roads:

  List of `RoadType` objects.

- includeFlux:

  Logical.

- includeAge:

  Logical.

## Value

Character string(s) to write to the suitable LANDIS-II input file.

## See also

Other Forest Roads Simulation helpers:
[`ForestRoadsSimulation`](https://for-cast.github.io/landisutils/reference/ForestRoadsSimulation.md),
[`elevationCostRange()`](https://for-cast.github.io/landisutils/reference/elevationCostRange.md),
[`exitRoadType()`](https://for-cast.github.io/landisutils/reference/exitRoadType.md),
[`insertElevationCosts()`](https://for-cast.github.io/landisutils/reference/insertElevationCosts.md),
[`insertExitRoadTypes()`](https://for-cast.github.io/landisutils/reference/insertExitRoadTypes.md),
[`roadType()`](https://for-cast.github.io/landisutils/reference/roadType.md)
