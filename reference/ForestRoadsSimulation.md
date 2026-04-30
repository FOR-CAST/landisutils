# Forest Roads Simulation Extension

Forest Roads Simulation Extension

Forest Roads Simulation Extension

## References

LANDIS-II Forest Roads Simulation v2 Extension User Guide
<https://github.com/Klemet/LANDIS-II-Forest-Roads-Simulation-extension/blob/master/docs/LANDIS-II%20Forest%20Roads%20Simulation%20v2.0%20User%20Guide.pdf>

## See also

Other Forest Roads Simulation helpers:
[`elevationCostRange()`](https://for-cast.github.io/landisutils/reference/elevationCostRange.md),
[`exitRoadType()`](https://for-cast.github.io/landisutils/reference/exitRoadType.md),
[`insertElevationCosts()`](https://for-cast.github.io/landisutils/reference/insertElevationCosts.md),
[`insertExitRoadTypes()`](https://for-cast.github.io/landisutils/reference/insertExitRoadTypes.md),
[`insertRoadTypes()`](https://for-cast.github.io/landisutils/reference/insertRoadTypes.md),
[`roadType()`](https://for-cast.github.io/landisutils/reference/roadType.md)

## Super class

[`landisutils::LandisExtension`](https://for-cast.github.io/landisutils/reference/LandisExtension.md)
-\> `ForestRoadsSimulation`

## Active bindings

- `HeuristicForNetworkConstruction`:

  Character. One of
  [.frsHeuristics](https://for-cast.github.io/landisutils/reference/dot-frsHeuristics.md);
  matched case-insensitively, stored in canonical user-guide form.

- `SkiddingDistance`:

  Numeric.

- `LoopingBehavior`:

  Character `"yes"`/`"no"`.

- `LoopingMinDistance`:

  Numeric.

- `LoopingMaxDistance`:

  Numeric.

- `LoopingMaxPercentageOfRoads`:

  Numeric.

- `LoopingMaxCost`:

  Numeric.

- `LoopingProbability`:

  Numeric.

- `OutputsOfRoadNetworkMaps`:

  Character.

- `OutputsOfRoadLog`:

  Character.

- `RasterOfBuildableZones`:

  Character.

- `InitialRoadNetworkMap`:

  Character.

- `DistanceCost`:

  Numeric.

- `CoarseElevationRaster`:

  Character.

- `CoarseElevationCosts`:

  List of `ElevationCostRange` objects.

- `FineElevationRaster`:

  Character.

- `FineElevationCosts`:

  List of `ElevationCostRange` objects.

- `CoarseWaterRaster`:

  Character.

- `CoarseWaterCost`:

  Numeric.

- `FineWaterRaster`:

  Character.

- `FineWaterCost`:

  Numeric.

- `SoilsRaster`:

  Character.

- `SimulationOfRoadAging`:

  Character `"yes"`/`"no"`.

- `SimulationOfWoodFlux`:

  Character `"yes"`/`"no"`.

- `RoadTypes`:

  List of `RoadType` objects.

- `RoadTypesForExitingWood`:

  List of `ExitRoadType` objects.

## Methods

### Public methods

- [`ForestRoadsSimulation$new()`](#method-ForestRoadsSimulation-new)

- [`ForestRoadsSimulation$write()`](#method-ForestRoadsSimulation-write)

- [`ForestRoadsSimulation$clone()`](#method-ForestRoadsSimulation-clone)

Inherited methods

- [`landisutils::LandisExtension$add_file()`](https://for-cast.github.io/landisutils/reference/LandisExtension.html#method-add_file)

------------------------------------------------------------------------

### Method `new()`

#### Usage

    ForestRoadsSimulation$new(
      path,
      Timestep = NULL,
      HeuristicForNetworkConstruction = "ClosestFirst",
      SkiddingDistance = NULL,
      LoopingBehavior = FALSE,
      LoopingMinDistance = NULL,
      LoopingMaxDistance = NULL,
      LoopingMaxPercentageOfRoads = NULL,
      LoopingMaxCost = NULL,
      LoopingProbability = NULL,
      OutputsOfRoadNetworkMaps = NULL,
      OutputsOfRoadLog = NULL,
      RasterOfBuildableZones = NULL,
      InitialRoadNetworkMap = NULL,
      DistanceCost = NULL,
      CoarseElevationRaster = NULL,
      CoarseElevationCosts = list(),
      FineElevationRaster = NULL,
      FineElevationCosts = NULL,
      CoarseWaterRaster = NULL,
      CoarseWaterCost = NULL,
      FineWaterRaster = NULL,
      FineWaterCost = NULL,
      SoilsRaster = NULL,
      SimulationOfRoadAging = FALSE,
      SimulationOfWoodFlux = FALSE,
      RoadTypes = list(),
      RoadTypesForExitingWood = list()
    )

#### Arguments

- `path`:

  Character. Directory path.

- `Timestep`:

  Integer. Years between road-construction cycles.

- `HeuristicForNetworkConstruction`:

  Character. One of `"Random"`, `"ClosestFirst"`, `"FarthestFirst"`
  (case-insensitive). Normalized on write to the form expected by the
  upstream parser (`"Closestfirst"`, `"Farthestfirst"`).

- `SkiddingDistance`:

  Numeric. Distance (m) wood can be skidded before requiring a road.

- `LoopingBehavior`:

  Logical (or `"yes"`/`"no"`). Whether road looping is enabled.

- `LoopingMinDistance, LoopingMaxDistance, LoopingMaxPercentageOfRoads, LoopingMaxCost, LoopingProbability`:

  Numeric. Required when `LoopingBehavior` is truthy.

- `OutputsOfRoadNetworkMaps`:

  Character. Output path for road-network raster maps.

- `OutputsOfRoadLog`:

  Character. Output directory for road-construction logs.

- `RasterOfBuildableZones`:

  Character. Input raster path defining buildable zones.

- `InitialRoadNetworkMap`:

  Character. Input raster path with the initial road network.

- `DistanceCost`:

  Numeric. Base cost per unit distance.

- `CoarseElevationRaster`:

  Character. Coarse-elevation raster path.

- `CoarseElevationCosts`:

  List of `ElevationCostRange` objects (see
  [`elevationCostRange()`](https://for-cast.github.io/landisutils/reference/elevationCostRange.md)).

- `FineElevationRaster`:

  (Optional) Character. Fine-elevation raster path; pass `NULL` or
  `"none"` to disable.

- `FineElevationCosts`:

  (Optional) List of `ElevationCostRange` objects; required when
  `FineElevationRaster` is set.

- `CoarseWaterRaster`:

  (Optional) Character. Coarse-water raster path.

- `CoarseWaterCost`:

  (Optional) Numeric. Required when `CoarseWaterRaster` is set.

- `FineWaterRaster`:

  (Optional) Character. Fine-water raster path.

- `FineWaterCost`:

  (Optional) Numeric. Required when `FineWaterRaster` is set.

- `SoilsRaster`:

  (Optional) Character. Soils raster path.

- `SimulationOfRoadAging`:

  Logical (or `"yes"`/`"no"`).

- `SimulationOfWoodFlux`:

  Logical (or `"yes"`/`"no"`).

- `RoadTypes`:

  List of `RoadType` objects (see
  [`roadType()`](https://for-cast.github.io/landisutils/reference/roadType.md)).

- `RoadTypesForExitingWood`:

  List of `ExitRoadType` objects (see
  [`exitRoadType()`](https://for-cast.github.io/landisutils/reference/exitRoadType.md)).

------------------------------------------------------------------------

### Method [`write()`](https://rdrr.io/r/base/write.html)

Write extension inputs to disk

#### Usage

    ForestRoadsSimulation$write()

------------------------------------------------------------------------

### Method `clone()`

The objects of this class are cloneable with this method.

#### Usage

    ForestRoadsSimulation$clone(deep = FALSE)

#### Arguments

- `deep`:

  Whether to make a deep clone.
