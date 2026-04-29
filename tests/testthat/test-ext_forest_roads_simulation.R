testthat::test_that("Forest Roads Simulation extension is properly created", {
  ## NOTE: reference values from the LANDIS-II Forest Roads Simulation v2 example input:
  ## Examples/Core_v8_Example/input/disturbances/roads.txt
  ## <https://github.com/Klemet/LANDIS-II-Forest-Roads-Simulation-extension/tree/master/Examples/Core_v8_Example>
  ## (this extension is by Klemet; no Foundation `testings/Core8-*` directory exists.)

  tmp_pth <- withr::local_tempdir("test_ForestRoads_")

  coarse_costs <- list(
    elevationCostRange(0, 9, 0),
    elevationCostRange(9, 16, 1000),
    elevationCostRange(16, 41, 3000),
    elevationCostRange(41, 100, 10000)
  )

  fine_costs <- list(
    elevationCostRange(0, 5, 1),
    elevationCostRange(5, 8, 1.5),
    elevationCostRange(8, 1000, 2)
  )

  road_types <- list(
    roadType(id = 3L, name = "Tertiary",  costMultiplier = 1.0,
             fluxMin = 0, fluxMax = 500, maxAge = 20L),
    roadType(id = 2L, name = "Secondary", costMultiplier = 1.5,
             fluxMin = 500, fluxMax = 1500, maxAge = 30L),
    roadType(id = 1L, name = "Primary",   costMultiplier = 5.0,
             fluxMin = 1500, fluxMax = 100000, maxAge = 80L)
  )

  exit_road_types <- list(
    exitRoadType(id = 5L, name = "Sawmill"),
    exitRoadType(id = 6L, name = "MainRoadNetworkPaved")
  )

  ext <- ForestRoadsSimulation$new(
    path = tmp_pth,
    Timestep = 10L,
    HeuristicForNetworkConstruction = "Farthestfirst",
    SkiddingDistance = 150,
    LoopingBehavior = TRUE,
    LoopingMinDistance = 500,
    LoopingMaxDistance = 1200,
    LoopingMaxPercentageOfRoads = 10,
    LoopingMaxCost = 10,
    LoopingProbability = 16,
    RasterOfBuildableZones = "input/disturbances/soils.tif",
    InitialRoadNetworkMap = "input/disturbances/roads.tif",
    DistanceCost = 10000.6,
    CoarseElevationRaster = "input/disturbances/coarse_elevation.tif",
    CoarseElevationCosts = coarse_costs,
    FineElevationRaster = "input/disturbances/fine_elevation.tif",
    FineElevationCosts = fine_costs,
    CoarseWaterRaster = "input/disturbances/coarse_water.tif",
    CoarseWaterCost = 1000000,
    FineWaterRaster = "input/disturbances/fine_water.tif",
    FineWaterCost = 10000,
    SoilsRaster = NULL,
    SimulationOfRoadAging = TRUE,
    SimulationOfWoodFlux = TRUE,
    RoadTypes = road_types,
    RoadTypesForExitingWood = exit_road_types
  )
  ext$write()

  testthat::expect_true(all(file.exists(file.path(tmp_pth, ext$files))))

  contents <- readLines(file.path(tmp_pth, ext$files[1]))
  testthat::expect_true(any(grepl("^LandisData\\s+\"Forest Roads Simulation\"", contents)))
  testthat::expect_true(any(grepl("^Timestep\\s+10", contents)))
  testthat::expect_true(any(grepl("^HeuristicForNetworkConstruction\\s+Farthestfirst$", contents)))
  testthat::expect_true(any(grepl("^SkiddingDistance\\s+150", contents)))
  testthat::expect_true(any(grepl("^LoopingBehavior\\s+yes$", contents)))
  testthat::expect_true(any(grepl("^LoopingMinDistance\\s+500", contents)))
  testthat::expect_true(any(grepl("^LoopingProbability\\s+16", contents)))
  testthat::expect_true(any(grepl("^DistanceCost\\s+10000\\.6", contents)))
  testthat::expect_true(any(grepl("^CoarseElevationCosts", contents)))
  testthat::expect_true(any(grepl("^FineElevationCosts", contents)))
  testthat::expect_true(any(grepl("^CoarseWaterCost\\s+1000000", contents)))
  testthat::expect_true(any(grepl("^FineWaterCost\\s+10000", contents)))
  testthat::expect_true(any(grepl("^SoilsRaster\\s+none$", contents)))
  testthat::expect_true(any(grepl("^SimulationOfRoadAging\\s+yes$", contents)))
  testthat::expect_true(any(grepl("^SimulationOfWoodFlux\\s+yes$", contents)))
  testthat::expect_true(any(grepl("^RoadTypes", contents)))
  testthat::expect_true(any(grepl("Tertiary", contents)))
  testthat::expect_true(any(grepl("^RoadTypesForExitingWood", contents)))
  testthat::expect_true(any(grepl("Sawmill", contents)))
  testthat::expect_true(any(grepl("MainRoadNetworkPaved", contents)))

  withr::deferred_run()
})

testthat::test_that("ForestRoadsSimulation requires Looping* params when LoopingBehavior is on", {
  tmp_pth <- withr::local_tempdir("test_ForestRoads_")

  ext <- ForestRoadsSimulation$new(
    path = tmp_pth,
    Timestep = 10L,
    SkiddingDistance = 150,
    LoopingBehavior = TRUE,
    RasterOfBuildableZones = "z.tif",
    InitialRoadNetworkMap = "r.tif",
    DistanceCost = 1,
    CoarseElevationRaster = "ce.tif",
    CoarseElevationCosts = list(elevationCostRange(0, 100, 0)),
    RoadTypes = list(roadType(1L, "main", 1.0)),
    RoadTypesForExitingWood = list(exitRoadType(2L, "exit"))
  )
  ## missing LoopingMinDistance et al. -> error
  testthat::expect_error(ext$write())

  withr::deferred_run()
})

testthat::test_that("roadType validates flux/age coupling", {
  testthat::expect_error(roadType(id = 1L, name = "x", costMultiplier = 1.0, fluxMin = 100))
  testthat::expect_error(roadType(id = 1L, name = "has space", costMultiplier = 1.0))
})

testthat::test_that("elevationCostRange rejects upper < lower", {
  testthat::expect_error(elevationCostRange(lower = 100, upper = 10, value = 0))
})

testthat::test_that("HeuristicForNetworkConstruction validates and normalizes case", {
  tmp_pth <- withr::local_tempdir("test_ForestRoads_")

  ## user-guide spelling (mixed case) is accepted and stored canonically
  ext <- ForestRoadsSimulation$new(path = tmp_pth)
  testthat::expect_equal(ext$HeuristicForNetworkConstruction, "ClosestFirst")

  ext$HeuristicForNetworkConstruction <- "FarthestFirst"
  testthat::expect_equal(ext$HeuristicForNetworkConstruction, "FarthestFirst")

  ## upstream-example spelling (lowercase "first") is also accepted
  ext$HeuristicForNetworkConstruction <- "Farthestfirst"
  testthat::expect_equal(ext$HeuristicForNetworkConstruction, "FarthestFirst")

  ext$HeuristicForNetworkConstruction <- "random"
  testthat::expect_equal(ext$HeuristicForNetworkConstruction, "Random")

  ## anything else is rejected
  testthat::expect_error(
    ext$HeuristicForNetworkConstruction <- "Greedy",
    "must be one of"
  )

  withr::deferred_run()
})
