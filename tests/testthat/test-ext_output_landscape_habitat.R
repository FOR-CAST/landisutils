testthat::test_that("Landscape Habitat Output extension is properly created", {
  ## NOTE: reference values from the LANDIS-II Core8 / Landscape Habitat v2 test input:
  ## testings/Core8.0-LandscapeHabitat2.0/output-landscape-habitat.txt
  ## <https://github.com/LANDIS-II-Foundation/Extension-Output-Landscape-Habitat/tree/master/testings/Core8.0-LandscapeHabitat2.0>

  tmp_pth <- withr::local_tempdir("test_OutputLandscapeHabitat_")

  reclass2 <- data.frame(
    ForestType = c("Open", "Regen", "LowlandCon", "UplandCon"),
    AgeRange = c("All", "1 to 15", "All", "All"),
    Species = c("None", "All", "thujocci", "piceglau abiebals pinuresi pinubank")
  )

  derived <- c(
    LowlandFor = "reclass2[LowlandCon] + reclass2[LowlandHdwd] + reclass2[LowlandMix]",
    UplandFor = "reclass2[UplandCon] + reclass2[UplandHdwd] + reclass2[UplandMix]"
  )

  neighbor <- data.frame(
    Name = c("loguc200", "uf500"),
    LocalVar = c("reclass2[UplandCon]", "UplandFor"),
    NeighborRadius = c(200L, 500L),
    Transform = c("ln", "none")
  )

  climate <- data.frame(
    Name = "temp",
    Year = "current",
    Months = "3 to 6",
    Source = "Library",
    ClimateVar = "temp",
    Transform = "none"
  )

  species_models <- list(
    SPP1 = data.frame(
      Parameter = c("intercept", "loguc200", "temp", "uf500"),
      Type = c("int", "neighbor", "climate", "neighbor"),
      Value = c(-5.228327, 0.823546, -0.180834, 0.020801)
    ),
    SPP2 = data.frame(
      Parameter = c("intercept", "logforest200"),
      Type = c("int", "neighbor"),
      Value = c(-8.25, 1.922)
    )
  )

  ext <- OutputLandscapeHabitat$new(
    path = tmp_pth,
    Timestep = 10L,
    LocalVariables = list(reclass2 = reclass2),
    DerivedLocalVariables = derived,
    NeighborhoodVariables = neighbor,
    ClimateVariables = climate,
    SpeciesModels = species_models
  )
  ext$write()

  testthat::expect_true(all(file.exists(file.path(tmp_pth, ext$files))))

  contents <- readLines(file.path(tmp_pth, ext$files[1]))
  testthat::expect_true(any(grepl("^LandisData\\s+\"Output Landscape Habitat\"", contents)))
  testthat::expect_true(any(grepl("^Timestep\\s+10", contents)))
  testthat::expect_true(any(grepl("^LocalVariables", contents)))
  testthat::expect_true(any(grepl("reclass2", contents)))
  testthat::expect_true(any(grepl("LowlandCon", contents)))
  testthat::expect_true(any(grepl("^DerivedLocalVariables", contents)))
  testthat::expect_true(any(grepl("LowlandFor", contents)))
  testthat::expect_true(any(grepl("^NeighborhoodVariables", contents)))
  testthat::expect_true(any(grepl("loguc200", contents)))
  testthat::expect_true(any(grepl("^ClimateVariables", contents)))
  testthat::expect_true(any(grepl("^SpeciesModels", contents)))
  testthat::expect_true(any(grepl("SPP1\\s+->", contents)))
  testthat::expect_true(any(grepl("intercept", contents)))
  testthat::expect_true(any(grepl("-5\\.228327", contents)))
  testthat::expect_true(any(grepl("^LogFile", contents)))

  withr::deferred_run()
})

testthat::test_that("OutputLandscapeHabitat write() requires SpeciesModels", {
  tmp_pth <- withr::local_tempdir("test_OutputLandscapeHabitat_")

  ext <- OutputLandscapeHabitat$new(path = tmp_pth, Timestep = 10L)
  testthat::expect_error(ext$write())

  withr::deferred_run()
})

testthat::test_that("OutputLandscapeHabitat enforces literal MapFileName placeholders", {
  ## Per the Output Landscape Habitat v2 User Guide, section 2.10/2.11:
  ##  - LocalVarMapFileNames    requires {local-var-name}    + {timestep}
  ##  - NeighborVarMapFileNames requires {neighbor-var-name} + {timestep}
  ##  - ClimateVarMapFileNames  requires {climate-var-name}  + {timestep}
  ##  - DistanceVarMapFileNames requires {distance-var-name} + {timestep}
  ##  - SpeciesMapFileNames     requires {species-name}      + {timestep}
  ##  - SpeciesLogFileNames     requires {species-name}      (no {timestep})
  tmp_pth <- withr::local_tempdir("test_OutputLandscapeHabitat_")

  ext <- OutputLandscapeHabitat$new(path = tmp_pth, Timestep = 10L)

  ## missing {timestep}
  testthat::expect_error(ext$LocalVarMapFileNames <- "out/{local-var-name}.tif", "timestep")
  ## missing {local-var-name}
  testthat::expect_error(ext$LocalVarMapFileNames <- "out/local-{timestep}.tif", "local-var-name")

  testthat::expect_error(ext$NeighborVarMapFileNames <- "out/{neighbor-var-name}.tif", "timestep")
  testthat::expect_error(
    ext$NeighborVarMapFileNames <- "out/neighbor-{timestep}.tif",
    "neighbor-var-name"
  )

  testthat::expect_error(ext$ClimateVarMapFileNames <- "out/{climate-var-name}.tif", "timestep")
  testthat::expect_error(
    ext$ClimateVarMapFileNames <- "out/climate-{timestep}.tif",
    "climate-var-name"
  )

  testthat::expect_error(ext$DistanceVarMapFileNames <- "out/{distance-var-name}.tif", "timestep")
  testthat::expect_error(
    ext$DistanceVarMapFileNames <- "out/distance-{timestep}.tif",
    "distance-var-name"
  )

  testthat::expect_error(ext$SpeciesMapFileNames <- "out/{species-name}.tif", "timestep")
  testthat::expect_error(ext$SpeciesMapFileNames <- "out/habitat-{timestep}.tif", "species-name")

  testthat::expect_error(ext$SpeciesLogFileNames <- "out/log.csv", "species-name")

  ## valid templates assign without error
  testthat::expect_silent({
    ext$LocalVarMapFileNames <- "out/{local-var-name}-{timestep}.tif"
    ext$NeighborVarMapFileNames <- "out/{neighbor-var-name}-{timestep}.tif"
    ext$ClimateVarMapFileNames <- "out/{climate-var-name}-{timestep}.tif"
    ext$DistanceVarMapFileNames <- "out/{distance-var-name}-{timestep}.tif"
    ext$SpeciesMapFileNames <- "out/{species-name}-{timestep}.tif"
    ext$SpeciesLogFileNames <- "out/{species-name}_log.csv"
  })

  withr::deferred_run()
})
