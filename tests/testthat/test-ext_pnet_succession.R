testthat::test_that("PnET-Succession inputs are properly created", {
  ## NOTE: reference values from the LANDIS-II Core v8 / PnET-Succession example input:
  ## deploy/examples/biomass-Pnet-succession-example-v8/PnET-succession.txt
  ## <https://github.com/LANDIS-II-Foundation/Extension-PnET-Succession/tree/master/deploy/examples/biomass-Pnet-succession-example-v8>

  tmp_pth <- withr::local_tempdir("test_PnETSuccession_")

  ## stub input files in a temp directory -- the scaffolding only checks that
  ## referenced paths exist, not that their contents are valid LANDIS-II files.
  ## NOTE: upstream reference uses ".gis" for raster inputs; we use ".tif" here.
  init_comm_csv <- file.path(tmp_pth, "initial-communities.csv")
  init_comm_map <- file.path(tmp_pth, "initial-communities.tif")
  spp_params <- file.path(tmp_pth, "PnETSpeciesParameters.txt")
  eco_params <- file.path(tmp_pth, "EcoregionParameters.txt")
  generic_params <- file.path(tmp_pth, "GenericPnETParameters.txt")
  outsites <- file.path(tmp_pth, "PNEToutputsites.txt")

  stub_files <- c(init_comm_csv, init_comm_map, spp_params, eco_params, generic_params, outsites)
  purrr::walk2(.x = rep("", length(stub_files)), .y = stub_files, .f = writeLines)

  ext_pnet <- PnETSuccession$new(
    path = tmp_pth,
    Timestep = 10L,
    StartYear = 2000L,
    SeedingAlgorithm = "WardSeedDispersal",
    Latitude = 42,
    PNEToutputsites = outsites,
    InitialCommunities = init_comm_csv,
    InitialCommunitiesMap = init_comm_map,
    PnETGenericParameters = generic_params,
    PnETSpeciesParameters = spp_params,
    EcoregionParameters = eco_params
  )

  ext_pnet$write()

  testthat::expect_true(all(file.exists(file.path(tmp_pth, ext_pnet$files))))
  testthat::expect_equal(ext_pnet$type, "succession")
  testthat::expect_equal(ext_pnet$SeedingAlgorithm, "WardSeedDispersal")

  written <- readLines(file.path(tmp_pth, ext_pnet$files[1]))
  testthat::expect_true(any(grepl('LandisData.*"PnET-Succession"', written)))
  testthat::expect_true(any(grepl("^Timestep\\s+10", written)))
  testthat::expect_true(any(grepl("^StartYear\\s+2000", written)))
  testthat::expect_true(any(grepl("^SeedingAlgorithm\\s+\"?WardSeedDispersal", written)))
  testthat::expect_true(any(grepl("^Latitude\\s+42", written)))
  testthat::expect_true(any(grepl("^PNEToutputsites\\s", written)))
  testthat::expect_true(any(grepl("^InitialCommunities\\s", written)))
  testthat::expect_true(any(grepl("^InitialCommunitiesMap\\s", written)))
  testthat::expect_true(any(grepl("^PnETGenericParameters\\s", written)))
  testthat::expect_true(any(grepl("^PnETSpeciesParameters\\s", written)))
  testthat::expect_true(any(grepl("^EcoregionParameters\\s", written)))

  withr::deferred_run()
})

testthat::test_that("PnET-Succession rejects invalid SeedingAlgorithm", {
  tmp_pth <- withr::local_tempdir("test_PnETSuccession_invalid_")

  testthat::expect_error(
    PnETSuccession$new(path = tmp_pth, SeedingAlgorithm = "bogus"),
    regexp = "WardSeedDispersal"
  )

  withr::deferred_run()
})

testthat::test_that("PnET-Succession CohortBinSize must be >= Timestep", {
  tmp_pth <- withr::local_tempdir("test_PnETSuccession_binsize_")

  testthat::expect_error(PnETSuccession$new(path = tmp_pth, Timestep = 10L, CohortBinSize = 5L))

  withr::deferred_run()
})

testthat::test_that("prepPnETGenericParameters writes a valid file", {
  tmp_pth <- withr::local_tempdir("test_PnETGeneric_")

  f <- prepPnETGenericParameters(
    params = list(MaxCanopyLayers = 4L, IMAX = 5L, Wythers = TRUE, MaxPest = 0.3),
    path = tmp_pth
  )

  testthat::expect_true(file.exists(f))
  written <- readLines(f)
  testthat::expect_true(any(grepl('LandisData.*"PnETGenericParameters"', written)))
  testthat::expect_true(any(grepl("^MaxCanopyLayers\\s+4", written)))
  testthat::expect_true(any(grepl("^Wythers\\s+yes", written)))
  testthat::expect_true(any(grepl("^MaxPest\\s+0\\.3", written)))

  testthat::expect_error(
    prepPnETGenericParameters(params = list(BogusParam = 1), path = tmp_pth),
    regexp = "BogusParam"
  )

  withr::deferred_run()
})

testthat::test_that("prepPnETSpeciesParameters validates columns", {
  tmp_pth <- withr::local_tempdir("test_PnETSpecies_")

  spp <- data.frame(
    SpeciesCode = c("abiebals", "acerrubr"),
    FolN = c(1.1, 2.1),
    SLWmax = c(170, 60),
    SLWDel = c(0, 0.2),
    TOfol = c(0.25, 1),
    AmaxA = c(5.3, -46),
    AmaxB = c(21.5, 71.9),
    HalfSat = c(154, 154),
    H3 = c(143, 147),
    H4 = c(5, 5),
    PsnAgeRed = c(2, 3),
    PsnTMin = c(19, 26),
    PsnTOpt = c(36, 40),
    k = c(0.5, 0.58),
    FracFol = c(0.07, 0.02),
    FrActWd = c(0.00004, 0.00004),
    stringsAsFactors = FALSE
  )

  f <- prepPnETSpeciesParameters(spp, path = tmp_pth)
  testthat::expect_true(file.exists(f))

  ## reject unknown column
  bad <- spp
  bad$WUEConst <- 0.1
  testthat::expect_error(prepPnETSpeciesParameters(bad, path = tmp_pth), regexp = "WUEConst")

  ## reject missing required column
  missing <- spp[, setdiff(colnames(spp), "FolN")]
  testthat::expect_error(prepPnETSpeciesParameters(missing, path = tmp_pth), regexp = "FolN")

  withr::deferred_run()
})

testthat::test_that("prepPnETEcoregionParameters validates columns", {
  tmp_pth <- withr::local_tempdir("test_PnETEcoregion_")

  eco <- data.frame(
    EcoregionName = c("eco1", "eco2"),
    SoilType = c("CLAY", "LOAM"),
    RootingDepth = c(1000L, 1200L),
    PrecLossFrac = c(0.0, 0.1),
    EvapDepth = c(100L, 100L),
    LeakageFrac = c(1.0, 1.0),
    PrecIntConst = c(0.1, 0.1),
    SnowSublimFrac = c(0.15, 0.15),
    ClimateFileName = c("climate.txt", "climate.txt"),
    stringsAsFactors = FALSE
  )

  f <- prepPnETEcoregionParameters(eco, path = tmp_pth)
  testthat::expect_true(file.exists(f))

  ## reject missing required column
  missing <- eco[, setdiff(colnames(eco), "RootingDepth")]
  testthat::expect_error(
    prepPnETEcoregionParameters(missing, path = tmp_pth),
    regexp = "RootingDepth"
  )

  withr::deferred_run()
})

testthat::test_that("prepPnETDisturbanceReductions defaults match user-guide example", {
  tmp_pth <- withr::local_tempdir("test_PnETDistRed_")

  f <- prepPnETDisturbanceReductions(path = tmp_pth)
  testthat::expect_true(file.exists(f))

  written <- readLines(f)
  testthat::expect_true(any(grepl(
    "^DisturbanceReductions\\s+fire\\s+wind\\s+harvest\\s+bda",
    written
  )))
  testthat::expect_true(any(grepl("^WoodReduction\\s+0\\.33", written)))
  testthat::expect_true(any(grepl("^FolReduction\\s+1", written)))
  testthat::expect_true(any(grepl("^DeadWoodReduction\\s+0\\.7", written)))

  ## reject out-of-range values
  bad <- defaultPnETDisturbanceReductions()
  bad$fire[1L] <- 1.5
  testthat::expect_error(prepPnETDisturbanceReductions(bad, path = tmp_pth))

  withr::deferred_run()
})

testthat::test_that("prepPnETClimateFile writes header and rows", {
  tmp_pth <- withr::local_tempdir("test_PnETClimate_")

  clim <- data.frame(
    Year = 1980L:1981L,
    Month = c(1L, 1L),
    TMax = c(-0.4, 1.6),
    TMin = c(-10, -7.2),
    PAR = c(47.78, 53.32),
    Prec = c(697.67, 496.07),
    CO2 = c(338, 338),
    stringsAsFactors = FALSE
  )

  f <- prepPnETClimateFile(clim, path = tmp_pth)
  testthat::expect_true(file.exists(f))
  written <- readLines(f)
  testthat::expect_match(written[1L], "^Year Month TMax TMin PAR Prec CO2$")
  testthat::expect_length(written, 3L) ## header + 2 obs

  ## with optional O3
  clim$O3 <- c(0, 1.2)
  f2 <- prepPnETClimateFile(clim, path = tmp_pth, filename = "climate_o3.txt")
  testthat::expect_match(readLines(f2)[1L], "O3$")

  withr::deferred_run()
})

testthat::test_that("prepPNEToutputsites supports map and rowcol coords", {
  tmp_pth <- withr::local_tempdir("test_PNEToutputsites_")

  rc <- data.frame(Site = "Site1", Row = 9L, Column = 9L, stringsAsFactors = FALSE)
  f1 <- prepPNEToutputsites(rc, path = tmp_pth, coords = "rowcol", filename = "rc.txt")
  testthat::expect_true(any(grepl("^PNEToutputsites\\s+Row\\s+Column", readLines(f1))))

  mc <- data.frame(
    Site = "Site1",
    MapCoordinatesX = 650661.773,
    MapCoordinatesY = 533426.497,
    MapCoordinatesMaxX = 663025.658594,
    MapCoordinatesMaxY = 544946.023893,
    stringsAsFactors = FALSE
  )
  f2 <- prepPNEToutputsites(mc, path = tmp_pth, coords = "map", filename = "mc.txt")
  testthat::expect_true(any(grepl("^PNEToutputsites\\s+MapCoordinatesX", readLines(f2))))

  ## missing required column for the chosen form
  testthat::expect_error(
    prepPNEToutputsites(rc, path = tmp_pth, coords = "map"),
    regexp = "MapCoordinates"
  )

  withr::deferred_run()
})
