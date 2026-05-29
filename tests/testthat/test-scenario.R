## NOTE: all BiomassSuccession sample values from
## <https://github.com/LANDIS-II-Foundation/Extension-Biomass-Succession/blob/master/testings/CoreV8.0-BiomassSuccession7.0/biomass-succession.txt>

## minimal fixture shared across tests in this file --------------------------

.scenario_fixture <- function(tmp_pth) {
  ## stub input files (content irrelevant; only existence is checked)
  clim_file <- file.path(tmp_pth, "biomass-succession_ClimateGenerator.txt")
  init_comm_files <- c(
    file.path(tmp_pth, "biomass-succession_InitialCommunities.csv"),
    file.path(tmp_pth, "initial-communities.tif")
  )
  spp_file <- file.path(tmp_pth, "SpeciesData.csv")
  spperd_file <- file.path(tmp_pth, "SppEcoregionData.csv")
  file.create(c(
    clim_file,
    init_comm_files,
    spp_file,
    spperd_file,
    file.path(tmp_pth, c("ecoregions.txt", "ecoregions.tif", "species.txt", "climate.txt"))
  ))

  ext_bs <- BiomassSuccession$new(
    path = tmp_pth,
    Timestep = 10,
    SeedingAlgorithm = "WardSeedDispersal",
    InitialCommunitiesFiles = init_comm_files,
    ClimateConfigFile = clim_file,
    CalibrateMode = NULL,
    SpinupCohorts = FALSE,
    SpinupMortalityFraction = 0.05,
    MinRelativeBiomass = tibble::tribble(
      ~ShadeClass , ~Eco1 , ~Eco2 ,
      NA_integer_ , "101" , "102" ,
      1L          , "25%" , "25%" ,
      2L          , "45%" , "45%" ,
      3L          , "56%" , "56%" ,
      4L          , "70%" , "70%" ,
      5L          , "90%" , "90%"
    ),
    SufficientLight = tibble::tribble(
      ~class , ~X0 , ~X1 , ~X2  , ~X3  , ~X4  , ~X5  ,
      1L     , 1.0 , 0.5 , 0.25 , 0.0  , 0.0  , 0.0  ,
      2L     , 1.0 , 1.0 , 0.5  , 0.25 , 0.0  , 0.0  ,
      3L     , 1.0 , 1.0 , 1.0  , 0.5  , 0.25 , 0.0  ,
      4L     , 1.0 , 1.0 , 1.0  , 1.0  , 0.5  , 0.25 ,
      5L     , 0.1 , 0.5 , 1.0  , 1.0  , 1.0  , 1.0
    ),
    SpeciesDataFile = spp_file,
    EcoregionParameters = tibble::tribble(
      ~ecoregion , ~AET ,
      "101"      ,  600 ,
      "102"      ,  600
    ),
    SpeciesEcoregionDataFile = spperd_file,
    FireReductionParameters = tibble::tribble(
      ~Severity , ~WoodLitterReduct , ~LitterReduct ,
      1L        , 0.0               , 0.5           ,
      2L        , 0.0               , 0.75          ,
      3L        , 0.0               , 1.0
    ),
    HarvestReductionParameters = tibble::tribble(
      ~Name            , ~WoodLitterReduct , ~LitterReduct , ~CohortWoodRemoval , ~CohortLeafRemoval ,
      "MaxAgeClearcut" , 0.5               , 0.15          , 0.8                , 0.0                ,
      "PatchCutting"   , 1.0               , 1.0           , 1.0                , 0.0
    )
  )
  ext_bs$write()

  cc <- LandisClimateConfig$new(path = tmp_pth)
  cc$add_file("climate.txt")

  list(ext = ext_bs, cc = cc)
}

.scenario_call <- function(name, extensions, cc, tmp_pth, ...) {
  scenario(
    name = name,
    extensions = extensions,
    climate_config = cc,
    path = tmp_pth,
    CellLength = 100,
    DisturbancesRandomOrder = FALSE,
    Duration = 20,
    EcoregionsFiles = c(file.path(tmp_pth, "ecoregions.txt"), file.path(tmp_pth, "ecoregions.tif")),
    SpeciesInputFile = file.path(tmp_pth, "species.txt"),
    ...
  )
}

## ---------------------------------------------------------------------------

testthat::test_that("scenario() writes scenario.txt and returns a LandisScenario", {
  tmp_pth <- withr::local_tempdir("test_scenario_")
  fix <- .scenario_fixture(tmp_pth)

  scen <- .scenario_call("test_scenario", list(fix$ext), fix$cc, tmp_pth)

  testthat::expect_true(is(scen, "LandisScenario"))
  testthat::expect_equal(
    normalizePath(as.character(scen$path), mustWork = FALSE),
    normalizePath(as.character(tmp_pth), mustWork = FALSE)
  )
  testthat::expect_true(all(file.exists(file.path(scen$path, scen$files))))
})

testthat::test_that("scenario() scenario.txt has correct header and key parameters", {
  tmp_pth <- withr::local_tempdir("test_scenario_content_")
  fix <- .scenario_fixture(tmp_pth)

  .scenario_call("test_scenario", list(fix$ext), fix$cc, tmp_pth, RandomNumberSeed = 4357L)

  lines <- readLines(file.path(tmp_pth, "test_scenario.txt"))

  testthat::expect_true(any(grepl('LandisData.*"Scenario"', lines)))
  testthat::expect_true(any(grepl("^Duration\\s+20$", lines)))
  testthat::expect_true(any(grepl("^Species\\s+species\\.txt$", lines)))
  testthat::expect_true(any(grepl("^Ecoregions\\s+ecoregions\\.txt$", lines)))
  testthat::expect_true(any(grepl("^EcoregionsMap\\s+ecoregions\\.tif$", lines)))
  testthat::expect_true(any(grepl("^CellLength\\s+100$", lines)))
  testthat::expect_true(any(grepl('"Biomass Succession"', lines)))
  testthat::expect_true(any(grepl("biomass-succession\\.txt", lines)))
  testthat::expect_true(any(grepl("^DisturbancesRandomOrder\\s+no$", lines)))
  testthat::expect_true(any(grepl("^RandomNumberSeed\\s+4357", lines)))
})

testthat::test_that("scenario() with NULL RandomNumberSeed writes a commented-out line", {
  tmp_pth <- withr::local_tempdir("test_scenario_seed_")
  fix <- .scenario_fixture(tmp_pth)

  .scenario_call("test_scenario", list(fix$ext), fix$cc, tmp_pth, RandomNumberSeed = NULL)

  lines <- readLines(file.path(tmp_pth, "test_scenario.txt"))
  seed_line <- grep("RandomNumberSeed", lines, value = TRUE)

  testthat::expect_true(startsWith(seed_line, ">>"))
})

testthat::test_that("scenario() lists a disturbance extension in scenario.txt", {
  tmp_pth <- withr::local_tempdir("test_scenario_disturbance_")
  fix <- .scenario_fixture(tmp_pth)

  ## minimal disturbance extension stub — no real data files required
  writeLines("", file.path(tmp_pth, "test-disturbance.txt"))
  ext_dist <- LandisExtension$new(
    LandisData = "Test Disturbance",
    type = "disturbance",
    path = tmp_pth
  )
  ext_dist$add_file("test-disturbance.txt")

  .scenario_call("test_scenario", list(fix$ext, ext_dist), fix$cc, tmp_pth)

  lines <- readLines(file.path(tmp_pth, "test_scenario.txt"))

  testthat::expect_true(any(grepl('"Test Disturbance"', lines)))
  testthat::expect_true(any(grepl("test-disturbance\\.txt", lines)))
})

testthat::test_that("LandisScenario$replicate() creates subdirs and copies files", {
  tmp_pth <- withr::local_tempdir("test_scenario_replicate_")
  fix <- .scenario_fixture(tmp_pth)

  scen <- .scenario_call("test_scenario", list(fix$ext), fix$cc, tmp_pth)

  testthat::expect_equal(scen$reps, 0L)

  scen$replicate(n = 3)

  testthat::expect_equal(scen$reps, 3L)
  testthat::expect_true(fs::dir_exists(file.path(tmp_pth, "rep01")))
  testthat::expect_true(fs::dir_exists(file.path(tmp_pth, "rep02")))
  testthat::expect_true(fs::dir_exists(file.path(tmp_pth, "rep03")))
  testthat::expect_true(all(file.exists(file.path(tmp_pth, "rep01", scen$files))))
})

testthat::test_that("scenario() writes output_manifest.txt listing scenario- and extension-level outputs", {
  tmp_pth <- withr::local_tempdir("test_scenario_manifest_")
  fix <- .scenario_fixture(tmp_pth)

  scen <- .scenario_call("test_scenario", list(fix$ext), fix$cc, tmp_pth)

  testthat::expect_true("output_manifest.txt" %in% scen$files)
  manifest_path <- file.path(tmp_pth, "output_manifest.txt")
  testthat::expect_true(file.exists(manifest_path))
  entries <- readLines(manifest_path)
  ## scenario-level fixed outputs are always present
  testthat::expect_true("Landis-log.txt" %in% entries)
  testthat::expect_true("Metadata/LANDIS-II v8.0/LANDIS-II v8.0.xml" %in% entries)
})

testthat::test_that("LandisScenario$output_files is read-only and returns fixed scenario outputs", {
  tmp_pth <- withr::local_tempdir("test_scenario_output_files_")
  fix <- .scenario_fixture(tmp_pth)

  scen <- .scenario_call("test_scenario", list(fix$ext), fix$cc, tmp_pth)

  testthat::expect_setequal(
    scen$output_files,
    c("Landis-log.txt", "Metadata/LANDIS-II v8.0/LANDIS-II v8.0.xml")
  )
  testthat::expect_snapshot(error = TRUE, scen$output_files <- "something.txt")
})

testthat::test_that("LandisScenario$replicate() is additive and leaves existing reps untouched", {
  tmp_pth <- withr::local_tempdir("test_scenario_replicate_add_")
  fix <- .scenario_fixture(tmp_pth)

  scen <- .scenario_call("test_scenario", list(fix$ext), fix$cc, tmp_pth)

  scen$replicate(n = 3)
  writeLines("sentinel", file.path(tmp_pth, "rep01", "sentinel.txt"))

  scen$replicate(n = 1)

  testthat::expect_equal(scen$reps, 4L)
  testthat::expect_true(fs::dir_exists(file.path(tmp_pth, "rep04")))
  testthat::expect_true(all(file.exists(file.path(tmp_pth, "rep04", scen$files))))
  testthat::expect_true(file.exists(file.path(tmp_pth, "rep01", "sentinel.txt")))
})
