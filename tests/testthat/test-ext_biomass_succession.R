testthat::test_that("Biomass Succession inputs are properly created", {
  ## NOTE: all sample values from Biomass Succession v7.0/7.1 test files (for LANDIS-II v8)
  ## <https://github.com/LANDIS-II-Foundation/Extension-Biomass-Succession/blob/master/testings/CoreV8.0-BiomassSuccession7.0/biomass-succession.txt>

  tmp_pth <- withr::local_tempdir("test_BiomassSuccession_")

  min_rel_b <- tibble::tribble(
    ~ShadeClass , ~Eco1 , ~Eco2  , ## TODO adjust colnames
    NA_integer_ , "101" , "102"  ,
    1L          , "25%" , "25%"  ,
    2L          , "45%" , "45%"  ,
    3L          , "56%" , "56%"  ,
    4L          , "70%" , "70%"  ,
    5L          , "90%" , "90% "
  )

  suff_light <- tibble::tribble(
    ~class , ~X0 , ~X1 , ~X2  , ~X3  , ~X4  , ~X5  ,
    1L     , 1.0 , 0.5 , 0.25 , 0.0  , 0.0  , 0.0  ,
    2L     , 1.0 , 1.0 , 0.5  , 0.25 , 0.0  , 0.0  ,
    3L     , 1.0 , 1.0 , 1.0  , 0.5  , 0.25 , 0.0  ,
    4L     , 1.0 , 1.0 , 1.0  , 1.0  , 0.5  , 0.25 ,
    5L     , 0.1 , 0.5 , 1.0  , 1.0  , 1.0  , 1.0
  )

  erp_df <- tibble::tribble(
    ~ecoregion , ~AET ,
    "101"      ,  600 ,
    "102"      ,  600
  )

  frp_df <- tibble::tribble(
    ~Severity , ~WoodLitterReduct , ~LitterReduct ,
    1L        , 0.0               , 0.5           ,
    2L        , 0.0               , 0.75          ,
    3L        , 0.0               , 1.0
  )

  hrp_df <- tibble::tribble(
    ~Name            , ~WoodLitterReduct , ~LitterReduct , ~CohortWoodRemoval , ~CohortLeafRemoval ,
    "MaxAgeClearcut" , 0.5               , 0.15          , 0.8                , 0.0                ,
    "PatchCutting"   , 1.0               , 1.0           , 1.0                , 0.0
  )

  ## don't need to function; just need to exist
  clim_file <- file.path(tmp_pth, "biomass-succession_ClimateGenerator.txt")
  init_comm_files <- c(
    file.path(tmp_pth, "biomass-succession_InitialCommunities.csv"),
    file.path(tmp_pth, "initial-communities.tif")
  )
  spp_file <- file.path(tmp_pth, "SpeciesData.csv")
  spperd_file <- file.path(tmp_pth, "SppEcoregionData.csv")
  all_files <- c(clim_file, init_comm_files, spp_file, spperd_file)
  purrr::walk2(.x = rep("", length(all_files)), .y = all_files, .f = writeLines)

  ## create the Biomass Succession extension config object
  ext_biomass_succession <- BiomassSuccession$new(
    path = tmp_pth,
    Timestep = 10,
    SeedingAlgorithm = "WardSeedDispersal",
    InitialCommunitiesFiles = init_comm_files,
    ClimateConfigFile = clim_file,
    CalibrateMode = NULL, ## optional
    SpinupCohorts = FALSE, ## optional; v7.1
    SpinupMortalityFraction = 0.05, ## v7.1
    MinRelativeBiomass = min_rel_b,
    SufficientLight = suff_light,
    SpeciesDataFile = spp_file,
    EcoregionParameters = erp_df,
    SpeciesEcoregionDataFile = spperd_file,
    FireReductionParameters = frp_df,
    HarvestReductionParameters = hrp_df
  )

  ext_biomass_succession$write()

  testthat::expect_true(all(file.exists(file.path(tmp_pth, ext_biomass_succession$files))))

  withr::deferred_run()
})

testthat::test_that("SpinupCohorts / SpinupMortalityFraction are optional (omitted when NULL)", {
  ## The Core8 CoreV8.0-BiomassSuccession7.0 grammar has no Spinup* keywords; the v8 parser aborts on
  ## them. They must therefore be written only when explicitly set (they remain available for v7.1).
  tmp_pth <- withr::local_tempdir("test_BiomassSuccession_spinup_")

  min_rel_b <- tibble::tribble(
    ~ShadeClass , ~Eco1 ,
    NA_integer_ , "101" ,
    1L          , "25%"
  )
  suff_light <- tibble::tribble(
    ~class , ~X0 , ~X1 , ~X2  , ~X3 , ~X4 , ~X5 ,
    1L     , 1.0 , 0.5 , 0.25 , 0.0 , 0.0 , 0.0
  )
  frp_df <- tibble::tribble(
    ~Severity , ~WoodLitterReduct , ~LitterReduct ,
    1L        , 0.0               , 0.5
  )
  hrp_df <- tibble::tribble(
    ~Name            , ~WoodLitterReduct , ~LitterReduct , ~CohortWoodRemoval , ~CohortLeafRemoval ,
    "MaxAgeClearcut" , 0.5               , 0.15          , 0.8                , 0.0
  )

  mk <- function(spinup_cohorts, spinup_mort) {
    BiomassSuccession$new(
      path = tmp_pth,
      Timestep = 10,
      InitialCommunitiesFiles = file.path(tmp_pth, "ic.csv"),
      ClimateConfigFile = file.path(tmp_pth, "climate.txt"),
      SpinupCohorts = spinup_cohorts,
      SpinupMortalityFraction = spinup_mort,
      MinRelativeBiomass = min_rel_b,
      SufficientLight = suff_light,
      SpeciesDataFile = file.path(tmp_pth, "spp.csv"),
      EcoregionParameters = tibble::tibble(ecoregion = "101", AET = 600),
      SpeciesEcoregionDataFile = file.path(tmp_pth, "sppeco.csv"),
      FireReductionParameters = frp_df,
      HarvestReductionParameters = hrp_df
    )
  }

  ## referenced input files must exist (add_file() checks); contents are irrelevant here
  ref_files <- file.path(tmp_pth, c("ic.csv", "climate.txt", "spp.csv", "sppeco.csv"))
  purrr::walk2(.x = rep("", length(ref_files)), .y = ref_files, .f = writeLines)

  ## NULL (default) -> Spinup* lines absent
  mk(NULL, NULL)$write()
  omitted <- readLines(file.path(tmp_pth, "biomass-succession.txt"))
  testthat::expect_length(grep("Spinup", omitted), 0L)

  ## explicitly set -> both lines present
  mk(FALSE, 0.05)$write()
  present <- readLines(file.path(tmp_pth, "biomass-succession.txt"))
  testthat::expect_length(grep("^SpinupCohorts", present), 1L)
  testthat::expect_length(grep("^SpinupMortalityFraction", present), 1L)
})
