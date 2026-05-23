testthat::test_that("Biomass Harvest inputs are properly created", {
  ## NOTE: reference values from the LANDIS-II Core v8 / Biomass Harvest v7 test input:
  ## testings/Core8-Harvest7.0/harvest_SetUp.txt
  ## <https://github.com/LANDIS-II-Foundation/Extension-Biomass-Harvest/tree/master/testings/Core8-Harvest7.0>

  tmp_pth <- withr::local_tempdir("test_BiomassHarvest_")

  ## the two input maps don't need to be valid rasters for this test;
  ## they only need to exist when `add_file()` is called at write time.
  mgmt_file <- file.path(tmp_pth, "harvest_Management_zones.tif")
  stands_file <- file.path(tmp_pth, "harvest_Stands.tif")
  purrr::walk(c(mgmt_file, stands_file), ~ writeLines("", .x))

  ## Single prescription from the upstream Core8-Harvest7.0 reference.
  rx_maxage <- harvestPrescription(
    name = "MaxAgeClearcut",
    StandRanking = "MaxCohortAge",
    SiteSelection = "Complete",
    CohortsRemoved = "ClearCut",
    MultipleRepeat = 20L
  )

  ## Additional prescription to exercise the SpeciesList / Economic / PatchCutting
  ## branches (not covered by the minimal upstream reference).
  econ_rank <- tibble::tribble(
    ~Species   , ~EconomicRank , ~MinimumAge ,
    "acersacc" , 100L          , 60L         ,
    "acerrubr" ,  50L          , 60L
  )
  species_list <- tibble::tribble(
    ~Species   , ~Cohorts            ,
    "abiebals" , "All"               ,
    "acersacc" , "AllExceptYoungest" ,
    "acerrubr" , "AllExceptYoungest" ,
    "pinubank" , "50"
  )
  rx_maple <- harvestPrescription(
    name = "MapleHarvest",
    StandRanking = "Economic",
    EconomicRankTable = econ_rank,
    MinimumAge = 20L,
    SiteSelection = "Complete",
    CohortsRemoved = "SpeciesList",
    SpeciesList = species_list
  )

  ## HarvestImplementations row from the upstream reference.
  impl <- tibble::tribble(
    ~MgmtArea , ~Prescription    , ~HarvestArea , ~BeginTime , ~EndTime ,
    1L        , "MaxAgeClearcut" , "10%"        ,  5L        ,  6L      ,
    1L        , "MapleHarvest"   , "5%"         , 10L        , 50L
  )

  ext_bh <- BiomassHarvest$new(
    path = tmp_pth,
    Timestep = 5L, ## upstream reference uses 5
    ManagementAreas = mgmt_file,
    Stands = stands_file,
    Prescriptions = list(rx_maxage, rx_maple),
    HarvestImplementations = impl,
    PrescriptionMaps = "harvest/biomass-harvest-prescripts-{timestep}.tif",
    BiomassMaps = "harvest/biomass-removed-{timestep}.tif",
    EventLog = file.path(tmp_pth, "harvest/biomass-harvest-event-log.csv"),
    SummaryLog = file.path(tmp_pth, "harvest/biomass-harvest-summary-log.csv")
  )

  ext_bh$write()

  testthat::expect_true(all(file.exists(file.path(tmp_pth, ext_bh$files))))

  contents <- readLines(file.path(tmp_pth, ext_bh$files[1]))
  ## NOTE: v7 user guide §3.1 says LandisData should be "Harvest", but the
  ## released v6.0 extension (shipping in the LANDIS-II v8 core) still requires
  ## "Biomass Harvest" and explicitly errors otherwise (verified against
  ## ghcr.io/landis-ii-foundation/landis-ii-v8-release:main).
  testthat::expect_true(any(grepl("^LandisData\\s+\"Biomass Harvest\"", contents)))
  testthat::expect_true(any(grepl("^Timestep\\s+5", contents)))
  testthat::expect_true(any(grepl("^ManagementAreas", contents)))
  testthat::expect_true(any(grepl("^Stands", contents)))
  testthat::expect_true(any(grepl("^Prescription\\s+MaxAgeClearcut", contents)))
  testthat::expect_true(any(grepl("^StandRanking\\s+MaxCohortAge", contents)))
  testthat::expect_true(any(grepl("^SiteSelection\\s+Complete", contents)))
  testthat::expect_true(any(grepl("^CohortsRemoved\\s+ClearCut", contents)))
  testthat::expect_true(any(grepl("^MultipleRepeat\\s+20", contents)))
  testthat::expect_true(any(grepl("^Prescription\\s+MapleHarvest", contents)))
  testthat::expect_true(any(grepl("^StandRanking\\s+Economic", contents)))
  testthat::expect_true(any(grepl("^CohortsRemoved\\s+SpeciesList", contents)))
  testthat::expect_true(any(grepl("^HarvestImplementations", contents)))
  testthat::expect_true(any(grepl("^\\s*1\\s+MaxAgeClearcut\\s+10%\\s+5\\s+6", contents)))
  testthat::expect_true(any(grepl(
    "^PrescriptionMaps\\s+harvest/biomass-harvest-prescripts-\\{timestep\\}\\.tif",
    contents
  )))
  testthat::expect_true(any(grepl(
    "^BiomassMaps\\s+harvest/biomass-removed-\\{timestep\\}\\.tif",
    contents
  )))
  testthat::expect_true(any(grepl(
    "^EventLog\\s+harvest/biomass-harvest-event-log\\.csv",
    contents
  )))
  testthat::expect_true(any(grepl(
    "^SummaryLog\\s+harvest/biomass-harvest-summary-log\\.csv",
    contents
  )))

  withr::deferred_run()
})
