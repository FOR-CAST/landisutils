testthat::test_that("Biomass Browse extension is properly created", {
  ## NOTE: parameter list and example values from the LANDIS-II Biomass
  ## Browse v2.0 User Guide (Section 4 + the upstream
  ## tests/examples/DynamicUngulateBrowse.txt):
  ## docs/LANDIS-II Biomass Browse v2.0 User Guide.pdf
  ## <https://github.com/LANDIS-II-Foundation/Extension-Biomass-Browse/tree/master/docs>

  tmp_pth <- withr::local_tempdir("test_BiomassBrowse_")

  ## stub the input files referenced by `.relPath()`-validated parameters
  zone_map <- file.path(tmp_pth, "ecoregions.gis")
  pop_file <- file.path(tmp_pth, "DefinedUngulatePopulation.txt")
  for (f in c(zone_map, pop_file)) {
    writeLines("", f)
  }

  spt <- data.frame(
    Species = c("acerrubr", "acersacc", "tsugcana"),
    Preference = c(0.3, 0.5, 0.5),
    GrowthReductionThreshold = c(0.5, 0.5, 0.5),
    GrowthReductionMaximum = c(0.4, 0.4, 0.4),
    MortalityThreshold = c(0.5, 0.5, 0.5),
    MortalityMaximum = c(0.1, 0.1, 0.1)
  )

  suppressWarnings({
    ext <- BiomassBrowse$new(
      path = tmp_pth,
      Timestep = 1L,
      SpeciesTable = spt,
      ZoneMap = zone_map,
      BrowseMethod = "Population",
      DefinedPopulationFile = pop_file,
      ConsumptionRate = 2738L,
      ANPPForageProp = 0.66,
      MinBrowsePropinReach = 0.50,
      BrowseBiomassThreshold = 0.05,
      EscapeBrowsePropLong = 0.57,
      SitePreference = 500L,
      SitePrefMapNames = "browse/SitePref-{timestep}.tif",
      SiteForageMapNames = "browse/SiteForage-{timestep}.tif",
      SiteHSIMapNames = "browse/HSI-{timestep}.tif",
      SitePopulationMapNames = "browse/Pop-{timestep}.tif",
      BiomassRemovedMapNames = "browse/BioRemoved-{timestep}.tif"
    )
  })
  ext$write()

  testthat::expect_true(all(file.exists(file.path(tmp_pth, ext$files))))

  contents <- readLines(file.path(tmp_pth, ext$files[1]))
  testthat::expect_true(any(grepl("^LandisData\\s+\"Biomass Browse\"", contents)))
  testthat::expect_true(any(grepl("^Timestep\\s+1", contents)))
  testthat::expect_true(any(grepl("^SpeciesTable", contents)))
  testthat::expect_true(any(grepl("acerrubr", contents)))
  testthat::expect_true(any(grepl("^ZoneMap\\s+ecoregions\\.gis", contents)))
  testthat::expect_true(any(grepl("^BrowseMethod\\s+Population", contents)))
  testthat::expect_true(any(grepl(
    "^DefinedPopulationFile\\s+DefinedUngulatePopulation\\.txt",
    contents
  )))
  testthat::expect_true(any(grepl("^ConsumptionRate\\s+2738", contents)))
  testthat::expect_true(any(grepl("^ANPPForageProp\\s+0\\.66", contents)))
  testthat::expect_true(any(grepl("^MinBrowsePropinReach\\s+0\\.5", contents)))
  testthat::expect_true(any(grepl("^BrowseBiomassThreshold\\s+0\\.05", contents)))
  testthat::expect_true(any(grepl("^EscapeBrowsePropLong\\s+0\\.57", contents)))
  testthat::expect_true(any(grepl("^SitePreference\\s+500", contents)))
  testthat::expect_true(any(grepl("^SitePrefMapNames\\s+browse/SitePref-", contents)))
  testthat::expect_true(any(grepl("^SiteHSIMapNames\\s+browse/HSI-", contents)))
  testthat::expect_true(any(grepl("^BiomassRemovedMapNames\\s+browse/BioRemoved-", contents)))

  ## DynamicPopulation block not provided -> should not appear
  testthat::expect_false(any(grepl("^DynamicPopulation", contents)))

  withr::deferred_run()
})

testthat::test_that("BiomassBrowse$new() warns about missing LANDIS-II v8 compatibility", {
  tmp_pth <- withr::local_tempdir("test_BiomassBrowse_")

  testthat::expect_warning(
    BiomassBrowse$new(path = tmp_pth, Timestep = 1L),
    regexp = "LANDIS-II v8"
  )

  withr::deferred_run()
})

testthat::test_that("BiomassBrowse rejects invalid SpeciesTable shape", {
  tmp_pth <- withr::local_tempdir("test_BiomassBrowse_")

  ## missing required columns
  bad <- data.frame(Species = "abiebals", Preference = 0.5)
  suppressWarnings(testthat::expect_error(BiomassBrowse$new(
    path = tmp_pth,
    Timestep = 1L,
    SpeciesTable = bad
  )))

  ## Preference out of range
  bad2 <- data.frame(
    Species = "abiebals",
    Preference = 1.5,
    GrowthReductionThreshold = 0.5,
    GrowthReductionMaximum = 0.4,
    MortalityThreshold = 0.5,
    MortalityMaximum = 0.1
  )
  suppressWarnings(testthat::expect_error(BiomassBrowse$new(
    path = tmp_pth,
    Timestep = 1L,
    SpeciesTable = bad2
  )))

  withr::deferred_run()
})

testthat::test_that("BiomassBrowse rejects invalid enum values", {
  tmp_pth <- withr::local_tempdir("test_BiomassBrowse_")

  suppressWarnings(testthat::expect_error(BiomassBrowse$new(
    path = tmp_pth,
    Timestep = 1L,
    BrowseMethod = "Triangular"
  )))
  suppressWarnings(testthat::expect_error(BiomassBrowse$new(
    path = tmp_pth,
    Timestep = 1L,
    ForageInReachMethod = "RandomEachCohort"
  )))

  withr::deferred_run()
})

testthat::test_that("BiomassBrowse write() requires HSI inputs and population file", {
  tmp_pth <- withr::local_tempdir("test_BiomassBrowse_")

  spt <- data.frame(
    Species = "abiebals",
    Preference = 0.5,
    GrowthReductionThreshold = 0.5,
    GrowthReductionMaximum = 0.4,
    MortalityThreshold = 0.5,
    MortalityMaximum = 0.1
  )

  ## no ForageQuantity / SitePreference -> error at write() (HSI inputs missing)
  zone_map <- file.path(tmp_pth, "ecoregions.gis")
  pop_file <- file.path(tmp_pth, "DefinedUngulatePopulation.txt")
  for (f in c(zone_map, pop_file)) {
    writeLines("", f)
  }

  suppressWarnings({
    ext <- BiomassBrowse$new(
      path = tmp_pth,
      Timestep = 1L,
      SpeciesTable = spt,
      ZoneMap = zone_map,
      DefinedPopulationFile = pop_file,
      ConsumptionRate = 1000L,
      MinBrowsePropinReach = 0.5,
      BrowseBiomassThreshold = 0.05,
      EscapeBrowsePropLong = 0.57
    )
  })
  testthat::expect_error(ext$write())

  withr::deferred_run()
})

testthat::test_that("BiomassBrowse DynamicPopulation block emits required keys", {
  tmp_pth <- withr::local_tempdir("test_BiomassBrowse_")

  spt <- data.frame(
    Species = "abiebals",
    Preference = 0.5,
    GrowthReductionThreshold = 0.5,
    GrowthReductionMaximum = 0.4,
    MortalityThreshold = 0.5,
    MortalityMaximum = 0.1
  )
  zone_map <- file.path(tmp_pth, "ecoregions.gis")
  pop_file <- file.path(tmp_pth, "DefinedUngulatePopulation.txt")
  for (f in c(zone_map, pop_file)) {
    writeLines("", f)
  }

  suppressWarnings({
    ext <- BiomassBrowse$new(
      path = tmp_pth,
      Timestep = 1L,
      SpeciesTable = spt,
      ZoneMap = zone_map,
      DefinedPopulationFile = pop_file,
      DynamicPopulation = list(
        RMin = 0.0,
        RMax = 0.5,
        MortalityMin = 0.0,
        MortalityMax = 0.3,
        PredationMin = 0.0,
        PredationMax = 0.1,
        HarvestMin = 0.0,
        HarvestMax = 0.0
      ),
      ConsumptionRate = 1000L,
      MinBrowsePropinReach = 0.5,
      BrowseBiomassThreshold = 0.05,
      EscapeBrowsePropLong = 0.57,
      SitePreference = 500L
    )
  })
  ext$write()

  contents <- readLines(file.path(tmp_pth, ext$files[1]))
  testthat::expect_true(any(grepl("^DynamicPopulation", contents)))
  testthat::expect_true(any(grepl("^RMin\\s+0", contents)))
  testthat::expect_true(any(grepl("^RMax\\s+0\\.5", contents)))
  testthat::expect_true(any(grepl("^MortalityMax\\s+0\\.3", contents)))
  testthat::expect_true(any(grepl("^HarvestMax\\s+0", contents)))

  withr::deferred_run()
})
