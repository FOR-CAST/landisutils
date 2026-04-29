testthat::test_that("Output Biomass Reclass inputs are properly created", {
  ## NOTE: values from the upstream reference input:
  ## testings/Core8-BiomassReclass4.0/output_BiomassReclass.txt
  ## <https://github.com/LANDIS-II-Foundation/Extension-Output-Biomass-Reclass/tree/master/testings/Core8-BiomassReclass4.0>

  tmp_pth <- withr::local_tempdir("test_OutputBiomassReclass_")

  reclass_maps <- list(
    reclass1 = list(
      MapleHardwood = c("acersacc", "betualle", "abiebals", "acerrubr", "fraxamer", "pinubank"),
      NorthernPines = c("pinubank", "pinuresi", "pinustro"),
      Oaks          = c("querelli", "querrubr"),
      OtherConifers = c("piceglau", "thujocci"),
      Other         = c("poputrem", "betupapy")
    ),
    reclass2 = list(
      MapleHardwood = c("acersacc", "betualle", "acerrubr", "fraxamer"),
      NorthernPines = c("pinubank", "pinuresi", "pinustro"),
      OtherConifers = c("piceglau", "thujocci", "abiebals"),
      Other         = c("poputrem", "betupapy", "querelli", "querrubr")
    )
  )

  map_pattern <- "outputs/biomass-reclass/biomass-reclass-{reclass-map-name}-{timestep}.tif"

  ext_bmr <- OutputBiomassReclass$new(
    path = tmp_pth,
    Timestep = 15L,
    ReclassMaps = reclass_maps,
    MapFileNames = map_pattern
  )

  ext_bmr$write()

  testthat::expect_true(all(file.exists(file.path(tmp_pth, ext_bmr$files))))

  contents <- readLines(file.path(tmp_pth, ext_bmr$files[1]))

  testthat::expect_true(any(grepl("^LandisData\\s+\"Output Biomass Reclass\"", contents)))
  testthat::expect_true(any(grepl("^Timestep\\s+15", contents)))
  testthat::expect_true(any(grepl("^ReclassMaps", contents)))

  ## reclassification headers (with the '->' separator) should be present
  testthat::expect_true(any(grepl("reclass1\\s*->\\s*MapleHardwood", contents)))
  testthat::expect_true(any(grepl("reclass2\\s*->\\s*MapleHardwood", contents)))

  ## forest-type / species rows should be present for both reclassifications
  testthat::expect_true(any(grepl(
    "MapleHardwood\\s+acersacc\\s+betualle\\s+abiebals\\s+acerrubr\\s+fraxamer\\s+pinubank",
    contents
  )))
  testthat::expect_true(any(grepl("NorthernPines\\s+pinubank\\s+pinuresi\\s+pinustro", contents)))
  testthat::expect_true(any(grepl("Oaks\\s+querelli\\s+querrubr", contents)))
  testthat::expect_true(any(grepl("OtherConifers\\s+piceglau\\s+thujocci\\s+abiebals", contents)))
  testthat::expect_true(any(grepl(
    "Other\\s+poputrem\\s+betupapy\\s+querelli\\s+querrubr",
    contents
  )))

  ## MapFileNames pattern is written verbatim
  testthat::expect_true(any(grepl("^MapFileNames", contents)))
  testthat::expect_true(any(grepl(
    "biomass-reclass-\\{reclass-map-name\\}-\\{timestep\\}\\.tif",
    contents
  )))

  ## invalid MapFileNames (missing required placeholders) should error
  testthat::expect_error(
    OutputBiomassReclass$new(
      path = tmp_pth,
      Timestep = 15L,
      ReclassMaps = reclass_maps,
      MapFileNames = "outputs/biomass-reclass/bad-pattern.tif"
    )
  )

  withr::deferred_run()
})
