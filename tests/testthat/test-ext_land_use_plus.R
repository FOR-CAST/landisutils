testthat::test_that("Land Use Plus extension is properly created", {
  ## NOTE: reference values from the LANDIS-II Core8 / Land Use Plus v4 test input:
  ## testing/Core8.0-LUC4.0/land-use-change-inputs.txt
  ## <https://github.com/LANDIS-II-Foundation/Extension-Land-Use-Plus/tree/master/testing/Core8.0-LUC4.0>

  tmp_pth <- withr::local_tempdir("test_LandUsePlus_")

  forest_lu <- landUseType(
    name = "forest",
    mapCode = 4L,
    allowHarvest = "no",
    changes = list(landCoverChange(type = "NoChange"))
  )

  fips_harvest_lu <- landUseType(
    name = "FIPSandHarvest",
    mapCode = 3L,
    allowHarvest = "yes",
    changes = list(
      landCoverChange(
        type = "RemoveTrees",
        repeatHarvest = TRUE,
        cohorts = list(
          cohortSelector(
            species = "acerrubr",
            ranges = data.frame(low = 1, high = 200, percent = 20)
          ),
          cohortSelector(
            species = "pinustro",
            ranges = data.frame(low = 50, high = 300, percent = 30)
          )
        )
      ),
      landCoverChange(
        type = "InsectDefoliation",
        cohorts = cohortSelector(
          species = "querrubr",
          ranges = data.frame(low = 1, high = 300, percent = 60)
        )
      )
    )
  )

  multi_range_lu <- landUseType(
    name = "FIPS",
    mapCode = 1L,
    allowHarvest = "yes",
    changes = list(landCoverChange(
      type = "InsectDefoliation",
      cohorts = list(
        cohortSelector(
          species = "querrubr",
          ranges = data.frame(low = 1, high = 300, percent = 75)
        ),
        cohortSelector(
          species = "pinustro",
          ranges = data.frame(low = c(1, 71), high = c(62, 200), percent = c(20, 25))
        )
      )
    ))
  )

  ext <- LandUsePlus$new(
    path = tmp_pth,
    Timestep = 1L,
    InputMaps = "landuse-{timestep}.tif",
    SiteLog = "land-use/site-log.csv",
    LandUses = list(forest_lu, fips_harvest_lu, multi_range_lu)
  )
  ext$write()

  testthat::expect_true(all(file.exists(file.path(tmp_pth, ext$files))))

  contents <- readLines(file.path(tmp_pth, ext$files[1]))
  testthat::expect_true(any(grepl("^LandisData\\s+\"Land Use\"", contents)))
  testthat::expect_true(any(grepl("^Timestep\\s+1", contents)))
  testthat::expect_true(any(grepl("^InputMaps\\s+landuse-\\{timestep\\}\\.tif$", contents)))
  testthat::expect_true(any(grepl("site-log\\.csv", contents)))

  testthat::expect_true(any(grepl("^LandUse\\s+\"forest\"", contents)))
  testthat::expect_true(any(grepl("^MapCode\\s+4", contents)))
  testthat::expect_true(any(grepl("^AllowHarvest\\?\\s+no", contents)))
  testthat::expect_true(any(grepl("^LandCoverChange\\s+NoChange", contents)))

  testthat::expect_true(any(grepl("^LandUse\\s+\"FIPSandHarvest\"", contents)))
  testthat::expect_true(any(grepl("^LandCoverChange\\s+RemoveTrees", contents)))
  testthat::expect_true(any(grepl("^RepeatHarvest\\?\\s+yes", contents)))
  testthat::expect_true(any(grepl("acerrubr 1-200\\(20%\\)", contents)))
  testthat::expect_true(any(grepl("pinustro 50-300\\(30%\\)", contents)))
  testthat::expect_true(any(grepl("^LandCoverChange\\s+InsectDefoliation", contents)))

  ## multi-range cohort selector
  testthat::expect_true(any(grepl("pinustro 1-62\\(20%\\) 71-200\\(25%\\)", contents)))

  withr::deferred_run()
})

testthat::test_that("LandUsePlus rejects duplicate LandUse names or mapCodes", {
  tmp_pth <- withr::local_tempdir("test_LandUsePlus_")

  lu1 <- landUseType(
    name = "forest",
    mapCode = 1L,
    allowHarvest = "no",
    changes = landCoverChange(type = "NoChange")
  )
  lu2 <- landUseType(
    name = "forest",
    mapCode = 2L,
    allowHarvest = "no",
    changes = landCoverChange(type = "NoChange")
  )

  ext <- LandUsePlus$new(path = tmp_pth, Timestep = 1L, LandUses = list(lu1, lu2))
  testthat::expect_error(ext$write())

  withr::deferred_run()
})

testthat::test_that("landCoverChange enforces type / cohort consistency", {
  testthat::expect_error(landCoverChange(type = "Foo"))
  testthat::expect_error(landCoverChange(
    type = "RemoveTrees" ## missing cohorts
  ))
  testthat::expect_error(landCoverChange(
    type = "NoChange",
    cohorts = cohortSelector("acerrubr", data.frame(low = 1, high = 10, percent = 50))
  ))
})

testthat::test_that("cohortSelector rejects bad ranges", {
  testthat::expect_error(cohortSelector(
    "acerrubr",
    data.frame(low = 100, high = 10, percent = 50) ## low > high
  ))
  testthat::expect_error(cohortSelector(
    "acerrubr",
    data.frame(low = 1, high = 10, percent = 150) ## percent out of range
  ))
})

testthat::test_that("LandUsePlus$InputMaps requires `{timestep}` and `.tif`", {
  tmp_pth <- withr::local_tempdir("test_LandUsePlus_")

  ## missing {timestep} placeholder
  testthat::expect_error(
    LandUsePlus$new(path = tmp_pth, Timestep = 1L, InputMaps = "landuse-0.tif"),
    "\\{timestep\\}"
  )

  ## missing .tif extension
  testthat::expect_error(
    LandUsePlus$new(path = tmp_pth, Timestep = 1L, InputMaps = "landuse-{timestep}.img"),
    "\\.tif"
  )
  testthat::expect_error(
    LandUsePlus$new(path = tmp_pth, Timestep = 1L, InputMaps = "landuse-{timestep}"),
    "\\.tif"
  )

  ## case-insensitive `.TIF` is accepted
  testthat::expect_no_error(LandUsePlus$new(
    path = tmp_pth,
    Timestep = 1L,
    InputMaps = "landuse-{timestep}.TIF"
  ))

  withr::deferred_run()
})
