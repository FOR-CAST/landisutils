testthat::test_that("Original Fire (and Biomass Succession) inputs are properly created", {
  testthat::skip_if_not_installed("climateR")
  testthat::skip_if_not_installed("map")
  testthat::skip_if_not_installed("SpaDES.core")
  testthat::skip_if_not_installed("withr")
  testthat::skip_if_not_installed("zonal")

  studyAreaName <- "Chine"

  d_proj <- file.path("~/GitHub/BC_HRV")
  d_ins <- file.path(d_proj, "inputs")
  d_outs <- file.path(d_proj, "outputs")
  d_runs <- file.path(
    d_outs,
    glue::glue("{studyAreaName}_landis_LH_hrv_NDTBEC_FRT_res125")
  )

  ## sim files use relative path to inputs and outputs,
  ## so make sure it points to right place e.g., during tests
  if (!dir.exists("inputs")) {
    file.symlink(d_ins, "inputs")
  }
  if (!dir.exists("outputs")) {
    file.symlink(d_outs, "outputs")
  }

  f1 <- file.path(d_runs, "simOutPreamble_Chine.rds")
  f2 <- file.path(d_runs, "simOutDataPrep_Chine.rds")

  testthat::skip_if_not(all(file.exists(f1, f2)))

  sim1 <- SpaDES.core::loadSimList(f1)
  sim2 <- SpaDES.core::loadSimList(f2)

  ## initial communities
  cohortData <- sim2[["cohortData"]]
  pixelGroupMap <- sim2[["pixelGroupMap"]]

  ## ecoregion
  ecoregion <- sim2[["ecoregion"]]
  ecoregionMap <- sim2[["ecoregionMap"]]
  ecoregionPolys <- terra::as.polygons(ecoregionMap) |>
    sf::st_as_sf()
  ecoregionPolys$ecoregion <- paste0(ecoregionPolys$ecoregion, "_81") ## append lcc code

  ## fireRegimePolys
  fireRegimePolys <- sim2[["fireRegimePolys"]]

  ## other
  minRelativeB <- sim2[["minRelativeB"]]
  species <- sim2[["species"]]
  speciesEcoregion <- sim2[["speciesEcoregion"]]
  speciesLayers <- sim2[["speciesLayers"]]
  standAgeMap <- sim2[["standAgeMap"]] |> terra::crop(speciesLayers)
  sufficientLight <- sim2[["sufficientLight"]]

  rm(sim1)
  rm(sim2)

  ## prepare landis input files ----------------------------------------------------------------

  tmp_pth <- withr::local_tempdir("test_Original_Fire_")

  fire_spp_csv_file <- prepSpeciesData(species, tmp_pth, type = "fire")

  frp_table <- prepFireRegionParametersTable(fireRegimePolys)

  fire_dmg_df <- data.frame(
    CohortAgePercentLongevity = c(20, 50, 85, 100),
    FireSeverityMinusFireTolerance = c(-2, -1, 0, 1)
  )

  ## below is e.g., boreal (stand replacing fires)
  fuel_crv_df <- data.frame(
    FireRegionName = frp_table$FireRegionName,
    S1 = rep(-1, nrow(frp_table)),
    S2 = rep(-1, nrow(frp_table)),
    S3 = rep(-1, nrow(frp_table)),
    S4 = rep(-1, nrow(frp_table)),
    S5 = rep(15, nrow(frp_table))
  )

  ifrm_file <- terra::rasterize(
    fireRegimePolys,
    standAgeMap,
    field = "PolyID",
    background = 0
  ) |>
    prepInitialFireRegionsMap(
      file = file.path(tmp_pth, "fire-regions-map.tif")
    )

  log_file <- file.path(tmp_pth, "original-fire/fire/log.csv")
  sum_log_file <- file.path(tmp_pth, "original-fire/fire/summary-log.csv")

  of_file <- OriginalFireInput(
    path = tmp_pth,

    DynamicFireRegionsTable = NULL,
    FireRegionParametersTable = frp_table,
    FireDamageTable = fire_dmg_df,
    FuelCurveTable = fuel_crv_df,
    InitialFireRegionsMap = ifrm_file,
    LogFile = log_file,
    Species_CSV_File = fire_spp_csv_file,
    SummaryLogFile = sum_log_file,
    Timestep = 1,
    WindCurveTable = NULL
  )

  testthat::expect_true(file.exists(of_file))

  ## prepare landis scenario file --------------------------------------------------------------

  er_files <- prepEcoregionsFiles(
    ecoregion = ecoregion,
    ecoregionMap = ecoregionMap,
    path = tmp_pth
  )

  testthat::expect_true(all(file.exists(er_files)))

  core_spp_file <- prepSpeciesData(species, tmp_pth, type = "core")

  testthat::expect_true(file.exists(core_spp_file))

  skip() ## TODO: need Biomass Succession pieces

  scenario_name <- glue::glue("scenario_{studyAreaName}")
  scenario_file <- scenario(
    name = scenario_name,
    extensions = list(
      succession = c("Biomass Succession" = bse_file),
      disturbance = c("Original Fire" = of_file)
    ),
    path = tmp_pth,

    ## additional arguments
    CellLength = terra::res(ecoregionMap)[1],
    DisturbancesRandomOrder = FALSE,
    Duration = 20, ## TODO: longer sims for production runs; get sim times from simList
    EcoregionsFiles = er_files,
    RandomNumberSeed = NULL, ## optional
    SpeciesInputFile = core_spp_file
  )

  testthat::expect_true(file.exists(scenario_file))

  # rep_files <- replicate(scenario_file, reps = 3)
  #
  # testthat::expect_true(all(file.exists(rep_files)))

  ## run the landis scenario -------------------------------------------------------------------
  testthat::skip_if_not(nzchar(landis_find()))
  ## TODO
  landis_run(scenario_file)

  withr::deferred_run()
})
