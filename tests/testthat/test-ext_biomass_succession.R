testthat::test_that("Biomass Succession inputs are properly created", {
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
  sufficientLight <- sim2[["sufficientLight"]]

  rm(sim1)
  rm(sim2)

  ## prepare landis input files ----------------------------------------------------------------
  tmp_pth <- withr::local_tempdir("test_Biomass_Succession_")

  ## climate data
  clim_file <- file.path(tmp_pth, "climate-data-daily.csv")
  clim_vars <- c("prcp", "tmax", "tmin")
  clim_years <- 2011:2012 ## availability is 1980 to last-year

  daily_weather <- purrr::map(
    .x = clim_vars,
    .f = prep_daily_weather,
    studyArea = ecoregionPolys,
    id = "ecoregion",
    start = glue::glue("{head(clim_years, 1)}-01-01"),
    end = glue::glue("{tail(clim_years, 1)}-12-31")
  ) |>
    purrr::list_rbind()

  writeClimateData(daily_weather, clim_file)

  testthat::expect_true(file.exists(clim_file))

  cc_file <- prepClimateConfigFile(
    path = tmp_pth,
    ClimateTimeSeries = "Daily_RandomYears",
    ClimateFile = basename(clim_file),
    SpinUpClimateTimeSeries = "Daily_RandomYears",
    SpinUpClimateFile = basename(clim_file),
    GenerateClimateOutputFiles = "yes",
    UsingFireClimate = "no" ## TODO: allow 'yes' (need springstart/winterstart)
  )

  testthat::expect_true(file.exists(cc_file))

  aet_df <- purrr::map(
    .x = "aet",
    .f = prep_monthly_weather,
    studyArea = ecoregionPolys,
    id = "ecoregion",
    start = glue::glue("{head(clim_years, 1)}-01-01"),
    end = glue::glue("{tail(clim_years, 1)}-12-31")
  ) |>
    purrr::list_rbind() |>
    dplyr::filter(Year <= tail(clim_years, 1)) ## match end year

  erp_df <- prepEcoregionParameters(aet_df)

  frp_df <- prepFireReductionParameters(NULL) ## NULL uses dummy defaults

  hrp_df <- prepHarvestReductionParameters(NULL) ## ## NULL uses dummy defaults

  ic_objs <- simplifyCohorts(cohortData, pixelGroupMap, ageBin = 20)

  ic_files <- prepInitialCommunities(
    cohortData = ic_objs[[1]],
    pixelGroupMap = ic_objs[[2]],
    path = tmp_pth
  )

  testthat::expect_true(all(file.exists(ic_files)))

  mrb_df <- prepMinRelativeBiomass(minRelativeB)

  spp_file <- prepSpeciesData(species, tmp_pth, type = "succession")

  testthat::expect_true(file.exists(spp_file))

  spperd_file <- prepSpeciesEcoregionDataFile(speciesEcoregion, tmp_pth)

  testthat::expect_true(all(file.exists(spperd_file)))

  sfl_df <- sufficientLight

  bse_file <- BiomassSuccessionInput(
    path = tmp_pth,
    CalibrateMode = FALSE,
    ClimateConfigFile = cc_file,
    EcoregionParameters = erp_df,
    FireReductionParameters = frp_df,
    HarvestReductionParameters = hrp_df,
    InitialCommunitiesFiles = ic_files,
    MinRelativeBiomass = mrb_df,
    SeedingAlgorithm = "WardSeedDispersal",
    SpeciesDataFile = spp_file,
    SpeciesEcoregionDataFile = spperd_file,
    SufficientLight = sfl_df,
    Timestep = 10
  )

  ## prepare landis scenario file --------------------------------------------------------------

  er_files <- prepEcoregionsFiles(
    ecoregion = ecoregion,
    ecoregionMap = ecoregionMap,
    path = tmp_pth
  )

  testthat::expect_true(all(file.exists(er_files)))

  core_spp_file <- prepSpeciesData(species, tmp_pth, type = "core")

  testthat::expect_true(file.exists(core_spp_file))

  scenario_name <- glue::glue("scenario_{studyAreaName}")
  scenario_file <- scenario(
    name = scenario_name,
    extensions = list(
      succession = c("Biomass Succession" = bse_file)
    ),
    path = tmp_pth,

    ## additional arguments
    CellLength = terra::res(ecoregionMap)[1],
    DisturbancesRandomOrder = FALSE,
    Duration = 20,
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
