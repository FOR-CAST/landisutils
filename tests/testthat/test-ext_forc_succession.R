testthat::test_that("Forest Carbon Succession (ForCS) inputs are properly created", {
  ## NOTE: sample values follow ForCS v4.0.2 (Sep 2025) user guide and the
  ## upstream v8 test scenario.
  ## <https://github.com/LANDIS-II-Foundation/Extension-ForCS-Succession/blob/master/deploy/installer/LANDIS-II%20CForC%20Succession%20v4.0.2%20User%20Guide%20September%202025.pdf>
  ## <https://github.com/LANDIS-II-Foundation/Extension-ForCS-Succession/blob/master/testing/v8%20Scenario/FORC-successionv4simpleroots.txt>

  tmp_pth <- withr::local_tempdir("test_ForCS_")

  clim_file <- prepClimateFile(
    df = tibble::tribble(
      ~year , ~ecoregion , ~avg_temp ,
          0 , "eco1"     , 5         ,
          0 , "eco2"     , 2         ,
          5 , "eco1"     , 5.5       ,
          5 , "eco2"     , 2.5       ,
         10 , "eco1"     , 6         ,
         10 , "eco2"     , 3         ,
         20 , "eco1"     , 7         ,
         20 , "eco2"     , 5
    ),
    path = tmp_pth,
    filename = "ForCS_climate.txt"
  )

  disturb_fire_dom <- tibble::tribble(
    ~intensity , ~from_dom , ~to_air , ~to_dom , ~to_fps ,
             1 ,         1 , 0.5     , 0.0     , 0.0     ,
             1 ,         3 , 0.2     , 0.0     , 0.0     ,
             2 ,         1 , 0.5     , 0.0     , 0.0     ,
             2 ,         2 , 0.25    , 0.0     , 0.0     ,
             2 ,         3 , 0.35    , 0.0     , 0.0     ,
             2 ,         8 , 0.0     , 0.5     , 0.0     ,
             2 ,         9 , 0.0     , 1.0     , 0.0     ,
             3 ,         1 , 0.65    , 0.0     , 0.0     ,
             3 ,         2 , 0.35    , 0.0     , 0.0     ,
             3 ,         3 , 0.4     , 0.0     , 0.0     ,
             3 ,         8 , 0.0     , 0.75    , 0.0     ,
             3 ,         9 , 0.5     , 0.5     , 0.0     ,
             4 ,         1 , 1.0     , 0.0     , 0.0     ,
             4 ,         2 , 0.5     , 0.0     , 0.0     ,
             4 ,         3 , 0.4     , 0.0     , 0.0     ,
             4 ,         5 , 0.1     , 0.0     , 0.0     ,
             4 ,         8 , 0.0     , 1.0     , 0.0     ,
             4 ,         9 , 0.7     , 0.3     , 0.0     ,
             5 ,         1 , 1.0     , 0.0     , 0.0     ,
             5 ,         2 , 0.65    , 0.0     , 0.0     ,
             5 ,         3 , 0.45    , 0.0     , 0.0     ,
             5 ,         5 , 0.1     , 0.0     , 0.0     ,
             5 ,         8 , 0.1     , 0.9     , 0.0     ,
             5 ,         9 , 0.7     , 0.3     , 0.0
  )

  disturb_other_dom <- tibble::tribble(
    ~disturb_type , ~from_dom , ~to_air , ~to_dom , ~to_fps ,
    "Clearcut"    ,         1 , 0.0     , 0.0     , 0.0     ,
    "Clearcut"    ,         8 , 0.0     , 0.4     , 0.6     ,
    "wind"        ,         8 , 0.0     , 0.8     , 0.0     ,
    "wind"        ,         9 , 0.0     , 1.0     , 0.0
  )

  disturb_fire_biomass = tibble::tribble(
    ~intensity , ~from_biomass , ~to_air , ~to_fps , ~to_dom ,
             1 ,             1 , 0       ,       0 , 1       ,
             1 ,             2 , 0.5     ,       0 , 0.5     ,
             1 ,             3 , 0       ,       0 , 1       ,
             1 ,             5 , 0       ,       0 , 1       ,
             1 ,             6 , 0       ,       0 , 1       ,
             2 ,             1 , 0       ,       0 , 1       ,
             2 ,             2 , 0.7     ,       0 , 0.3     ,
             2 ,             3 , 0.1     ,       0 , 0.9     ,
             2 ,             5 , 0       ,       0 , 1       ,
             2 ,             6 , 0       ,       0 , 1       ,
             3 ,             1 , 0       ,       0 , 1       ,
             3 ,             2 , 0.75    ,       0 , 0.25    ,
             3 ,             3 , 0.1     ,       0 , 0.9     ,
             3 ,             5 , 0       ,       0 , 1       ,
             3 ,             6 , 0       ,       0 , 1       ,
             4 ,             1 , 0       ,       0 , 1       ,
             4 ,             2 , 1       ,       0 , 0       ,
             4 ,             3 , 0.2     ,       0 , 0.8     ,
             4 ,             5 , 0       ,       0 , 1       ,
             4 ,             6 , 0       ,       0 , 1       ,
             5 ,             1 , 0       ,       0 , 1       ,
             5 ,             2 , 1       ,       0 , 0       ,
             5 ,             3 , 0.3     ,       0 , 0.7     ,
             5 ,             5 , 0       ,       0 , 1       ,
             5 ,             6 , 0       ,       0 , 1
  )

  disturb_other_biomass = tibble::tribble(
    ~disturb_type , ~from_biomass , ~to_air , ~to_fps , ~to_dom ,
    "Clearcut"    ,             1 , 0       ,       1 , 0       ,
    "Clearcut"    ,             2 , 0       ,       0 , 1       ,
    "Clearcut"    ,             3 , 0       ,       0 , 1       ,
    "Clearcut"    ,             5 , 0       ,       0 , 1       ,
    "Clearcut"    ,             6 , 0       ,       0 , 1       ,
    "wind"        ,             1 , 0       ,       0 , 1       ,
    "wind"        ,             2 , 0       ,       0 , 1       ,
    "wind"        ,             3 , 0       ,       0 , 1       ,
    "wind"        ,             5 , 0       ,       0 , 1       ,
    "wind"        ,             6 , 0       ,       0 , 1       ,
    "bda"         ,             1 , 0       ,       0 , 1       ,
    "bda"         ,             2 , 0.3     ,       0 , 0.7     ,
    "bda"         ,             3 , 0.2     ,       0 , 0.8     ,
    "bda"         ,             5 , 0       ,       0 , 1       ,
    "bda"         ,             6 , 0       ,       0 , 1
  )

  dm_file <- prepDisturbanceMatrixFile(
    DisturbFireTransferDOM = disturb_fire_dom,
    DisturbOtherTransferDOM = disturb_other_dom,
    DisturbFireTransferBiomass = disturb_fire_biomass,
    DisturbOtherTransferBiomass = disturb_other_biomass,
    path = tmp_pth,
    filename = "ForCS_DM.txt"
  )

  snag_file <- prepSnagFile(
    tibble::tribble(
      ~species   , ~age_at_death , ~time_since_death , ~cause  ,
      "querelli" ,            40 ,                 1 , "other" ,
      "pinubank" ,            88 ,                12 , "wind"
    ),
    path = tmp_pth,
    filename = "ForCS_snags.txt"
  )

  ## don't need working files, they just need to exist
  init_comm_files <- c(
    file.path(tmp_pth, "initial-communities.csv"),
    file.path(tmp_pth, "initial-communities.gis")
  )
  purrr::walk2(.x = rep("", length(init_comm_files)), .y = init_comm_files, .f = writeLines)

  output_tables <- data.frame(Biomass = 1, DOM_Pools = 1, Fluxes = 1, Summary = 1)

  for_cs_map_control <- data.frame(
    BiomassC = 1, SDOMC = 1, NBP = 1, NEP = 1, NPP = 1, RH = 1, ToFPS = 1
  )

  spin_up <- data.frame(Flag = 1, BiomassSpinUpFlag = 1, Tolerance = 1.0, MaxIter = 20)

  avail_light_biomass <- data.frame(
    Class = 1L:5L,
    eco1 = c(30, 35, 55, 80, 100),
    eco2 = c(30, 35, 55, 80, 100)
  )

  light_est <- data.frame(
    class = 1L:5L,
    X0 = c(1.0, 0.5, 0.0, 0.0, 0.0),
    X1 = c(0.0, 1.0, 0.5, 0.0, 0.0),
    X2 = c(0.0, 0.0, 1.0, 0.5, 0.0),
    X3 = c(0.0, 0.0, 0.5, 1.0, 0.5),
    X4 = c(0.0, 0.0, 0.0, 0.0, 1.0),
    X5 = c(0.0, 0.0, 0.0, 0.0, 0.5)
  )

  species_params <- tibble::tribble(
    ~species , ~leaf_long , ~mort_shp , ~merch_min_age , ~merch_a , ~merch_b , ~prop_non_merch , ~growth_shp , ~shade_tol , ~fire_tol ,
    "pinubank" , 3.0 , 10 , 5 , 0.7546 , 0.983 , 0.25 , 0.9 , 1L , 2L ,
    "querelli" , 1.0 , 10 , 5 , 0.7546 , 0.983 , 0.25 , 0.9 , 2L , 4L
  )

  dom_pools <- tibble::tribble(
    ~id , ~name                   , ~prop_to_atmosphere ,
      1 , "Very Fast Aboveground" , 0.815               ,
      2 , "Very Fast Belowground" , 0.83                ,
      3 , "Fast Aboveground"      , 0.83                ,
      4 , "Fast Belowground"      , 0.83                ,
      5 , "Medium"                , 0.83                ,
      6 , "Slow Aboveground"      , 0.83                ,
      7 , "Slow Belowground"      , 0.83                ,
      8 , "Stem Snag"             , 0.83                ,
      9 , "Other Snag"            , 0.83                ,
     10 , "Extra pool"            , 0.83
  )

  ## EcoSppDOMParameters: per v4.0.2 user guide §3 sample input
  ecosppdom_pinubank <- tibble::tribble(
    ~dom_pool , ~decay_rate , ~amt_t0 , ~q10 ,
            1 , 0.355       ,    1.49 , 2.65 ,
            2 , 0.5         ,    0.07 , 2    ,
            3 , 0.1435      ,  158.48 , 2    ,
            4 , 0.0374      ,  288.71 , 2    ,
            5 , 0.015       , 1349.40 , 2    ,
            6 , 0.0033      , 1927.71 , 2    ,
            7 , 0.0187      ,  851.21 , 2    ,
            8 , 0.07175     ,  314.88 , 2    ,
            9 , 0.07        ,   45.53 , 2    ,
           10 , 0           ,    0.00 , 2
  )
  ecosppdom_querelli <- tibble::tribble(
    ~dom_pool , ~decay_rate , ~amt_t0 , ~q10 ,
            1 , 0.355       ,    0.34 , 2.65 ,
            2 , 0.5         ,    0.02 , 2    ,
            3 , 0.1435      ,    5.15 , 2    ,
            4 , 0.0374      ,  143.23 , 2    ,
            5 , 0.015       , 2476.27 , 2    ,
            6 , 0.0033      , 4075.40 , 2    ,
            7 , 0.0187      , 2339.42 , 2    ,
            8 , 0.07175     ,    7.45 , 2    ,
            9 , 0.07        ,    1.97 , 2    ,
           10 , 0           ,    0.00 , 2
  )
  ecosppdom_params <- dplyr::bind_rows(
    dplyr::mutate(ecosppdom_pinubank, ecoregion = "eco1", species = "pinubank"),
    dplyr::mutate(ecosppdom_pinubank, ecoregion = "eco2", species = "pinubank"),
    dplyr::mutate(ecosppdom_querelli, ecoregion = "eco1", species = "querelli"),
    dplyr::mutate(ecosppdom_querelli, ecoregion = "eco2", species = "querelli")
  ) |>
    dplyr::select(ecoregion, species, dom_pool, decay_rate, amt_t0, q10)

  forcs_props <- data.frame(
    BiomassFine = 0.5,
    BiomassCoarse = 0.5,
    AnnualSlowAGtoSlowBG = 0.006,
    AnnualStemSnagToMedium = 0.032,
    AnnualBranchSnagToFastAG = 0.1
  )

  anpp_timeseries <- tibble::tribble(
    ~year , ~ecoregion , ~species   , ~anpp , ~anpp_std ,
        0 , "eco1"     , "pinubank" ,   648 ,       0.1 ,
        0 , "eco1"     , "querelli" ,  1415 ,       0.1 ,
        0 , "eco2"     , "pinubank" ,   648 ,       0.1 ,
        0 , "eco2"     , "querelli" ,  1415 ,       0.1
  )

  maxb_timeseries <- tibble::tribble(
    ~year , ~ecoregion , ~species   , ~maxb ,
        0 , "eco1"     , "pinubank" , 15000 ,
        0 , "eco1"     , "querelli" , 25000 ,
        0 , "eco2"     , "pinubank" , 15000 ,
        0 , "eco2"     , "querelli" , 25000
  )

  est_prob <- tibble::tribble(
    ~year , ~ecoregion , ~species   , ~probability ,
        0 , "eco1"     , "pinubank" , 0.1          ,
        0 , "eco1"     , "querelli" , 0.1          ,
        0 , "eco2"     , "pinubank" , 0.1          ,
        0 , "eco2"     , "querelli" , 0.1
  )

  root_dynamics <- tibble::tribble(
    ~ecoregion , ~species   , ~min_abio , ~root_abio , ~prop_fine_rt , ~fr_turnover , ~cr_turnover ,
    "eco1"     , "pinubank" ,         0 , 0.399      , 0.18          , 0.6          , 0.02         ,
    "eco1"     , "querelli" ,         0 , 0.403      , 0.18          , 1            , 0.02         ,
    "eco2"     , "pinubank" ,         0 , 0.399      , 0.18          , 0.6          , 0.02         ,
    "eco2"     , "querelli" ,         0 , 0.403      , 0.18          , 1            , 0.02
  )

  ext_forcs <- ForCS$new(
    path = tmp_pth,
    Timestep = 1,
    SeedingAlgorithm = "WardSeedDispersal",
    ForCSClimateFile = clim_file,
    InitialCommunitiesFiles = init_comm_files,
    DisturbanceMatrixFile = dm_file,
    SnagFile = snag_file,
    OutputTables = output_tables,
    ForCSMapControl = for_cs_map_control,
    MapOutputInterval = 1,
    SpinUp = spin_up,
    AvailableLightBiomass = avail_light_biomass,
    LightEstablishmentTable = light_est,
    SpeciesParameters = species_params,
    DOMPools = dom_pools,
    EcoSppDOMParameters = ecosppdom_params,
    ForCSProportions = forcs_props,
    ANPPTimeSeries = anpp_timeseries,
    MaxBiomassTimeSeries = maxb_timeseries,
    EstablishProbabilities = est_prob,
    RootDynamics = root_dynamics
  )

  ext_forcs$write()

  testthat::expect_true(all(file.exists(file.path(tmp_pth, ext_forcs$files))))

  contents <- readLines(file.path(tmp_pth, ext_forcs$files[1]))
  testthat::expect_true(any(grepl("^LandisData\\s+\"ForC Succession\"", contents)))
  testthat::expect_true(any(grepl("^SeedingAlgorithm\\s+WardSeedDispersal", contents)))
  testthat::expect_true(any(grepl("^ForCSClimateFile\\s+", contents)))
  testthat::expect_true(any(grepl("^ForCSMapControl", contents)))
  testthat::expect_true(any(grepl("^MapOutputInterval\\s+1", contents)))
  testthat::expect_true(any(grepl("^SpinUp", contents)))
  testthat::expect_true(any(grepl("^SpeciesParameters", contents)))

  withr::deferred_run()
})

testthat::test_that("ForCS can be written without a SnagFile", {
  tmp_pth <- withr::local_tempdir("test_ForCS_")

  clim_file <- prepClimateFile(
    df = tibble::tribble(
      ~year , ~ecoregion , ~avg_temp ,
          0 , "eco1"     , 5
    ),
    path = tmp_pth,
    filename = "ForCS_climate.txt"
  )

  dm_file <- prepDisturbanceMatrixFile(
    DisturbFireTransferDOM = data.frame(intensity = 1, from_dom = 1, to_air = 0, to_dom = 0, to_fps = 0),
    DisturbOtherTransferDOM = data.frame(disturb_type = "wind", from_dom = 8, to_air = 0, to_dom = 1, to_fps = 0),
    DisturbFireTransferBiomass = data.frame(intensity = 1, from_biomass = 1, to_air = 0, to_fps = 0, to_dom = 1),
    DisturbOtherTransferBiomass = data.frame(disturb_type = "wind", from_biomass = 1, to_air = 0, to_fps = 0, to_dom = 1),
    path = tmp_pth,
    filename = "ForCS_DM.txt"
  )

  init_comm_files <- c(
    file.path(tmp_pth, "initial-communities.csv"),
    file.path(tmp_pth, "initial-communities.gis")
  )
  purrr::walk2(.x = rep("", length(init_comm_files)), .y = init_comm_files, .f = writeLines)

  ext_forcs <- ForCS$new(
    path = tmp_pth,
    Timestep = 1,
    SeedingAlgorithm = "WardSeedDispersal",
    ForCSClimateFile = clim_file,
    InitialCommunitiesFiles = init_comm_files,
    DisturbanceMatrixFile = dm_file,
    SnagFile = NULL,
    OutputTables = data.frame(Biomass = 1, DOM_Pools = 1, Fluxes = 1, Summary = 1),
    ForCSMapControl = data.frame(BiomassC = 1, SDOMC = 1, NBP = 1, NEP = 1, NPP = 1, RH = 1, ToFPS = 1),
    MapOutputInterval = 1,
    SpinUp = data.frame(Flag = 1, BiomassSpinUpFlag = 1, Tolerance = 1.0, MaxIter = 20),
    AvailableLightBiomass = data.frame(
      Class = 1L:5L,
      eco1 = c(30, 35, 55, 80, 100),
      eco2 = c(30, 35, 55, 80, 100)
    ),
    LightEstablishmentTable = data.frame(
      class = 1L:5L,
      X0 = c(1, 0.5, 0, 0, 0),
      X1 = c(0, 1, 0.5, 0, 0),
      X2 = c(0, 0, 1, 0.5, 0),
      X3 = c(0, 0, 0.5, 1, 0.5),
      X4 = c(0, 0, 0, 0, 1),
      X5 = c(0, 0, 0, 0, 0.5)
    ),
    SpeciesParameters = data.frame(
      species = "pinubank",
      leaf_long = 3.0, mort_shp = 10, merch_min_age = 5,
      merch_a = 0.7546, merch_b = 0.983,
      prop_non_merch = 0.25, growth_shp = 0.9,
      shade_tol = 1L, fire_tol = 2L
    ),
    DOMPools = data.frame(
      id = 1:10,
      name = c(
        "Very Fast Aboveground", "Very Fast Belowground", "Fast Aboveground",
        "Fast Belowground", "Medium", "Slow Aboveground", "Slow Belowground",
        "Stem Snag", "Other Snag", "Extra pool"
      ),
      prop_to_atmosphere = c(0.815, rep(0.83, 9))
    ),
    EcoSppDOMParameters = data.frame(
      ecoregion = "eco1", species = "pinubank",
      dom_pool = 1:10,
      decay_rate = c(0.355, 0.5, 0.1435, 0.0374, 0.015, 0.0033, 0.0187, 0.07175, 0.07, 0),
      amt_t0 = c(1.49, 0.07, 158.48, 288.71, 1349.40, 1927.71, 851.21, 314.88, 45.53, 0),
      q10 = c(2.65, rep(2, 9))
    ),
    ForCSProportions = data.frame(
      BiomassFine = 0.5, BiomassCoarse = 0.5,
      AnnualSlowAGtoSlowBG = 0.006, AnnualStemSnagToMedium = 0.032,
      AnnualBranchSnagToFastAG = 0.1
    ),
    ANPPTimeSeries = data.frame(
      year = 0, ecoregion = "eco1", species = "pinubank", anpp = 648, anpp_std = 0.1
    ),
    MaxBiomassTimeSeries = data.frame(
      year = 0, ecoregion = "eco1", species = "pinubank", maxb = 15000
    ),
    EstablishProbabilities = data.frame(
      year = 0, ecoregion = "eco1", species = "pinubank", probability = 0.1
    ),
    RootDynamics = data.frame(
      ecoregion = "eco1", species = "pinubank",
      min_abio = 0, root_abio = 0.399, prop_fine_rt = 0.18,
      fr_turnover = 0.6, cr_turnover = 0.02
    )
  )

  testthat::expect_no_error(ext_forcs$write())

  contents <- readLines(file.path(tmp_pth, ext_forcs$files[1]))
  testthat::expect_false(any(grepl("^SnagFile", contents)))

  withr::deferred_run()
})
