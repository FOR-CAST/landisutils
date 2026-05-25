testthat::test_that("Climate BDA inputs are properly created", {
  ## NOTE: reference values adapted from the LANDIS-II Core8 / Climate BDA v5 test input:
  ## testings/Core8-ClimateBDA5.0/Climate-BDA_Agent_BUDWORM.txt
  ## <https://github.com/LANDIS-II-Foundation/Extension-Base-BDA/tree/master/testings/Core8-ClimateBDA5.0>

  tmp_pth <- withr::local_tempdir("test_ClimateBDA_")

  sp <- tibble::tribble(
    ~Species        , ~MinorHostAge     , ~MinorHostSRDProb , ~SecondHostAge  , ~SecondHostSRDProb ,
    ~MajorHostAge   , ~MajorHostSRDProb , ~Class3Age        , ~Class3VulnProb , ~Class2Age         ,
    ~Class2VulnProb , ~Class1Age        , ~Class1VulnProb   , ~CFSConifer     ,
    "abiebals"      ,                 0 , 0.25              ,              20 , 0.5                , 40 , 1.0 , 11 , 1.0 , 20 , 1.0  , 50 , 1.0  , "yes" ,
    "piceglau"      ,                 0 , 0.25              ,              20 , 0.5                , 40 , 1.0 ,  0 , 0   , 20 , 0.15 , 50 , 0.42 , "yes"
  )

  agent <- bdaAgent(
    name = "budworm",
    BDPCalibrator = 1,
    SRDMode = "mean",
    OutbreakPattern = "CyclicNormal",
    Mean = 33.5,
    StDev = 10.6,
    TimeSinceLastEpidemic = 10L,
    TemporalType = "variablepulse",
    MinROS = 0L,
    MaxROS = 3L,
    Dispersal = "no",
    EpidemicThresh = 0.5,
    InitialEpicenterNum = 0L,
    OutbreakEpicenterCoeff = 0.01,
    OutbreakEpicenterThresh = 0.0,
    SeedEpicenter = "yes",
    SeedEpicenterCoeff = 0.5,
    DispersalTemplate = "MaxRadius",
    NeighborFlag = "yes",
    NeighborSpeedUp = "none",
    NeighborRadius = 150,
    NeighborShape = "uniform",
    NeighborWeight = 100,
    IntensityClass2_BDP = 0.25,
    IntensityClass3_BDP = 0.50,
    BDASpeciesParameters = sp,
    IgnoredSpecies = c("pinubank", "pinuresi", "pinustro", "thujocci", "tsugcana")
  )

  ext_bda <- ClimateBDA$new(
    path = tmp_pth,
    Timestep = 1L,
    Agents = list(agent),
    SRDMapNames = "bda/{agentName}-SRD-{timestep}.tif",
    NRDMapNames = "bda/{agentName}-NRD-{timestep}.tif",
    BDPMapNames = "bda/{agentName}-BDP-{timestep}.tif",
    LogFile = "bda/bda-log.csv"
  )

  ext_bda$write()

  testthat::expect_true(all(file.exists(file.path(tmp_pth, ext_bda$files))))
  testthat::expect_length(ext_bda$files, 2L) ## main file + one agent file

  ## main extension file
  setup <- readLines(file.path(tmp_pth, ext_bda$files[1]))
  testthat::expect_true(any(grepl("^LandisData\\s+\"Climate BDA\"", setup)))
  testthat::expect_true(any(grepl("^Timestep\\s+1", setup)))
  testthat::expect_true(any(grepl("^MapNames\\s+bda/\\{agentName\\}-\\{timestep\\}", setup)))
  testthat::expect_true(any(grepl("^SRDMapNames", setup)))
  testthat::expect_true(any(grepl("^NRDMapNames", setup)))
  testthat::expect_true(any(grepl("^BDPMapNames", setup)))
  testthat::expect_true(any(grepl("^LogFile", setup)))
  testthat::expect_true(any(grepl("bda-log\\.csv", setup)))
  testthat::expect_true(any(grepl("^BDAInputFiles\\s+bda/budworm\\.txt", setup)))

  ## per-agent file
  agent_path <- file.path(tmp_pth, "bda", "budworm.txt")
  testthat::expect_true(file.exists(agent_path))
  agent_lines <- readLines(agent_path)
  testthat::expect_true(any(grepl("^LandisData\\s+\"BDA Agent\"", agent_lines)))
  testthat::expect_true(any(grepl("^BDAAgentName\\s+budworm$", agent_lines)))
  testthat::expect_true(any(grepl("^BDPCalibrator\\s+1", agent_lines)))
  testthat::expect_true(any(grepl("^SRDMode\\s+mean$", agent_lines)))
  testthat::expect_true(any(grepl("^OutbreakPattern\\s+CyclicNormal$", agent_lines)))
  testthat::expect_true(any(grepl("^Mean\\s+33\\.5", agent_lines)))
  testthat::expect_true(any(grepl("^StDev\\s+10\\.6", agent_lines)))
  testthat::expect_true(any(grepl("^TemporalType\\s+variablepulse$", agent_lines)))
  testthat::expect_true(any(grepl("^Dispersal\\s+no$", agent_lines)))
  testthat::expect_true(any(grepl("^DispersalRate", agent_lines))) ## always emitted (v8-release parser requires it)
  testthat::expect_true(any(grepl("^SeedEpicenter\\s+yes$", agent_lines)))
  testthat::expect_true(any(grepl("^SeedEpicenterCoeff\\s+0\\.5", agent_lines)))
  testthat::expect_true(any(grepl("^DispersalTemplate\\s+MaxRadius$", agent_lines)))
  testthat::expect_true(any(grepl("^NeighborFlag\\s+yes$", agent_lines)))
  testthat::expect_true(any(grepl("^NeighborRadius\\s+150", agent_lines)))
  testthat::expect_true(any(grepl("^IntensityClass2_BDP\\s+0\\.25", agent_lines)))
  testthat::expect_true(any(grepl("^IntensityClass3_BDP\\s+0\\.5", agent_lines)))
  testthat::expect_true(any(grepl("^BDASpeciesParameters", agent_lines)))
  testthat::expect_true(any(grepl("^abiebals", agent_lines)))
  testthat::expect_true(any(grepl("^piceglau", agent_lines)))
  testthat::expect_true(any(grepl("^IgnoredSpecies", agent_lines)))
  testthat::expect_true(any(grepl("^pinubank$", agent_lines)))
  testthat::expect_true(any(grepl("^tsugcana$", agent_lines)))

  withr::deferred_run()
})

testthat::test_that("bdaAgent rejects invalid inputs", {
  sp <- data.frame(
    Species = "abiebals",
    MinorHostAge = 0,
    MinorHostSRDProb = 0.25,
    SecondHostAge = 20,
    SecondHostSRDProb = 0.5,
    MajorHostAge = 40,
    MajorHostSRDProb = 1.0,
    Class3Age = 11,
    Class3VulnProb = 1.0,
    Class2Age = 20,
    Class2VulnProb = 1.0,
    Class1Age = 50,
    Class1VulnProb = 1.0,
    CFSConifer = "yes"
  )

  base_args <- list(
    name = "budworm",
    BDPCalibrator = 1,
    SRDMode = "mean",
    TimeSinceLastEpidemic = 10L,
    TemporalType = "variablepulse",
    MinROS = 0L,
    MaxROS = 3L,
    Dispersal = "no",
    EpidemicThresh = 0.5,
    InitialEpicenterNum = 0L,
    OutbreakEpicenterCoeff = 0.01,
    OutbreakEpicenterThresh = 0,
    SeedEpicenter = "no",
    DispersalTemplate = "MaxRadius",
    NeighborFlag = "no",
    IntensityClass2_BDP = 0.25,
    IntensityClass3_BDP = 0.5,
    BDASpeciesParameters = sp
  )

  ## invalid OutbreakPattern
  testthat::expect_error(do.call(bdaAgent, c(base_args, list(OutbreakPattern = "Foo"))))

  ## CyclicNormal without Mean / StDev
  testthat::expect_error(do.call(bdaAgent, c(base_args, list(OutbreakPattern = "CyclicNormal"))))

  ## BDASpeciesParameters missing required columns
  bad_sp <- sp[, c("Species", "MinorHostAge")]
  bad_args <- base_args
  bad_args$BDASpeciesParameters <- bad_sp
  testthat::expect_error(do.call(
    bdaAgent,
    c(bad_args, list(OutbreakPattern = "CyclicNormal", Mean = 33.5, StDev = 10.6))
  ))
})

testthat::test_that("ClimateBDA rejects empty Agents list at write time", {
  tmp_pth <- withr::local_tempdir("test_ClimateBDA_")

  ext_bda <- ClimateBDA$new(path = tmp_pth, Timestep = 1L)
  testthat::expect_error(ext_bda$write())

  withr::deferred_run()
})
