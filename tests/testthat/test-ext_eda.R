testthat::test_that("EDA extension is properly created", {
  ## NOTE: reference values from the LANDIS-II Core8 / EDA v3 test input:
  ## tests/Core8.0-EDA3.0/EDA-input.txt and P_ramorum.txt
  ## <https://github.com/LANDIS-II-Foundation/Extension-Base-EDA/tree/master/tests/Core8.0-EDA3.0>

  tmp_pth <- withr::local_tempdir("test_EDA_")

  sp <- data.frame(
    Species = c("Umbecali", "Lithdens"),
    LowAge = c(5, 5),
    LowScore = c(3, 2),
    MediumAge = c(15, 20),
    MediumScore = c(6, 4),
    HighAge = c(40, 60),
    HighScore = c(10, 7),
    VulnLowAge = c(999, 5),
    VulnLowMortProb = c(0, 0.14),
    VulnMediumAge = c(999, 15),
    VulnMediumMortProb = c(0, 0.25),
    VulnHighAge = c(999, 30),
    VulnHighMortProb = c(0, 0.30),
    CFSConifer = c("no", "yes"),
    MortalityPlot = c("no", "yes")
  )

  agent <- edaAgent(
    name = "ramorum",
    SHIMode = "mean",
    TransmissionRate = 1.8,
    AcquisitionRate = 0.4,
    InitialEpidemMap = "initEpidem1.img",
    DispersalType = "STATIC",
    DispersalKernel = "PowerLaw",
    DispersalMaxDist = 1000,
    AlphaCoef = 3.55,
    EDASpeciesParameters = sp,
    IgnoredSpecies = c("Abiebrac", "Pinupond")
  )

  ext_eda <- EDA$new(
    path = tmp_pth,
    Timestep = 1L,
    Agents = list(agent),
    LogFile = "eda/eda-log.csv"
  )
  ext_eda$write()

  testthat::expect_true(all(file.exists(file.path(tmp_pth, ext_eda$files))))
  testthat::expect_length(ext_eda$files, 2L) ## main + one agent

  ## main file
  main <- readLines(file.path(tmp_pth, ext_eda$files[1]))
  testthat::expect_true(any(grepl("^LandisData\\s+\"EDA\"", main)))
  testthat::expect_true(any(grepl("^Timestep\\s+1", main)))
  testthat::expect_true(any(grepl("^MapNames\\s+eda/\\{agentName\\}-\\{timestep\\}", main)))
  testthat::expect_true(any(grepl("^MORTMapNames\\s+eda/\\{agentName\\}-MORT", main)))
  testthat::expect_true(any(grepl("eda-log\\.csv", main)))
  testthat::expect_true(any(grepl("^EDAInputFiles\\s+eda/ramorum\\.txt", main)))

  ## agent file
  agent_file <- file.path(tmp_pth, "eda", "ramorum.txt")
  testthat::expect_true(file.exists(agent_file))
  agent_lines <- readLines(agent_file)
  testthat::expect_true(any(grepl("^LandisData\\s+\"EDA Agent\"", agent_lines)))
  testthat::expect_true(any(grepl("^EDAAgentName\\s+ramorum$", agent_lines)))
  testthat::expect_true(any(grepl("^SHIMode\\s+mean$", agent_lines)))
  testthat::expect_true(any(grepl("^TransmissionRate\\s+1\\.8", agent_lines)))
  testthat::expect_true(any(grepl("^AcquisitionRate\\s+0\\.4", agent_lines)))
  testthat::expect_true(any(grepl("^DispersalType\\s+STATIC$", agent_lines)))
  testthat::expect_true(any(grepl("^DispersalKernel\\s+PowerLaw$", agent_lines)))
  testthat::expect_true(any(grepl("^DispersalMaxDist\\s+1000", agent_lines)))
  testthat::expect_true(any(grepl("^AlphaCoef\\s+3\\.55", agent_lines)))
  testthat::expect_true(any(grepl("^EDASpeciesParameters", agent_lines)))
  testthat::expect_true(any(grepl("^Umbecali", agent_lines)))
  testthat::expect_true(any(grepl("^Lithdens", agent_lines)))
  testthat::expect_true(any(grepl("^IgnoredSpecies", agent_lines)))
  testthat::expect_true(any(grepl("^Abiebrac$", agent_lines)))

  withr::deferred_run()
})

testthat::test_that("edaAgent rejects invalid inputs", {
  base_args <- list(
    name = "ramorum",
    SHIMode = "mean",
    TransmissionRate = 1.8,
    AcquisitionRate = 0.4,
    DispersalType = "STATIC",
    DispersalKernel = "PowerLaw",
    DispersalMaxDist = 1000,
    AlphaCoef = 3.55
  )

  ## missing EDASpeciesParameters
  testthat::expect_error(do.call(edaAgent, base_args))

  ## bad SHIMode
  bad_sp <- data.frame(matrix(0, nrow = 1, ncol = length(.edaSpeciesParameterCols)))
  colnames(bad_sp) <- .edaSpeciesParameterCols
  bad_sp$Species <- "Umbecali"
  bad_sp$CFSConifer <- "no"
  bad_sp$MortalityPlot <- "no"
  testthat::expect_error(do.call(
    edaAgent,
    c(base_args, list(SHIMode = "weighted", EDASpeciesParameters = bad_sp))
  ))
})

testthat::test_that("EDA rejects empty Agents list at write time", {
  tmp_pth <- withr::local_tempdir("test_EDA_")

  ext_eda <- EDA$new(path = tmp_pth, Timestep = 1L)
  testthat::expect_error(ext_eda$write())

  withr::deferred_run()
})
