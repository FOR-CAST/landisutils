#' Climate BDA (Biological Disturbance Agent) Extension
#'
#' @include ext_utils.R
#'
#' @references LANDIS-II Biological Disturbance Agent v5 Extension User Guide
#'   <https://github.com/LANDIS-II-Foundation/Extension-Base-BDA/blob/master/docs/LANDIS-II%20Biological%20Disturbance%20Agent%20v5%20User%20Guide.pdf>
#'
#' @family Climate BDA helpers
#'
#' @export
ClimateBDA <- R6Class(
  "ClimateBDA",
  inherit = LandisExtension,
  public = list(
    #' @param path Character. Directory path.
    #' @param Timestep Integer.
    #' @param Agents List of `BDAAgent` objects (see [bdaAgent()]).
    #' @param MapNames Character. Output map filename pattern; must contain
    #'   the literal `{agentName}` and `{timestep}` placeholders.
    #' @param SRDMapNames (Optional) Character. SRD output map filename pattern;
    #'   must contain the literal `{agentName}` and `{timestep}` placeholders.
    #' @param NRDMapNames (Optional) Character. NRD output map filename pattern;
    #'   must contain the literal `{agentName}` and `{timestep}` placeholders.
    #' @param BDPMapNames (Optional) Character. BDP output map filename pattern;
    #'   must contain the literal `{agentName}` and `{timestep}` placeholders.
    #' @param LogFile Character. Relative file path for the BDA CSV log.
    initialize = function(
      path,
      Timestep = NULL,
      Agents = list(),
      MapNames = NULL,
      SRDMapNames = NULL,
      NRDMapNames = NULL,
      BDPMapNames = NULL,
      LogFile = "bda/bda-log.csv"
    ) {
      stopifnot(!is.null(path))

      ## LandisExtension fields
      private$.LandisData <- "Climate BDA"
      self$Timestep <- Timestep

      self$type <- "disturbance"
      self$path <- path
      self$files <- "climate-bda.txt" ## file won't exist yet

      ## additional fields for this extension
      self$Agents <- Agents
      self$MapNames <- MapNames %||% "bda/{agentName}-{timestep}.tif"
      self$SRDMapNames <- SRDMapNames
      self$NRDMapNames <- NRDMapNames
      self$BDPMapNames <- BDPMapNames
      self$LogFile <- LogFile
    },

    #' @param value `BDAAgent` object to append to `$Agents`.
    add_agent = function(value) {
      stopifnot(inherits(value, "BDAAgent"))
      private$.Agents <- c(private$.Agents, list(value))
    },

    #' @description Write extension inputs to disk
    write = function() {
      stopifnot(length(self$Agents) >= 1L, !is.null(self$MapNames), !is.null(self$LogFile))

      ## write each per-agent file first; collect their relative filenames
      .checkPath(file.path(self$path, "bda"))
      agent_files <- vapply(self$Agents, function(a) writeAgentFile(a, self$path), character(1))

      writeLines(
        c(
          insertLandisData(private$.LandisData),
          insertValue("Timestep", self$Timestep),
          insertValue("MapNames", self$MapNames),
          if (!is.null(self$SRDMapNames)) insertValue("SRDMapNames", self$SRDMapNames),
          if (!is.null(self$NRDMapNames)) insertValue("NRDMapNames", self$NRDMapNames),
          if (!is.null(self$BDPMapNames)) insertValue("BDPMapNames", self$BDPMapNames),
          insertFile("LogFile", self$LogFile),
          insertBDAInputFiles(agent_files)
        ),
        file.path(self$path, self$files[1])
      )

      for (f in agent_files) {
        self$add_file(f)
      }

      return(invisible(self))
    }
  ),

  private = list(
    .Agents = list(),
    .MapNames = NULL,
    .SRDMapNames = NULL,
    .NRDMapNames = NULL,
    .BDPMapNames = NULL,
    .LogFile = NULL
  ),

  active = list(
    #' @field Agents List of `BDAAgent` objects.
    Agents = function(value) {
      if (missing(value)) {
        return(private$.Agents)
      } else {
        if (is.null(value)) {
          value <- list()
        } else if (inherits(value, "BDAAgent")) {
          value <- list(value)
        }
        stopifnot(is.list(value), all(vapply(value, inherits, logical(1), "BDAAgent")))
        private$.Agents <- value
      }
    },

    #' @field MapNames Character. Output map filename pattern; must contain the
    #'   literal `{agentName}` and `{timestep}` placeholders.
    MapNames = function(value) {
      if (missing(value)) {
        return(private$.MapNames)
      } else {
        stopifnot(
          !is.null(value),
          grepl("{agentName}", value, fixed = TRUE),
          grepl("{timestep}", value, fixed = TRUE)
        )
        private$.MapNames <- value
      }
    },

    #' @field SRDMapNames (Optional) Character. SRD output map filename pattern;
    #'   must contain `{agentName}` and `{timestep}`.
    SRDMapNames = function(value) {
      if (missing(value)) {
        return(private$.SRDMapNames)
      } else {
        if (!is.null(value)) {
          stopifnot(
            grepl("{agentName}", value, fixed = TRUE),
            grepl("{timestep}", value, fixed = TRUE)
          )
        }
        private$.SRDMapNames <- value
      }
    },

    #' @field NRDMapNames (Optional) Character. NRD output map filename pattern;
    #'   must contain `{agentName}` and `{timestep}`.
    NRDMapNames = function(value) {
      if (missing(value)) {
        return(private$.NRDMapNames)
      } else {
        if (!is.null(value)) {
          stopifnot(
            grepl("{agentName}", value, fixed = TRUE),
            grepl("{timestep}", value, fixed = TRUE)
          )
        }
        private$.NRDMapNames <- value
      }
    },

    #' @field BDPMapNames (Optional) Character. BDP output map filename pattern;
    #'   must contain `{agentName}` and `{timestep}`.
    BDPMapNames = function(value) {
      if (missing(value)) {
        return(private$.BDPMapNames)
      } else {
        if (!is.null(value)) {
          stopifnot(
            grepl("{agentName}", value, fixed = TRUE),
            grepl("{timestep}", value, fixed = TRUE)
          )
        }
        private$.BDPMapNames <- value
      }
    },

    #' @field LogFile Character. Relative file path.
    LogFile = function(value) {
      if (missing(value)) {
        return(private$.LogFile)
      } else {
        private$.LogFile <- .relPath(value, self$path)
      }
    }
  )
)

#' Allowed Climate BDA `SRDMode` values
#' @keywords internal
.bdaSRDMode <- c("max", "mean")

#' Allowed Climate BDA `OutbreakPattern` values
#' @keywords internal
.bdaOutbreakPattern <- c("CyclicNormal", "CyclicUniform")

#' Allowed Climate BDA `TemporalType` values
#' @keywords internal
.bdaTemporalType <- c("pulse", "variablepulse")

#' Allowed Climate BDA `DispersalTemplate` values
#' @keywords internal
.bdaDispersalTemplate <- c("MaxRadius", "4N", "8N", "12N", "24N")

#' Allowed Climate BDA `NeighborSpeedUp` values
#' @keywords internal
.bdaNeighborSpeedUp <- c("none", "2x", "3x", "4x")

#' Allowed Climate BDA `NeighborShape` values
#' @keywords internal
.bdaNeighborShape <- c("uniform", "linear", "gaussian")

#' Required columns for `BDASpeciesParameters`
#' @keywords internal
.bdaSpeciesParameterCols <- c(
  "Species",
  "MinorHostAge",
  "MinorHostSRDProb",
  "SecondHostAge",
  "SecondHostSRDProb",
  "MajorHostAge",
  "MajorHostSRDProb",
  "Class3Age",
  "Class3VulnProb",
  "Class2Age",
  "Class2VulnProb",
  "Class1Age",
  "Class1VulnProb",
  "CFSConifer"
)

#' Construct a Climate BDA agent
#'
#' Returns a validated agent specification. One or more of these is passed
#' to [ClimateBDA] via `Agents`; each is written to its own per-agent input file
#' on disk and listed in the main extension file's `BDAInputFiles` block.
#'
#' @param name Character. Agent name (no spaces); used as `BDAAgentName` and
#'   as the basename of the per-agent input file.
#' @param BDPCalibrator Numeric. BDP calibrator.
#' @param SRDMode Character. One of `"max"` or `"mean"`.
#' @param StartYear,EndYear (Optional) Integer. Outbreak window.
#' @param OutbreakPattern Character. One of `"CyclicNormal"` or `"CyclicUniform"`.
#' @param Mean,StDev Numeric. Required when `OutbreakPattern = "CyclicNormal"`.
#' @param MaxInterval,MinInterval Numeric. Required when
#'   `OutbreakPattern = "CyclicUniform"`.
#' @param TimeSinceLastEpidemic Integer. Years.
#' @param TemporalType Character. One of `"pulse"` or `"variablepulse"`.
#' @param MinROS,MaxROS Integer. Region-of-spread bounds.
#' @param Dispersal Logical / `"yes"` / `"no"`. Whether dispersal is enabled.
#' @param DispersalRate Numeric. Meters/year. Required when
#'   `Dispersal` is truthy.
#' @param EpidemicThresh Numeric.
#' @param InitialEpicenterNum Integer.
#' @param OutbreakEpicenterCoeff,OutbreakEpicenterThresh Numeric.
#' @param SeedEpicenter Logical / `"yes"` / `"no"`.
#' @param SeedEpicenterMax (Optional, v5+) Integer.
#' @param SeedEpicenterCoeff Numeric. Required when `SeedEpicenter` is truthy.
#' @param DispersalTemplate Character. One of `"MaxRadius"`, `"4N"`, `"8N"`,
#'   `"12N"`, or `"24N"`.
#' @param NeighborFlag Logical / `"yes"` / `"no"`. Whether the neighborhood
#'   resource calculation is enabled.
#' @param NeighborSpeedUp Character. One of `"none"`, `"2x"`, `"3x"`, or `"4x"`.
#'   Required when `NeighborFlag` is truthy.
#' @param NeighborRadius Numeric. Meters. Required when `NeighborFlag` is truthy.
#' @param NeighborShape Character. One of `"uniform"`, `"linear"`, or `"gaussian"`.
#'   Required when `NeighborFlag` is truthy.
#' @param NeighborWeight Numeric. Required when `NeighborFlag` is truthy.
#' @param IntensityClass2_BDP,IntensityClass3_BDP Numeric. BDP thresholds for
#'   intensity classes 2 and 3 (class 1 is hardwired to 0.0).
#' @param EcoregionModifiers (Optional) `data.frame` with columns `Ecoregion`,
#'   `CapacityModifier`.
#' @param DisturbanceModifiers (Optional) `data.frame` with columns
#'   `SRDModifier`, `Duration`, `Type`. The `Type` column may contain
#'   space-separated disturbance type names.
#' @param BDASpeciesParameters `data.frame` with columns `Species`,
#'   `MinorHostAge`, `MinorHostSRDProb`, `SecondHostAge`, `SecondHostSRDProb`,
#'   `MajorHostAge`, `MajorHostSRDProb`, `Class3Age`, `Class3VulnProb`,
#'   `Class2Age`, `Class2VulnProb`, `Class1Age`, `Class1VulnProb`, `CFSConifer`.
#' @param IgnoredSpecies (Optional) Character vector of species codes to
#'   exclude from BDA disturbance.
#'
#' @returns A list of class `"BDAAgent"`.
#'
#' @family Climate BDA helpers
#'
#' @export
bdaAgent <- function(
  name,
  BDPCalibrator = NULL,
  SRDMode = NULL,
  StartYear = NULL,
  EndYear = NULL,
  OutbreakPattern = NULL,
  Mean = NULL,
  StDev = NULL,
  MaxInterval = NULL,
  MinInterval = NULL,
  TimeSinceLastEpidemic = NULL,
  TemporalType = NULL,
  MinROS = NULL,
  MaxROS = NULL,
  Dispersal = NULL,
  DispersalRate = NULL,
  EpidemicThresh = NULL,
  InitialEpicenterNum = NULL,
  OutbreakEpicenterCoeff = NULL,
  OutbreakEpicenterThresh = NULL,
  SeedEpicenter = NULL,
  SeedEpicenterMax = NULL,
  SeedEpicenterCoeff = NULL,
  DispersalTemplate = NULL,
  NeighborFlag = NULL,
  NeighborSpeedUp = NULL,
  NeighborRadius = NULL,
  NeighborShape = NULL,
  NeighborWeight = NULL,
  IntensityClass2_BDP = NULL,
  IntensityClass3_BDP = NULL,
  EcoregionModifiers = NULL,
  DisturbanceModifiers = NULL,
  BDASpeciesParameters = NULL,
  IgnoredSpecies = NULL
) {
  stopifnot(
    is.character(name),
    length(name) == 1L,
    !grepl("\\s", name),
    !is.null(BDPCalibrator),
    !is.null(SRDMode),
    SRDMode %in% .bdaSRDMode,
    !is.null(OutbreakPattern),
    OutbreakPattern %in% .bdaOutbreakPattern,
    !is.null(TimeSinceLastEpidemic),
    !is.null(TemporalType),
    TemporalType %in% .bdaTemporalType,
    !is.null(MinROS),
    !is.null(MaxROS),
    MaxROS >= MinROS,
    !is.null(Dispersal),
    !is.null(EpidemicThresh),
    !is.null(InitialEpicenterNum),
    !is.null(OutbreakEpicenterCoeff),
    !is.null(OutbreakEpicenterThresh),
    !is.null(SeedEpicenter),
    !is.null(DispersalTemplate),
    DispersalTemplate %in% .bdaDispersalTemplate,
    !is.null(NeighborFlag),
    !is.null(IntensityClass2_BDP),
    !is.null(IntensityClass3_BDP)
  )

  if (OutbreakPattern == "CyclicNormal") {
    stopifnot(!is.null(Mean), !is.null(StDev))
  }
  if (OutbreakPattern == "CyclicUniform") {
    stopifnot(!is.null(MaxInterval), !is.null(MinInterval), MaxInterval >= MinInterval)
  }

  dispersal_yn <- yesno(Dispersal)
  if (dispersal_yn == "yes") {
    stopifnot(!is.null(DispersalRate))
  }

  seed_epi_yn <- yesno(SeedEpicenter)
  if (seed_epi_yn == "yes") {
    stopifnot(!is.null(SeedEpicenterCoeff))
  }

  neighbor_yn <- yesno(NeighborFlag)
  if (neighbor_yn == "yes") {
    stopifnot(
      !is.null(NeighborSpeedUp),
      NeighborSpeedUp %in% .bdaNeighborSpeedUp,
      !is.null(NeighborRadius),
      !is.null(NeighborShape),
      NeighborShape %in% .bdaNeighborShape,
      !is.null(NeighborWeight)
    )
  }

  stopifnot(
    !is.null(BDASpeciesParameters),
    is.data.frame(BDASpeciesParameters),
    all(.bdaSpeciesParameterCols %in% colnames(BDASpeciesParameters))
  )

  if (!is.null(EcoregionModifiers)) {
    stopifnot(
      is.data.frame(EcoregionModifiers),
      all(c("Ecoregion", "CapacityModifier") %in% colnames(EcoregionModifiers))
    )
  }
  if (!is.null(DisturbanceModifiers)) {
    stopifnot(
      is.data.frame(DisturbanceModifiers),
      all(c("SRDModifier", "Duration", "Type") %in% colnames(DisturbanceModifiers))
    )
  }
  if (!is.null(IgnoredSpecies)) {
    stopifnot(is.character(IgnoredSpecies))
  }

  structure(
    list(
      name = name,
      BDPCalibrator = BDPCalibrator,
      SRDMode = SRDMode,
      StartYear = StartYear,
      EndYear = EndYear,
      OutbreakPattern = OutbreakPattern,
      Mean = Mean,
      StDev = StDev,
      MaxInterval = MaxInterval,
      MinInterval = MinInterval,
      TimeSinceLastEpidemic = TimeSinceLastEpidemic,
      TemporalType = TemporalType,
      MinROS = MinROS,
      MaxROS = MaxROS,
      Dispersal = dispersal_yn,
      DispersalRate = DispersalRate,
      EpidemicThresh = EpidemicThresh,
      InitialEpicenterNum = InitialEpicenterNum,
      OutbreakEpicenterCoeff = OutbreakEpicenterCoeff,
      OutbreakEpicenterThresh = OutbreakEpicenterThresh,
      SeedEpicenter = seed_epi_yn,
      SeedEpicenterMax = SeedEpicenterMax,
      SeedEpicenterCoeff = SeedEpicenterCoeff,
      DispersalTemplate = DispersalTemplate,
      NeighborFlag = neighbor_yn,
      NeighborSpeedUp = NeighborSpeedUp,
      NeighborRadius = NeighborRadius,
      NeighborShape = NeighborShape,
      NeighborWeight = NeighborWeight,
      IntensityClass2_BDP = IntensityClass2_BDP,
      IntensityClass3_BDP = IntensityClass3_BDP,
      EcoregionModifiers = EcoregionModifiers,
      DisturbanceModifiers = DisturbanceModifiers,
      BDASpeciesParameters = BDASpeciesParameters,
      IgnoredSpecies = IgnoredSpecies
    ),
    class = "BDAAgent"
  )
}

#' Specify the `BDAInputFiles` block in the main Climate BDA file
#'
#' @param files Character vector of relative paths to per-agent input files.
#'   The keyword is emitted on the same line as the first file; subsequent
#'   files appear on subsequent lines (matches the reference parser in
#'   `Extension-Base-BDA/src/InputParameterParser.cs`).
#'
#' @template return_insert
#'
#' @family Climate BDA helpers
#'
#' @keywords internal
insertBDAInputFiles <- function(files) {
  stopifnot(is.character(files), length(files) >= 1L)

  c(glue::glue("BDAInputFiles    {files[1]}"), if (length(files) > 1L) files[-1], glue::glue(""))
}

#' Write a per-agent Climate BDA input file
#'
#' @param agent `BDAAgent` object (see [bdaAgent()]).
#' @param base_path Character. Path to the extension's directory; the per-agent
#'   file is written to `<base_path>/bda/<agent$name>.txt`.
#'
#' @returns Character. The relative path to the written file (relative to
#'   `base_path`), suitable for inclusion in the main file's `BDAInputFiles`
#'   block.
#'
#' @family Climate BDA helpers
#'
#' @keywords internal
writeAgentFile <- function(agent, base_path) {
  stopifnot(inherits(agent, "BDAAgent"))

  rel_file <- file.path("bda", glue::glue("{agent$name}.txt"))
  full_file <- file.path(base_path, rel_file)

  writeLines(
    c(
      insertLandisData("BDA Agent"),
      insertAgentHeader(agent),
      insertOutbreakPattern(agent),
      insertDispersal(agent),
      insertNeighborhood(agent),
      insertIntensityClasses(agent),
      insertEcoregionModifiers(agent$EcoregionModifiers),
      insertDisturbanceModifiers(agent$DisturbanceModifiers),
      insertBDASpeciesParameters(agent$BDASpeciesParameters),
      insertIgnoredSpecies(agent$IgnoredSpecies)
    ),
    full_file
  )

  rel_file
}

#' Specify the header block of a Climate BDA agent file
#'
#' @param agent `BDAAgent` object.
#'
#' @template return_insert
#'
#' @family Climate BDA helpers
#'
#' @keywords internal
insertAgentHeader <- function(agent) {
  c(
    insertValue("BDAAgentName", agent$name, blank_line = FALSE),
    insertValue("BDPCalibrator", agent$BDPCalibrator, blank_line = FALSE),
    insertValue("SRDMode", agent$SRDMode, blank_line = FALSE),
    if (!is.null(agent$StartYear)) {
      insertValue("StartYear", as.integer(agent$StartYear), blank_line = FALSE)
    },
    if (!is.null(agent$EndYear)) {
      insertValue("EndYear", as.integer(agent$EndYear), blank_line = FALSE)
    },
    glue::glue("")
  )
}

#' Specify the outbreak-pattern block of a Climate BDA agent file
#'
#' @param agent `BDAAgent` object.
#'
#' @template return_insert
#'
#' @family Climate BDA helpers
#'
#' @keywords internal
insertOutbreakPattern <- function(agent) {
  pattern_lines <- if (agent$OutbreakPattern == "CyclicNormal") {
    c(
      insertValue("Mean", agent$Mean, blank_line = FALSE),
      insertValue("StDev", agent$StDev, blank_line = FALSE)
    )
  } else {
    c(
      insertValue("MaxInterval", agent$MaxInterval, blank_line = FALSE),
      insertValue("MinInterval", agent$MinInterval, blank_line = FALSE)
    )
  }

  c(
    insertValue("OutbreakPattern", agent$OutbreakPattern, blank_line = FALSE),
    pattern_lines,
    insertValue(
      "TimeSinceLastEpidemic",
      as.integer(agent$TimeSinceLastEpidemic),
      blank_line = FALSE
    ),
    insertValue("TemporalType", agent$TemporalType, blank_line = FALSE),
    insertValue("MinROS", as.integer(agent$MinROS), blank_line = FALSE),
    insertValue("MaxROS", as.integer(agent$MaxROS), blank_line = FALSE),
    glue::glue("")
  )
}

#' Specify the dispersal block of a Climate BDA agent file
#'
#' @param agent `BDAAgent` object.
#'
#' @template return_insert
#'
#' @family Climate BDA helpers
#'
#' @keywords internal
insertDispersal <- function(agent) {
  ## DispersalRate is required by the BDA agent parser regardless of the
  ## `Dispersal` toggle, and must be > 0 even when dispersal is disabled
  ## (verified against the v8-release runtime). Use a placeholder value when
  ## dispersal is off and no rate was given.
  rate <- agent$DispersalRate %||% 4000
  c(
    insertValue("Dispersal", agent$Dispersal, blank_line = FALSE),
    insertValue("DispersalRate", rate, blank_line = FALSE),
    insertValue("EpidemicThresh", agent$EpidemicThresh, blank_line = FALSE),
    insertValue("InitialEpicenterNum", as.integer(agent$InitialEpicenterNum), blank_line = FALSE),
    insertValue("OutbreakEpicenterCoeff", agent$OutbreakEpicenterCoeff, blank_line = FALSE),
    insertValue("OutbreakEpicenterThresh", agent$OutbreakEpicenterThresh, blank_line = FALSE),
    insertValue("SeedEpicenter", agent$SeedEpicenter, blank_line = FALSE),
    ## SeedEpicenterMax (v5+) and SeedEpicenterCoeff are both required by the
    ## v8-release parser even when SeedEpicenter is "no"; supply placeholder
    ## values when the agent didn't specify them.
    insertValue("SeedEpicenterMax", as.integer(agent$SeedEpicenterMax %||% 25), blank_line = FALSE),
    insertValue("SeedEpicenterCoeff", agent$SeedEpicenterCoeff %||% 0.5, blank_line = FALSE),
    insertValue("DispersalTemplate", agent$DispersalTemplate, blank_line = FALSE),
    glue::glue("")
  )
}

#' Specify the neighborhood-resource block of a Climate BDA agent file
#'
#' @param agent `BDAAgent` object.
#'
#' @template return_insert
#'
#' @family Climate BDA helpers
#'
#' @keywords internal
insertNeighborhood <- function(agent) {
  lines <- insertValue("NeighborFlag", agent$NeighborFlag, blank_line = FALSE)

  if (agent$NeighborFlag == "yes") {
    lines <- c(
      lines,
      insertValue("NeighborSpeedUp", agent$NeighborSpeedUp, blank_line = FALSE),
      insertValue("NeighborRadius", as.integer(agent$NeighborRadius), blank_line = FALSE),
      insertValue("NeighborShape", agent$NeighborShape, blank_line = FALSE),
      insertValue("NeighborWeight", agent$NeighborWeight, blank_line = FALSE)
    )
  }

  c(lines, glue::glue(""))
}

#' Specify the intensity-class block of a Climate BDA agent file
#'
#' Class 1 is hardwired to 0.0 in LANDIS-II, so only classes 2 and 3 are
#' written.
#'
#' @param agent `BDAAgent` object.
#'
#' @template return_insert
#'
#' @family Climate BDA helpers
#'
#' @keywords internal
insertIntensityClasses <- function(agent) {
  c(
    insertValue("IntensityClass2_BDP", agent$IntensityClass2_BDP, blank_line = FALSE),
    insertValue("IntensityClass3_BDP", agent$IntensityClass3_BDP, blank_line = FALSE),
    glue::glue("")
  )
}

#' Specify the optional `EcoregionModifiers` table
#'
#' @param df (Optional) `data.frame` with columns `Ecoregion`, `CapacityModifier`.
#'   When `NULL`, nothing is written.
#'
#' @template return_insert
#'
#' @family Climate BDA helpers
#'
#' @keywords internal
insertEcoregionModifiers <- function(df) {
  if (is.null(df)) {
    return(character(0))
  }
  stopifnot(is.data.frame(df), all(c("Ecoregion", "CapacityModifier") %in% colnames(df)))

  c(
    glue::glue("EcoregionModifiers"),
    glue::glue(">> Ecoregion    Capacity Modifier"),
    glue::glue(">> ---------    -----------------"),
    apply(df[, c("Ecoregion", "CapacityModifier")], 1, function(x) {
      glue::glue_collapse(x, sep = "    ")
    }),
    glue::glue("")
  )
}

#' Specify the optional `DisturbanceModifiers` table
#'
#' @param df (Optional) `data.frame` with columns `SRDModifier`, `Duration`,
#'   `Type`. When `NULL`, nothing is written.
#'
#' @template return_insert
#'
#' @family Climate BDA helpers
#'
#' @keywords internal
insertDisturbanceModifiers <- function(df) {
  if (is.null(df)) {
    return(character(0))
  }
  stopifnot(is.data.frame(df), all(c("SRDModifier", "Duration", "Type") %in% colnames(df)))

  c(
    glue::glue("DisturbanceModifiers"),
    glue::glue(">> SRD Modifier    Duration    Type"),
    glue::glue(">> ------------    --------    ----"),
    apply(df[, c("SRDModifier", "Duration", "Type")], 1, function(x) {
      glue::glue_collapse(x, sep = "    ")
    }),
    glue::glue("")
  )
}

#' Specify the `BDASpeciesParameters` table
#'
#' @param df `data.frame` with the columns enumerated in `.bdaSpeciesParameterCols`.
#'
#' @template return_insert
#'
#' @family Climate BDA helpers
#'
#' @keywords internal
insertBDASpeciesParameters <- function(df) {
  stopifnot(is.data.frame(df), all(.bdaSpeciesParameterCols %in% colnames(df)))

  c(
    glue::glue("BDASpeciesParameters"),
    glue::glue(
      ">>          ----------- Host Value -----------    ",
      "------------ Susceptibility ------------"
    ),
    glue::glue(
      ">> Species  MinorHost     SecondHost    MajorHost     ",
      "Class3        Class2        Class1        CFS"
    ),
    glue::glue(
      ">>          Age SRDProb   Age SRDProb   Age SRDProb   ",
      "Age VulnProb  Age VulnProb  Age VulnProb  Conifer?"
    ),
    glue::glue(
      ">> -------------------------------------------------------",
      "----------------------------------------------"
    ),
    apply(df[, .bdaSpeciesParameterCols], 1, function(x) {
      glue::glue_collapse(x, sep = "    ")
    }),
    glue::glue("")
  )
}

#' Specify the optional `IgnoredSpecies` block
#'
#' @param species (Optional) Character vector of species codes.
#'
#' @template return_insert
#'
#' @family Climate BDA helpers
#'
#' @keywords internal
insertIgnoredSpecies <- function(species) {
  if (is.null(species) || length(species) == 0L) {
    return(character(0))
  }

  c(glue::glue("IgnoredSpecies"), species, glue::glue(""))
}
