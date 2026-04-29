#' Epidemiological Disturbance Agents (EDA) Extension
#'
#' @include ext_utils.R
#'
#' @references LANDIS-II Base EDA v3 Extension User Guide
#'   <https://github.com/LANDIS-II-Foundation/Extension-Base-EDA/blob/master/docs/LANDIS-II%20Base%20EDA%20v3%20User%20Guide.pdf>
#'
#' @family EDA helpers
#'
#' @export
EDA <- R6Class(
  "EDA",
  inherit = LandisExtension,
  public = list(
    #' @param path Character. Directory path.
    #' @param Timestep Integer. Years between updates.
    #' @param Agents List of `EDAAgent` objects (see [edaAgent()]).
    #' @param MapNames Character. Output map filename pattern; must contain
    #'   the literal `{agentName}` and `{timestep}` placeholders.
    #' @param MORTMapNames (Optional) Character. Mortality output map filename
    #'   pattern; must contain the literal `{agentName}` and `{timestep}` placeholders.
    #' @param LogFile Character. Relative file path for the EDA CSV log.
    initialize = function(
      path,
      Timestep = NULL,
      Agents = list(),
      MapNames = NULL,
      MORTMapNames = NULL,
      LogFile = "eda/eda-log.csv"
    ) {
      stopifnot(!is.null(path))

      ## LandisExtension fields
      private$.LandisData <- "EDA"
      self$Timestep <- Timestep

      self$type <- "disturbance"
      self$path <- path
      self$files <- "eda.txt" ## file won't exist yet

      ## additional fields
      self$Agents <- Agents
      self$MapNames <- MapNames %||% "eda/{agentName}-{timestep}.tif"
      self$MORTMapNames <- MORTMapNames %||%
        "eda/{agentName}-MORT-{timestep}.tif"
      self$LogFile <- LogFile
    },

    #' @param value `EDAAgent` object to append to `$Agents`.
    add_agent = function(value) {
      stopifnot(inherits(value, "EDAAgent"))
      private$.Agents <- c(private$.Agents, list(value))
    },

    #' @description Write extension inputs to disk
    write = function() {
      stopifnot(
        length(self$Agents) >= 1L,
        !is.null(self$MapNames),
        !is.null(self$LogFile)
      )

      .checkPath(file.path(self$path, "eda"))
      agent_files <- vapply(
        self$Agents,
        function(a) writeEDAAgentFile(a, self$path),
        character(1)
      )

      writeLines(
        c(
          insertLandisData(private$.LandisData),
          insertValue("Timestep", self$Timestep),
          insertValue("MapNames", self$MapNames),
          if (!is.null(self$MORTMapNames)) {
            insertValue("MORTMapNames", self$MORTMapNames)
          },
          insertValue("LogFile", self$LogFile),
          insertEDAInputFiles(agent_files)
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
    .MORTMapNames = NULL,
    .LogFile = NULL
  ),

  active = list(
    #' @field Agents List of `EDAAgent` objects.
    Agents = function(value) {
      if (missing(value)) {
        return(private$.Agents)
      } else {
        if (is.null(value)) {
          value <- list()
        } else if (inherits(value, "EDAAgent")) {
          value <- list(value)
        }
        stopifnot(
          is.list(value),
          all(vapply(value, inherits, logical(1), "EDAAgent"))
        )
        private$.Agents <- value
      }
    },

    #' @field MapNames Character. Output map filename pattern; must contain
    #'   the literal `{agentName}` and `{timestep}` placeholders -- LANDIS-II
    #'   replaces them with the agent name and simulation year, e.g.
    #'   `"eda/{agentName}-{timestep}.tif"`.
    MapNames = function(value) {
      if (missing(value)) {
        return(private$.MapNames)
      } else {
        if (!is.null(value)) {
          stopifnot(
            "MapNames must be a character string." = is.character(value),
            "MapNames must contain the literal `{agentName}` placeholder." = grepl(
              "{agentName}",
              value,
              fixed = TRUE
            ),
            "MapNames must contain the literal `{timestep}` placeholder." = grepl(
              "{timestep}",
              value,
              fixed = TRUE
            )
          )
        }
        private$.MapNames <- value
      }
    },

    #' @field MORTMapNames (Optional) Character. Mortality output map filename
    #'   pattern; must contain the literal `{agentName}` and `{timestep}`
    #'   placeholders, e.g. `"eda/{agentName}-MORT-{timestep}.tif"`.
    MORTMapNames = function(value) {
      if (missing(value)) {
        return(private$.MORTMapNames)
      } else {
        if (!is.null(value)) {
          stopifnot(
            "MORTMapNames must be a character string." = is.character(value),
            "MORTMapNames must contain the literal `{agentName}` placeholder." = grepl(
              "{agentName}",
              value,
              fixed = TRUE
            ),
            "MORTMapNames must contain the literal `{timestep}` placeholder." = grepl(
              "{timestep}",
              value,
              fixed = TRUE
            )
          )
        }
        private$.MORTMapNames <- value
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

#' Allowed EDA agent `SHIMode` values
#' @keywords internal
.edaSHIMode <- c("mean", "max")

#' Allowed EDA agent `DispersalType` values
#' @keywords internal
.edaDispersalType <- c("STATIC", "DYNAMIC")

#' Allowed EDA agent `DispersalKernel` values
#' @keywords internal
.edaDispersalKernel <- c("PowerLaw", "NegExp")

#' Required columns for `EDASpeciesParameters`
#' @keywords internal
.edaSpeciesParameterCols <- c(
  "Species",
  "LowAge",
  "LowScore",
  "MediumAge",
  "MediumScore",
  "HighAge",
  "HighScore",
  "VulnLowAge",
  "VulnLowMortProb",
  "VulnMediumAge",
  "VulnMediumMortProb",
  "VulnHighAge",
  "VulnHighMortProb",
  "CFSConifer",
  "MortalityPlot"
)

#' Construct an EDA agent
#'
#' Returns a validated agent specification used by [EDA]. Each agent is
#' written to its own per-agent input file on disk and listed in the main
#' extension file's `EDAInputFiles` block.
#'
#' Climate-related blocks (`ClimateVariables`, `DerivedClimateVariables`,
#' `WeatherIndexVariables`, `AnnualWeatherIndex`, formula blocks, etc.)
#' vary widely between pathogens; supply them verbatim via `extraClimateLines`
#' rather than modelling each one as an active binding.
#'
#' @param name Character. Agent name (no spaces).
#' @param SHIMode Character. One of `"mean"` or `"max"`.
#' @param StartYear,EndYear (Optional) Integer. Outbreak window.
#' @param TransmissionRate Numeric (`beta0`). Mean rate at which an infected
#'   cell infects another cell per timestep.
#' @param AcquisitionRate Numeric (`rD`). Rate of acquisition of detectable
#'   symptoms per timestep.
#' @param InitialEpidemMap (Optional) Character. Relative path to the
#'   initial-outbreak raster.
#' @param DispersalType Character. One of `"STATIC"` or `"DYNAMIC"`.
#' @param DispersalKernel Character. One of `"PowerLaw"` or `"NegExp"`.
#' @param DispersalMaxDist Numeric. Cut-off distance (m) for the dispersal kernel.
#' @param AlphaCoef Numeric. Exponent of the dispersal kernel.
#' @param EDASpeciesParameters `data.frame` with required columns:
#'   `r .fmtKeys(.edaSpeciesParameterCols)`.
#' @param DisturbanceModifiers (Optional) `data.frame` with columns
#'   `SHIModifier`, `Duration`, `Type`.
#' @param IgnoredSpecies (Optional) Character vector of species codes to
#'   exclude from the SHI calculation.
#' @param extraClimateLines (Optional) Character vector of raw lines for
#'   the climate-input section (e.g. `ClimateVariables` table).
#'   Inserted verbatim, before the transmission block.
#'
#' @returns A list of class `"EDAAgent"`.
#'
#' @family EDA helpers
#'
#' @export
edaAgent <- function(
  name,
  SHIMode = "mean",
  StartYear = NULL,
  EndYear = NULL,
  TransmissionRate = NULL,
  AcquisitionRate = NULL,
  InitialEpidemMap = NULL,
  DispersalType = "STATIC",
  DispersalKernel = "PowerLaw",
  DispersalMaxDist = NULL,
  AlphaCoef = NULL,
  EDASpeciesParameters = NULL,
  DisturbanceModifiers = NULL,
  IgnoredSpecies = NULL,
  extraClimateLines = character(0)
) {
  stopifnot(
    is.character(name),
    length(name) == 1L,
    !grepl("\\s", name),
    SHIMode %in% .edaSHIMode,
    !is.null(TransmissionRate),
    is.numeric(TransmissionRate),
    !is.null(AcquisitionRate),
    is.numeric(AcquisitionRate),
    DispersalType %in% .edaDispersalType,
    DispersalKernel %in% .edaDispersalKernel,
    !is.null(DispersalMaxDist),
    is.numeric(DispersalMaxDist),
    DispersalMaxDist > 0,
    !is.null(AlphaCoef),
    is.numeric(AlphaCoef),
    !is.null(EDASpeciesParameters),
    is.data.frame(EDASpeciesParameters),
    all(.edaSpeciesParameterCols %in% colnames(EDASpeciesParameters))
  )

  if (!is.null(DisturbanceModifiers)) {
    stopifnot(
      is.data.frame(DisturbanceModifiers),
      all(
        c("SHIModifier", "Duration", "Type") %in% colnames(DisturbanceModifiers)
      )
    )
  }
  if (!is.null(IgnoredSpecies)) {
    stopifnot(is.character(IgnoredSpecies))
  }
  stopifnot(is.character(extraClimateLines))

  structure(
    list(
      name = name,
      SHIMode = SHIMode,
      StartYear = StartYear,
      EndYear = EndYear,
      TransmissionRate = TransmissionRate,
      AcquisitionRate = AcquisitionRate,
      InitialEpidemMap = InitialEpidemMap,
      DispersalType = DispersalType,
      DispersalKernel = DispersalKernel,
      DispersalMaxDist = DispersalMaxDist,
      AlphaCoef = AlphaCoef,
      EDASpeciesParameters = EDASpeciesParameters,
      DisturbanceModifiers = DisturbanceModifiers,
      IgnoredSpecies = IgnoredSpecies,
      extraClimateLines = extraClimateLines
    ),
    class = "EDAAgent"
  )
}

#' Specify the `EDAInputFiles` block in the main EDA file
#'
#' @param files Character vector of relative paths to per-agent input files.
#'
#' @template return_insert
#'
#' @family EDA helpers
#'
#' @keywords internal
insertEDAInputFiles <- function(files) {
  stopifnot(is.character(files), length(files) >= 1L)

  c(
    glue::glue("EDAInputFiles    {files[1]}"),
    if (length(files) > 1L) {
      vapply(
        files[-1],
        function(p) {
          as.character(glue::glue("                 {p}"))
        },
        character(1),
        USE.NAMES = FALSE
      )
    },
    glue::glue("")
  )
}

#' Write a per-agent EDA input file
#'
#' @param agent `EDAAgent` object (see [edaAgent()]).
#' @param base_path Character. Path to the extension's directory; the per-
#'   agent file is written to `<base_path>/eda/<agent$name>.txt`.
#'
#' @returns Character. The relative path to the written file.
#'
#' @family EDA helpers
#'
#' @keywords internal
writeEDAAgentFile <- function(agent, base_path) {
  stopifnot(inherits(agent, "EDAAgent"))

  rel_file <- file.path("eda", glue::glue("{agent$name}.txt"))
  full_file <- file.path(base_path, rel_file)

  writeLines(
    c(
      insertLandisData("EDA Agent"),
      insertValue("EDAAgentName", agent$name, blank_line = FALSE),
      insertValue("SHIMode", agent$SHIMode),
      if (!is.null(agent$StartYear)) {
        insertValue(
          "StartYear",
          as.integer(agent$StartYear),
          blank_line = FALSE
        )
      },
      if (!is.null(agent$EndYear)) {
        insertValue("EndYear", as.integer(agent$EndYear))
      },
      agent$extraClimateLines,
      if (length(agent$extraClimateLines) > 0L) glue::glue(""),
      insertValue(
        "TransmissionRate",
        agent$TransmissionRate,
        blank_line = FALSE
      ),
      insertValue("AcquisitionRate", agent$AcquisitionRate, blank_line = FALSE),
      if (!is.null(agent$InitialEpidemMap)) {
        insertValue(
          "InitialEpidemMap",
          agent$InitialEpidemMap,
          blank_line = FALSE
        )
      },
      insertValue("DispersalType", agent$DispersalType, blank_line = FALSE),
      insertValue("DispersalKernel", agent$DispersalKernel, blank_line = FALSE),
      insertValue(
        "DispersalMaxDist",
        agent$DispersalMaxDist,
        blank_line = FALSE
      ),
      insertValue("AlphaCoef", agent$AlphaCoef),
      insertEDADisturbanceModifiers(agent$DisturbanceModifiers),
      insertEDASpeciesParameters(agent$EDASpeciesParameters),
      insertEDAIgnoredSpecies(agent$IgnoredSpecies)
    ),
    full_file
  )

  rel_file
}

#' Specify the optional `DisturbanceModifiers` table for an EDA agent
#'
#' @param df (Optional) `data.frame` with columns `SHIModifier`, `Duration`,
#'   `Type`. When `NULL`, nothing is written.
#'
#' @template return_insert
#'
#' @family EDA helpers
#'
#' @keywords internal
insertEDADisturbanceModifiers <- function(df) {
  if (is.null(df)) {
    return(character(0))
  }
  stopifnot(
    is.data.frame(df),
    all(c("SHIModifier", "Duration", "Type") %in% colnames(df))
  )

  c(
    glue::glue("DisturbanceModifiers"),
    glue::glue(">> SHI Modifier   Duration    Type"),
    glue::glue(">> ------------   --------    ----"),
    apply(df[, c("SHIModifier", "Duration", "Type")], 1, function(x) {
      glue::glue_collapse(x, sep = "    ")
    }),
    glue::glue("")
  )
}

#' Specify the `EDASpeciesParameters` block for an EDA agent
#'
#' @param df `data.frame` with required columns:
#'   `r .fmtKeys(.edaSpeciesParameterCols)`.
#'
#' @template return_insert
#'
#' @family EDA helpers
#'
#' @keywords internal
insertEDASpeciesParameters <- function(df) {
  stopifnot(
    is.data.frame(df),
    all(.edaSpeciesParameterCols %in% colnames(df))
  )

  c(
    glue::glue("EDASpeciesParameters"),
    glue::glue(
      ">> Species  Low Age/Score   Med Age/Score   High Age/Score   ",
      "Vuln Low Age/Prob   Vuln Med Age/Prob   Vuln High Age/Prob   ",
      "CFSConifer   MortPlot"
    ),
    apply(df[, .edaSpeciesParameterCols], 1, function(x) {
      glue::glue_collapse(x, sep = "    ")
    }),
    glue::glue("")
  )
}

#' Specify the optional `IgnoredSpecies` block for an EDA agent
#'
#' @param species (Optional) Character vector of species codes.
#'
#' @template return_insert
#'
#' @family EDA helpers
#'
#' @keywords internal
insertEDAIgnoredSpecies <- function(species) {
  if (is.null(species) || length(species) == 0L) {
    return(character(0))
  }

  c(
    glue::glue("IgnoredSpecies"),
    species,
    glue::glue("")
  )
}
