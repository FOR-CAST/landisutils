utils::globalVariables(c("AgeAtDeath"))

#' Forest Carbon Succession (ForCS) Extension
#'
#' @include ext_utils.R
#'
#' @references LANDIS-II Forest Carbon Succession (CForC) v4.0.2 User Guide
#'   <https://github.com/LANDIS-II-Foundation/Extension-ForCS-Succession/blob/master/deploy/installer/LANDIS-II%20CForC%20Succession%20v4.0.2%20User%20Guide%20September%202025.pdf>
#'
#' @seealso
#' Helpers that prepare inputs for this extension:
#' [prepClimateFile()],
#' [prepDisturbanceMatrixFile()],
#' [prepSnagFile()].
#' Shared scenario inputs:
#' [prepInitialCommunities()],
#' [prepSpeciesData()].
#'
#' @family ForCS helpers
#'
#' @export
#'
#' @examples
#' ## See vignette for example usage
#'
ForCS <- R6Class(
  "ForCS", ## ForC Succession
  inherit = LandisExtension,
  public = list(
    #' @param path Character. Directory path.
    #' @param Timestep Integer.
    #' @template param_SeedingAlgorithm
    #' @param ForCSClimateFile Character. Relative file path. Mean-annual-temperature
    #'   climate file specific to ForCS (see [prepClimateFile()]).
    #' @param InitialCommunitiesFiles Character. Two-element vector of relative
    #'   file paths: the initial-communities text/CSV and the initial-communities
    #'   raster.
    #' @param DisturbanceMatrixFile Character. Relative file path.
    #' @param SnagFile (Optional) Character. Relative file path. May be `NULL`.
    #' @param OutputTables `data.frame` corresponding to `ForCSOutput` (one row,
    #'   four columns: Biomass, DOM_Pools, Fluxes, Summary intervals).
    #' @param ForCSMapControl `data.frame` (one row, seven columns: `BiomassC`,
    #'   `SDOMC`, `NBP`, `NEP`, `NPP`, `RH`, `ToFPS` toggles).
    #' @param MapOutputInterval Integer. Map output interval (years).
    #' @param SpinUp `data.frame` (one row, four columns: On/Off Flag,
    #'   Biomass Spin-up Flag, Tolerance %, Max Iterations).
    #' @param AvailableLightBiomass `data.frame`.
    #' @param LightEstablishmentTable `data.frame`.
    #' @param SpeciesParameters `data.frame` with 10 columns: `Species`,
    #'   `LeafLong`, `MortalShape`, `MerchMinAge`, `MerchCurveA`, `MerchCurveB`,
    #'   `PropNonMerchToFastAG`, `GrowthShape`, `ShadeTolerance`, `FireTolerance`.
    #' @param DOMPools `data.frame`.
    #' @param EcoSppDOMParameters `data.frame`.
    #' @param ForCSProportions `data.frame`.
    #' @param ANPPTimeSeries `data.frame`.
    #' @param MaxBiomassTimeSeries `data.frame`.
    #' @param EstablishProbabilities `data.frame`.
    #' @param RootDynamics `data.frame`.
    #'
    initialize = function(
      path,
      Timestep = 1,
      SeedingAlgorithm = NULL,
      ForCSClimateFile = NULL,
      InitialCommunitiesFiles = NULL,
      DisturbanceMatrixFile = NULL,
      SnagFile = NULL,
      OutputTables = NULL,
      ForCSMapControl = NULL,
      MapOutputInterval = NULL,
      SpinUp = NULL,
      AvailableLightBiomass = NULL,
      LightEstablishmentTable = NULL,
      SpeciesParameters = NULL,
      DOMPools = NULL,
      EcoSppDOMParameters = NULL,
      ForCSProportions = NULL,
      ANPPTimeSeries = NULL,
      MaxBiomassTimeSeries = NULL,
      EstablishProbabilities = NULL,
      RootDynamics = NULL
    ) {
      stopifnot(!is.null(path))

      ## LandisExtension fields
      private$.LandisData <- "ForC Succession"
      self$Timestep <- Timestep

      self$type <- "succession"
      self$path <- path
      self$files <- "forc-succession.txt" ## file won't exist yet

      ## additional fields for this extension
      self$SeedingAlgorithm <- SeedingAlgorithm %||% "WardSeedDispersal"
      self$ForCSClimateFile <- ForCSClimateFile
      self$InitialCommunitiesFiles <- InitialCommunitiesFiles
      self$DisturbanceMatrixFile <- DisturbanceMatrixFile
      self$SnagFile <- SnagFile
      self$OutputTables <- OutputTables
      self$ForCSMapControl <- ForCSMapControl
      self$MapOutputInterval <- MapOutputInterval
      self$SpinUp <- SpinUp
      self$AvailableLightBiomass <- AvailableLightBiomass
      self$LightEstablishmentTable <- LightEstablishmentTable
      self$SpeciesParameters <- SpeciesParameters
      self$DOMPools <- DOMPools
      self$EcoSppDOMParameters <- EcoSppDOMParameters
      self$ForCSProportions <- ForCSProportions
      self$ANPPTimeSeries <- ANPPTimeSeries
      self$MaxBiomassTimeSeries <- MaxBiomassTimeSeries
      self$EstablishProbabilities <- EstablishProbabilities
      self$RootDynamics <- RootDynamics
    },

    #' @description Write extension inputs to disk
    write = function() {
      ## Note on ForCSClimateFile: !is.null() is not enough -- when the caller
      ## passes character(0) (e.g. from a {targets} target that returned an
      ## empty path in the wrong upstream mode), the active binding stores
      ## character(0), the !is.null() check passes, but the rendered
      ## forc-succession.txt silently omits the ForCSClimateFile line and
      ## then fails to parse at sim time. Require non-empty character(s).
      stopifnot(
        !is.null(self$SeedingAlgorithm),
        is.character(self$ForCSClimateFile),
        length(self$ForCSClimateFile) >= 1L,
        all(nzchar(self$ForCSClimateFile)),
        !is.null(self$InitialCommunitiesFiles),
        !is.null(self$DisturbanceMatrixFile),
        !is.null(self$OutputTables),
        !is.null(self$ForCSMapControl),
        !is.null(self$MapOutputInterval),
        !is.null(self$SpinUp),
        !is.null(self$AvailableLightBiomass),
        !is.null(self$LightEstablishmentTable),
        !is.null(self$SpeciesParameters),
        !is.null(self$DOMPools),
        !is.null(self$EcoSppDOMParameters),
        !is.null(self$ForCSProportions),
        !is.null(self$ANPPTimeSeries),
        !is.null(self$MaxBiomassTimeSeries),
        !is.null(self$EstablishProbabilities),
        !is.null(self$RootDynamics)
      )

      snag_lines <- if (!is.null(self$SnagFile)) {
        insertFile("SnagFile", self$SnagFile)
      } else {
        character(0)
      }

      writeLines(
        c(
          insertLandisData(private$.LandisData),
          insertValue("Timestep", self$Timestep),
          insertValue("SeedingAlgorithm", self$SeedingAlgorithm),
          insertFile("ForCSClimateFile", self$ForCSClimateFile),
          insertInitialCommunities(self$InitialCommunitiesFiles),
          insertFile("DisturbanceMatrixFile", self$DisturbanceMatrixFile),
          snag_lines,
          insertOutputTables(self$OutputTables),
          insertForCSMapControl(self$ForCSMapControl),
          insertValue("MapOutputInterval", self$MapOutputInterval),
          insertSpinUp(self$SpinUp),
          insertAvailableLightBiomass(self$AvailableLightBiomass),
          insertLightEstablishmentTable(self$LightEstablishmentTable),
          insertSpeciesParameters(self$SpeciesParameters),
          insertDOMPools(self$DOMPools),
          insertEcoSppDOMParameters(self$EcoSppDOMParameters, self$path),
          insertForCSProportions(self$ForCSProportions),
          insertANPPTimeSeries(self$ANPPTimeSeries, self$path),
          insertMaxBiomassTimeSeries(self$MaxBiomassTimeSeries, self$path),
          insertEstablishProbabilities(self$EstablishProbabilities, self$path),
          insertRootDynamics(self$RootDynamics)
        ),
        file.path(self$path, self$files[1])
      )

      ## declare files
      self$add_file(self$ForCSClimateFile)
      self$add_file(self$InitialCommunitiesFiles)
      self$add_file(self$DisturbanceMatrixFile)
      if (!is.null(self$SnagFile)) {
        self$add_file(self$SnagFile)
      }
      ## ForCS v4 CSV files written by insert*() above
      self$add_file("ForCS_EcoSppDOMParameters.csv")
      self$add_file("ForCS_ANPPTimeSeries.csv")
      self$add_file("ForCS_MaxBiomassTimeSeries.csv")
      self$add_file("ForCS_EstablishProbabilities.csv")

      return(invisible(self))
    }
  ),

  private = list(
    .SeedingAlgorithm = NULL,
    .ForCSClimateFile = NULL,
    .InitialCommunitiesFiles = NULL,
    .DisturbanceMatrixFile = NULL,
    .SnagFile = NULL,
    .OutputTables = NULL,
    .ForCSMapControl = NULL,
    .MapOutputInterval = NULL,
    .SpinUp = NULL,
    .AvailableLightBiomass = NULL,
    .LightEstablishmentTable = NULL,
    .SpeciesParameters = NULL,
    .DOMPools = NULL,
    .EcoSppDOMParameters = NULL,
    .ForCSProportions = NULL,
    .ANPPTimeSeries = NULL,
    .MaxBiomassTimeSeries = NULL,
    .EstablishProbabilities = NULL,
    .RootDynamics = NULL
  ),

  active = list(
    #' @template field_SeedingAlgorithm
    SeedingAlgorithm = function(value) {
      if (missing(value)) {
        return(private$.SeedingAlgorithm)
      } else {
        .checkSeedingAlgorithm(value)
        private$.SeedingAlgorithm <- value
      }
    },

    #' @field ForCSClimateFile Character. Relative file path.
    ForCSClimateFile = function(value) {
      if (missing(value)) {
        return(private$.ForCSClimateFile)
      } else {
        private$.ForCSClimateFile <- .relPath(value, self$path)
      }
    },

    #' @field InitialCommunitiesFiles Character. Relative file paths.
    InitialCommunitiesFiles = function(value) {
      if (missing(value)) {
        return(private$.InitialCommunitiesFiles)
      } else {
        private$.InitialCommunitiesFiles <- .relPath(value, self$path)
      }
    },

    #' @field DisturbanceMatrixFile Character. Relative file path.
    DisturbanceMatrixFile = function(value) {
      if (missing(value)) {
        return(private$.DisturbanceMatrixFile)
      } else {
        private$.DisturbanceMatrixFile <- .relPath(value, self$path)
      }
    },

    #' @field SnagFile (Optional) Character. Relative file path.
    SnagFile = function(value) {
      if (missing(value)) {
        return(private$.SnagFile)
      } else {
        private$.SnagFile <- .relPath(value, self$path)
      }
    },

    #' @field OutputTables `data.frame`.
    OutputTables = function(value) {
      if (missing(value)) {
        return(private$.OutputTables)
      } else {
        stopifnot(inherits(value, "data.frame")) ## TODO: additional column checks

        private$.OutputTables <- value
      }
    },

    #' @field ForCSMapControl `data.frame`.
    ForCSMapControl = function(value) {
      if (missing(value)) {
        return(private$.ForCSMapControl)
      } else {
        stopifnot(inherits(value, "data.frame"), nrow(value) == 1L, ncol(value) == 7L)

        private$.ForCSMapControl <- value
      }
    },

    #' @field MapOutputInterval Integer.
    MapOutputInterval = function(value) {
      if (missing(value)) {
        return(private$.MapOutputInterval)
      } else {
        stopifnot(is.numeric(value), length(value) == 1L, value > 0)

        private$.MapOutputInterval <- as.integer(value)
      }
    },

    #' @field SpinUp `data.frame`.
    SpinUp = function(value) {
      if (missing(value)) {
        return(private$.SpinUp)
      } else {
        stopifnot(inherits(value, "data.frame"), nrow(value) == 1L, ncol(value) == 4L)

        private$.SpinUp <- value
      }
    },

    #' @field AvailableLightBiomass `data.frame`.
    AvailableLightBiomass = function(value) {
      if (missing(value)) {
        return(private$.AvailableLightBiomass)
      } else {
        stopifnot(inherits(value, "data.frame")) ## TODO: additional column checks

        private$.AvailableLightBiomass <- value
      }
    },

    #' @field LightEstablishmentTable `data.frame`.
    LightEstablishmentTable = function(value) {
      if (missing(value)) {
        return(private$.LightEstablishmentTable)
      } else {
        stopifnot(inherits(value, "data.frame")) ## TODO: additional column checks

        private$.LightEstablishmentTable <- value
      }
    },

    #' @field SpeciesParameters `data.frame`.
    SpeciesParameters = function(value) {
      if (missing(value)) {
        return(private$.SpeciesParameters)
      } else {
        stopifnot(inherits(value, "data.frame")) ## TODO: additional column checks

        private$.SpeciesParameters <- value
      }
    },

    #' @field DOMPools `data.frame`.
    DOMPools = function(value) {
      if (missing(value)) {
        return(private$.DOMPools)
      } else {
        stopifnot(inherits(value, "data.frame")) ## TODO: additional column checks

        private$.DOMPools <- value
      }
    },

    #' @field EcoSppDOMParameters `data.frame`.
    EcoSppDOMParameters = function(value) {
      if (missing(value)) {
        return(private$.EcoSppDOMParameters)
      } else {
        stopifnot(inherits(value, "data.frame")) ## TODO: additional column checks

        private$.EcoSppDOMParameters <- value
      }
    },

    #' @field ForCSProportions `data.frame`.
    ForCSProportions = function(value) {
      if (missing(value)) {
        return(private$.ForCSProportions)
      } else {
        stopifnot(inherits(value, "data.frame")) ## TODO: additional column checks

        private$.ForCSProportions <- value
      }
    },

    #' @field ANPPTimeSeries `data.frame`.
    ANPPTimeSeries = function(value) {
      if (missing(value)) {
        return(private$.ANPPTimeSeries)
      } else {
        stopifnot(inherits(value, "data.frame")) ## TODO: additional column checks

        private$.ANPPTimeSeries <- value
      }
    },

    #' @field MaxBiomassTimeSeries `data.frame`.
    MaxBiomassTimeSeries = function(value) {
      if (missing(value)) {
        return(private$.MaxBiomassTimeSeries)
      } else {
        stopifnot(inherits(value, "data.frame")) ## TODO: additional column checks

        private$.MaxBiomassTimeSeries <- value
      }
    },

    #' @field EstablishProbabilities `data.frame`.
    EstablishProbabilities = function(value) {
      if (missing(value)) {
        return(private$.EstablishProbabilities)
      } else {
        stopifnot(inherits(value, "data.frame")) ## TODO: additional column checks

        private$.EstablishProbabilities <- value
      }
    },

    #' @field RootDynamics `data.frame`.
    RootDynamics = function(value) {
      if (missing(value)) {
        return(private$.RootDynamics)
      } else {
        stopifnot(inherits(value, "data.frame")) ## TODO: additional column checks

        private$.RootDynamics <- value
      }
    },

    #' @field output_files Character vector of ForCS-specific log CSV files
    #'   produced at run time, relative to the scenario directory (root level).
    #'   These fixed names are written by the ForCS extension regardless of
    #'   scenario configuration. See [LandisExtension] for the contract.
    output_files = function(value) {
      if (!missing(value)) {
        stop("`output_files` is read-only", call. = FALSE)
      }
      c(
        "log_BiomassC.csv",
        "log_Flux.csv",
        "log_FluxBio.csv",
        "log_FluxDOM.csv",
        "log_Pools.csv",
        "log_Summary.csv"
      )
    }
  )
)

#' Prepare `ForCSClimateFile` for Forest Carbon Succession (ForCS) extension
#'
#' @param df `data.frame`
#'
#' @template param_path
#'
#' @template param_filename
#'
#' @template return_file
#'
#' @returns data.frame
#'
#' @family ForCS helpers
#'
#' @export
prepClimateFile <- function(df = NULL, path, filename = "ForCS_climate.txt") {
  ## TODO: additional checks for columns
  stopifnot(is.data.frame(df), ncol(df) == 3L)

  file <- file.path(path, filename)
  writeLines(
    c(
      insertLandisData("ForC Succession"),
      glue::glue(""),
      glue::glue("ClimateTable"),
      glue::glue(">> Time  Ecoregion  AvgT"),
      glue::glue(">> Step             (C)"),
      glue::glue(">> ---------------------"),
      apply(df, 1, function(x) {
        glue::glue_collapse(x, sep = "  ")
      }),
      glue::glue("") ## blank line after each item group
    ),
    file
  )

  return(file)
}

#' Prepare `DisturbanceMatrixFile` for Forest Carbon Succession (ForCS) extension
#'
#' @param DisturbFireTransferDOM `data.frame`
#'
#' @param DisturbOtherTransferDOM `data.frame`
#'
#' @param DisturbFireTransferBiomass `data.frame`
#'
#' @param DisturbOtherTransferBiomass `data.frame`
#'
#' @template param_path
#'
#' @template param_filename
#'
#' @template return_file
#'
#' @returns data.frame
#'
#' @family ForCS helpers
#'
#' @export
prepDisturbanceMatrixFile <- function(
  DisturbFireTransferDOM = NULL,
  DisturbOtherTransferDOM = NULL,
  DisturbFireTransferBiomass = NULL,
  DisturbOtherTransferBiomass = NULL,
  path,
  filename = "ForCS_DM.txt"
) {
  ## TODO: additional checks for columns
  stopifnot(
    is.data.frame(DisturbFireTransferDOM),
    is.data.frame(DisturbOtherTransferDOM),
    is.data.frame(DisturbFireTransferBiomass),
    is.data.frame(DisturbOtherTransferBiomass)
  )

  file <- file.path(path, filename)
  writeLines(
    c(
      insertLandisData("ForC Succession"),
      glue::glue(""),
      glue::glue("DisturbFireTransferDOM"),
      glue::glue(">> Intensity  From     To   To   To"),
      glue::glue(">>            Biomass  Air  DOM  FPS"),
      glue::glue(">> -----------------------------------"),
      apply(DisturbFireTransferDOM, 1, function(x) {
        glue::glue_collapse(x, sep = "  ")
      }),
      glue::glue(""), ## blank line after each item group
      glue::glue("DisturbOtherTransferDOM"),
      glue::glue(">> Disturbance  From  To   To   To"),
      glue::glue(">> Type         DOM   Air  DOM  FPS"),
      glue::glue(">> --------------------------------"),
      apply(DisturbOtherTransferDOM, 1, function(x) {
        glue::glue_collapse(x, sep = "  ")
      }),
      glue::glue(""), ## blank line after each item group
      glue::glue("DisturbFireTransferBiomass"),
      glue::glue(">> Intensity  From     To   To   To"),
      glue::glue(">>            Biomass  Air  FPS  DOM"),
      glue::glue(">> -----------------------------------"),
      apply(DisturbFireTransferBiomass, 1, function(x) {
        glue::glue_collapse(x, sep = "  ")
      }),
      glue::glue(""), ## blank line after each item group
      glue::glue("DisturbOtherTransferBiomass"),
      glue::glue(">> Disturbance  From     To   To   To"),
      glue::glue(">> Type         Biomass  Air  FPS  DOM"),
      glue::glue(">> -----------------------------------"),
      apply(DisturbOtherTransferBiomass, 1, function(x) {
        glue::glue_collapse(x, sep = "  ")
      }),
      glue::glue("") ## add blank line after each item group
    ),
    file
  )

  return(file)
}

#' Prepare `SnagFile` for Forest Carbon Succession (ForCS) extension
#'
#' @param df `data.frame`
#'
#' @template param_path
#'
#' @template param_filename
#'
#' @template return_file
#'
#' @returns data.frame
#'
#' @family ForCS helpers
#'
#' @export
prepSnagFile <- function(df = NULL, path, filename = "ForCS_snags.txt") {
  stopifnot(is.data.frame(df), ncol(df) == 4L, nrow(df) <= 20L)

  colnames(df) <- c("Species", "AgeAtDeath", "TimeSinceDeath", "Cause")
  df <- dplyr::arrange(df, AgeAtDeath) ## needs to be ascending order by AgeAtDeath

  file <- file.path(path, filename)
  writeLines(
    c(
      insertLandisData("ForC Succession"),
      glue::glue(""),
      glue::glue("SnagData"),
      glue::glue(">> Species  AgeAtDeath  TimeSinceDeath  Cause"),
      glue::glue(">> ------------------------------------------"),
      apply(df, 1, function(x) {
        glue::glue_collapse(x, sep = "  ")
      }),
      glue::glue("") ## blank line after each item group
    ),
    file
  )

  return(file)
}

#' Specify `OutputTables` for Forest Carbon Succession (ForCS) extension
#'
#' @param df data.frame corresponding to `ForCSOutput` table
#'
#' @returns data.frame
#'
#' @family ForCS helpers
#'
#' @export
insertOutputTables <- function(df = NULL) {
  if (is.null(df)) {
    df <- data.frame(Biomass = 1, DOM_Pools = 1, Fluxes = 1, Summary = 1)
  }

  c(
    glue::glue("ForCSOutput"),
    glue::glue(">> Output interval"),
    glue::glue(">> Biomass  DOM_pools  Fluxes  Summary"),
    glue::glue(">> -----------------------------------"),
    paste0("    ", glue::glue("{df[1, ]}") |> glue::glue_collapse(sep = "        ")),
    glue::glue("") ## add blank line after each item group
  )
}

#' Specify `SpinUp` for Forest Carbon Succession (ForCS) extension
#'
#' @param df data.frame with one row and four columns: On/Off flag, Biomass
#'   spin-up flag, Tolerance (%), Max iterations.
#'
#' @returns data.frame
#'
#' @family ForCS helpers
#'
#' @export
insertSpinUp <- function(df) {
  stopifnot(inherits(df, "data.frame"), nrow(df) == 1L, ncol(df) == 4L)

  c(
    glue::glue("SpinUp"),
    glue::glue(">>  On/Off  Biomass    Tolerance  Max"),
    glue::glue(">>  Flag    Spin-up    %          Iterations"),
    glue::glue(">>          Flag"),
    glue::glue(">>  ----------------------------------------"),
    apply(df, 1, function(x) {
      glue::glue_collapse(x, sep = "  ")
    }),
    glue::glue("") ## add blank line after each item group
  )
}

#' Specify `ForCSMapControl` for Forest Carbon Succession (ForCS) extension
#'
#' @param df data.frame with one row and seven columns: `BiomassC`, `SDOMC`,
#'   `NBP`, `NEP`, `NPP`, `RH`, `ToFPS` toggles (each `0` or `1`).
#'
#' @returns data.frame
#'
#' @family ForCS helpers
#'
#' @export
insertForCSMapControl <- function(df) {
  stopifnot(inherits(df, "data.frame"), nrow(df) == 1L, ncol(df) == 7L)

  c(
    glue::glue("ForCSMapControl"),
    glue::glue(">>  BiomassC  SDOMC  NBP  NEP  NPP  RH  ToFPS"),
    glue::glue(">>  -----------------------------------------"),
    apply(df, 1, function(x) {
      glue::glue_collapse(x, sep = "  ")
    }),
    glue::glue("") ## add blank line after each item group
  )
}

#' Specify `AvailableLightBiomass` for Forest Carbon Succession (ForCS) extension
#'
#' @param df data.frame corresponding to `AvailableLightBiomass` table
#'
#' @returns data.frame
#'
#' @family ForCS helpers
#'
#' @export
insertAvailableLightBiomass <- function(df) {
  df <- cbind(df[, 1], apply(df[, -1], 2, appendPercent))

  c(
    glue::glue("AvailableLightBiomass"),
    glue::glue(">>  Shade"),
    glue::glue(">>  Class   Ecoregions"),
    glue::glue_collapse(colnames(df), sep = "   "),
    apply(df, 1, function(x) {
      glue::glue_collapse(x, sep = "  ")
    }),
    glue::glue("") ## add blank line after each item group
  )
}

#' Specify `LightEstablishmentTable` for Forest Carbon Succession (ForCS) extension
#'
#' @param df data.frame corresponding to `LightEstablishmentTable` table
#'
#' @returns data.frame
#'
#' @family ForCS helpers
#'
#' @export
insertLightEstablishmentTable <- function(df) {
  c(
    glue::glue("LightEstablishmentTable"),
    glue::glue(">>  Spp Shade        Probability"),
    glue::glue(">>  Class            by Actual Shade"),
    glue::glue(">>  ----------------------------------"),
    glue::glue(">>        0    1    2    3    4    5"),
    paste0(
      "   1            ",
      glue::glue("{format(df[1, -1], nsmall = 2)}") |> glue::glue_collapse(sep = "  ")
    ),
    paste0(
      "   2            ",
      glue::glue("{format(df[2, -1], nsmall = 2)}") |> glue::glue_collapse(sep = "  ")
    ),
    paste0(
      "   3            ",
      glue::glue("{format(df[3, -1], nsmall = 2)}") |> glue::glue_collapse(sep = "  ")
    ),
    paste0(
      "   4            ",
      glue::glue("{format(df[4, -1], nsmall = 2)}") |> glue::glue_collapse(sep = "  ")
    ),
    paste0(
      "   5            ",
      glue::glue("{format(df[5, -1], nsmall = 2)}") |> glue::glue_collapse(sep = "  ")
    ),
    glue::glue("") ## add blank line after each item group
  )
}

#' Specify `SpeciesParameters` for Forest Carbon Succession (ForCS) extension
#'
#' @param df data.frame with 10 columns: `Species`, `LeafLong`, `MortalShape`,
#'   `MerchMinAge`, `MerchCurveA`, `MerchCurveB`, `PropNonMerchToFastAG`,
#'   `GrowthShape`, `ShadeTolerance`, `FireTolerance`.
#'
#' @returns data.frame
#'
#' @family ForCS helpers
#'
#' @export
insertSpeciesParameters <- function(df) {
  stopifnot(ncol(df) == 10L)

  c(
    glue::glue("SpeciesParameters"),
    glue::glue(
      ">>  Species  Leaf  Mortal  Merchant  Merch    Merch     Prop       Growth  Shade      Fire"
    ),
    glue::glue(
      ">>           Long  Shape   Stems     Shape    Shape     Non-merch  Shape   Tolerance  Tolerance"
    ),
    glue::glue(">>                 Param   Min Age   Param a  Param b   to FastAG  Param"),
    glue::glue(
      ">>  ----------------------------------------------------------------------------------------"
    ),
    apply(df, 1, function(x) {
      glue::glue_collapse(x, sep = "  ")
    }),
    glue::glue("") ## add blank line after each item group
  )
}

#' Specify `DOMPools` for Forest Carbon Succession (ForCS) extension
#'
#' @param df data.frame corresponding to `DOMPools` table
#'
#' @returns data.frame
#'
#' @family ForCS helpers
#'
#' @export
insertDOMPools <- function(df) {
  stopifnot(ncol(df) == 3L)
  df[[2]] <- sprintf('"%s"', df[[2]]) ## LANDIS requires quoted strings for multi-word names

  c(
    glue::glue("DOMPools"),
    glue::glue(">>  ID    Name            Proportion to"),
    glue::glue(">>                        Atmosphere"),
    glue::glue(">>  -----------------------------------"),
    apply(df, 1, function(x) {
      glue::glue_collapse(x, sep = "  ")
    }),
    glue::glue("") ## add blank line after each item group
  )
}

#' Specify `EcoSppDOMParameters` for Forest Carbon Succession (ForCS) extension
#'
#' ForCS v4 reads this table from a CSV file rather than inline in the main config.
#' This function writes `ForCS_EcoSppDOMParameters.csv` to `path` and
#' returns the keyword + filename reference for inclusion in the main config.
#'
#' @param df data.frame corresponding to `EcoSppDOMParameters` table
#' @param path Character. Directory where the CSV file will be written.
#'
#' @returns Character vector (keyword line for the main config).
#'
#' @family ForCS helpers
#'
#' @export
insertEcoSppDOMParameters <- function(df, path) {
  stopifnot(ncol(df) == 6L)
  csv_file <- "ForCS_EcoSppDOMParameters.csv"
  colnames(df) <- c("Ecoregion", "Species", "DOMPool", "DecayRate", "AmountT0", "Q10")
  utils::write.csv(df, file.path(path, csv_file), row.names = FALSE, quote = FALSE)

  c(
    glue::glue("EcoSppDOMParameters\t\"{csv_file}\""),
    glue::glue("") ## add blank line after each item group
  )
}

#' Specify `ForCSProportions` for Forest Carbon Succession (ForCS) extension
#'
#' @param df data.frame corresponding to `ForCSProportions` table
#'
#' @returns data.frame
#'
#' @family ForCS helpers
#'
#' @export
insertForCSProportions <- function(df) {
  stopifnot(ncol(df) == 5L)

  c(
    glue::glue("ForCSProportions"),
    glue::glue(">>  Biomass  Biomass  Annual     Annual     Annual"),
    glue::glue(">>  Fine     Coarse   SlowAG     StemSnag   BranchSnag"),
    glue::glue(">>                    to SlowBG  to Medium  to FastAG"),
    glue::glue(">>  --------------------------------------------------"),
    apply(df, 1, function(x) {
      glue::glue_collapse(x, sep = "  ")
    }),
    glue::glue("") ## add blank line after each item group
  )
}

#' Specify `ANPPTimeSeries` for Forest Carbon Succession (ForCS) extension
#'
#' ForCS v4 reads this table from a CSV file rather than inline in the main
#' config. This function writes `ForCS_ANPPTimeSeries.csv` to `path` and returns
#' the keyword + filename reference for inclusion in the main config.
#'
#' @param df data.frame corresponding to `ANPPTimeSeries` table
#' @param path Character. Directory where the CSV file will be written.
#'
#' @returns Character vector (keyword line for the main config).
#'
#' @family ForCS helpers
#'
#' @export
insertANPPTimeSeries <- function(df, path) {
  stopifnot(ncol(df) == 5L)
  csv_file <- "ForCS_ANPPTimeSeries.csv"
  colnames(df) <- c("Year", "Ecoregion", "Species", "ANPP", "ANPP-Std")
  utils::write.csv(df, file.path(path, csv_file), row.names = FALSE, quote = FALSE)

  c(
    glue::glue("ANPPTimeSeries\t\"{csv_file}\""),
    glue::glue("") ## add blank line after each item group
  )
}

#' Specify `MaxBiomassTimeSeries` for Forest Carbon Succession (ForCS) extension
#'
#' ForCS v4 reads this table from a CSV file rather than inline in the main
#' config. This function writes `ForCS_MaxBiomassTimeSeries.csv` to `path` and
#' returns the keyword + filename reference for inclusion in the main config.
#'
#' @param df data.frame corresponding to `MaxBiomassTimeSeries` table
#' @param path Character. Directory where the CSV file will be written.
#'
#' @returns Character vector (keyword line for the main config).
#'
#' @family ForCS helpers
#'
#' @export
insertMaxBiomassTimeSeries <- function(df, path) {
  stopifnot(ncol(df) == 4L)
  csv_file <- "ForCS_MaxBiomassTimeSeries.csv"
  colnames(df) <- c("Year", "Ecoregion", "Species", "MaxBiomass")
  utils::write.csv(df, file.path(path, csv_file), row.names = FALSE, quote = FALSE)

  c(
    glue::glue("MaxBiomassTimeSeries\t\"{csv_file}\""),
    glue::glue("") ## add blank line after each item group
  )
}

#' Specify `EstablishProbabilities` for Forest Carbon Succession (ForCS) extension
#'
#' ForCS v4 reads this table from a CSV file rather than inline in the main
#' config. This function writes `ForCS_EstablishProbabilities.csv` to `path` and
#' returns the keyword + filename reference for inclusion in the main config.
#'
#' @param df data.frame corresponding to `EstablishProbabilities` table
#' @param path Character. Directory where the CSV file will be written.
#'
#' @returns Character vector (keyword line for the main config).
#'
#' @family ForCS helpers
#'
#' @export
insertEstablishProbabilities <- function(df, path) {
  stopifnot(ncol(df) == 4L)
  csv_file <- "ForCS_EstablishProbabilities.csv"
  colnames(df) <- c("Year", "Ecoregion", "Species", "Probability")
  utils::write.csv(df, file.path(path, csv_file), row.names = FALSE, quote = FALSE)

  c(
    glue::glue("EstablishProbabilities\t\"{csv_file}\""),
    glue::glue("") ## add blank line after each item group
  )
}

#' Specify `RootDynamics` for Forest Carbon Succession (ForCS) extension
#'
#' @param df data.frame corresponding to `RootDynamics` table
#'
#' @returns data.frame
#'
#' @family ForCS helpers
#'
#' @export
insertRootDynamics <- function(df) {
  stopifnot(
    ncol(df) == 7L
    ## TODO: verify MinABio values are in ascending order by species
  )

  c(
    glue::glue("RootDynamics"),
    glue::glue(">>  Ecoregion  Species  MinABio  Root  PropFineRt  Frturnover  Crturnover"),
    glue::glue(">>                      (g/m2)   Abio"),
    glue::glue(">>  ---------------------------------------------------------------------"),
    apply(df, 1, function(x) {
      glue::glue_collapse(x, sep = "  ")
    }),
    glue::glue("") ## add blank line after each item group
  )
}
