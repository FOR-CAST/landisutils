#' Root Rot Extension
#'
#' @section LANDIS-II v8 compatibility:
#' Root Rot has not yet been updated by the LANDIS-II developers for
#' LANDIS-II v8; the most recent upstream release targets the v7 core only.
#' This R6 class is provided to track the v1.0 schema and exercise the
#' input-file generation, but `$write()`-produced files cannot currently be
#' run through a LANDIS-II v8 console. Constructing a `RootRot` object emits
#' a one-time `warning()` to remind users of this. The warning will be removed once
#' a v8-compatible Root Rot release is available
#'
#' @include ext_utils.R
#'
#' @references LANDIS-II Root Rot v1.0 Extension User Guide
#'   <https://github.com/LANDIS-II-Foundation/Extension-Root-Rot/blob/master/Docs/LANDIS-II%20Root%20Rot%20v1.0%20User%20Guide.pdf>
#'
#' @family Root Rot helpers
#'
#' @export
RootRot <- R6Class(
  "RootRot",
  inherit = LandisExtension,
  public = list(
    #' @param path Character. Directory path.
    #' @param Timestep Integer. Years between updates.
    #' @param Pathogen (Optional) Character. Pathogen name to annotate alongside
    #'   the `LandisData` header (e.g. `"Phytophthora cinnamomi"`).
    #' @param InputMap (Optional) Character. Initial infection raster path.
    #'   When omitted, the model assumes all cells begin Uninfected.
    #' @param SpeciesSusceptibility `data.frame` with columns `Species`,
    #'   `Index1` (initial susceptibility), `Index2` (secondary susceptibility);
    #'   both indices in `[0, 1]`.
    #' @param LethalTemp Numeric `<= 0` (°C). Minimum air temperature below
    #'   which the pathogen cannot survive.
    #' @param MinSoilTemp Numeric (°C). Minimum soil temperature below which
    #'   the pathogen cannot transition Uninfected → Infected.
    #' @param PhWet,PhDry,PhMax Numeric `> 0` (m). Pressure-head thresholds:
    #'   wet, dry, and extremely-dry conditions.
    #' @param MinProbID,MaxProbDI Numeric in `[0, 1]`. Min infection→diseased
    #'   probability and max diseased→infected probability.
    #' @param OutputMapName (Optional) Character. Infection-status output
    #'   raster pattern; must contain `{timestep}`.
    #' @param TOLDMapName (Optional) Character. Time-of-Last-Disease output
    #'   raster pattern; must contain `{timestep}`.
    #' @param LethalTempMapName (Optional) Character. Lethal-temperature
    #'   output raster pattern; must contain `{timestep}`.
    #' @param TotalBiomassRemovedMapName (Optional) Character. Total biomass-
    #'   removed output raster pattern; must contain `{timestep}`.
    #' @param SpeciesBiomassRemovedMapName (Optional) Character. Per-species
    #'   biomass-removed output raster pattern; must contain both `{species}`
    #'   and `{timestep}`.
    #' @param EventLog (Optional) Character. Relative file path for the events
    #'   CSV log; pass `NULL` to disable.
    #' @param SummaryLog (Optional) Character. Relative file path for the
    #'   summary CSV log; pass `NULL` to disable.
    initialize = function(
      path,
      Timestep = NULL,
      Pathogen = NULL,
      InputMap = NULL,
      SpeciesSusceptibility = NULL,
      LethalTemp = NULL,
      MinSoilTemp = NULL,
      PhWet = NULL,
      PhDry = NULL,
      PhMax = NULL,
      MinProbID = NULL,
      MaxProbDI = NULL,
      OutputMapName = NULL,
      TOLDMapName = NULL,
      LethalTempMapName = NULL,
      TotalBiomassRemovedMapName = NULL,
      SpeciesBiomassRemovedMapName = NULL,
      EventLog = "rootrot/events.csv",
      SummaryLog = "rootrot/summary.csv"
    ) {
      stopifnot(!is.null(path))

      warning(
        "Root Rot has not yet been updated for LANDIS-II v8; ",
        "the input file produced by `$write()` will only be parseable ",
        "by a v7-compatible Root Rot release. ",
        "See the package README and `?RootRot` for status.",
        call. = FALSE
      )

      ## LandisExtension fields
      private$.LandisData <- "Root Rot"
      self$Timestep <- Timestep

      self$type <- "disturbance"
      self$path <- path
      self$files <- "root-rot.txt" ## file won't exist yet

      ## additional fields for this extension
      self$Pathogen <- Pathogen
      self$InputMap <- InputMap
      self$SpeciesSusceptibility <- SpeciesSusceptibility
      self$LethalTemp <- LethalTemp
      self$MinSoilTemp <- MinSoilTemp
      self$PhWet <- PhWet
      self$PhDry <- PhDry
      self$PhMax <- PhMax
      self$MinProbID <- MinProbID
      self$MaxProbDI <- MaxProbDI
      self$OutputMapName <- OutputMapName %||% "rootrot/RootRot-{timestep}.img"
      self$TOLDMapName <- TOLDMapName %||% "rootrot/TOLD-{timestep}.img"
      self$LethalTempMapName <- LethalTempMapName
      self$TotalBiomassRemovedMapName <- TotalBiomassRemovedMapName %||%
        "rootrot/BiomassRemoved-{timestep}.img"
      self$SpeciesBiomassRemovedMapName <- SpeciesBiomassRemovedMapName %||%
        "rootrot/BiomassRemoved-{species}-{timestep}.img"
      self$EventLog <- EventLog
      self$SummaryLog <- SummaryLog
    },

    #' @description Write extension inputs to disk
    write = function() {
      stopifnot(
        !is.null(self$SpeciesSusceptibility),
        !is.null(self$LethalTemp),
        !is.null(self$MinSoilTemp),
        !is.null(self$PhWet),
        !is.null(self$PhDry),
        !is.null(self$PhMax),
        !is.null(self$MinProbID),
        !is.null(self$MaxProbDI)
      )

      header <- if (!is.null(self$Pathogen) && nzchar(self$Pathogen)) {
        c(
          landisutilsHeader(),
          glue::glue("LandisData  \"{private$.LandisData}\"  <<{self$Pathogen}"),
          glue::glue("")
        )
      } else {
        insertLandisData(private$.LandisData)
      }

      writeLines(
        c(
          header,
          insertValue("Timestep", self$Timestep),
          if (!is.null(self$InputMap)) insertValue("InputMap", self$InputMap),
          insertRootRotSpeciesSusceptibility(self$SpeciesSusceptibility),
          insertValue("LethalTemp", self$LethalTemp, blank_line = FALSE),
          insertValue("MinSoilTemp", self$MinSoilTemp, blank_line = FALSE),
          insertValue("PhWet", self$PhWet, blank_line = FALSE),
          insertValue("PhDry", self$PhDry, blank_line = FALSE),
          insertValue("PhMax", self$PhMax, blank_line = FALSE),
          insertValue("MinProbID", self$MinProbID, blank_line = FALSE),
          insertValue("MaxProbDI", self$MaxProbDI),
          if (!is.null(self$OutputMapName)) {
            insertValue("OutputMapName", self$OutputMapName, blank_line = FALSE)
          },
          if (!is.null(self$TOLDMapName)) {
            insertValue("TOLDMapName", self$TOLDMapName, blank_line = FALSE)
          },
          if (!is.null(self$LethalTempMapName)) {
            insertValue("LethalTempMapName", self$LethalTempMapName, blank_line = FALSE)
          },
          if (!is.null(self$TotalBiomassRemovedMapName)) {
            insertValue(
              "TotalBiomassRemovedMapName",
              self$TotalBiomassRemovedMapName,
              blank_line = FALSE
            )
          },
          if (!is.null(self$SpeciesBiomassRemovedMapName)) {
            insertValue("SpeciesBiomassRemovedMapName", self$SpeciesBiomassRemovedMapName)
          },
          if (!is.null(self$EventLog)) {
            insertValue("EventLog", self$EventLog, blank_line = FALSE)
          },
          if (!is.null(self$SummaryLog)) {
            insertValue("SummaryLog", self$SummaryLog)
          }
        ),
        file.path(self$path, self$files[1])
      )

      return(invisible(self))
    }
  ),

  private = list(
    .Pathogen = NULL,
    .InputMap = NULL,
    .SpeciesSusceptibility = NULL,
    .LethalTemp = NULL,
    .MinSoilTemp = NULL,
    .PhWet = NULL,
    .PhDry = NULL,
    .PhMax = NULL,
    .MinProbID = NULL,
    .MaxProbDI = NULL,
    .OutputMapName = NULL,
    .TOLDMapName = NULL,
    .LethalTempMapName = NULL,
    .TotalBiomassRemovedMapName = NULL,
    .SpeciesBiomassRemovedMapName = NULL,
    .EventLog = NULL,
    .SummaryLog = NULL
  ),

  active = list(
    #' @field Pathogen (Optional) Character. Annotated alongside the header.
    Pathogen = function(value) {
      if (missing(value)) {
        return(private$.Pathogen)
      } else {
        if (!is.null(value)) {
          stopifnot(is.character(value), length(value) == 1L)
        }
        private$.Pathogen <- value
      }
    },

    #' @field InputMap (Optional) Character. Initial infection raster path.
    InputMap = function(value) {
      if (missing(value)) {
        return(private$.InputMap)
      } else {
        if (!is.null(value)) {
          private$.InputMap <- .relPath(value, self$path)
        } else {
          private$.InputMap <- NULL
        }
      }
    },

    #' @field SpeciesSusceptibility `data.frame` with columns `Species`,
    #'   `Index1`, `Index2`.
    SpeciesSusceptibility = function(value) {
      if (missing(value)) {
        return(private$.SpeciesSusceptibility)
      } else {
        if (!is.null(value)) {
          stopifnot(
            is.data.frame(value),
            all(c("Species", "Index1", "Index2") %in% colnames(value)),
            all(value$Index1 >= 0, value$Index1 <= 1),
            all(value$Index2 >= 0, value$Index2 <= 1)
          )
        }
        private$.SpeciesSusceptibility <- value
      }
    },

    #' @field LethalTemp Numeric `<= 0` (°C). Minimum air temperature below
    #'   which the pathogen cannot survive.
    LethalTemp = function(value) {
      if (missing(value)) {
        return(private$.LethalTemp)
      } else {
        if (!is.null(value)) {
          stopifnot(is.numeric(value), length(value) == 1L, value <= 0)
        }
        private$.LethalTemp <- value
      }
    },

    #' @field MinSoilTemp Numeric (°C). Minimum soil temperature below which
    #'   the pathogen cannot transition Uninfected -> Infected.
    MinSoilTemp = function(value) {
      if (missing(value)) {
        return(private$.MinSoilTemp)
      } else {
        if (!is.null(value)) {
          stopifnot(is.numeric(value), length(value) == 1L)
        }
        private$.MinSoilTemp <- value
      }
    },

    #' @field PhWet Numeric. Pressure head (wet condition threshold).
    PhWet = function(value) {
      if (missing(value)) {
        return(private$.PhWet)
      } else {
        if (!is.null(value)) {
          stopifnot(is.numeric(value), length(value) == 1L, value >= 0)
        }
        private$.PhWet <- value
      }
    },

    #' @field PhDry Numeric. Pressure head (dry condition threshold).
    PhDry = function(value) {
      if (missing(value)) {
        return(private$.PhDry)
      } else {
        if (!is.null(value)) {
          stopifnot(is.numeric(value), length(value) == 1L, value >= 0)
        }
        private$.PhDry <- value
      }
    },

    #' @field PhMax Numeric. Pressure head (maximum threshold).
    PhMax = function(value) {
      if (missing(value)) {
        return(private$.PhMax)
      } else {
        if (!is.null(value)) {
          stopifnot(is.numeric(value), length(value) == 1L, value >= 0)
        }
        private$.PhMax <- value
      }
    },

    #' @field MinProbID Numeric in `[0, 1]`.
    MinProbID = function(value) {
      if (missing(value)) {
        return(private$.MinProbID)
      } else {
        if (!is.null(value)) {
          stopifnot(is.numeric(value), length(value) == 1L, value >= 0, value <= 1)
        }
        private$.MinProbID <- value
      }
    },

    #' @field MaxProbDI Numeric in `[0, 1]`.
    MaxProbDI = function(value) {
      if (missing(value)) {
        return(private$.MaxProbDI)
      } else {
        if (!is.null(value)) {
          stopifnot(is.numeric(value), length(value) == 1L, value >= 0, value <= 1)
        }
        private$.MaxProbDI <- value
      }
    },

    #' @field OutputMapName Character. Infection-status output raster pattern;
    #'   must contain `{timestep}`.
    OutputMapName = function(value) {
      if (missing(value)) {
        return(private$.OutputMapName)
      } else {
        if (!is.null(value)) {
          stopifnot(grepl("{timestep}", value, fixed = TRUE))
        }
        private$.OutputMapName <- value
      }
    },

    #' @field TOLDMapName Character. Time-of-Last-Disease output raster
    #'   pattern; must contain `{timestep}`.
    TOLDMapName = function(value) {
      if (missing(value)) {
        return(private$.TOLDMapName)
      } else {
        if (!is.null(value)) {
          stopifnot(grepl("{timestep}", value, fixed = TRUE))
        }
        private$.TOLDMapName <- value
      }
    },

    #' @field LethalTempMapName Character. Lethal-temperature output raster
    #'   pattern; must contain `{timestep}`.
    LethalTempMapName = function(value) {
      if (missing(value)) {
        return(private$.LethalTempMapName)
      } else {
        if (!is.null(value)) {
          stopifnot(grepl("{timestep}", value, fixed = TRUE))
        }
        private$.LethalTempMapName <- value
      }
    },

    #' @field TotalBiomassRemovedMapName Character. Total biomass-removed
    #'   output pattern; must contain `{timestep}`.
    TotalBiomassRemovedMapName = function(value) {
      if (missing(value)) {
        return(private$.TotalBiomassRemovedMapName)
      } else {
        if (!is.null(value)) {
          stopifnot(grepl("{timestep}", value, fixed = TRUE))
        }
        private$.TotalBiomassRemovedMapName <- value
      }
    },

    #' @field SpeciesBiomassRemovedMapName Character. Per-species biomass-
    #'   removed output pattern; must contain `{species}` and `{timestep}`.
    SpeciesBiomassRemovedMapName = function(value) {
      if (missing(value)) {
        return(private$.SpeciesBiomassRemovedMapName)
      } else {
        if (!is.null(value)) {
          stopifnot(
            grepl("{species}", value, fixed = TRUE),
            grepl("{timestep}", value, fixed = TRUE)
          )
        }
        private$.SpeciesBiomassRemovedMapName <- value
      }
    },

    #' @field EventLog (Optional) Character. Relative file path for the
    #'   events CSV log; `NULL` disables.
    EventLog = function(value) {
      if (missing(value)) {
        return(private$.EventLog)
      } else if (is.null(value)) {
        private$.EventLog <- NULL
      } else {
        private$.EventLog <- .relPath(value, self$path)
      }
    },

    #' @field SummaryLog (Optional) Character. Relative file path for the
    #'   summary CSV log; `NULL` disables.
    SummaryLog = function(value) {
      if (missing(value)) {
        return(private$.SummaryLog)
      } else if (is.null(value)) {
        private$.SummaryLog <- NULL
      } else {
        private$.SummaryLog <- .relPath(value, self$path)
      }
    }
  )
)

#' Specify the `SpeciesSusceptibility` block for the Root Rot extension
#'
#' @param df `data.frame` with columns `Species`, `Index1`, `Index2`.
#'
#' @template return_insert
#'
#' @family Root Rot helpers
#'
#' @keywords internal
insertRootRotSpeciesSusceptibility <- function(df) {
  stopifnot(is.data.frame(df), all(c("Species", "Index1", "Index2") %in% colnames(df)))

  rows <- apply(df[, c("Species", "Index1", "Index2"), drop = FALSE], 1, function(x) {
    glue::glue("{format(x[['Species']], width = -10)}{x[['Index1']]}    {x[['Index2']]}")
  })

  c(
    glue::glue("SpeciesSusceptibility    <<index - 0.0=unsusceptible; 1.0=highly susceptible"),
    glue::glue(">> species    1st     2nd"),
    rows,
    glue::glue("")
  )
}
