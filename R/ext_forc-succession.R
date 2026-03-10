utils::globalVariables(c("AgeAtDeath"))

#' Forest Carbon Succession (ForCS) Extension
#'
#' @include ext_utils.R
#'
#' @references LANDIS-II Social-Climate-Fire v4 User Guide
#'   <https://github.com/LANDIS-II-Foundation/Extension-Social-Climate-Fire/blob/master/docs/LANDIS-II%20Social-Climate-Fire%20v4%20User%20Guide.pdf>
#'
#' @export
#'
#' @examples
#' ## See vignette for example usage
#'
ForCS <- R6Class(
  "ForC Succession",
  inherit = LandisExtension,
  public = list(
    #' @param path Character. Directory path.
    #' @param Timestep Integer.
    #' @param SeedingAlgorithm Character. Dispersal algorithm to use.
    #'        One of `"WardSeedDispersal"`, `"NoDispersal"`, `"UniversalDispersal"`.
    #' @param ClimateFile Character. Relative file path.
    #' @param InitialCommunitiesFiles Character. Relative file paths.
    #' @param DisturbanceMatrixFile Character. Relative file path.
    #' @param SnagFile (Optional) Character. Relative file path.
    #' @param OutputTables `data.frame`.
    #' @param SoilSpinupControls `data.frame`.
    #' @param AvailableLightBiomass `data.frame`.
    #' @param LightEstablishmentTable `data.frame`.
    #' @param SpeciesParameters `data.frame`.
    #' @param DOMPools `data.frame`.
    #' @param EcoSppDOMParameters `data.frame`.
    #' @param ForCSProportions `data.frame`.
    #' @param ANPPTimeSeries `data.frame`.
    #' @param MaxBiomassTimeSeries `data.frame`.
    #' @param EstablishProbabilities `data.frame`.
    #' @param RootDynamics `data.frame`.
    #'
    #' @references TODO
    #'
    initialize = function(
      path,
      Timestep = 1,
      SeedingAlgorithm = NULL,
      ClimateFile = NULL,
      InitialCommunitiesFiles = NULL,
      DisturbanceMatrixFile = NULL,
      SnagFile = NULL,
      OutputTables = NULL,
      SoilSpinupControls = NULL,
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
      self$ClimateFile <- ClimateFile
      self$InitialCommunitiesFiles <- InitialCommunitiesFiles
      self$DisturbanceMatrixFile <- DisturbanceMatrixFile
      self$SnagFile <- SnagFile
      self$OutputTables <- OutputTables
      self$SoilSpinupControls <- SoilSpinupControls
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
      stopifnot(
        !is.null(self$SeedingAlgorithm),
        !is.null(self$ClimateFile),
        !is.null(self$InitialCommunitiesFiles),
        !is.null(self$DisturbanceMatrixFile),
        !is.null(self$SnagFile),
        !is.null(self$OutputTables),
        !is.null(self$SoilSpinupControls),
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

      writeLines(
        c(
          insertLandisData(private$.LandisData),
          insertValue("Timestep", self$Timestep),
          insertFile("ClimateFile", self$ClimateFile),
          insertInitialCommunities(self$InitialCommunitiesFiles), ## TODO
          insertFile("DisturbanceMatrixFile", self$DisturbanceMatrixFile),
          insertFile("SnagFile", self$SnagFile),
          insertOutputTables(self$OutputTables),
          insertSoilSpinupControls(self$SoilSpinupControls),
          insertAvailableLightBiomass(self$AvailableLightBiomass),
          insertLightEstablishmentTable(self$LightEstablishmentTable),
          insertSpeciesParameters(self$SpeciesParameters),
          insertDOMPools(self$DOMPools),
          insertEcoSppDOMParameters(self$EcoSppDOMParameters),
          insertForCSProportions(self$ForCSProportions),
          insertANPPTimeSeries(self$ANPPTimeSeries),
          insertMaxBiomassTimeSeries(self$MaxBiomassTimeSeries),
          insertEstablishProbabilities(self$EstablishProbabilities),
          insertRootDynamics(self$RootDynamics)
        ),
        file.path(self$path, self$files[1])
      )

      ## declare files
      self$add_file(self$ClimateFile)
      self$add_file(self$InitialCommunitiesFiles)
      self$add_file(self$DisturbanceMatrixFile)
      self$add_file(self$SnagFile)

      return(invisible(self))
    }
  ),

  private = list(
    .SeedingAlgorithm = NULL,
    .ClimateFile = NULL,
    .InitialCommunitiesFiles = NULL,
    .DisturbanceMatrixFile = NULL,
    .SnagFile = NULL,
    .OutputTables = NULL,
    .SoilSpinupControls = NULL,
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
    #' @field SeedingAlgorithm Character. Dispersal algorithm to use.
    #'        One of `"WardSeedDispersal"`, `"NoDispersal"`, `"UniversalDispersal"`.
    SeedingAlgorithm = function(value) {
      if (missing(value)) {
        return(private$.SeedingAlgorithm)
      } else {
        stopifnot(value %in% c("WardSeedDispersal", "NoDispersal", "UniversalDispersal"))

        private$.SeedingAlgorithm <- value
      }
    },

    #' @field ClimateFile Character. Relative file path.
    ClimateFile = function(value) {
      if (missing(value)) {
        return(private$.ClimateFile)
      } else {
        private$.ClimateFile <- .relPath(value, self$path)
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

    #' @field SoilSpinupControls `data.frame`.
    SoilSpinupControls = function(value) {
      if (missing(value)) {
        return(private$.SoilSpinupControls)
      } else {
        stopifnot(inherits(value, "data.frame")) ## TODO: additional column checks

        private$.SoilSpinupControls <- value
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
    }
  )
)

#' Prepare `ClimateFile` for Forest Carbon Succession (ForCS) extension
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

#' Specify `SoilSpinup` for Forest Carbon Succession (ForCS) extension
#'
#' @param df data.frame corresponding to `SoilSpinup` table
#'
#' @returns data.frame
#'
#' @export
insertSoilSpinupControls <- function(df) {
  c(
    glue::glue("SoilSpinup"),
    glue::glue(">>  On/Off  Tolerance  Max"),
    glue::glue(">>  Flag    %          Iterations"),
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
#' @param df data.frame corresponding to `SpeciesParameters` table
#'
#' @returns data.frame
#'
#' @export
insertSpeciesParameters <- function(df) {
  stopifnot(ncol(df) == 8L)

  c(
    glue::glue("SpeciesParameters"),
    glue::glue(">>  Species  Leaf  Mortal  Merchant  Merch    Merch     Prop       Growth"),
    glue::glue(">>           Long  Shape   Stems     Shape    Shape     Non-merch  Shape"),
    glue::glue(">>                 Param   Min Age   Param a  Param b   to FastAG  Param"),
    glue::glue(">>  ---------------------------------------------------------------------"),
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
#' @export
insertDOMPools <- function(df) {
  stopifnot(ncol(df) == 3L)

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
#' @param df data.frame corresponding to `EcoSppDOMParameters` table
#'
#' @returns data.frame
#'
#' @export
insertEcoSppDOMParameters <- function(df) {
  stopifnot(ncol(df) == 6L)

  c(
    glue::glue("EcoSppDOMParameters"),
    glue::glue(">>  Ecoregion  Species  DOM   Decay  Amount  Q10 Ref"),
    glue::glue(">>                      Pool  Rate   at T0   Temp 10C"),
    glue::glue(">>  -------------------------------------------------"),
    apply(df, 1, function(x) {
      glue::glue_collapse(x, sep = "  ")
    }),
    glue::glue("") ## add blank line after each item group
  )
}

#' Specify `ForCSProportions` for Forest Carbon Succession (ForCS) extension
#'
#' @param df data.frame corresponding to `ForCSProportions` table
#'
#' @returns data.frame
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
#' @param df data.frame corresponding to `ANPPTimeSeries` table
#'
#' @returns data.frame
#'
#' @export
insertANPPTimeSeries <- function(df) {
  stopifnot(ncol(df) == 5L)

  c(
    glue::glue("ANPPTimeSeries"),
    glue::glue(">>  Year  Ecoregion  Species  ANPP       ANPP-std"),
    glue::glue(">>                            (g/m2/yr)"),
    glue::glue(">>  ---------------------------------------------"),
    apply(df, 1, function(x) {
      glue::glue_collapse(x, sep = "  ")
    }),
    glue::glue("") ## add blank line after each item group
  )
}

#' Specify `MaxBiomassTimeSeries` for Forest Carbon Succession (ForCS) extension
#'
#' @param df data.frame corresponding to `MaxBiomassTimeSeries` table
#'
#' @returns data.frame
#'
#' @export
insertMaxBiomassTimeSeries <- function(df) {
  stopifnot(ncol(df) == 4L)

  c(
    glue::glue("MaxBiomassTimeSeries"),
    glue::glue(">>  Year  Ecoregion  Species  Max Biomass (g/m2)"),
    glue::glue(">>  --------------------------------------------"),
    apply(df, 1, function(x) {
      glue::glue_collapse(x, sep = "  ")
    }),
    glue::glue("") ## add blank line after each item group
  )
}

#' Specify `EstablishProbabilities` for Forest Carbon Succession (ForCS) extension
#'
#' @param df data.frame corresponding to `EstablishProbabilities` table
#'
#' @returns data.frame
#'
#' @export
insertEstablishProbabilities <- function(df) {
  stopifnot(ncol(df) == 4L)

  c(
    glue::glue("EstablishProbabilities"),
    glue::glue(">>  Year  Ecoregion  Species  Probability"),
    glue::glue(">>  -------------------------------------"),
    apply(df, 1, function(x) {
      glue::glue_collapse(x, sep = "  ")
    }),
    glue::glue("") ## add blank line after each item group
  )
}

#' Specify `RootDynamics` for Forest Carbon Succession (ForCS) extension
#'
#' @param df data.frame corresponding to `RootDynamics` table
#'
#' @returns data.frame
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
