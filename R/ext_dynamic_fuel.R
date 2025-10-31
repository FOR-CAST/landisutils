#' Dynamic Fuel System Extension
#'
#' @include ext_utils.R
#'
#' @references LANDIS-II Dynamic Fuel System Extension v4.0 User Guide
#'   <https://github.com/LANDIS-II-Foundation/Extension-Dynamic-Biomass-Fuels/blob/master/docs/LANDIS-II%20Dynamic%20Fuel%20System%20v4.0%20User%20Guide.pdf>
#'
#' @export
DynamicFuels <- R6Class(
  "DynamicFuels",
  inherit = LandisExtension,
  public = list(
    #' @param path Character. Directory path.
    #' @param Timestep Integer.
    #' @param SpeciesFuelCoefficients `data.frame`.
    #' @param HardwoodMaximum Integer.
    #' @param DeadFirMaxAge Integer. The duration of influence for the BDA extension's dead conifer index.
    #' @param FuelTypes `data.frame`. See [prepFuelTypesTable()].
    #' @param EcoregionTable `data.frame`.
    #' @param DisturbanceConversionTable `data.frame`.
    #' @param MapFileNames Character. File pattern for writing outputs to disk.
    #' @param PctConiferMapName Character. File pattern for writing outputs to disk.
    #' @param PctDeadFirMapName Character. File pattern for writing outputs to disk.
    initialize = function(
      path,
      Timestep = 10,
      SpeciesFuelCoefficients = NULL,
      HardwoodMaximum = 0L,
      DeadFirMaxAge = 15L,
      FuelTypes = NULL,
      EcoregionTable = data.frame(FuelType = integer(0), Ecoregion = character(0)),
      DisturbanceConversionTable = NULL,
      MapFileNames = NULL,
      PctConiferMapName = NULL,
      PctDeadFirMapName = NULL
    ) {
      stopifnot(!is.null(path))

      ## LandisExtension fields
      private$.LandisData <- "Dynamic Fuels"
      self$Timestep <- Timestep

      self$name <- "Dynamic Fuels"
      self$type <- "disturbance"
      self$path <- path
      self$files <- "dynamic-fuels.txt" ## file won't exist yet

      ## additional fields for this extension
      self$SpeciesFuelCoefficients <- SpeciesFuelCoefficients
      self$HardwoodMaximum <- HardwoodMaximum
      self$DeadFirMaxAge <- DeadFirMaxAge
      self$FuelTypes <- FuelTypes
      self$EcoregionTable <- EcoregionTable
      self$DisturbanceConversionTable <- DisturbanceConversionTable
      self$MapFileNames <- MapFileNames %||% MapNames("FuelType", "fire", self$path)
      self$PctConiferMapName <- PctConiferMapName %||% MapNames("PctConifer", "fire", self$path)
      self$PctDeadFirMapName <- PctDeadFirMapName %||% MapNames("PctDeadFir", "fire", self$path)
    },

    #' @description Write extension inputs to disk
    write = function() {
      stopifnot(!is.null(self$FireReductionParameters))

      writeLines(
        c(
          insertLandisData(private$.LandisData),
          insertValue("Timestep", self$Timestep),
          insertSpeciesFuelCoefficients(self$SpeciesFuelCoefficients),
          insertValue("HardwoodMaximum", self$HardwoodMaximum),
          insertDeadFirMaxAge(self$DeadFirMaxAge),
          insertFuelTypesTable(self$FuelTypes),
          insertEcoregionTable(self$EcoregionTable),
          insertDisturbanceConversionTable(self$DisturbanceConversionTable),
          insertFile("MapFileNames", self$MapFileNames),
          insertFile("PctConiferMapName", self$PctConiferMapName),
          insertFile("PctDeadFirMapName", self$PctDeadFirMapName)
        ),
        file.path(self$path, self$files[1])
      )
    }
  ),
  private = list(
    .SpeciesFuelCoefficients = NULL,
    .HardwoodMaximum = NULL,
    .DeadFirMaxAge = NULL,
    .FuelTypes = NULL,
    .EcoregionTable = NULL,
    .DisturbanceConversionTable = NULL,
    .MapFileNames = NULL,
    .PctConiferMapName = NULL,
    .PctDeadFirMapName = NULL
  ),
  active = list(
    #' @field SpeciesFuelCoefficients `data.frame`.
    SpeciesFuelCoefficients = function(value) {
      if (missing(value)) {
        return(private$.SpeciesFuelCoefficients)
      } else {
        stopifnot(is.data.frame(value))
        private$.SpeciesFuelCoefficients <- value
      }
    },

    #' @field HardwoodMaximum Integer.
    HardwoodMaximum = function(value) {
      if (missing(value)) {
        return(private$.HardwoodMaximum)
      } else {
        value <- as.integer(value)
        stopifnot(dplyr::between(value, 0L, 100L))

        private$.HardwoodMaximum <- value
      }
    },

    #' @field DeadFirMaxAge Integer. The duration of influence for the BDA extension's dead conifer index.
    DeadFirMaxAge = function(value) {
      if (missing(value)) {
        return(private$.DeadFirMaxAge)
      } else {
        stopifnot(value >= 0L)
        private$.DeadFirMaxAge <- value
      }
    },

    #' @field FuelTypes `data.frame`. See [prepFuelTypesTable()].
    FuelTypes = function(value) {
      if (missing(value)) {
        return(private$.FuelTypes)
      } else {
        stopifnot(
          is.data.frame(value),
          ncol(value) == 5,
          identical(colnames(value), c("FuelType", "BaseFuel", "AgeMin", "AgeMax", "Species"))
        )

        private$.FuelTypes <- value
      }
    },

    #' @field EcoregionTable `data.frame`.
    EcoregionTable = function(value) {
      if (missing(value)) {
        return(private$.EcoregionTable)
      } else {
        stopifnot(is.data.frame(value))
        private$.EcoregionTable <- value
      }
    },

    #' @field DisturbanceConversionTable `data.frame`.
    DisturbanceConversionTable = function(value) {
      if (missing(value)) {
        return(private$.DisturbanceConversionTable)
      } else {
        stopifnot(
          is.data.frame(value),
          ncol(df) == 3,
          identical(colnames(df), c("FuelType", "Duration", "Prescription"))
        )
        private$.DisturbanceConversionTable <- value
      }
    },

    #' @field MapFileNames Character. File pattern for writing outputs to disk.
    MapFileNames = function(value) {
      if (missing(value)) {
        return(private$.MapFileNames)
      } else {
        private$.MapFileNames <- value
      }
    },

    #' @field PctConiferMapName Character. File pattern for writing outputs to disk.
    PctConiferMapName = function(value) {
      if (missing(value)) {
        return(private$.PctConiferMapName)
      } else {
        private$.PctConiferMapName <- value
      }
    },

    #' @field PctDeadFirMapName Character. File pattern for writing outputs to disk.
    PctDeadFirMapName = function(value) {
      if (missing(value)) {
        return(private$.PctDeadFirMapName)
      } else {
        private$.PctDeadFirMapName <- value
      }
    }
  )
)

#' Specify Dynamic Fuel Extension's Species Fuel Coefficients
#'
#' @param df data.frame corresponding to Species Fuel Coefficients
#'
#' @template return_insert
#'
insertSpeciesFuelCoefficients <- function(df) {
  c(
    glue::glue(">> Species    Fuel"),
    glue::glue(">>            Coefficient"),
    glue::glue(">> -------    -----------"),
    apply(df, 1, function(x) {
      glue::glue_collapse(x, sep = "    ")
    }),
    glue::glue("") ## add blank line after each item group
  )
}

#' Create Dynamic Fire Extension's Fuel Types Table
#'
#' Assign fuel types to one of the Canadian Fire Behavior Prediction System fuel types.
#'
#' @returns data.frame for use with [insertFuelTypesTable()]
#'
#' @references
#' Natural Resources Canada. 2019. Fire Behaviour Prediction System Fuel Type Dsecriptions.
#'   <https://cwfis.cfs.nrcan.gc.ca/background/fueltypes/c1>
#'
#' @export
prepFuelTypesTable <- function() {
  browser() ## TODO
  df <- data.frame(
    FuelType = integer(0),
    BaseFuel = character(0),
    AgeMin = integer(0),
    AgeMax = integer(0),
    Species = character(0)
  )

  return(df)
}

#' Specify Dynamic Fuel Extension's Fuel Types Table
#'
#' @param df data.frame corresponding to `FuelTypesTable`, with columns:
#'   `FuelType` (int), `BaseFuel` (char), `AgeMin` (int), `AgeMax` (int), `Species` (char).
#'
#' @template return_insert
#'
insertFuelTypesTable <- function(df) {
  df <- df |>
    dplyr::mutate(AgeRange = glue::glue("{AgeMin} to {AgeMax}")) |>
    dplyr::mutate(AgeMin = NULL, AgeMax = NULL)

  c(
    glue::glue("FuelTypes"),
    glue::glue(">> Fuel Type    Base Fuel    Age Range    Species"),
    glue::glue(">> ---------    ---------    ---------    ----------------"),
    apply(df, 1, function(x) {
      glue::glue_collapse(x, sep = "    ")
    }),
    glue::glue("") ## add blank line after each item group
  )
}

#' Specify Dynamic Fuel Extension's Ecoregion Table
#'
#' @param df data.frame corresponding to `EcoregionTable`, with columns:
#'   `FuelType` (int) and `Ecoregion` (char).
#'
#' @template return_insert
#'
insertEcoregionTable <- function(df) {
  c(
    glue::glue("EcoregionTable"),
    glue::glue(">> Fuel Type    Ecoregion"),
    glue::glue(">> ---------    ---------"),
    apply(df, 1, function(x) {
      glue::glue_collapse(x, sep = "    ")
    }),
    glue::glue("") ## add blank line after each item group
  )
}

#' Create Dynamic Fire Extension's Disturbance Conversion Table
#'
#' @returns data.frame for use with [insertDisturbanceConversionTable()]
#'
#' @export
prepDisturbanceConversionTable <- function() {
  browser() ## TODO
  df <- data.frame(FuelType = integer(0), Duration = integer(0), Prescription = character(0))

  return(df)
}

#' Specify Dynamic Fuel Extension's Disturbance Conversion Table
#'
#' @param df data.frame corresponding to `DisturbanceConversionTable`, with columns:
#'   `FuelType` (int), `Duration` (int), and `Prescription` (char).
#'
#' @template return_insert
#'
insertDisturbanceConversionTable <- function(df) {
  c(
    glue::glue("DisturbanceConversionTable"),
    glue::glue(">> Fuel Type    Duration    Prescription"),
    glue::glue(">> ---------    --------    ------------"),
    apply(df, 1, function(x) {
      glue::glue_collapse(x, sep = "    ")
    }),
    glue::glue("") ## add blank line after each item group
  )
}
