#' Dynamic Fuel System Extension
#'
#' @include ext_utils.R
#'
#' @references LANDIS-II Dynamic Fuel System Extension v4.0 User Guide
#'   <https://github.com/LANDIS-II-Foundation/Extension-Dynamic-Biomass-Fuels/blob/master/docs/LANDIS-II%20Dynamic%20Fuel%20System%20v4.0%20User%20Guide.pdf>
#'
#' @seealso
#' Helpers that prepare inputs for this extension:
#' [prepFuelTypesTable()],
#' [prepDisturbanceConversionTable()].
#'
#' @family Dynamic Fuels helpers
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
    #' @param PctConiferFileName Character. File pattern for writing outputs to disk.
    #' @param PctDeadFirFileName Character. File pattern for writing outputs to disk.
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
      PctConiferFileName = NULL,
      PctDeadFirFileName = NULL
    ) {
      stopifnot(!is.null(path))

      ## LandisExtension fields
      ## Upstream Core8 reference (Extension-Dynamic-Biomass-Fuels @ master,
      ## testings/Core8-DynamicFuels4.0/dynamic-fire_SetUpFuel.txt) and the
      ## v8-release parser register this extension as "Dynamic Fuel System",
      ## not "Dynamic Fuels".
      private$.LandisData <- "Dynamic Fuel System"
      self$Timestep <- Timestep

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
      self$PctConiferFileName <- PctConiferFileName %||% MapNames("PctConifer", "fire", self$path)
      self$PctDeadFirFileName <- PctDeadFirFileName %||% MapNames("PctDeadFir", "fire", self$path)
    },

    #' @description Write extension inputs to disk
    write = function() {
      writeLines(
        c(
          insertLandisData(private$.LandisData),
          insertValue("Timestep", self$Timestep),
          insertSpeciesFuelCoefficients(self$SpeciesFuelCoefficients),
          insertValue("HardwoodMaximum", self$HardwoodMaximum),
          insertValue("DeadFirMaxAge", self$DeadFirMaxAge),
          insertFuelTypesTable(self$FuelTypes),
          insertEcoregionTable(self$EcoregionTable),
          insertDisturbanceConversionTable(self$DisturbanceConversionTable),
          insertFile("MapFileNames", self$MapFileNames),
          insertFile("PctConiferFileName", self$PctConiferFileName),
          insertFile("PctDeadFirFileName", self$PctDeadFirFileName)
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
    .PctConiferFileName = NULL,
    .PctDeadFirFileName = NULL
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
          ncol(value) == 4,
          identical(colnames(value), c("Fuel", "Type", "Duration", "Prescription"))
        )
        private$.DisturbanceConversionTable <- value
      }
    },

    #' @field MapFileNames Character. File pattern for writing outputs to disk;
    #'   must contain the literal `{timestep}` placeholder (the only variable
    #'   the upstream Dynamic Fuels `MapNames` parser knows -- see `MapFileNames.cs`).
    MapFileNames = function(value) {
      if (missing(value)) {
        return(private$.MapFileNames)
      } else {
        stopifnot(
          "MapFileNames must contain the literal `{timestep}` placeholder." =
            is.character(value) && grepl("{timestep}", value, fixed = TRUE)
        )
        private$.MapFileNames <- value
      }
    },

    #' @field PctConiferFileName Character. File pattern for writing outputs to disk;
    #'   must contain the literal `{timestep}` placeholder.
    PctConiferFileName = function(value) {
      if (missing(value)) {
        return(private$.PctConiferFileName)
      } else {
        stopifnot(
          "PctConiferFileName must contain the literal `{timestep}` placeholder." =
            is.character(value) && grepl("{timestep}", value, fixed = TRUE)
        )
        private$.PctConiferFileName <- value
      }
    },

    #' @field PctDeadFirFileName Character. File pattern for writing outputs to disk;
    #'   must contain the literal `{timestep}` placeholder.
    PctDeadFirFileName = function(value) {
      if (missing(value)) {
        return(private$.PctDeadFirFileName)
      } else {
        stopifnot(
          "PctDeadFirFileName must contain the literal `{timestep}` placeholder." =
            is.character(value) && grepl("{timestep}", value, fixed = TRUE)
        )
        private$.PctDeadFirFileName <- value
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
#' @family Dynamic Fuels helpers
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
#' Natural Resources Canada. 2019. Fire Behavior Prediction System Fuel Type Descriptions.
#'   <https://cwfis.cfs.nrcan.gc.ca/background/fueltypes/c1>
#'
#' @family Dynamic Fuels helpers
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
#' @family Dynamic Fuels helpers
#'
insertFuelTypesTable <- function(df) {
  ## Iterate row-wise rather than via apply(): the Species column is a
  ## list-column whose entries are character vectors, and apply() coerces
  ## them via `as.character(list(...))`, producing literal "list(\"sp\")"
  ## tokens that the LANDIS-II parser rejects.
  rows <- vapply(seq_len(nrow(df)), function(i) {
    paste(
      df$FuelType[i],
      df$BaseFuel[i],
      glue::glue("{df$AgeMin[i]} to {df$AgeMax[i]}"),
      glue::glue_collapse(unlist(df$Species[i]), sep = "  "),
      sep = "    "
    )
  }, character(1))

  c(
    glue::glue("FuelTypes"),
    glue::glue(">> Fuel Type    Base Fuel    Age Range    Species"),
    glue::glue(">> ---------    ---------    ---------    ----------------"),
    rows,
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
#' @family Dynamic Fuels helpers
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
#' @family Dynamic Fuels helpers
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
#'   `Fuel` (int), `Type` (int), `Duration` (int), and `Prescription` (char).
#'
#' @template return_insert
#'
#' @family Dynamic Fuels helpers
#'
insertDisturbanceConversionTable <- function(df) {
  c(
    glue::glue("DisturbanceConversionTable"),
    glue::glue(">> Fuel  Type    Duration    Prescription"),
    glue::glue(">> ----  ---    --------    ------------"),
    apply(df, 1, function(x) {
      glue::glue_collapse(x, sep = "    ")
    }),
    glue::glue("") ## add blank line after each item group
  )
}
