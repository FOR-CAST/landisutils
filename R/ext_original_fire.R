#' Original Fire Extension
#'
#' @include ext_utils.R
#'
#' @references LANDIS-II Original Fire v5.0 Extension User Guide
#' <https://github.com/LANDIS-II-Foundation/Extension-Base-Fire/blob/master/docs/LANDIS-II%20Original%20Fire%20v5.0%20User%20Guide.pdf>
#'
#' @export
OriginalFire <- R6Class(
  "OriginalFire",
  inherit = LandisExtension,
  public = list(
    #' @param path Character. Directory path.
    #' @param Timestep Integer.
    #' @param Species_CSV_File Character. Relative file path.
    #' @param FireRegionParametersTable `data.frame`.
    #' @param InitialFireRegionsMap `SpatRaster`.
    #' @param DynamicFireRegionsTable `data.frame`.
    #' @param FuelCurveTable `data.frame`.
    #' @param WindCurveTable `data.frame`.
    #' @param FireDamageTable `data.frame`.
    #' @param MapNames Character. File pattern for writing outputs to disk.
    #' @param LogFile Character. Relative file path.
    #' @param SummaryLogFile Character. Relative file path.
    initialize = function(
      path,
      Timestep = NULL,
      Species_CSV_File = NULL,
      FireRegionParametersTable = NULL,
      InitialFireRegionsMap = NULL,
      DynamicFireRegionsTable = NULL,
      FuelCurveTable = NULL,
      WindCurveTable = NULL,
      FireDamageTable = NULL,
      MapNames = NULL,
      LogFile = "fire/log.csv",
      SummaryLogFile = "fire/summary-log.csv"
    ) {
      stopifnot(!is.null(path))

      ## LandisExtension fields
      private$.LandisData <- "Original Fire"
      self$Timestep <- Timestep

      self$type <- "disturbance"
      self$path <- path
      self$files <- "original-fire.txt" ## file won't exist yet

      ## additional fields for this extension
      self$Species_CSV_File <- Species_CSV_File
      self$FireRegionParametersTable <- FireRegionParametersTable
      self$InitialFireRegionsMap <- InitialFireRegionsMap
      self$DynamicFireRegionsTable <- DynamicFireRegionsTable
      self$FuelCurveTable <- FuelCurveTable
      self$WindCurveTable <- WindCurveTable
      self$FireDamageTable <- FireDamageTable
      self$MapNames <- MapNames %||% MapNames("severity", "fire", self$path)
      self$LogFile <- LogFile
      self$SummaryLogFile <- SummaryLogFile
    },

    #' @description Write extension inputs to disk
    write = function() {
      stopifnot(
        !is.null(self$FireRegionParametersTable),
        !is.null(self$InitialFireRegionsMap),
        !is.null(self$LogFile),
        !is.null(self$Species_CSV_File),
        !is.null(self$SummaryLogFile)
      )

      writeLines(
        c(
          insertLandisData(private$.LandisData),
          insertValue("Timestep", self$Timestep),
          insertFile("Species_CSV_File", self$Species_CSV_File),
          insertFireRegionParametersTable(self$FireRegionParametersTable),
          insertFile("InitialFireRegionsMap", self$InitialFireRegionsMap),
          insertDynamicTable("DynamicFireRegionsTable", self$DynamicFireRegionsTable),
          insertFuelCurveTable(self$FuelCurveTable),
          insertWindCurveTable(self$WindCurveTable),
          insertFireDamageTable(self$FireDamageTable),
          insertFile("MapNames", self$MapNames),
          insertFile("LogFile", self$LogFile),
          insertFile("SummaryLogFile", self$SummaryLogFile)
        ),
        file.path(self$path, self$files[1])
      )

      self$add_file(self$InitialFireRegionsMap)
      self$add_file(self$Species_CSV_File)

      return(invisible(self))
    }
  ),

  private = list(
    .Species_CSV_File = NULL,
    .FireRegionParametersTable = NULL,
    .InitialFireRegionsMap = NULL,
    .DynamicFireRegionsTable = NULL,
    .FuelCurveTable = NULL,
    .WindCurveTable = NULL,
    .FireDamageTable = NULL,
    .MapNames = NULL,
    .LogFile = NULL,
    .SummaryLogFile = NULL
  ),

  active = list(
    #' @field Species_CSV_File Character. Relative file path.
    Species_CSV_File = function(value) {
      if (missing(value)) {
        return(private$.Species_CSV_File)
      } else {
        private$.Species_CSV_File <- .relPath(value, self$path)
      }
    },

    #' @field FireRegionParametersTable `data.frame`.
    FireRegionParametersTable = function(value) {
      if (missing(value)) {
        return(private$.FireRegionParametersTable)
      } else {
        private$.FireRegionParametersTable <- value
      }
    },

    #' @field InitialFireRegionsMap Character. Relative file path.
    InitialFireRegionsMap = function(value) {
      if (missing(value)) {
        return(private$.InitialFireRegionsMap)
      } else {
        private$.InitialFireRegionsMap <- .relPath(value, self$path)
      }
    },

    #' @field DynamicFireRegionsTable `data.frame`.
    DynamicFireRegionsTable = function(value) {
      if (missing(value)) {
        return(private$.DynamicFireRegionsTable)
      } else {
        private$.DynamicFireRegionsTable <- value
      }
    },

    #' @field FuelCurveTable `data.frame`.
    FuelCurveTable = function(value) {
      if (missing(value)) {
        return(private$.FuelCurveTable)
      } else {
        private$.FuelCurveTable <- value
      }
    },

    #' @field WindCurveTable `data.frame`.
    WindCurveTable = function(value) {
      if (missing(value)) {
        return(private$.WindCurveTable)
      } else {
        private$.WindCurveTable <- value
      }
    },

    #' @field FireDamageTable `data.frame`.
    FireDamageTable = function(value) {
      if (missing(value)) {
        return(private$.FireDamageTable)
      } else {
        private$.FireDamageTable <- value
      }
    },

    #' @field MapNames Character. File pattern for writing outputs to disk.
    MapNames = function(value) {
      if (missing(value)) {
        return(private$.MapNames)
      } else {
        private$.MapNames <- value
      }
    },

    #' @field LogFile Character. Relative file path.
    LogFile = function(value) {
      if (missing(value)) {
        return(private$.LogFile)
      } else {
        private$.LogFile <- .relPath(value, self$path)
      }
    },

    #' @field SummaryLogFile Character. Relative file path.
    SummaryLogFile = function(value) {
      if (missing(value)) {
        return(private$.SummaryLogFile)
      } else {
        private$.SummaryLogFile <- .relPath(value, self$path)
      }
    }
  )
)

#' Prepare Original Fire Extension `FireRegionParameters` Table
#'
#' @param sf `sf` polygon object
#'
#' @returns data.frame
#'
#' @export
prepFireRegionParametersTable <- function(sf) {
  sf::st_drop_geometry(sf) |>
    dplyr::mutate(
      FireRegionName = glue::glue("FRT_{FRT}"), ## TODO: use 1st col, not hardcoded 'FRT'
      MapCode = PolyID,
      MeanSize = xBar * cellSize,
      MinSize = cellSize, ## always one pixel?
      MaxSize = emfs_ha,
      IgnitionProb = pIgnition,
      k = round(1.0 / (empiricalBurnRate))
    ) |>
    dplyr::select(FireRegionName, MapCode, MeanSize, MinSize, MaxSize, IgnitionProb, k) |>
    dplyr::bind_rows(
      ## must include values for mapcode 0 (i.e., the NAs)
      data.frame(
        FireRegionName = "FRT_0", ## TODO: use 1st col, not hardcoded 'FRT'
        MapCode = 0,
        MeanSize = 0,
        MinSize = 0, ## always one pixel?
        MaxSize = 0,
        IgnitionProb = 0,
        k = 0
      )
    )
}

#' Specify Fire Region Parameters Table
#'
#' @param df data.frame corresponding to Fire Region Parameters Table
#'
#' @template return_insert
#'
insertFireRegionParametersTable <- function(df) {
  c(
    glue::glue(">> Fire Region Parameters"),
    glue::glue(">> "),
    glue::glue(">> Region  Map    Mean  Min   Max   Ignition  Fire"),
    glue::glue(">> Name    Code   Size  Size  Size  Prob      k"),
    glue::glue(">> -----------------------------------------------"),
    apply(df, 1, function(x) {
      glue::glue_collapse(x, sep = "    ")
    }),
    glue::glue("") ## add blank line after each item group
  )
}

#' Create Original Fire `InitialFireRegionsMap`
#'
#' @param r `SpatRaster` corresponding to initial fire regions map
#'
#' @template param_file
#'
#' @template return_file
#'
#' @export
prepInitialFireRegionsMap <- function(r, file = "fire-regions-map.tif") {
  terra::writeRaster(
    r,
    file,
    overwrite = TRUE,
    # datatype = "INT2U", ## corresponds best to 65535 values; but LANDIS doesn't like it?
    datatype = "INT2S", ## this works, but limits mapcodes to 32767
    NAflag = 0L
  )

  return(file)
}

#' Specify Original Fire `FuelCurveTable`
#'
#' @param df data.frame
#'
#' @template return_insert
#'
insertFuelCurveTable <- function(df) {
  c(
    glue::glue("FuelCurveTable"),
    glue::glue(">> Fireregion    S1  S2  S3  S4  S5"),
    glue::glue(">> --------------------------------"),
    apply(df, 1, function(x) {
      glue::glue_collapse(x, sep = "    ")
    }),
    glue::glue("") ## add blank line after each item group
  )
}

#' Specify Original Fire `WindCurveTable`
#'
#' @param df data.frame or NULL
#'
#' @template return_insert
#'
insertWindCurveTable <- function(df) {
  c(
    glue::glue("WindCurveTable"),
    glue::glue(">> Ecoregion    S5  S4  S3  S2  S1"),
    glue::glue(">> -------------------------------"),
    if (!is.null(df)) {
      apply(df, 1, function(x) {
        glue::glue_collapse(x, sep = "  ")
      })
    },
    glue::glue("") ## add blank line after each item group
  )
}
