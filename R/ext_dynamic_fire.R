#' Dynamic Fire System Extension
#'
#' @include ext_utils.R
#'
#' @references LANDIS-II Dynamic Fire System Extension v4 User Guide
#'   <https://github.com/LANDIS-II-Foundation/Extension-Dynamic-Fire-System/blob/master/docs/LANDIS-II%20Dynamic%20Fire%20System%20v4%20User%20Guide.pdf>
#'
#' @export
DynamicFire <- R6Class(
  "DynamicFire",
  inherit = LandisExtension,
  public = list(
    #' @param path Character. Directory path.
    #' @param Timestep Integer.
    #' @param EventSizeType Character. One of "size_based" or "duration_based".
    #' @param BuildUpIndex Logical, or character indicating "yes" or "no".
    #' @param WeatherRandomizer Integer `[0, 4]`.
    #' @param FireSizesTable `data.frame`.
    #' @param InitialFireEcoregionsMap Character. Relative file path.
    #' @param DynamicEcoregionTable `data.frame`.
    #' @param GroundSlopeFile Character. Relative file path.
    #' @param UphillSlopeAzimuthMap Character. Relative file path.
    #' @param SeasonTable `data.frame`.
    #' @param InitialWeatherDatabase Character. Relative file path.
    #' @param DynamicWeatherTable `data.frame`.
    #' @param FuelTypeTable `data.frame`.
    #' @param SeverityCalibrationFactor `data.frame`.
    #' @param FireDamageTable `data.frame`.
    #' @param MapNames Character. File pattern for writing outputs to disk.
    #' @param LogFile Character. Relative file path.
    #' @param SummaryLogFile Character. Relative file path.
    initialize = function(
      path,
      Timestep = 10,
      EventSizeType = NULL,
      BuildUpIndex = NULL,
      WeatherRandomizer = NULL,
      FireSizesTable = NULL,
      InitialFireEcoregionsMap = NULL,
      DynamicEcoregionTable = NULL,
      GroundSlopeFile = NULL,
      UphillSlopeAzimuthMap = NULL,
      SeasonTable = NULL,
      InitialWeatherDatabase = NULL,
      DynamicWeatherTable = NULL,
      FuelTypeTable = NULL,
      SeverityCalibrationFactor = NULL,
      FireDamageTable = NULL,
      MapNames = NULL,
      LogFile = "fire/log.csv",
      SummaryLogFile = "fire/summary-log.csv"
    ) {
      stopifnot(!is.null(path))

      ## LandisExtension fields
      private$LandisData <- "Dynamic Fire System"
      self$Timestep <- Timestep

      self$type <- "disturbance"
      self$path <- path
      self$files <- "dynamic-fire.txt" ## file won't exist yet

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
        !is.null(self$BuildUpIndex),
        !is.null(self$DynamicWeatherTable),
        !is.null(self$FireDamageTable),
        !is.null(self$FireSizesTable),
        !is.null(self$FuelTypeTable),
        !is.null(self$InitialFireEcoregionsMap),
        !is.null(self$InitialWeatherDatabase),
        !is.null(self$LogFile),
        !is.null(self$SeasonTable),
        !is.null(self$SeverityCalibrationFactor),
        !is.null(self$SummaryLogFile)
      )

      writeLines(
        c(
          insertLandisData(private$.LandisData),
          insertValue("Timestep", self$Timestep),
          insertValue("EventSizeType", self$EventSizeType),
          insertBuildUpIndex(self$BuildUpIndex),
          insertValue("WeatherRandomizer", self$WeatherRandomizer),
          insertFireSizesTable(self$FireSizesTable),
          insertFile("InitialFireEcoregionsMap", self$InitialFireEcoregionsMap),
          insertDynamicTable("DynamicEcoregionTable", self$DynamicEcoregionTable),
          insertFile("GroundSlopeFile", self$GroundSlopeFile),
          insertFile("UphillSlopeAzimuthMap", self$UphillSlopeAzimuthMap),
          insertSeasonTable(self$SeasonTable),
          insertFile("InitialWeatherDatabase", self$InitialWeatherDatabase),
          insertDynamicTable("DynamicWeatherTable", self$DynamicWeatherTable),
          insertFuelTypeTable(self$FuelTypeTable),
          insertValue("SeverityCalibrationFactor", self$SeverityCalibrationFactor),
          insertFireDamageTable(self$FireDamageTable),
          insertFile("MapNames", self$MapNames),
          insertFile("LogFile", self$LogFile),
          insertFile("SummaryLogFile", self$SummaryLogFile)
        ),
        file.path(self$path, self$files[1])
      )

      self$add_file(self$GroundSlopeFile)
      self$add_file(self$InitialFireEcoregionsMap)
      if (!is.null(self$UphillSlopeAzimuthMap)) {
        self$add_file(self$UphillSlopeAzimuthMap)
      }

      return(invisible(self))
    }
  ),

  private = list(
    .EventSizeType = NULL,
    .BuildUpIndex = NULL,
    .WeatherRandomizer = NULL,
    .FireSizesTable = NULL,
    .InitialFireEcoregionsMap = NULL,
    .DynamicEcoregionTable = NULL,
    .GroundSlopeFile = NULL,
    .UphillSlopeAzimuthMap = NULL,
    .SeasonTable = NULL,
    .InitialWeatherDatabase = NULL,
    .DynamicWeatherTable = NULL,
    .FuelTypeTable = NULL,
    .SeverityCalibrationFactor = NULL,
    .FireDamageTable = NULL,
    .MapNames = NULL,
    .LogFile = NULL,
    .SummaryLogFile = NULL
  ),

  active = list(
    #' @field EventSizeType Character. One of "size_based" or "duration_based".
    EventSizeType = function(value) {
      if (missing(value)) {
        return(private$.EventSizeType)
      } else {
        allowed <- c("size_based", "duration_based")
        value <- tolower(value)
        value <- value %||% allowed[1]

        stopifnot(value %in% allowed)

        private$.EventSizeType <- value
      }
    },

    #' @field BuildUpIndex Logical, or character indicating "yes" or "no".
    BuildUpIndex = function(value) {
      if (missing(value)) {
        return(private$.BuildUpIndex)
      } else {
        private$.BuildUpIndex <- yesno(value)
      }
    },

    #' @field WeatherRandomizer  (Optional) Integer `[0,4]` controlling the strength of the
    #' linkage between fire size/duration distribution and weather distribution.
    WeatherRandomizer = function(value) {
      if (missing(value)) {
        return(private$.WeatherRandomizer)
      } else {
        value <- as.integer(value)
        stopifnot(dplyr::between(value, 0L, 4L))

        private$.WeatherRandomizer <- value
      }
    },

    #' @field FireSizesTable `data.frame`.
    FireSizesTable = function(value) {
      if (missing(value)) {
        return(private$.FireSizesTable)
      } else {
        private$.FireSizesTable <- value
      }
    },

    #' @field InitialFireEcoregionsMap Character. Relative file path.
    InitialFireEcoregionsMap = function(value) {
      if (missing(value)) {
        return(private$.InitialFireEcoregionsMap)
      } else {
        private$.InitialFireEcoregionsMap <- .relPath(value, self$path)
      }
    },

    #' @field DynamicEcoregionTable `data.frame`.
    DynamicEcoregionTable = function(value) {
      if (missing(value)) {
        return(private$.DynamicEcoregionTable)
      } else {
        private$.DynamicEcoregionTable <- value
      }
    },

    #' @field GroundSlopeFile Character. Relative file path.
    GroundSlopeFile = function(value) {
      if (missing(value)) {
        return(private$.GroundSlopeFile)
      } else {
        private$.GroundSlopeFile <- .relPath(value, self$path)
      }
    },

    #' @field UphillSlopeAzimuthMap Character. Relative file path.
    UphillSlopeAzimuthMap = function(value) {
      if (missing(value)) {
        return(private$.UphillSlopeAzimuthMap)
      } else {
        private$.UphillSlopeAzimuthMap <- .relPath(value, self$path)
      }
    },

    #' @field SeasonTable `data.frame`.
    SeasonTable = function(value) {
      if (missing(value)) {
        return(private$.SeasonTable)
      } else {
        private$.SeasonTable <- value
      }
    },

    #' @field InitialWeatherDatabase Character. Relative file path.
    InitialWeatherDatabase = function(value) {
      if (missing(value)) {
        return(private$.InitialWeatherDatabase)
      } else {
        private$.InitialWeatherDatabase <- .relPath(value, self$path)
      }
    },

    #' @field DynamicWeatherTable `data.frame`.
    DynamicWeatherTable = function(value) {
      if (missing(value)) {
        return(private$.DynamicWeatherTable)
      } else {
        private$.DynamicWeatherTable <- value
      }
    },

    #' @field FuelTypeTable `data.frame`.
    FuelTypeTable = function(value) {
      if (missing(value)) {
        return(private$.FuelTypeTable)
      } else {
        private$.FuelTypeTable <- value
      }
    },

    #' @field SeverityCalibrationFactor Numeric.
    SeverityCalibrationFactor = function(value) {
      if (missing(value)) {
        return(private$.SeverityCalibrationFactor)
      } else {
        stopifnot(is.numeric(value), value >= 0)
        private$.SeverityCalibrationFactor <- value
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

#' Specify Dynamic Fire Extension `BuildUpIndex`
#'
#' @param bui  Logical, indicating whether to turn os Build Up Index.
#'
#' @template return_insert
#'
#' @export
insertBuildUpIndex <- function(bui = FALSE) {
  if (isTRUE(bui)) {
    c(
      glue::glue("BuildUpIndex    yes"),
      glue::glue("") ## add blank line after each item group
    )
  } else {
    c(
      glue::glue("BuildUpIndex    no"),
      glue::glue("") ## add blank line after each item group
    )
  }
}


#' Prepare Dynamic Fire Extension `FireSizes` Table
#'
#' @param df data.frame corresponding to `FireSizes` table
#'
#' @returns data.frame
#'
#' @export
prepFireSizesTable <- function(df = NULL) {
  browser() ## TODO: build this; enforce colnames and types / ranges
  data.frame(
    EcoCode = integer(0), ## 0 <= EcoCode < 65535;
    EcoName = character(0),
    Mu = numeric(0), ## Mu >= 0
    Sigma = numeric(0), ## Sigma >= 0
    Max = numeric(0), ## Max >= 0
    SpFMCLo = integer(0), ## percent
    SpFMCHi = integer(0), ## percent
    SpHiProp = numeric(0), ## proportion
    SumFMCLo = integer(0), ## percent
    SumFMCHi = integer(0), ## percent
    SumHiProp = numeric(0), ## proportion
    FallFMCLo = integer(0), ## percent
    FallFMCHi = integer(0), ## percent
    FallHiProp = numeric(0), ## proportion
    OpenFuelIndex = integer(0), ## 1 <= OpenFuelIndex <= N_fuel_types
    NumFires = numeric(0), ## NumFires >= 0
  )
}

#' Specify Fire Size Table
#'
#' @param df data.frame corresponding to Fire Sizes Table
#'
#' @template return_insert
#'
#' @export
insertFireSizesTable <- function(df = NULL) {
  c(
    glue::glue(">> Fire Sizes (parameters applied for both size and duration based)"),
    glue::glue(">> "),
    glue::glue(">> {glue::glue_collapse(colnames(df), sep = '    ')}"),
    glue::glue(">> ----------------------------------------------------------------"),
    apply(df, 1, function(x) {
      glue::glue_collapse(x, sep = "    ")
    }),
    glue::glue("") ## add blank line after each item group
  )
}

#' Prepare Dynamic Fire Extension `DynamicFireEcoregionTable`
#'
#' @param year Integer vector of simulation years
#'
#' @param filename Character vector of filenames corresponding to each of `year`.
#'
#' @returns data.frame
#'
#' @export
prepFireSizesTable <- function(year, filename) {
  stopifnot(length(year) == length(filename))

  data.frame(Year = year, FileName = filename)
}

#' Specify Dynamic Fire Extension `GroundSlopeFile`
#'
#' @template param_file
#'
#' @template return_insert
#'
#' @export
insertGroundSlopeFile <- function(file) {
  insertFile("GroundSlopeFile", file)
}

#' Specify Dynamic Fire Extension `UphillSlopeAzimuthMap`
#'
#' @template param_file
#'
#' @template return_insert
#'
#' @export
insertUphillSlopeAzimuthMap <- function(file) {
  insertFile("UphillSlopeAzimuthMap", file)
}

#' Specify Dynamic Fire Extension's Season Table
#'
#' @param df data.frame corresponding to `SeasonTable`.
#'
#' @template return_insert
#'
#' @export
insertSeasonTable <- function(df) {
  stopifnot(
    identical(colnames(df), c("Name", "LeafStatus", "PropFire", "PercentCurling", "DayLengthProp")),
    nrow(df) == 3,
    identical(df[["Name"]], c("Spring", "Summer", "Fall")),
    identical(df[["LeafStatus"]], c("LeafOff", "LeafOn", "LeafOff")),
    all(dplyr::between(df[["PropFire"]], 0.0, 1.0)),
    all(dplyr::between(df[["PercentCurling"]], 0, 100)),
    all(dplyr::between(df[["DayLengthProp"]], 0.0, 1.0))
  )

  df[["PercentCurling"]] <- as.integer(df[["PercentCurling"]])

  c(
    glue::glue("SeasonTable"),
    glue::glue(">>           Leaf      Proportion    Percent    DayLength"),
    glue::glue(">> Name      Status    Fire          Curing     Proportion"),
    glue::glue(">> -----------------------------------------------"),
    apply(df, 1, function(x) {
      glue::glue_collapse(x, sep = "    ")
    }),
    glue::glue("") ## add blank line after each item group
  )
}

#' Create Dynamic Fire Extension's Initial Weather Database
#'
#' @param df data.frame
#'
#' @template param_path
#'
#' @template return_file
#'
#' @export
prepInitialWeatherDatabase <- function(df, path) {
  browser() ## TODO
  df

  file <- file.path(path, "initial-weather-database.csv")
  write.csv(df, file, row.names = FALSE)

  return(file)
}

#' Prepare Dynamic Fire Extension `DynamicWeatherTable`
#'
#' @param year Integer vector of simulation years.
#'
#' @param filename Character vector of filenames corresponding to each of `year`.
#'
#' @returns data.frame
#'
#' @export
prepDynamicWeatherTable <- function(year, filename) {
  stopifnot(length(year) == length(filename))

  data.frame(Year = year, FileName = filename)
}

#' Specify Dynamic Fire Extension `FuelTypeTable`
#'
#' @param df data.frame specifying the rate of spread parameters for each base surface type,
#'           following the Canadian Forest Fire Behavior Prediction System (Forestry Canada, 1992).
#'           Default values from Tables 6, 7, 8 and Appendix 1.
#'
#' @template return_insert
#'
#' @references
#' Forestry Canada. 1992. Development and structure of the Canadian Forest Fire Behavior Prediction System.
#'   Forestry Canada, Headquarters, Fire Danger Group and Science and Sustainable Development Directorate, Ottawa.
#'   Information Report ST-X-3. 64 p. <https://ostrnrcan-dostrncan.canada.ca/handle/1845/235421>
#'
#' Natural Resources Canada. 2019. Fire Behaviour Prediction System Fuel Type Dsecriptions.
#'   <https://cwfis.cfs.nrcan.gc.ca/background/fueltypes/c1>
#'
#' Wotton, B.M.; Alexander, M.E.; Taylor, S.W. 2009. Updates and revisions to the 1992 Canadian Forest
#'   Fire Behavior Prediction System. Natural Resources Canada, Canadian Forest Service,
#'   Great Lakes Forestry Centre, Sault Ste. Marie, Ontario. Information Report GLC-X-10. 45 p.
#'   <https://ostrnrcan-dostrncan.canada.ca/handle/1845/247839>
#'
#' @export
insertFuelTypeTable <- function(df = NULL) {
  if (is.null(df)) {
    ## NOTE: default table derived from Canadian Forest Fire Behavior Prediction System (1992):
    ## - Table 6 (p26): rate of spread parameters (a, b, c) except for mixedwood (M classes);
    ## - Table 7 (p34): additional rate of spread parameters (BUI, q, maxBE);
    ## - Table 8 (p35): crown base height (CBH) for fuel types subject to crowning;
    ## - Appendix 1 (p60): rate of spread parameters (a, b, c, q, BUI, CBH);
    ##
    ## NOTE: likely typo in LANDIS-II sample table for M1 CBH (used 0, but CFFBPS report uses 6);

    df <- tibble::tribble(
      ~Index , ~Base               , ~Surface , ~IgnProb , ~a  , ~b     , ~c  , ~q   , ~BUI , ~maxBE , ~CBH ,
           1 , "Conifer"           , "C1"     , 1.0      ,  90 , 0.0649 , 4.5 , 0.90 ,   72 , 1.076  ,    2 ,
           2 , "Conifer"           , "C2"     , 1.0      , 110 , 0.0282 , 1.5 , 0.70 ,   64 , 1.321  ,    3 ,
           3 , "Conifer"           , "C3"     , 1.0      , 110 , 0.0444 , 3.0 , 0.75 ,   62 , 1.261  ,    8 ,
           4 , "Conifer"           , "C4"     , 1.0      , 110 , 0.0293 , 1.5 , 0.80 ,   66 , 1.184  ,    4 ,
           5 , "Conifer"           , "C5"     , 1.0      ,  30 , 0.0697 , 4.0 , 0.80 ,   56 , 1.220  ,   18 ,
           6 , "ConiferPlantation" , "C6"     , 1.0      ,  30 , 0.0800 , 3.0 , 0.80 ,   62 , 1.197  ,    7 ,
           7 , "Conifer"           , "C7"     , 1.0      ,  45 , 0.0305 , 2.0 , 0.85 ,  106 , 1.134  ,   10 ,
           8 , "Deciduous"         , "D1"     , 0.5      ,  30 , 0.0232 , 1.6 , 0.90 ,   32 , 1.179  ,    0 ,
           9 , "Conifer"           , "M1"     , 1.0      ,   0 , 0.0000 , 0.0 , 0.80 ,   50 , 1.250  ,    6 ,
          10 , "Conifer"           , "M2"     , 1.0      ,   0 , 0.0000 , 0.0 , 0.80 ,   50 , 1.250  ,    6 ,
          11 , "Conifer"           , "M3"     , 1.0      ,   0 , 0.0000 , 0.0 , 0.80 ,   50 , 1.250  ,    6 ,
          12 , "Conifer"           , "M4"     , 1.0      ,   0 , 0.0000 , 0.0 , 0.80 ,   50 , 1.250  ,    6 ,
          13 , "Slash"             , "S1"     , 1.0      ,  75 , 0.0297 , 1.3 , 0.75 ,   38 , 1.460  ,    0 ,
          14 , "Slash"             , "S2"     , 1.0      ,  40 , 0.0438 , 1.7 , 0.75 ,   63 , 1.256  ,    0 ,
          15 , "Slash"             , "S3"     , 1.0      ,  55 , 0.0829 , 3.2 , 0.75 ,   31 , 1.590  ,    0 ,
          16 , "Open"              , "O1a"    , 1.0      , 190 , 0.0310 , 1.4 , 1.00 ,    1 , 1.000  ,    0 ,
          17 , "Open"              , "O1b"    , 1.0      , 250 , 0.0350 , 1.7 , 1.00 ,    1 , 1.000  ,    0
    ) |>
      as.data.frame()
  }

  stopifnot(ncol(df) == 11)

  c(
    glue::glue("FuelTypeTable"),
    glue::glue(">> Allowed base types:     Conifer, ConiferPlantation, Deciduous, Slash, Open."),
    glue::glue(">> Allowed surface types:  See Canadian Fire Behavior System (CFBS)."),
    glue::glue(">> {glue::glue_collapse(colnames(df), sep = '    ')}"),
    glue::glue(">>          Type        Type   "),
    glue::glue(">> ----------------------------------------------------------------"),
    apply(df, 1, function(x) {
      glue::glue_collapse(x, sep = "    ")
    }),
    glue::glue("") ## add blank line after each item group
  )
}
