#' Landscape Habitat Output Extension
#'
#' @include ext_utils.R
#'
#' @references LANDIS-II Output Landscape Habitat v2 Extension User Guide
#'   <https://github.com/LANDIS-II-Foundation/Extension-Output-Landscape-Habitat/blob/master/docs/LANDIS-II%20Landscape%20Habitat%20Output%20v2%20User%20Guide.pdf>
#'
#' @family Landscape Habitat Output helpers
#'
#' @export
OutputLandscapeHabitat <- R6Class(
  "OutputLandscapeHabitat",
  inherit = LandisExtension,
  public = list(
    #' @param path Character. Directory path.
    #' @param Timestep Integer. Years between updates.
    #' @param LocalVariables (Optional) Named list mapping reclass map names
    #'   to data frames with columns `ForestType`, `AgeRange`, `Species`.
    #'   Each row defines one forest-type/age/species rule.
    #' @param DerivedLocalVariables (Optional) Named character vector. Names
    #'   are derived-variable names; values are arithmetic expressions
    #'   referencing the local variables (e.g.
    #'   `"reclass2[LowlandCon] + reclass2[LowlandHdwd]"`).
    #' @param NeighborhoodVariables (Optional) `data.frame` with columns
    #'   `Name`, `LocalVar`, `NeighborRadius`, `Transform`.
    #' @param ClimateVariables (Optional) `data.frame` with columns
    #'   `Name`, `Year`, `Months`, `Source`, `ClimateVar`, `Transform`.
    #' @param DistanceVariables (Optional) `data.frame` with columns
    #'   `Name`, `LocalVar`, `Transform`.
    #' @param SpeciesModels Named list. Each name is a species name; each
    #'   value is a `data.frame` with columns `Parameter`, `Type`, `Value`
    #'   describing the logistic-regression terms (use `"intercept"` for
    #'   the intercept row).
    #' @param LocalVarMapFileNames (Optional) Character. Output template for
    #'   local-variable maps; must contain the literal placeholders
    #'   `{local-var-name}` and `{timestep}`.
    #' @param NeighborVarMapFileNames (Optional) Character. Output template
    #'   for neighborhood maps; must contain the literal placeholders
    #'   `{neighbor-var-name}` and `{timestep}`.
    #' @param ClimateVarMapFileNames (Optional) Character. Output template
    #'   for climate maps; must contain the literal placeholders
    #'   `{climate-var-name}` and `{timestep}`.
    #' @param DistanceVarMapFileNames (Optional) Character. Output template
    #'   for distance maps; must contain the literal placeholders
    #'   `{distance-var-name}` and `{timestep}`.
    #' @param SpeciesMapFileNames (Optional) Character. Output template for
    #'   species-model maps; must contain the literal placeholders
    #'   `{species-name}` and `{timestep}`.
    #' @param SpeciesLogFileNames (Optional) Character. Output template for
    #'   species log files; must contain the literal placeholder
    #'   `{species-name}`.
    #' @param LogFile Character. Relative file path for the main log.
    initialize = function(
      path,
      Timestep = NULL,
      LocalVariables = NULL,
      DerivedLocalVariables = NULL,
      NeighborhoodVariables = NULL,
      ClimateVariables = NULL,
      DistanceVariables = NULL,
      SpeciesModels = NULL,
      LocalVarMapFileNames = NULL,
      NeighborVarMapFileNames = NULL,
      ClimateVarMapFileNames = NULL,
      DistanceVarMapFileNames = NULL,
      SpeciesMapFileNames = NULL,
      SpeciesLogFileNames = NULL,
      LogFile = "output/landscape-habitat/landscape_habitat_log.csv"
    ) {
      stopifnot(!is.null(path))

      ## LandisExtension fields
      private$.LandisData <- "Output Landscape Habitat"
      self$Timestep <- Timestep

      self$type <- "other"
      self$path <- path
      self$files <- "output-landscape-habitat.txt" ## file won't exist yet

      ## additional fields
      self$LocalVariables <- LocalVariables
      self$DerivedLocalVariables <- DerivedLocalVariables
      self$NeighborhoodVariables <- NeighborhoodVariables
      self$ClimateVariables <- ClimateVariables
      self$DistanceVariables <- DistanceVariables
      self$SpeciesModels <- SpeciesModels
      self$LocalVarMapFileNames <- LocalVarMapFileNames %||%
        "output/landscape-habitat/{local-var-name}-{timestep}.tif"
      self$NeighborVarMapFileNames <- NeighborVarMapFileNames %||%
        "output/landscape-habitat/{neighbor-var-name}-{timestep}.tif"
      self$ClimateVarMapFileNames <- ClimateVarMapFileNames %||%
        "output/landscape-habitat/{climate-var-name}-{timestep}.tif"
      self$DistanceVarMapFileNames <- DistanceVarMapFileNames
      self$SpeciesMapFileNames <- SpeciesMapFileNames %||%
        "output/landscape-habitat/habitat-{species-name}-{timestep}.tif"
      self$SpeciesLogFileNames <- SpeciesLogFileNames %||%
        "output/landscape-habitat/{species-name}_log.csv"
      self$LogFile <- LogFile
    },

    #' @description Write extension inputs to disk
    write = function() {
      stopifnot(
        !is.null(self$SpeciesModels),
        length(self$SpeciesModels) >= 1L,
        !is.null(self$LogFile)
      )

      writeLines(
        c(
          insertLandisData(private$.LandisData),
          insertValue("Timestep", self$Timestep),
          if (!is.null(self$LocalVariables)) {
            insertLandscapeHabitatLocalVariables(self$LocalVariables)
          },
          if (!is.null(self$DerivedLocalVariables)) {
            insertLandscapeHabitatDerivedLocalVariables(self$DerivedLocalVariables)
          },
          if (!is.null(self$NeighborhoodVariables)) {
            insertLandscapeHabitatNeighborhoodVariables(self$NeighborhoodVariables)
          },
          if (!is.null(self$ClimateVariables)) {
            insertLandscapeHabitatClimateVariables(self$ClimateVariables)
          },
          if (!is.null(self$DistanceVariables)) {
            insertLandscapeHabitatDistanceVariables(self$DistanceVariables)
          },
          insertLandscapeHabitatSpeciesModels(self$SpeciesModels),
          insertValue("LocalVarMapFileNames", self$LocalVarMapFileNames),
          insertValue("NeighborVarMapFileNames", self$NeighborVarMapFileNames),
          insertValue("ClimateVarMapFileNames", self$ClimateVarMapFileNames),
          if (!is.null(self$DistanceVarMapFileNames)) {
            insertValue("DistanceVarMapFileNames", self$DistanceVarMapFileNames)
          },
          insertValue("SpeciesMapFileNames", self$SpeciesMapFileNames),
          insertValue("SpeciesLogFileNames", self$SpeciesLogFileNames),
          insertValue("LogFile", self$LogFile)
        ),
        file.path(self$path, self$files[1])
      )

      return(invisible(self))
    }
  ),

  private = list(
    .LocalVariables = NULL,
    .DerivedLocalVariables = NULL,
    .NeighborhoodVariables = NULL,
    .ClimateVariables = NULL,
    .DistanceVariables = NULL,
    .SpeciesModels = NULL,
    .LocalVarMapFileNames = NULL,
    .NeighborVarMapFileNames = NULL,
    .ClimateVarMapFileNames = NULL,
    .DistanceVarMapFileNames = NULL,
    .SpeciesMapFileNames = NULL,
    .SpeciesLogFileNames = NULL,
    .LogFile = NULL
  ),

  active = list(
    #' @field LocalVariables (Optional) Named list of `data.frame`s.
    LocalVariables = function(value) {
      if (missing(value)) {
        return(private$.LocalVariables)
      }
      if (!is.null(value)) {
        stopifnot(
          is.list(value),
          !is.null(names(value)),
          all(nzchar(names(value))),
          all(vapply(
            value,
            function(df) {
              is.data.frame(df) && all(c("ForestType", "AgeRange", "Species") %in% colnames(df))
            },
            logical(1)
          ))
        )
      }
      private$.LocalVariables <- value
    },

    #' @field DerivedLocalVariables (Optional) Named character vector.
    DerivedLocalVariables = function(value) {
      if (missing(value)) {
        return(private$.DerivedLocalVariables)
      }
      if (!is.null(value)) {
        stopifnot(is.character(value), !is.null(names(value)), all(nzchar(names(value))))
      }
      private$.DerivedLocalVariables <- value
    },

    #' @field NeighborhoodVariables (Optional) `data.frame`.
    NeighborhoodVariables = function(value) {
      if (missing(value)) {
        return(private$.NeighborhoodVariables)
      }
      if (!is.null(value)) {
        stopifnot(
          is.data.frame(value),
          all(c("Name", "LocalVar", "NeighborRadius", "Transform") %in% colnames(value))
        )
      }
      private$.NeighborhoodVariables <- value
    },

    #' @field ClimateVariables (Optional) `data.frame`.
    ClimateVariables = function(value) {
      if (missing(value)) {
        return(private$.ClimateVariables)
      }
      if (!is.null(value)) {
        stopifnot(
          is.data.frame(value),
          all(c("Name", "Year", "Months", "Source", "ClimateVar", "Transform") %in% colnames(value))
        )
      }
      private$.ClimateVariables <- value
    },

    #' @field DistanceVariables (Optional) `data.frame`.
    DistanceVariables = function(value) {
      if (missing(value)) {
        return(private$.DistanceVariables)
      }
      if (!is.null(value)) {
        stopifnot(
          is.data.frame(value),
          all(c("Name", "LocalVar", "Transform") %in% colnames(value))
        )
      }
      private$.DistanceVariables <- value
    },

    #' @field SpeciesModels Named list of `data.frame`s with columns
    #'   `Parameter`, `Type`, `Value`.
    SpeciesModels = function(value) {
      if (missing(value)) {
        return(private$.SpeciesModels)
      }
      if (!is.null(value)) {
        stopifnot(
          is.list(value),
          !is.null(names(value)),
          all(nzchar(names(value))),
          all(vapply(
            value,
            function(df) {
              is.data.frame(df) && all(c("Parameter", "Type", "Value") %in% colnames(df))
            },
            logical(1)
          ))
        )
      }
      private$.SpeciesModels <- value
    },

    #' @field LocalVarMapFileNames Character. Must contain the literal
    #'   placeholders `{local-var-name}` and `{timestep}`.
    LocalVarMapFileNames = function(value) {
      if (missing(value)) {
        return(private$.LocalVarMapFileNames)
      }
      if (!is.null(value)) {
        stopifnot(
          "LocalVarMapFileNames must be a character string." = is.character(value),
          "LocalVarMapFileNames must contain the literal `{local-var-name}` placeholder." = grepl(
            "{local-var-name}",
            value,
            fixed = TRUE
          ),
          "LocalVarMapFileNames must contain the literal `{timestep}` placeholder." = grepl(
            "{timestep}",
            value,
            fixed = TRUE
          )
        )
      }
      private$.LocalVarMapFileNames <- value
    },

    #' @field NeighborVarMapFileNames Character. Must contain the literal
    #'   placeholders `{neighbor-var-name}` and `{timestep}`.
    NeighborVarMapFileNames = function(value) {
      if (missing(value)) {
        return(private$.NeighborVarMapFileNames)
      }
      if (!is.null(value)) {
        stopifnot(
          "NeighborVarMapFileNames must be a character string." = is.character(value),
          "NeighborVarMapFileNames must contain the literal `{neighbor-var-name}` placeholder." = grepl(
            "{neighbor-var-name}",
            value,
            fixed = TRUE
          ),
          "NeighborVarMapFileNames must contain the literal `{timestep}` placeholder." = grepl(
            "{timestep}",
            value,
            fixed = TRUE
          )
        )
      }
      private$.NeighborVarMapFileNames <- value
    },

    #' @field ClimateVarMapFileNames Character. Must contain the literal
    #'   placeholders `{climate-var-name}` and `{timestep}`.
    ClimateVarMapFileNames = function(value) {
      if (missing(value)) {
        return(private$.ClimateVarMapFileNames)
      }
      if (!is.null(value)) {
        stopifnot(
          "ClimateVarMapFileNames must be a character string." = is.character(value),
          "ClimateVarMapFileNames must contain the literal `{climate-var-name}` placeholder." = grepl(
            "{climate-var-name}",
            value,
            fixed = TRUE
          ),
          "ClimateVarMapFileNames must contain the literal `{timestep}` placeholder." = grepl(
            "{timestep}",
            value,
            fixed = TRUE
          )
        )
      }
      private$.ClimateVarMapFileNames <- value
    },

    #' @field DistanceVarMapFileNames Character. Must contain the literal
    #'   placeholders `{distance-var-name}` and `{timestep}`.
    DistanceVarMapFileNames = function(value) {
      if (missing(value)) {
        return(private$.DistanceVarMapFileNames)
      }
      if (!is.null(value)) {
        stopifnot(
          "DistanceVarMapFileNames must be a character string." = is.character(value),
          "DistanceVarMapFileNames must contain the literal `{distance-var-name}` placeholder." = grepl(
            "{distance-var-name}",
            value,
            fixed = TRUE
          ),
          "DistanceVarMapFileNames must contain the literal `{timestep}` placeholder." = grepl(
            "{timestep}",
            value,
            fixed = TRUE
          )
        )
      }
      private$.DistanceVarMapFileNames <- value
    },

    #' @field SpeciesMapFileNames Character. Must contain the literal
    #'   placeholders `{species-name}` and `{timestep}`.
    SpeciesMapFileNames = function(value) {
      if (missing(value)) {
        return(private$.SpeciesMapFileNames)
      }
      if (!is.null(value)) {
        stopifnot(
          "SpeciesMapFileNames must be a character string." = is.character(value),
          "SpeciesMapFileNames must contain the literal `{species-name}` placeholder." = grepl(
            "{species-name}",
            value,
            fixed = TRUE
          ),
          "SpeciesMapFileNames must contain the literal `{timestep}` placeholder." = grepl(
            "{timestep}",
            value,
            fixed = TRUE
          )
        )
      }
      private$.SpeciesMapFileNames <- value
    },

    #' @field SpeciesLogFileNames Character. Must contain the literal
    #'   placeholder `{species-name}`.
    SpeciesLogFileNames = function(value) {
      if (missing(value)) {
        return(private$.SpeciesLogFileNames)
      }
      if (!is.null(value)) {
        stopifnot(
          "SpeciesLogFileNames must be a character string." = is.character(value),
          "SpeciesLogFileNames must contain the literal `{species-name}` placeholder." = grepl(
            "{species-name}",
            value,
            fixed = TRUE
          )
        )
      }
      private$.SpeciesLogFileNames <- value
    },

    #' @field LogFile Character.
    LogFile = function(value) {
      if (missing(value)) {
        private$.LogFile
      } else {
        private$.LogFile <- .relPath(value, self$path)
      }
    },

    output_files = function(value) {
      if (!missing(value)) {
        stop("`output_files` is read-only", call. = FALSE)
      }
      c(self$LogFile)
    }
  )
)

#' Specify the `LocalVariables` block for the Landscape Habitat extension
#'
#' @param x Named list mapping reclass map names to `data.frame`s with
#'   columns `ForestType`, `AgeRange`, `Species`.
#'
#' @template return_insert
#'
#' @family Landscape Habitat Output helpers
#'
#' @keywords internal
insertLandscapeHabitatLocalVariables <- function(x) {
  stopifnot(is.list(x), length(x) >= 1L)

  blocks <- lapply(names(x), function(nm) {
    df <- x[[nm]]
    rows <- vapply(
      seq_len(nrow(df)),
      function(i) {
        prefix <- if (i == 1L) sprintf("   %s -> \t", nm) else strrep(" ", nchar(nm) + 7L)
        paste0(
          prefix,
          format(df$ForestType[i], width = -16),
          "\t",
          format(df$AgeRange[i], width = -10),
          "\t",
          df$Species[i]
        )
      },
      character(1)
    )
    c(rows, "")
  })

  c(
    glue::glue("LocalVariables"),
    glue::glue(">> Map Name    Forest Type    AgeRange    Species"),
    glue::glue(">> --------    -----------    --------    -------"),
    unlist(blocks),
    glue::glue("")
  )
}

#' Specify the `DerivedLocalVariables` block for the Landscape Habitat extension
#'
#' @param x Named character vector mapping derived-variable names to
#'   arithmetic expressions referencing local variables.
#'
#' @template return_insert
#'
#' @family Landscape Habitat Output helpers
#'
#' @keywords internal
insertLandscapeHabitatDerivedLocalVariables <- function(x) {
  stopifnot(is.character(x), !is.null(names(x)))

  rows <- vapply(
    seq_along(x),
    function(i) {
      sprintf("   %-12s -> %s", names(x)[i], x[[i]])
    },
    character(1)
  )

  c(
    glue::glue("DerivedLocalVariables"),
    glue::glue(">> Var Name    Calc"),
    glue::glue(">> --------    ----"),
    rows,
    glue::glue("")
  )
}

#' Specify the `NeighborhoodVariables` block for the Landscape Habitat extension
#'
#' @param df `data.frame` with columns `Name`, `LocalVar`, `NeighborRadius`,
#'   `Transform`.
#'
#' @template return_insert
#'
#' @family Landscape Habitat Output helpers
#'
#' @keywords internal
insertLandscapeHabitatNeighborhoodVariables <- function(df) {
  stopifnot(is.data.frame(df))

  rows <- apply(
    df[, c("Name", "LocalVar", "NeighborRadius", "Transform"), drop = FALSE],
    1,
    function(x) {
      sprintf(
        "%s\t%s\t%s\t%s",
        x[["Name"]],
        x[["LocalVar"]],
        x[["NeighborRadius"]],
        x[["Transform"]]
      )
    }
  )

  c(
    glue::glue("NeighborhoodVariables"),
    glue::glue(">> Var Name    LocalVar    NeighborRadius    Transform"),
    glue::glue(">> --------    --------    --------------    ---------"),
    rows,
    glue::glue("")
  )
}

#' Specify the `ClimateVariables` block for the Landscape Habitat extension
#'
#' @param df `data.frame` with columns `Name`, `Year`, `Months`, `Source`,
#'   `ClimateVar`, `Transform`.
#'
#' @template return_insert
#'
#' @family Landscape Habitat Output helpers
#'
#' @keywords internal
insertLandscapeHabitatClimateVariables <- function(df) {
  stopifnot(is.data.frame(df))

  rows <- apply(
    df[, c("Name", "Year", "Months", "Source", "ClimateVar", "Transform"), drop = FALSE],
    1,
    function(x) {
      paste(
        x[["Name"]],
        x[["Year"]],
        x[["Months"]],
        x[["Source"]],
        x[["ClimateVar"]],
        x[["Transform"]],
        sep = "\t"
      )
    }
  )

  c(
    glue::glue("ClimateVariables"),
    glue::glue(">> Var Name    Year    Months    Source    ClimateVar    Transform"),
    glue::glue(">> --------    ----    ------    ------    ----------    ---------"),
    rows,
    glue::glue("")
  )
}

#' Specify the `DistanceVariables` block for the Landscape Habitat extension
#'
#' @param df `data.frame` with columns `Name`, `LocalVar`, `Transform`.
#'
#' @template return_insert
#'
#' @family Landscape Habitat Output helpers
#'
#' @keywords internal
insertLandscapeHabitatDistanceVariables <- function(df) {
  stopifnot(is.data.frame(df))

  rows <- apply(df[, c("Name", "LocalVar", "Transform"), drop = FALSE], 1, function(x) {
    sprintf("%s\t%s\t%s", x[["Name"]], x[["LocalVar"]], x[["Transform"]])
  })

  c(
    glue::glue("DistanceVariables"),
    glue::glue(">> Var Name    LocalVar    Transform"),
    glue::glue(">> --------    --------    ---------"),
    rows,
    glue::glue("")
  )
}

#' Specify the `SpeciesModels` block for the Landscape Habitat extension
#'
#' @param x Named list whose names are species names and values are
#'   `data.frame`s with columns `Parameter`, `Type`, `Value` (one row
#'   per term, including `Parameter = "intercept"`).
#'
#' @template return_insert
#'
#' @family Landscape Habitat Output helpers
#'
#' @keywords internal
insertLandscapeHabitatSpeciesModels <- function(x) {
  stopifnot(is.list(x), !is.null(names(x)), length(x) >= 1L)

  blocks <- lapply(names(x), function(sp) {
    df <- x[[sp]]
    rows <- vapply(
      seq_len(nrow(df)),
      function(i) {
        prefix <- if (i == 1L) sprintf("%s\t->\t", sp) else "\t\t"
        paste0(
          prefix,
          format(df$Parameter[i], width = -16),
          "\t",
          format(df$Type[i], width = -10),
          "\t",
          df$Value[i]
        )
      },
      character(1)
    )
    c(rows, "")
  })

  c(
    glue::glue("SpeciesModels"),
    glue::glue(">> Species    Parameter    Type    Value"),
    glue::glue(">> -------    ---------    ----    -----"),
    unlist(blocks),
    glue::glue("")
  )
}
