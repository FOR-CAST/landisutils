#' Land Use Plus Extension
#'
#' @include ext_utils.R
#'
#' @references LANDIS-II Land Use Plus v4 Extension User Guide
#'   <https://github.com/LANDIS-II-Foundation/Extension-Land-Use-Plus/blob/master/docs/LANDIS-II%20Land%20Use%20Plus%20v4%20User%20Guide.pdf>
#'
#' @family Land Use Plus helpers
#'
#' @export
LandUsePlus <- R6Class(
  "LandUsePlus",
  inherit = LandisExtension,
  public = list(
    #' @param path Character. Directory path.
    #' @param Timestep Integer. Years between land-use evaluations.
    #' @param InputMaps Character. Template path for the per-timestep land-use
    #'   rasters (e.g. `"landuse-{timestep}.tif"`).
    #' @param SiteLog (Optional) Character. Relative path to the per-site CSV
    #'   change log.
    #' @param ExternalScript,ExternalExecutable (Optional) Character. Used to
    #'   enable the LANDIS-Pause hook for an external script (user guide
    #'   sections 3.2.5 and 3.2.6).
    #' @param LandUses List of `LandUseType` objects (see [landUseType()]).
    initialize = function(
      path,
      Timestep = NULL,
      InputMaps = NULL,
      SiteLog = NULL,
      ExternalScript = NULL,
      ExternalExecutable = NULL,
      LandUses = list()
    ) {
      stopifnot(!is.null(path))

      ## LandisExtension fields. The v8-release Docker image registers this
      ## extension as "Land Use Change" in scenario.txt's extension list, but
      ## the parser inside the extension's own config file expects the legacy
      ## "Land Use" header. Both verified against
      ## ghcr.io/landis-ii-foundation/landis-ii-v8-release:main.
      private$.LandisData <- "Land Use"
      private$.scenarioName <- "Land Use Change"
      self$Timestep <- Timestep

      ## v8-release classifies "Land Use Change" as a disturbance extension
      ## (verified against the runtime parser); the v7 docs called it "other"
      ## but Core8 + parser say disturbance.
      self$type <- "disturbance"
      self$path <- path
      self$files <- "land-use.txt" ## file won't exist yet

      ## additional fields
      self$InputMaps <- InputMaps %||% "landuse-{timestep}.tif"
      self$SiteLog <- SiteLog
      self$ExternalScript <- ExternalScript
      self$ExternalExecutable <- ExternalExecutable
      self$LandUses <- LandUses
    },

    #' @param value `LandUseType` object to append to `$LandUses`.
    add_land_use = function(value) {
      stopifnot(inherits(value, "LandUseType"))
      private$.LandUses <- c(private$.LandUses, list(value))
    },

    #' @description Write extension inputs to disk
    write = function() {
      stopifnot(!is.null(self$InputMaps), length(self$LandUses) >= 1L)

      ## ensure unique LandUse names and MapCodes
      nms <- vapply(self$LandUses, function(lu) lu$name, character(1))
      codes <- vapply(self$LandUses, function(lu) lu$mapCode, integer(1))
      stopifnot(!anyDuplicated(nms), !anyDuplicated(codes))

      writeLines(
        c(
          insertLandisData(private$.LandisData),
          insertValue("Timestep", self$Timestep),
          insertValue("InputMaps", self$InputMaps),
          if (!is.null(self$SiteLog)) insertValue("SiteLog", self$SiteLog),
          if (!is.null(self$ExternalScript)) {
            insertValue("ExternalScript", self$ExternalScript)
          },
          if (!is.null(self$ExternalExecutable)) {
            insertValue("ExternalExecutable", self$ExternalExecutable)
          },
          unlist(lapply(self$LandUses, insertLandUseType))
        ),
        file.path(self$path, self$files[1])
      )

      return(invisible(self))
    }
  ),

  private = list(
    .InputMaps = NULL,
    .SiteLog = NULL,
    .ExternalScript = NULL,
    .ExternalExecutable = NULL,
    .LandUses = list()
  ),

  active = list(
    #' @field InputMaps Character. Template for per-timestep land-use rasters;
    #'   must contain the literal placeholder `{timestep}` and end in `.tif`
    #'   (LU+ v4 only accepts GeoTIFF).
    InputMaps = function(value) {
      if (missing(value)) {
        return(private$.InputMaps)
      } else {
        if (!is.null(value)) {
          if (!grepl("{timestep}", value, fixed = TRUE)) {
            stop("`InputMaps` must contain the literal placeholder `{timestep}`.")
          }
          if (!grepl("\\.tif$", value, ignore.case = TRUE)) {
            stop("`InputMaps` must end in `.tif` (LU+ v4 only accepts GeoTIFF).")
          }
        }
        private$.InputMaps <- value
      }
    },

    #' @field SiteLog (Optional) Character. Relative file path.
    SiteLog = function(value) {
      if (missing(value)) {
        return(private$.SiteLog)
      } else {
        if (!is.null(value)) {
          private$.SiteLog <- .relPath(value, self$path)
        } else {
          private$.SiteLog <- NULL
        }
      }
    },

    #' @field ExternalScript (Optional) Character.
    ExternalScript = function(value) {
      if (missing(value)) {
        private$.ExternalScript
      } else {
        private$.ExternalScript <- value
      }
    },

    #' @field ExternalExecutable (Optional) Character.
    ExternalExecutable = function(value) {
      if (missing(value)) {
        private$.ExternalExecutable
      } else {
        private$.ExternalExecutable <- value
      }
    },

    #' @field LandUses List of `LandUseType` objects.
    LandUses = function(value) {
      if (missing(value)) {
        return(private$.LandUses)
      } else {
        if (is.null(value)) {
          private$.LandUses <- list()
          return(invisible())
        }
        if (inherits(value, "LandUseType")) {
          value <- list(value)
        }
        stopifnot(is.list(value), all(vapply(value, inherits, logical(1), "LandUseType")))
        private$.LandUses <- value
      }
    },

    output_files = function(value) {
      if (!missing(value)) {
        stop("`output_files` is read-only", call. = FALSE)
      }
      c(self$SiteLog) ## NULL when SiteLog not configured
    }
  )
)

#' Allowed Land Use Plus `LandCoverChange` types
#' @keywords internal
.landCoverChangeTypes <- c("NoChange", "RemoveTrees", "InsectDefoliation")

#' Construct a Land Use type
#'
#' Defines one land use entry for [LandUsePlus]. Each land use has a
#' unique `name` and `mapCode`, an `allowHarvest` flag, an optional
#' `preventEstablishment` flag, and one or more `landCoverChange()`
#' blocks describing what happens to existing trees.
#'
#' @param name Character. Land-use name (no internal whitespace; quoted
#'   automatically when written).
#' @param mapCode Integer. Numeric code matching the input raster.
#' @param allowHarvest Logical (or `"yes"`/`"no"`). Whether harvesting is
#'   permitted. Defaults to `TRUE` to match the LU+ v4 user-guide default.
#' @param preventEstablishment Logical. When `TRUE`, the keyword
#'   `PreventEstablishment` is emitted (which blocks tree establishment).
#' @param changes List of `LandCoverChange` objects (see [landCoverChange()]).
#' @param plant (Optional) Character. Species code planted after change.
#'
#' @returns A list of class `"LandUseType"`.
#'
#' @family Land Use Plus helpers
#'
#' @export
landUseType <- function(
  name,
  mapCode,
  allowHarvest = TRUE,
  preventEstablishment = FALSE,
  changes = list(),
  plant = NULL
) {
  stopifnot(
    is.character(name),
    length(name) == 1L,
    nzchar(name),
    !is.null(mapCode),
    as.integer(mapCode) > 0L,
    !is.null(allowHarvest)
  )

  if (inherits(changes, "LandCoverChange")) {
    changes <- list(changes)
  }
  stopifnot(
    is.list(changes),
    length(changes) >= 1L,
    all(vapply(changes, inherits, logical(1), "LandCoverChange"))
  )

  if (!is.null(plant)) {
    stopifnot(is.character(plant), length(plant) == 1L)
  }

  structure(
    list(
      name = name,
      mapCode = as.integer(mapCode),
      allowHarvest = yesno(allowHarvest),
      preventEstablishment = isTRUE(preventEstablishment),
      changes = changes,
      plant = plant
    ),
    class = "LandUseType"
  )
}

#' Construct a Land Cover Change for a [landUseType()]
#'
#' One `landCoverChange()` describes what happens to existing tree cohorts
#' on a site that transitions to (or remains in) a particular land use.
#'
#' @param type Character. One of `"NoChange"`, `"RemoveTrees"`,
#'   `"InsectDefoliation"`.
#' @param repeatHarvest Logical (or `"yes"`/`"no"`). When `TRUE`, the
#'   removal/defoliation is re-applied each timestep.
#' @param cohorts (Required for `"RemoveTrees"` / `"InsectDefoliation"`)
#'   List of `CohortSelector` objects (see [cohortSelector()]).
#'
#' @returns A list of class `"LandCoverChange"`.
#'
#' @family Land Use Plus helpers
#'
#' @export
landCoverChange <- function(type, repeatHarvest = FALSE, cohorts = NULL) {
  stopifnot(type %in% .landCoverChangeTypes)

  if (type == "NoChange") {
    if (!is.null(cohorts)) {
      stop("`cohorts` must be NULL when type = \"NoChange\".")
    }
  } else {
    if (inherits(cohorts, "CohortSelector")) {
      cohorts <- list(cohorts)
    }
    stopifnot(
      is.list(cohorts),
      length(cohorts) >= 1L,
      all(vapply(cohorts, inherits, logical(1), "CohortSelector"))
    )
  }

  structure(
    list(type = type, repeatHarvest = yesno(repeatHarvest), cohorts = cohorts),
    class = "LandCoverChange"
  )
}

#' Construct a cohort selector for a Land Use Plus change block
#'
#' Bundles a species code with one or more `(low, high, percent)` age-range
#' selections. Multiple ranges may be passed for the same species.
#'
#' @param species Character. Species code.
#' @param ranges `data.frame` with columns `low`, `high`, `percent`. `low`
#'   may equal `high` (single age) and `percent` is the proportion (0-100)
#'   of cohorts in that range that are affected.
#'
#' @returns A list of class `"CohortSelector"`.
#'
#' @examples
#' cohortSelector(
#'   species = "querrubr",
#'   ranges = data.frame(low = c(1, 71), high = c(62, 200), percent = c(20, 25))
#' )
#'
#' @family Land Use Plus helpers
#'
#' @export
cohortSelector <- function(species, ranges) {
  stopifnot(
    is.character(species),
    length(species) == 1L,
    nzchar(species),
    is.data.frame(ranges),
    all(c("low", "high", "percent") %in% colnames(ranges)),
    all(ranges$low <= ranges$high),
    all(ranges$percent >= 0, ranges$percent <= 100)
  )

  structure(list(species = species, ranges = ranges), class = "CohortSelector")
}

#' Specify a single `LandUse` block for the Land Use Plus extension
#'
#' @param lu `LandUseType` object.
#'
#' @template return_insert
#'
#' @family Land Use Plus helpers
#'
#' @keywords internal
insertLandUseType <- function(lu) {
  stopifnot(inherits(lu, "LandUseType"))

  c(
    glue::glue(">>------------------------------------"),
    glue::glue("LandUse          \"{lu$name}\""),
    glue::glue("MapCode          {lu$mapCode}"),
    glue::glue("AllowHarvest?    {lu$allowHarvest}"),
    if (lu$preventEstablishment) glue::glue("PreventEstablishment"),
    unlist(lapply(lu$changes, insertLandCoverChange)),
    if (!is.null(lu$plant)) c(glue::glue("Plant            {lu$plant}"), glue::glue(""))
  )
}

#' Specify a single `LandCoverChange` block within a `LandUse`
#'
#' @param lcc `LandCoverChange` object.
#'
#' @template return_insert
#'
#' @family Land Use Plus helpers
#'
#' @keywords internal
insertLandCoverChange <- function(lcc) {
  stopifnot(inherits(lcc, "LandCoverChange"))

  if (lcc$type == "NoChange") {
    return(c(glue::glue("LandCoverChange  NoChange"), glue::glue("")))
  }

  c(
    glue::glue("LandCoverChange  {lcc$type}"),
    glue::glue("RepeatHarvest?   {lcc$repeatHarvest}"),
    unlist(lapply(lcc$cohorts, insertCohortSelector)),
    glue::glue("")
  )
}

#' Specify a single cohort-selector line within a `LandCoverChange`
#'
#' @param sel `CohortSelector` object.
#'
#' @template return_insert
#'
#' @family Land Use Plus helpers
#'
#' @keywords internal
insertCohortSelector <- function(sel) {
  stopifnot(inherits(sel, "CohortSelector"))

  rng_str <- paste(
    apply(sel$ranges, 1, function(r) {
      lo <- as.integer(r[["low"]])
      hi <- as.integer(r[["high"]])
      pc <- r[["percent"]]
      if (lo == hi) {
        sprintf("%d(%g%%)", lo, pc)
      } else {
        sprintf("%d-%d(%g%%)", lo, hi, pc)
      }
    }),
    collapse = " "
  )

  glue::glue("    {sel$species} {rng_str}")
}
