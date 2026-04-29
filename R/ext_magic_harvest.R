#' Magic Harvest Extension
#'
#' Wraps an external script invocation that mutates a Biomass Harvest
#' parameter file at each timestep.
#'
#' @include ext_utils.R
#'
#' @references LANDIS-II Magic Harvest v2 Extension User Guide
#'   <https://github.com/Klemet/LANDIS-II-Magic-Harvest>
#'
#' @family Magic Harvest helpers
#'
#' @export
MagicHarvest <- R6Class(
  "MagicHarvest",
  inherit = LandisExtension,
  public = list(
    #' @param path Character. Directory path.
    #' @param Timestep Integer. Years between Magic Harvest invocations.
    #' @param HarvestExtensionParameterFile Character. Relative path to the
    #'   wrapped harvest extension's parameter file (e.g. a Biomass Harvest `.txt`).
    #' @param ProcessToLaunch Character. Program name to execute
    #'   (e.g. `"python"`, `"Rscript"`); must be callable from the shell.
    #' @param ProcessArguments Character. Arguments passed to the process;
    #'   may contain a literal `{timestep}` placeholder.
    #'   Use `""` or `"{none}"` for no arguments.
    #' @param NoHarvestReInitialization Logical (or `"true"`/`"false"`). When
    #'   `TRUE`, skips re-initialization of the wrapped harvest extension
    #'   between timesteps. Optional; defaults to `FALSE`. LANDIS-II's Magic
    #'   Harvest parser requires the literal token `true` or `false` here
    #'   (not `yes`/`no`); see [truefalse()].
    initialize = function(
      path,
      Timestep = NULL,
      HarvestExtensionParameterFile = NULL,
      ProcessToLaunch = NULL,
      ProcessArguments = NULL,
      NoHarvestReInitialization = FALSE
    ) {
      stopifnot(!is.null(path))

      ## LandisExtension fields
      private$.LandisData <- "Magic Harvest"
      self$Timestep <- Timestep

      self$type <- "disturbance"
      self$path <- path
      self$files <- "magic-harvest.txt" ## file won't exist yet

      ## additional fields for this extension
      self$HarvestExtensionParameterFile <- HarvestExtensionParameterFile
      self$ProcessToLaunch <- ProcessToLaunch
      self$ProcessArguments <- ProcessArguments
      self$NoHarvestReInitialization <- NoHarvestReInitialization
    },

    #' @description Write extension inputs to disk
    write = function() {
      stopifnot(
        !is.null(self$Timestep),
        !is.null(self$HarvestExtensionParameterFile),
        !is.null(self$ProcessToLaunch),
        !is.null(self$ProcessArguments)
      )

      writeLines(
        c(
          insertLandisData(private$.LandisData),
          insertValue("Timestep", self$Timestep),
          insertValue("HarvestExtensionParameterFile", self$HarvestExtensionParameterFile),
          insertValue("ProcessToLaunch", self$ProcessToLaunch),
          insertValue("ProcessArguments", self$ProcessArguments),
          insertValue("NoHarvestReInitialization", self$NoHarvestReInitialization)
        ),
        file.path(self$path, self$files[1])
      )

      return(invisible(self))
    }
  ),

  private = list(
    .HarvestExtensionParameterFile = NULL,
    .ProcessToLaunch = NULL,
    .ProcessArguments = NULL,
    .NoHarvestReInitialization = NULL
  ),

  active = list(
    #' @field HarvestExtensionParameterFile Character. Relative path to the
    #'   wrapped harvest extension's parameter file.
    HarvestExtensionParameterFile = function(value) {
      if (missing(value)) {
        return(private$.HarvestExtensionParameterFile)
      } else {
        if (!is.null(value)) {
          stopifnot(is.character(value), length(value) == 1L, nzchar(value))
        }
        private$.HarvestExtensionParameterFile <- value
      }
    },

    #' @field ProcessToLaunch Character. Program name to execute.
    ProcessToLaunch = function(value) {
      if (missing(value)) {
        return(private$.ProcessToLaunch)
      } else {
        if (!is.null(value)) {
          stopifnot(is.character(value), length(value) == 1L, nzchar(value))
        }
        private$.ProcessToLaunch <- value
      }
    },

    #' @field ProcessArguments Character. Arguments passed to the process.
    ProcessArguments = function(value) {
      if (missing(value)) {
        return(private$.ProcessArguments)
      } else {
        if (!is.null(value)) {
          stopifnot(is.character(value), length(value) == 1L)
        }
        private$.ProcessArguments <- value
      }
    },

    #' @field NoHarvestReInitialization Character `"true"` or `"false"` (set via
    #'   logical or `"true"`/`"false"`). The Magic Harvest extension parser
    #'   only accepts boolean tokens here, not `yes`/`no`.
    NoHarvestReInitialization = function(value) {
      if (missing(value)) {
        return(private$.NoHarvestReInitialization)
      } else {
        private$.NoHarvestReInitialization <- truefalse(value)
      }
    }
  )
)
