#' Biomass Reclassification Output Extension
#'
#' @references LANDIS-II Output Biomass Reclassification v4 Extension User Guide
#'   <https://github.com/LANDIS-II-Foundation/Extension-Output-Biomass-Reclass/blob/master/docs/LANDIS-II%20Biomass%20Reclass%20Output%20v4%20User%20Guide.pdf>
#'
#' @family Biomass Reclass Output helpers
#'
#' @export
OutputBiomassReclass <- R6Class(
  "OutputBiomassReclass",
  inherit = LandisExtension,
  public = list(
    #' @param path Character. Directory path.
    #' @param Timestep Integer.
    #' @param ReclassMaps Nested named list. Top-level names are reclassification
    #'   map names (no spaces). Each element is itself a named list whose names
    #'   are forest types and whose values are character vectors of species codes.
    #'   Species prefixed with `-` are subtracted from the forest type's
    #'   dominance value. See user guide §2.3.
    #' @param MapFileNames Character. Output-map filename pattern; must contain
    #'   the literals `{reclass-map-name}` and `{timestep}`.
    initialize = function(
      path = NULL,
      Timestep = 10L,
      ReclassMaps = NULL,
      MapFileNames = NULL
    ) {
      stopifnot(!is.null(path))

      ## LandisExtension fields
      private$.LandisData <- "Output Biomass Reclass"
      self$Timestep <- Timestep

      self$type <- "other"
      self$path <- path
      self$files <- "output-biomass-reclass.txt" ## file won't exist yet

      ## additional fields for this extension
      self$ReclassMaps <- ReclassMaps
      self$MapFileNames <- MapFileNames %||%
        "outputs/biomass-reclass/biomass-reclass-{reclass-map-name}-{timestep}.tif"
    },

    #' @description Write extension inputs to disk
    write = function() {
      stopifnot(
        !is.null(self$ReclassMaps),
        length(self$ReclassMaps) >= 1L
      )

      writeLines(
        c(
          insertLandisData(private$.LandisData),
          insertValue("Timestep", self$Timestep),
          insertReclassMaps(self$ReclassMaps),
          insertValue("MapFileNames", self$MapFileNames)
        ),
        file.path(self$path, self$files[1])
      )

      return(invisible(self))
    }
  ),

  private = list(
    .ReclassMaps = NULL,
    .MapFileNames = NULL
  ),

  active = list(
    #' @field ReclassMaps Nested named list. See `$initialize()` for the expected
    #'   structure.
    ReclassMaps = function(value) {
      if (missing(value)) {
        return(private$.ReclassMaps)
      } else {
        if (is.null(value)) {
          private$.ReclassMaps <- NULL
          return(invisible())
        }
        stopifnot(
          is.list(value),
          !is.null(names(value)),
          all(nzchar(names(value))),
          !any(grepl("\\s", names(value))),
          all(vapply(value, is.list, logical(1))),
          all(vapply(value, function(v) !is.null(names(v)), logical(1)))
        )
        private$.ReclassMaps <- value
      }
    },

    #' @field MapFileNames Character. Output filename pattern; must contain
    #'   `{reclass-map-name}` and `{timestep}`.
    MapFileNames = function(value) {
      if (missing(value)) {
        return(private$.MapFileNames)
      } else {
        stopifnot(
          grepl("{reclass-map-name}", value, fixed = TRUE),
          grepl("{timestep}", value, fixed = TRUE)
        )
        private$.MapFileNames <- value
      }
    }
  )
)

#' Specify the `ReclassMaps` block for the Output Biomass Reclass extension
#'
#' @param x Nested named list (see [OutputBiomassReclass]) whose top-level
#'   names are reclassification map names and whose values are named lists
#'   mapping forest type -> character vector of species codes.
#'
#' @template return_insert
#'
#' @family Biomass Reclass Output helpers
#'
#' @keywords internal
insertReclassMaps <- function(x) {
  stopifnot(is.list(x), length(x) >= 1L, !is.null(names(x)))

  ## align forest-type column across all reclass maps for readability
  type_width <- max(nchar(unlist(lapply(x, names))), 13L) + 2L

  blocks <- lapply(names(x), function(nm) {
    forest_types <- x[[nm]]
    lines <- character(0)
    for (i in seq_along(forest_types)) {
      ftype <- names(forest_types)[i]
      species <- paste(forest_types[[i]], collapse = " ")
      prefix <- if (i == 1L) {
        glue::glue("   {nm} -> ")
      } else {
        strrep(" ", nchar(nm) + 7L)
      }
      type_col <- formatC(ftype, width = -type_width, flag = "-")
      lines <- c(lines, paste0(prefix, type_col, species))
    }
    c(lines, "")
  })

  c(
    glue::glue("ReclassMaps"),
    glue::glue(">> Map Name    Forest Type    Species"),
    glue::glue(">> --------    -------------  -------"),
    unlist(blocks),
    glue::glue("") ## blank line after the item group
  )
}
