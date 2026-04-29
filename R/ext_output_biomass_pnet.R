#' PnET Biomass Output Extension
#'
#' Companion output extension to [PnETSuccession]; both share the same user
#' guide. §11 of the v6.0 guide documents the `LandisData "Output-PnET"`
#' input file format and the supported output keywords (e.g. `Biomass`,
#' `LeafAreaIndex`, the `Monthly*` keywords, etc.); §11.5 covers the
#' output-file-name templates and the `{species}` / `{timestep}` placeholders.
#'
#' @include ext_utils.R
#'
#' @references LANDIS-II PnET-Succession v6.0 Extension User Guide (§11
#'   covers the Output-PnET input file)
#'   <https://github.com/LANDIS-II-Foundation/Extension-Output-Biomass-PnET/blob/master/docs/LANDIS-II%20PnET-Succession%20v6.0%20User%20Guide%20Jan21%202026.pdf>
#'
#' @seealso
#' [PnETSuccession] (this extension's required succession backend; output
#' fields here consume PnET-specific cohort state).
#'
#' @family PnET Output helpers
#'
#' @export
OutputBiomassPnET <- R6Class(
  "OutputBiomassPnET",
  inherit = LandisExtension,
  public = list(
    #' @param path Character. Directory path.
    #' @param Timestep Integer. Years between output snapshots.
    #' @param Species Character vector of species codes, or `"all"` / `"none"`.
    #' @param Outputs Named list mapping LANDIS-II output keywords (any of
    #'   `r paste0("\\code{", .pnetOutputKeywords, "}", collapse = ", ")`) to character file
    #'   patterns. Each pattern may contain `{timestep}`, `{species}`,
    #'   `{month}`, or `{year}` placeholders depending on the keyword.
    #'   Use `defaultPnETOutputFiles()` for a sensible default set.
    initialize = function(
      path,
      Timestep = NULL,
      Species = "all",
      Outputs = NULL
    ) {
      stopifnot(!is.null(path))

      ## LandisExtension fields
      private$.LandisData <- "Output-PnET"
      self$Timestep <- Timestep

      self$type <- "other"
      self$path <- path
      self$files <- "output-biomass-pnet.txt" ## file won't exist yet

      ## additional fields for this extension
      self$Species <- Species
      self$Outputs <- Outputs %||% defaultPnETOutputFiles()
    },

    #' @description Write extension inputs to disk
    write = function() {
      stopifnot(!is.null(self$Timestep), !is.null(self$Species))

      species_line <- if (length(self$Species) == 1L && self$Species %in% c("all", "none")) {
        glue::glue("Species {toupper(self$Species)}")
      } else {
        glue::glue("Species {paste(self$Species, collapse = ' ')}")
      }

      output_lines <- unlist(lapply(names(self$Outputs), function(nm) {
        insertValue(nm, self$Outputs[[nm]])
      }))

      writeLines(
        c(
          insertLandisData(private$.LandisData),
          insertValue("Timestep", self$Timestep),
          species_line,
          glue::glue(""),
          output_lines
        ),
        file.path(self$path, self$files[1])
      )

      return(invisible(self))
    }
  ),

  private = list(
    .Species = NULL,
    .Outputs = NULL
  ),

  active = list(
    #' @field Species Character vector of species codes, or `"all"` / `"none"`.
    Species = function(value) {
      if (missing(value)) {
        return(private$.Species)
      } else {
        if (!is.null(value)) {
          stopifnot(is.character(value), length(value) >= 1L, all(nzchar(value)))
        }
        private$.Species <- value
      }
    },

    #' @field Outputs Named list mapping LANDIS-II output keywords to file
    #'   patterns.
    Outputs = function(value) {
      if (missing(value)) {
        return(private$.Outputs)
      } else {
        if (is.null(value)) {
          private$.Outputs <- list()
          return(invisible())
        }
        stopifnot(
          is.list(value),
          !is.null(names(value)),
          all(nzchar(names(value))),
          all(names(value) %in% .pnetOutputKeywords),
          all(vapply(value, function(v) is.character(v) && length(v) == 1L, logical(1)))
        )
        private$.Outputs <- value
      }
    }
  )
)

#' Recognized output-keyword names for the PnET Output extension
#'
#' Names accepted by [OutputBiomassPnET]'s `Outputs` argument; mirrors the
#' upstream parser in `Extension-Output-Biomass-PnET/src/InputParametersParser.cs`.
#'
#' @keywords internal
.pnetOutputKeywords <- c(
  "Biomass",
  "BelowgroundBiomass",
  "WoodBiomass",
  "FoliageBiomass",
  "RootBiomass",
  "WoodySenescence",
  "FoliageSenescence",
  "AgeDistribution",
  "CohortsPerSpecies",
  "CohortBalance",
  "LeafAreaIndex",
  "MonthlyAvgLAI",
  "Water",
  "MonthlyAvgWater",
  "MonthlyEvap",
  "MonthlyInterception",
  "MonthlyActualTrans",
  "MonthlyPotentialTrans",
  "MonthlyPotentialEvap",
  "MonthlyLeakage",
  "MonthlyRunoff",
  "MonthlyAET",
  "AET",
  "AETAvg",
  "PET",
  "AnnualPsn",
  "MonthlyNetPsn",
  "MonthlyGrossPsn",
  "MonthlyFolResp",
  "MonthlyMaintResp",
  "NSC",
  "WoodyDebris",
  "Litter",
  "SubCanopyPAR",
  "Albedo",
  "MonthlyActiveLayerDepth",
  "MonthlyFrostDepth",
  "MonthlyAvgSnowPack",
  "EstablishmentTable",
  "EstablishmentProbability",
  "Establishment",
  "MortalityTable",
  "SiteMossDepth"
)

#' Default `Outputs` list for the PnET Biomass Output extension
#'
#' Covers the most commonly enabled outputs (matches the example in the
#' upstream `OneCellSimulation/biomass.output.txt`).
#'
#' @returns Named list mapping output keywords to relative file patterns.
#'
#' @family PnET Output helpers
#'
#' @export
defaultPnETOutputFiles <- function() {
  list(
    Biomass = "output/biomass/{species}/biomass-{timestep}.img",
    LeafAreaIndex = "output/leaf-area-index/lai-{timestep}.img",
    Water = "output/soil-water/water-{timestep}.img",
    SubCanopyPAR = "output/sub-canopy-par/scpar-{timestep}.img",
    AgeDistribution = "output/age-distribution/age-{timestep}.img",
    CohortBalance = "output/cohort-balance.txt",
    FoliageSenescence = "output/senescence/foliage-{timestep}.img",
    WoodySenescence = "output/senescence/woody-{timestep}.img"
  )
}
