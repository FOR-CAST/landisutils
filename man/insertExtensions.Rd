% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/scenarios.R
\name{insertSuccessionExtensions}
\alias{insertSuccessionExtensions}
\alias{insertDisturbanceExtensions}
\alias{insertOtherExtensions}
\title{Specify Scenario Extensions Tables}
\usage{
insertSuccessionExtensions(exts = NULL)

insertDisturbanceExtensions(exts = NULL)

insertOtherExtensions(exts = NULL)
}
\arguments{
\item{exts}{Named list of extensions specifying the initialization file to use.}
}
\value{
Character string(s) to write to the suitable LANDIS-II input file.
}
\description{
A scenario must specify exactly one succession extension,
zero or more disturbance extensions, and zero or more other extensions.
}
\examples{
list("Biomass Succession" = "biomass-succession.txt") |>
  insertSuccessionExtensions()

list(
  "Base Fire" = "base-fire.txt",
  "Base Harvest" = "base-harvest.txt"
) |>
  insertDisturbanceExtensions()

list(
  "Output Biomass By Age" = "output-biomass-by-age.txt",
  "Output Cohort Statistics" = "output-cohort-statistics.txt"
) |>
  insertOtherExtensions()

}
