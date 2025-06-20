% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ecoregions.R
\name{prepEcoregionsFiles}
\alias{prepEcoregionsFiles}
\title{Create \code{Ecoregions} and \code{EcoregionsMap} Files}
\usage{
prepEcoregionsFiles(ecoregion, ecoregionMap, path = NULL)
}
\arguments{
\item{ecoregion}{\code{data.frame}}

\item{ecoregionMap}{\code{SpatRaster}}

\item{path}{Character. Path specifying a directory to use for the scenario runs.}
}
\value{
Character string(s) specifying suitable LANDIS-II input file(s),
created as a side effect of the function.
}
\description{
Create \code{Ecoregions} and \code{EcoregionsMap} Files
}
