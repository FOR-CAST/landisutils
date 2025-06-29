% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/climate.R
\name{writeClimateData}
\alias{writeClimateData}
\title{Write LANDIS-II Climate Input File to disk}
\usage{
writeClimateData(df, file)
}
\arguments{
\item{df}{data.frame corresponding to Climate Input Data table}

\item{file}{character, specifying the path to the file.}
}
\value{
Character string(s) specifying suitable LANDIS-II input file(s),
created as a side effect of the function.
}
\description{
Simple wrapper around \code{\link[utils:write.table]{utils::write.csv()}}.
}
