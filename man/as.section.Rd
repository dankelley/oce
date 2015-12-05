% vim:textwidth=80:expandtab:shiftwidth=2:softtabstop=2
\name{as.section}
\alias{as.section}
\title{Coerce ctd data into section dataset}
\description{Coerces a ctd dataset into a section dataset.}

\usage{as.section(salinity, temperature, pressure, longitude, latitude,  station)
}
\arguments{
  \item{salinity}{Salinity, in a vector holding values for all stations,
      or an object of \code{\link{argo-class}}.}
  \item{temperature}{Temperature, in a vector holding values for all stations.}
  \item{pressure}{Pressure, in a vector holding values for all stations.}
  \item{longitude}{Longitude, in a vector holding values for all stations.}
  \item{latitude}{Latitude, in a vector holding values for all stations.}
  \item{station}{Station identifier.}
}

\details{Sometimes the data from an entire cruise will be combined into a single
  set.  This function isolates individual stations from such data sets, and
  combines them into a section. It can also be used on argo datasets.}

\value{An object of \code{\link[base]{class}} \code{"section"} (for details, see
  \code{\link{read.section}}).}

\seealso{The documentation for \code{\link{section-class}} explains the
  structure of CTD objects, and also outlines the other functions dealing with
  them.}

\author{Dan Kelley}

\keyword{misc}
