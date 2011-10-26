\name{as.drifter}

\alias{as.drifter}

\title{Coerce data into drifter dataset}

\description{Coerces a dataset into a drifter dataset.}

\usage{
as.drifter(time, longitude, latitude, salinity, temperature, pressure, 
           id, filename="", missingValue)
}

\arguments{
  \item{time}{time of observation}
  \item{longitude}{longitude of observation}
  \item{latitude}{latitude of observation}
  \item{salinity}{salinity of observation}
  \item{temperature}{temperature of observation}
  \item{pressure}{pressure of observation}
  \item{id}{drifter identifier}
  \item{filename}{source filename}
  \item{missingValue}{optional missing value, indicating data that should be
      taken as \code{NA}.}
}

\details{This function assembles vectors into a \code{drifter} object, e.g. so
    that \code{\link{plot.drifter}} can be used.}

\value{An object of \code{\link[base]{class}} \code{"drifter"}.}

\seealso{The documentation for \code{\link{drifter-class}} explains the
    structure of drifter objects, and also outlines the other functions dealing
    with them.}

\author{Dan Kelley}

\keyword{misc}
