% vim:textwidth=80:expandtab:shiftwidth=2:softtabstop=2
\name{as.argo}

\alias{as.argo}

\title{Coerce data into argo dataset}

\description{Coerces a dataset into an argo dataset.}

\usage{
as.argo(time, longitude, latitude, salinity, temperature, pressure, 
        units=NULL, id, filename="", missingValue)
}

\arguments{
  \item{time}{time of observation}
  \item{longitude}{longitude of observation}
  \item{latitude}{latitude of observation}
  \item{salinity}{salinity of observation}
  \item{temperature}{temperature of observation}
  \item{pressure}{pressure of observation}
  \item{units}{an optional list containing units. If \code{NULL}, the default,
     then \code{"degree east"} is used for \code{longitude},
     \code{"degree north"} for \code{latitude},
     \code{""} for \code{salinity},
     \code{"ITS-90"} for \code{temperature}, and
     \code{"dbar"} for \code{pressure}.}
  \item{id}{argo identifier}
  \item{filename}{source filename}
  \item{missingValue}{optional missing value, indicating data that should be
      taken as \code{NA}.}
}

\details{This function assembles vectors into a \code{\link{argo-class}} object,
  e.g. so that \code{\link{plot.argo}} can be used. Use \code{\link{oceSetData}}
  to add other columns to the standard set, if e.g.  the device measures
  nitrate, etc.}

\value{An object of \code{\link[base]{class}} \code{"argo"}.}

\seealso{The documentation for \code{\link{argo-class}} explains the structure
  of argo objects, and also outlines the other functions dealing with them.}

\author{Dan Kelley}

\keyword{misc}
