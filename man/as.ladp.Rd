\name{as.ladp}
\alias{as.ladp}
\title{Coerce data into an ladp object}
\description{Coerces data into an ladp object}

\usage{as.ladp(longitude, latitude, station, time,
    pressure, u, v, uz, vz, salinity, temperature, ...)}

\arguments{
    \item{longitude}{longitude in degrees east (use negative in western hemisphere).}
    \item{latitude}{latitude in degrees east (use negative in southern hemisphere).}
    \item{station}{number or string indicating station ID.}
    \item{time}{time at the start of the profile, constructed by e.g. \code{\link{as.POSIXct}}.}
    \item{pressure}{pressure in decibars, through the water column.}
    \item{u}{eastward velocity (m/s).}
    \item{v}{northward velocity (m/s).}
    \item{uz}{vertical derivative of eastward velocity (1/s).}
    \item{vz}{vertical derivative of northward velocity (1/s).}
    \item{salinity}{salinity through the water column, in practical salinity units.}
    \item{temperature}{temperature through the water column.}
    \item{\dots}{optional additional data columns.}
}

\details{This function assembles vectors of pressure and velocity, possibly
    also with shears, salinity, temperature, etc.}

\value{An object of \code{\link[base]{class}} \code{"ladp"}.}

\seealso{The documentation for \code{\link{ladp-class}} explains the structure
    of \code{ladp} objects, and also outlines the other functions dealing with
    them.}

\author{Dan Kelley}

\keyword{misc}
