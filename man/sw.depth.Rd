\name{sw.depth}

\alias{sw.depth}

\title{Water depth}

\description{Compute depth from pressure and latitude.}

\usage{
d <- sw.depth(ctd)
d <- sw.depth(p, lat=45, degrees=TRUE)
}

\arguments{
  \item{ctd}{an object of \code{\link[base]{class}} "\code{ctd}".}
  \item{p}{Pressure [dbar]}
  \item{lat}{Latitude in \eqn{^\circ}{deg}N or radians north of the equator.}
  \item{degrees}{Flag indicating whether degrees are used for latitude;
    if set to \code{FALSE}, radians are used.}
}

\details{In the first form, the argument is a \code{ctd} object, from
  which the pressure and latitude are extracted and used to for the
  calculation.

  The information given below is adapted from the fortran code at
  \url{http://sam.ucsd.edu/sio210/propseawater/ppsw_fortran/ppsw.f}, on
  which this R code was based.

  Depth in meters from pressure in decibars using Saunders and
  Fofonoff's method, with the formula refitted for 1980 equation of
  state

  Check value: depth = 9712.653 m for p=10000 decibars, latitude=30 deg
  above for standard ocean: t=0 deg. celsuis ; s=35 (ipss-78) }

\value{Depth below the ocean surface, in metres.}

\examples{d <- sw.depth(10, 45);}

\references{(authors?), 1976, (title?), Deep-Sea Res., 23, 109-111.}

\author{Dan Kelley}

\keyword{misc}
