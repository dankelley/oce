\name{sw.depth}

\alias{sw.depth}

\title{Water depth}

\description{Compute depth from pressure and latitude.}

\usage{sw.depth(p, lat=45, degrees=TRUE)}

\arguments{
  \item{p}{either pressure [dbar], in which case \code{lat} must also be
    given, or a \code{ctd} object, in which case \code{lat} will be
    inferred from the object.}
  \item{lat}{Latitude in \eqn{^\circ}{deg}N or radians north of the equator.}
  \item{degrees}{Flag indicating whether degrees are used for latitude;
    if set to \code{FALSE}, radians are used.}
}

\details{If the first argument is a \code{ctd} object, then the pressure
  and latitude are extracted from it, and used to for the calculation.

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
