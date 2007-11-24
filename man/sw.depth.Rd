\name{sw.depth}
\alias{sw.depth}
\title{Water depth}
\description{
  Compute depth from pressure and latitude.
}
\usage{
  d <- sw.depth(p, lat, degrees=TRUE);
}
\arguments{
  \item{p}{Pressure [dbar].}
  \item{lat}{Latitude in \eqn{^\circ}{deg}N or radians north of the equator.}
  \item{degrees}{Flag indicating whether degrees are used for latitude;
    if set to \code{FALSE}, radians are used.}
}
\details{
The information given below is adapted from the fortran code at 

\code{http://sam.ucsd.edu/sio210/propseawater/ppsw_fortran/ppsw.f},

which is where this R code came from.

Depth in meters from pressure in decibars using
Saunders and Fofonoff's method, with the 
formula refitted for 1980 equation of state

Checkvalue: depth = 9712.653 m for p=10000 decibars, latitude=30 deg
above for standard ocean: t=0 deg. celsuis ; s=35 (ipss-78)
}
\value{
  Depth [m].
}
\seealso{
  N/A.
}
\examples{
 d <- sw.depth(10, 45);
}
\references{
(authors), 1976, (title), Deep-Sea Res., 23, 109-111.
}
\author{Dan Kelley \email{Dan.Kelley@Dal.Ca}}
\keyword{misc}
