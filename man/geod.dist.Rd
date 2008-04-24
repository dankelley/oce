\name{geod.dist}
\alias{geod.dist}
\title{Geodesic distance on earth}
\description{ Compute geodesic distance on surface of earth.}
\usage{
km <- geod.dist(section)
km <- geod.dist(lat1, lon1, lat2, lon2);
}
\arguments{
  \item{section}{a \code{section} object}
  \item{lat1}{latitude or a list of latitudes}
  \item{lon1}{longitude or list of longitudes}
  \item{lat2}{latitude or list of latitudes}
  \item{lon2}{longitude or list of longitudes}
}

\details{This calculates distance between points on the earth, measured
  along the surface. The method involves the solution of the geodetic
  inverse problem, using T. Vincenty's modification of Rainsford's
  method with Helmert's elliptical terms.

  In the first form, with a single argument being an object of class
  \code{section}, the result is vector containing the distances of the
  individual stations from the first one in the section.

  In the second form, if the "1" and "2" lists are of equal length, then
  the result is the pairwise distances. However, if the length of "2" is
  shorter than the length of "1", then \emph{only} the first value in
  the "2" list is used, repeated over and over to match the length of
  "1".

  A common use is for "1" to contain a vector of positions along a
  cruise track, and for "2" to contain a reference point;
  e.g. \code{geod.dist(lats,lons,lats[1],lons[1])} gives distance along
  the track starting from zero.
}

\value{Distance, or vector of differences, measured along the surface of the earth.}

\seealso{N/A.}

\examples{
 # There are roughly 111km per degree of latitude
 km <- geod.dist(45, 100, 46, 100)/1000 
}

\references{T. Vincenty, "Direct and Inverse Solutions of Ellipsoid on the Ellipsoid with Application of Nested Equations", Survey Review, April 1975.  (As of late 2007, this is available at \url{http://www.ngs.noaa.gov/PUBS_LIB/inverse.pdf}.)
}

\author{Dan Kelley packaged this, based on R code sent to him by Darren Gillis, who in 2003 had modified Fortran code written in 1974 by (according to comment cards in the source) L. Pfeifer and J. G. Gergen.}

\keyword{misc}
