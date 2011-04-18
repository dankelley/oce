\name{geodDist}
\alias{geodDist}
\title{Geodesic distance on earth}
\description{ Compute geodesic distance on surface of earth.}

\usage{geodDist(lat1, lon1, lat2, lon2)}

\arguments{
  \item{lat1}{latitude or a vector of latitudes, \strong{or} a section}
  \item{lon1}{longitude or vector of longitudes}
  \item{lat2}{latitude or vector of latitudes}
  \item{lon2}{longitude or vector of longitudes}
}

\details{This calculates distance between points on the earth, measured
  along the surface. The method involves the solution of the geodetic
  inverse problem, using T. Vincenty's modification of Rainsford's
  method with Helmert's elliptical terms.

  If the first argument is an object of class \code{section}, the result
  is vector containing the distances of the individual stations from the
  first one in the section.

  If the first argument is a vector, then the "1" and "2" vectors are of
  equal length, then the result is the pairwise distances. However, if
  the length of "2" is shorter than the length of "1", then \emph{only}
  the first value in the "2" vector is used, repeated over and over to
  match the length of "1".

  A common use is for "1" to contain a vector of positions along a
  cruise track, and for "2" to contain a reference point;
  e.g. \code{geodDist(lats,lons,lats[1],lons[1])} gives distance along
  the track starting from zero.
}

\value{Distance, or vector of differences, measured along the surface of the earth.}

\seealso{\code{\link{geodXy}}}

\examples{
 # There are roughly 111km per degree of latitude
 km <- geodDist(45, 100, 46, 100)/1000 
}

\references{T. Vincenty, "Direct and Inverse Solutions of Ellipsoid on
  the Ellipsoid with Application of Nested Equations", \emph{Survey
  Review}, April 1975.  (As of early 2009, this document is available at
  \url{http://www.ngs.noaa.gov/PUBS_LIB/inverse.pdf}.)  }

\author{Dan Kelley packaged this, based on R code sent to him by Darren
  Gillis, who in 2003 had modified Fortran code written in 1974 by
  (according to comment cards in the source) L. Pfeifer and
  J. G. Gergen.}

\keyword{misc}
