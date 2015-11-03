% vim:textwidth=80:expandtab:shiftwidth=2:softtabstop=2
\name{geodDist}

\alias{geodDist}

\title{Geodesic distance on earth}

\description{Compute geodesic distance on surface of earth.}

\usage{geodDist(longitude1, latitude1=NULL, longitude2=NULL, latitude2=NULL, alongPath=FALSE)}

\arguments{

  \item{longitude1}{longitude or a vector of longitudes, \strong{or} a
    \code{section} object, from which longitude and latitude are extracted and
    used instead of the next three arguments}

  \item{latitude1}{latitude or vector of latitudes (ignored if \code{longitude1}
    is a \code{section} object)}

  \item{longitude2}{optional longitude or vector of longitudes (ignored if
    \code{alongPath=TRUE})}

  \item{latitude2}{optional latitude or vector of latitudes (ignored if
    \code{alongPath=TRUE})}

  \item{alongPath}{boolean indicating whether to compute distance along the
    path, as opposed to distance from the reference point.  If
    \code{alongPath=TRUE}, any values provided for \code{latitude2} and
    \code{longitude2} will be ignored.}

}

\details{This calculates geodesic distance between points on the earth, i.e.
  distance measured along the (presumed ellipsoidal) surface. The method
  involves the solution of the geodetic inverse problem, using T. Vincenty's
  modification of Rainsford's method with Helmert's elliptical terms.

  The function may be used in several different ways.

  Case 1: \code{longitude1} is a \code{section} object. The values of
  \code{latitude1}, \code{longitude2}, and \code{latitude2} arguments are
  ignored, and the behaviour depends on the value of the \code{alongPath}
  argument.  If \code{alongPath=FALSE}, the return value contains the geodetic
  distances of each station from the first one.  If \code{alongPath=TRUE}, the
  return value is the geodetic distance along the path connecting the stations,
  in the order in which they are stored in the section.

  Case 2: \code{longitude1} is a vector.  If \code{longitude2} and
  \code{latitude2} are not given, then the return value is a vector containing
  the distances of each point from the first one, \emph{or} the distancce along
  the path connecting the points, according to the value of \code{alongPath}.
  On the other hand, if both \code{longitude2} and \code{latitude2} are
  specified, then the return result depends on the length of these arguments.
  If they are each of length 1, then they are taken as a reference point, from
  which the distances to \code{longitude1} and \code{latitude1} are calculated
  (ignoring the value of \code{alongPath}).  However, if they are of the same
  length as \code{longitude1} and \code{latitude1}, then the return value is the
  distance between corresponding (\code{longitude1},\code{latitude1}) and
  (\code{longitude2},\code{latitude2}) values. 

}

\value{Vector of distances in kilometres.}

\seealso{\code{\link{geodXy}}}

\examples{
library(oce)
km <- geodDist(100, 45, 100, 46)
data(section)
geodDist(section)
geodDist(section, alongPath=TRUE)
}

\references{T. Vincenty, "Direct and Inverse Solutions of Ellipsoid on the
  Ellipsoid with Application of Nested Equations", \emph{Survey Review}, April
  1975.  (As of early 2009, this document is available at
  \url{http://www.ngs.noaa.gov/PUBS_LIB/inverse.pdf}.)  }

\author{Dan Kelley based this on R code sent to him by Darren Gillis, who in
  2003 had modified Fortran code that, according to comments in the source, had
  been written in 1974 by L. Pfeifer and J. G. Gergen.}

\keyword{misc}
