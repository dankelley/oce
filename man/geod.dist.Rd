\name{geod.dist}
\alias{geod.dist}
\title{Geodesic distance on earth}
\description{
  Compute geodesic distance on surface of earth.
}
\usage{
  km <- geod.dist(lat1, lon1, lat2, lon2);
}
\arguments{
  \item{lat1}{latitude or a list of latitudes}
  \item{lon1}{longitude or list of longitudes}
  \item{lat2}{latitude or list of latitudes}
  \item{lon2}{longitude or list of longitudes}
}
\details{
Solution of the geodetic inverse problem after T.Vincenty
modified Rainsford's method with Helmert's elliptical terms
effective in any azimuth and at any distance short of antipodal
standpoint/forepoint must not be the geographic pole. 

If the "1" and "2" lists are of equal length, then the result is
the pairwise distances.  However, if the length of "2" is shorter 
than the length of "1", then \emph{only} the first value in the
"2" list is used, repeated over and over to match the length of
"1".

A common use is for "1" to contain a vector of positions 
along a cruise track, and for "2" to contain a reference point; 
e.g. geod.dist(lats,lons,lats[1],lons[1]) gives distance
along the track starting from zero.

}
\value{
  Distance between "1" and "2" in metres, measured along the surface of
  the earth.  If "1" is a vector, the returned value is a vector of the same length.
}

\seealso{
  N/A.
}
\examples{
 # There are roughly 111km per degree of latitude
 km <- geod.dist(45, 100, 46, 100)/1000 
}
\references{}
\author{Dan Kelley \email{Dan.Kelley@Dal.Ca} packaged this, based on R code sent to him by Darren Gillis, who in 2003 had modified Fortran code written in 1974 by (according to comment cards in the source) L. Pfeifer and J. G. Gergen.}
\keyword{misc}
