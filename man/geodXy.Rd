% vim:textwidth=80:expandtab:shiftwidth=2:softtabstop=2
\name{geodXy}
\alias{geodXy}
\title{Convert lon/lat to x/y on earth}
\description{Convert lon/lat to x/y on earth}

\usage{geodXy(lon, lat, lon.ref, lat.ref, rotate=0)}

\arguments{
  \item{lon}{vector of longitudes}
  \item{lat}{vector of latitudes}
  \item{lon.ref}{numeric, reference longitude}
  \item{lat.ref}{numeric, reference latitude}
  \item{rotate}{numeric, counterclockwise angle, in degrees, by which to
    rotate the (\code{x}, \code{y}) coordinates about the reference
    point.  This is useful in rotating the coordinate system to align
    with a coastline, a mean current, etc.}
}

\details{Be aware that the calculation is devised by the author and is without
  known precedence in the literature, so users might have to explain it in their
  publications, perhaps by referring to Kelley (2015).

  The method employs geodesic calculations of the distances along geodesic
  curves, i.e. akin to great-circle curves that go along the surface of the
  ellipsoidal earth; see \code{\link{geodDist}}. The results are minimally
  sensitive to the ellipsoidal geometry assumed, but this is not a matter in
  serious question today. Note that the results are quite unlike the values
  returned from a map projection; in the latter case, the results vary greatly
  across a range of popular projections. Use the present function for things
  like gridding data or calculating drifter speeds.
 
  Consider the \code{i}-th point in the \code{lon} and \code{lat} vectors.  The
  value of \code{x[i]} is inferred from the distance along a geodesic curve from
  from (\code{lon[i]}, \code{lat[i]}) to (\code{lon.ref[i]}, \code{lat[i]}),
  i.e. the distance along a line of constant latitude.  Similarly, \code{y[i]}
  is inferred the geodesic distance from (\code{lon[i]}, \code{lat[i]}) to
    (\code{lon[i]}, \code{lat.ref}). Once the distances are inferred, signs are
    calculated from determining the sign of \code{lon[i]-lon.ref} for
    \code{x[i]} and similarly \code{lat[i]-lat.ref} for \code{y[i]}.

    The bottom panel of example shows that the difference between these
    ellipse-based calculations and sphere-based calculations tends to be rather
    small (although the stations are nearly along a line of latitude, so the
    locally-sized sphere is expected to work well).

}

\value{Data frame of \code{x} and \code{y}, geodesic distance components,
  measured in metres. See \dQuote{Details} for the definitions.}

\seealso{\code{\link{geodDist}}}

\examples{
library(oce)

data(section)
lon <- section[["longitude", "byStation"]]
lat<- section[["latitude", "byStation"]]
lon <- lon
lat <- lat
lonR <- lon[1]
latR <- lat[1]
## 1. ellipse
km <- 1e3 # nicer for graphs
xy <- geodXy(lon, lat, lonR, latR) / km
## 2. sphere, with scale tailored to mean local latitude
kmperdeg <- geodDist(0, mean(lat)-0.5, 0, mean(lat)+0.5) # mid-latitude estimate
X <- (lon - lonR) * kmperdeg * cos(lat * pi / 180)
Y <- (lat - latR) * kmperdeg
XY <- list(x=X, y=Y)
## plot, with labels for sphere-ellipse deviations
par(mfrow=c(2,1), mar=c(3, 3, 1, 1), mgp=c(2, 0.7, 0))
plot(lon, lat, asp=1/cos(median(lat*pi/180)))
plot(xy$x, xy$y, asp=1, xlab="x [km]", ylab="y [km]")
rms <- function(x) sqrt(mean(x^2))
mtext(sprintf("RMS dev.: x \%.2f km, y \%.2f km",
              rms(xy$x-XY$x), rms(xy$y-XY$y)), side=3, line=-1)
mtext(sprintf("RMS dev / span: x \%.2g, y \%.2g",
              rms(xy$x-XY$x)/diff(range(xy$x)),
              rms(xy$y-XY$y)/diff(range(xy$y))),
      side=3, line=-2)
}

\author{Dan Kelley}

\references{Dan Kelley, 2015. Oceanographic Analysis with R. To be submitted to
  Springer-Verlag.}

\keyword{misc}

