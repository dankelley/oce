## vim:textwidth=80:expandtab:shiftwidth=4:softtabstop=4


#' Convert From Geographical to Geodesic Coordinates
#' 
#' The method employs geodesic calculations of the distances along geodesic
#' curves, i.e. akin to great-circle curves that go along the surface of the
#' ellipsoidal earth; see \code{\link{geodDist}}. The results are minimally
#' sensitive to the ellipsoidal geometry assumed, but this is not a matter in
#' serious question today. Note that the results are quite unlike the values
#' returned from a map projection; in the latter case, the results vary greatly
#' across a range of popular projections. Use the present function for things
#' like gridding data or calculating drifter speeds.
#' 
#' Consider the \code{i}-th point in the \code{longitude} and \code{latitude}
#' vectors.  The value of \code{x[i]} is inferred from the distance along a
#' geodesic curve from from (\code{longitude[i]}, \code{latitude[i]}) to
#' (\code{longitudeRef[i]}, \code{latitude[i]}), i.e. the distance along a line
#' of constant latitude.  Similarly, \code{y[i]} is inferred the geodesic
#' distance from (\code{longitude[i]}, \code{latitude[i]}) to
#' (\code{longitude[i]}, \code{latitudeRef}). Once the distances are inferred,
#' signs are calculated from determining the sign of
#' \code{longitude[i]-longitudeRef} for \code{x[i]} and similarly \code{y[i]}.
#' 
#' @param longitude vector of longitudes
#' @param latitude vector of latitudes
#' @param longitudeRef numeric, reference longitude
#' @param latitudeRef numeric, reference latitude
#' @param rotate numeric, counterclockwise angle, in degrees, by which to
#' rotate the (\code{x}, \code{y}) coordinates about the reference point.  This
#' is useful in rotating the coordinate system to align with a coastline, a
#' mean current, etc.
#' @return Data frame of \code{x} and \code{y}, geodesic distance components,
#' measured in metres. See \dQuote{Details} for the definitions.
#'
#' @section Change notification: Until 2015-11-02, the names of the arguments
#' were \code{lon}, \code{lat}, \code{lon.ref} and \code{lat.ref}; these were
#' changed to be more in keeping with names in the rest of oce.
#' 
#' @section Caution: The calculation is devised by the author and is without known
#' precedent in the literature, so users might have to explain it in their
#' publications--hence the detailed discussion below.
#' @author Dan Kelley
#' @seealso \code{\link{geodDist}}
#' @examples
#' library(oce)
#' data(section)
#' lon <- section[["longitude", "byStation"]]
#' lat<- section[["latitude", "byStation"]]
#' lon <- lon
#' lat <- lat
#' lonR <- lon[1]
#' latR <- lat[1]
#' ## 1. ellipse
#' km <- 1e3 # nicer for graphs
#' xy <- geodXy(lon, lat, lonR, latR) / km
#' ## 2. sphere, with scale tailored to mean local latitude
#' kmperdeg <- geodDist(0, mean(lat)-0.5, 0, mean(lat)+0.5) # mid-latitude estimate
#' X <- (lon - lonR) * kmperdeg * cos(lat * pi / 180)
#' Y <- (lat - latR) * kmperdeg
#' XY <- list(x=X, y=Y)
#' ## plot, with labels for sphere-ellipse deviations
#' par(mfrow=c(2,1), mar=c(3, 3, 1, 1), mgp=c(2, 0.7, 0))
#' plot(lon, lat, asp=1/cos(median(lat*pi/180)))
#' plot(xy$x, xy$y, asp=1, xlab="x [km]", ylab="y [km]")
#' rms<- function(x) sqrt(mean(x^2))
#' mtext(sprintf("RMS dev.: x %.2f km, y %.2f km",
#'               rms(xy$x-XY$x), rms(xy$y-XY$y)), side=3, line=-1)
#' mtext(sprintf("RMS dev / span: x %.2g, y %.2g",
#'               rms(xy$x-XY$x)/diff(range(xy$x)),
#'               rms(xy$y-XY$y)/diff(range(xy$y))),
#'       side=3, line=-2)
#' 
#' @section Caution: This is possibly useful, possibly not. The method changed in Oct, 2015.
#' @family functions relating to geodesy
geodXy <- function(longitude, latitude, longitudeRef=0, latitudeRef=0, rotate=0)
{
    a <- 6378137.00          # WGS84 major axis
    f <- 1/298.257223563     # WGS84 flattening parameter
    if (missing(longitude) || missing(latitude)) stop("must provide longitude and latitude")
    n <- length(longitude)
    if (length(latitude) != n) stop("longitude and latitude vectors of unequal length")
    xy  <- .C("geod_xy", as.integer(n), as.double(latitude), as.double(longitude),
              as.double(latitudeRef), as.double(longitudeRef), as.double(a), as.double(f),
              x = double(n), y = double(n), PACKAGE = "oce")
    if (rotate != 0) {
        S <- sin(rotate * pi / 180)
        C <- cos(rotate * pi / 180)
        r <- matrix(c(C,S,-S,C),nrow=2)
        rxy <- r %*% rbind(xy$x, xy$y)
        xy$x <- rxy[1,]
        xy$y <- rxy[2,]
    }
    data.frame(x=xy$x, y=xy$y)
}

#' Inverse geodesic calculation
#'
#' The calculation is done by finding a minimum value of a cost
#' function that is the vector difference between (\code{x},\code{y})
#' and the corresponding values returned by \code{\link{geodXy}}. This
#' minimum is calculated in C for speed, using the \code{nmmin} function
#' that is the underpinning for the Nelder-Meade version of the R function
#' \code{\link{optim}}.
#'
#' @param x value of x in metres, as given by \code{\link{geodXy}}
#' @param y value of y in metres, as given by \code{\link{geodXy}}
#' @param longitudeRef reference longitude, as supplied to \code{\link{geodXy}}
#' @param latitudeRef reference latitude, as supplied to \code{\link{geodXy}}
#' @return a data frame containing \code{longitude} and \code{latitude}
#' @family functions relating to geodesy
geodXyInverse <- function(x, y, longitudeRef=0, latitudeRef=0)
{
    a <- 6378137.00          # WGS84 major axis
    f <- 1/298.257223563     # WGS84 flattening parameter
    if (missing(x) || missing(y)) stop("must provide x and y")
    n <- length(x)
    if (length(y) != n) stop("x and y vectors of unequal length")
    ll <- .C("geod_xy_inverse", as.integer(n), as.double(x), as.double(y),
             as.double(latitudeRef), as.double(longitudeRef), as.double(a), as.double(f),
             longitude = double(n), latitude = double(n), PACKAGE = "oce")
    data.frame(longitude=ll$longitude, latitude=ll$latitude)
}


#' Compute geodesic distance on surface of earth.
#' 
#' This calculates geodesic distance between points on the earth, i.e.
#' distance measured along the (presumed ellipsoidal) surface. The method
#' involves the solution of the geodetic inverse problem, using T. Vincenty's
#' modification of Rainsford's method with Helmert's elliptical terms.
#' 
#' The function may be used in several different ways.
#' 
#' Case 1: \code{longitude1} is a \code{section} object. The values of
#' \code{latitude1}, \code{longitude2}, and \code{latitude2} arguments are
#' ignored, and the behaviour depends on the value of the \code{alongPath}
#' argument.  If \code{alongPath=FALSE}, the return value contains the geodetic
#' distances of each station from the first one.  If \code{alongPath=TRUE}, the
#' return value is the geodetic distance along the path connecting the
#' stations, in the order in which they are stored in the section.
#' 
#' Case 2: \code{longitude1} is a vector.  If \code{longitude2} and
#' \code{latitude2} are not given, then the return value is a vector containing
#' the distances of each point from the first one, \emph{or} the distancce
#' along the path connecting the points, according to the value of
#' \code{alongPath}.  On the other hand, if both \code{longitude2} and
#' \code{latitude2} are specified, then the return result depends on the length
#' of these arguments.  If they are each of length 1, then they are taken as a
#' reference point, from which the distances to \code{longitude1} and
#' \code{latitude1} are calculated (ignoring the value of \code{alongPath}).
#' However, if they are of the same length as \code{longitude1} and
#' \code{latitude1}, then the return value is the distance between
#' corresponding (\code{longitude1},\code{latitude1}) and
#' (\code{longitude2},\code{latitude2}) values.
#' 
#' @param longitude1 longitude or a vector of longitudes, \strong{or} a
#' \code{section} object, from which longitude and latitude are extracted and
#' used instead of the next three arguments
#' @param latitude1 latitude or vector of latitudes (ignored if
#' \code{longitude1} is a \code{section} object)
#' @param longitude2 optional longitude or vector of longitudes (ignored if
#' \code{alongPath=TRUE})
#' @param latitude2 optional latitude or vector of latitudes (ignored if
#' \code{alongPath=TRUE})
#' @param alongPath boolean indicating whether to compute distance along the
#' path, as opposed to distance from the reference point.  If
#' \code{alongPath=TRUE}, any values provided for \code{latitude2} and
#' \code{longitude2} will be ignored.
#' @return Vector of distances in kilometres.
#' @author Dan Kelley based this on R code sent to him by Darren Gillis, who in
#' 2003 had modified Fortran code that, according to comments in the source,
#' had been written in 1974 by L. Pfeifer and J. G. Gergen.
#' @seealso \code{\link{geodXy}}
#' @references T. Vincenty, "Direct and Inverse Solutions of Ellipsoid on the
#' Ellipsoid with Application of Nested Equations", \emph{Survey Review}, April
#' 1975.  (As of early 2009, this document is available at
#' \url{http://www.ngs.noaa.gov/PUBS_LIB/inverse.pdf}.)
#' @examples
#' library(oce)
#' km <- geodDist(100, 45, 100, 46)
#' data(section)
#' geodDist(section)
#' geodDist(section, alongPath=TRUE)
#' 
#' @family functions relating to geodesy
geodDist <- function (longitude1, latitude1=NULL, longitude2=NULL, latitude2=NULL, alongPath=FALSE)
{
    a <- 6378137.00          # WGS84 major axis
    f <- 1/298.257223563     # WGS84 flattening parameter
    if (inherits(longitude1, "section")) {
        section <- longitude1
        latitude <- section[["latitude", "byStation"]]
        longitude <- section[["longitude", "byStation"]]
        if (alongPath) {
            res <- .Call("geoddist_alongpath", latitude, longitude, a, f) / 1000
        } else {
            n <- length(section@data$station)
            res <- .Call("geoddist",
                         as.double(rep(latitude[1], length.out=n)),
                         as.double(rep(longitude[1], length.out=n)),
                         as.double(latitude), as.double(longitude),
                         as.double(a), as.double(f)) / 1000
        }
    } else {
        if (alongPath) {
            res <- .Call("geoddist_alongpath", latitude1, longitude1, a, f) / 1000
        } else {
            n1 <- length(latitude1)
            if (length(longitude1) != n1)
                stop("latitude1 and longitude1 must be vectors of the same length")
            n2 <- length(latitude2)
            if (length(longitude2) != n2)
                stop("latitude2 and longitude2 must be vectors of the same length")
            if (n2 < n1) { # copy first (latitude2, longitude2)
                latitude2 <- rep(latitude2[1], n1)
                longitude2 <- rep(longitude2[1], n1)
            } else { # trim
                latitude2 <- latitude2[1:n1]
                longitude2 <- longitude2[1:n1]
            }
            res <- .Call("geoddist",
                      as.double(latitude1), as.double(longitude1),
                      as.double(latitude2), as.double(longitude2),
                      as.double(a), as.double(f)) / 1000
        }
    }
    res
}



#' Great-circle segments between points on earth
#' 
#' Each pair in the \code{longitude} and \code{latitude} vectors is considered
#' in turn.  For long vectors, this may be slow.
#' 
#' @param longitude vector of longitudes, in degrees east
#' @param latitude vector of latitudes, in degrees north
#' @param dmax maximum angular separation to tolerate between sub-segments, in
#' degrees.
#' @return Data frame of \code{longitude} and \code{latitude}.
#' @author Dan Kelley, based on code from Clark Richards, in turn based on
#' formulae provided by Ed Williams [1].
#' @references 1. \url{http://williams.best.vwh.net/avform.htm#Intermediate}
#' @examples
#' \dontrun{
#' library(oce)
#' data(coastlineWorld)
#' mapPlot(coastlineWorld, type='l',
#'         longitudelim=c(-80,10), latitudelim=c(35,80),
#'         projection="+proj=ortho", orientation=c(35, -35, 0))
#' ## Great circle from New York to Paris (Lindberg's flight)
#' l <- geodGc(c(-73.94,2.35), c(40.67,48.86), 1) 
#' mapLines(l$longitude, l$latitude, col='red', lwd=2)
#' }
#' 
#' @family functions relating to geodesy
geodGc <- function(longitude, latitude, dmax)
{
    n <- length(latitude)
    if (n != length(longitude))
        stop("lengths of longitude and latude must match")
    d2r <- atan2(1, 1) / 45
    rlat <- d2r * latitude
    rlon <- d2r * longitude
    lon <- NULL
    lat <- NULL
    ## FIXME: if this is slow, may need to use C
    for (i in seq.int(1, n-1)) {
        d <- 2 * asin(sqrt((sin((rlat[i] - rlat[i+1])/2))^2
                           + cos(rlat[i]) * cos(rlat[i+1]) * (sin((rlon[i] - rlon[i+1])/2))^2))
        ddeg <- d / d2r
        if (ddeg < dmax) {
            lon <- c(lon, longitude[i])
            lat <- c(lat, latitude[i])
        } else {
            f <- seq(0, 1, length.out=ceiling(1 + ddeg/dmax))
            A <- sin((1 - f) * d) / sin(d)
            B <- sin(f * d) / sin(d)
            x <- A * cos(rlat[i]) * cos(rlon[i]) + B * cos(rlat[i+1]) * cos(rlon[i+1])
            y <- A * cos(rlat[i]) * sin(rlon[i]) + B * cos(rlat[i+1]) * sin(rlon[i+1])
            z <- A * sin(rlat[i])              + B * sin(rlat[i+1])
            lat <- atan2(z, sqrt(x^2+y^2)) / d2r
            lon <- atan2(y, x) / d2r
        }
    }
    lon <- c(lon, longitude[n])
    lat <- c(lat, latitude[n])
    ## use range 0 to 360 if input longitudes in that way
    if (any(longitude > 180))
        lon <- ifelse(lon < 0, lon+360, lon)
    list(longitude=lon, latitude=lat)
}


