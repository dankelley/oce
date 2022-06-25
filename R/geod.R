# vim:textwidth=80:expandtab:shiftwidth=4:softtabstop=4

#' Convert From Geographical to Geodesic Coordinates
#'
#' The method, which may be useful in determining coordinate systems for a
#' mooring array or a ship transects, calculates (x,y) from distance calculations
#' along geodesic curves.  See \dQuote{Caution}.
#'
#' The calculation is as follows.
#' Consider the `i`-th point in the `longitude` and `latitude`
#' vectors.  To calculate `x[i]`, [geodDist()] is
#' used is to find the distance
#' *along a geodesic curve* connecting (`longitude[i]`, `latitude[i]`) with
#' (`longitudeRef`, `latitude[i]`). The resultant distance
#' is multiplied by -1 if `longitude[i]-longitudeRef` is negative,
#' and the result is assigned to `x[i]`.
#' A similar procedure is used for `y[i]`.
#'
#' @section Caution: This scheme is without known precedent in the literature, and
#' users should read the documentation carefully before deciding to use it.
#'
#' @param longitude,latitude vector of longitude and latitude
#'
#' @param longitudeRef,latitudeRef numeric reference location. Poor results
#' will be returned if these values are not close to the locations described
#' by `longitude` and `latitude`. A sensible approach might be
#' to set `longitudeRef` to `longitude[1]`, etc.
#' @template debugTemplate
#'
#' @return `geodXy` returns a data frame of `x` and `y`,
#' geodesic distance components, measured in metres.
#'
#' @section Change history:
#' On 2015-11-02, the names of the arguments were changed from `lon`, etc., to
#' `longitude`, etc., to be in keeping with other oce functions.
#'
#' On 2017-04-05, four changes were made.
#' 1. Default values of `longitudeRef` and `latitudeRef` were removed,
#' since the old defaults were inappropriate to most work.
#' 2. The argument called `rotate` was eliminated, because it only made
#' sense if the mean resultant x and y were zero.
#' 3. The example was made more useful.
#' 4. Pointers were made to [lonlat2utm()], which may be more useful.
#'
#' @author Dan Kelley
#'
#' @seealso [geodDist()]
#'
#' @examples
#'\donttest{
#' # Develop a transect-based axis system for final data(section) stations
#' library(oce)
#' data(section)
#' lon <- tail(section[["longitude", "byStation"]], 26)
#' lat <- tail(section[["latitude", "byStation"]], 26)
#' lonR <- tail(lon, 1)
#' latR <- tail(lat, 1)
#' data(coastlineWorld)
#' mapPlot(coastlineWorld, projection="+proj=merc",
#'         longitudelim=c(-75,-65), latitudelim=c(35,43), col="gray")
#' mapPoints(lon, lat)
#' XY <- geodXy(lon,lat,mean(lon), mean(lat))
#' angle <- 180/pi*atan(coef(lm(y~x, data=XY))[2])
#' mapCoordinateSystem(lonR, latR, 500, angle, col=2)
#' # Compare UTM calculation
#' UTM <- lonlat2utm(lon, lat, zone=18) # we need to set the zone for this task!
#' angleUTM <- 180/pi*atan(coef(lm(northing~easting, data=UTM))[2])
#' mapCoordinateSystem(lonR, latR, 500, angleUTM, col=3)
#' legend("topright", lwd=1, col=2:3, bg="white", title="Axis Rotation Angle",
#'        legend=c(sprintf("geod: %.1f deg", angle),
#'                 sprintf("utm:  %.1f deg", angleUTM)))
#'}
#' @family functions relating to geodesy
geodXy <- function(longitude, latitude, longitudeRef, latitudeRef, debug=getOption("oceDebug"))
{
    a <- 6378137.00          # WGS84 major axis
    f <- 1/298.257223563     # WGS84 flattening parameter
    if (missing(longitude) || missing(latitude)) stop("must provide longitude and latitude")
    if (missing(longitudeRef)) stop("must provide longitudeRef")
    if (missing(latitudeRef)) stop("must provide latitudeRef")
    n <- length(longitude)
    if (length(latitude) != n) stop("longitude and latitude vectors of unequal length")
    ##xy  <- .C("geod_xy", NAOK=TRUE, PACKAGE="oce",
    xy  <- do_geod_xy(longitude, latitude, longitudeRef, latitudeRef, a, f)
    ## if (rotate != 0) {
    ##     S <- sin(rotate * pi / 180)
    ##     C <- cos(rotate * pi / 180)
    ##     r <- matrix(c(C, S, -S, C), nrow=2)
    ##     rxy <- r %*% rbind(xy$x, xy$y)
    ##     xy$x <- rxy[1, ]
    ##     xy$y <- rxy[2, ]
    ## }
    data.frame(x=xy$x, y=xy$y)
}

#' Inverse Geodesic Calculation
#'
#' The calculation is done by finding a minimum value of a cost
#' function that is the vector difference between (`x`,`y`)
#' and the corresponding values returned by [geodXy()].
#' See \dQuote{Caution}.
#'
#' The minimum is calculated in C for speed, using the `nmmin` function
#' that is the underpinning for the Nelder-Meade version of the R function
#' [optim()]. If you find odd results, try setting `debug=1`
#' and rerunning, to see whether this optimizer is having difficulty
#' finding a minimum of the mismatch function.
#'
#' @param x value of x in metres, as given by [geodXy()]
#'
#' @param y value of y in metres, as given by [geodXy()]
#'
#' @param longitudeRef reference longitude, as supplied to [geodXy()]
#'
#' @param latitudeRef reference latitude, as supplied to [geodXy()]
#'
#' @template debugTemplate
#'
#' @section Caution: This scheme is without known precedent in the literature, and
#' users should read the documentation carefully before deciding to use it.
#'
#' @return a data frame containing `longitude` and `latitude`
#'
#' @family functions relating to geodesy
geodXyInverse <- function(x, y, longitudeRef, latitudeRef, debug=getOption("oceDebug"))
{
    a <- 6378137.00          # WGS84 major axis
    f <- 1/298.257223563     # WGS84 flattening parameter
    if (missing(x) || missing(y)) stop("must provide x and y")
    if (missing(longitudeRef)) stop("must provide longitudeRef")
    if (missing(latitudeRef)) stop("must provide latitudeRef")
    n <- length(x)
    if (length(y) != n) stop("x and y vectors of unequal length")
    ##ll <- .C("geod_xy_inverse", NAOK=TRUE, PACKAGE="oce",
    ll <- do_geod_xy_inverse(x, y, longitudeRef, latitudeRef, a, f)
    data.frame(longitude=ll$longitude, latitude=ll$latitude)
}


#' Compute Geodesic Distance on Surface of Earth
#'
#' This calculates geodesic distance between points on the earth, i.e.
#' distance measured along the (presumed ellipsoidal) surface. The method
#' involves the solution of the geodetic inverse problem, using T. Vincenty's
#' (1975) modification of Rainsford's method with Helmert's elliptical terms.
#'
#' The function may be used in several different ways.
#'
#' Case 1: `longitude1` is a `section` object. The values of
#' `latitude1`, `longitude2`, and `latitude2` arguments are
#' ignored, and the behaviour depends on the value of the `alongPath`
#' argument.  If `alongPath=FALSE`, the return value contains the geodetic
#' distances of each station from the first one.  If `alongPath=TRUE`, the
#' return value is the geodetic distance along the path connecting the
#' stations, in the order in which they are stored in the section.
#'
#' Case 2: `longitude1` is a vector.  If `longitude2` and
#' `latitude2` are not given, then the return value is a vector containing
#' the distances of each point from the first one, *or* the distance
#' along the path connecting the points, according to the value of
#' `alongPath`.  On the other hand, if both `longitude2` and
#' `latitude2` are specified, then the return result depends on the length
#' of these arguments.  If they are each of length 1, then they are taken as a
#' reference point, from which the distances to `longitude1` and
#' `latitude1` are calculated (ignoring the value of `alongPath`).
#' However, if they are of the same length as `longitude1` and
#' `latitude1`, then the return value is the distance between
#' corresponding (`longitude1`,`latitude1`) and
#' (`longitude2`,`latitude2`) values.
#'
#' @param longitude1 longitude or a vector of longitudes, *or* a
#' `section` object, from which longitude and latitude are extracted and
#' used instead of the next three arguments
#'
#' @param latitude1 latitude or vector of latitudes (ignored if
#' `longitude1` is a `section` object)
#'
#' @param longitude2 optional longitude or vector of longitudes (ignored if
#' `alongPath=TRUE`)
#'
#' @param latitude2 optional latitude or vector of latitudes (ignored if
#' `alongPath=TRUE`)
#'
#' @param alongPath boolean indicating whether to compute distance along the
#' path, as opposed to distance from the reference point.  If
#' `alongPath=TRUE`, any values provided for `latitude2` and
#' `longitude2` will be ignored.
#'
#' @return Vector of distances in kilometres.
#'
#' @author Dan Kelley based this on R code sent to him by Darren Gillis, who in
#' 2003 had modified Fortran code that, according to comments in the source,
#' had been written in 1974 by L. Pfeifer and J. G. Gergen.
#'
#' @seealso [geodXy()]
#'
#' @references Vincenty, T. "Direct and Inverse Solutions of Geodesics on the
#' Ellipsoid with Application of Nested Equations." Survey Review 23, no. 176
#' (April 1, 1975): 88–93. https://doi.org/10.1179/sre.1975.23.176.88.
#'
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
        longitude <- section[["longitude", "byStation"]]
        latitude <- section[["latitude", "byStation"]]
        if (alongPath) {
            ##res <- .Call("geoddist_alongpath", latitude, longitude, a, f) / 1000
            res <- do_geoddist_alongpath(longitude, latitude, a, f) / 1000
        } else {
            n <- length(section@data$station)
            ##res <- .Call("geoddist",
            res <- do_geoddist(rep(longitude[1], length.out=n), rep(latitude[1], length.out=n),
                         longitude, latitude, a, f) / 1000
        }
    } else {
        if (alongPath) {
            ##res <- .Call("geoddist_alongpath", latitude1, longitude1, a, f) / 1000
            res <- do_geoddist_alongpath(longitude1, latitude1, a, f) / 1000
        } else {
            n1 <- length(latitude1)
            if (length(longitude1) != n1)
                stop("latitude1 and longitude1 must be vectors of the same length")
            if (is.null(longitude2))
                longitude2 <- longitude1[1]
            if (is.null(latitude2))
                latitude2 <- latitude1[1]
            n2 <- length(latitude2)
            if (length(longitude2) != n2)
                stop("latitude2 and longitude2 must be vectors of the same length")
            if (n2 < n1) {
                ## copy first (latitude2, longitude2)
                latitude2 <- rep(latitude2[1], n1)
                longitude2 <- rep(longitude2[1], n1)
            } else {
                ## trim
                latitude2 <- latitude2[1:n1]
                longitude2 <- longitude2[1:n1]
            }
            ##res <- .Call("geoddist",
            res <- do_geoddist(longitude1, latitude1, longitude2, latitude2, a, f) / 1000
        }
    }
    res
}



#' Great-circle Segments Between Points on Earth
#'
#' Each pair in the `longitude` and `latitude` vectors is considered
#' in turn.  For long vectors, this may be slow.
#'
#' @param longitude vector of longitudes, in degrees east
#'
#' @param latitude vector of latitudes, in degrees north
#'
#' @param dmax maximum angular separation to tolerate between sub-segments, in
#' degrees.
#'
#' @return Data frame of `longitude` and `latitude`.
#'
#' @author Dan Kelley, based on code from Clark Richards, in turn based on
#' formulae provided by Ed Williams (see reference 1)].
#'
#' @references 1. `http://williams.best.vwh.net/avform.htm#Intermediate`
#' (link worked for years but failed 2017-01-16).
#'
#' @examples
#'\donttest{
#' library(oce)
#' data(coastlineWorld)
#' mapPlot(coastlineWorld, type="l",
#'         longitudelim=c(-80,10), latitudelim=c(35,80),
#'         projection="+proj=merc")
#' ## Great circle from New York to Paris (Lindberg's flight)
#' l <- geodGc(c(-73.94,2.35), c(40.67,48.86), 1)
#' mapLines(l$longitude, l$latitude, col='red', lwd=2)
#'}
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
        d <- 2 * asin(sqrt((sin((rlat[i] - rlat[i+1])/2))^2 # nolint (no space before opening parenthesis)
                           + cos(rlat[i]) * cos(rlat[i+1]) * (sin((rlon[i] - rlon[i+1])/2))^2)) # nolint (no space before opening parenthesis)

        ddeg <- d / d2r
        if (ddeg < dmax) {
            lon <- c(lon, longitude[i])
            lat <- c(lat, latitude[i])
        } else {
            f <- seq(0, 1, length.out=ceiling(1 + ddeg/dmax))
            A <- sin((1 - f) * d) / sin(d) # nolint (no space before opening parenthesis)
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
