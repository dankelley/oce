## vim:textwidth=80:expandtab:shiftwidth=4:softtabstop=4

## This is possibly useful, possibly not. The method changed in Oct, 2015.
## See the docs for methodology.
geodXy <- function(lon, lat, lon.ref=0, lat.ref=0, rotate=0)
{
    a <- 6378137.00          # WGS84 major axis
    f <- 1/298.257223563     # WGS84 flattening parameter
    if (missing(lon) || missing(lat)) stop("must provide lon and lat")
    n <- length(lon)
    if (length(lat) != n) stop("lon and lat vectors of unequal length")
    xy  <- .C("geod_xy", as.integer(n), as.double(lat), as.double(lon),
              as.double(lat.ref), as.double(lon.ref), as.double(a), as.double(f),
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

#' inverse geodesic calculation
#'
#' @details The calculation is done by finding a minimum value of a cost
#' function that is the vector difference between (\code{x},\code{y})
#' and the corresponding values returned by \code{\link{geodXy}}. This
#' minimum is calculated in C for speed, using the \code{nmmin} function
#' that is the underpinning for the Nelder-Meade version of the R function
#' \code{\link{optim}}.
#'
#' @param x value of x in metres, as given by \code{\link{geodXy}}
#' @param y value of y in metres, as given by \code{\link{geodXy}}
#' @param lon.ref reference longitude, as supplied to \code{\link{geodXy}}
#' @param lat.ref reference latitude, as supplied to \code{\link{geodXy}}
#' @return a data frame containing \code{longitude} and \code{latitude}
geodXyInverse <- function(x, y, lon.ref=0, lat.ref=0)
{
    a <- 6378137.00          # WGS84 major axis
    f <- 1/298.257223563     # WGS84 flattening parameter
    if (missing(x) || missing(y)) stop("must provide x and y")
    n <- length(x)
    if (length(y) != n) stop("x and y vectors of unequal length")
    ll <- .C("geod_xy_inverse", as.integer(n), as.double(x), as.double(y),
             as.double(lat.ref), as.double(lon.ref), as.double(a), as.double(f),
             longitude = double(n), latitude = double(n), PACKAGE = "oce")
    data.frame(longitude=ll$longitude, latitude=ll$latitude)
}

geodDist <- function (lon1, lat1=NULL, lon2=NULL, lat2=NULL, alongPath=FALSE)
{
    a <- 6378137.00          # WGS84 major axis
    f <- 1/298.257223563     # WGS84 flattening parameter
    if (inherits(lon1, "section")) {
        section <- lon1
        lat <- section[["latitude", "byStation"]]
        lon <- section[["longitude", "byStation"]]
        if (alongPath) {
            res <- .Call("geoddist_alongpath", lat, lon, a, f) / 1000
        } else {
            n <- length(section@data$station)
            res <- .Call("geoddist",
                         as.double(rep(lat[1], length.out=n)), as.double(rep(lon[1], length.out=n)),
                         as.double(lat), as.double(lon),
                         as.double(a), as.double(f), as.double(1), as.double(1),
                         dist = double(n)) / 1000
        }
    } else {
        if (alongPath) {
            res <- .Call("geoddist_alongpath", lat1, lon1, a, f) / 1000
        } else {
            n1 <- length(lat1)
            if (length(lon1) != n1)
                stop("lat1 and lon1 must be vectors of the same length")
            n2 <- length(lat2)
            if (length(lon2) != n2)
                stop("lat2 and lon2 must be vectors of the same length")
            if (n2 < n1) { # copy first (lat2, lon2)
                lat2 <- rep(lat2[1], n1)
                lon2 <- rep(lon2[1], n1)
            } else { # trim
                lat2 <- lat2[1:n1]
                lon2 <- lon2[1:n1]
            }
            res <- .Call("geoddist",
                      as.double(lat1), as.double(lon1),
                      as.double(lat2), as.double(lon2),
                      as.double(a), as.double(f), as.double(1), as.double(1)) / 1000
        }
    }
    res
}

