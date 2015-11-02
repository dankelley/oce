## vim:textwidth=80:expandtab:shiftwidth=4:softtabstop=4

## This is possibly useful, possibly not. The method changed in Oct, 2015.
## See the docs for methodology.
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
#' @param longitudeRef reference longitude, as supplied to \code{\link{geodXy}}
#' @param latitudeRef reference latitude, as supplied to \code{\link{geodXy}}
#' @return a data frame containing \code{longitude} and \code{latitude}
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

