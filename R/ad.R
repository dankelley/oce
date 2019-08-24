#' Report Statistics of adp or adv Velocities
#'
#' Report statistics of ADP or ADV velocities, such as means and variance
#' ellipses.
#'
#' @param x an [adp-class] or [adv-class] object.
#'
#' @param control An optional [list] used to specify more information.
#' This is presently ignored for `adv` objects.  For `adp` objects, if
#' `control$bin` is an integer, it is taken as the bin to be selected
#' (otherwise, an average across bins is used).
#'
#' @param \dots additional arguments that are used in the call to
#' [mean()].
#'
#' @return A list containing items the major and minor axes of the covariance
#' ellipse (`ellipseMajor` and `ellipseMinor`), the angle of the
#' major axis anticlockwise of the horizontal axis (`ellipseAngle`), and
#' the x and y components of the mean velocity (`uMean` and `vMean`).
#'
#' @author Dan Kelley
#'
#' @examples
#' library(oce)
#' data(adp)
#' a <- velocityStatistics(adp)
#' print(a)
#' t <- seq(0, 2*pi, length.out=100)
#' theta <- a$ellipseAngle * pi / 180
#' y <- a$ellipseMajor * cos(t) * sin(theta) + a$ellipseMinor * sin(t) * cos(theta)
#' x <- a$ellipseMajor * cos(t) * cos(theta) - a$ellipseMinor * sin(t) * sin(theta)
#' plot(adp, which="uv+ellipse+arrow")
#' lines(x, y, col='blue', lty="dashed", lwd=5)
#' arrows(0, 0, a$uMean, a$vMean, lwd=5, length=1/10, col='blue', lty="dashed")
#'
#' @family things related to adp data
#' @family things related to adv data
velocityStatistics <- function(x, control, ...)
{
    if (inherits(x, "adp")) {
        if (!missing(control) && !is.null(control$bin)) {
            if (control$bin < 1)
                stop("cannot have control$bin less than 1, but got ", control$bin)
            max.bin <- dim(x@data$v)[2]
            if (control$bin > max.bin)
                stop("cannot have control$bin larger than ", max.bin, " but got ", control$bin)
            u <- x@data$v[, control$bin, 1]
            v <- x@data$v[, control$bin, 2]
        } else {
            u <- apply(x@data$v[, , 1], 1, mean, na.rm=TRUE) # depth mean
            v <- apply(x@data$v[, , 2], 1, mean, na.rm=TRUE) # depth mean
        }
    } else if (inherits(x, "adv")) {
        u <- x@data$v[, 1]
        v <- x@data$v[, 2]
    }
    ok <- !is.na(u) & !is.na(v)
    u <- u[ok]
    v <- v[ok]
    e <- eigen(cov(data.frame(u, v)))
    ellipseMajor <- sqrt(e$values[1])
    ellipseMinor <- sqrt(e$values[2])
    ellipseAngle <- atan2(e$vectors[2, 1], e$vectors[1, 1]) * 45 / atan2(1, 1)
    uMean <- mean(u, ...)
    vMean <- mean(v, ...)
    list(ellipseMajor=ellipseMajor, ellipseMinor=ellipseMinor, ellipseAngle=ellipseAngle,
         uMean=uMean, vMean=vMean)
}


#' Change ADV or ADP coordinate systems
#'
#' Convert velocity data from an acoustic-Doppler velocimeter or
#' acoustic-Doppler profiler from one coordinate system to another.
#'
#' @param x an [adp-class] or [adv-class] object.
#'
#' @param \dots extra arguments that are passed on to [beamToXyzAdp()]
#' or [beamToXyzAdv()].
#'
#' @return An object of the same class as `x`, but with velocities
#' in xyz coordinates instead of beam coordinates.
#'
#' @author Dan Kelley
#'
#' @family things related to adp data
#' @family things related to adv data
beamToXyz <- function(x, ...)
{
    if (inherits(x, "adp"))
        beamToXyzAdp(x, ...)
    else if (inherits(x, "adv"))
        beamToXyzAdv(x, ...)
    else
        stop("class of object must inherit from either 'adv' or 'adp'")
}

#' Convert Acoustic-Doppler Data From xyz to enu Coordinates
#'
#' @param x an [adp-class] or [adv-class] object.
#'
#' @param \dots extra arguments that are passed on to [xyzToEnuAdp()]
#' or [xyzToEnuAdv()]; see the documentation for those functions,
#' for the details.
#'
#' @return An object of the same class as `x`, but with velocities
#' in east-north-up coordinates instead of xyz coordinates.
#'
#' @family things related to adp data
#' @family things related to adv data
xyzToEnu <- function(x, ...)
{
    if (inherits(x, "adp"))
        xyzToEnuAdp(x=x, ...)
    else if (inherits(x, "adv"))
        xyzToEnuAdv(x=x, ...)
    else
        stop("class of object must inherit from either 'adv' or 'adp'")
}


#' Rotate acoustic-Doppler data to a new coordinate system
#'
#' @param x an [adp-class] or [adv-class] object.
#'
#' @param \dots extra arguments that are passed on to [enuToOtherAdp()]
#' or [enuToOtherAdv()].
#'
#' @return An object of the same class as `x`, but with velocities
#' in the rotated coordinate system
#'
#' @family things related to adp data
#' @family things related to adv data
enuToOther <- function(x, ...)
{
    if (inherits(x, "adp"))
        enuToOtherAdp(x, ...)
    else if (inherits(x, "adv"))
        enuToOtherAdv(x, ...)
    else
        stop("class of object must inherit from either 'adv' or 'adp'")
}

#' Rotate acoustic-Doppler data to the enu coordinate system
#'
#' @param x an [adp-class] or [adv-class] object.
#'
#' @param \dots extra arguments that are passed on to [toEnuAdp()]
#' or [toEnuAdv()].
#'
#' @return An object of the same class as `x`, but with velocities
#' in the enu coordinate system
#'
#' @family things related to adp data
#' @family things related to adv data
toEnu <- function(x, ...)
{
    if (inherits(x, "adp"))
        toEnuAdp(x, ...)
    else if (inherits(x, "adv"))
        toEnuAdv(x, ...)
    else
        stop("class of object must inherit from either 'adv' or 'adp'")
}
