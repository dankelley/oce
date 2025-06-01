# vim:textwidth=80:expandtab:shiftwidth=4:softtabstop=4

#' Earth Magnetic Declination, Inclination, and Intensity
#'
#' Implements the 12th, 13th and 145h generations of the International
#' Geomagnetic Reference Field (IGRF), based on a reworked version of a Fortran
#' code downloaded from a NOAA website (see \dQuote{References}).
#'
#' The Fortran code (subroutines `igrf12syn`, `igrf13syn` and `igrf14syn`) seem
#' to have been written by Susan Macmillan of the British Geological Survey.
#' Comments in the source code `igrf14syn` (the current default used here)
#' indicate that its coefficients were agreed to in November 2024 by the IAGA
#' Working Group V-MOD.  Other comments in that code suggest that the proposed
#' application time interval is from years 1900 to 2030, inclusive, but that
#' only dates from 1945 to 2020 are to be considered definitive.
#'
#' @param longitude longitude in degrees east (negative for degrees west), as a
#' number, a vector, or a matrix.
#'
#' @param latitude latitude in degrees north, as a number, vector, or matrix.
#' The shape (length or dimensions) must conform to the dimensions of `longitude`.
#'
#' @param time The time at which the field is desired. This may be a
#' single value or a vector or matrix that is structured to match
#' `longitude` and `latitude`. The value may a decimal year,
#' a POSIXt time, or a Date time.
#'
#' @param version an integer that must be 12, 13 or 14, to specify
#' the version number of the formulae. Note that 13 became the default
#' on 2020 March 3 and that 14 became the default on 2025 January 7.
#'
#' @return A list containing `declination`, `inclination`, and
#' `intensity`.
#'
#' @author Dan Kelley wrote the R code and a fortran wrapper to the `igrf1X.f`
#' subroutine (where X can be 12, 13 or 14), which was written by Susan
#' Macmillan of the British Geological Survey and distributed ``without
#' limitation'' (email from SM to DK dated June 5, 2015).  This version was
#' updated subsequent to that date; see \dQuote{Historical Notes}.
#'
## @section Historical Notes:
## For about a decade, `magneticField` used the version 12 formulae provided
## by IAGA, but the code was updated on March 3, 2020, to version 13.  Example
## 3 shows that the differences in declination are typically under 2 degrees
## (with 95 percent of the data lying between -1.7 and 0.7 degrees).
#'
#' @references
#'
#' 1. The underlying Fortran code for version 12 is from `igrf12.f`, downloaded
#'    the NOAA website `https://www.ngdc.noaa.gov/IAGA/vmod/igrf.html` on June
#'    7, 2015. For version 13, `igrf13.f` was downloaded from the NOAA website
#'    `https://www.ngdc.noaa.gov/IAGA/vmod/igrf.html` on March 3, 2020. For
#'    version 14, `igrf14.f` was downloaded from the NOAA website
#'    `https://www.ngdc.noaa.gov/IAGA/vmod/igrf14.f` on January 7, 2025.
#'
#' 2. Witze, Alexandra. \dQuote{Earth's Magnetic Field Is Acting up and
#'    Geologists Don't Know Why.} Nature 565 (January 9, 2019): 143.
#'    \doi{10.1038/d41586-019-00007-1}
#'
#' 3. Alken, P., E. Thébault, C. D. Beggan, H. Amit, J. Aubert, J. Baerenzung,
#'    T. N. Bondar, et al. "International Geomagnetic Reference Field: The
#'    Thirteenth Generation." Earth, Planets and Space 73, no. 1 (December
#'    2021): 49. \doi{10.1186/s40623-020-01288-x}.
#'
#' @examples
#' library(oce)
#' # 1. Today's value at Halifax NS
#' magneticField(-(63 + 36 / 60), 44 + 39 / 60, Sys.Date())
#'
#' # 2. World map of declination in year 2025.
#' \donttest{
#' data(coastlineWorld)
#' par(mar = rep(0.5, 4)) # no axes on whole-world projection
#' mapPlot(coastlineWorld, projection = "+proj=robin", col = "lightgray")
#' # Construct matrix holding declination
#' lon <- seq(-180, 180)
#' lat <- seq(-90, 90)
#' dec2025 <- function(lon, lat) {
#'     magneticField(lon, lat, 2025)$declination
#' }
#' dec <- outer(lon, lat, dec2025) # hint: outer() is very handy!
#' # Contour, unlabelled for small increments, labeled for
#' # larger increments.
#' mapContour(lon, lat, dec,
#'     col = "blue", levels = seq(-180, -5, 5),
#'     lty = 3, drawlabels = FALSE
#' )
#' mapContour(lon, lat, dec, col = "blue", levels = seq(-180, -20, 20))
#' mapContour(lon, lat, dec,
#'     col = "red", levels = seq(5, 180, 5),
#'     lty = 3, drawlabels = FALSE
#' )
#' mapContour(lon, lat, dec, col = "red", levels = seq(20, 180, 20))
#' mapContour(lon, lat, dec, levels = 180, col = "black", lwd = 2, drawlabels = FALSE)
#' mapContour(lon, lat, dec, levels = 0, col = "black", lwd = 2)
#' }
#'
#' # 3. Declination differences between versions 13 and 14
#' lon <- seq(-180, 180)
#' lat <- seq(-90, 90)
#' decDiff <- function(lon, lat) {
#'     old <- magneticField(lon, lat, 2025, version = 13)$declination
#'     new <- magneticField(lon, lat, 2025, version = 14)$declination
#'     new - old
#' }
#' decDiff <- outer(lon, lat, decDiff)
#' t.test(decDiff)
#' @family things related to magnetism
magneticField <- function(longitude, latitude, time, version = 14) {
    if (missing(longitude) || missing(latitude) || missing(time)) {
        stop("must provide longitude, latitude, and time")
    }
    dim <- dim(latitude)
    if (!all(dim == dim(longitude))) {
        stop("dimensions of longitude and latitude must agree")
    }
    n <- length(latitude)
    if (inherits(time, "Date")) {
        time <- as.POSIXct(time, tz = "UTC")
    }
    if (inherits(time, "POSIXt")) {
        d <- as.POSIXlt(time, tz = "UTC")
        year <- d$year + 1900
        yearday <- d$yday
        time <- year + yearday / 365.25 # ignore leap year issue (formulae not daily)
    }
    if (length(time) == 1) {
        time <- rep(time, n)
    } else {
        if (!all(dim == dim(time))) {
            stop("dimensions of latitude and time must agree")
        }
    }
    if (!is.null(dim)) {
        dim(longitude) <- n
        dim(latitude) <- n
        dim(time) <- n
    }
    # isv <- 0
    # itype <- 1                          # geodetic
    # alt <- 0.0                          # altitude in km
    elong <- ifelse(longitude < 0, 360 + longitude, longitude)
    colat <- 90 - latitude
    iversion <- as.integer(version)
    if (!(iversion %in% c(12L, 13L, 14L))) {
        stop("version must be 12, 13 or 14, but it is ", iversion)
    }
    # message("time:", time, " (", as.numeric(time), ")")
    r <- .Fortran("md_driver",
        as.double(colat), as.double(elong), as.double(time),
        as.integer(n),
        declination = double(n),
        inclination = double(n),
        intensity = double(n),
        as.integer(iversion)
    )
    declination <- r$declination
    inclination <- r$inclination
    intensity <- r$intensity
    if (!is.null(dim)) {
        dim(declination) <- dim
        dim(inclination) <- dim
        dim(intensity) <- dim
    }
    list(declination = declination, inclination = inclination, intensity = intensity)
}
