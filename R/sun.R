#' Solar Angle as Function of Space and Time
#'
#' Solar angle as function of space and time.
#'
#' Based on NASA-provided Fortran program, in turn (according to comments in
#' the code) based on "The Astronomical Almanac".
#'
#' @param t time, a POSIXt object (converted to timezone \code{"UTC"},
#' if it is not already in that timezone), or a numeric value that
#' corresponds to such a time.
#' @param longitude observer longitude in degrees east
#' @param latitude observer latitude in degrees north
#' @param useRefraction boolean, set to \code{TRUE} to apply a correction for
#' atmospheric refraction
#' @return A list containing the following.  \item{time}{time}
#' \item{azimuth}{azimuth, in degrees eastward of north, from 0 to 360.  (See
#' diagram below.)} \item{altitude}{altitude, in degrees above the horizon,
#' ranging from -90 to 90.  (See diagram below.)} \item{diameter}{solar
#' diameter, in degrees} \item{distance}{distance to sun, in astronomical
#' units}
#' \if{html}{\figure{starCoords.png options:width=400px}{starCoords.png}}
#'
#' @author Dan Kelley
#' @seealso The equivalent function for the moon is \code{\link{moonAngle}}.
#' @references Based on Fortran code retrieved from
#' ftp://climate1.gsfc.nasa.gov/wiscombe/Solar_Rad/SunAngles/sunae.f on
#' 2009-11-1.  Comments in that code list as references:
#'
#' Michalsky, J., 1988: The Astronomical Almanac's algorithm for approximate
#' solar position (1950-2050), Solar Energy 40, 227-235
#'
#' The Astronomical Almanac, U.S. Gov't Printing Office, Washington, D.C.
#' (published every year).
#'
#' The code comments suggest that the appendix in Michalsky (1988) contains
#' errors, and declares the use of the following formulae in the 1995 version
#' the Almanac: \itemize{ \item p. A12: approximation to sunrise/set times;
#' \item p. B61: solar altitude (AKA elevation) and azimuth; \item p. B62:
#' refraction correction; \item p. C24: mean longitude, mean anomaly, ecliptic
#' longitude, obliquity of ecliptic, right ascension, declination, Earth-Sun
#' distance, angular diameter of Sun; \item p. L2: Greenwich mean sidereal time
#' (ignoring T^2, T^3 terms).  }
#'
#' The code lists authors as Dr. Joe Michalsky and Dr. Lee Harrison (State
#' University of New York), with modifications by Dr. Warren Wiscombe (NASA
#' Goddard Space Flight Center).
#' @examples
#'
#' rise <- as.POSIXct("2011-03-03 06:49:00", tz="UTC") + 4*3600
#' set <- as.POSIXct("2011-03-03 18:04:00", tz="UTC") + 4*3600
#' mismatch <- function(lonlat)
#' {
#'     sunAngle(rise, lonlat[1], lonlat[2])$altitude^2 + sunAngle(set, lonlat[1], lonlat[2])$altitude^2
#' }
#' result <- optim(c(1,1), mismatch)
#' lon.hfx <- (-63.55274)
#' lat.hfx <- 44.65
#' dist <- geodDist(result$par[1], result$par[2], lon.hfx, lat.hfx)
#' cat(sprintf("Infer Halifax latitude %.2f and longitude %.2f; distance mismatch %.0f km",
#'             result$par[2], result$par[1], dist))
#' @family things related to astronomy
sunAngle <- function(t, longitude=0, latitude=0, useRefraction=FALSE)
{
    if (missing(t)) stop("must provide t")
    if (!inherits(t, "POSIXt")) {
        if (is.numeric(t)) {
            tref <- as.POSIXct("2000-01-01 00:00:00", tz="UTC") # arbitrary
            t <- t - as.numeric(tref) + tref
        } else {
            stop("t must be POSIXt or a number corresponding to POSIXt (in UTC)")
        }
    }
    t <- as.POSIXct(t) # so we can get length ... FIXME: silly, I know
    if ("UTC" != attr(as.POSIXct(t[1]), "tzone"))
        attributes(t)$tzone <- "UTC"
    tOrig <- t
    ok <- !is.na(t)
    ntOrig <- length(t)
    nt <- sum(ok)
    t <- t[ok]
    t <- as.POSIXlt(t) # so we can get yday etc ... FIXME: silly, I know
    nlon <- length(longitude)
    nlat <- length(latitude)
    if (nlon != nlat) stop("lengths of longitude and latitude must match")
    if (nlon == 1) {
        longitude <- rep(longitude, nt) # often, give a time vector but just one location
        latitude <- rep(latitude, nt)
    } else {
        if (ntOrig != nlon) stop("lengths of t, latitude and longitude must match, unless last two are of length 1")
    }
    ## need vectors to handle NA
    azOut <- rep(NA, length.out=ntOrig)
    elOut <- rep(NA, length.out=ntOrig)
    soldiaOut <- rep(NA, length.out=ntOrig)
    soldstOut <- rep(NA, length.out=ntOrig)

    ## the code below is derived from fortran code, downloaded 2009-11-1 from
    ## ftp://climate1.gsfc.nasa.gov/wiscombe/Solar_Rad/SunAngles/sunae.f
    t <- as.POSIXlt(t)                 # use this so we can work on hours, etc
    if ("UTC" != attr(as.POSIXct(t[1]), "tzone"))
        stop("t must be in UTC")
    year <- t$year + 1900
    if (any(year < 1950) || any(year > 2050))
        stop("year=", year, " is outside acceptable range")
    day <- t$yday + 1
    if (any(day < 1) || any(day > 366))
        stop("day is not in range 1 to 366")
    hour <- t$hour + t$min / 60 + t$sec / 3600
    if (any(hour < -13) || any(hour > 36))
        stop("hour outside range -13 to 36")
    if (any(latitude <  -90)) {
        warning("latitude(s) trimmed to range -90 to 90")
        latitude[latitude <  -90] <- -90
    }
    if (any(latitude >   90)) {
        warning("latitude(s) trimmed to range -90 to 90")
        latitude[latitude >   90] <-  90
    }
    if (any(longitude < -180)) {
        warning("longitude(s) trimmed to range -180 to 180")
        longitude[longitude < -180] <- -180
    }
    if (any(longitude >  180)) {
        warning("longitude(s) trimmed to range -180 to 180")
        longitude[longitude >  180] <-  180
    }

    delta <- year - 1949
    leap <- delta %/% 4
    ## FIXME: using fortran-style int and mod here; must check for leap-year cases
    jd <- 32916.5 + (delta * 365 + leap + day) + hour / 24
    jd <- jd + ifelse(0 == (year %% 100) & 0 != (year %% 400), 1, 0)
    time <- jd - 51545
    mnlong <- 280.460 + 0.9856474 * time
    mnlong <- mnlong %% 360
    mnlong <- mnlong + ifelse(mnlong < 0, 360, 0)
    mnanom <- 357.528 + 0.9856003 * time
    mnanom <- mnanom %% 360
    mnanom <- mnanom + ifelse(mnanom < 0, 360, 0)
    rpd <- pi / 180
    mnanom <- mnanom * rpd
    eclong <- mnlong + 1.915*sin(mnanom) + 0.020*sin(2 * mnanom)
    eclong <- eclong %% 360
    eclong <- eclong + ifelse (eclong < 0, 360, 0)
    oblqec <- 23.439 - 0.0000004 * time
    eclong <- eclong * rpd
    oblqec <- oblqec * rpd
    num <- cos(oblqec) * sin(eclong)
    den <- cos(eclong)
    ra <- atan(num / den)
    ra <- ra + ifelse(den < 0, pi, ifelse(num < 0, 2 * pi, 0))
    dec <- asin(sin(oblqec) * sin(eclong))
    gmst <- 6.697375 + 0.0657098242 * time + hour
    gmst <- gmst %% 24
    gmst <- gmst + ifelse(gmst < 0, 24, 0)
    lmst <- gmst + longitude/15
    lmst <- lmst %% 24
    lmst <- lmst + ifelse(lmst < 0, 24, 0)
    lmst <- lmst * 15 * rpd
    ha <- lmst - ra
    ha <- ha + ifelse (ha < (-pi), 2 * pi, 0)
    ha <- ha - ifelse (ha > pi, 2 * pi, 0)
    el <- asin(sin(dec) * sin(latitude * rpd) + cos(dec) * cos(latitude*rpd)*cos(ha))
    ## pin the arg to range -1 to 1 (issue 1004)
    sinAz <- -cos(dec) * sin(ha) / cos(el)
    az <- ifelse(sinAz < (-1), -pi/2,
                 ifelse(sinAz > 1, pi/2,
                        asin(sinAz)))
    az <-  ifelse(sin(dec) - sin(el) * sin(latitude * rpd ) > 0,
                  ifelse (sin(az) < 0, az + 2 * pi, az),
                  pi - az)
    el <- el / rpd
    az <- az / rpd
    if (useRefraction) {
        refrac <- ifelse(el >= 19.225,
                         0.00452 * 3.51823 / tan(el * rpd),
                         ifelse (el > (-0.766) & el < 19.225,
                                 3.51823 * (0.1594 + el * (0.0196 + 0.00002 * el)) / (1 + el * (0.505 + 0.0845 * el)),
                                 0))
        el  <- el + refrac
    }
    soldst <- 1.00014 - 0.01671 * cos(mnanom) - 0.00014 * cos(2 * mnanom)
    soldia <- 0.5332 / soldst
    if (is.na(el) || any(el < (-90.0)) || any(el > 90))
        stop("output argument el out of range")
    if (is.na(az) || any(az < 0) || any(az > 360))
        stop("output argument az out of range")
    azOut[ok] <- az
    elOut[ok] <- el
    soldiaOut[ok] <- soldia
    soldstOut[ok] <- soldst
    list(time=tOrig, azimuth=azOut, altitude=elOut, diameter=soldiaOut, distance=soldstOut)
}
