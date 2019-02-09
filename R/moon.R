#' Convert ecliptical to equatorial coordinate
#'
#' Convert from ecliptical to equatorial coordinates, using
#' equations 8.3 and 8.4 of [1], or, equivalently,
#' equations 12.3 and 12.4 of [2].
#'
#' @param lambda longitude, in degrees, or a data frame containing
#' \code{lambda}, \code{beta}, and \code{epsilon}, in which case the next to
#' arguments are ignored.
#' @param beta geocentric latitude, in degrees
#' @param epsilon obliquity of the ecliptic, in degrees
#' @return A data frame containing columns \code{rightAscension} and
#' \code{declination} both in degrees.
#' @author Dan Kelley, based on formulae in [1] and [2].
#' @references 1. Meeus, Jean, 1982. Astronomical formulae for Calculators.
#' Willmann-Bell. Richmond VA, USA. 201 pages.
#'
#' 2. Meeus, Jean, 1991. Astronomical algorithms.  Willmann-Bell, Richmond VA,
#' USA. 429 pages.
#' The code is based on [1]; see help(moonAngle,"oce") for comments on
#' the differences in formulae found in [2].  Indeed, [2] is only cited
#' here in case readers want to check the ideas of the formulae; DK
#' has found that [2] is available to him via his university library
#' inter-library loan system, whereas he owns a copy of [1].
#' @family things related to astronomy
eclipticalToEquatorial <- function(lambda, beta, epsilon)
{
    if (is.data.frame(lambda)) {
        beta <- lambda$beta
        epsilon <- lambda$epsilon
        lambda <- lambda$lambda
    }
    RPD <- atan2(1, 1) / 45            # radians per degree
    alpha <- atan2(sin(RPD * lambda) * cos(RPD * epsilon) - tan(RPD * beta) * sin(RPD * epsilon), cos(RPD * lambda))
    delta <- asin(sin(RPD * beta) * cos(RPD * epsilon) + cos(RPD * beta) * sin(RPD * epsilon) * sin(RPD * lambda))
    data.frame(rightAscension=alpha/RPD, declination=delta/RPD)
}


#' Convert equatorial to local horizontal coordinate
#'
#' Convert from equatorial coordinates to local horizontal coordinates, i.e.
#' azimuth and altitude.
#' The method is taken from equations 8.5 and 8.6 of [1], or, equivalently,
#' from equations 12.5 and 12.6 of [2].
#'
#' @param rightAscension right ascension, e.g. calculated with
#' \code{\link{eclipticalToEquatorial}}.
#' @param declination declination, e.g. calculated with
#' \code{\link{eclipticalToEquatorial}}.
#' @param t time of observation.
#' @param longitude longitude of observation, positive in eastern hemisphere.
#' @param latitude latitude of observation, positive in northern hemisphere.
#' @return A data frame containing columns \code{altitude} (angle above
#' horizon, in degrees) and \code{azimuth} (angle anticlockwise from south, in
#' degrees).
#' @author Dan Kelley, based on formulae in [1] and [2].
#' @references
#' 1. Meeus, Jean, 1982. Astronomical formulae for Calculators.
#' Willmann-Bell. Richmond VA, USA. 201 pages.
#'
#' 2. Meeus, Jean, 1991. Astronomical algorithms.  Willmann-Bell, Richmond VA,
#' USA. 429 pages.
#' @family things related to astronomy
equatorialToLocalHorizontal <- function(rightAscension, declination, t, longitude, latitude)
{
    RPD <- atan2(1, 1) / 45            # radians per degree
    ## sidereal Greenwich time (in hours)
    theta0 <- siderealTime(t)
    H <- theta0 * 15 + longitude - rightAscension
    ## Local horizontal coordinates; see [1] eq 8.5 and 8.6 or [2] eq 12.5 and 12.6
    A <- atan2(sin(RPD * H), cos(RPD * H) * sin(RPD * latitude) - tan(RPD * declination) * cos(RPD * latitude))
    ## the atan2() form matches websites on azimuth at Halifax in April 2012
    h <- asin(sin(RPD * latitude) * sin(RPD * declination) + cos(RPD * latitude) * cos(RPD * declination) * cos(RPD * H))
    data.frame(azimuth=A/RPD, altitude=h/RPD)
}


#' Convert a POSIXt time to a sidereal time
#'
#' Convert a POSIXt time to a sidereal time, using
#' the method in Chapter 7 of Meeus (1982).  The small correction
#' that he discusses after his equation 7.1 is not applied here.
#'
#' @param t a time, in POSIXt format, e.g. as created by
#' \code{\link{as.POSIXct}}, \code{\link{as.POSIXlt}}, or
#' \code{\link{numberAsPOSIXct}}.  If this is provided, the other arguments are
#' ignored.
#' @return A sidereal time, in hours in the range from 0 to 24.
#' @author Dan Kelley
#' @references Meeus, Jean, 1982.  Astronomical formulae for Calculators.
#' Willmann-Bell. Richmond VA, USA. 201 pages
#' @examples
#'
#' t <- ISOdatetime(1978, 11, 13, 0, 0, 0, tz="UTC")
#' print(siderealTime(t))
#'
#' @family things related to astronomy
siderealTime <- function(t)
{
    tt <- as.POSIXlt(t)
    n <- length(tt$hour)
    tt$hour <- rep(0, n)
    tt$min <- rep(0, n)
    tt$sec <- rep(0, n)
    jd <- julianDay(t)
    jd0 <- julianDay(tt)
    T <- (jd0 - 2415020.0) / 36525      # [1] eq 7.1 (different in [2])
    hoursLeftOver <- 24 * (jd - jd0)
    res <- 6.6460656 + 2400.051262 * T + 0.00002581 * T * T
    res <- res + 1.002737908 * hoursLeftOver
    res <- res %% 24
    res
}



#' Convert a POSIXt time to a Julian day
#'
#' Convert a POSIXt time to a Julian day, using the method provided in
#' Chapter 3 of Meeus (1982).  It should be noted that
#' Meeus and other astronomical treatments use fractional days, whereas the
#' present code follows the R convention of specifying days in whole numbers,
#' with hours, minutes, and seconds also provided as necessary.  Conversion is
#' simple, as illustrated in the example for 1977 April 26.4, for which Meeus
#' calculates julian day 2443259.9.  Note that the R documentation for
#' \code{\link{julian}} suggests another formula, but the point of the present
#' function is to match the other Meeus formulae, so that suggestion is ignored
#' here.
#'
#' @param t a time, in POSIXt format, e.g. as created by
#' \code{\link{as.POSIXct}}, \code{\link{as.POSIXlt}}, or
#' \code{\link{numberAsPOSIXct}}.  If this is provided, the other arguments are
#' ignored.
#' @param year year, to be provided along with \code{month}, etc., if \code{t}
#' is not provided.
#' @param month month, numbered with January being 1.
#' @param day day in month, starting at 1.
#' @param hour hour of day.
#' @param min minute of hour
#' @param sec second of hour
#' @param tz timezone
#' @return A Julian-Day number, in astronomical convention as explained in
#' Meeus.
#' @author Dan Kelley
#' @references Meeus, Jean, 1982.  Astronomical formulae for Calculators.
#' Willmann-Bell. Richmond VA, USA. 201 pages
#' @examples
#'
#' t <- ISOdatetime(1977, 4, 26, hour=0, min=0, sec=0, tz="UTC")+0.4*86400
#' expect_equal(julianDay(t), 2443259.9) # example from Meeus
#'
#' @family things related to astronomy
#' @family things related to time
julianDay <- function(t, year=NA, month=NA, day=NA, hour=NA, min=NA, sec=NA, tz="UTC")
{
    if (!inherits(t, "POSIXt"))  {
        if (is.na(year) || is.na(month) || is.na(day) || is.na(hour)
            || is.na(min) || is.na(sec))
            stop("must supply year, month, day, hour, min, sec, and tz")
        t <- ISOdatetime(year, month, day, hour, min, sec, tz=tz)
    }
    tt <- as.POSIXlt(t, tz=tz)
    year <- tt$year + 1900
    month <- tt$mon + 1
    day <- tt$mday + (tt$hour + tt$min / 60 + tt$sec / 3600) / 24
    m <- ifelse(month <= 2, month + 12, month)
    y <- ifelse(month <= 2, year - 1, year)
    A <- floor(y / 100)
    B <- 2 - A + floor(A / 4)
    jd <- floor(365.25 * y) + floor(30.6001 * (m + 1)) + day + 1720994.5
    ## correct for Gregorian calendar
    jd <- ifelse(tt > ISOdatetime(1582, 10, 15, 0, 0, 0), jd + B, jd)
    jd
}



#' Julian-Day number to Julian century
#'
#' Convert a Julian-Day number to a time in julian centuries since noon on
#' January 1, 1900.
#' The method follows Meese (1982 equation 15.1).  The example reproduces the
#' example provided by Meeuse (1982 example 15.a), with fractional error 3e-8.
#'
#' @param jd a julian day number, e.g. as given by \code{\link{julianDay}}.
#' @return Julian century since noon on January 1, 1900.
#' @author Dan Kelley
#' @references Meeus, Jean, 1982.  Astronomical formulae for Calculators.
#' Willmann-Bell. Richmond VA, USA. 201 pages
#' @examples
#'
#' t <- ISOdatetime(1978, 11, 13, 4, 35, 0, tz="UTC")
#' jca <- julianCenturyAnomaly(julianDay(t))
#' cat(format(t), "is Julian Century anomaly", format(jca, digits=8), "\n")
#'
#' @family things related to astronomy
#' @family things related to time
julianCenturyAnomaly <- function(jd)
{
    (jd - 2415020.0) / 36525         # [1] Meeus 1982 (eq 7.1 or 15.1)
}


#' Lunar Angle as Function of Space and Time
#'
#' The calculations are based on formulae provided by Meeus [1982], primarily
#' in chapters 6, 18, and 30.  The first step is to compute sidereal time as
#' formulated in Meeus [1982] chapter 7, which in turn uses Julian day computed
#' according to as formulae in Meeus [1982] chapter 3.  Using these quantities,
#' formulae in Meeus [1982] chapter 30 are then used to compute geocentric
#' longitude (\eqn{lambda}{lambda}, in the Meeus notation), geocentric latitude
#' (\eqn{beta}{beta}), and parallax.  Then the \code{obliquity} of the ecliptic
#' is computed with Meeus [1982] equation 18.4.  Equatorial coordinates (right
#' ascension and declination) are computed with equations 8.3 and 8.4 from
#' Meeus [1982], using \code{\link{eclipticalToEquatorial}}.  The hour angle
#' (\eqn{H}{H}) is computed using the unnumbered equation preceding Meeus's
#' [1982] equation 8.1.  Finally, Meeus [1982] equations 8.5 and 8.6 are used
#' to calculate the local azimuth and altitude of the moon, using
#' \code{\link{equatorialToLocalHorizontal}}.
#'
#' @param t time, a POSIXt object (converted to timezone \code{"UTC"},
#' if it is not already in that timezone), or a numeric value that
#' corresponds to such a time.
#' @param longitude observer longitude in degrees east
#' @param latitude observer latitude in degrees north
#' @param useRefraction boolean, set to \code{TRUE} to apply a correction for
#' atmospheric refraction.  (Ignored at present.)
#' @return A list containing the following.  \item{time}{time}
#' \item{azimuth}{moon azimuth, in degrees eastward of north, from 0 to 360.
#' Note: this is not the convention used by Meeus, who uses degrees westward of
#' South.  (See diagram below.)} \item{altitude}{moon altitude, in degrees from
#' -90 to 90.  (See diagram below.)} \item{rightAscension}{right ascension, in
#' degrees} \item{declination}{declination, in degrees}
#' \item{lambda}{geocentric longitude, in degrees} \item{beta}{geocentric
#' latitude, in degrees} \item{diameter}{lunar diameter, in degrees.}
#' \item{distance}{earth-moon distance, in kilometers)}
#' \item{illuminatedFraction}{fraction of moon's visible disk that is
#' illuminated} \item{phase}{phase of the moon, defined in equation 32.3 of
#' Meeus [1982].  The fractional part of which is 0 for new moon, 1/4 for first
#' quarter, 1/2 for full moon, and 3/4 for last quarter.}
#' \if{html}{\figure{starCoords.png options:width=400px}{starCoords}}
#'
#' @section Alternate formulations:
#' Formulae provide by Meeus [1982] are used
#' for all calculations here.  Meeus [1991] provides formulae that are similar,
#' but that differ in the 5th or 6th digits.  For example, the formula for
#' ephemeris time in Meeus [1991] differs from that in Meeus [1992] at the 5th
#' digit, and almost all of the approximately 200 coefficients in the relevant
#' formulae also differ in the 5th and 6th digits.  Discussion of the changing
#' formulations is best left to members of the astronomical community.  For the
#' present purpose, it may be sufficient to note that \code{moonAngle}, based
#' on Meeus [1982], reproduces the values provided in example 45.a of Meeus
#' [1991] to 4 significant digits, e.g. with all angles matching to under 2
#' minutes of arc.
#' @author Dan Kelley, based on formulae in Meeus [1982].
#' @seealso The equivalent function for the sun is \code{\link{sunAngle}}.
#' @references Meeus, Jean, 1982.  Astronomical formulae for calculators.
#' Willmann-Bell. Richmond VA, USA. 201 pages.
#'
#' Meeus, Jean, 1991. Astronomical algorithms.  Willmann-Bell, Richmond VA,
#' USA. 429 pages.
#' @examples
#'
#' library(oce)
#' par(mfrow=c(3,2))
#' y <- 2012
#' m <- 4
#' days <- 1:3
#' ## Halifax sunrise/sunset (see e.g. https://www.timeanddate.com/worldclock)
#' rises <- ISOdatetime(y, m, days,c(13,15,16), c(55, 04, 16),0,tz="UTC") + 3 * 3600 # ADT
#' sets <- ISOdatetime(y, m,days,c(3,4,4), c(42, 15, 45),0,tz="UTC") + 3 * 3600
#' azrises <- c(69, 75, 82)
#' azsets <- c(293, 288, 281)
#' latitude <- 44.65
#' longitude <- -63.6
#' for (i in 1:3) {
#'     t <- ISOdatetime(y, m, days[i],0,0,0,tz="UTC") + seq(0, 24*3600, 3600/4)
#'     ma <- moonAngle(t, longitude, latitude)
#'     oce.plot.ts(t, ma$altitude, type='l', mar=c(2, 3, 1, 1), cex=1/2, ylab="Altitude")
#'     abline(h=0)
#'     points(rises[i], 0, col='red', pch=3, lwd=2, cex=1.5)
#'     points(sets[i], 0, col='blue', pch=3, lwd=2, cex=1.5)
#'     oce.plot.ts(t, ma$azimuth, type='l', mar=c(2, 3, 1, 1), cex=1/2, ylab="Azimuth")
#'     points(rises[i], -180+azrises[i], col='red', pch=3, lwd=2, cex=1.5)
#'     points(sets[i], -180+azsets[i], col='blue', pch=3, lwd=2, cex=1.5)
#' }
#'
#' @family things related to astronomy
moonAngle <- function(t, longitude=0, latitude=0, useRefraction=TRUE)
{
    if (missing(t)) stop("must give 't'")
    if (!inherits(t, "POSIXt")) {
        if (is.numeric(t)) {
            tref <- as.POSIXct("2000-01-01 00:00:00", tz="UTC") # arbitrary
            t <- t - as.numeric(tref) + tref
        } else {
            stop("t must be POSIXt or a number corresponding to POSIXt (in UTC)")
        }
    }
    if ("UTC" != attr(as.POSIXct(t[1]), "tzone"))
        attributes(t)$tzone <- "UTC"
    if (missing(longitude)) stop("must give 'longitude'")
    if (missing(latitude)) stop("must give 'latitude'")
    RPD <- atan2(1, 1) / 45            # radians per degree
    ## In this cde, the symbol names follow [1] Meeus 1982 chapter 30, with e.g. "p"
    ## used to indicate primes, e.g. Lp stands for L' in Meeus' notation.
    ## Also, T2 and T3 are powers on T.
    T <- julianCenturyAnomaly(julianDay(t))
    T2 <- T * T
    T3 <- T * T2
    ## Step 1 (top of Meuus page 148, chapter 30): mean quantaties
    ## moon mean longitude
    Lp <-    270.434164 + 481267.8831 * T - 0.001133 * T2 + 0.0000019 * T3
    ## sun mean amomaly
    M <-     358.475833 +  35999.0498 * T - 0.000150 * T2 - 0.0000033 * T3
    ## moon mean amomaly
    Mp <-    296.104608 + 477198.8491 * T + 0.009192 * T2 + 0.0000144 * T3
    ## moon mean elongation
    D <-     350.737486 + 445267.1142 * T - 0.001436 * T2 + 0.0000019 * T3
    ## moon distance from ascending node
    F <-      11.250889 + 483202.0251 * T - 0.003211 * T2 - 0.0000003 * T3
    ## longitude of moon ascending node
    Omega <- 259.183275 -   1934.1420 * T + 0.002078 * T2 + 0.0000022 * T3
    ## Step 2 (to bottom of p 148, chapter 30): add periodic variations ("additive terms")
    ## note that 'tmp' is redefined every few lines
    tmp <- sin(RPD * (51.2 + 20.2 * T))
    Lp <- Lp + 0.000233 * tmp
    M  <- M  - 0.001778 * tmp
    Mp <- Mp + 0.000817 * tmp
    D  <-  D + 0.002011 * tmp
    tmp <- 0.003964 * sin(RPD * (346.560 + 132.870 * T - 0.0091731 * T2))
    Lp <- Lp +  tmp
    Mp <- Mp +  tmp
    D  <- D  +  tmp
    F  <- F  +  tmp
    tmp <- sin(RPD * Omega)
    Lp <- Lp + 0.001964 * tmp
    Mp <- Mp + 0.002541 * tmp
    D  <- D  + 0.001964 * tmp
    F  <- F  - 0.024691 * tmp
    F  <- F  - 0.004328 * sin(RPD * (Omega + 275.05 - 2.30 * T))
    ## Step 3: [1] Meeus p 149
    e <- 1 - 0.002495 * T - 0.00000752 * T2
    e2 <- e * e
    lambda <- Lp +
    (     6.288750 * sin(RPD * (Mp                 ))) +
    (     1.274018 * sin(RPD * (2 * D - Mp         ))) +
    (     0.658309 * sin(RPD * (2 * D              ))) +
    (     0.213616 * sin(RPD * (2 * Mp             ))) +
    (e * -0.185596 * sin(RPD * (M                  ))) +
    (    -0.114336 * sin(RPD * (2 * F              ))) +
    (     0.058793 * sin(RPD * (2 * D - 2 * Mp     ))) +
    (e *  0.057212 * sin(RPD * (2 * D - M - Mp     ))) +
    (     0.053320 * sin(RPD * (2 * D + Mp         ))) +
    (e *  0.045874 * sin(RPD * (2 * D - M          ))) +
    (e *  0.041024 * sin(RPD * (Mp - M             ))) +
    (    -0.034718 * sin(RPD * (D                  ))) +
    (e * -0.030465 * sin(RPD * (M + Mp             ))) +
    (     0.015326 * sin(RPD * (2 * D - 2 * F      ))) +
    (    -0.012528 * sin(RPD * (2 * F + Mp         ))) +
    (    -0.010980 * sin(RPD * (2 * F - Mp         ))) +
    (     0.010674 * sin(RPD * (4 * D - Mp         ))) +
    (     0.010034 * sin(RPD * (3 * M              ))) +
    (     0.008548 * sin(RPD * (4 * D - 2 * Mp     ))) +
    (e * -0.007910 * sin(RPD * (M - Mp + 2 * D     ))) +
    (e * -0.006783 * sin(RPD * (2 * D + M          ))) +
    (     0.005162 * sin(RPD * (Mp - D             ))) +
    (e *  0.005000 * sin(RPD * (M  + D             ))) +
    (e *  0.004049 * sin(RPD * (Mp - M + 2 * D     ))) +
    (     0.003996 * sin(RPD * (2 * Mp + 2 * D     ))) +
    (     0.003862 * sin(RPD * (4 * D              ))) +
    (     0.003665 * sin(RPD * (2 * D - 3 * Mp     ))) +
    (e *  0.002696 * sin(RPD * (2 * Mp - M         ))) +
    (     0.002602 * sin(RPD * (Mp - 2 * F - 2 * D ))) +
    (e *  0.002396 * sin(RPD * (2 * D - M - 2 * Mp ))) +
    (    -0.002349 * sin(RPD * (Mp + D             ))) +
    (e2*  0.002249 * sin(RPD * (2 * D - 2 * M      ))) +
    (e * -0.002125 * sin(RPD * (2 * Mp + M         ))) +
    (e2* -0.002079 * sin(RPD * (2 * M              ))) +
    (e2*  0.002059 * sin(RPD * (2 * D - Mp - 2 * M ))) +
    (    -0.001773 * sin(RPD * (Mp + 2 * D - 2 * F ))) +
    (    -0.001595 * sin(RPD * (2 * F + 2 * D      ))) +
    (e *  0.001220 * sin(RPD * (4 * D - M - Mp     ))) +
    (    -0.001110 * sin(RPD * (2 * Mp + 2 * F     ))) +
    (     0.000892 * sin(RPD * (Mp - 3 * D         ))) +
    (e * -0.000811 * sin(RPD * (M + Mp + 2 * D     ))) +
    (e *  0.000761 * sin(RPD * (4 * D - M - 2 * Mp ))) +
    (e2*  0.000717 * sin(RPD * (Mp - 2 * M         ))) +
    (e2*  0.000704 * sin(RPD * (Mp - 2 * M - 2 * D ))) +
    (e *  0.000693 * sin(RPD * (M - 2 * Mp + 2 * D ))) +
    (e *  0.000598 * sin(RPD * (2 * D - M - 2 * F  ))) +
    (     0.000550 * sin(RPD * (Mp + 4 * D         ))) +
    (     0.000538 * sin(RPD * (4 * Mp             ))) +
    (e *  0.000521 * sin(RPD * (4 * D - M          ))) +
    (     0.000486 * sin(RPD * (2 * M - D          )))
    lambda <- lambda %% 360
    B <-  0 +
    (     5.128189 * sin(RPD * (F                  ))) +
    (     0.280606 * sin(RPD * (Mp + F             ))) +
    (     0.277693 * sin(RPD * (Mp - F             ))) +
    (     0.173238 * sin(RPD * (2 * D - F          ))) +
    (     0.055413 * sin(RPD * (2 * D + F - Mp     ))) +
    (     0.046272 * sin(RPD * (2 * D - F - Mp     ))) +
    (     0.032573 * sin(RPD * (2 * D + F          ))) +
    (     0.017198 * sin(RPD * (2 * Mp + F         ))) +
    (     0.009267 * sin(RPD * (2 * D + Mp - F     ))) +
    (     0.008823 * sin(RPD * (2 * Mp - F         ))) +
    (     0.008247 * sin(RPD * (2 * D - M - F      ))) +
    (     0.004323 * sin(RPD * (2 * D - F - 2 * Mp ))) +
    (     0.004200 * sin(RPD * (2 * D + F + Mp     ))) +
    (e *  0.003372 * sin(RPD * (F - M - 2 * D      ))) +
    (e *  0.002472 * sin(RPD * (2 * D + F - M - Mp ))) +
    (e *  0.002222 * sin(RPD * (2 * D + F - M      ))) +
    (e *  0.002072 * sin(RPD * (2 * D - F - M - Mp ))) +
    (e *  0.001877 * sin(RPD * (F - M + Mp         ))) +
    (     0.001828 * sin(RPD * (4 * D - F - Mp     ))) +
    (e * -0.001803 * sin(RPD * (F + M              ))) +
    (    -0.001750 * sin(RPD * (3 * F              ))) +
    (e *  0.001570 * sin(RPD * (Mp - M - F         ))) +
    (    -0.001487 * sin(RPD * (F + D              ))) +
    (e * -0.001481 * sin(RPD * (F + M + Mp         ))) +
    (e *  0.001417 * sin(RPD * (F - M - Mp         ))) +
    (e *  0.001350 * sin(RPD * (F - M              ))) +
    (     0.001330 * sin(RPD * (F - D              ))) +
    (     0.001106 * sin(RPD * (F + 3 * Mp         ))) +
    (     0.001020 * sin(RPD * (4 * D - F          ))) +
    (     0.000833 * sin(RPD * (F + 4 * D - Mp     ))) +
    (     0.000781 * sin(RPD * (Mp - 3 * F         ))) +
    (     0.000670 * sin(RPD * (F + 4 * D - 2 * Mp ))) +
    (     0.000606 * sin(RPD * (2 * D - 3 * F      ))) +
    (     0.000597 * sin(RPD * (2 * D + 2 * Mp - F ))) +
    (e *  0.000492 * sin(RPD * (2 * D + Mp - M - F ))) +
    (     0.000450 * sin(RPD * (2 * Mp - F - 2 * D ))) +
    (     0.000439 * sin(RPD * (3 * Mp - F         ))) +
    (     0.000423 * sin(RPD * (F + 2 * D + 2 * Mp ))) +
    (     0.000422 * sin(RPD * (2 * D - F - 3 * Mp ))) +
    (e * -0.000367 * sin(RPD * (F + F + 2 * D - Mp ))) +
    (e * -0.000353 * sin(RPD * (M + F + 2 * D      ))) +
    (     0.000331 * sin(RPD * (F + 4 * D          ))) +
    (e *  0.000317 * sin(RPD * (2 * D + F - M + Mp ))) +
    (e2*  0.000306 * sin(RPD * (2 * D - 2 * M - F  ))) +
    (    -0.000283 * sin(RPD * (Mp + 3 * F         )))
    omega1 <- 0.0004664 * cos(RPD * Omega)
    omega2 <- 0.0000754 * cos(RPD * (Omega + 275.05 - 2.30 * T))
    beta <- B * (1 - omega1 - omega2)
    pi <- 0.950724 +
    (     0.051818 * cos(RPD * (Mp                ))) +
    (     0.009531 * cos(RPD * (2 * D - Mp        ))) +
    (     0.007843 * cos(RPD * (2 * D             ))) +
    (     0.002824 * cos(RPD * (2 * Mp            ))) +
    (     0.000857 * cos(RPD * (2 * D + Mp        ))) +
    (e *  0.000533 * cos(RPD * (2 * D - M         ))) +
    (e *  0.000401 * cos(RPD * (2 * D - M - Mp    ))) +
    (e *  0.000320 * cos(RPD * (Mp - M            ))) +
    (    -0.000271 * cos(RPD * (D                 ))) + #OK above
    (e * -0.000264 * cos(RPD * (M + Mp            ))) +
    (    -0.000198 * cos(RPD * (2 * F - Mp        ))) +
    (     0.000173 * cos(RPD * (3 * Mp            ))) +
    (     0.000167 * cos(RPD * (4 * D - Mp        ))) +
    (e * -0.000111 * cos(RPD * (M                 ))) +
    (     0.000103 * cos(RPD * (4 * D - 2 * Mp    ))) +
    (    -0.000084 * cos(RPD * (2 * Mp - 2 * D    ))) +
    (e * -0.000083 * cos(RPD * (2 * D + M         ))) +
    (     0.000079 * cos(RPD * (2 * D + 2 * Mp    ))) + # ok above
    (     0.000072 * cos(RPD * (4 * D             ))) +
    (e *  0.000064 * cos(RPD * (2 * D - M + Mp    ))) +
    (e * -0.000063 * cos(RPD * (2 * D + M - Mp    ))) +
    (e *  0.000041 * cos(RPD * (M + D             ))) +
    (e *  0.000035 * cos(RPD * (2 * Mp - M        ))) +
    (    -0.000033 * cos(RPD * (3 * Mp - 2 * D    ))) +
    (    -0.000030 * cos(RPD * (Mp + D            ))) +
    (    -0.000029 * cos(RPD * (2 * F - 2 * D     ))) +
    (e * -0.000029 * cos(RPD * (2 * Mp + M        ))) +
    (e2*  0.000026 * cos(RPD * (2 * D - 2 * M     ))) +
    (    -0.000023 * cos(RPD * (2 * F - 2 * D + Mp))) +
    (e *  0.000019 * cos(RPD * (4 * D - M - Mp    )))
    ## For coordinate conversions, need epsilon (obliquity of the ecliptic)
    ## as defined in Meuus eq 18.4, page 81.
    epsilon <- 23.452294 - 0.0130125 * T - 0.00000164 * T2 + 0.000000503 * T3
    ec <- eclipticalToEquatorial(lambda, beta, epsilon)
    ##lh <- equatorialToLocalHorizontal(ec$rightAscension, ec$declination, t, latitude, longitude)
    lh <- equatorialToLocalHorizontal(rightAscension=ec$rightAscension,
                                      declination=ec$declination,
                                      t=t,
                                      longitude=longitude,
                                      latitude=latitude)
    ## Illuminated fraction, [1] chapter 31 (second, approximate, formula)
    D <- D %% 360 # need this; could have done it earlier, actually
    illfr <- 180 - D - 6.289 * sin(RPD * Mp) +
    2.100 * sin(RPD * M) -
    1.274 * sin(RPD * (2 * D - Mp)) -
    0.658 * sin(RPD * 2 * D) -
    0.2114 * sin(RPD * 2 * Mp) -
    0.112 * sin(RPD * D)
    illuminatedFraction <- (1 + cos(RPD * illfr)) / 2
    phase <- T * 1236.85               # Meeus [1982] eq 32.3 page 160

    ## The 180 in azimuth converts from astron convention with azimuth=westward
    ## angle from South, to eastward from North.
    res <- list(time=t,
                 azimuth=lh$azimuth + 180,
                 altitude=lh$altitude,
                 rightAscension=ec$rightAscension, declination=ec$declination,
                 lambda=lambda %% 360, beta=beta,
                 diameter=pi, distance=6378.14 / sin(RPD * pi),
                 illuminatedFraction=illuminatedFraction,
                 phase=phase)
    res
}
