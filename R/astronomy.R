#' Convert astronomical angle in degrees to hours, minutes and seconds
#'
#' The purpose of [angle2hms] is to facilitate comparison of
#' `rightAscension` angles computed by [sunAngle()] and [moonAngle()]
#' with angles reported in astronomical sources and software, which
#' often employ an hour-minute-second notation. In that notation,
#' decimal hour is computed as 24/360 times the angle in degrees,
#' and from that decimal hour are compute integer hour and minute
#' values, plus a decimal second value. It is common in the
#' astronomical literature to use strings to represent the results,
#' e.g. with \eqn{11^h40^m48^s.10}{11h40m48s.10} for the
#' value used in the \dQuote{Examples}; see Chapter 1 of
#' Meeus (1991) for more on angle calculation and representation.
#'
#' @param angle numerical value giving an angle in degrees
#'
#' @return [angle2hms] returns a list containing
#' values `time` (a numerical value for decimal hour, between 0 and 24),
#' `hour`, `minute`, and `second` (the last of which may have a fractional
#' part), and `string`, a character value indicates the time in hour-minute-second
#' notation, with the second part to two decimal places and intervening `h`,
#' `m` and `s` characters between the units.
#'
#' @examples
#' # A randomly-chosen example on page 99 of Meeus (1991).
#' angle2hms(177.74208) # string component 11h50m58s.10
#'
#' @references
#' * Meeus, Jean. Astronomical Algorithms. Second Edition.
#' Richmond, Virginia, USA: Willmann-Bell, 1991.
#'
#' @author Dan Kelley
#'
#' @family things related to astronomy
angle2hms <- function(angle)
{
    hourDecimal <- 24 * angle / 360
    hour <- floor(hourDecimal)
    minute <- floor(60*(hourDecimal - hour))
    second <- 3600*(hourDecimal - hour - minute / 60)
    floorSecond <- floor(second)
    centiSecond <- round(100 * (second - floorSecond))
    string <- sprintf("%.0fh%.0fm%.0fs.%.0f", hour, minute, floorSecond, centiSecond)
    list(hourDecimal=hourDecimal, hour=hour, minute=minute, second=second, string=string)
}

#' Convert ecliptical to equatorial coordinate
#'
#' Convert from ecliptical to equatorial coordinates, using
#' equations 8.3 and 8.4 of reference 1, or, equivalently,
#' equations 12.3 and 12.4 of reference 2.
#'
#' The code is based on reference 1; see [moonAngle()] for comments on
#' the differences in formulae found in reference 2.  Indeed,
#' reference 2 is only cited
#' here in case readers want to check the ideas of the formulae; DK
#' has found that reference 2 is available to him via his university library
#' inter-library loan system, whereas he owns a copy of reference 1.
#'
#' @param lambda longitude, in degrees, or a data frame containing
#' `lambda`, `beta`, and `epsilon`, in which case the next to
#' arguments are ignored
#'
#' @param beta geocentric latitude, in degrees
#'
#' @param epsilon obliquity of the ecliptic, in degrees
#'
#' @return A data frame containing columns `rightAscension` and
#' `declination` both in degrees.
#'
#' @author Dan Kelley, based on formulae in references 1 and 2.
#'
#' @references
#' * Meeus, Jean. Astronomical Formulas for Calculators. Second Edition.
#' Richmond, Virginia, USA: Willmann-Bell, 1982.
#'
#' * Meeus, Jean. Astronomical Algorithms. Second Edition.
#' Richmond, Virginia, USA: Willmann-Bell, 1991.
#'
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
#' The method is taken from equations 8.5 and 8.6 of reference 1, or, equivalently,
#' from equations 12.5 and 12.6 of reference 2.
#'
#' @param rightAscension right ascension, e.g. calculated with
#' [eclipticalToEquatorial()].
#'
#' @param declination declination, e.g. calculated with
#' [eclipticalToEquatorial()].
#'
#' @param t time of observation.
#'
#' @param longitude longitude of observation, positive in eastern hemisphere.
#'
#' @param latitude latitude of observation, positive in northern hemisphere.
#'
#' @return A data frame containing columns `altitude` (angle above
#' horizon, in degrees) and `azimuth` (angle anticlockwise from south, in
#' degrees).
#'
#' @author Dan Kelley, based on formulae in references 1 and 2.
#'
#' @references
#' * Meeus, Jean. Astronomical Formulas for Calculators. Second Edition.
#' Richmond, Virginia, USA: Willmann-Bell, 1982.
#'
#' * Meeus, Jean. Astronomical Algorithms. Second Edition.
#' Richmond, Virginia, USA: Willmann-Bell, 1991.
#'
#' @family things related to astronomy
equatorialToLocalHorizontal <- function(rightAscension, declination, t, longitude, latitude)
{
    RPD <- atan2(1, 1) / 45            # radians per degree
    ## sidereal Greenwich time (in hours)
    theta0 <- siderealTime(t)
    H <- theta0 * 15 + longitude - rightAscension
    ## Local horizontal coordinates; see reference 1 eq 8.5 and 8.6 or reference 2 eq 12.5 and 12.6
    A <- atan2(sin(RPD * H), cos(RPD * H) * sin(RPD * latitude) - tan(RPD * declination) * cos(RPD * latitude))
    ## the atan2() form matches websites on azimuth at Halifax in April 2012
    h <- asin(sin(RPD * latitude) * sin(RPD * declination) + cos(RPD * latitude) * cos(RPD * declination) * cos(RPD * H))
    data.frame(azimuth=A/RPD, altitude=h/RPD)
}


#' Convert a POSIXt time to a sidereal time
#'
#' Convert a POSIXt time to a sidereal time, using
#' the method in Chapter 7 of reference 1.  The small correction
#' that he discusses after his equation 7.1 is not applied here.
#'
#' @param t a time, in POSIXt format, e.g. as created by
#' [as.POSIXct()], [as.POSIXlt()], or
#' [numberAsPOSIXct()].  If this is provided, the other arguments are
#' ignored.
#'
#' @return A sidereal time, in hours in the range from 0 to 24.
#'
#' @author Dan Kelley
#'
#' @references
#' * Meeus, Jean. Astronomical Formulas for Calculators. Second Edition.
#' Richmond, Virginia, USA: Willmann-Bell, 1982.
#'
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
    T <- (jd0 - 2415020.0) / 36525      # reference 1 eq 7.1 (different in reference 2)
    hoursLeftOver <- 24 * (jd - jd0)
    res <- 6.6460656 + 2400.051262 * T + 0.00002581 * T * T
    res <- res + 1.002737908 * hoursLeftOver
    res <- res %% 24
    res
}


#' Convert a time to a Julian day
#'
#' Convert a POSIXt time (given as either the `t` argument
#' or as the `year`, `month`, and other arguments) to a Julian day,
#' using the method provided in
#' Chapter 3 of Meeus (1982).  It should be noted that
#' Meeus and other astronomical treatments use fractional days, whereas the
#' present code follows the R convention of specifying days in whole numbers,
#' with hours, minutes, and seconds also provided as necessary.  Conversion is
#' simple, as illustrated in the example for 1977 April 26.4, for which Meeus
#' calculates julian day 2443259.9.  Note that the R documentation for
#' [julian()] suggests another formula, but the point of the present
#' function is to match the other Meeus formulae, so that suggestion is ignored
#' here.
#'
#' @param t a time, in POSIXt format, e.g. as created by
#' [as.POSIXct()], [as.POSIXlt()], or
#' [numberAsPOSIXct()], or a character string that can be
#' converted to a time using [as.POSIXct()].  If `t` is provided,
#' the other arguments are ignored.
#'
#' @param year year, to be provided along with `month`, etc., if `t`
#' is not provided.
#'
#' @param month numerical value for the month, with January being 1.
#' (This is required if `t` is not provided.)
#'
#' @param day numerical value for day in month, starting at 1.
#' (This is required if `t` is not provided.)
#'
#' @param hour numerical value for hour of day, in range 0 to 24.
#' (This is required if `t` is not provided.)
#'
#' @param min numerical value of the minute of the hour.
#' (This is required if `t` is not provided.)
#'
#' @param sec numerical value for the second of the minute.
#' (This is required if `t` is not provided.)
#'
#' @param tz timezone
#'
#' @return A Julian-Day number, in astronomical convention as explained in
#' Meeus.
#'
#' @author Dan Kelley
#'
#' @references
#' * Meeus, Jean. Astronomical Formulas for Calculators. Second Edition.
#' Richmond, Virginia, USA: Willmann-Bell, 1982.
#'
#' @examples
#' library(oce)
#' ## example from Meeus
#' t <- ISOdatetime(1977, 4, 26, hour=0, min=0, sec=0, tz="UTC")+0.4*86400
#' stopifnot(all.equal(julianDay(t), 2443259.9))
#'
#' @family things related to astronomy
#' @family things related to time
julianDay <- function(t, year=NA, month=NA, day=NA, hour=NA, min=NA, sec=NA, tz="UTC")
{
    if (missing(t))  {
        if (is.na(year) || is.na(month) || is.na(day) || is.na(hour)
            || is.na(min) || is.na(sec))
            stop("must supply year, month, day, hour, min, and sec")
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
#' The method follows reference 1 equation 15.1.  The example reproduces the
#' example provided by reference 1 example 15.a, with fractional error 3e-8.
#'
#' @param jd a julian day number, e.g. as given by [julianDay()].
#'
#' @return Julian century since noon on January 1, 1900.
#'
#' @author Dan Kelley
#'
#' @references
#' * Meeus, Jean. Astronomical Formulas for Calculators. Second Edition.
#' Richmond, Virginia, USA: Willmann-Bell, 1982.
#'
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
    (jd - 2415020.0) / 36525         # reference 1 Meeus 1982 (eq 7.1 or 15.1)
}

