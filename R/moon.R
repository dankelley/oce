## References used in this file:
##
## Meeus, Jean, 1982.  Astronomical formuae for Calculators.
## Willmann-Bell. Richmond VA, USA. 201 pages.

julianDay <- function(t, year, month, day, hour, min, sec, tz="UTC")
{
    ## Meeus (1982 ch 3)
    ## as.numeric(julian(t)+2440587.5)) is suggested in R doc on julian()
    if (!inherits(t, "POSIXt"))  {
        if (missing(month) || missing(day) || missing(hour)
            || missing(min) || missing(sec) || missing(tz))
            stop("must supply month, day, hour, min, sec, and tz")
        tt <- ISOdatetime(year, month, day, hour, min, sec, tz=tz)
    }
    tt <- as.POSIXlt(t)
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

julianCenturyAnomaly <- function(jd)
{
    (jd - 2415020.0) / 36525.0         # Meeus 1982 (eq 15.1)
}

moonAngle <- function(t, lat, lon, useRefraction=TRUE)
{

}

