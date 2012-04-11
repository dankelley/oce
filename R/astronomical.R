julianDay <- function(t, year, month, day, hour, min, sec, tz="UTC")
{
    if (inherits(t, "POSIXt")) {
        tt <- as.POSIXlt(t)
    } else {
        if (missing(month) || missing(day) || missing(hour) || missing(min) || missing(sec) || missing(tz)) stop("must supply month, day, hour, min, sec, and tz")
        tt <- ISOdatetime(year, month, day, hour, min, sec, tz=tz)
    }
    year <- tt$year + 1900
    month <- tt$mon + 1
    day <- tt$mday + (tt$hour + tt$min / 60 + tt$sec / 3600) / 24 
    m <- ifelse(month <= 2, month + 12, month)
    y <- ifelse(month <= 2, year - 1, year)
    A <- floor(y / 100)
    B <- 2 - A + floor(A / 4)
    jd <- floor(365.25 * y) + floor(30.6001 * (m + 1)) + day + 1720994.5
    jd <- ifelse(tt > ISOdatetime(1582, 10, 15, 0, 0, 0), jd + B, jd)
    ## rval <- list(jd=jd, old=as.numeric(julian(t)+2440587.5))
    rval <- jd
    rval
}
