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
    (jd - 2415020.0) / 36525         # Meeus 1982 (eq 15.1)
}

moonAngle <- function(t, lat, lon, useRefraction=TRUE)
{
    ## In this cde, the symbol names follow Meeus chapter 30, with e.g. "p"
    ## used to indicate primes, e.g. Lp stands for L' in Meeus' notation.
    ## Also, T2 and T3 are powers on T.
    if (TRUE) {
        t <- ISOdatetime(1979, 12, 7, 0, 0, 0, tz="ET")
        cat(sprintf("t %s\n", format(t)))
    }
    T <- julianCenturyAnomaly(julianDay(t))
    T2 <- T * T
    T3 <- T * T2
    ## Step 1 (top of Meuus page 148, chapter 30): mean quanties
    ## moon mean longitude
    Lp <-    270.434164 + 481267.8831 * T - 0.001133 * T2 + 0.0000019 * T3
    ## sun mean amomaly
    M <-     358.475833 +  35999.0498 * T - 0.000150 * T2 - 0.0000033 * T3
    ## moon mean amomaly
    Mp <-    296.104608 + 477198.8491 * T + 0.009192 * T3 + 0.0000144 * T3
    ## moon mean elongation
    D <-     350.737486 + 445267.1142 * T - 0.001436 * T2 + 0.0000019 * T3
    ## moon distance from ascending node
    F <-      11.250889 + 483202.0251 * T - 0.003211 * T2 - 0.0000003 * T3
    omega <- 259.183275 -   1934.1420 * T + 0.002078 * T2 + 0.0000022 * T3
    ## Step 2 (bottom of p 148, chapter 30): add periodic variations ("additive terms")
    if (TRUE) {
        cat(sprintf("Step 1: mean values\n\tT %.10f\n\tLp %.4f\n\tM %.4f\n\tMp %.4f\n\tD %.4f\n\tF %.4f\n\tomega %.4f\n",
                    T, Lp, M, Mp, D, F, omega))
        stopifnot(all.equal(T, 0.7993018480))
    }
    if (TRUE) {
        cat(sprintf("Step 2: after additive terms\n\tT %.10f\n\tLp %.4f\n\tM %.4f\n\tMp %.4f\n\tD %.4f\n\tF %.4f\n\tomega %.4f\n",
                    T, Lp, M, Mp, D, F, omega))
    }
    data.frame(azimuth=1:3, elevation=1:3, diameter=rep(0, 3), distance=rep(0, 3))
}

