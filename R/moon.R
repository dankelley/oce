## References used in this file:
##
## Meeus, Jean, 1982.  Astronomical formuae for Calculators.
## Willmann-Bell. Richmond VA, USA. 201 pages.

debugMoonAngle <- !TRUE

##
## > siderealTime(ISOdatetime(1978,11,13,4,34,0,tz="UTC"))
## [1] 8.029539657
## cf  8.0295394 (Meeus 1982 page 40)
siderealTime <- function(t)
{                                   
    ## Calculate siderial time at greenwich (Meuss 1982 equation 7.1, page 39).
    ## NB. the correction he later discusses seems to be sub-second,
    ## so we ignore it.  A test value, below, matches his text.
    ## > format(theta0(julianCenturyAnomaly(2443825.5)),digits=10)
    ## [1] "3.450369881"
    tt <- as.POSIXlt(t)
    n <- length(tt$hour)
    tt$hour <- rep(0, n)
    tt$min <- rep(0, n)
    tt$sec <- rep(0, n)
    jd <- julianDay(t)
    jd0 <- julianDay(tt)
    T <- (jd0 - 2415020.0) / 36525      # Meeus 1982 (eq 7.1)
    hoursLeftOver <- 24 * (jd - jd0)
    rval <- 6.6460656 + 2400.051262 * T + 0.00002581 * T * T + 1.002737908 * hoursLeftOver
    rval <- rval %% 24
    rval
}

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
    tt <- as.POSIXlt(t, tz=tz)
    ##cat("julianDay has first 2 times:", format(t[1:2],"%y-%m-%d %H:%M:%S %Z"), "->", format(tt[1:2], "%y-%m-%d %H:%M:%S %Z"), "\n")
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
    (jd - 2415020.0) / 36525         # Meeus 1982 (eq 7.1 or 15.1)
}

moonAngle <- function(t, latitude, longitude, useRefraction=TRUE)
{
    RPD <- atan2(1, 1) / 45            # radians per degree
    ## In this cde, the symbol names follow Meeus chapter 30, with e.g. "p"
    ## used to indicate primes, e.g. Lp stands for L' in Meeus' notation.
    ## Also, T2 and T3 are powers on T.
    if (debugMoonAngle) {
        t <- ISOdatetime(1979, 12, 7, 0, 0, 0, tz="ET")
        cat(sprintf("t %s\n", format(t)))
    }
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
    if (debugMoonAngle) {
        cat(sprintf("Step 1: mean values\n\tLp %.4f (%.4f)\n\tM %.4f (%.4f)\n\tMp %.4f (%.4f)\n\tD %.4f (%.4f)\n\tF %.4f (%.4f)\n\tOmega %.4f (%.4f)\n",
                    Lp, Lp %% 360,
                    M, M %% 360,
                    Mp, Mp %% 360,
                    D, D %% 360,
                    F, F %% 360, 
                    Omega, Omega %% 360))
        stopifnot(all.equal(T, 0.7993018480))
    }
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
    ## Step 3: Meeus p 149
    e <- 1 - 0.002495 * T - 0.00000752 * T2
    e2 <- e * e
    if (debugMoonAngle) {
        cat(sprintf("Step 2: after additive terms\n\tLp %.4f (%.4f)\n\tM %.4f (%.4f)\n\tMp %.4f (%.4f)\n\tD %.4f (%.4f)\n\tF %.4f (%.4f)\n\tOmega %.4f (%.4f)\n\te %.10f\n",
                    Lp, Lp %% 360,
                    M, M %% 360,
                    Mp, Mp %% 360,
                    D, D %% 360,
                    F, F %% 360, 
                    Omega, Omega %% 360,
                    e))
        stopifnot(all.equal(e, 0.998001, 0.000001))
    }
    lambda <- Lp +
    (     6.288750 * sin(RPD * (Mp            ))) +
    (     1.274018 * sin(RPD * (2 * D - Mp    ))) +
    (     0.658309 * sin(RPD * (2 * D         ))) +
    (     0.213616 * sin(RPD * (2 * Mp        ))) +
    (e * -0.185596 * sin(RPD * (M             ))) +
    (    -0.114336 * sin(RPD * (2 * F         ))) +
    (     0.058793 * sin(RPD * (2 * D - 2 * Mp))) +
    (e *  0.057212 * sin(RPD * (2 * D - M - Mp))) +
    (     0.053320 * sin(RPD * (2 * D + Mp    ))) +
    (e *  0.045874 * sin(RPD * (2 * D - M     ))) +
    (e *  0.041024 * sin(RPD * (Mp - M        ))) +
    (    -0.034718 * sin(RPD * (D             ))) +
    (    -0.030465 * sin(RPD * (M + Mp        ))) +
    (     0.015326 * sin(RPD * (2 * D - 2 * F )))
    lambda <- lambda %% 360
    ##warning("several terms in lambda ignored\n")
    B <-  0 +
    (     5.128189 * sin(RPD * (F             ))) +
    (     0.280606 * sin(RPD * (Mp + F        ))) +
    (     0.277693 * sin(RPD * (Mp - F        ))) +
    (     0.173238 * sin(RPD * (2 * D - F     ))) +
    (     0.055413 * sin(RPD * (2 * D + F - Mp))) +
    (     0.046272 * sin(RPD * (2 * D - F - Mp))) +
    (     0.032573 * sin(RPD * (2 * D + F     ))) +
    (     0.017198 * sin(RPD * (2 * Mp + F    ))) +
    (     0.009267 * sin(RPD * (2 * D + Mp - F))) +
    (     0.008823 * sin(RPD * (2 * Mp - F    ))) +
    (     0.008247 * sin(RPD * (2 * D - M - F )))
    ##warning("several terms in B ignored\n")
    omega1 <- 0.0004664 * cos(RPD * Omega)
    omega2 <- 0.0000754 * cos(RPD * (Omega + 275.05 - 2.30 * T))
    beta <- B * (1 - omega1 - omega2)
    pi <- 0.950724 +
    (     0.051818 * cos(RPD * (Mp            ))) +
    (     0.009531 * cos(RPD * (2 * D - Mp    ))) +
    (     0.007843 * cos(RPD * (2 * D         ))) +
    (     0.002824 * cos(RPD * (2 * Mp        ))) +
    (     0.000857 * cos(RPD * (2 * D + Mp    ))) +
    (e *  0.000533 * cos(RPD * (2 * D - M     )))
    ##warning("several terms in pi ignored\n")
    if (debugMoonAngle) {
        cat(sprintf("Step 3:\n\tlambda %.4f (%.4f)\n\tB %.4f (%.4f)\n\tbeta %.4f (%.4f)\n\tomega1 %.7f\n\tomega2 %.7f\n\tpi %.7f\n",
                    lambda, lambda %% 360,
                    B, B %% 360,
                    beta, beta %% 360,
                    omega1,
                    omega2, 
                    pi))
        ##stopifnot(all.equal(omega1, -0.0004164, 0.001, scale=1e-4)) # obscure syntax
        stopifnot(abs(omega1 - (-0.0004164)) < 0.0000002)
        stopifnot(abs(omega2 - (+0.0000301)) < 0.0000002)
        stopifnot(all.equal(lambda, 113.6604, tol=0.01))
        stopifnot(all.equal(beta, -3.163037, tol=0.001))
        stopifnot(all.equal(pi, 0.930249, tol=0.001))
    }
    ## For coordinate conversions, need epsilon (obliquity of the ecliptic) 
    ## as defined in Meuus eq 18.4, page 81.
    epsilon <- 23.452294 - 0.0130125 * T - 0.00000164 * T2 + 0.000000503 * T3
    ## Transform ecliptical to equatorial coordinates (Meuss eq 8.3 and 8.4) page 44
    ## alpha = right ascension [in radians, here]
    alpha <- atan((sin(RPD * lambda) * cos(RPD * epsilon) - tan(RPD * beta) * sin(RPD * epsilon)) / cos(RPD * lambda))
    print(alpha[1:3]/RPD)
#    alpha <- atan2(sin(RPD * lambda) * cos(RPD * epsilon) - tan(RPD * beta) * sin(RPD * epsilon), cos(RPD * lambda))
    print(alpha[1:3]/RPD)
    ## FIXME: atan2 and atan are giving different results; is this a clue?
    ## delta = declination [in radians, here] FIXME: can there be a cut-point issue here?
    delta <- asin(sin(RPD * beta) * cos(RPD * epsilon) + cos(RPD * beta) * sin(RPD * epsilon) * sin(RPD * lambda))
    ## sidereal time at Greenwhich at 0000UTC (in hours)
    theta0 <- siderealTime(t)
    H <- theta0 * 15 - longitude - alpha / RPD
    ## Transform to local horizontal coordinates (A=azimuth, h=altitude); Meuss eq 8.5 and 8.6 page 44
    A <- atan((sin(RPD * H)) / (cos(RPD * H) * sin(RPD * latitude) - tan(delta) * cos(RPD * latitude)))
    A <- atan2(sin(RPD * H), cos(RPD * H) * sin(RPD * latitude) - tan(delta) * cos(RPD * latitude))
    ## the atan2() form matches websites on azimuth at Halifax in April 2012
    ##A <- ifelse(A < 0, A + 180*RPD, A)
    h <- asin(sin(RPD * latitude) * sin(delta) + cos(RPD * latitude) * cos(delta) * cos(RPD * H))
    rval <- data.frame(t=t, azimuth=A / RPD, altitude=h / RPD, diameter=pi, distance=6378.14 / sin(RPD * pi),
                       T=T, lambda=lambda %% 360, beta=beta, epsilon=epsilon, H=H, theta0=theta0)
    rval
}

if (interactive()) {
warning("FIXME: why do pink lines work, i.e. offset 3.6 hours??\n")
par(mfrow=c(3,2))
y <- 2012
m <- 4
days <- 1:3
rises <- ISOdatetime(y, m, days,c(13,15,16), c(55, 04, 16),0,tz="UTC") + 3 * 3600
sets <- ISOdatetime(y, m,days,c(3,4,4), c(42, 15, 45),0,tz="UTC") + 3 * 3600
azrises <- c(69, 75, 82)
azsets <- c(293, 288, 281)
for (i in 1:3) {
    t <- ISOdatetime(y, m, days[i],0,0,0,tz="UTC") + seq(0, 24*3600, 3600/4)
    ma <- moonAngle(t, 44.65, -63.6) 
    oce.plot.ts(t, ma$altitude)
    abline(h=0)
    abline(v=rises[i], col='red')
    abline(v=rises[i]+3.6*3600, col='red', lty='dotted') ## seems to be offset by 2.5h
    abline(v=sets[i], col='blue')
    abline(v=3.6*3600+sets[i], col='blue', lty=2)
    oce.plot.ts(t, ma$azimuth)
    abline(h=0)
    abline(v=rises[i], col='red')
    abline(v=rises[i]+3.6*3600, col='red', lty='dotted') ## seems to be offset by 2.5h
    abline(v=3.6*3600+sets[i], col='blue', lty=2)
    abline(v=sets[i], col='blue')
    abline(h=-180+azrises[i], col='red', lty=2)
    abline(h=-180+azsets[i], col='blue', lty=2)
}
}
## http://www.timeanddate.com/worldclock/astronomy.html?n=286&month=4&year=2012&obj=moon&afl=-12&day=1
## FIXME: why does adding 3.6 hours make a match to the pred of the above-named website?

