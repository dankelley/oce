sun.angle <- function(t, lat, lon, use.refraction=TRUE)
{
    if (missing(t))
        stop("must provide t")
    if (missing(lat))
        stop("must provide lat")
    if (missing(lon))
        stop("must provide lon")
    if (!inherits(t, "POSIXt")) {
        if (is.numeric(t)) {
            tref <- as.POSIXct("2000-01-01 00:00:00", tz="UTC") # arbitrary
            t <- t - as.numeric(tref) + tref
        } else {
            stop("t must be POSIXt or a number corresponding to POSIXt (in UTC)")
        }
    }
    dim <- dim(t)
    nt <- length(t)
    nlat <- length(lat)
    nlon <- length(lon)
    if (nlon != nlat)
        stop("lengths of lon and lat must match")
    if (nlon == 1) {
        lon <- rep(lon, nt)             # often, give a time vector but just on location
        lat <- rep(lat, nt)
    } else {
        if (nt != nlon)
            stop("lengths of t and lon must match")
    }
    ## the code below is derived from fortran code, downloaded 2009-11-1 from
    ## ftp://climate1.gsfc.nasa.gov/wiscombe/Solar_Rad/SunAngles/sunae.f
    t <- as.POSIXlt(t)                 # use this so we can work on hours, etc
    if ("UTC" != attr(t, "tzone"))
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
    if (any(lat <  -90)) {
        warning("latitude(s) trimmed to range -90 to 90")
        lat[lat <  -90] <- -90
    }
    if (any(lat >   90)) {
        warning("latitude(s) trimmed to range -90 to 90")
        lat[lat >   90] <-  90
    }
    if (any(lon < -180)) {
        warning("longitude(s) trimmed to range -180 to 180")
        lon[lon < -180] <- -180
    }
    if (any(lon >  180)) {
        warning("longitude(s) trimmed to range -180 to 180")
        lon[lon >  180] <-  180
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
    lmst <- gmst + lon/15
    lmst <- lmst %% 24
    lmst <- lmst + ifelse(lmst < 0, 24, 0)
    lmst <- lmst * 15 * rpd
    ha <- lmst - ra
    ha <- ha + ifelse (ha < (-pi), 2 * pi, 0)
    ha <- ha - ifelse (ha > pi, 2 * pi, 0)
    el <- asin(sin(dec) * sin(lat * rpd) + cos(dec) * cos(lat*rpd)*cos(ha))
    az <- asin(-cos(dec) * sin(ha) / cos(el))
    az <-  ifelse(sin(dec) - sin(el) * sin(lat * rpd ) > 0,
                  ifelse (sin(az) < 0, az + 2 * pi, az),
                  pi - az)
    el <- el / rpd
    az <- az / rpd
    if (use.refraction) {
        refrac <- ifelse(el >= 19.225,
                         0.00452 * 3.51823 / tan(el * rpd),
                         ifelse (el > (-0.766) & el < 19.225,
                                 3.51823 * (0.1594 + el * (0.0196 + 0.00002 * el)) / (1. + el * (0.505 + 0.0845 * el)),
                                 0))
        el  <- el + refrac
    }
    soldst <- 1.00014 - 0.01671 * cos(mnanom) - 0.00014 * cos(2 * mnanom)
    soldia <- 0.5332 / soldst
    if (any(el < (-90.0)) || any(el > 90))
        stop("output argument el out of range")
    if (any(az < 0) || any(az > 360))
        stop("output argument az out of range")
    data.frame(azimuth=az, elevation=el, solar.diameter=soldia, solar.distance=soldst)
}


