sun.angle <- function(t, lat, lon)
{
    if (missing(t)) stop("must provide t")
    if (!inherits(t, "POSIXt")) stop("t must be POSIXt")
    ## should check GMT, too.
    if (missing(lat)) stop("must provide lat")
    if (missing(lon)) stop("must provide lon")
    dim <- dim(t)
    nt <- length(t)
    nlat <- length(lat)
    nlon <- length(lon)
    if (nt != nlat) stop("lengths of t and lat must match")
    if (nt != nlon) stop("lengths of t and lon must match")
    tt <- as.POSIXlt(t)
    year <- tt$year + 1900
    day <- tt$yday + 1                  # fortran wants first day to be called 1
    hour <- tt$hour + tt$min / 60 + tt$sec / 3600
    az <- vector("numeric", length=nt)
    el <- vector("numeric", length=nt)
    soldia <- vector("numeric", length=nt)
    soldst <- vector("numeric", length=nt)
    for (i in 1:nt) {
        #print(i)
        #print(year[i])
        #print(day[i])
        #print(hour[i])
        a <- .Fortran("sunae", as.integer(year[i]), as.integer(day[i]), as.double(hour[i]), as.double(lat[i]), as.double(lon[i]), az=double(1), el=double(1), soldia=double(1), soldst=double(1))
        az[i] <- a$az
        el[i] <- a$el
        soldia[i] <- a$soldia
        soldst[i] <- a$soldst
    }
    data.frame(az=az, el=el, soldia=soldia, soldst=soldst)
}
