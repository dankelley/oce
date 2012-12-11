library(mapproj)
mapContour <- function(longitude=seq(0, 1, length.out=nrow(z)),
                       latitude=seq(0, 1, length.out=ncol(z)),
                       z,
                       nlevels=10, levels=pretty(range(z, na.rm=TRUE), nlevels), 
                       ##labels=null,
                       ##xlim=range(longitude, finite=TRUE),
                       #ylim=range(latitude, finite=TRUE),
                       ##labcex=0.6,
                       #drawlabels=TRUE,
                       ##method="flattest",
                       ##vfont,
                       ## axes=TRUE, frame.plot=axes,
                       col=par("fg"), lty=par("lty"), lwd=par("lwd"))
{
    cl <- contourLines(x=longitude, y=latitude, z=z, nlevels=nlevels, levels=levels)
    for (i in 1:length(cl)) {
        ##cat(cl[[i]]$level, 'm\n')
        mapLines(cl[[i]]$x, cl[[i]]$y, lty=lty, lwd=lwd, col=col)
    }
    ## FIXME: labels, using labcex and vfont
}


mapPlot <- function(longitude, latitude, longitudelim, latitudelim, grid,
                    projection="mercator", parameters=NULL, orientation=NULL,
                    ...)
{
    if (inherits(longitude, "coastline")) {
        latitude <- longitude[['latitude']]
        longitude <- longitude[['longitude']]
    }
    xy <- mapproject(longitude, latitude,
                     projection=projection, parameters=parameters, orientation=orientation)
    limitsGiven <- !missing(latitudelim) && !missing(longitudelim)
    if (limitsGiven) {
        box <- mapproject(c(longitudelim[1], longitudelim[1], longitudelim[2], longitudelim[2]),
                          c(latitudelim[1], latitudelim[2], latitudelim[2], latitudelim[1]))
        plot(xy$x, xy$y,
             xlim=range(box$x, na.rm=TRUE), ylim=range(box$y, na.rm=TRUE),
             xlab="", ylab="", asp=1, axes=FALSE, ...)
    } else {
        plot(xy$x, xy$y,
             xlab="", ylab="", asp=1, axes=FALSE, ...)
    }
    usr <- par('usr')
    mapMeridians(grid, lty='dotted', col='darkgray')
    mapZones(grid, lty='dotted', col='darkgray')
    box()
}

mapMeridians <- function(lat, ...)
{
    if (missing(lat))
        lat <- TRUE
    if (is.logical(lat)) {
        if (!lat)
            return
        lat <- 10
    }
    if (length(lat) == 1)
        lat <- seq(-90, 90, lat)
    usr <- par('usr')
    n <- 360                           # number of points on line
    for (l in lat) {
        ## FIXME: should use mapLines here
        line <- mapproject(seq(-180, 180, length.out=n), rep(l, n))
        x <- line$x
        y <- line$y
        ok <- !is.na(x) & !is.na(y)
        x <- x[ok]
        y <- y[ok]
        if (length(x) & any(usr[1] <= x & x <= usr[2] & usr[3] <= y & y <= usr[4])) {
            lines(x, y, ...)
        }
    }
}

mapZones <- function(lon, ...)
{
    if (missing(lon))
        lon <- TRUE
    if (is.logical(lon)) {
        if (!lon)
            return
        lon <- 10
    }
    if (length(lon) == 1)
        ##lon <- rep(seq(-180, 180, lon), each=360/lon)
        lon <- seq(-180, 180, lon)
    usr <- par('usr')
    n <- 360                           # number of points on line
    for (l in lon) {
        ## FIXME: should use mapLines here
        line <- mapproject(rep(l, n), seq(-90+10, 90-10, length.out=n))
        x <- line$x
        y <- line$y
        ok <- !is.na(x) & !is.na(y)
        x <- x[ok]
        y <- y[ok]
        if (length(x) & any(usr[1] <= x & x <= usr[2] & usr[3] <= y & y <= usr[4])) {
            lines(x, y, ...)
        }
    }
}

mapLines <- function(longitude, latitude, greatCircle=FALSE, ...)
{
    if (inherits(longitude, "coastline")) {
        latitude <- longitude[['latitude']]
        longitude <- longitude[['longitude']]
    }
    if (greatCircle)
        warning("mapLines() does not yet handle argument 'greatCircle'")
    xy <- mapproject(longitude, latitude)
    ok <- !is.na(xy$x) & !is.na(xy$y)
    usr <- par('usr')
    if (any(usr[1] <= xy$x[ok] & xy$x[ok] <= usr[2] & usr[3] <= xy$y[ok] & xy$y[ok] <= usr[4])) {
        lines(xy$x, xy$y, ...)
    }
}

mapPoints <- function(longitude, latitude, ...)
{
    ok <- !is.na(longitude) & !is.na(latitude)
    longitude <- longitude[ok]
    latitude <- latitude[ok]
    if (length(longitude) > 0) {
        xy <- mapproject(longitude, latitude)
        points(xy$x, xy$y, ...)
    }
}

mapText <- function(longitude, latitude, labels, ...)
{
    ok <- !is.na(longitude) & !is.na(latitude)
    longitude <- longitude[ok]
    latitude <- latitude[ok]
    labels <- labels[ok]
    if (length(longitude) > 0) {
        xy <- mapproject(longitude, latitude)
        text(xy$x, xy$y, labels, ...)
    }
}

formatPosition <- function(latlon, isLat=TRUE, type=c("list", "string", "expression"), showHemi=FALSE)
{
    type <- match.arg(type)
    signs <- sign(latlon)
    x <- abs(latlon)
    degrees <- floor(x)
    minutes <- floor(60 * (x - degrees))
    seconds <- 3600 * (x - degrees - minutes / 60)
    seconds <- round(seconds, 2)

    ## prevent rounding errors from giving e.g. seconds=60
    ##print(data.frame(degrees,minutes,seconds))
    secondsOverflow <- seconds == 60
    seconds <- ifelse(secondsOverflow, 0, seconds)
    minutes <- ifelse(secondsOverflow, minutes+1, minutes)
    minutesOverflow <- minutes == 60
    degrees <- ifelse(minutesOverflow, degrees+1, degrees)
    ##print(data.frame(degrees,minutes,seconds))

    noSeconds <- all(seconds == 0)
    noMinutes <- noSeconds & all(minutes == 0)
    hemispheres <- if (isLat) ifelse(signs, "N", "S") else ifelse(signs, "E", "W")
    oceDebug(0, "noSeconds=", noSeconds, "noMinutes=", noMinutes, "\n")
    if (type == "list") {
        if (noMinutes)
            rval <- list(degrees, hemispheres)
        else if (noSeconds)
            rval <- list(degrees, minutes, hemispheres)
        else
            rval <- list(degrees, minutes, seconds, hemispheres)
    } else if (type == "string") {
        if (noMinutes)
            rval <- sprintf("%02d %s", degrees, hemispheres)
        else if (noSeconds)
            rval <- sprintf("%02d %02d %s", degrees, minutes, hemispheres)
        else
            rval <- sprintf("%02d %02d %04.2f %s", degrees, minutes, seconds, hemispheres)
    } else if (type == "expression") {
        n <- length(degrees)
        rval <- vector("expression", n)
        for (i in 1:n) {
            if (noMinutes) {
                rval[i] <- as.expression(substitute(d*degree*hemi,
                                                    list(d=degrees[i],
                                                         hemi=hemispheres[i])))
            } else if (noSeconds) {
                rval[i] <- as.expression(substitute(d*degree*phantom(.)*m*minute*hemi,
                                                    list(d=degrees[i],
                                                         m=sprintf("%02d", minutes[i]),
                                                         hemi=hemispheres[i])))
            } else {
                rval[i] <- as.expression(substitute(d*degree*phantom(.)*m*minute*phantom(.)*s*second*hemi,
                                                    list(d=degrees[i],
                                                         m=sprintf("%02d", minutes[i]),
                                                         s=sprintf("%02d", seconds[i]),
                                                         hemi=hemispheres[i])))
            }
        }
    }
    rval
}

map2lonlat <- function(xusr, yusr, tolerance=1e-5)
{
    n <- length(xusr)
    if (length(yusr) != n)
        error("lengths of x and y must match")
    lon <- rep(NA, n)
    lat <- rep(NA, n)
    ## FIXME: will this trick with the .Last.projection always work??
    or <- get(".Last.projection", envir = globalenv())$orientation # lat lon somethingElse
    for (i in 1:n) {
        t <- try({
            error <- FALSE
            o <- optim(c(or[2], or[1]), function(x) {xy<-mapproject(x[1], x[2]); error <<- xy$error; sqrt((xy$x-xusr[i])^2+(xy$y-yusr[i])^2)}, control=list(abstol=1e-4))
            ##cat(sprintf("%.2f %.2f [%.5e]\n", o$par[1], o$par[2], o$value))
            if (o$convergence == 0 && !error && o$value < 100+tolerance) {
                lonlat <- o$par
                lon[i] <- lonlat[1]
                lat[i] <- lonlat[2]
            }
        }, silent=TRUE)
    }
    ## bad <- lat < -90 | lat > 90 | lon < -180 | lon > 180
    ## lon[bad] <- NA
    ## lat[bad] <- NA
    lon <- ifelse(lon < -180, lon+360, lon)
    lon <- ifelse(lon >  180, lon-360, lon)
    list(longitude=lon, latitude=lat)
}

mapPolygon <- function(longitude, latitude, density=NULL, angle=45,
                       border=NULL, col=NA, lty=par('lty'), ..., fillOddEven=FALSE)
{
    if (inherits(longitude, "coastline")) {
        latitude <- longitude[['latitude']]
        longitude <- longitude[['longitude']]
    }
    n <- length(longitude)
    xy <- mapproject(longitude, latitude)
    ##bad <- is.na(xy$x) | is.na(xy$y)
    ##polygon(xy$x[!bad], xy$y[!bad],
    ##        density=density, angle=angle, border=border, col=col, lty=lty, ..., fillOddEven=fillOddEven)
    polygon(xy$x, xy$y,
            density=density, angle=angle, border=border, col=col, lty=lty, ..., fillOddEven=fillOddEven)
}

mapImage <- function(longitude, latitude, z)
{
    ni <- dim(z)[1]
    nj <- dim(z)[2]
    dlongitude <- longitude[2] - longitude[1]
    dlatitude <- latitude[2] - latitude[1]
    cols <- oceColorsJet(100)
    zmin <- min(z, na.rm=TRUE)
    zmax <- max(z, na.rm=TRUE)
    zrange <- zmax - zmin
    for (i in 1:ni) {
        for (j in 1:nj) {
            col <- cols[100 * (z[i,j] - zmin)/ zrange]
            mapPolygon(longitude[i]+dlongitude*c(0, 1, 1, 0, 0),
                       latitude[j]+dlatitude*c(0, 0, 1, 1, 0),
                       col=col, border=NA)
        }
    }
}

## http://williams.best.vwh.net/avform.htm#Intermediate
## interpreted by CR; typo corrected by DK
geodGc <- function(lon1, lat1, lon2, lat2, dmax)
{
    pi <- 4 * atan2(1, 1)
    rlat1 <- pi * lat1 / 180
    rlat2 <- pi * lat2 / 180
    rlon1 <- pi * lon1 / 180
    rlon2 <- pi * lon2 / 180
    d <- 2 * asin(sqrt((sin((rlat1 - rlat2)/2))^2
                       + cos(rlat1) * cos(rlat2) * (sin((rlon1 - rlon2)/2))^2))
    ddeg <- d * 180 / pi
    if (ddeg < dmax) {
        rval <- list(longitude=c(lon1, lon2), latitude=c(lat1, lat2))
    } else {
        f <- seq(0, 1, length.out=ceiling(1+ddeg/dmax))
        A <- sin((1 - f) * d) / sin(d)
        B <- sin(f * d) / sin(d)
        x  <-  A * cos(rlat1) * cos(rlon1) + B * cos(rlat2) * cos(rlon2)
        y  <-  A * cos(rlat1) * sin(rlon1) + B * cos(rlat2) * sin(rlon2)
        z  <-  A * sin(rlat1)              + B * sin(rlat2)
        lat <- atan2(z,sqrt(x^2+y^2))
        lon <- atan2(y,x)
        rval <- list(longitude=180/pi*lon, latitude=180/pi*lat)
    }
    rval
}

