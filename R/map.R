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
    if ("data" %in% slotNames(longitude) && # handle e.g. 'topo' class
        3 == sum(c("longitude","latitude","z") %in% names(longitude@data))) {
        z <- longitude@data$z
        latitude <- longitude@data$latitude
        longitude <- longitude@data$longitude
    }
    nlevels <- length(levels)
    col <- rep(col, nlevels)
    lty <- rep(lty, nlevels)
    lwd <- rep(lwd, nlevels)
    for (ilevel in 1:nlevels) {
        cl <- contourLines(x=longitude, y=latitude, z=z, levels=levels[ilevel])
        for (segment in 1:length(cl)) {
            if (length(cl) > 0) {
                mapLines(cl[[segment]]$x, cl[[segment]]$y, lty=lty[ilevel], lwd=lwd[ilevel], col=col[ilevel])
            }
        }
    }
    ## FIXME: labels, using labcex and vfont
}

mapPlot <- function(longitude, latitude, longitudelim, latitudelim, grid=TRUE,
                    bg, fill=NULL, type='l', axes=TRUE, drawBox=TRUE,
                    polarCircle=0,
                    projection="mollweide", parameters=NULL, orientation=NULL,
                    debug=getOption("oceDebug"),
                    ...)
{
    oceDebug(debug, "\b\bmapPlot(...) {\n")
    if ("data" %in% slotNames(longitude) && # handle e.g. 'coastline' class
        2 == sum(c("longitude","latitude") %in% names(longitude@data))) {
        latitude <- longitude@data$latitude
        longitude <- longitude@data$longitude
    }
    if (length(grid) > 2)
        grid <- grid[1:2]
    if (length(grid) == 1)
        grid <- rep(grid[1], 2)
    drawGrid <- (is.logical(grid[1]) && grid[1]) || (is.numeric(grid[1]) && grid[1] > 0)
    if (is.logical(grid[1]) && grid[1])
        grid <- rep(15, 2)
    xy <- mapproject(longitude, latitude,
                     projection=projection, parameters=parameters, orientation=orientation)
    limitsGiven <- !missing(latitudelim) && !missing(longitudelim)
    x <- xy$x
    y <- xy$y
    if (projection %in% c('mollweide', 'polyconic')) { ## FIXME: should probably add other proj here
        ## trim wild points [github issue 227]
        ## FIXME: below is a kludge to avoid weird horiz lines; it
        ## FIXME: would be better to complete the polygons, so they 
        ## FIXME: can be filled.  It might be smart to do this in C
        d <- c(0, sqrt(diff(x)^2 + diff(y)^2))
        d[!is.finite(d)] <- 0          # FIXME: ok?
        ##dc <- as.numeric(quantile(d, 1-100*(1/3/length(x)), na.rm=TRUE)) # FIXME: criterion
        ##bad <- d > dc
        bad <- (d / diff(range(x, na.rm=TRUE))) > 0.1
        ## FIXME: this should finish off polygons, but that is a bit tricky, e.g.
        ## FIXME: should we create a series of points to a trace along the edge 
        ## FIXME: the visible earth?
        if (sum(bad))
            warning("mapPlot(): trimming spurious edge-to-edge lines; filling may be inaccurate", call.=FALSE)
        x[bad] <- NA                       
        y[bad] <- NA
    }

    if (limitsGiven) {
        box <- mapproject(c(longitudelim[1], longitudelim[1],
                            longitudelim[2], longitudelim[2]),
                          c(latitudelim[1], latitudelim[2],
                            latitudelim[2], latitudelim[1]))
        plot(x, y, type=type,
             xlim=range(box$x, na.rm=TRUE), ylim=range(box$y, na.rm=TRUE),
             xlab="", ylab="", asp=1, axes=FALSE, ...)
    } else {
        plot(x, y, type=type,
             xlab="", ylab="", asp=1, axes=FALSE, ...)
    }
    if (!is.null(fill))
        polygon(x, y, col=fill, ...)
    usr <- par('usr')
    mapMeridians(grid[2])
    mapZones(grid[1], polarCircle=polarCircle)
    if (drawBox)
        box()
    drawGrid <- (is.logical(grid[1]) && grid[1]) || grid[1] > 0
    if (axes && drawGrid) {
        options <- options('warn') # optimize() makes warnings for NA, which we will get
        options(warn=-1) 
        inc <- if (is.logical(grid[2]) && grid[2]) 25 else grid[2]
        usr <- par('usr')
        labelAt <- NULL
        labels <- NULL
        lastAtY <- NA
        ##cat("inc:", inc, "\n")
        if (drawGrid) {
            for (latlab in seq(-90, 90, inc)) {
                oceDebug(debug, "latlab", latlab, "\n")
                if (-90 <= latlab && latlab <= 90) {
                    try({
                        o <- optimize(function(lon) abs(mapproject(lon, latlab)$x-usr[1]),
                                      lower=-180, upper=180)
                        if (o$object > 0.01)
                            next
                        lonlab <- o$minimum
                        at <- mapproject(lonlab, latlab)
                        if (usr[3] < at$y && at$y < usr[4]) {
                            labelAt <- c(labelAt, at$y)
                            labels <- c(labels,
                                        formatPosition(latlab, isLat=TRUE, type="string", showHemi=TRUE))
                            oceDebug(debug, "  y axis: ",
                                     formatPosition(latlab, isLat=TRUE, type="string", showHemi=TRUE),
                                     "at$y", at$y, "lastAtY", lastAtY, "\n")
                            mtext(formatPosition(latlab, isLat=TRUE, type="expression", showHemi=TRUE),
                                  line=par('mgp')[2]-abs(par('tcl')), # no ticks, so move closer
                                  side=2, at=at$y, srt=90) # how to rotate?
                            lastAtY <- at$y
                        }
                    }, silent=TRUE)
                }
            }
        }
        labelAt <- NULL
        inc <- if (is.logical(grid[1]) && grid[1]) 15 else grid[1]
        labels <- NULL
        lastx <- NA
        dxMin <- (usr[2] - usr[1]) / 10
        mgp <- par('mgp')
        for (lonlab in seq(-180, 180, inc)) {
            if (-180 <= lonlab && lonlab <= 180) { # the limits are the lonlim
                try({
                    o <- optimize(function(lat) abs(mapproject(lonlab, lat)$y-usr[3]), lower=-89, upper=89)
                    if (o$object > 0.01)
                        next
                    latlab <- o$minimum
                    at <- mapproject(lonlab, latlab)
                                        #if (lonlab > -66 && lonlab < -64) browser()
                    if (usr[1] < at$x && at$x < usr[2]) {
                        labelAt <- c(labelAt, at$x)
                        labels <- c(labels,
                                    formatPosition(lonlab, isLat=FALSE, type="string", showHemi=TRUE))
                        if (is.na(lastx) || abs(at$x - lastx) > dxMin) {
                            mtext(formatPosition(lonlab, isLat=FALSE, type="expression", showHemi=TRUE),
                                  side=1,
                                  line=mgp[2]-abs(par('tcl')), # no ticks, so move closer
                                  at=at$x)
                            lastx <- at$x
                        }
                        oceDebug(debug, "  x axis: ",
                                 formatPosition(lonlab, isLat=FALSE, type="string", showHemi=TRUE),
                                 "at$x", at$x, "lastx", lastx, "\n")
                    }
                }, silent=TRUE)
            }
        }
        options(warn=options$warn) 
    }
    oceDebug(debug, "\b\b} # mapPlot(...)\n")
}

mapMeridians <- function(lat, lty='dotted', lwd=par('lwd'), col='lightgray', ...)
{
    if (missing(lat))
        lat <- TRUE
    if (is.logical(lat)) {
        if (!lat)
            return()
        lat <- 15
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
        if (0 == length(x))
            next
        y <- y[ok]
        if (0 == length(y))
            next
        ## FIXME: below is a kludge to avoid weird horiz lines; it
        ## FIXME: would be better to complete the polygons, so they 
        ## FIXME: can be filled.  It might be smart to do this in C
        d <- c(0, sqrt(diff(x)^2 + diff(y)^2))
        d[!is.finite(d)] <- 0
        if (0 == length(d))
            next
        dc <- as.numeric(quantile(d, 0.99, na.rm=TRUE)) # FIXME: criterion
        bad <- d > dc
        x[bad] <- NA                   # FIXME: add, don't replace
        y[bad] <- NA                   # FIXME: add, don't replace
        if (length(x) & length(y) & any(usr[1] <= x & x <= usr[2] & usr[3] <= y & y <= usr[4], na.rm=TRUE)) {
            lines(x, y, lty=lty, lwd=lwd, col=col, ...)
        }
    }
}

mapZones <- function(lon, polarCircle=0, lty='dotted', lwd=par('lwd'), col='lightgray', ...)
{
    if (missing(lon))
        lon <- TRUE
    if (is.logical(lon)) {
        if (!lon)
            return()
        lon <- 15
    }
    if (length(lon) == 1)
        ##lon <- rep(seq(-180, 180, lon), each=360/lon)
        lon <- seq(-180, 180, lon)
    if (polarCircle < 0 || polarCircle > 90)
        polarCircle <- 0

    usr <- par('usr')
    n <- 360                           # number of points on line
    for (l in lon) {
        ## FIXME: should use mapLines here
        line <- mapproject(rep(l, n), seq(-90+polarCircle, 90-polarCircle, length.out=n))
        x <- line$x
        y <- line$y
        ok <- !is.na(x) & !is.na(y)
        x <- x[ok]
        y <- y[ok]
        if (length(x) & any(usr[1] <= x & x <= usr[2] & usr[3] <= y & y <= usr[4], na.rm=TRUE)) {
            lines(x, y, lty=lty, lwd=lwd, col=col, ...)
        }
    }
}

mapLines <- function(longitude, latitude, greatCircle=FALSE, ...)
{
    if ("data" %in% slotNames(longitude) && # handle e.g. 'coastline' class
        2 == sum(c("longitude","latitude") %in% names(longitude@data))) {
        latitude <- longitude@data$latitude
        longitude <- longitude@data$longitude
    }
    if (2 == sum(c("longitude", "latitude") %in% names(longitude))) {
        latitude <- longitude$latitude
        longitude <- longitude$longitude
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
    if ("data" %in% slotNames(longitude) && # handle e.g. 'coastline' class
        2 == sum(c("longitude","latitude") %in% names(longitude@data))) {
        latitude <- longitude@data$latitude
        longitude <- longitude@data$longitude
    }
    if (2 == sum(c("longitude", "latitude") %in% names(longitude))) {
        latitude <- longitude$latitude
        longitude <- longitude$longitude
    }
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
    hemispheres <- if (isLat) ifelse(signs>0, "N", "S") else ifelse(signs>0, "E", "W")
    hemispheres[signs==0] <- ""
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

mapLocator <- function(n=512, type='n', ...)
{
    xy <- locator(n, type, ...)
    rval <- map2lonlat(xy$x, xy$y)
    if (type == 'l')
        mapLines(rval$longitude, rval$latitude, ...)
    else if (type == 'p')
        mapPoints(rval$longitude, rval$latitude, ...)
    rval
}

map2lonlat <- function(xusr, yusr, tolerance=1e-4)
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
            ## FIXME: find better way to do the inverse mapping
            ## FIXME: here, try two starting points and pick best
            o <- optim(c(or[2], or[1]), function(x) {xy<-mapproject(x[1], x[2]); error <<- xy$error; sqrt((xy$x-xusr[i])^2+(xy$y-yusr[i])^2)}, control=list(abstol=1e-4))
            if (o$value > 100*tolerance) {
                oo <- optim(c(0, 0), function(x) {xy<-mapproject(x[1], x[2]); error <<- xy$error; sqrt((xy$x-xusr[i])^2+(xy$y-yusr[i])^2)}, control=list(abstol=1e-4))
                if (oo$value < o$value)
                    o <- oo
            }
            ##cat(sprintf("%.2f %.2f [%.5e]\n", o$par[1], o$par[2], o$value))
            if (o$convergence == 0 && !error && o$value < tolerance) {
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

mapImage <- function(longitude, latitude, z, zlim, zclip=FALSE, breaks, col,
                     filledContour=FALSE, missingColor=NULL, debug=getOption("oceDebug"))
{
    oceDebug(debug, "\b\bmapImage(..., ",
             " missingColor='", missingColor, "', ",
             " filledContour=", filledContour, ", ",
             ", ...) {\n", sep="")
 
    if (filledContour)
        warning("mapImage() cannot yet handle filledContour\n")
    if ("data" %in% slotNames(longitude)) {
        if (3 == sum(c("longitude","latitude","z") %in% names(longitude@data))) { # e.g. a topo object
            z <- longitude@data$z
            latitude <- longitude@data$latitude
            longitude <- longitude@data$longitude # destroys container
        }
    } else {
        names <- names(longitude)
        if ("x" %in% names && "y" %in% names && "z" %in% names) {
            z <- longitude$z
            latitude <- longitude$y
            longitude <- longitude$x   # destroys container
        }
    }
    breaksGiven <- !missing(breaks)
    if (!breaksGiven) {
        zrange <- range(z, na.rm=TRUE)
        if (missing(zlim)) {
            if (missing(col)) {
                breaks <- pretty(zrange, n=10)
                if (breaks[1] < zrange[1]) breaks[1] <- zrange[1]
                if (breaks[length(breaks)] > zrange[2]) breaks[length(breaks)] <- zrange[2]
            } else {
                breaks <- seq(zrange[1], zrange[2], length.out=if(is.function(col))128 else 1+length(col))
            }
            breaksOrig <- breaks
        } else {
            if (missing(col))
                breaks <- c(zlim[1], pretty(zlim), zlim[2])
            else
                breaks <- seq(zlim[1], zlim[2], length.out=if(is.function(col))128 else 1+length(col))
            breaksOrig <- breaks
            breaks[1] <- min(zrange[1], breaks[1])
            breaks[length(breaks)] <- max(breaks[length(breaks)], zrange[2])
        }
    } else {
        breaksOrig <- breaks
        if (1 == length(breaks)) {
            breaks <- pretty(z, n=breaks)
        }
    }
    if (missing(col))
        col <- oceColorsPalette(n=length(breaks)-1)
    if (is.function(col))
        col <- col(n=length(breaks)-1)
    ni <- dim(z)[1]
    nj <- dim(z)[2]
    dlongitude <- longitude[2] - longitude[1] # FIXME: incorrect for irregular grids
    dlatitude <- latitude[2] - latitude[1]
    zmin <- min(z, na.rm=TRUE)
    zmax <- max(z, na.rm=TRUE)
    zrange <- zmax - zmin
    lty <- par('lty')
    usr <- par('usr')
    xmin <- usr[1]
    xmax <- usr[2]
    ymin <- usr[3]
    ymax <- usr[4]
    allowedSpan <- (xmax - xmin) / 5   # KLUDGE: avoid lines crossing whole domain
    if (zclip) {
        oceDebug(debug, "using missingColour for out-of-range values\n")
        z[z < zlim[1]] <- NA
        z[z > zlim[2]] <- NA
    } else {
        if (!missing(zlim)) {
            oceDebug(debug, "using range colours for out-of-range values\n")
            small <- sqrt(.Machine$double.eps)
            z[z <= zlim[1]] <- zlim[1] + small
            z[z >= zlim[2]] <- zlim[2] - small
        }
    }
    for (i in 1:ni) {
        for (j in 1:nj) {
            ##col <- cols[100 * (z[i,j] - zmin)/ zrange]
            ## Speed improvement by 1.5X: avoid calling mapPolygon()
            xy <- mapproject(longitude[i]+dlongitude*c(0, 1, 1, 0, 0), latitude[j]+dlatitude*c(0, 0, 1, 1, 0))
            ## avoid lines crossing whole domain
            ## Speed improvement: skip offscale patches [FIXME: would be faster in latlon, skipping mapproject]
            if (xmax < min(xy$x, na.rm=TRUE))
                next
            if (max(xy$x, na.rm=TRUE) < xmin)
                next
            if (ymax < min(xy$y, na.rm=TRUE))
                next
            if (max(xy$y, na.rm=TRUE) < ymin)
                next
            if (abs(xy$x[1] - xy$x[2]) > allowedSpan)
                next
            zz <- z[i, j]
            if (is.finite(zz)) {
                thiscol <- col[-1 + which(zz < breaks)[1]]
                polygon(xy$x, xy$y, col=thiscol, lty=lty, border=NA, fillOddEven=FALSE)
            } else if (!is.null(missingColor)) {
                polygon(xy$x, xy$y, col=missingColor, lty=lty, border=NA, fillOddEven=FALSE)
            }
        }
    }
    oceDebug(debug, "\b\b} # mapImage()\n")
    invisible()
}

## http://williams.best.vwh.net/avform.htm#Intermediate
## interpreted by CR; typo corrected by DK
geodGc <- function(longitude, latitude, dmax)
{
    n <- length(latitude)
    if (n != length(longitude))
        stop("lengths of longitude and latude must match")
    d2r <- atan2(1, 1) / 45
    rlat <- d2r * latitude
    rlon <- d2r * longitude
    lon <- NULL
    lat <- NULL
    ## FIXME: if this is slow, may need to use C
    for (i in seq.int(1, n-1)) {
        d <- 2 * asin(sqrt((sin((rlat[i] - rlat[i+1])/2))^2
                           + cos(rlat[i]) * cos(rlat[i+1]) * (sin((rlon[i] - rlon[i+1])/2))^2))
        ddeg <- d / d2r
        if (ddeg < dmax) {
            lon <- c(lon, longitude[i])
            lat <- c(lat, latitude[i])
        } else {
            f <- seq(0, 1, length.out=ceiling(1 + ddeg/dmax))
            A <- sin((1 - f) * d) / sin(d)
            B <- sin(f * d) / sin(d)
            x <- A * cos(rlat[i]) * cos(rlon[i]) + B * cos(rlat[i+1]) * cos(rlon[i+1])
            y <- A * cos(rlat[i]) * sin(rlon[i]) + B * cos(rlat[i+1]) * sin(rlon[i+1])
            z <- A * sin(rlat[i])              + B * sin(rlat[i+1])
            lat <- atan2(z, sqrt(x^2+y^2)) / d2r
            lon <- atan2(y, x) / d2r
        }
    }
    lon <- c(lon, longitude[n])
    lat <- c(lat, latitude[n])
    list(longitude=lon, latitude=lat)
}

