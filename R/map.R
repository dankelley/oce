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
    if (!exists(".Last.projection") || .Last.projection()$proj == "")
        stop("must create a map first, with mapPlot()\n")
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
    xx <- seq_along(longitude)
    yy <- seq_along(latitude)
    if (length(xx) > 1 && diff(longitude[1:2]) < 0) {
        xx <- rev(xx)
        z <- z[xx,]
        ##cat("flipped in x\n")
    }
    if (length(yy) > 1 && diff(latitude[1:2]) < 0) {
        yy <- rev(yy)
        z <- z[,yy]
        ##cat("flipped in y\n")
    }
    for (ilevel in 1:nlevels) {
        cl <- contourLines(x=longitude[xx],
                           y=latitude[yy],
                           z=z, levels=levels[ilevel])
        for (segment in 1:length(cl)) {
            if (length(cl) > 0) {
                mapLines(cl[[segment]]$x, cl[[segment]]$y,
                         lty=lty[ilevel], lwd=lwd[ilevel], col=col[ilevel])
                ## if (segment == 1) {
                ##     cat(str(cl[[segment]]$x))
                ##     cat(str(cl[[segment]]$y))
                ## }
            }
        }
    }
    ## FIXME: labels, using labcex and vfont
}

mapLongitudeLatitudeXY <- function(longitude, latitude)
{
    if (missing(longitude))
        stop("must give 'longitude' and possibly 'latitude'")
    if (!exists(".Last.projection") || .Last.projection()$proj == "")
        stop("must create a map first, with mapPlot()\n")
    if (!missing(longitude) && ("data" %in% slotNames(longitude))) {
        tmp <- longitude@data
        if (("longitude" %in% names(tmp)) && ("latitude" %in% names(tmp))) {
            latitude <- tmp$latitude
            longitude <- tmp$longitude
        }
    }
    proj <- mapproject(longitude, latitude)
    list(x=proj$x, y=proj$y) # if other properties prove helpful, may add them
} 

##mapLegend <- function(longitude, latitude, ...)
##{
##    proj <- mapproject(longitude, latitude)
##    legend(x=proj$x, y=proj$y, ...)
##}

mapPlot <- function(longitude, latitude, longitudelim, latitudelim, grid=TRUE,
                    bg, fill=NULL, type='l', axes=TRUE, drawBox=TRUE, showHemi=TRUE,
                    polarCircle=0,
                    projection="mollweide", parameters=NULL, orientation=NULL,
                    debug=getOption("oceDebug"),
                    ...)
{
    oceDebug(debug, "mapPlot(longitude, latitude", 
            ", longitudelim=",
             if (missing(longitudelim)) "(missing)" else c("c(", paste(longitudelim, collapse=","), ")"),
             ", longitudelim=",
             if (missing(latitudelim)) "(missing)" else c("c(", paste(latitudelim, collapse=","), ")"),
             ", ...) {\n", unindent=1)
    if (missing(longitude)) {
        data("coastlineWorld", envir=environment())
        longitude <- get("coastlineWorld")
    }
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
    if (!missing(latitudelim) && 0 == diff(latitudelim)) stop("lattudelim must contain two distinct values")
    if (!missing(longitudelim) && 0 == diff(longitudelim)) stop("longitudelim must contain two distinct values")
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

        if (debug > 0 && sum(bad))
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
    ## FIXME: meridians and zones should be added later because they can change depending
    ## FIXME: on the 'pretty' operation below.
    ##if (grid[2]) mapMeridians(seq(-90, 90, grid[2]))
    ##if (grid[1]) mapZones(seq(-180, 180, grid[1]), polarCircle=polarCircle)
    if (drawBox)
        box()
    drawGrid <- (is.logical(grid[1]) && grid[1]) || grid[1] > 0
    if (axes && drawGrid) {
        options <- options('warn') # optimize() makes warnings for NA, which we will get
        options(warn=-1) 
        inc <- if (is.logical(grid[2]) && grid[2]) 25 else grid[2]
        latlabs <- seq(-90, 90, inc)
        if (!missing(latitudelim)) { 
            incBest <- diff(pretty(latitudelim, n=4, n.min=3))[1]
            oceDebug(debug, "latitudelim:", latitudelim, ", inc:", inc, ", incBest:", incBest, "\n")
            if (inc / incBest > 3) {
                ## extend range because the lims may not agree given the plot geometry
                latSmall <- mean(latitudelim) - 5 * (latitudelim[2] - latitudelim[1])
                latLarge <- mean(latitudelim) + 5 * (latitudelim[2] - latitudelim[1])
                latlabs <- pretty(c(latSmall, latLarge), n=4, n.min=3)
                ##latlabs <- pretty(latitudelim, n=2, n.min=1)
                grid[2] <- diff(latlabs[1:2])
            }
        }
        if ((is.logical(grid[1]) && grid[1]) || grid[1] > 0) {
            mapMeridians(latlabs)
        }
        oceDebug(debug, "latlabs:", latlabs, "\n")
        usr <- par('usr')
        labelAt <- NULL
        lab <- vector("expression", length(latlabs))
        nlab <- 0
        lastAtY <- NA
        ##cat("inc:", inc, "\n")
        if (drawGrid) {
            for (latlab in latlabs) {
                ##cat("latlab:", latlab, "\n")
                if (-90 <= latlab && latlab <= 90) {
                    try({
                        o <- optimize(function(lon) abs(mapproject(lon, latlab)$x-usr[1]),
                                      lower=-180, upper=180)
                        if (o$objective > 0.01)
                            next
                        lonlab <- o$minimum
                        at <- mapproject(lonlab, latlab)
                        if (usr[3] < at$y && at$y < usr[4]) {
                            ##cat("lonlab:", lonlab, " INSIDE\n")
                            labelAt <- c(labelAt, at$y)
                            nlab <- nlab + 1
                            lab[nlab] <- formatPosition(latlab, isLat=TRUE, type="expression", showHemi=showHemi)
                            oceDebug(debug, "  y axis: ",
                                     formatPosition(latlab, isLat=TRUE, type="string", showHemi=showHemi),
                                     "at$y", at$y, "lastAtY", lastAtY, "\n")
                            if (debug>90) {
                                mtext(formatPosition(latlab, isLat=TRUE, type="expression", showHemi=showHemi),
                                      line=par('mgp')[2]-abs(par('tcl')), # no ticks, so move closer
                                      side=2, at=at$y, srt=90, cex=par('cex'), ...) # how to rotate?
                                warning("DEVELOPER message: since debug>90, axis labels were drawn with 'mtext' instead of 'axis'")
                            }
                            lastAtY <- at$y
                        } else {
                            ##cat("lonlab:", lonlab, "OUTSIDE\n")
                        }
                    }, silent=TRUE)
                }
            }
        }
        if (nlab > 0) {
            if (debug<=90) { # FIXME: 2014-01-09 remove this eventually
                axis(2, at=labelAt, labels=lab[1:nlab], col.ticks="lightgray",
                     mgp=getOption('oceMgp'))
            }
        }
        labelAt <- NULL
        inc <- if (is.logical(grid[1]) && grid[1]) 15 else grid[1]
        lonlabs <- seq(-180, 180, inc)
        if (!missing(longitudelim)) { 
            oceDebug(debug, "longitudelim:", longitudelim, ", inc:", inc, ", incBest:", incBest, "\n")
            incBest <- diff(pretty(longitudelim, n=4, n.min=3))[1]
            if (inc / incBest > 3) {
                ## extend range because the lims may not agree given the plot geometry
                lonSmall <- mean(longitudelim) - 5 * (longitudelim[2] - longitudelim[1])
                lonLarge <- mean(longitudelim) + 5 * (longitudelim[2] - longitudelim[1])
                lonlabs <- pretty(c(lonSmall, lonLarge), n=4, n.min=3)
                ##lonlabs <- pretty(longitudelim, n=2, n.min=1)
                grid[1] <- diff(lonlabs[1:2])
            }
        }
        ## Prevent labelling both 180W and 180E on top of each other (not sure how
        ## axis permits this, actually).
        oceDebug(debug, "lonlabs:", lonlabs, "\n")
        if ((is.logical(grid[2]) && grid[2]) || grid[2] > 0) {
            mapZones(lonlabs)
        }
        lab <- vector("expression", length(latlabs))
        nlab <- 0
        lastx <- NA
        dxMin <- (usr[2] - usr[1]) / 10
        mgp <- par('mgp')
        for (lonlab in lonlabs) {
            if (-180 <= lonlab && lonlab < 180) { # the limits are the lonlim
                try({
                    o <- optimize(function(lat) abs(mapproject(lonlab, lat)$y-usr[3]), lower=-89, upper=89)
                    if (o$object > 0.01)
                        next
                    latlab <- o$minimum
                    at <- mapproject(lonlab, latlab)
                    if (usr[1] < at$x && at$x < usr[2]) {
                        labelAt <- c(labelAt, at$x)
                        nlab <- nlab + 1
                        lab[nlab] <- formatPosition(lonlab, isLat=FALSE, type="expression", showHemi=showHemi)
                        if (is.na(lastx) || abs(at$x - lastx) > dxMin) {
                            if (debug>90) {
                                mtext(formatPosition(lonlab, isLat=FALSE, type="expression", showHemi=showHemi),
                                      side=1,
                                      line=mgp[2]-abs(par('tcl')), # no ticks, so move closer
                                      at=at$x, cex=par('cex'), ...)
                                warning("DEVELOPER message: since debug>90, axis labels were drawn with 'mtext' instead of 'axis'")
                            }
                            lastx <- at$x
                        }
                        oceDebug(debug, "  x axis: ",
                                 formatPosition(lonlab, isLat=FALSE, type="string", showHemi=showHemi),
                                 "at$x", at$x, "lastx", lastx, "\n")
                    }
                }, silent=TRUE)
            }
        }
        if (nlab > 0) {
            if (debug<=90) { # FIXME: 2014-01-09 remove this eventually
                axis(1, at=labelAt, labels=lab[1:nlab], col.ticks="lightgray",
                     mgp=getOption('oceMgp'))
            }
        }
        options(warn=options$warn) 
    }
    oceDebug(debug, "} # mapPlot()\n", unindent=1)
}

mapMeridians <- function(latitude, lty='solid', lwd=0.5*par('lwd'), col='darkgray', ...)
{
    if (missing(latitude))
        latitude <- TRUE
    if (is.logical(latitude)) {
        if (!latitude)
            return()
        latitude <- seq(-90, 90, 15)
    }
    usr <- par('usr')
    n <- 360                           # number of points on line
    for (l in latitude) {
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
        ##2014-01-09 dc <- as.numeric(quantile(d, 0.99, na.rm=TRUE)) # FIXME: criterion
        dc <- as.numeric(median(d, na.rm=TRUE))
        bad <- d > 3 * dc
        x[bad] <- NA                   # FIXME: add, don't replace
        y[bad] <- NA                   # FIXME: add, don't replace
        ## NB. used to check for points in region but when zoomed in closely, there may be none!
        ##if (length(x) & length(y) & any(usr[1] <= x & x <= usr[2] & usr[3] <= y & y <= usr[4], na.rm=TRUE)) {
        lines(x, y, lty=lty, lwd=lwd, col=col, ...)
        ##points(x, y, pch=20, col='red', cex=.7)
        ##}
    }
}

mapScalebar <- function(x, y=NULL, length,
                        lwd=4*par("lwd"), cex=1.2*par("cex"),
                        col="black")
{
    if (!is.null(y))
        stop("y must be NULL in this (early) version of mapScalebar()\n")
    if (missing(x))
        x <- "topleft"
    else if (!match.arg(x, "topleft"))
        stop("x must be \"topleft\"in this (early) version of mapScalebar()\n")
    usr <- par('usr')

    ## determine scale from centre of region
    x0 <- 0.5 * (usr[1] + usr[2])
    y0 <- 0.5 * (usr[3] + usr[4])
    dusr <- 0.001 * (usr[2] - usr[1])
    x1 <- x0 + dusr
    ##cat("dusr:", dusr, "\n")
    y1 <- y0
    lonlat0 <- map2lonlat(x0, y0)
    lonlat1 <- map2lonlat(x1, y1)
    dkm <- geodDist(lonlat0$longitude, lonlat0$latitude,
                      lonlat1$longitude, lonlat1$latitude)
    ##cat("dkm:", dkm, "\n")

    kmPerUsr <- dkm / dusr
    ##cat("kmPerUsr:", kmPerUsr, "\n")
    
    if (missing(length))
        length <- diff(pretty(kmPerUsr*(usr[4]-usr[3]))[1:2])
    ##cat("length:", length, "\n")
    frac <- length / kmPerUsr
    ##cat("frac:", frac, "\n")
    xBar <- usr[1] + 0.05 * (usr[2] - usr[1])
    yBar <- usr[4] - 0.05 * (usr[4] - usr[3])
    lines(xBar + c(0, frac), rep(yBar, 2), lwd=lwd, col=col)
    text(xBar + frac / 2, yBar, pos=1, sprintf("%.0f km", length),
         cex=cex, col=col)
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




mapZones <- function(longitude, polarCircle=0, lty='solid', lwd=0.5*par('lwd'), col='darkgray', ...)
{
    if (missing(longitude))
        longitude <- TRUE
    if (is.logical(longitude)) {
        if (!longitude)
            return()
        longitude <- seq(-180, 180, 15)
    }
    if (polarCircle < 0 || polarCircle > 90)
        polarCircle <- 0
    usr <- par('usr')
    n <- 360                           # number of points on line
    for (l in longitude) {
        ## FIXME: should use mapLines here
        line <- mapproject(rep(l, n), seq(-90+polarCircle, 90-polarCircle, length.out=n))
        x <- line$x
        y <- line$y
        ok <- !is.na(x) & !is.na(y)
        x <- x[ok]
        y <- y[ok]
        ## NB. used to check for points in region but when zoomed in closely, there may be none!
        ##if (length(x) & any(usr[1] <= x & x <= usr[2] & usr[3] <= y & y <= usr[4], na.rm=TRUE)) {
        lines(x, y, lty=lty, lwd=lwd, col=col, ...)
        ##}
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
    DX <- usr[2] - usr[1]
    if (any(usr[1] <= xy$x[ok] & xy$x[ok] <= usr[2] & usr[3] <= xy$y[ok] & xy$y[ok] <= usr[4])) {
        dx <- c(0, abs(diff(xy$x, na.rm=TRUE)))
        bad <- dx / DX > 0.5
        if (any(bad, na.rm=TRUE)) { # FIXME: a kludge that may be problematic
            xy$x[bad] <- NA
        }
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

formatPosition <- function(latlon, isLat=TRUE, type=c("list", "string", "expression"), showHemi=TRUE)
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
    if (showHemi) {
        hemispheres <- if (isLat) ifelse(signs>0, "N", "S") else ifelse(signs>0, "E", "W")
        hemispheres[signs==0] <- ""
    } else {
        hemispheres <- rep("", length(signs))
    }
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
                ##rval[i] <- as.expression(substitute(d*degree*phantom(.)*m*minute*hemi,
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
    ## The first of the following is ok in R 2.15 but the second is needed in R 3.0.1;
    ## see http://github.com/dankelley/oce/issues/346 for more on this issue.
    t <- try({
            or <- get(".Last.projection", envir = globalenv())$orientation
    }, silent=TRUE)
    if (class(t) == "try-error") {
        or <- .Last.projection()$orientation # was as in the above commented-out line until 2013-10-10
    }
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
    if ("data" %in% slotNames(longitude) && # handle e.g. 'coastline' class
        2 == sum(c("longitude","latitude") %in% names(longitude@data))) {
        latitude <- longitude@data$latitude
        longitude <- longitude@data$longitude
    }
    if (2 == sum(c("longitude", "latitude") %in% names(longitude))) {
        latitude <- longitude$latitude
        longitude <- longitude$longitude
    }
    n <- length(longitude)
    xy <- mapproject(longitude, latitude)
    ##bad <- is.na(xy$x) | is.na(xy$y)
    ##polygon(xy$x[!bad], xy$y[!bad],
    ##        density=density, angle=angle, border=border, col=col, lty=lty, ..., fillOddEven=fillOddEven)
    polygon(xy$x, xy$y,
            density=density, angle=angle, border=border, col=col, lty=lty, ..., fillOddEven=fillOddEven)
}

mapImage <- function(longitude, latitude, z, zlim, zclip=FALSE,
                     breaks, col, colormap, border=NA,
                     lwd=par("lwd"), lty=par("lty"),
                     filledContour=FALSE, missingColor=NA, debug=getOption("oceDebug"))
{
    if (!exists(".Last.projection") || .Last.projection()$proj == "")
        stop("must create a map first, with mapPlot()\n")
    oceDebug(debug, "mapImage(..., ",
             " missingColor='", missingColor, "', ",
             " filledContour=", filledContour, ", ",
             ", ...) {\n", sep="", unindent=1)
 
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
    if (!missing(colormap)) { # takes precedence over breaks and col
        breaks <- colormap$breaks
        col <- colormap$col
        missingColor <- colormap$missingColor
    } else {
        if (!breaksGiven) {
            small <- .Machine$double.eps
            zrange <- range(z, na.rm=TRUE)
            if (missing(zlim)) {
                if (missing(col)) {
                    breaks <- pretty(zrange+small*c(-1,1), n=10)
                    ## FIXME: the extension of the breaks is to try to avoid missing endpoints
                    if (breaks[1] < zrange[1])
                        breaks[1] <- zrange[1] * (1 - small)
                    if (breaks[length(breaks)] > zrange[2])
                        breaks[length(breaks)] <- zrange[2] * (1 + small)
                } else {
                    breaks <- seq(zrange[1]-small, zrange[2]+small,
                                  length.out=if(is.function(col))128 else 1+length(col))
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
    }
    ni <- dim(z)[1]
    nj <- dim(z)[2]
    dlongitude <- longitude[2] - longitude[1] # FIXME: incorrect for irregular grids
    dlatitude <- latitude[2] - latitude[1]
    zmin <- min(z, na.rm=TRUE)
    zmax <- max(z, na.rm=TRUE)
    zrange <- zmax - zmin
    usr <- par('usr')
    xmin <- usr[1]
    xmax <- usr[2]
    ymin <- usr[3]
    ymax <- usr[4]
    allowedSpan <- (xmax - xmin) / 5   # KLUDGE: avoid lines crossing whole domain
    small <- .Machine$double.eps
    if (zclip) {
        oceDebug(debug, "using missingColor for out-of-range values\n")
        z[z < zlim[1]] <- NA
        z[z > zlim[2]] <- NA
    } else {
        if (!missing(zlim)) {
            oceDebug(debug, "using range colours for out-of-range values\n")
            z[z <= zlim[1]] <- zlim[1] * (1 + small)
            z[z >= zlim[2]] <- zlim[2] * (1 - small)
        }
    }
    if (debug != 99) {                 # test new method (much faster)
        ## Construct polygons centred on the specified longitudes and latitudes.  Each
        ## polygon has 5 points, four to trace the boundary and a fifth that is (NA,NA),
        ## to signal the end of the polygon.  The z values (and hence the colours)
        ## map one per polygon.
        poly <- .Call("map_assemble_polygons", longitude, latitude, z,
                      NAOK=TRUE, PACKAGE="oce")
        ## The docs on mapproject say it needs -ve longitude for degW, but it works ok without that
        ##if (max(poly$longitude, na.rm=TRUE) > 180) {
        ##    warning("shifting longitude\n")
        ##    poly$longitude <- ifelse(poly$longitude > 180, poly$longitude - 360, poly$longitude)
        ##}
        xy <- mapproject(poly$longitude, poly$latitude)
        ## map_check_polygons tries to fix up longitude cut-point problem, which
        ## otherwise leads to lines crossing the graph horizontally because the
        ## x value can sometimes alternate from one end of the domain to the otherr
        ## because (I suppose) of a numerical error.
        Z <- matrix(z)
        r <- .Call("map_check_polygons", xy$x, xy$y, poly$z,
                   diff(par('usr'))[1:2]/5, par('usr'),
                   NAOK=TRUE, PACKAGE="oce")
        colorLookup <- function (ij) {
            if (is.na(Z[ij]))
                return(missingColor)
            w <- which(Z[ij] < breaks * (1 + small))
            if (length(w) && w[1] > 1) col[-1 + w[1]] else missingColor
        }
        col <- sapply(1:(ni*nj), colorLookup)
        polygon(xy$x[r$okPoint&!r$clippedPoint], xy$y[r$okPoint&!r$clippedPoint],
                col=col[r$okPolygon&!r$clippedPolygon],
                border=border, lwd=lwd, lty=lty, fillOddEven=FALSE)
    } else {
        for (i in 1:ni) {
            for (j in 1:nj) {
                xy <- mapproject(longitude[i]+dlongitude*c(-0.5, 0.5, 0.5, -0.5),
                                 latitude[j]+dlatitude*c(-0.5, -0.5, 0.5, 0.5))
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
                    thiscol <- col[-1 + which(zz < breaks * (1 + small))[1]]
                    polygon(xy$x, xy$y, col=thiscol, border=border,
                            lwd=lwd, lty=lty, fillOddEven=FALSE)
                } else if (!is.null(missingColor)) {
                    polygon(xy$x, xy$y, col=missingColor, border=border,
                            lwd=lwd, lty=lty, fillOddEven=FALSE)
                }
            }
        }
    }
    oceDebug(debug, "} # mapImage()\n", unindent=1)
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

lonlat2utm <- function(longitude, latitude, km=TRUE)
{
    ## Code from [wikipedia](http://en.wikipedia.org/wiki/Universal_Transverse_Mercator_coordinate_system)
    longitude <- ifelse(longitude < 0, longitude+360, longitude)
    rpd <- atan2(1, 1) / 45
    lambda <- longitude * rpd
    phi <- latitude * rpd
    a <- 6378.137                          # earth radius in WSG84 (in km for these formulae)
    f <- 1 / 298.257223563                 # flatening
    n <- f / (2 - f)
    A <- (a / (1 + n)) * (1 + n^2/4 + n^4/64)
    t <- sinh(atanh(sin(phi)) - (2*sqrt(n))/(1+n) * atanh((2*sqrt(n))/(1+n)*sin(phi)))
    zone <- floor((180+longitude)/6)  # FIXME: this works for zone but not positive its ok
    zone <- ifelse(zone > 60, zone-60, zone)
    lambda0 <- rpd * (zone * 6 - 183)
    xiprime <- atan(t / cos(lambda - lambda0))
    etaprime <- atanh(sin(lambda - lambda0) / sqrt(1 + t^2))
    alpha1 <- (1/2)*n - (2/3)*n^2 + (5/16)*n^3
    alpha2 <- (13/48)*n^2 - (3/5)*n^3
    alpha3 <- (61/240)*n^3
    ## sigma and tau needed only if calculating k and gamma, which we are not.
    ## sigma <- 1 + 2*(  alpha1*cos(2*xiprime)*cosh(2*etaprime) +
    ##                 2*alpha2*cos(4*xiprime)*cosh(4*etaprime) +
    ##                 3*alpha3*cos(6*xiprime)*cosh(6*etaprime))
    ## tau <-       2*(  alpha1*sin(2*xiprime)*sinh(2*etaprime) +
    ##                 2*alpha2*sin(4*xiprime)*sinh(4*etaprime) +
    ##                 3*alpha3*sin(6*xiprime)*sinh(6*etaprime))
    k0 <- 0.9996
    E0 <- 500                              # km
    E <- E0 + k0 * A * (etaprime + (alpha1*cos(2*xiprime)*sinh(2*etaprime)+
                                    alpha2*cos(4*xiprime)*sinh(4*etaprime)+
                                    alpha3*cos(6*xiprime)*sinh(6*etaprime)))
    N0 <- ifelse(latitude>0, 0, 10000)
    N <- N0 + k0 * A * (xiprime  + (alpha1*sin(2*xiprime)*cosh(2*etaprime)+
                                    alpha2*sin(4*xiprime)*cosh(4*etaprime)+
                                    alpha3*sin(6*xiprime)*cosh(6*etaprime)))
    easting <- if (km) E else 1000 * E
    northing <- if (km) N else 1000 * N
    list(easting=easting, northing=northing, zone=zone,
         hemisphere=ifelse(latitude>0, "N", "S"))
}

utm2lonlat <- function(easting, northing, zone=1, hemisphere="N", km=TRUE) 
{
    ## Code from [wikipedia](http://en.wikipedia.org/wiki/Universal_Transverse_Mercator_coordinate_system)
    a <- 6378.137                          # earth radius in WSG84 (in km for these formulae)
    f <- 1 / 298.257223563                 # flatening
    n <- f / (2 - f)
    A <- (a / (1 + n)) * (1 + n^2/4 + n^4/64)
    beta1 <- (1/2)*n - (2/3)*n^2 + (37/96)*n^3
    beta2 <- (1/48)*n^2 + (1/15)*n^3
    beta3 <- (17/480)*n^3
    delta1 <- 2*n - (2/3)*n^2 - 2*n^3
    delta2 <- (7/3)*n^2 - (8/5)*n^3
    delta3 <- (56/15)*n^3
    if (!km) {
        northing <- northing / 1000
        easting <- easting / 1000
    }
    N0 <- if (hemisphere=="N") 0 else 10000
    k0 <- 0.9996
    E0 <- 500                              # km
    xi <- (northing - N0) / (k0 * A)
    eta <- (easting - E0) / (k0 * A)
    xiprime <-   xi -   (beta1*sin(2*xi)*cosh(2*eta) +  beta2*sin(4*xi)*cosh(4*eta) +  beta3*sin(6*xi)*cosh(6*eta))
    etaprime <- eta -   (beta1*cos(2*xi)*sinh(2*eta) +  beta2*cos(4*xi)*sinh(4*eta) +  beta3*cos(6*xi)*sinh(6*eta))
    ## sigmaprime and tauprime not needed in present calculation
    ##sigmaprime <- 1 - 2*(beta1*cos(2*xi)*cosh(2*eta) +2*beta2*cos(4*xi)*cosh(4*eta) +3*beta3*cos(6*xi)*cosh(6*eta))
    ##tauprime <-       2*(beta1*sin(2*xi)*sinh(2*eta) +2*beta2*sin(4*xi)*sinh(4*eta) +3*beta3*sin(6*xi)*sinh(6*eta))
    chi <- asin(sin(xiprime)/cosh(etaprime)) # Q: in deg or radian?
    phi <- chi + (delta1*sin(2*chi) + delta2*sin(4*chi) + delta3*sin(6*chi))
    latitude <- 45 * phi / atan2(1, 1)
    lambda0 <- zone * 6 - 183
    longitude <- lambda0 + 45/atan2(1,1)*atan(sinh(etaprime) / cos(xiprime))
    list(longitude=longitude, latitude=latitude)
}

