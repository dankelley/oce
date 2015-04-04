## Author notes on proj4:
## 1. http://stackoverflow.com/questions/tagged/proj4
## 2. proj4 used by:
##    1. openstreetmap

.axis <- local({
    val <- list(longitude=NULL, latitude=NULL)
    function(new) if (!missing(new)) val <<- new else val
})

.Last.proj4  <- local({                # emulate mapproj
    val <- list(projection="")
    function(new) if(!missing(new)) val <<- new else val
})

usingProj4 <- function() 0 < nchar(.Last.proj4()$projection)

fixneg <- function(v)
{
    rval <- v
    for (i in seq_along(v)) {
        if (rval[i] == "0N") {
            rval[i] <- "0"
        } else if (rval[i] == "0E") {
            rval[i] <- "0"
        } else if ("-" == substr(v[i], 1, 1)) {
            ##cat("rval[i]=", rval[i], "\n")
            rval[i] <- gsub("^-", "", v[i])
            rval[i] <- gsub("E", "W", rval[i])
            rval[i] <- gsub("N", "S", rval[i])
            ##cat(" -> rval[i]=", rval[i], "\n")
        }
    }
    rval
}

mapAxis <- function(side=1:2, longitude=NULL, latitude=NULL,
                    tick=TRUE, line=NA, pos=NA, outer=FALSE, font=NA,
                    lty="solid", lwd=1, lwd.ticks=lwd, col=NULL, col.ticks=NULL,
                    hadj=NA, padj=NA, tcl=-0.3, debug=getOption("oceDebug"))
{
    oceDebug(debug, "mapAxis(side=c(", paste(side, collapse=","), ")",
             ", longitude=", if (length(longitude)) c(longitude[1], "...") else "NULL",
             ", latitude=", if (length(latitude)) c(latitude[1], "...") else "NULL",
             ") { \n", unindent=1, sep="")
    axis <- .axis()
    #if (debug > 0) print(axis)
    if (is.null(longitude) && is.null(latitude)) {
        longitude <- axis$longitude
        latitude <- axis$latitude
    }
    if (is.null(longitude) && is.null(latitude))
        return()
    ## if (is.null(axis$longitude)) oceDebug(debug, "should auto generate longitude grid and then axis\n")
    ## if (is.null(axis$latitude)) oceDebug(debug, "should auto generate latitude grid and then axis\n")
    ## if (missing(longitude)) longitude <- axis$longitude
    ## if (missing(latitude)) latitude <- axis$latitude
    if (missing(side))
        side <- 1:2
    usr <- par('usr')
    axisSpan <- max(usr[2]-usr[1], usr[4]-usr[3])
    MGP <- c(2, 0.5, 0)            # first item ignored since not writing "longitude" etc
    if (1 %in% side) {
        oceDebug(debug, "drawing axis on side 1\n")
        AT <- NULL
        LAB <- NULL
        for (lon in longitude) {
            if (debug > 3) oceDebug(debug, "check longitude", lon, "for axis on side=1\n")
            ## Seek a point at this lon that matches the lon-lat relationship on side=1
            o <- optimize(function(lat) abs(lonlat2map(lon,lat)$y-usr[3]),lower=-89.9999,upper=89.9999)
            if (is.na(o$objective) || o$objective > 0.01*axisSpan) {
                if (debug > 3) oceDebug(debug, "  longitude", lon, "is unmappable\n")
                next
            }
            ## Check that the point matches lat, as well as lon (this matters for full-globe)
            P <- lonlat2map(lon, o$minimum)
            ## oceDebug(debug, "Investigate point at x=", P$x, ", y=", P$y, "; note usr[3]=", usr[3], "\n")
            x <- P$x
            if (is.finite(P$y) && (abs(P$y - usr[3]) < 0.01 * (usr[4] - usr[3]))) {
                if (!is.na(x) && usr[1] < x && x < usr[2]) {
                    label <- fixneg(paste(lon, "E", sep=""))
                    ##mtext(label, side=1, at=x)
                    AT <- c(AT, x)
                    LAB <- c(LAB, label)
                    if (debug > 3) oceDebug(debug, "  ", label, "intersects side 1\n")
                } else {
                    if (debug > 3) oceDebug(debug, "    ", lon, "E does not intersect side 1\n")
                }
            } else {
                oceDebug(debug, "skipping off-globe point\n")
            }
        }
        if (!is.null(AT)) {
            axis(side=1, at=AT, labels=fixneg(LAB), mgp=MGP,
                 tick=tick, line=line, pos=pos, outer=outer, font=font,
                 lty=lty, lwd=lwd, lwd.ticks=lwd.ticks, col=col, col.ticks=col.ticks,
                 hadj=hadj, padj=padj, tcl=tcl) 
        }
        if (length(latitude)) {
            warning("mapAxis(side=1) cannot draw latitude labels yet; contact author if you need this\n")
        }
    }
    if (2 %in% side) {
        oceDebug(debug, "drawing axis on side 2\n")
        AT <- NULL
        LAB <- NULL
        for (lat in latitude) {
            if (debug > 3)
                oceDebug(debug, "check ", lat, "N for axis on side=2\n", sep="")
            ## Seek a point at this lon that matches the lon-lat relationship on side=1
            for (hemisphere in 1:2) {
                LONLOOK <- if (1 == hemisphere) c(-360, 0) else c(0, 360)
                o <- optimize(function(lon) abs(lonlat2map(lon, lat)$x-usr[1]),
                              lower=LONLOOK[1], upper=LONLOOK[2])
                if (is.na(o$objective) || o$objective > 0.01*axisSpan) {
                    if (debug > 3)
                        oceDebug(debug, "  ", lat, "N is unmappable [hemisphere ", hemisphere, "]; o$objective=", o$objective, "\n", sep="")
                    next
                }
                ##cat("lat:", lat, ", o$minimum:", o$minimum, "(best)\n")
                ## Check that the point matches lat, as well as lon (this matters for full-globe)
                P <- lonlat2map(o$minimum, lat)
                ## oceDebug(debug, "Investigate point at x=", P$x, ", y=", P$y, "; note usr[3]=", usr[3], "\n")
                y <- P$y
                if (is.finite(P$x) && (abs(P$x - usr[1]) < 0.01 * (usr[2] - usr[1]))) {
                    if (!is.na(y) && usr[3] < y && y < usr[4]) {
                        label <- fixneg(paste(lat, "N", sep=""))
                        ## mtext(label, side=2, at=y)
                        AT <- c(AT, y)
                        LAB <- c(LAB, label)
                        if (debug > 3)
                            oceDebug(debug, "  ", label, " intersects side 2 [hemisphere ", hemisphere, "]\n", sep="")
                    } else {
                        if (debug > 3)
                            oceDebug(debug, "  ", lat, "N does not intersect side 2 [hemisphere ", hemisphere, "]\n", sep="")
                    }
                } else {
                    oceDebug(debug, "skipping off-globe point\n")
                }
            }
        }
        if (!is.null(AT)) {
            axis(side=2, at=AT, labels=fixneg(LAB), mgp=MGP,
                 tick=tick, line=line, pos=pos, outer=outer, font=font,
                 lty=lty, lwd=lwd, lwd.ticks=lwd.ticks, col=col, col.ticks=col.ticks,
                 hadj=hadj, padj=padj, tcl=tcl) 
        }
        if (length(longitude)) {
            warning("mapAxis(side=2) cannot draw longitude labels yet; contact author if you need this\n")
        }
    }
    if (3 %in% side) {
        oceDebug(debug, "drawing axis on side 3 NOT CODED YET\n")
    }
    if (4 %in% side) {
        oceDebug(debug, "drawing axis on side 4 NOT CODED YET\n")
    }
    oceDebug(debug, "} # mapAxis()\n", unindent=1)
}

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
    if (!usingProj4() && (!exists(".Last.projection") || 0 == nchar(mapproj::.Last.projection()$projection)))
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

mapDirectionField <- function(longitude, latitude, u, v,
                              scale=1, length=0.05, code=2, col=par("fg"), ...)
{
    ## handle case where lon and lat are coords on edges of grid
    if (is.matrix(u)) {
        if (is.vector(longitude) && is.vector(latitude)) {
            nlon <- length(longitude)
            nlat <- length(latitude)
            longitude <- matrix(rep(longitude, nlat), nrow=nlon)
            latitude <- matrix(rep(latitude, nlon), byrow=TRUE, nrow=nlon)
        }
    }
    xy <- lonlat2map(longitude, latitude)
    ## Calculate spatially-dependent scale (fails for off-page points)
    ## Calculate lon-lat at ends of arrows
    scalex <- scale / cos(pi * latitude / 180)
    latEnd <- latitude + v * scale
    lonEnd <- longitude + u * scalex
    xy <- lonlat2map(longitude, latitude)
    xyEnd <- lonlat2map(lonEnd, latEnd)
    arrows(xy$x, xy$y, xyEnd$x, xyEnd$y, length=length, code=code, col=col, ...)
}


mapLongitudeLatitudeXY <- function(longitude, latitude)
{
    if (missing(longitude))
        stop("must give 'longitude' and possibly 'latitude'")
    if (!missing(longitude) && ("data" %in% slotNames(longitude))) {
        tmp <- longitude@data
        if (("longitude" %in% names(tmp)) && ("latitude" %in% names(tmp))) {
            latitude <- tmp$latitude
            longitude <- tmp$longitude
        }
    }
    proj <- lonlat2map(longitude, latitude)
    list(x=proj$x, y=proj$y) # if other properties prove helpful, may add them
} 

mapPlot <- function(longitude, latitude, longitudelim, latitudelim, grid=TRUE,
                    bg, fill=NULL, type='l', axes=TRUE, drawBox=TRUE, showHemi=TRUE,
                    polarCircle=0, lonlabel=NULL, latlabel=NULL, sides=NULL,
                    projection="+proj=moll", parameters=NULL, orientation=NULL,
                    trim=TRUE, debug=getOption("oceDebug"),
                    ...)
{
    dots <- list(...)
    gridOrig <- grid
    if (1 == length(gridOrig))
        gridOrig <- rep(gridOrig, 2)
    oceDebug(debug, "mapPlot(longitude, latitude", 
             ", longitudelim=", if (missing(longitudelim)) "(missing)" else c("c(", paste(format(longitudelim, digits=4), collapse=","), ")"),
             ", longitudelim=", if (missing(latitudelim)) "(missing)" else c("c(", paste(format(latitudelim, digits=4), collapse=","), ")"),
             ", projection=\"", projection, "\"",
             ", grid=", grid,
             ", ...) {\n", sep="", unindent=1)
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
    # FIXME: 20150326
    #if (is.logical(grid[1]) && grid[1])
    #    grid <- rep(15, 2)
    xy <- lonlat2map(longitude, latitude, projection=projection, parameters=parameters, orientation=orientation)
    if (!missing(latitudelim) && 0 == diff(latitudelim)) stop("lattudelim must contain two distinct values")
    if (!missing(longitudelim) && 0 == diff(longitudelim)) stop("longitudelim must contain two distinct values")
    limitsGiven <- !missing(latitudelim) && !missing(longitudelim)
    x <- xy$x
    xrange <- range(x, na.rm=TRUE)
    y <- xy$y
    yrange <- range(y, na.rm=TRUE)
    ## FIXME: should permit the use of PROJ.4 projections that lack inverses.
    #if (usingProj4() && length(grep("wintri", projection)))
    #    stop("cannot handle +proj=wintri because it has no inverse")
    #if (usingProj4() && length(grep("aitoff", projection)))
    #    stop("cannot handle +proj=aitoff because it has no inverse")
    xorig <- x
    yorig <- y
    ## FIXME: maybe *always* do this.
    ## FIXME: maybe *skip Antarctica*.
    if (usingProj4() ||
        projection %in% c('mollweide', 'polyconic')) { ## kludge trim wild points [github issue 227]
        ## FIXME: below is a kludge to avoid weird horiz lines; it
        ## FIXME: would be better to complete the polygons, so they 
        ## FIXME: can be filled.  It might be smart to do this in C
        d <- c(0, sqrt(diff(x)^2 + diff(y)^2))
        d[!is.finite(d)] <- 0          # FIXME: ok?
        ##dc <- as.numeric(quantile(d, 1-100*(1/3/length(x)), na.rm=TRUE)) # FIXME: criterion
        ##bad <- d > dc
        ##bad <- 0.1 < (d / diff(range(x, na.rm=TRUE)))
        antarctic <- latitude < -60
        bad <- ((d / diff(range(x, na.rm=TRUE))) > 0.1) & !antarctic
        ## FIXME: this should finish off polygons, but that is a bit tricky, e.g.
        ## FIXME: should we create a series of points to a trace along the edge 
        ## FIXME: the visible earth?
        if (debug > 0 && sum(bad))    # FIXME should be debug>0
            warning("mapPlot(): trimming ", sum(bad), " spurious edge-to-edge lines; filling may be inaccurate", call.=FALSE)
        x[bad] <- NA                       
        y[bad] <- NA
    }

    bad2 <- !is.finite(x) | !is.finite(y)
    x[bad2] <- NA
    y[bad2] <- NA

    dotnames <- names(dots)
    if ("xlim" %in% dotnames || "ylim" %in% dotnames || "xaxs" %in% dotnames || "yaxs" %in% dotnames) {
        ## for issue 539, i.e. repeated scales
        plot(x, y, type=type, xlab="", ylab="", asp=1, axes=FALSE, ...)
    } else {
        if (limitsGiven) {
            ## transform so can do e.g. latlim=c(70, 110) to centre on pole
            ##message("latitudelim: ", paste(latitudelim, collapse=" "))
            ##message("longitudelim: ", paste(longitudelim, collapse=" "))
            if (latitudelim[2] > 90) {
                longitudelim[2] <- 360 + longitudelim[2] - 180
                latitudelim[2] <- 180 - latitudelim[2]
            }
            ##message("latitudelim: ", paste(latitudelim, collapse=" "))
            ##message("longitudelim: ", paste(longitudelim, collapse=" "))
            n <- 10
            BOXx <- c(rep(longitudelim[1], n), seq(longitudelim[1], longitudelim[2], length.out=n),
                      rep(longitudelim[2], n), seq(longitudelim[2], longitudelim[1], length.out=n))
            BOXy <- c(seq(latitudelim[1], latitudelim[2], length.out=n), rep(latitudelim[2], n),
                      seq(latitudelim[2], latitudelim[1], length.out=n), rep(latitudelim[1], n))
            box <- lonlat2map(BOXx, BOXy)
            bad3 <- !is.finite(box$x) | !is.finite(box$y)
            box$x <- box$x[!bad3]
            box$y <- box$y[!bad3]
            plot(x, y, type=type,
                 xlim=range(box$x, na.rm=TRUE), ylim=range(box$y, na.rm=TRUE),
                 xlab="", ylab="", asp=1, axes=FALSE, ...)
            ## points(jitter(box$x), jitter(box$y), pch=1, col='red')
        } else {
            plot(x, y, type=type,
                 xlab="", ylab="", asp=1, axes=FALSE, ...)
        }
    }
    ## Remove any island/lake that is entirely offscale.  This is not a perfect
    ## solution to the Antarctica/stereographic problem of issue 545, because the
    ## line segment between two offscale points might intersect the box.  For
    ## this reason, it is done only when trim=TRUE.
    if (trim && usingProj4()) {
        ## trim out any polygons that have all points offscale
        usr <- par("usr")
        w <- which(is.na(xorig))
        if (length(w)) {
            for (iw in seq(1, -1+length(w))) {
                ##message("check chunk", iw)
                look <- seq.int(w[iw]+1, w[iw+1]-1)
                xl <- xorig[look]
                yl <- yorig[look]
                offscale <- yl < usr[3] | xl < usr[1] | yl > usr[4] | xl > usr[2]
                offscale <- offscale[is.finite(offscale)]
                if (all(offscale)) { # probably faster to do this than to make new vectors
                    ##message("  TRIM")
                    x[look] <- NA
                    y[look] <- NA
                }
            }
        }
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
    ## message("xrange:", paste(round(xrange, 2), collapse=" "))
    ## message("usr[1:2]:", paste(round(usr[1:2], 2), collapse=" "))
    ## message("yrange:", paste(round(yrange, 2), collapse=" "))
    ## message("usr[3:4]:", paste(round(usr[3:4], 2), collapse=" "))
    fractionOfGlobe <- usr[1] > xrange[1] || xrange[2] > usr[2] || usr[3] > yrange[1] || yrange[2] > usr[4]
    oceDebug(debug, "initially fractionOfGlobe:", fractionOfGlobe, "\n")
    ## Also turn off axes if it's nearly the whole globe
    xfrac <- diff(xrange)/(usr[2]-usr[1]) > 0.7
    yfrac  <- diff(yrange)/(usr[4]-usr[3]) > 0.7
    if (!xfrac) fractionOfGlobe <- FALSE
    if (!yfrac) fractionOfGlobe <- FALSE
    oceDebug(debug, "xfrac:", xfrac, "\n")
    oceDebug(debug, "yfrac:", yfrac, "\n")
    oceDebug(debug, "finally fractionOfGlobe:", fractionOfGlobe, "\n")
    if (axes || drawGrid) {
        ## Grid lines and axes.
        ## Find ll and ur corners of plot, if possible, for use in calculating
        ## lon and lat spans.  This will not work in all cases; e.g. for a
        ## world map in mollweide projection, the bounding points will be "in
        ## space", so we must check for this when we calculate the span.
        usr <- par('usr')
        xll <- usr[1]
        yll <- usr[3]
        xur <- usr[2]
        yur <- usr[4]

        options <- options('warn') # turn off warnings temporarily
        options(warn=-1) 

        ## Estimate the span (km) of the displayed portion of the earth.
        span <- 10e3 # assume hemispheric, if we cannot determine otherwise
        if (TRUE) {                    # 2014-11-16 for issue 543
            ## Measure lower-left to upper-right diagonal
            ll <- map2lonlat(usr[1], usr[3])
            ur <- map2lonlat(usr[2], usr[4])
            if (is.finite(ll$longitude) && is.finite(ll$latitude) &&
                is.finite(ur$longitude) && is.finite(ur$latitude)) {
                ## estimate span in deg lat by dividing by 111km
                span <- geodDist(ll$longitude, ll$latitude, ur$longitude, ur$latitude) / 111
            }
        } else {                       # 2014-11-16 for issue 543 (will delete next in a week or so)
            ## Now next may fail for e.g. molleweide has ll and ur that are
            ## un-invertable, since the globe may not fill the whole plotting area.
            mlat <- mean(longitude,na.rm=TRUE)
            mlon <- mean(latitude, na.rm=TRUE)
            ll <- map2lonlat(xll, yll)
            if (is.na(ll$longitude))
                ll <- map2lonlat(xll, yll, init=c(mlon, mlat))
            ur <- map2lonlat(xur, yur)
            if (is.na(ur$longitude))
                ur <- map2lonlat(xur, yur, init=c(mlon, mlat))
            if (!is.finite(ll$longitude) || !is.finite(ll$latitude) ||
                !is.finite(ur$longitude) || !is.finite(ur$latitude)) {
                ur <- list(longitude=180, latitude=90)
                ll <- list(longitude=-180, latitude=-90)
            }
            spanLat <- if (!is.finite(ur$latitude - ll$latitude)) diff(latitudelim) else ur$latitude - ll$latitude
            spanLon <- if (!is.finite(ur$longitude - ll$longitude)) diff(longitudelim) else ur$longitude - ll$longitude
            span <- min(abs(spanLat), abs(spanLon))
        }
        oceDebug(debug, "span:", span, "\n")
        ## Use span to make auto-scale the grid.
        if (is.logical(grid)) {
            grid <- c(15, 15)
            if (gridOrig[1]) {
                grid[1] <- if (span > 45) 15 else if (span > 10) 5 else if (span > 3) 2 else 1/60
            }
            if (gridOrig[2]) {
                grid[2] <- if (span > 45) 15 else if (span > 10) 5 else if (span > 3) 2 else 1/60
            }
        }
        oceDebug(debug, "grid:", grid[1], " ", grid[2], "\n")
        if ((is.logical(grid[1]) && grid[1]) || (is.finite(grid[1]) && grid[1] > 0)) {
            mapGrid(longitude=NULL, dlatitude=grid[2], polarCircle=polarCircle)
        }
        if ((is.logical(grid[2]) && grid[2]) || (is.finite(grid[2]) && grid[2] > 0)) {
            mapGrid(dlongitude=grid[1], latitude=NULL, polarCircle=polarCircle)
        }
        if (axes) {
            mapAxis(side=1, longitude=.axis()$longitude, debug=debug-1)
            mapAxis(side=2, latitude=.axis()$latitude, debug=debug-1)
        }
        ## 2014-11-16 major code revision
        ## 2014-11-16    if (is.null(lonlabel))
        ## 2014-11-16        lonlabel <- lonlabs
        ## 2014-11-16    if (is.null(latlabel))
        ## 2014-11-16        latlabel <- latlabs
        ## 2014-11-16    if (is.null(sides))
        ## 2014-11-16        sides <- 1:2
        ## 2014-11-16    ## Tighten axis labels since know no superscripts or names
        ## 2014-11-16    TICK <- TRUE # ticks look bad for angled grid lines
        ## 2014-11-16    TCL <- -0.3
        ## 2014-11-16    MGP <- c(2, 0.5, 0)            # first item ignored since not writing "longitude" etc
        ## 2014-11-16    axisSpan <- max(usr[2]-usr[1], usr[4]-usr[3])

        ## 2014-11-16    xdelta <- diff(usr[1:2]) / 1000 # used to ensure label in valid domain
        ## 2014-11-16    ydelta <- diff(usr[3:4]) / 1000
        ## 2014-11-16    proj4 <- usingProj4()

        ## 2014-11-16    if (1 %in% sides) {            # bottom side
        ## 2014-11-16        AT <- NULL
        ## 2014-11-16        LAB <- NULL
        ## 2014-11-16        for (lab in lonlabel) {
        ## 2014-11-16            o <- optimize(function(lat) abs(lonlat2map(lab,lat)$y-usr[3]),lower=-89.9999,upper=89.9999)
        ## 2014-11-16            if (is.na(o$objective) || o$objective > 0.01*axisSpan) next
        ## 2014-11-16            x <- lonlat2map(lab, o$minimum)$x
        ## 2014-11-16            if (!is.na(x) && usr[1] < x && x < usr[2]) {
        ## 2014-11-16                AT <- c(AT, x)
        ## 2014-11-16                LAB <- c(LAB, paste(lab, "E", sep=""))
        ## 2014-11-16                oceDebug(debug, "lonlabel", lab, "E intersects side 1\n")
        ## 2014-11-16            } else {
        ## 2014-11-16                oceDebug(debug, "lonlabel", lab, "E does not intersect side 1\n")
        ## 2014-11-16            }
        ## 2014-11-16        }
        ## 2014-11-16        ## message("is.null(AT):", is.null(AT))
        ## 2014-11-16        ## message("axes:", axes)
        ## 2014-11-16        ## message("fractionOfGlobe:", fractionOfGlobe)
        ## 2014-11-16        if (!is.null(AT) && axes && fractionOfGlobe) axis(side=1, at=AT, labels=fixneg(LAB), tick=TICK, tcl=TCL, mgp=MGP)
        ## 2014-11-16    }
        ## 2014-11-16    if (2 %in% sides) {    # left side
        ## 2014-11-16        oceDebug(debug, "side=2 proj4: ", proj4, "\n")
        ## 2014-11-16        AT <- NULL
        ## 2014-11-16        LAB <- NULL
        ## 2014-11-16        for (lab in latlabel) {
        ## 2014-11-16            ##oceDebug(debug, "examine lab=", lab, "N\n")
        ## 2014-11-16            ## next is a test of two methods; ideally one will work for both projections
        ## 2014-11-16            if (proj4) { ## 2014-09-21 new scheme for axis labels, only on side=2 for testing [issue 526]
        ## 2014-11-16                ## optimize(function(y) {cat(y,'\n');abs(map2lonlat(usr[1], y)$latitude - lab)},lower=usr[3]-10*ydelta, upper=usr[4]+10*ydelta)
        ## 2014-11-16                o <- optimize(function(y) abs(map2lonlat(usr[1], y)$latitude - lab),
        ## 2014-11-16                              lower=usr[3]-ydelta, upper=usr[4]+ydelta)
        ## 2014-11-16                ## check if found inside box
        ## 2014-11-16                if (is.na(o$objective)) {
        ## 2014-11-16                    oceDebug(debug, "objective is bad: ", o$objective, "; axisSpan=", axisSpan, "\n");
        ## 2014-11-16                    next
        ## 2014-11-16                }
        ## 2014-11-16                ## if (debug && 1>abs(lab-15)) browser()
        ## 2014-11-16                if (o$minimum < usr[3] || usr[4] < o$minimum) {
        ## 2014-11-16                    oceDebug(debug, "min ", o$minimum, " not in box usr[3]=", usr[3], " and usr[4]=", usr[4], " (", lab, "N)\n", sep="")
        ## 2014-11-16                    next
        ## 2014-11-16                }
        ## 2014-11-16                y <- o$minimum
        ## 2014-11-16                if (!is.na(y) && usr[3] < y && y < usr[4]) {
        ## 2014-11-16                    AT <- c(AT, y)
        ## 2014-11-16                    LAB <- c(LAB, paste(lab, "N", sep=""))
        ## 2014-11-16                    oceDebug(debug, "latlabel", lab, "N intersects side 2 at y=", y, "\n")
        ## 2014-11-16                } else {
        ## 2014-11-16                    oceDebug(debug, "latlabel", lab, "N does not intersect side 2; y=", y, "and usr[3]=", usr[3], "and usr[4]=", usr[4], "\n")
        ## 2014-11-16                }
        ## 2014-11-16            } else {
        ## 2014-11-16                if (is.finite(ll$longitude) && is.finite(ll$latitude)) {
        ## 2014-11-16                    o <- optimize(function(lon) abs(lonlat2map(lon,lab)$x-usr[1]),
        ## 2014-11-16                                  lower=ll$longitude-90,upper=ll$longitude+90)
        ## 2014-11-16                                    #if (abs(lab-60)<2)browser()
        ## 2014-11-16                    if (is.na(o$objective))
        ## 2014-11-16                        next
        ## 2014-11-16                    y <- lonlat2map(o$minimum, lab)$y
        ## 2014-11-16                    if (!is.na(y) && usr[3] <= y && y <= usr[4]) {
        ## 2014-11-16                        AT <- c(AT, y)
        ## 2014-11-16                        LAB <- c(LAB, paste(lab, "N", sep=""))
        ## 2014-11-16                        oceDebug(debug, "latlabel", lab, "N intersects side 2 at y=", y, "\n")
        ## 2014-11-16                    } else {
        ## 2014-11-16                        oceDebug(debug, "latlabel", lab, "N does not intersect side 2; y=", y, "and usr[3]=", usr[3], "and usr[4]=", usr[4], "\n")
        ## 2014-11-16                    }
        ## 2014-11-16                }
        ## 2014-11-16            }
        ## 2014-11-16        }
        ## 2014-11-16        if (!is.null(AT) && axes && fractionOfGlobe) axis(side=2, at=AT, labels=fixneg(LAB), tick=TICK, tcl=TCL, mgp=MGP)
        ## 2014-11-16    }
        ## 2014-11-16    if (3 %in% sides) {    # topside
        ## 2014-11-16        warning("axis on top side of map is not working yet (contact developer); FIXME: handle the 2 proj methods")
        ## 2014-11-16        AT <- NULL
        ## 2014-11-16        LAB <- NULL
        ## 2014-11-16        for (lab in lonlabel) {
        ## 2014-11-16            o <- optimize(function(lat) abs(mapproject(lab,lat)$y-usr[4]),lower=-90,upper=90)
        ## 2014-11-16            if (is.na(o$objective) || o$objective > 0.01) next
        ## 2014-11-16            x <- mapproject(lab, o$minimum)$x
        ## 2014-11-16            if (!is.na(x) && usr[3] < x && x < usr[4]) {
        ## 2014-11-16                AT <- c(AT, x)
        ## 2014-11-16                LAB <- c(LAB, paste(lab, "E", sep=""))
        ## 2014-11-16                oceDebug(debug, "lonlabel", lab, "E intersects side 3\n")
        ## 2014-11-16            }
        ## 2014-11-16        }
        ## 2014-11-16        for (lab in latlabel) {
        ## 2014-11-16            t <- try({o <- optimize(function(lon) abs(mapproject(lon,lab)$y-usr[4]),lower=-180,upper=180)})
        ## 2014-11-16            if (is.na(o$objective) || o$objective > 0.01) next
        ## 2014-11-16            x <- mapproject(o$minimum, lab)$x
        ## 2014-11-16            if (!is.na(x) && usr[3] < x && x < usr[4]) {
        ## 2014-11-16                AT <- c(AT, x)
        ## 2014-11-16                LAB <- c(LAB, paste(lab, "N", sep=""))
        ## 2014-11-16                oceDebug(debug, "latlabel", lab, "N intersects side 3\n")
        ## 2014-11-16            }
        ## 2014-11-16        }
        ## 2014-11-16        if (!is.null(AT) && axes && fractionOfGlobe) axis(side=3, at=AT, labels=fixneg(LAB), tick=TICK, tcl=TCL, mgp=MGP)
        ## 2014-11-16    }
        ## 2014-11-16    if (4 %in% sides) {    # right side
        ## 2014-11-16        warning("axis on right-hand side of map is not working yet (contact developer)")
        ## 2014-11-16        AT <- NULL
        ## 2014-11-16        LAB <- NULL
        ## 2014-11-16        for (lab in lonlabel) {
        ## 2014-11-16            o <- optimize(function(lat) abs(mapproject(lab,lat)$x-usr[2]),lower=-90,upper=90)
        ## 2014-11-16            if (is.na(o$objective) || o$objective > 0.01) next
        ## 2014-11-16            y <- mapproject(lab, o$minimum)$y
        ## 2014-11-16            if (!is.na(y) && usr[3] < y && y < usr[4]) {
        ## 2014-11-16                AT <- c(AT, y)
        ## 2014-11-16                LAB <- c(LAB, paste(lab, "E", sep=""))
        ## 2014-11-16                oceDebug(debug, "lonlabel", lab, "E intersects side 4\n")
        ## 2014-11-16            }
        ## 2014-11-16        }
        ## 2014-11-16        for (lab in latlabel) {
        ## 2014-11-16            t <- try({o <- optimize(function(lon) abs(mapproject(lon,lab)$x-usr[2]),lower=-180,upper=180)})
        ## 2014-11-16            if (is.na(o$objective) || o$objective > 0.01) next
        ## 2014-11-16            y <- mapproject(lab, o$minimum)$y
        ## 2014-11-16            if (!is.na(y) && usr[3] < y && y < usr[4]) {
        ## 2014-11-16                AT <- c(AT, y)
        ## 2014-11-16                LAB <- c(LAB, paste(lab, "N", sep=""))
        ## 2014-11-16                oceDebug(debug, "latlabel", lab, "N intersects side 4\n")
        ## 2014-11-16            }
        ## 2014-11-16        }
        ## 2014-11-16        if (!is.null(AT) && axes && fractionOfGlobe) axis(side=4, at=AT, labels=fixneg(LAB), tick=TICK, tcl=TCL, mgp=MGP)
        ## 2014-11-16    }
        options(warn=options$warn) 
    }
    oceDebug(debug, "} # mapPlot()\n", unindent=1)
}

mapGrid <- function(dlongitude=15, dlatitude=15, longitude, latitude,
                    col="darkgray", lty="solid", lwd=0.5*par("lwd"), polarCircle=0,
                    debug=getOption("oceDebug"))
{
    warning("FIXME: mapGrid() should check if longitude or latitude is NULL\n")
    small <- 0
    if (missing(longitude))
        longitude <- seq(-180, 180, dlongitude)
    if (missing(latitude))
        latitude <- seq(-90+small, 90-small, dlatitude)
    n <- 360                           # number of points on line
    xspan <- diff(par('usr')[1:2])
    ## Update the global axis information
    axisOLD <- .axis()
    .axis(list(longitude=if (!missing(longitude) && length(longitude)) longitude else axisOLD$longitude,
               latitude=if (!missing(latitude) && length(latitude)) latitude else axisOLD$latitude))
    for (l in latitude) {              # FIXME: maybe we should use mapLines here
        if (debug > 2) oceDebug(debug, "lat=", l, " N\n")
        line <- lonlat2map(seq(-180+small, 180-small, length.out=n), rep(l, n))
        x <- line$x
        y <- line$y
        ok <- !is.na(x) & !is.na(y)
        x <- x[ok]
        if (0 == length(x)) next
        y <- y[ok]
        if (0 == length(y)) next
        ## Remove ugly horizontal lines that can occur for 
        ## projections that show the edge of the earth.
        xJump <- abs(diff(x))
        if (any(is.finite(xJump))) {
            ## FIXME: the number in the next line might need adjustment.
            xJumpMedian <- median(xJump, na.rm=TRUE)
            if (!is.na(xJumpMedian)) {
                horizontalJump <- c(FALSE, xJump > 3 * xJumpMedian)
                if (any(horizontalJump)) {
                    x[horizontalJump] <- NA
                }
            }
            lines(x, y, lty=lty, lwd=lwd, col=col)
        }
    }
    if (polarCircle < 0 || polarCircle > 90)
        polarCircle <- 0
    n <- 360                           # number of points on line
    for (l in longitude) {             # FIXME: should use mapLines here
        if (debug > 2) oceDebug(debug, "lon=", l, " N\n")
        line <- lonlat2map(rep(l, n), seq(-90+polarCircle+small, 90-polarCircle-small, length.out=n))
        x <- line$x
        y <- line$y
        ok <- !is.na(x) & !is.na(y)
        x <- x[ok]
        if (0 == length(x)) next
        y <- y[ok]
        if (0 == length(y)) next
        lines(x, y, lty=lty, lwd=lwd, col=col)
    }
}

mapMeridians <- function(latitude, lty='solid', lwd=0.5*par('lwd'), col='darkgray', ...)
{
    warning("Use mapGrid(longitude=NULL,latitude=latitude) instead of mapMeridians(latitude)")
    if (missing(latitude))
        latitude <- TRUE
    small <- 0.001
    if (is.logical(latitude)) {
        if (!latitude)
            return()
        latitude <- seq(-90+small, 90-small, length.out=13)
    }
    usr <- par('usr')
    n <- 360                           # number of points on line
    for (l in latitude) {
        ## FIXME: maybe we should use mapLines here
        ## message("mapMeridian at ", l, " N")
        line <- lonlat2map(seq(-180+small, 180-small, length.out=n), rep(l, n))
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
        if (FALSE) { # this was a bad idea, e.g. in orthographic, lines cross whole domain
            d <- c(0, sqrt(diff(x)^2 + diff(y)^2))
            d[!is.finite(d)] <- 0
            if (0 == length(d))
                next
            ##2014-01-09 dc <- as.numeric(quantile(d, 0.99, na.rm=TRUE)) # FIXME: criterion
            dc <- as.numeric(median(d, na.rm=TRUE))
            bad <- d > 3 * dc
            x[bad] <- NA                   # FIXME: add, don't replace
            y[bad] <- NA                   # FIXME: add, don't replace
        }
        ## NB. used to check for points in region but when zoomed in closely, there may be none!
        ##if (length(x) & length(y) & any(usr[1] <= x & x <= usr[2] & usr[3] <= y & y <= usr[4], na.rm=TRUE)) {
        lines(x, y, lty=lty, lwd=lwd, col=col, ...)
    }
}

mapScalebar <- function(x, y=NULL, length,
                        lwd=1.5*par("lwd"), cex=par("cex"),
                        col="black")
{
    if (0 == nchar(mapproj::.Last.projection()$projection) && 0 == nchar(.Last.proj4()$projection)) {
        warning("mapScalebar() only works for plots created with projections")
        return()
    }
    if (!is.null(y))
        stop("y must be NULL in this (early) version of mapScalebar()\n")
    if (missing(x))
        x <- "topleft"
    else if (is.na(pmatch(x, c("topleft", "topright"))))
        stop("x must be \"topleft\" or \"topright\", but it is \"", x, "\"\n")
    usr <- par('usr')
    ## determine scale from centre of region
    x0 <- 0.5 * (usr[1] + usr[2])
    y0 <- 0.5 * (usr[3] + usr[4])
    dusr <- 0.01 * (usr[2] - usr[1]) # 1 percent of device width
    x1 <- x0 + dusr
    y1 <- y0
    lonlat0 <- map2lonlat(x0, y0)
    lonlat1 <- map2lonlat(x1, y1)
    dkm <- geodDist(lonlat0$longitude, lonlat0$latitude,
                      lonlat1$longitude, lonlat1$latitude)
    kmPerUsr <- dkm / dusr
    ##message("kmPerUsr: ", kmPerUsr)
    if (missing(length)) {
        # corner to corner distance
        ccd <- kmPerUsr*sqrt((usr[2]-usr[1])^2+(usr[4]-usr[3])^2)
        length <- diff(pretty(c(0, ccd), n=12)[1:2])
    }
    frac <- length / kmPerUsr
    cin <- par('cin')[1]
    cinx <- xinch(cin)
    ciny <- yinch(cin)
    ## FIXME: when get more options for x, revisit next few lines
    xBar <- usr[1] + cinx / 2
    if (is.character(x) && x == "topright")
        xBar <- usr[2] - frac - 3.5 * cinx
    yBar <- usr[4] - ciny / 2
    ## Draw white-out underlay box with a border, since otherwise
    ## it's hard to see a scalebar on a complex map.
    llBox <- list(x=xBar, y=yBar - 3 * ciny)
    urBox <- list(x=xBar + frac + 3 * cinx, y=yBar)
    polygon(c(llBox$x, llBox$x, urBox$x, urBox$x),
            c(llBox$y, urBox$y, urBox$y, llBox$y),
            border="black", col='white')
    ## Draw the scalebar and text below.
    lines(xBar + cinx + c(0, frac), rep(yBar-ciny, 2), lwd=lwd, col=col, lend=2)
    lines(rep(xBar+cinx, 2), yBar - ciny + c(-ciny, ciny)/3,
          col=col, lwd=lwd)
    lines(rep(xBar++cinx+frac, 2), yBar - ciny + c(-ciny, ciny)/3,
          col=col, lwd=lwd)
    text(xBar+cinx, yBar-2.2*ciny, pos=4, adj=0, offset=0,
         sprintf("%.0f km", length), cex=cex, col=col)
}

mapText <- function(longitude, latitude, labels, ...)
{
    ok <- !is.na(longitude) & !is.na(latitude)
    longitude <- longitude[ok]
    latitude <- latitude[ok]
    labels <- labels[ok]
    if (length(longitude) > 0) {
        xy <- lonlat2map(longitude, latitude)
        text(xy$x, xy$y, labels, ...)
    }
}

mapZones <- function(longitude, polarCircle=0, lty='solid', lwd=0.5*par('lwd'), col='darkgray', ...)
{
    warning("Use mapGrid(longitude=longitude,latitude=NULL) instead of mapZones(longitude)")
    if (missing(longitude))
        longitude <- TRUE
    small <- 0.001
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
        line <- lonlat2map(rep(l, n), seq(-90+polarCircle+small, 90-polarCircle-small, length.out=n))
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
    xy <- lonlat2map(longitude, latitude)
    ok <- !is.na(xy$x) & !is.na(xy$y)
    usr <- par('usr')
    DX <- usr[2] - usr[1]
    if (any(usr[1] <= xy$x[ok] & xy$x[ok] <= usr[2] & usr[3] <= xy$y[ok] & xy$y[ok] <= usr[4])) {
        dx <- c(0, abs(diff(xy$x, na.rm=TRUE)))
        bad <- dx / DX > 0.1
        if (any(bad, na.rm=TRUE)) { # FIXME: a kludge that may be problematic
            xy$x[bad] <- NA
        }
        lines(xy$x, xy$y, ...)
    }
}

mapPoints <- function(longitude, latitude, debug=getOption("oceDebug"), ...)
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
    if (inherits(longitude, "section") || inherits(longitude, "ctd")) {
        latitude <- longitude[["latitude", "byStation"]]
        longitude <- longitude[["longitude", "byStation"]]
    }
    ok <- !is.na(longitude) & !is.na(latitude)
    longitude <- longitude[ok]
    latitude <- latitude[ok]
    if (length(longitude) > 0) {
        xy <- lonlat2map(longitude, latitude)
        points(xy$x, xy$y, ...)
    }
    if (debug > 90) {
        cat("par('usr'):", paste(par("usr"), collapse=" "), "\n")
        cat("first point: lon:", longitude[1], ", lat:", latitude[1], "x:", xy$x[1], "y:", xy$y[1], "\n")
    }
}

mapArrows <- function(longitude0, latitude0,
                      longitude1=longitude0, latitude1=latitude0,
                      length=0.25, angle=30,
                      code=2, col=par("fg"), lty=par("lty"),
                      lwd=par("lwd"), ...)
{
    if (length(longitude0) != length(latitude0))
        stop("lengths of longitude0 and latitude0 must match but they are ", length(longitude0), " and ", length(longitude1))
    if (length(longitude1) != length(latitude1))
        stop("lengths of longitude1 and latitude1 must match but they are ", length(longitude1), " and ", length(longitude1))
    ok <- !is.na(longitude0) & !is.na(latitude0) & !is.na(longitude1) & !is.na(latitude1)
    longitude0 <- longitude0[ok]
    latitude0 <- latitude0[ok]
    longitude1 <- longitude1[ok]
    latitude1 <- latitude1[ok]
    if (length(longitude) > 0) {
        xy0 <- lonlat2map(longitude0, latitude0)
        xy1 <- lonlat2map(longitude1, latitude1)
        arrows(xy0$x, xy0$y, xy1$x, xy1$y,
               length=length, angle=angle, code=code, col=col, lty=lty, lwd=lwd, ...)
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
        if (noMinutes) {
            rval <- sprintf("%2d%s", degrees, hemispheres) # no space, so more labels
        } else if (noSeconds) {
            rval <- sprintf("%02d %02d' %s", degrees, minutes, hemispheres)
        } else {
            rval <- sprintf("%02d %02d' %04.2f\" %s", degrees, minutes, seconds, hemispheres)
        }
        Encoding(rval) <- "latin1"
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
                                                         s=sprintf("%02f", seconds[i]),
                                                         hemi=hemispheres[i])))
            }
        }
    }
    rval
}

mapLocator <- function(n=512, type='n', ...)
{
    xy <- locator(n, type, ...)
    print(xy)
    rval <- map2lonlat(xy$x, xy$y)
    if (type == 'l')
        mapLines(rval$longitude, rval$latitude, ...)
    else if (type == 'p')
        mapPoints(rval$longitude, rval$latitude, ...)
    rval
}

map2lonlat <- function(x, y, init=c(0,0))
{
    if (is.list(x)) {
        y <- x$y
        x <- x$x
    }
    n <- length(x)
    if (n != length(y))
        stop("lengths of x and y must match but they are ", n, " and ", length(y))
    ## NB. if projections are set by mapPlot() or lonlat2map(), only one of the 
    ## following two tests can be true.
    if (0 < nchar(.Last.proj4()$projection)) {
        if (!getOption("externalProj4", FALSE)) {
            ##message("doing PROJ.4 calculations within Oce, for speed and accuracy")
            XY <- .C("proj4_interface", as.character(.Last.proj4()$projection), as.integer(FALSE),
                     as.integer(n), as.double(x), as.double(y),
                     X=double(n), Y=double(n), NAOK=TRUE)
            return(list(longitude=XY$X, latitude=XY$Y))
        } else {
            ##message("doing projection calculations with 'proj4' package")
            if (!requireNamespace("proj4", quietly=TRUE))
                stop("must install 'proj4' package to get options(externalProj4=TRUE) to work")
            xy <- list(x=NA, y=NA)
            ## FIXME: maybe we should do point-by-point if this yields an error
            try({
                xy <- proj4::project(list(x=x, y=y), proj=.Last.proj4()$projection, inverse=TRUE)
            }, silent=TRUE)
            return(list(longitude=xy$x, latitude=xy$y))
        }
    }
    ## Now we know we are using mapproj-style
    lp <- mapproj::.Last.projection()
    projection <- lp$projection
    parameters <- lp$parameters
    orientation <- lp$orientation
    lon <- vector("numeric", n)
    lat <- vector("numeric", n)
    for (i in 1:n) {
        xy <- c(x[i], y[i])
        lon[i] <- NA
        lat[i] <- NA
        ##message("i:", i, ", xy[1]:", xy[1], ", xy[2]:", xy[2])
        try({
            error <- FALSE
            ## message("init:", init[1], " ", init[2])
            ## Note: using L-BFGS-B so we can limit the bounds; otherwise
            ## it can select lat > 90 etc.
            worstMisfit <- 0           # try to avoid errors with NA
            o <- optim(init,
                       function(xyTrial) {
                           xyp <- mapproject(xyTrial[1], xyTrial[2],
                                             projection=projection,
                                             parameters=parameters,
                                             orientation=orientation)
                           error <<- xyp$error
                           misfit <- sqrt((xyp$x-xy[1])^2+(xyp$y-xy[2])^2)
                           ## message(format(xyTrial[1], digits=4), "E ",
                           ##         format(xyTrial[2], digits=4), "N ",
                           ##         "misfit: ", format(misfit, digits=5), ", error: ", xyp$error)
                           if (error) {
                               ## message("got error so returning ", worstMisfit)
                               return(worstMisfit)
                           } else {
                               worstMisfit <<- max(misfit, worstMisfit, na.rm=TRUE)
                               ## message("no error; set worstMisfit ", worstMisfit)
                               return(misfit)
                           }
                       }, method="L-BFGS-B", lower=c(-180, -89.9999), upper=c(180, 89.9999))
            if (o$convergence == 0 && !error) {
                lonlat <- o$par
                lon[i] <- lonlat[1]
                lat[i] <- lonlat[2]
            }
            ## str(o)
        }, silent=TRUE)
    }
    ##message("map2lonlat returning lon=", lon, " lat=", lat)
    return(list(longitude=lon, latitude=lat))
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
    if (n > 0) {
        xy <- lonlat2map(longitude, latitude)
        polygon(xy$x, xy$y,
                density=density, angle=angle, border=border, col=col, lty=lty, ...,
                fillOddEven=fillOddEven)
    }
}

mapImage <- function(longitude, latitude, z, zlim, zclip=FALSE,
                     breaks, col, colormap, border=NA,
                     lwd=par("lwd"), lty=par("lty"),
                     filledContour=FALSE, missingColor=NA, debug=getOption("oceDebug"))
{
    if (!usingProj4() && (!exists(".Last.projection") || 0 == nchar(mapproj::.Last.projection()$projection)))
        stop("must create a map first, with mapPlot()\n")
    breaksGiven <- !missing(breaks)
    zlimGiven <- !missing(zlim)
    colGiven <- !missing(col)
    oceDebug(debug, "mapImage(..., ",
             " missingColor='", missingColor, "', ",
             " filledContour=", filledContour, ", ",
             ", ...) {\n", sep="", unindent=1)
 
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
    #if (filledContour)
    #    warning("mapImage() cannot yet handle filledContour\n")
    breaksGiven <- !missing(breaks)
    if (!missing(colormap)) { # takes precedence over breaks and col
        breaks <- colormap$breaks
        breaksGiven <- TRUE
        col <- colormap$col
        missingColor <- colormap$missingColor
        zclip <- colormap$zclip
        colGiven <- TRUE
    } 
    ## 20140816 (issues 517 and 522) START
    ## 20140816 The next few blocks (down to the 'ni <-' line) used to be the 'else'
    ## 20140816 of the above if block, but that just seemed to invite case-specific
    ## 20140816 errors. The new approach is to unroll the code to reduce the
    ## 20140816 number of cases.
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
            if (!colGiven) {## missing(col)) {
                oceDebug(debug, "zlim provided, but not breaks or col\n")
                breaks <- c(zlim[1], pretty(zlim, n=128), zlim[2])
            } else {
                oceDebug(debug, "zlim and col provided, but not breaks\n")
                breaks <- seq(zlim[1], zlim[2], length.out=if(is.function(col))128 else 1+length(col))
            }
            breaksOrig <- breaks
            breaks[1] <- min(zrange[1], breaks[1])
            breaks[length(breaks)] <- max(breaks[length(breaks)], zrange[2])
        }
    } else {
        breaksOrig <- breaks
        if (1 == length(breaks)) {
            oceDebug(debug, "only 1 break given, so taking that as number of breaks\n")
            breaks <- pretty(z, n=breaks)
        }
    }
    if (missing(col)) {
        col <- oce.colorsPalette(n=length(breaks)-1)
        oceDebug(debug, "using default col\n")
    }
    if (is.function(col)) {
        col <- col(n=length(breaks)-1)
        oceDebug(debug, "col is a function\n")
    }
    if (debug > 10) {
        message("zclip: ", zclip)
        message("breaks: ", paste(breaks, collapse=" "))
        message("col: ", paste(col, collapse=" "))
    }
    ## 20140816 END
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
    ## 20140802/issue516: next if block (clipping) rewritten
    if (zclip) {
        oceDebug(debug, "using missingColor for out-of-range values\n")
        if (zlimGiven) {
            z[z < zlim[1]] <- NA
            z[z > zlim[2]] <- NA
        }
    } else {
        if (zlimGiven) {
            oceDebug(debug, "using zlim colours for out-of-range values\n")
            zlimMin <- min(zlim, na.rm=TRUE)
            zlimMax <- max(zlim, na.rm=TRUE)
            z[z <= zlimMin] <- zlimMin * (1 + sign(zlimMin) * small)
            z[z >= zlimMax] <- zlimMax * (1 - sign(zlimMax) * small)
        } else if (breaksGiven) {
            oceDebug(debug, "using min/max colours for out-of-range values\n")
            breaksMin <- min(breaks, na.rm=TRUE)
            breaksMax <- max(breaks, na.rm=TRUE)
            z[z <= breaksMin] <- breaksMin * (1 + sign(breaksMin) * small)
            z[z >= breaksMax] <- breaksMax * (1 - sign(breaksMax) * small)
        } else {
            oceDebug(debug, "not clipping AND NEITHER zlim nor breaks suppled\n")
        }
    }
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

    xy <- lonlat2map(poly$longitude, poly$latitude)

    ## map_check_polygons tries to fix up longitude cut-point problem, which
    ## otherwise leads to lines crossing the graph horizontally because the
    ## x value can sometimes alternate from one end of the domain to the other.
    Z <- matrix(z)
    r <- .Call("map_check_polygons", xy$x, xy$y, poly$z,
               diff(par('usr'))[1:2]/5, par('usr'),
               NAOK=TRUE, PACKAGE="oce")
    breaksMin <- min(breaks, na.rm=TRUE)
    breaksMax <- max(breaks, na.rm=TRUE)
    if (filledContour) {
        oceDebug(debug, "using filled contours\n")
        zz <- as.vector(z)
        g <- expand.grid(longitude, latitude)
        longitudeGrid <- g[,1]
        latitudeGrid <- g[,2]
        rx <- range(xy$x, na.rm=TRUE)
        ry <- range(xy$y, na.rm=TRUE)
        f <- if (is.logical(filledContour)) 1 else as.integer(round(filledContour))
        xg <- seq(rx[1], rx[2], length.out=f*length(longitude))
        yg <- seq(ry[1], ry[2], length.out=f*length(latitude))
        xy <- lonlat2map(longitudeGrid, latitudeGrid)
        good <- is.finite(zz) & is.finite(xy$x) & is.finite(xy$y)
        if (!zclip) {
            ##message("# good: ", sum(good), " orig")
            zz[zz < breaksMin] <- breaksMin
            zz[zz > breaksMax] <- breaksMax
        }
        xx <- xy$x[good]
        yy <- xy$y[good]
        zz <- zz[good]
        if (requireNamespace("akima", quietly=TRUE)) {
            i <- akima::interp(xx, yy, zz, xg, yg)
            #levels <- breaks # FIXME: probably wrong
            .filled.contour(i$x, i$y, i$z, levels=breaks,col=col)
        } else {
            warning("must install.packages(\"akima\") to plot filled contours on maps")
        }
    } else {
        oceDebug(debug, "using polygons, as opposed to filled contours\n")
        colFirst <- col[1]
        colLast <- tail(col, 1)
        if (debug > 10) { ## FIXME (issue 522): retain this test code until 2014-oct
            message("breaksMin: ", breaksMin)
            message("breaksMax: ", breaksMax)
            message("Z:")
            print(Z)
        }
        colorLookup <- function (ij) {
            zval <- Z[ij]
            if (is.na(zval)) {
                if (debug > 10) { ## FIXME (issue 522): retain this test code until 2014-oct
                    message("z is NA")
                }
                return(missingColor)   # whether clipping or not
            }
            if (zval < breaksMin) {
                if (debug > 10) { ## FIXME (issue 522): retain this test code until 2014-oct
                    message("z: ", zval, " is < breaksMin")
                }
                return(if (zclip) missingColor else colFirst)
            }
            if (zval > breaksMax) {
                if (debug > 10) { ## FIXME (issue 522): retain this test code until 2014-oct
                    message("z: ", zval, " is > breaksMax")
                }
                return(if (zclip) missingColor else colLast)
            }
            ## issue522: this was w <- which(zval <= breaks)[1]
            w <- which(zval <= breaks)[1]
            #if (zval == 0) browser()
            ## if (debug > 10) { ## FIXME (issue 522): retain this test code until 2014-oct
            ##     message("zval:", zval, ", w:", w)
            ## }
            if (!is.na(w) && w > 1) {
                if (debug > 10) { ## FIXME (issue 522): retain this test code until 2014-oct
                    message("z: ", zval, ", w: ", w, ", using non-missing col: ", col[-1+w])
                }
                return(col[-1 + w]) 
            } else {
                if (debug > 10) { ## FIXME (issue 522): retain this test code until 2014-oct
                    message("z: ", zval, ", w: ", w, ", using missing col: ", missingColor)
                }
                return(missingColor)
            }
        }
        colPolygon <- sapply(1:(ni*nj), colorLookup)
        polygon(xy$x[r$okPoint & !r$clippedPoint], xy$y[r$okPoint & !r$clippedPoint],
                col=colPolygon[r$okPolygon & !r$clippedPolygon],
                border=border, lwd=lwd, lty=lty, fillOddEven=FALSE)
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

lonlat2utm <- function(longitude, latitude, zone, km=FALSE)
{
    names <- names(longitude)
    if (!is.null(names)) {
        if ("zone" %in% names)
            zone <- longitude$zone
        if ("longitude" %in% names && "latitude" %in% names) {
            latitude <- longitude$latitude
            longitude <- longitude$longitude
        }
    }
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
    if (missing(zone)) {
        zone <- floor((180+longitude)/6)  # FIXME: this works for zone but not positive its ok
        zone <- ifelse(zone > 60, zone-60, zone)
    }
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

utm2lonlat <- function(easting, northing, zone=1, hemisphere="N", km=FALSE) 
{
    names <- names(easting)
    if (!is.null(names)) {
        if ("easting" %in% names && "northing" %in% names && "zone" %in% names) {
            zone <- easting$zone
            northing <- easting$northing
            easting <- easting$easting
        }
    }
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

## This list of known projections includes only those with inverses. To create the list
## first I did
##      proj -lP > A
## in the commandline, and then I hand-edited out the entries that said that no
## inverse was available. Then I used grep and sed and an editor action to create the
## list.  The reason for demanding an inverse is to avoid errors that arise
## otherwise.  Eventually, a scheme could be set up for doing an approximate
## inverse within oce, but the arguments against that include (a) the difficulty
## in implementing it and (b) the possibility that non-invertable projections
## simply haven't been generated enough interest in the broader cartographic
## community to merit inclusion in oce.

## 1. calcofi is deleted because it's nor really a projection; it's more
##    like a coordinate transformation.
## 2. isea is delted because it causes segmentation faults that crash R,
##    at least on a world coastline test.
## 3. labrd is deleted because it returns NaN for every point
##    on the coastlineWorld; fixing this is not a high priority
##    given that it is a niche projection that has caused problems
##    in PROJ.4 also.
## 4. wintri is delted because it hangs R when drawing coastlineWorld.
knownProj4 <- c("aea", "aeqd", "aitoff", "alsk", "bipc", "bonne",
##knownProj4 <- c("aea", "aeqd", "aitoff", "alsk", "bipc", "bonne", #"calcofi",
                "cass", "cc", "cea", "collg", "crast", "eck1", "eck2", "eck3",
                "eck4", "eck5", "eck6", "eqc", "eqdc", "euler", "etmerc",
                "fahey", "fouc", "fouc_s", "gall", "geos", "gn_sinu", "gnom",
                "goode", "gs48", "gs50", "hatano", "healpix", "rhealpix",
                ##"igh", "imw_p", "isea", "kav5", "kav7", "krovak", "labrd",
                ##"igh", "imw_p", "kav5", "kav7", "krovak", "labrd",
                "igh", "imw_p", "kav5", "kav7", "krovak",
                "laea", "lonlat", "latlon", "lcc", "lcca", "leac", "lee_os",
                "loxim", "lsat", "mbt_s", "mbt_fps", "mbtfpp", "mbtfpq",
                "mbtfps", "merc", "mil_os", "mill", "moll", "murd1", "murd2",
                "murd3", "natearth", "nell", "nell_h", "nsper", "nzmg",
                "ob_tran", "ocea", "oea", "omerc", "ortho", "pconic", "poly",
                "putp1", "putp2", "putp3", "putp3p", "putp4", "putp4p",
                "putp5", "putp5p", "putp6", "putp6p", "qsc", "qua_aut",
                "robin", "rouss", "sinu", "somerc", "stere", "sterea",
                "gstmerc", "tcea", "tissot", "tmerc", "tpeqd", "tpers", "ups",
                "urm5", "urmfps", "utm", "vandg", "vitk1", "wag1", "wag2",
                #"wag3", "wag4", "wag5", "wag6", "weren", "wink1", "wintri")
                "wag3", "wag4", "wag5", "wag6", "weren", "wink1")

lonlat2map <- function(longitude, latitude, projection="", parameters=NULL, orientation=NULL)
{
    ## NOTE: the proj4 method can run into errors (e.g. "ortho" for points on opposite
    ## side of the earth) an may have to be done (slowly) point by point; a warning is
    ## issued if so.
    if (is.list(longitude)) {
        latitude <- longitude$latitude
        longitude <- longitude$longitude
    }
    n <- length(longitude)
    if (n != length(latitude))
        stop("lengths of longitude and latitude must match but they are ", n, " and ", length(latitude))
    ## Use proj4 if it has been set up (and still exists).
    if ("" == projection) projection <- .Last.proj4()$projection
    if ('+' != substr(projection, 1, 1)) {
        ## mapproj case
        xy <- mapproject(longitude, latitude,
                         projection=projection, parameters=parameters, orientation=orientation)
        .Last.proj4(list(projection=""))     # turn proj4 off, in case it was on
        if (nchar(projection) > 1 && (is.null(orientation) || (orientation[1] == 90 && orientation[3] == 0))) {
            cmd <- "+proj="
            proj <- "?"
            ## See http://www.remotesensing.org/geotiff/proj_list
            ## After the conversion there may be a comment listing corequisites
            if (projection == "aitoff") proj <- "(no equivalent)"
            if (projection == "albers") proj <- "aea" # needs lat0 lat1
            if (projection == "bonne") proj <- "bonne" # needs lat0
            if (projection == "gall") proj <- "gall"
            ## if (projection == "lambert") proj <- "laea" ## ??
            if (projection == "lambert") proj <- "lcc"
            if (projection == "mercator") proj <- "merc"
            if (projection == "mollweide") proj <- "moll"
            if (projection == "orthographic") proj <- "ortho"
            if (projection == "polyconic") proj <- "pconic"
            if (projection == "robin") proj <- "robin"
            ## FIXME: what about sterea?
            if (projection == "stereographic") proj <- "stere"
            if (projection == "wintri") proj <- "(no equivalent)"
            cmd <- paste("+proj=", proj, sep="")
            if (!is.null(parameters)) {
                names <- names(parameters)
                if ("lat0" %in% names) cmd <- paste(cmd, " +lat_0=", parameters[["lat0"]], sep="")
                if ("lat1" %in% names) cmd <- paste(cmd, " +lat_1=", parameters[["lat1"]], sep="")
            }
            if (!is.null(orientation))
                cmd <- paste(cmd, " +lon_0=", orientation[2], sep="")
            if (projection == "stereographic")
                cmd <- paste(cmd, " +lat_0=90", sep="")
            message("mapPlot() suggestion: try using projection=\"", cmd, "\"")
        }
    } else {                           
        ## proj4 case
        pr <- gsub(" .*$", "", gsub("^\\+proj=", "", projection))
        if (!(pr %in% knownProj4))
            stop("projection '", pr, "' is unknown; try one of: ", paste(knownProj4, collapse=','))
        #if (length(grep("aitoff", pr))) stop("+proj=aitoff cannot be used")
        #if (length(grep("robin", pr))) stop("+proj=robin cannot be used")
        #if (length(grep("wintri", pr))) stop("+proj=wintri cannot be used")
        ll <- cbind(longitude, latitude)
        if (!getOption("externalProj4", FALSE)) {
            ## message("doing PROJ.4 calculations within Oce, for speed and accuracy")
            if (0 == length(grep("ellps=", projection)))
                projection<- paste(projection, "+ellps=sphere")
            n <- length(longitude)
            XY <- .C("proj4_interface", as.character(projection), as.integer(TRUE),
                     as.integer(n), as.double(longitude), as.double(latitude),
                     X=double(n), Y=double(n), NAOK=TRUE)
            xy <- list(x=XY$X, y=XY$Y)
        } else {
            ## message("doing projection calculations with 'proj4' package")
            if (!requireNamespace("proj4", quietly=TRUE))
                stop("must install 'proj4' package to get options(externalProj4=TRUE) to work")
            m <- NULL                 # for the try()
            try({
                m <- proj4::project(ll, proj=projection)
            }, silent=TRUE)
            if (is.null(m)) {
                m <- matrix(unlist(lapply(1:n, function(i)
                                          {
                                              t <- try({proj4::project(ll[i,], proj=projection)}, silent=TRUE)
                                              if (inherits(t, "try-error")) c(NA, NA) else t[1,]
                                          })),
                            ncol=2, byrow=TRUE)
                warning("proj4 calculation is slow because it was done pointwise")
            }
            xy <- list(x=m[,1], y=m[,2])
        }
        .Last.proj4(list(projection=projection)) # turn on proj4
        mapproj::.Last.projection(list(projection="")) # turn off mapproj, in case it was on
    }
    xy
}


