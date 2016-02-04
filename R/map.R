## Author notes on PROJ.4:
## 1. http://stackoverflow.com/questions/tagged/proj4
## 2. PROJ.4 is used by the following R packages:
##    1. openstreetmap
##    2. proj4
##    3. rgdal

.axis <- local({
    val <- list(longitude=NULL, latitude=NULL)
    function(new) if (!missing(new)) val <<- new else val
})

.Projection <- local({                # emulate mapproj
    # type can be 'none' or 'proj4' (once, permitted 'mapproj' also)
    val <- list(type="none", projection="")
    function(new) if(!missing(new)) val <<- new else val
})

#' Shift longitude to range -180 to 180, if any element exceeds 180
#'
#' This is a utility function used by \code{\link{mapGrid}}. It works
#' simply by subtracting 180 from each longitude, if any longitude
#' in the vector exceeds 180.
#'
#' @param longitudes a numericl vector of longitudes
#' @return vector of longitudes, shifted to the desired range. 
shiftLongitude <- function(longitudes) {
    if (any(longitudes > 180)) longitudes-360 else longitudes
}

fixneg <- function(v)
{
    res <- v
    for (i in seq_along(v)) {
        if (res[i] == "0N") {
            res[i] <- "0"
        } else if (res[i] == "0E") {
            res[i] <- "0"
        } else if ("-" == substr(v[i], 1, 1)) {
            ##cat("res[i]=", res[i], "\n")
            res[i] <- gsub("^-", "", v[i])
            res[i] <- gsub("E", "W", res[i])
            res[i] <- gsub("N", "S", res[i])
            ##cat(" -> res[i]=", res[i], "\n")
        }
    }
    res
}

badFillFix1 <- function(x, y, latitude, projection="")
{
    ##xrange <- range(x, na.rm=TRUE)
    ##yrange <- range(y, na.rm=TRUE)
    if (TRUE) {
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
        x[bad] <- NA                       
        y[bad] <- NA
    }
    bad2 <- !is.finite(x) | !is.finite(y)
    x[bad2] <- NA
    y[bad2] <- NA
    list(x=x, y=y)
}

badFillFix2 <- function(x, y, xorig, yorig)
{
    usr <- par("usr")
    w <- which(is.na(xorig))
    if (length(w) > 1) {
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
    list(x=x, y=y)
}


mapAxis <- function(side=1:2, longitude=NULL, latitude=NULL,
                    tick=TRUE, line=NA, pos=NA, outer=FALSE, font=NA,
                    lty="solid", lwd=1, lwd.ticks=lwd, col=NULL, col.ticks=NULL,
                    hadj=NA, padj=NA, tcl=-0.3, debug=getOption("oceDebug"))
{
    if ("none" == .Projection()$type)
        stop("must create a map first, with mapPlot()\n")
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
                ## oceDebug(debug, "skipping off-globe point\n")
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
        f <- function(lon) lonlat2map(lon,lat)$x-usr[1]
        ## FIXME: if this uniroot() method looks good for side=2, try for side=1 also.
        LONLIST <- seq(-360, 360, 20) # smaller increments are slower but catch more labels
        for (lat in latitude) {
            if (debug > 3)
                oceDebug(debug, "check ", lat, "N for axis on side=2\n", sep="")
            ## Seek a point at this lon that matches the lon-lat relationship on side=1
            for (iLON in 2:length(LONLIST)) {
                #if (lat == 55) browser()
                LONLOOK <- LONLIST[iLON+c(-1,0)]
                #cat("LONLOOK[1]", LONLOOK[1], "f(...)", f(LONLOOK[1]), "\n")
                #cat("LONLOOK[2]", LONLOOK[2], "f(...)", f(LONLOOK[2]), "\n")
                f1 <- f(LONLOOK[1])
                if (!is.finite(f1))
                    next
                f2 <- f(LONLOOK[2])
                if (!is.finite(f2))
                    next
                if (f1 * f2 > 0)
                    next
                r <- uniroot(f, lower=LONLOOK[1], upper=LONLOOK[2], tol=1)
                P <- lonlat2map(r$root, lat)
                ##OLD| ## using optimize. This seems slower, and can hit boundaries.
                ##OLD| o <- optimize(function(lon) abs(lonlat2map(lon, lat)$x-usr[1]), lower=LONLOOK[1], upper=LONLOOK[2], tol=1)
                ##OLD| if (is.na(o$objective) || o$objective > 0.01*axisSpan) {
                ##OLD|     if (debug > 3) oceDebug(debug, "  ", lat, "N is unmappable for iLON=", iLON, "; o$objective=", o$objective, "\n", sep="")
                ##OLD|     next
                ##OLD| }
                ##OLD| # Check that the point matches lat, as well as lon (this matters for full-globe)
                #P <- lonlat2map(o$minimum, lat)
                y <- P$y
                if (is.finite(P$x) && (abs(P$x - usr[1]) < 0.01 * (usr[2] - usr[1]))) {
                    if (!is.na(y) && usr[3] < y && y < usr[4]) {
                        label <- fixneg(paste(lat, "N", sep=""))
                        AT <- c(AT, y)
                        LAB <- c(LAB, label)
                        if (debug > 3) oceDebug(debug, "  ", label, " intersects side 2\n", sep="")
                    } else {
                        if (debug > 3) oceDebug(debug, "  ", lat, "N does not intersect side 2\n", sep="")
                    }
                } else {
                    ##oceDebug(debug, "skipping off-globe point\n")
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
    if ("none" == .Projection()$type)
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
    if ("none" == .Projection()$type)
        stop("must create a map first, with mapPlot()\n")
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
    if ("none" == .Projection()$type)
        stop("must create a map first, with mapPlot()\n")
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
                    bg, fill,
                    border=NULL, col=NA, # 'col' default differs from plot.coastline(), owing to ugly-horiz.-line issue
                    type='l', axes=TRUE, drawBox=TRUE, showHemi=TRUE,
                    polarCircle=0, lonlabel=NULL, latlabel=NULL, sides=NULL,
                    projection="+proj=moll", tissot=FALSE, trim=TRUE,
                    debug=getOption("oceDebug"),
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
        data("coastlineWorld", package="oce", envir=environment())
        longitude <- get("coastlineWorld")
    }
    if (!missing(fill)) {
        ## permit call as documented before 2016-02-03
        ## Note: the code permitted fill=TRUE but this was never documented
        if (is.character(fill)) {
            col <- fill
        } else {
            if (is.logical(fill) && !fill) {
                col <- NULL
            }
        }
        warning("In mapPlot() : 'fill' being accepted for backwards compatibility; please use 'col' instead", call.=FALSE)
    }
 
    isTopo <- FALSE
    if (inherits(longitude, "topo")) {
        topo <- longitude
        isTopo <- TRUE
        ## set up to corners of topo lonlat box
        longitude <- range(topo[["longitude"]], na.rm=TRUE)
        latitude <- range(topo[["latitude"]], na.rm=TRUE)
    } else if ("data" %in% slotNames(longitude) && # handle e.g. 'coastline' class
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
    #message("000")
    if (nchar(projection) && substr(projection, 1, 1) != "+") {
        stop("use PROJ.4 format, e.g. projection=\"+proj=merc\" for Mercator\n", sep="")
        ##20150612 ## call mapproject() so it can remember the projection
        ##20150612 mapproj::mapproject(0, 0, projection=projection, parameters=parameters, orientation=orientation)
    }
    xy <- lonlat2map(longitude, latitude, projection=projection)
    if (!missing(latitudelim) && 0 == diff(latitudelim)) stop("lattudelim must contain two distinct values")
    if (!missing(longitudelim) && 0 == diff(longitudelim)) stop("longitudelim must contain two distinct values")
    limitsGiven <- !missing(latitudelim) && !missing(longitudelim)

    x <- xy$x
    y <- xy$y
    xorig <- xy$x
    yorig <- xy$y
    oce_uhl <- options()$oce_uhl
    if (!is.null(oce_uhl) && oce_uhl == "method 1") {
        ## Insert NA to break long horizontal jumps, which can be caused by coastline
        ## segments that "pass across" the edge of a plot.
        dx <- abs(diff(x))
        bigJumps <- which(dx > (mean(dx,na.rm=TRUE) + 10 * sd(dx, na.rm=TRUE)))
        ##print(longitude[bigJumps])
        for (j in bigJumps) {
            ##message("chopping j=", j)
            lenx <- length(x)
            x <- c(x[seq.int(1, j)], NA, x[seq.int(j+1, lenx)])
            longitude <- c(longitude[seq.int(1, j)], NA, longitude[seq.int(j+1, lenx)])
            y <- c(y[seq.int(1, j)], NA, y[seq.int(j+1, lenx)])
            latitude <- c(latitude[seq.int(1, j)], NA, latitude[seq.int(j+1, lenx)])
        }
        xy <- badFillFix1(x=x, y=y, latitude=latitude, projection=projection)
        x <- xy$x
        y <- xy$y
    }

    xrange <- range(x, na.rm=TRUE)
    yrange <- range(y, na.rm=TRUE)

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
            #message("111")
            box <- lonlat2map(BOXx, BOXy)
            #message("/111")
            bad3 <- !is.finite(box$x) | !is.finite(box$y)
            box$x <- box$x[!bad3]
            box$y <- box$y[!bad3]
            #message("FIXME: box is NA")
            plot(x, y, type=type,
                 xlim=range(box$x, na.rm=TRUE), ylim=range(box$y, na.rm=TRUE),
                 xlab="", ylab="", asp=1, axes=FALSE, ...)
            ## points(jitter(box$x), jitter(box$y), pch=1, col='red')
        } else {
            plot(x, y, type=type,
                 xlab="", ylab="", asp=1, axes=FALSE, ...)
        }
    }
    ## Remove any island/lake that is entirely offscale.  This is not a 
    ## solution to the Antarctica/stereographic problem of issue 545, because the
    ## line segment between two offscale points might intersect the box.  For
    ## this reason, it is done only when trim=TRUE.
    if (trim) {
        xy <- badFillFix2(x=x, y=y, xorig=xorig, yorig=yorig)
        x <- xy$x
        y <- xy$y
    }
    if (!is.null(col))
        polygon(x, y, border=border, col=col, ...)
    if (isTopo) {
        mapContour(topo[["longitude"]], topo[["latitude"]], topo[["z"]], ...)
    }
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
        ##xll <- usr[1]
        ##yll <- usr[3]
        ##xur <- usr[2]
        ##yur <- usr[4]

        options <- options('warn') # turn off warnings temporarily
        options(warn=-1) 

        if (is.logical(grid)) {
            ## Determining a grid automatically has proved to be quite tricky,
            ## and the code near this spot has been reworked repeatedly. 
            ## At one time, the code near this spot looked at par("usr")
            ## and tried to invert the corners, to get an idea of scale, and
            ## this failed because the Winkel Tripel ("wintri") projection
            ## goes into what seems to be an infinite loop when trying to do the
            ## inverse of a point that is beyond the edge of the earth
            ## disk. (I think it fails just on the disk edge, too.) When oce had
            ## the PROJ.4 code embedded within its src, this was not a problem,
            ## because I had a workaround.  This workaround has been reported
            ## to the PROJ.4 community, so I expect that sometime in the year
            ## 2015 this problem will go away.
            ##
            ## Given the above, the present code focusses near the centre of
            ## the plot region.  A region that might correspond to one tick
            ## on the axes (assuming 10 ticks per side) is inverse mapped,
            ## and the corners are used to determine a tick scale. Rather
            ## than use pretty(), the scale is determined from a list
            ## of standards (because maps should have 5deg increments, if
            ## this is good for a view, but not 4deg).
            usr <- par('usr')
            x0 <- 0.5 * sum(usr[1:2])
            y0 <- 0.5 * sum(usr[3:4])
            ntick <- 8
            dx <- (usr[2] - usr[1]) / ntick
            dy <- (usr[4] - usr[3]) / ntick
            ll <- map2lonlat(x0-dx, y0-dy)
            ur <- map2lonlat(x0+dx, y0+dy)
            ls <- geodDist(ll$longitude, ll$latitude, ll$longitude, ur$latitude)
            rs <- geodDist(ur$longitude, ll$latitude, ur$longitude, ur$latitude)
            ts <- geodDist(ll$longitude, ur$latitude, ur$longitude, ur$latitude)
            bs <- geodDist(ll$longitude, ll$latitude, ur$longitude, ll$latitude)
            t <- median(c(ls, rs, ts, bs)) / 111 # tick, in degrees
            oceDebug(debug, "t: ", t, "(scale for ticks, in deg)\n")
            ## message("tickEW: ", tickEW)
            ## message("tickNS: ", tickNS)
            ## message("tick: ", tick)
            if (!is.finite(t)) {
                grid <- c(5, 5) # may be ok in many instances
            } else {
                oceDebug(debug, "t: ", t, "\n")
                g <- if (t > 45) 45 else if (t > 10) 15 else if (t > 5) 10
                    else if (t > 4) 5 else if (t > 2) 1 else pretty(t)[2]
                grid <- rep(g, 2)
                oceDebug(debug, "grid:", grid[1], "\n")
            }
        }
        oceDebug(debug, "grid:", grid[1], " ", grid[2], "\n")
        oceDebug(debug, "drawgrid:", drawGrid, "\n")
        if (drawGrid) {
            mapGrid(longitude=NULL, dlatitude=grid[2], polarCircle=polarCircle,
                    longitudelim=longitudelim, latitudelim=latitudelim, debug=debug-1)
        }
        if (drawGrid) {
            mapGrid(dlongitude=grid[1], latitude=NULL, polarCircle=polarCircle,
                    longitudelim=longitudelim, latitudelim=latitudelim, debug=debug-1)
        }
        if (axes) {
            mapAxis(side=1, longitude=.axis()$longitude, debug=debug-1)
            mapAxis(side=2, latitude=.axis()$latitude, debug=debug-1)
        }
        if (tissot)
            mapTissot(grid, col='red', debug=debug-1)
        options(warn=options$warn) 
    }
    oceDebug(debug, "} # mapPlot()\n", unindent=1)
}

mapGrid <- function(dlongitude=15, dlatitude=15, longitude, latitude,
                    col="darkgray", lty="solid", lwd=0.5*par("lwd"), polarCircle=0,
                    longitudelim, latitudelim, 
                    debug=getOption("oceDebug"))
{
    if ("none" == .Projection()$type)
        stop("must create a map first, with mapPlot()\n")
    if (!missing(longitudelim))
        longitudelim <- shiftLongitude(longitudelim)
    oceDebug(debug, "mapGrid(dlongitude=", dlongitude, 
             ", datitude=", dlatitude, ", ..., polarCircle=", polarCircle,
             ", longitudelim=", if (missing(longitudelim)) "(missing)" else
                 paste("c(", paste(longitudelim, collapse=", "), ")"),
             ", latitudelim=", if (missing(latitudelim)) "(missing)" else
                 paste("c(", paste(latitudelim, collapse=", "), ")"),
             ", debug)\n", unindent=1, sep="")
    if (!missing(longitudelim)) {
        longitudelim <- shiftLongitude(longitudelim)
        oceDebug(debug, "shifted longitudelim to c(",
                 paste(longitudelim, collapse=","), ")\n")
    }
    small <- 0
    if (missing(longitude))
        longitude <- seq(-180, 180, dlongitude)
    if (missing(latitude))
        latitude <- seq(-90+small, 90-small, dlatitude)
    if (!missing(longitudelim)) {
        lonMin <- longitudelim[1] - diff(longitudelim) / 2
        lonMax <- longitudelim[2] + diff(longitudelim) / 2
        oceDebug(debug, "lonMin=", lonMin, ", lonMax=", lonMax, "\n")
        oceDebug(debug, "before trimming to longitudelim+: lon range ", paste(range(longitude, na.rm=TRUE), collapse=" "), "\n")
        longitude <- longitude[lonMin <= longitude & longitude <= lonMax]
        oceDebug(debug, "after: lon range ", paste(range(longitude), collapse=" "), "\n")
    }
    if (!missing(latitudelim)) {
        ## limit to 1.5 timex lon/lim limit range
        latMin <- latitudelim[1] - diff(latitudelim) / 2
        latMax <- latitudelim[2] + diff(latitudelim) / 2
        oceDebug(debug, "before trimming to latitudelim+: lat range ", paste(range(latitude, na.rm=TRUE), collapse=" "), "\n")
        latitude <- latitude[latMin <= latitude & latitude <= latMax]
        oceDebug(debug, "after: lat range ", paste(range(latitude), collapse=" "), "\n")
    }
    n <- 360                           # number of points on line
    ##xspan <- diff(par('usr')[1:2])
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
        if (debug > 2) oceDebug(debug, "lon=", l, " E\n")
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
    if ("none" == .Projection()$type)
        stop("must create a map first, with mapPlot()\n")
    warning("Use mapGrid(longitude=NULL,latitude=latitude) instead of mapMeridians(latitude)")
    if (missing(latitude))
        latitude <- TRUE
    small <- 0.001
    if (is.logical(latitude)) {
        if (!latitude)
            return()
        latitude <- seq(-90+small, 90-small, length.out=13)
    }
    ##usr <- par('usr')
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
    if ("none" == .Projection()$type)
        stop("must create a map first, with mapPlot()\n")
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
    if ("none" == .Projection()$type)
        stop("must create a map first, with mapPlot()\n")
    ok <- !is.na(longitude) & !is.na(latitude)
    longitude <- longitude[ok]
    latitude <- latitude[ok]
    labels <- labels[ok]
    if (length(longitude) > 0) {
        xy <- lonlat2map(longitude, latitude)
        text(xy$x, xy$y, labels, ...)
    }
}

mapTissot <- function(grid=rep(15, 2), scale=0.2, ...)
{
    if ("none" == .Projection()$type)
        stop("must create a map first, with mapPlot()\n")
    if (2 != length(grid) || !is.numeric(grid) || any(!is.finite(grid)))
        stop("grid must be of length 2, numeric, and finite")
    theta <- seq(0, 2*pi, length.out=16)
    d <- scale * min(grid, na.rm=TRUE)
    for (lon in seq(-180, 180, grid[1])) {
        for (lat in seq(-90, 90, grid[2])) {
            LAT <- lat + d*sin(theta)
            LON <- lon + d*cos(theta) / cos(lat * pi / 180)
            mapLines(LON, LAT, ...)
        }
    }
}

mapZones <- function(longitude, polarCircle=0, lty='solid', lwd=0.5*par('lwd'), col='darkgray', ...)
{
    if ("none" == .Projection()$type)
        stop("must create a map first, with mapPlot()\n")
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
    ##usr <- par('usr')
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
    if ("none" == .Projection()$type)
        stop("must create a map first, with mapPlot()\n")
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
    ##n <- length(longitude)
    ok <- !is.na(xy$x) & !is.na(xy$y)
    usr <- par('usr')
    ##DX <- usr[2] - usr[1]
    if (any(usr[1] <= xy$x[ok] & xy$x[ok] <= usr[2] & usr[3] <= xy$y[ok] & xy$y[ok] <= usr[4])) {
        ## 20150421 # Remove code that attempted to delete extraneous lines ... the problem
        ## 20150421 # is that there's no good way to know which are extraneous, and length
        ## 20150421 # is not the best indicator.
        ## 20150421 if (n > 10) { # don't mess with short segments 
        ## 20150421     dx <- c(0, abs(diff(xy$x, na.rm=TRUE)))
        ## 20150421     bad <- dx / DX > 0.1
        ## 20150421     if (any(bad, na.rm=TRUE)) { # FIXME: a kludge that may be problematic
        ## 20150421         xy$x[bad] <- NA
        ## 20150421     }
        ## 20150421 }
        lines(xy$x, xy$y, ...)
    }
}

mapPoints <- function(longitude, latitude, debug=getOption("oceDebug"), ...)
{
    if ("none" == .Projection()$type)
        stop("must create a map first, with mapPlot()\n")
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
}

mapArrows <- function(longitude0, latitude0,
                      longitude1=longitude0, latitude1=latitude0,
                      length=0.25, angle=30,
                      code=2, col=par("fg"), lty=par("lty"),
                      lwd=par("lwd"), ...)
{
    if ("none" == .Projection()$type)
        stop("must create a map first, with mapPlot()\n")
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
            res <- list(degrees, hemispheres)
        else if (noSeconds)
            res <- list(degrees, minutes, hemispheres)
        else
            res <- list(degrees, minutes, seconds, hemispheres)
    } else if (type == "string") {
        if (noMinutes) {
            res <- sprintf("%2d%s", degrees, hemispheres) # no space, so more labels
        } else if (noSeconds) {
            res <- sprintf("%02d %02d' %s", degrees, minutes, hemispheres)
        } else {
            res <- sprintf("%02d %02d' %04.2f\" %s", degrees, minutes, seconds, hemispheres)
        }
        Encoding(res) <- "latin1"
    } else if (type == "expression") {
        n <- length(degrees)
        res <- vector("expression", n)
        for (i in 1:n) {
            if (noMinutes) {
                res[i] <- as.expression(substitute(d*degree*hemi,
                                                    list(d=degrees[i],
                                                         hemi=hemispheres[i])))
            } else if (noSeconds) {
                ##res[i] <- as.expression(substitute(d*degree*phantom(.)*m*minute*hemi,
                res[i] <- as.expression(substitute(d*degree*phantom(.)*m*minute*hemi,
                                                    list(d=degrees[i],
                                                         m=sprintf("%02d", minutes[i]),
                                                         hemi=hemispheres[i])))
            } else {
                res[i] <- as.expression(substitute(d*degree*phantom(.)*m*minute*phantom(.)*s*second*hemi,
                                                    list(d=degrees[i],
                                                         m=sprintf("%02d", minutes[i]),
                                                         s=sprintf("%02f", seconds[i]),
                                                         hemi=hemispheres[i])))
            }
        }
    }
    res
}

mapLocator <- function(n=512, type='n', ...)
{
    if ("none" == .Projection()$type)
        stop("must create a map first, with mapPlot()\n")
    xy <- locator(n, type, ...)
    res <- map2lonlat(xy$x, xy$y)
    if (type == 'l')
        mapLines(res$longitude, res$latitude, ...)
    else if (type == 'p')
        mapPoints(res$longitude, res$latitude, ...)
    res
}

map2lonlat <- function(x, y, init=c(0,0))
{
    if ("none" == .Projection()$type)
        stop("must create a map first, with mapPlot()\n")
    if (is.list(x)) {
        y <- x$y
        x <- x$x
    }
    n <- length(x)
    if (n != length(y))
        stop("lengths of x and y must match but they are ", n, " and ", length(y))
    ##20150612 ## NB. if projections are set by mapPlot() or lonlat2map(), only one of the 
    ##20150612 ## following two tests can be true.
    ##20150612 if ("proj4" == .Projection()$type) {
    if (requireNamespace("rgdal", quietly=TRUE)) {
        owarn <- options()$warn
        options(warn=-1)
        XY <- rgdal::project(cbind(x, y), proj=as.character(.Projection()$projection), inv=TRUE)
        ## See https://github.com/dankelley/oce/issues/653#issuecomment-107040093 for why I gave
        ## up on the idea of using rawTransform().
        ##> n <- length(x)
        ##> XY <- rgdal::rawTransform(projfom=as.character(.Projection()$projection), projto="+proj=longlat", n=n, x=x, y=y)
        options(warn=owarn)
        return(list(longitude=XY[,1], latitude=XY[,2]))
        ## See https://github.com/dankelley/oce/issues/653#issuecomment-107040093 for why I gave
        ## up on the idea of using rawTransform().
        ##> return(list(longitude=XY[[1]], latitude=XY[[2]]))
    } else {
        stop('must install.packages("rgdal") to plot maps with projections')
    }
    ## 20150523 if (!getOption("externalProj4", FALSE)) {
    ## 20150523     ##message("doing PROJ.4 calculations within Oce, for speed and accuracy")
    ## 20150523     owarn <- options()$warn
    ## 20150523     options(warn=-1)
    ## 20150523     XY <- rgdal::project(cbind(x, y), proj=as.character(.Projection()$projection), inv=TRUE)
    ## 20150523     options(warn=owarn)
    ## 20150523     return(list(longitude=XY[,1], latitude=XY[,2]))
    ## 20150523     ##pre-rgdal XY <- .C("proj4_interface", as.character(.Projection()$projection), as.integer(FALSE),
    ## 20150523     ##pre-rgdal          as.integer(n), as.double(x), as.double(y),
    ## 20150523     ##pre-rgdal          X=double(n), Y=double(n), NAOK=TRUE)
    ## 20150523     ##pre-rgdal return(list(longitude=XY$X, latitude=XY$Y))
    ## 20150523 } else {
    ## 20150523     ##message("doing projection calculations with 'proj4' package")
    ## 20150523     if (!requireNamespace("proj4", quietly=TRUE))
    ## 20150523         stop("must install 'proj4' package to get options(externalProj4=TRUE) to work")
    ## 20150523     xy <- list(x=NA, y=NA)
    ## 20150523     ## FIXME: maybe we should do point-by-point if this yields an error
    ## 20150523     try({
    ## 20150523         xy <- proj4::project(list(x=x, y=y), proj=.Projection()$projection, inverse=TRUE)
    ## 20150523     }, silent=TRUE)
    ## 20150523     return(list(longitude=xy$x, latitude=xy$y))
    ## 20150523 }
    ## 20150612 } else if ("mapproj" == .Projection()$type) {
    ## 20150612     if (!requireNamespace("mapproj", quietly=TRUE))
    ## 20150612         stop("must install 'mapproj' package to use mapproj-style map projections")
    ## 20150612     lp <- mapproj::.Last.projection()
    ## 20150612     projection <- lp$projection
    ## 20150612     parameters <- lp$parameters
    ## 20150612     orientation <- lp$orientation
    ## 20150612     lon <- vector("numeric", n)
    ## 20150612     lat <- vector("numeric", n)
    ## 20150612     for (i in 1:n) {
    ## 20150612         xy <- c(x[i], y[i])
    ## 20150612         lon[i] <- NA
    ## 20150612         lat[i] <- NA
    ## 20150612         ##message("i:", i, ", xy[1]:", xy[1], ", xy[2]:", xy[2])
    ## 20150612         try({
    ## 20150612             error <- FALSE
    ## 20150612             ## message("init:", init[1], " ", init[2])
    ## 20150612             ## Note: using L-BFGS-B so we can limit the bounds; otherwise
    ## 20150612             ## it can select lat > 90 etc.
    ## 20150612             worstMisfit <- 0           # try to avoid errors with NA
    ## 20150612             o <- optim(init,
    ## 20150612                        function(xyTrial) {
    ## 20150612                            xyp <- mapproj::mapproject(xyTrial[1], xyTrial[2],
    ## 20150612                                                       projection=projection,
    ## 20150612                                                       parameters=parameters,
    ## 20150612                                                       orientation=orientation)
    ## 20150612                            error <<- xyp$error
    ## 20150612                            misfit <- sqrt((xyp$x-xy[1])^2+(xyp$y-xy[2])^2)
    ## 20150612                            ## message(format(xyTrial[1], digits=4), "E ",
    ## 20150612                            ##         format(xyTrial[2], digits=4), "N ",
    ## 20150612                            ##         "misfit: ", format(misfit, digits=5), ", error: ", xyp$error)
    ## 20150612                            if (error) {
    ## 20150612                                ## message("got error so returning ", worstMisfit)
    ## 20150612                                return(worstMisfit)
    ## 20150612                            } else {
    ## 20150612                                worstMisfit <<- max(misfit, worstMisfit, na.rm=TRUE)
    ## 20150612                                ## message("no error; set worstMisfit ", worstMisfit)
    ## 20150612                                return(misfit)
    ## 20150612                            }
    ## 20150612                        }, method="L-BFGS-B", lower=c(-180, -89.9999), upper=c(180, 89.9999))
    ## 20150612             if (o$convergence == 0 && !error) {
    ## 20150612                 lonlat <- o$par
    ## 20150612                 lon[i] <- lonlat[1]
    ## 20150612                 lat[i] <- lonlat[2]
    ## 20150612             }
    ## 20150612             ## str(o)
    ## 20150612         }, silent=TRUE)
    ## 20150612     }
    ## 20150612     ##message("map2lonlat returning lon=", lon, " lat=", lat)
    ## 20150612     return(list(longitude=lon, latitude=lat))
    ## 20150612 } else {
    ## 20150612     stop("unknown projection software type '", .Projection()$type, "'")
    ## 20150612 }
}

mapPolygon <- function(longitude, latitude, density=NULL, angle=45,
                       border=NULL, col=NA, lty=par('lty'), ..., fillOddEven=FALSE)
{
    if ("none" == .Projection()$type)
        stop("must create a map first, with mapPlot()\n")
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
        x <- xy$x
        y <- xy$y
        xorig <- xy$x
        yorig <- xy$y
        xy <- badFillFix1(x=x, y=y, latitude=latitude, projection="")
        xy <- badFillFix2(x=xy$x, y=xy$y, xorig=xorig, yorig=yorig)
        x <- xy$x
        y <- xy$y
        polygon(x, y, density=density, angle=angle, border=border, col=col, lty=lty, ..., fillOddEven=fillOddEven)
    }
}

mapImage <- function(longitude, latitude, z, zlim, zclip=FALSE,
                     breaks, col, colormap, border=NA,
                     lwd=par("lwd"), lty=par("lty"), missingColor=NA,
                     filledContour=FALSE, gridder="binMean2D",
                     debug=getOption("oceDebug"))
{
    if ("none" == .Projection()$type)
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
    if (!is.matrix(z))
        stop("z must be a matrix")
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
    oceDebug(debug, "zclip: ", zclip, "\n")
    oceDebug(debug, "breaks: ", paste(breaks, collapse=" "), "\n")
    oceDebug(debug, "col: ", paste(col, collapse=" "), "\n")
    ## 20140816 END
    ni <- dim(z)[1]
    nj <- dim(z)[2]
    ##dlongitude <- longitude[2] - longitude[1] # FIXME: incorrect for irregular grids
    ##dlatitude <- latitude[2] - latitude[1]
    zmin <- min(z, na.rm=TRUE)
    zmax <- max(z, na.rm=TRUE)
    zrange <- zmax - zmin
    ##usr <- par('usr')
    ##xmin <- usr[1]
    ##xmax <- usr[2]
    ##ymin <- usr[3]
    ##ymax <- usr[4]
    ##allowedSpan <- (xmax - xmin) / 5   # KLUDGE: avoid lines crossing whole domain
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
            oceDebug(debug, "extenging breaks range since no zlim given\n")
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
    ## issue #638 - kludge to get data into same longitue scheme as axes
    usr12 <- par("usr")[1:2]
    xrange <- range(xy$x, na.rm=TRUE)
    if (xrange[1] > usr12[2])
        xy$x <- xy$x - 360

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
        ##rx <- range(xy$x[is.finite(xy$x)], na.rm=TRUE)
        ##ry <- range(xy$y[is.finite(xy$y)], na.rm=TRUE)
        ##f <- if (is.logical(filledContour)) 1 else as.integer(round(filledContour))
        ## FIXME: I'm not sure this will work well generally; I'm setting NN to 
        ## FIXME: get about 5 points per grid cell.
        ## N is number of points in view
        N <- sum(par('usr')[1]<=xy$x & xy$x<=par('usr')[2] &
                 par('usr')[3]<=xy$y & xy$y<=par('usr')[4], na.rm=TRUE)
        NN <- sqrt(N / 10)
        xg <- seq(par('usr')[1], par('usr')[2], length.out=NN)
        yg <- seq(par('usr')[3], par('usr')[4], length.out=NN)
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
        xtrim <- par('usr')[1:2]
        ytrim <- par('usr')[3:4]
        inFrame <- xtrim[1] <= xx & xx <= xtrim[2] & ytrim[1] <= yy & yy <= ytrim[2]
        oceDebug(debug, "before trimming, length(xx): ", length(xx), "\n")
        xx <- xx[inFrame]
        yy <- yy[inFrame]
        zz <- zz[inFrame]
        oceDebug(debug, "after trimming, length(xx): ", length(xx), "\n")
        ## chop to points within plot area
        if (gridder== "akima") {
            oceDebug(debug, "using akima::interp()\n")
            if (requireNamespace("akima", quietly=TRUE)) {
                i <- akima::interp(x=xx, y=yy, z=zz, xo=xg, yo=yg)
            } else {
                stop("must install.packages(\"akima\") for mapImage() with filledContour and gridder='akima'")
            }
        } else {
            oceDebug(debug, "using binMean2D()\n")
            binned <- binMean2D(xx, yy, zz, xg, yg, fill=TRUE)
            i <- list(x=binned$xmids, y=binned$ymids, z=binned$result)
        }
        if (any(is.finite(i$z))) {
            ## issue726: add a tiny bit to breaks, to mimic filledContour=FALSE
            small <- .Machine$double.eps
            .filled.contour(i$x, i$y, i$z, levels=breaks+small, col=col)
        } else {
            warning("no valid z")
        }
    } else {
        oceDebug(debug, "using polygons, as opposed to filled contours\n")
        colFirst <- col[1]
        colLast <- tail(col, 1)
        ## if (debug > 10) { ## FIXME (issue 522): retain this test code until 2014-oct
        ##     message("breaksMin: ", breaksMin)
        ##     message("breaksMax: ", breaksMax)
        ##     message("Z:")
        ##     print(Z)
        ## }
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
            ## IMPORTANT: whether to write 'breaks' or 'breaks+small' below
            ## IMPORTANT: is at the heart of several issues, including
            ## IMPORTANT: issues 522, 655 and possibly 726.
            ## issue522: this was w <- which(zval <= breaks)[1]
            ## issue655: this was w <- which(zval <= breaks)[1]
            ## sometime later: w <- which(zval < breaks + 1*small)[1]
            w <- which(zval <= breaks)[1]
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
    ## use range 0 to 360 if input longitudes in that way
    if (any(longitude > 180))
        lon <- ifelse(lon < 0, lon+360, lon)
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
    if (missing(latitude))
        stop("latitude is missing")
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
        zone <- floor((longitude / 6) + 31)
        zone <- ifelse(zone > 60, zone-60, zone)
        ## message("zone not given; inferred to be ", zone)
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
    N0 <- 0                            # to obey Landsat-8 convention
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

## 1. alsk overdraws world, pluts it's a niche proj (Alaska)
## 2. calcofi is deleted because it's nor really a projection; it's more
##    like a coordinate transformation.
## 3. isea is deleted because it causes a segmentation fault on coastlineWorld.
## 4. krovak causes overdrawn coastlines, plus it's a niche proj (Czech Republic)
## 5. labrd is deleted because it returns NaN for every point
##    on the coastlineWorld; fixing this is not a high priority
##    given that it is a niche projection that has caused problems
##    in PROJ.4 also.
##knownProj4<-c("aea", "aeqd", "aitoff", "alsk", "bipc", "bonne", #"calcofi",
knownProj4 <- c("aea", "aeqd", "aitoff",         "bipc", "bonne",
                "cass", "cc", "cea", "collg", "crast", "eck1", "eck2", "eck3",
                "eck4", "eck5", "eck6", "eqc", "eqdc", "euler", "etmerc",
                "fahey", "fouc", "fouc_s", "gall", "geos", "gn_sinu", "gnom",
                ##"goode", "gs48", "gs50", "hatano", "healpix", "rhealpix",
                "goode",                   "hatano", "healpix", "rhealpix",
                ##"igh","imw_p", "isea", "kav5", "kav7", "krovak", "labrd",
                "igh",  "imw_p",         "kav5", "kav7",
                ##"laea", "lonlat", "latlon", "lcc", "lcca", "leac", "lee_os",
                "laea",   "lonlat", "latlon", "lcc", "lcca", "leac",
                "loxim", "lsat", "mbt_s", "mbt_fps", "mbtfpp", "mbtfpq",
                "mbtfps", "merc", "mil_os", "mill", "moll", "murd1", "murd2",
                ##"murd3", "natearth", "nell", "nell_h", "nsper", "nzmg",
                "murd3",   "natearth", "nell", "nell_h", "nsper",
                "ob_tran", "ocea", "oea", "omerc", "ortho", "pconic", "poly",
                "putp1", "putp2", "putp3", "putp3p", "putp4", "putp4p",
                "putp5", "putp5p", "putp6", "putp6p", "qsc", "qua_aut",
                "robin", "rouss", "sinu", "somerc", "stere", "sterea"
                ##"gstmerc", "tcea", "tissot", "tmerc", "tpeqd", "tpers", "ups"
                ,            "tcea", "tissot", "tmerc", "tpeqd", "tpers", "ups",
                "urm5", "urmfps", "utm", "vandg", "vitk1", "wag1", "wag2",
                "wag3", "wag4", "wag5", "wag6", "weren", "wink1", "wintri")

lonlat2map <- function(longitude, latitude, projection="")
{
    ##cat("map.R:1676 in lonlat2map(..., projection='", projection, "', ...)\n", sep="")
    ## NOTE: the proj4 method can run into errors (e.g. "ortho" for points on opposite
    ## side of the earth) an may have to be done (slowly) point by point; a warning is
    ## issued if so.
    if (is.list(longitude)) {
        latitude <- longitude$latitude
        longitude <- longitude$longitude
    }
    if (missing(latitude))
        stop("latitude is missing")
    n <- length(longitude)
    if (n != length(latitude))
        stop("lengths of longitude and latitude must match but they are ", n, " and ", length(latitude))
    ## Use proj4 if it has been set up (and still exists).
    #message("projection=", projection)
    if ("" == projection) projection <- .Projection()$projection # FIXME
    ##cat("  projection='", projection, "'\n", sep='')
    ## 20150612 if ('+' != substr(projection, 1, 1)) {
    ## 20150612     ## message("lonlat2map (mapproj case)")
    ## 20150612     ## mapproj case
    ## 20150612     if (!requireNamespace("mapproj", quietly=TRUE))
    ## 20150612         stop("must install 'mapproj' package to use mapproj-style map projections")
    ## 20150612     xy <- NULL
    ## 20150612     #message("parameters:")
    ## 20150612     #str(parameters)
    ## 20150612     #message("orientation:")
    ## 20150612     #str(orientation)
    ## 20150612     try({
    ## 20150612         #message("lon and lat:");
    ## 20150612         #str(data.frame(longitude, latitude))
    ## 20150612         if (is.null(parameters)) {
    ## 20150612             xy <- mapproj::mapproject(longitude, latitude)
    ## 20150612         } else {
    ## 20150612             xy <- mapproj::mapproject(longitude, latitude,
    ## 20150612                                       projection=projection, parameters=parameters, orientation=orientation)
    ## 20150612         }
    ## 20150612         #message("xy:")
    ## 20150612         #str(xy)
    ## 20150612         .Projection(list(type="mapproj", projection=projection))     # turn proj4 off, in case it was on
    ## 20150612         ## if (nchar(projection) > 1 && (is.null(orientation) || (orientation[1] == 90 && orientation[3] == 0))) {
    ## 20150612         ##     cmd <- "+proj="
    ## 20150612         ##     proj <- "?"
    ## 20150612         ##     ## See http://www.remotesensing.org/geotiff/proj_list
    ## 20150612         ##     ## After the conversion there may be a comment listing corequisites
    ## 20150612         ##     if (projection == "aitoff") proj <- "(no equivalent)"
    ## 20150612         ##     if (projection == "albers") proj <- "aea" # needs lat0 lat1
    ## 20150612         ##     if (projection == "bonne") proj <- "bonne" # needs lat0
    ## 20150612         ##     if (projection == "gall") proj <- "gall"
    ## 20150612         ##     ## if (projection == "lambert") proj <- "laea" ## ??
    ## 20150612         ##     if (projection == "lambert") proj <- "lcc"
    ## 20150612         ##     if (projection == "mercator") proj <- "merc"
    ## 20150612         ##     if (projection == "mollweide") proj <- "moll"
    ## 20150612         ##     if (projection == "orthographic") proj <- "ortho"
    ## 20150612         ##     if (projection == "polyconic") proj <- "pconic"
    ## 20150612         ##     if (projection == "robin") proj <- "robin"
    ## 20150612         ##     ## FIXME: what about sterea?
    ## 20150612         ##     if (projection == "stereographic") proj <- "stere"
    ## 20150612         ##     if (projection == "wintri") proj <- "wintri"
    ## 20150612         ##     cmd <- paste("+proj=", proj, sep="")
    ## 20150612         ##     if (!is.null(parameters)) {
    ## 20150612         ##         names <- names(parameters)
    ## 20150612         ##         if ("lat0" %in% names) cmd <- paste(cmd, " +lat_0=", parameters[["lat0"]], sep="")
    ## 20150612         ##         if ("lat1" %in% names) cmd <- paste(cmd, " +lat_1=", parameters[["lat1"]], sep="")
    ## 20150612         ##     }
    ## 20150612         ##     if (!is.null(orientation))
    ## 20150612         ##         cmd <- paste(cmd, " +lon_0=", orientation[2], sep="")
    ## 20150612         ##     if (projection == "stereographic")
    ## 20150612         ##         cmd <- paste(cmd, " +lat_0=90", sep="")
    ## 20150612         ##     message("mapPlot() suggestion: try using projection=\"", cmd, "\"")
    ## 20150612         ## }
    ## 20150612     }, silent=!TRUE)
    ## 20150612     if (is.null(xy)) {
    ## 20150612         xy <- list(x=NA, y=NA)
    ## 20150612         warning("problem with mapproj-style projection. Please use PROJ.4 style\n")
    ## 20150612     }
    ## 20150612     #message("xy:")
    ## 20150612     #str(xy)
    ## 20150612 } else {                           
    ##message("PROJ.4 case")
    ## proj4 case
    pr <- gsub(" .*$", "", gsub("^\\+proj=", "", projection))
    if (!(pr %in% knownProj4))
        stop("projection '", pr, "' is unknown; try one of: ", paste(knownProj4, collapse=','))
                                        #if (length(grep("aitoff", pr))) stop("+proj=aitoff cannot be used")
                                        #if (length(grep("robin", pr))) stop("+proj=robin cannot be used")
                                        #if (length(grep("wintri", pr))) stop("+proj=wintri cannot be used")
    ll <- cbind(longitude, latitude)
    ## Next added 20150523 for rgdal transition; keep old code for a while
    if (0 == length(grep("ellps=", projection)))
        projection<- paste(projection, "+ellps=sphere")
    n <- length(longitude)
    if (!requireNamespace("rgdal", quietly=TRUE))
        stop('must install.packages("rgdal") to plot maps with projections')
    owarn <- options()$warn
    options(warn=-1)
    XY <- rgdal::project(ll, proj=as.character(projection), inv=FALSE)
    options(warn=owarn)
    xy <- list(x=XY[,1], y=XY[,2])
    ## 20150523 if (!getOption("externalProj4", FALSE)) {
    ## 20150523     ## message("doing PROJ.4 calculations within Oce, for speed and accuracy")
    ## 20150523     if (0 == length(grep("ellps=", projection)))
    ## 20150523         projection<- paste(projection, "+ellps=sphere")
    ## 20150523     n <- length(longitude)
    ## 20150523     owarn <- options()$warn
    ## 20150523     options(warn=-1)
    ## 20150523     XY <- rgdal::project(ll, proj=as.character(projection), inv=FALSE)
    ## 20150523     options(warn=owarn)
    ## 20150523     xy <- list(x=XY[,1], y=XY[,2])
    ## 20150523     ##pre-rgdal XY <- .C("proj4_interface", as.character(projection), as.integer(TRUE),
    ## 20150523     ##pre-rgdal          as.integer(n), as.double(longitude), as.double(latitude),
    ## 20150523     ##pre-rgdal          X=double(n), Y=double(n), NAOK=TRUE)
    ## 20150523     ##pre-rgdal xy <- list(x=XY$X, y=XY$Y)
    ## 20150523 } else {
    ## 20150523     ## message("doing projection calculations with 'proj4' package")
    ## 20150523     if (!requireNamespace("proj4", quietly=TRUE))
    ## 20150523         stop("must install 'proj4' package to get options(externalProj4=TRUE) to work")
    ## 20150523     m <- NULL                 # for the try()
    ## 20150523     try({
    ## 20150523         m <- proj4::project(ll, proj=projection)
    ## 20150523     }, silent=TRUE)
    ## 20150523     if (is.null(m)) {
    ## 20150523         m <- matrix(unlist(lapply(1:n, function(i)
    ## 20150523                                   {
    ## 20150523                                       t <- try({proj4::project(ll[i,], proj=projection)}, silent=TRUE)
    ## 20150523                                       if (inherits(t, "try-error")) c(NA, NA) else t[1,]
    ## 20150523                                   })),
    ## 20150523                     ncol=2, byrow=TRUE)
    ## 20150523         warning("proj4 calculation is slow because it was done pointwise")
    ## 20150523     }
    ## 20150523     xy <- list(x=m[,1], y=m[,2])
    ## 20150523 }
    .Projection(list(type="proj4", projection=projection))
    ##mapproj::.Last.projection(list(projection="")) # turn off mapproj, in case it was on
    xy
}


