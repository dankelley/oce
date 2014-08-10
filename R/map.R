## Author notes on proj4: see
##  http://stackoverflow.com/questions/tagged/proj4

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
    if (!usingProj4() && (!exists(".Last.projection") || 0 == nchar(.Last.projection()$projection)))
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
                    projection="mollweide", parameters=NULL, orientation=NULL,
                    debug=getOption("oceDebug"),
                    ...)
{
    oceDebug(debug, "mapPlot(longitude, latitude", 
            ", longitudelim=",
             if (missing(latitudelim)) "(missing)" else c("c(", paste(format(longitudelim, digits=4), collapse=","), ")"),
             ", longitudelim=",
             if (missing(latitudelim)) "(missing)" else c("c(", paste(format(latitudelim, digits=4), collapse=","), ")"),
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
    if (is.logical(grid[1]) && grid[1])
        grid <- rep(15, 2)

    xy <- lonlat2map(longitude, latitude, 
                     projection=projection, parameters=parameters, orientation=orientation)
    if (!missing(latitudelim) && 0 == diff(latitudelim)) stop("lattudelim must contain two distinct values")
    if (!missing(longitudelim) && 0 == diff(longitudelim)) stop("longitudelim must contain two distinct values")
    limitsGiven <- !missing(latitudelim) && !missing(longitudelim)
    x <- xy$x
    y <- xy$y
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
        plot(x, y, type=type,
             xlim=range(box$x, na.rm=TRUE), ylim=range(box$y, na.rm=TRUE),
             xlab="", ylab="", asp=1, axes=FALSE, ...)
        ## points(jitter(box$x), jitter(box$y), pch=1, col='red')
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
        ## Grid lines and axes.
        ## 2014-06-29
        ## Find ll and ur corners of plot, if possible, for use in calculating
        ## lon and lat spans.  This will not work in all cases; e.g. for a
        ## world map in mollweide projection, the bounding points will be "in
        ## space", but we catch such problems when we calculate span below.
        usr <- par('usr')
        xll <- usr[1]
        yll <- usr[3]
        xur <- usr[2]
        yur <- usr[4]

        options <- options('warn') # turn off warnings temporarily
        options(warn=-1) 

        ## Now next may fail for e.g. molleweide has ll and ur that are
        ## un-invertable, since the globe may not fill the whole plotting area.
        ll <- map2lonlat(xll, yll)
        ur <- map2lonlat(xur, yur)
        if (!is.finite(ll$longitude) || !is.finite(ll$latitude) ||
            !is.finite(ur$longitude) || !is.finite(ur$latitude)) {
            ur <- list(longitude=180, latitude=90)
            ll <- list(longitude=-180, latitude=-90)
        }
        span <- if (!is.finite(ur$latitude - ll$latitude)) diff(latitudelim) else ur$latitude - ll$latitude
        oceDebug(debug, "span=", span, "\n")
        if (missing(latitudelim)) {
            if (span > 45) {
                grid[2] <- 15 # this looks nice on global maps
                latlabs <- seq(-90, 90, grid[2])
            } else {
                latlabs <- pretty(c(ll$latitude, ur$latitude))
                grid[2] <- diff(latlabs[1:2])
            }
            oceDebug(debug,  "latitudelim not provided; grid[2]=", grid[2], "\n")
        } else {
            ##span <- if (is.na(ur$latitude - ll$latitude)) diff(latitudelim) else ur$latitude - ll$latitude
            if (span > 45) {
                grid[2] <- 15 
                latlabs <- seq(-90, 90, grid[2])
                oceDebug(debug, "span=", span, "exceeds 45 so setting grid to ", grid[2], "\n")
            } else if (5 < span && span <= 45) {
                grid[2] <- 5 
                latlabs <- seq(-90, 90, grid[2])
                oceDebug(debug, "span=", span, "between 5 and 45 so setting grid to ", grid[2], "\n")
            } else {
                latlabs <- pretty(c(ll$latitude, ur$latitude))
                grid[2] <- diff(latlabs[1:2])
                oceDebug(debug, "latitudelim provided; setting grid[2]=", grid[2], "\n")
            }
        }

        if (missing(longitudelim)) {
            if (span > 45) {
                grid[1] <- 15 # this looks nice on global maps
                ## tweak endpoints to avoid losing things "on the edge" say for mollweid
                lonlabs <- seq(-180+0.01, 180-0.01, length.out=24)
                grid[1] <- lonlabs[2] - lonlabs[1]
            } else {
                lonlabs <- pretty(c(ll$longitude, ur$longitude))
                grid[1] <- diff(lonlabs[1:2])
            }
            oceDebug(debug, "no longitudelim provided; setting grid[1]=", grid[1], "\n")
        } else {
            span <- if (is.na(ur$longitude - ll$longitude))
                abs(diff(longitudelim)) else
                    abs(ur$longitude - ll$longitude)
            oceDebug(debug, "span:", span, "\n")
            if (45 < span) {
                oceDebug(debug, "lon case A", "\n")
                grid[1] <- 15          # divides 45 evenly
                lonlabs <- seq(-180, 180, grid[1])
            } else if (5 < span && span <= 45) {
                oceDebug(debug, "lon case B", "\n")
                grid[1] <- 5 
                lonlabs <- seq(-180, 180, grid[1])
            } else {
                oceDebug(debug, "lon case C", "\n")
                lonlabs <- pretty(c(ll$longitude, ur$longitude))
                grid[1] <- diff(lonlabs[1:2])
            }
            oceDebug(debug, "longitudelim provided; setting grid[1]=", grid[1], "\n")
        }
        oceDebug(debug, "grid:", grid[1], " ", grid[2], "\n")
        ## if (FALSE) { # before 2014-06-29
        ##     inc <- if (is.logical(grid[2]) && grid[2]) 25 else grid[2]
        ##     latlabs <- seq(-90, 90, inc)
        ##     if (!missing(latitudelim)) { 
        ##         incBest <- diff(pretty(latitudelim, n=4, n.min=3))[1]
        ##         oceDebug(debug, "latitudelim:", latitudelim, ", inc:", inc, ", incBest:", incBest, "\n")
        ##         if (inc / incBest > 3) {
        ##             message("initially latlabs");print(latlabs)
        ##             ## extend range because the lims may not agree given the plot geometry
        ##             latSmall <- mean(latitudelim) - 5 * (latitudelim[2] - latitudelim[1])
        ##             latLarge <- mean(latitudelim) + 5 * (latitudelim[2] - latitudelim[1])
        ##             latlabs <- pretty(c(latSmall, latLarge), n=4, n.min=3)
        ##             grid[2] <- diff(latlabs[1:2])
        ##             message("later latlabs");print(latlabs)
        ##         }
        ##     }
        ##     inc <- if (is.logical(grid[1]) && grid[1]) 15 else grid[1]
        ##     lonlabs <- seq(-180, 180, inc)
        ##     if (!missing(longitudelim)) { 
        ##         oceDebug(debug, "longitudelim:", longitudelim, ", inc:", inc, ", incBest:", incBest, "\n")
        ##         incBest <- diff(pretty(longitudelim, n=4, n.min=3))[1]
        ##         if (inc / incBest > 3) {
        ##             ## extend range because the lims may not agree given the plot geometry
        ##             lonSmall <- mean(longitudelim) - 5 * (longitudelim[2] - longitudelim[1])
        ##             lonLarge <- mean(longitudelim) + 5 * (longitudelim[2] - longitudelim[1])
        ##             lonlabs <- pretty(c(lonSmall, lonLarge), n=4, n.min=3)
        ##             grid[1] <- diff(lonlabs[1:2])
        ##         }
        ##     }
        ## }
        if ((is.logical(grid[1]) && grid[1]) || grid[1] > 0) {
            oceDebug(debug, "grid lines for lat:", latlabs, "\n")
            mapMeridians(latlabs)
        }
## <<<<<<< HEAD
##         oceDebug(debug, "latlabs:", latlabs, "\n")
##         usr <- par('usr')
##         labelAt <- NULL
##         lab <- vector("expression", length(latlabs))
##         nlab <- 0
##         lastAtY <- NA
##         ##cat("inc:", inc, "\n")
##         ##message("About to draw axes.  NOTE: not converted to proj4 yet")
##         usr <- par('usr')
##         scale <- max(usr[2] - usr[1], usr[4] - usr[3])
##         ##message("scale: ", scale)
##         if (drawGrid) {
##             for (latlab in latlabs) {
##                 if (-90 <= latlab && latlab <= 90) {
##                     try({
##                         o <- optimize(function(lon) {
##                                       abs(lonlat2map(lon, latlab)$x-usr[1])},
##                                       lower=-180, upper=180)
##                         ## message("latlab: ", latlab, ", o$objective/scale: ", o$objective/scale)
##                         if (o$objective > scale / 100)
##                             next
##                         lonlab <- o$minimum
##                         ## message("lonlab: ", lonlab)
##                         #if (abs(latlab - 45) < 2) browser()
##                         at <- lonlat2map(lonlab, latlab)
##                         ## message("at: ", paste(at, collapse=" "))
##                         if (usr[3] < at$y && at$y < usr[4]) {
##                             ## message("lonlab:", lonlab, " INSIDE")
##                             labelAt <- c(labelAt, at$y)
##                             nlab <- nlab + 1
##                             lab[nlab] <- formatPosition(round(latlab, digits=2), # round because we avoid poles
##                                                         isLat=TRUE, type="expression", showHemi=showHemi)
##                             oceDebug(debug, "  y axis: ",
##                                      formatPosition(latlab, isLat=TRUE, type="string", showHemi=showHemi),
##                                      "at$y", at$y, "lastAtY", lastAtY, "\n")
##                             if (debug>90) {
##                                 mtext(formatPosition(latlab, isLat=TRUE, type="expression", showHemi=showHemi),
##                                       line=par('mgp')[2]-abs(par('tcl')), # no ticks, so move closer
##                                       side=2, at=at$y, srt=90, cex=par('cex'), ...) # how to rotate?
##                                 warning("DEVELOPER message: since debug>90, axis labels were drawn with 'mtext' instead of 'axis'")
##                             }
##                             lastAtY <- at$y
##                         } else {
##                             ##cat("lonlab:", lonlab, "OUTSIDE\n")
##                         }
##                     }, silent=TRUE)
##                 }
##             }
##         }
##         if (nlab > 0) {
##             if (debug<=90) { # FIXME: 2014-01-09 remove this eventually
##                 axis(2, at=labelAt,
##                      labels=lab[1:nlab],
##                      ##labels=c("DAN", "BOY", "OK?"),
##                      col.ticks="lightgray",
##                      mgp=getOption('oceMgp'))
## =======
        if ((is.logical(grid[2]) && grid[2]) || grid[2] > 0) {
            oceDebug(debug, "grid lines for lon:", lonlabs, "\n")
            mapZones(lonlabs)
        }
        if (is.null(lonlabel))
            lonlabel <- lonlabs
        if (is.null(latlabel))
            latlabel <- latlabs
        if (is.null(sides))
            sides <- 1:2
        TICK <- FALSE                  # ticks look bad for angled grid lines
        LINE <- -3/4                   # labels snug to box
        axisSpan <- max(usr[2]-usr[1], usr[4]-usr[3])
        if (1 %in% sides) {    # bottom side
            AT <- NULL
            LAB <- NULL
            for (lab in lonlabel) {
                o <- optimize(function(lat) abs(lonlat2map(lab,lat)$y-usr[3]),lower=-89.9999,upper=89.9999)
                if (is.na(o$objective) || o$objective > 0.01*axisSpan) next
                x <- lonlat2map(lab, o$minimum)$x
                if (!is.na(x) && usr[1] < x && x < usr[2]) {
                    AT <- c(AT, x)
                    LAB <- c(LAB, paste(lab, "E", sep=""))
                    oceDebug(debug, "lonlabel", lab, "E intersects side 1\n")
                } else {
                    oceDebug(debug, "lonlabel", lab, "E does not intersect side 1\n")
                }
            }
            if (!is.null(AT)) axis(side=1, at=AT, labels=fixneg(LAB), line=LINE, tick=TICK)
        }
        if (2 %in% sides) {    # left side
            AT <- NULL
            LAB <- NULL
            for (lab in latlabel) {
                o <- optimize(function(lon) abs(lonlat2map(lon,lab)$x-usr[1]),lower=-180,upper=180)
                if (is.na(o$objective) || o$objective > 0.01*axisSpan) next
                y <- lonlat2map(o$minimum, lab)$y
                if (!is.na(y) && usr[3] < y && y < usr[4]) {
                    AT <- c(AT, y)
                    LAB <- c(LAB, paste(lab, "N", sep=""))
                    oceDebug(debug, "latlabel", lab, "N intersects side 2\n")
                }
            }
            if (!is.null(AT)) axis(side=2, at=AT, labels=fixneg(LAB), line=LINE, tick=TICK)
        }
##<<<<<<< HEAD
##        ## Prevent labelling both 180W and 180E on top of each other (not sure how
##        ## axis permits this, actually).
##        oceDebug(debug, "lonlabs:", lonlabs, "\n")
##        if ((is.logical(grid[2]) && grid[2]) || grid[2] > 0) {
##            mapZones(lonlabs)
##        }
##        lab <- vector("expression", length(latlabs))
##        nlab <- 0
##        lastx <- NA
##        dxMin <- (usr[2] - usr[1]) / 10
##        mgp <- par('mgp')
##        for (lonlab in lonlabs) {
##            if (-180 <= lonlab && lonlab < 180) { # the limits are the lonlim
##                ##message("lonlab: ", lonlab)
##                try({
##                    o <- optimize(function(lat) {
##                                  abs(lonlat2map(lonlab, lat)$y-usr[3])},
##                                  lower=-89, upper=89)
##                    if (o$object > scale / 100)
##                        next
##                    latlab <- o$minimum
##                    at <- lonlat2map(lonlab, latlab)
##                    if (usr[1] < at$x && at$x < usr[2]) {
##                        labelAt <- c(labelAt, at$x)
##                        nlab <- nlab + 1
##                        lab[nlab] <- formatPosition(lonlab, isLat=FALSE, type="expression", showHemi=showHemi)
##                        if (is.na(lastx) || abs(at$x - lastx) > dxMin) {
##                            if (debug>90) {
##                                mtext(formatPosition(lonlab, isLat=FALSE, type="expression", showHemi=showHemi),
##                                      side=1,
##                                      line=mgp[2]-abs(par('tcl')), # no ticks, so move closer
##                                      at=at$x, cex=par('cex'), ...)
##                                warning("DEVELOPER message: since debug>90, axis labels were drawn with 'mtext' instead of 'axis'")
##                            }
##                            lastx <- at$x
##                        }
##                        oceDebug(debug, "  x axis: ",
##                                 formatPosition(lonlab, isLat=FALSE, type="string", showHemi=showHemi),
##                                 "at$x", at$x, "lastx", lastx, "\n")
##                    }
##                }, silent=TRUE)
##=======
        if (3 %in% sides) {    # topside
            warning("axis on top side of map is not working yet (contact developer)")
            AT <- NULL
            LAB <- NULL
            for (lab in lonlabel) {
                o <- optimize(function(lat) abs(mapproject(lab,lat)$y-usr[4]),lower=-90,upper=90)
                if (is.na(o$objective) || o$objective > 0.01) next
                x <- mapproject(lab, o$minimum)$x
                if (!is.na(x) && usr[3] < x && x < usr[4]) {
                    AT <- c(AT, x)
                    LAB <- c(LAB, paste(lab, "E", sep=""))
                    oceDebug(debug, "lonlabel", lab, "E intersects side 3\n")
                }
##>>>>>>> map-axes
            }
            for (lab in latlabel) {
                t <- try({o <- optimize(function(lon) abs(mapproject(lon,lab)$y-usr[4]),lower=-180,upper=180)})
                if (is.na(o$objective) || o$objective > 0.01) next
                x <- mapproject(o$minimum, lab)$x
                if (!is.na(x) && usr[3] < x && x < usr[4]) {
                    AT <- c(AT, x)
                    LAB <- c(LAB, paste(lab, "N", sep=""))
                    oceDebug(debug, "latlabel", lab, "N intersects side 3\n")
                }
            }
            if (!is.null(AT)) axis(side=3, at=AT, labels=fixneg(LAB), line=LINE, tick=TICK)
        }
        if (4 %in% sides) {    # right side
            warning("axis on right-hand side of map is not working yet (contact developer)")
            AT <- NULL
            LAB <- NULL
            for (lab in lonlabel) {
                o <- optimize(function(lat) abs(mapproject(lab,lat)$x-usr[2]),lower=-90,upper=90)
                if (is.na(o$objective) || o$objective > 0.01) next
                y <- mapproject(lab, o$minimum)$y
                if (!is.na(y) && usr[3] < y && y < usr[4]) {
                    AT <- c(AT, y)
                    LAB <- c(LAB, paste(lab, "E", sep=""))
                    oceDebug(debug, "lonlabel", lab, "E intersects side 4\n")
                }
            }
            for (lab in latlabel) {
                t <- try({o <- optimize(function(lon) abs(mapproject(lon,lab)$x-usr[2]),lower=-180,upper=180)})
                if (is.na(o$objective) || o$objective > 0.01) next
                y <- mapproject(lab, o$minimum)$y
                if (!is.na(y) && usr[3] < y && y < usr[4]) {
                    AT <- c(AT, y)
                    LAB <- c(LAB, paste(lab, "N", sep=""))
                    oceDebug(debug, "latlabel", lab, "N intersects side 4\n")
                }
            }
            if (!is.null(AT)) axis(side=4, at=AT, labels=fixneg(LAB), line=LINE, tick=TICK)
        }
        ## 20114-06-28: below is code that used to draw axes, then only
        ## at bottom and left, and without intermixing of lats and lons.
        ##
        ## } else {
        ##     oceDebug(debug, "sides:", paste(sides, collapse=" "), "\n")
        ##     ## Neither lonlab and latlab was given, so use default axes
        ##     usr <- par('usr')
        ##     if (2 %in% sides) {            # left hand side
        ##         labelAt <- NULL
        ##         lab <- vector("character", length(latlabs))
        ##         nlab <- 0
        ##         lastAtY <- NA
        ##         if (drawGrid) {
        ##             for (latlab in latlabs) {
        ##                 if (-90 <= latlab && latlab <= 90) {
        ##                     try({
        ##                         o <- optimize(function(lon) abs(mapproject(lon, latlab)$x-usr[1]),
        ##                                       lower=-180, upper=180)
        ##                         if (o$objective > 0.01)
        ##                             next
        ##                         lonlab <- o$minimum
        ##                         at <- mapproject(lonlab, latlab)
        ##                         if (usr[3] < at$y && at$y < usr[4]) {
        ##                             labelAt <- c(labelAt, at$y)
        ##                             nlab <- nlab + 1
        ##                             lab[nlab] <- formatPosition(latlab, isLat=TRUE, type="string", showHemi=showHemi)
        ##                             oceDebug(debug, "  y axis: ",
        ##                                      formatPosition(latlab, isLat=TRUE, type="string", showHemi=showHemi),
        ##                                      "at$y", at$y, "lastAtY", lastAtY, "\n")
        ##                             if (debug>90) {
        ##                                 mtext(formatPosition(latlab, isLat=TRUE, type="string", showHemi=showHemi),
        ##                                       line=par('mgp')[2]-abs(par('tcl')), # no ticks, so move closer
        ##                                       side=2, at=at$y, srt=90, cex=par('cex'), ...) # how to rotate?
        ##                                 warning("DEVELOPER message: since debug>90, axis labels were drawn with 'mtext' instead of 'axis'")
        ##                             }
        ##                             lastAtY <- at$y
        ##                         } else {
        ##                             ##cat("lonlab:", lonlab, "OUTSIDE\n")
        ##                         }
        ##                     }, silent=TRUE)
        ##                 }
        ##             }
        ##         }
        ##         if (nlab > 0) {
        ##             axis(2, at=labelAt, labels=lab[1:nlab], col.ticks="lightgray",
        ##                  mgp=getOption('oceMgp'))
        ##         }
        ##     } ## side 2
        ##     if (1 %in% sides) { # bottom side
        ##         oceDebug("drawing lon and lat on side 1 (bottom)\n")
        ##         labelAt <- NULL
        ##         lab <- vector("character", length(latlabs))
        ##         nlab <- 0
        ##         lastx <- NA
        ##         dxMin <- (usr[2] - usr[1]) / 10
        ##         mgp <- par('mgp')
        ##         for (lonlab in lonlabs) {
        ##             if (-180 <= lonlab && lonlab < 180) { # the limits are the lonlim
        ##                 try({
        ##                     o <- optimize(function(lat) abs(mapproject(lonlab, lat)$y-usr[3]), lower=-89, upper=89)
        ##                     if (o$object > 0.01)
        ##                         next
        ##                     latlab <- o$minimum
        ##                     at <- mapproject(lonlab, latlab)
        ##                     if (usr[1] < at$x && at$x < usr[2]) {
        ##                         labelAt <- c(labelAt, at$x)
        ##                         nlab <- nlab + 1
        ##                         lab[nlab] <- formatPosition(lonlab, isLat=FALSE, type="string", showHemi=showHemi)
        ##                         if (is.na(lastx) || abs(at$x - lastx) > dxMin) {
        ##                             if (debug>90) {
        ##                                 mtext(formatPosition(lonlab, isLat=FALSE, type="string", showHemi=showHemi),
        ##                                       side=1,
        ##                                       line=mgp[2]-abs(par('tcl')), # no ticks, so move closer
        ##                                       at=at$x, cex=par('cex'), ...)
        ##                                 warning("DEVELOPER message: since debug>90, axis labels were drawn with 'mtext' instead of 'axis'")
        ##                             }
        ##                             lastx <- at$x
        ##                         }
        ##                         oceDebug(debug, "  x axis: ",
        ##                                  formatPosition(lonlab, isLat=FALSE, type="string", showHemi=showHemi),
        ##                                  "at$x", at$x, "lastx", lastx, "\n")
        ##                     }
        ##                 }, silent=TRUE)
        ##             }
        ##         }
        ##         if (nlab > 0) {
        ##             if (debug<=90) { # FIXME: 2014-01-09 remove this eventually
        ##                 axis(1, at=labelAt, labels=lab[1:nlab], col.ticks="lightgray",
        ##                      mgp=getOption('oceMgp'))
        ##             }
        ##         }
        ##     }
        ## }
        options(warn=options$warn) 
    }
    oceDebug(debug, "} # mapPlot()\n", unindent=1)
}

mapMeridians <- function(latitude, lty='solid', lwd=0.5*par('lwd'), col='darkgray', ...)
{
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

    ## handle proj4
    ## proj4 <- ""
    ## if (usingProj4()) {
    ##     proj4 <- .Last.proj4()$projection
    ##     if (1 > nchar(proj4)) stop("must call mapPlot() first")
    ## }
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
    if (0 == nchar(.Last.projection()$projection) && 0 == nchar(.Last.proj4()$projection)) {
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
        ## str(xy)
        ## if (usingProj4()) {
        ##     proj4 <- .Last.proj4()$projection
        ##     if (1 > nchar(proj4)) stop("must call mapPlot() first")
        ##     xy <- project(list(longitude=longitude, latitude=latitude), proj=proj4)
        ## } else {
        ##     xy <- mapproject(longitude, latitude)
        ## }
        ## str(xy)
        text(xy$x, xy$y, labels, ...)
    }
}

mapZones <- function(longitude, polarCircle=0, lty='solid', lwd=0.5*par('lwd'), col='darkgray', ...)
{
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
        ##message("proj4-style projection exists")
        xy <- project(list(x=x, y=y), proj=.Last.proj4()$projection, inverse=TRUE)
        return(list(longitude=xy$x, latitude=xy$y))
    } else if (0 == nchar(.Last.projection()$projection)) {
        ##stop("must first set up a projection by calling mapPlot() or lonlat2map()")
    }
    ##message("mapproj-style projection exists")
    ## OK, we know we are using mapproj-style
    lp <- .Last.projection()
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
                           ##message(format(xyTrial[1], digits=4), "E ",
                           ##        format(xyTrial[2], digits=4), "N ",
                           ##        "misfit: ", format(misfit, digits=5), ", error: ", xyp$error)
                           if (error) {
                               return(worstMisfit)
                           } else {
                               worstMisfit <- max(misfit, worstMisfit)
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
    if (!usingProj4() && (!exists(".Last.projection") || 0 == nchar(.Last.projection()$projection)))
        stop("must create a map first, with mapPlot()\n")
    breaksGiven <- !missing(breaks)
    zlimGiven <- !missing(zlim)
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
        zclip <- colormap$zclip
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
                if (missing(col)) {
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
                breaks <- pretty(z, n=breaks)
            }
        }
        if (missing(col))
            col <- oceColorsPalette(n=length(breaks)-1)
        if (is.function(col))
            col <- col(n=length(breaks)-1)
    }
    ## message("mapImage() col:  ", paste(col, collapse=" "))
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
    colFirst <- col[1]
    colLast <- tail(col, 1)
    colorLookup <- function (ij) {
        zval <- Z[ij]
        if (is.na(zval)) return(missingColor)   # whether clipping or not
        if (zval < breaksMin) return(if (zclip) missingColor else colFirst)
        if (zval > breaksMax) return(if (zclip) missingColor else colLast)
        ## w <- which(Z[ij] <= breaks * (1 + small))[1]
        w <- which(zval <= breaks)[1]
        ## if (is.na(w)) message("w=NA for Z[", ij, "] = ", Z[ij])
        ## if (w<=1) message("w<=1 for Z[", ij, "] = ", Z[ij])
        ##if (w <= 1) message("w<=1 at ij: ", ij, "; Z[ij]: ", Z[ij])
        ## FIXME: maybe should be [w] below?  And why is w=1 so bad?
        if (!is.na(w) && w > 1) return(col[-1 + w]) else return(missingColor)
    }
    ## message("range(Z): ", paste(range(Z, na.rm=TRUE), collapse=" to "))
    ## message("head(breaks): ", paste(head(breaks), collapse=" "))
    colPolygon <- sapply(1:(ni*nj), colorLookup)
    ## message("ni*nj: ", ni*nj)
    ## message("below is unique(colPolygon):")
    ## str(unique(colPolygon))
    polygon(xy$x[r$okPoint & !r$clippedPoint], xy$y[r$okPoint & !r$clippedPoint],
            col=colPolygon[r$okPolygon & !r$clippedPolygon],
            border=border, lwd=lwd, lty=lty, fillOddEven=FALSE)

    ## if (debug==5 && !is.na(missingColor)) {
    ##     message("number missing color: ", sum(colPolygon==missingColor))
    ##     message("zclip: ", zclip)
    ## }

    ## message("number of clipped polygons: ", sum(r$clippedPolygon))
    ## message("number of clipped points: ", sum(r$clippedPoints))
    ## message("number of NOT ok points: ", sum(!r$okPoint))
    ## message("number of NOT ok polygons: ", sum(!r$okPolygon))
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
    if (0 == length(grep("^\\+proj", projection))) { 
        ## mapproj case
        xy <- mapproject(longitude, latitude,
                         projection=projection,
                         parameters=parameters, orientation=orientation)
        .Last.proj4(list(projection=""))     # turn proj4 off, in case it was on
    } else {                           
        ## proj4 case
        ll <- cbind(longitude, latitude)
        m <- NULL                 # for the try()
        try({
            m <- project(ll, proj=projection)
        }, silent=TRUE)
        if (is.null(m)) {
            m <- matrix(unlist(lapply(1:n, function(i)
                                      {
                                          t <- try({project(ll[i,], proj=projection)}, silent=TRUE)
                                          if (inherits(t, "try-error")) c(NA, NA) else t[1,]
                                      })),
                        ncol=2, byrow=TRUE)
            warning("proj4 calculation is slow because errors meant it had to be done pointwise")
            ##message("proj4 calculation is slow because errors meant it had to be done pointwise")
        }
        xy <- list(x=m[,1], y=m[,2])
        .Last.proj4(list(projection=projection)) # turn on proj4
        .Last.projection(list(projection="")) # turn off mapproj, in case it was on
    }
    xy
}


