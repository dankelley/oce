plot.topo <- function(x,
                      xlab="", ylab="",
                      asp,
                      center, span,
                      expand=1.5,
                      water.z,
                      water.col,
                      water.lty,
                      water.lwd,
                      land.z,
                      land.col,
                      land.lty,
                      land.lwd,
                      legend.loc="topright",
                      mgp=getOption("oce.mgp"),
                      mar=c(mgp[1]+1,mgp[1]+1,1,1),
                      debug=getOption("oce.debug"),
                      ...)
{
    if (!inherits(x, "topo")) stop("method is only for topo objects")
    oce.debug(debug, "\b\bplot.topo() {\n")

    opar <- par(no.readonly = TRUE)
#    on.exit(par(opar))
    par(mgp=mgp, mar=mar)
    dots <- list(...)

    ######
    gave.center <- !missing(center)
    gave.span <- !missing(span)
    if (gave.center != gave.span)
        stop("must give both 'center' and 'span', or neither one")
    if (gave.center) {
        if (length(center) != 2)
            stop("'center' must contain two values, latitude in deg N and longitude in deg E")
        if (!missing(asp))
            warning("argument 'asp' being ignored, because argument 'center' was given")
        asp <- 1 / cos(center[1] * pi / 180) #  ignore any provided asp
        yr <- center[1] + span * c(-1/2, 1/2) / 111.11
        xr <- center[2] + span * c(-1/2, 1/2) / 111.11 * asp
        oce.debug(debug, "gave center; calculated xr=",xr," yr=", yr, " asp=", asp, "\n")
    } else {
        if (missing(asp)) {
            if ("ylim" %in% names(dots))
                asp <- 1 / cos(mean(range(dots$ylim, na.rm=TRUE)) * pi / 180) # dy/dx
            else
                asp <- 1 / cos(mean(range(x$data$latitude,na.rm=TRUE)) * pi / 180) # dy/dx
        }
        ## Expand
        xr0 <- range(x$data$longitude, na.rm=TRUE)
        yr0 <- range(x$data$latitude, na.rm=TRUE)
        oce.debug(debug, "xr0=", xr0, "\n")
        oce.debug(debug, "yr0=", yr0, "\n")
        if (expand >= 0 && max(abs(xr0)) < 100 && max(abs(yr0) < 70)) { # don't expand if full map
            xr <- mean(xr0) + expand * diff(xr0) * c(-1/2, 1/2)
            yr <- mean(yr0) + expand * diff(yr0) * c(-1/2, 1/2)
        } else {
            xr <- xr0
            yr <- yr0
        }
    }
    zr <- range(x$data$z, na.rm=TRUE)
    if (gave.center && !is.null(dots$xlim))
        stop("cannot give 'xlim' argument if the 'center' argument was given")
    if (gave.center && !is.null(dots$ylim))
        stop("cannot give 'ylim' argument if the 'center' argument was given")
    ## auto-scale based on data in window, if window provided
    if (!is.null(dots$xlim) && !is.null(dots$ylim)) {
        xr <- dots$xlim
        yr <- dots$ylim
    } else {
        xr <- range(x$data$longitude)
        yr <- range(x$data$latitude)
    }

    ## The following is a somewhat provisional hack, to get around a
    ## tendency of plot() to produce latitudes past the poles.
    ## BUG: the use of par("pin") seems to mess up resizing in aqua windows.
    asp.page <- par("pin")[2] / par("pin")[1] # dy / dx
    oce.debug(debug, "par('pin')=",par('pin'), "asp=",asp,"asp.page=", asp.page, "\n")
    if (asp < asp.page) {               # FIXME: this seems to have x and y mixed up (asp=dy/dx)
        oce.debug(debug, "type 1 (will narrow x range)\n")
        d <- asp / asp.page * diff(xr)
        xr <- mean(xr) + d * c(-1/2, 1/2)
        oce.debug(debug, "xr narrowed to:", xr, "\n")
        ## xr[2] <- xr[1] + (xr[2] - xr[1]) * (asp / asp.page)
    } else {
        oce.debug(debug, "type 2 (will narrow y range)\n")
        d <- asp / asp.page * diff(yr)
        yr <- mean(yr) + d * c(-1/2, 1/2)
        oce.debug(debug, "yr narrowed to:", yr, "\n")
        ##yr[2] <- yr[1] + (yr[2] - yr[1]) / (asp / asp.page)
    }

    oce.debug(debug, "xr:", xr, "(before trimming)\n")
    oce.debug(debug, "yr:", yr, "(before trimming)\n")
#    if (xr[1] < -180) xr[1] <- -180
#    if (xr[2] >  180) xr[2] <- 180
    if (yr[1] < -90)  yr[1] <- -90
    if (yr[2] >  90)  yr[2] <-  90
    oce.debug(debug, "xr:", xr, "(after trimming)\n")
    oce.debug(debug, "yr:", yr, "(after trimming)\n")

    ## Data may not extend across plot region
    lon.range <- range(x$data$longitude, na.rm=TRUE)
    lat.range <- range(x$data$latitude, na.rm=TRUE)
    if (xr[1] < lon.range[1]) xr[1] <- lon.range[1]
    if (xr[2] > lon.range[2]) xr[2] <- lon.range[2]
    if (yr[1] < lat.range[1]) yr[1] <- lat.range[1]
    if (yr[2] > lat.range[2]) yr[2] <- lat.range[2]

    plot(xr, yr, asp=asp, xlab=xlab, ylab=ylab, type="n", xaxs="i", yaxs="i", axes=FALSE, ...)
    if (debug > 0)
        points(xr, yr, col="blue", pch=20, cex=3)

    xr.pretty <- pretty(xr)
    yr.pretty <- pretty(yr)
    oce.debug(debug, "xr.pretty=", xr.pretty, "(before trimming)\n")
    oce.debug(debug, "yr.pretty=", yr.pretty, "(before trimming)\n")

if (0){
    if (!(min(yr.pretty) > -80 && max(yr.pretty) < 80))
        yr.pretty <- seq(-90, 90, 45)
    yr.pretty <- subset(yr.pretty, yr.pretty >= yr[1])
    yr.pretty <- subset(yr.pretty, yr.pretty <= yr[2])
    if (!(min(xr.pretty) > 0 && max(xr.pretty) < 360))
        xr.pretty <- seq(0, 360, 45)
    oce.debug(debug, "xr.pretty=", xr.pretty, "(after trimming)\n")
    oce.debug(debug, "yr.pretty=", yr.pretty, "(after trimming)\n")
}


    oce.debug(debug, "xr.pretty=", xr.pretty, "(before trimming)\n")
    oce.debug(debug, "yr.pretty=", yr.pretty, "(before trimming)\n")
    xr.pretty <- subset(xr.pretty, xr.pretty >= xr[1] & xr.pretty <= xr[2])
    yr.pretty <- subset(yr.pretty, yr.pretty >= yr[1] & yr.pretty <= yr[2])
    oce.debug(debug, "xr.pretty=", xr.pretty, "(after trimming)\n")
    oce.debug(debug, "yr.pretty=", yr.pretty, "(after trimming)\n")

    lines(c(xr[1], xr[2], xr[2], xr[1], xr[1]), c(yr[1], yr[1], yr[2], yr[2], yr[1])) # axis box
    axis(1, at=xr.pretty, pos=yr[1])
    axis(3, at=xr.pretty, pos=max(yr), labels=FALSE)
    axis(2, at=yr.pretty, pos=xr[1])
    axis(4, at=yr.pretty, pos=max(xr), labels=FALSE)

    oce.debug(debug, "xr=", xr, "yr=",yr,"\n")
    yaxp <- par("yaxp")
    oce.debug(debug, "par(yaxp)",par("yaxp"),"\n")
    oce.debug(debug, "par(pin)",par("pin"),"\n")

    ## need to clip because contour() does not do so
    xclip <- x$data$longitude < xr[1] | xr[2] < x$data$longitude
    yclip <- x$data$latitude < yr[1] | yr[2] < x$data$latitude
    xx <- x$data$longitude[!xclip]
    yy <- x$data$latitude[!yclip]
    zz <- x$data$z[!xclip, !yclip]
    zr <- range(zz)

    contour(xx, yy, zz,
            levels=0, drawlabels=FALSE, add=TRUE,
            col="black")                # coastline is always black

    legend <- lwd <- lty <- col <- NULL
    if (zr[1] < 0) {
        if (missing(water.z)) {
            if (zr[2] > 0) {
                water.z <- pretty(c(zr[1], 0))
                water.z <- water.z[water.z!=0]
                #cat("water.z=");print(water.z)
                ## Do some tricks to get shelf water as well as deep
                if (max(water.z) == -1000)
                    water.z <- c(water.z, -500, -250, -100, -50)
                else if (max(water.z) == -500)
                    water.z <- c(water.z, -400, -300, -200, -150, -100, -50)
                #cat("after tricks, water.z=");print(water.z)
            } else {
                water.z <- pretty(zr)
            }
            water.z <- rev(sort(water.z))
        }
        nz <- length(water.z)
        if (missing(water.col))
            water.col <- oce.colors.gebco(nz, "water", "line")
        if (missing(water.lty))
            water.lty <- rep(par("lty"), nz)
        else if (length(water.lty) == 1)
            water.lty <- rep(water.lty, nz)
        if (missing(water.lwd))
            water.lwd <- rep(par("lwd"), nz)
        else if (length(water.lwd) == 1)
            water.lwd <- rep(water.lwd, nz)
        legend <- c(legend, water.z)
        lwd    <- c(lwd,    water.lwd)
        lty    <- c(lty,    water.lty)
        col    <- c(col,    water.col)
        #contour(x$data$longitude, x$data$latitude, x$data$z,
        contour(xx, yy, zz,
                levels=water.z, lwd=water.lwd, lty=water.lty, col=water.col,
                drawlabels=FALSE, add=TRUE, ...)
    }
    if (zr[2] > 0) {
        if (missing(land.z)) {
            if (zr[1] < 0) {
                land.z <- pretty(c(0, zr[2]))
                land.z <- land.z[land.z!=0]
            } else {
                land.z <- pretty(zr)
            }
        }
        nz <- length(land.z)
        if (nz > 0) {
            if (missing(land.col))
                land.col <- oce.colors.gebco(nz, "land", "line")
            if (missing(land.lty))
                land.lty <- rep(par("lty"), nz)
            else if (length(land.lty) == 1)
                land.lty <- rep(land.lty, nz)
            if (missing(land.lwd))
                land.lwd <- rep(par("lwd"), nz)
            else if (length(land.lwd) == 1)
                land.lwd <- rep(land.lwd, nz)
            legend <- c(legend, land.z)
            lwd    <- c(lwd,    land.lwd)
            lty    <- c(lty,    land.lty)
            col    <- c(col,    land.col)
            contour(xx, yy, zz,
                    levels=land.z, lwd=land.lwd, lty=land.lty, col=land.col,
                    drawlabels=FALSE, add=TRUE, ...)
        }
    }
    if (!is.null(legend.loc)) {
        o <- rev(order(legend))
        legend(legend.loc, lwd=lwd[o], lty=lty[o],
               bg="white", legend=legend[o], col=col[o])
    }
   oce.debug(debug, "\b\b} # plot.topo()\n")
}

read.topo <- function(file, log.action, ...)
{
    nh <- 6
    header <- readLines(file, n=nh)
    ncols <- as.numeric(strsplit(header[1],"[ ]+",perl=TRUE)[[1]][2])
    nrows <- as.numeric(strsplit(header[2],"[ ]+",perl=TRUE)[[1]][2])
    lon.ll <- as.numeric(strsplit(header[3],"[ ]+",perl=TRUE)[[1]][2])
    lat.ll <- as.numeric(strsplit(header[4],"[ ]+",perl=TRUE)[[1]][2])
    cellsize <- as.numeric(strsplit(header[5],"[ ]+",perl=TRUE)[[1]][2])
    zz <- as.matrix(read.table(file, header=FALSE, skip=nh),byrow=TRUE)
    longitude <- lon.ll + cellsize * seq(0, ncols-1)
    latitude <- lat.ll + cellsize * seq(0, nrows-1)
    z <- t(zz[dim(zz)[1]:1,])
    if (missing(log.action)) log.action <- paste(deparse(match.call()), sep="", collapse="")
    log.item <- processing.log.item(log.action)
    as.topo(longitude, latitude, z, filename=file, log.action=log.item)
}

as.topo <- function(longitude, latitude, z, filename="", log.action)
{
    ncols <- length(longitude)
    nrows <- length(latitude)
    lon.ll <- min(longitude, na.rm=TRUE)
    lat.ll <- min(latitude, na.rm=TRUE)
    dim <- dim(z)
    if (dim[1] != ncols) stop("longitude vector has length ", ncols, ", which does not match matrix width ", dim[1])
    if (dim[2] != nrows) stop("latitude vector has length ", ncols, ", which does not match matrix height ", dim[2])
    data <- list(longitude=longitude, latitude=latitude, z=z)
    metadata <- list(filename=file, ncols=ncols, nrows=nrows, lon.ll=lon.ll, lat.ll=lat.ll)
    if (missing(log.action)) log.action <- paste(deparse(match.call()), sep="", collapse="")
    log.item <- processing.log.item(log.action)
    rval <- list(data=data, metadata=metadata, processing.log=log.item)
    class(rval) <- c("topo", "oce")
    rval
}

summary.topo <- function(object, ...)
{
    if (!inherits(object, "topo")) stop("method is only for topo objects")
    res <- list(lat.range=range(object$data$lat, na.rm=TRUE),
                lon.range=range(object$data$lon, na.rm=TRUE),
                z.range=range(object$data$z, na.rm=TRUE),
                processing.log=processing.log.summary(object))
    class(res) <- "summary.topo"
    res
}

print.summary.topo <- function(x, digits=max(6, getOption("digits") - 1), ...)
{
    cat("\nETOPO dataset\n")
    cat("latitude range:", format(x$lat.range[1], digits),
        " to ", format(x$lat.range[2], digits), "\n")
    cat("longitude range:", format(x$lon.range[1], digits),
        " to ", format(x$lon.range[2], digits), "\n")
    cat("elevation range:", format(x$z.range[1], digits=digits),
        " to ", format(x$z.range[2], digits), "\n")
    cat("Processing Log:\n", ...)
    cat(x$processing.log, ...)
    invisible(x)
}
