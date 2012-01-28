setMethod(f="initialize",
          signature="topo",
          definition=function(.Object,longitude,latitude,z,filename="") {
              if (!missing(longitude)) .Object@data$longitude <- longitude
              if (!missing(latitude)) .Object@data$latitude <- latitude
              if (!missing(z)) .Object@data$z <- z
              .Object@metadata$filename <- filename
              .Object@processingLog$time <- as.POSIXct(Sys.time())
              .Object@processingLog$value <- "create 'topo' object"
              return(.Object)
          })

setMethod(f="[[",
          signature="topo",
          definition=function(x, i, j, drop) {
              ## 'j' can be for times, as in OCE
              ##if (!missing(j)) cat("j=", j, "*****\n")
              i <- match.arg(i, c("longitude","latitude","z", "filename"))
              if (i == "longitude") return(x@data$longitude)
              else if (i == "latitude") return(x@data$latitude)
              else if (i == "z") return(x@data$z)
              else if (i == "filename") return(x@metadata$filename)
              else stop("cannot access \"", i, "\"") # cannot get here
          })


topoInterpolate <- function(latitude, longitude, topo)
{
    if (missing(latitude))
        stop("must supply latitude")
    if (missing(longitude))
        stop("must supply longitude")
    if (missing(topo))
        stop("must supply topo")
    if (length(latitude) != length(longitude))
        stop("lengths of latitude and longitude must match")
    .Call("topo_interpolate", latitude, longitude, topo[["latitude"]], topo[["longitude"]], topo[["z"]])
}


plot.topo <- function(x,
                      xlab="", ylab="",
                      asp,
                      clatitude, clongitude, span,
                      ##center, span,
                      expand=1.5,
                      water.z,
                      col.water,
                      lty.water,
                      lwd.water,
                      land.z,
                      col.land,
                      lty.land,
                      lwd.land,
                      location="topright",
                      mgp=getOption("oceMgp"),
                      mar=c(mgp[1]+1,mgp[1]+1,1,1),
                      debug=getOption("oceDebug"),
                      ...)
{
    if (!inherits(x, "topo"))
        stop("method is only for topo objects")
    oceDebug(debug, "\b\bplot.topo() {\n")

    opar <- par(no.readonly = TRUE)
    ##on.exit(par(opar))
    par(mgp=mgp, mar=mar)
    dots <- list(...)
    dotsNames <- names(dots)
    if ("center" %in% dotsNames) stop("please use 'clatitude' and 'clongitude' instead of 'center'")
    gave.center <- !missing(clatitude) && !missing(clongitude)

    gave.span <- !missing(span)
    if (gave.center != gave.span) stop("must give all of 'clatitude', 'clongitude' and 'span', or none of them")
    if (gave.center) {
        if (!missing(asp))
            warning("argument 'asp' being ignored, because argument 'center' was given")
        asp <- 1 / cos(clatitude * atan2(1, 1) / 45) #  ignore any provided asp, because lat from center over-rides it
        xr <- clongitude + span * c(-1/2, 1/2) / 111.11 / asp
        yr <- clatitude  + span * c(-1/2, 1/2) / 111.11
        oceDebug(debug, "gave center; calculated xr=", xr," yr=", yr, " asp=", asp, "\n")
    } else {
        if (missing(asp)) {
            if ("ylim" %in% dotsNames)
                asp <- 1 / cos(mean(range(dots$ylim, na.rm=TRUE)) * pi / 180) # dy/dx
            else
                asp <- 1 / cos(mean(range(x[["latitude"]],na.rm=TRUE)) * pi / 180) # dy/dx
        }
        ## Expand
        xr0 <- range(x[["longitude"]], na.rm=TRUE)
        yr0 <- range(x[["latitude"]], na.rm=TRUE)
        oceDebug(debug, "xr0=", xr0, "\n")
        oceDebug(debug, "yr0=", yr0, "\n")
        if (expand >= 0 && max(abs(xr0)) < 100 && max(abs(yr0) < 70)) { # don't expand if full map
            xr <- mean(xr0) + expand * diff(xr0) * c(-1/2, 1/2)
            yr <- mean(yr0) + expand * diff(yr0) * c(-1/2, 1/2)
        } else {
            xr <- xr0
            yr <- yr0
        }
    }
    zr <- range(x[["z"]], na.rm=TRUE)
    if (gave.center && !is.null(dots$xlim))
        stop("cannot give 'xlim' argument if the 'center' argument was given")
    if (gave.center && !is.null(dots$ylim))
        stop("cannot give 'ylim' argument if the 'center' argument was given")
    ## auto-scale based on data in window, if window provided
    if (!is.null(dots$xlim) && !is.null(dots$ylim)) {
        xr <- dots$xlim
        yr <- dots$ylim
    }

    ## The following is a somewhat provisional hack, to get around a
    ## tendency of plot() to produce latitudes past the poles.
    ## BUG: the use of par("pin") seems to mess up resizing in aqua windows.
    asp.page <- par("pin")[2] / par("pin")[1] # dy / dx
    oceDebug(debug, "par('pin')=",par('pin'), "asp=",asp,"asp.page=", asp.page, "\n")
    if (asp > asp.page) {               # FIXME: this seems to have x and y mixed up (asp=dy/dx)
        oceDebug(debug, "type 1 (will narrow x range)\n")
        d <- asp / asp.page * diff(xr)
        xr <- mean(xr) + d * c(-1/2, 1/2)
        oceDebug(debug, "xr narrowed to:", xr, "\n")
        ## xr[2] <- xr[1] + (xr[2] - xr[1]) * (asp / asp.page)
    } else {
        oceDebug(debug, "type 2 (will narrow y range)\n")
        d <- asp / asp.page * diff(yr)
        yr <- mean(yr) + d * c(-1/2, 1/2)
        oceDebug(debug, "yr narrowed to:", yr, "\n")
        ##yr[2] <- yr[1] + (yr[2] - yr[1]) / (asp / asp.page)
    }

    oceDebug(debug, "xr:", xr, "(before trimming)\n")
    oceDebug(debug, "yr:", yr, "(before trimming)\n")
#    if (xr[1] < -180) xr[1] <- -180
#    if (xr[2] >  180) xr[2] <- 180
    if (yr[1] < -90)  yr[1] <- -90
    if (yr[2] >  90)  yr[2] <-  90
    oceDebug(debug, "xr:", xr, "(after trimming)\n")
    oceDebug(debug, "yr:", yr, "(after trimming)\n")

    ## Data may not extend across plot region
    lon.range <- range(x[["longitude"]], na.rm=TRUE)
    lat.range <- range(x[["latitude"]], na.rm=TRUE)
    if (xr[1] < lon.range[1]) xr[1] <- lon.range[1]
    if (xr[2] > lon.range[2]) xr[2] <- lon.range[2]
    if (yr[1] < lat.range[1]) yr[1] <- lat.range[1]
    if (yr[2] > lat.range[2]) yr[2] <- lat.range[2]

    plot(xr, yr, asp=asp, xlab=xlab, ylab=ylab, type="n", xaxs="i", yaxs="i", axes=FALSE, ...)
    if (debug > 0)
        points(xr, yr, col="blue", pch=20, cex=3)

    xr.pretty <- pretty(xr)
    yr.pretty <- pretty(yr)
    oceDebug(debug, "xr.pretty=", xr.pretty, "(before trimming)\n")
    xr.pretty <- subset(xr.pretty, xr.pretty >= xr[1] & xr.pretty <= xr[2])
    oceDebug(debug, "xr.pretty=", xr.pretty, "(after trimming)\n")
    oceDebug(debug, "yr.pretty=", yr.pretty, "(before trimming)\n")
    yr.pretty <- subset(yr.pretty, yr.pretty >= yr[1] & yr.pretty <= yr[2])
    oceDebug(debug, "yr.pretty=", yr.pretty, "(after trimming)\n")

    lines(c(xr[1], xr[2], xr[2], xr[1], xr[1]), c(yr[1], yr[1], yr[2], yr[2], yr[1])) # axis box
    axis(1, at=xr.pretty, pos=yr[1])
    axis(3, at=xr.pretty, pos=max(yr), labels=FALSE)
    axis(2, at=yr.pretty, pos=xr[1])
    axis(4, at=yr.pretty, pos=max(xr), labels=FALSE)

    oceDebug(debug, "xr=", xr, "yr=",yr,"\n")
    yaxp <- par("yaxp")
    oceDebug(debug, "par(yaxp)",par("yaxp"),"\n")
    oceDebug(debug, "par(pin)",par("pin"),"\n")

    ## need to clip because contour() does not do so
    xx <- x[["longitude"]]
    yy <- x[["latitude"]]
    xclip <- xx < xr[1] | xr[2] < xx
    yclip <- yy < yr[1] | yr[2] < yy
    xx <- xx[!xclip]
    if (length(xx) < 1)
        stop("there are no topographic data within the longitudes of the plot region.")
    yy <- yy[!yclip]
    if (length(yy) < 1)
        stop("there are no topographic data within the latitudes of the plot region.")
    zz <- x[["z"]][!xclip, !yclip]
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
        if (missing(col.water))
            col.water <- oceColorsGebco(nz, "water", "line")
        if (missing(lty.water))
            lty.water <- rep(par("lty"), nz)
        else if (length(lty.water) == 1)
            lty.water <- rep(lty.water, nz)
        if (missing(lwd.water))
            lwd.water <- rep(par("lwd"), nz)
        else if (length(lwd.water) == 1)
            lwd.water <- rep(lwd.water, nz)
        legend <- c(legend, water.z)
        lwd    <- c(lwd,    lwd.water)
        lty    <- c(lty,    lty.water)
        col    <- c(col,    col.water)
        contour(xx, yy, zz,
                levels=water.z, lwd=lwd.water, lty=lty.water, col=col.water,
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
            if (missing(col.land))
                col.land <- oceColorsGebco(nz, "land", "line")
            if (missing(lty.land))
                lty.land <- rep(par("lty"), nz)
            else if (length(lty.land) == 1)
                lty.land <- rep(lty.land, nz)
            if (missing(lwd.land))
                lwd.land <- rep(par("lwd"), nz)
            else if (length(lwd.land) == 1)
                lwd.land <- rep(lwd.land, nz)
            legend <- c(legend, land.z)
            lwd    <- c(lwd,    lwd.land)
            lty    <- c(lty,    lty.land)
            col    <- c(col,    col.land)
            contour(xx, yy, zz,
                    levels=land.z, lwd=lwd.land, lty=lty.land, col=col.land,
                    drawlabels=FALSE, add=TRUE, ...)
        }
    }
    if (!is.null(location)) {
        o <- rev(order(legend))
        legend(location, lwd=lwd[o], lty=lty[o],
               bg="white", legend=legend[o], col=col[o])
    }
    oceDebug(debug, "\b\b} # plot.topo()\n")
    invisible()
}

read.topo <- function(file, processingLog, ...)
{
    nh <- 6
    header <- readLines(file, n=nh)
    ncol <- as.numeric(strsplit(header[1],"[ ]+",perl=TRUE)[[1]][2])
    nrow <- as.numeric(strsplit(header[2],"[ ]+",perl=TRUE)[[1]][2])
    longitudeLowerLeft <- as.numeric(strsplit(header[3],"[ ]+",perl=TRUE)[[1]][2])
    latitudeLowerLeft <- as.numeric(strsplit(header[4],"[ ]+",perl=TRUE)[[1]][2])
    cellSize <- as.numeric(strsplit(header[5],"[ ]+",perl=TRUE)[[1]][2])
    zz <- as.matrix(read.table(file, header=FALSE, skip=nh), byrow=TRUE)
    rownames(zz) <- NULL
    colnames(zz) <- NULL
    longitude <- longitudeLowerLeft + cellSize * seq(0, ncol-1)
    latitude <- latitudeLowerLeft + cellSize * seq(0, nrow-1)
    z <- t(zz[dim(zz)[1]:1,])
    if (missing(processingLog))
        processingLog <- paste(deparse(match.call()), sep="", collapse="")
    hitem <- processingLogItem(processingLog)
    as.topo(longitude, latitude, z, filename=file, processingLog=hitem)
}

as.topo <- function(longitude, latitude, z, filename="", processingLog)
{
    ncols <- length(longitude)
    nrows <- length(latitude)
    longitudeLowerLeft <- min(longitude, na.rm=TRUE)
    latitudeLowerLeft <- min(latitude, na.rm=TRUE)
    dim <- dim(z)
    if (dim[1] != ncols)
        stop("longitude vector has length ", ncols, ", which does not match matrix width ", dim[1])
    if (dim[2] != nrows)
        stop("latitude vector has length ", ncols, ", which does not match matrix height ", dim[2])
    if (missing(processingLog))
        processingLog <- processingLogItem(paste(deparse(match.call()), sep="", collapse=""))
    rval <- new("topo", latitude=latitude, longitude=longitude, z=z, filename=filename)
    if (missing(processingLog))
        processingLog <- paste(deparse(match.call()), sep="", collapse="")
    rval@processingLog <- processingLog(rval@processingLog, processingLog)
    rval
}

summary.topo <- function(object, ...)
{
    if (!inherits(object, "topo"))
        stop("method is only for topo objects")
    digits <- 4
    latRange <- range(object[["latitude"]], na.rm=TRUE)
    lonRange <- range(object[["longitude"]], na.rm=TRUE)
    zRange <- range(object[["z"]], na.rm=TRUE)
    cat("\nTopo dataset\n------------\n")
    cat("* Source:          ", object[["filename"]], "\n")
    cat("* Latitude range:  ", format(latRange[1], digits),
        " to ", format(latRange[2], digits), "\n")
    cat("* Longitude range: ", format(lonRange[1], digits),
        " to ", format(lonRange[2], digits), "\n")
    cat("* Elevation range: ", format(zRange[1], digits=digits),
        " to ", format(zRange[2], digits), "\n")
    processingLogShow(object)
}

