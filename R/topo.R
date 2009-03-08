plot.topo <- function(x,
                      water.z, water.col, water.lty, water.lwd,
                      land.z,   land.col,  land.lty,  land.lwd,
                      legend.loc="topright",
                      ...)
{
    if (!inherits(x, "topo")) stop("method is only for topo objects")
    lat.range <- range(x$data$lat, na.rm=TRUE)
    asp <- 1 / cos(mean(lat.range)*pi/180)
    zr <- range(x$data$z, na.rm=TRUE)

    ## auto-scale based on data in window, if window provided
    args <- list(...)
    if (!is.null(args$xlim) && !is.null(args$ylim)) {
        xf <- (args$xlim[1] <= x$data$lon) & (x$data$lon <= args$xlim[2])
        yf <- (args$ylim[1] <= x$data$lat) & (x$data$lat <= args$ylim[2])
        zr <- range(x$data$z[xf, yf], na.rm=TRUE)
    } else {
        zr <- range(x$data$z, na.rm=TRUE)
    }

    plot(range(x$data$lon, na.rm=TRUE), range(x$data$lat, na.rm=TRUE),
         asp=asp, xaxs="i", yaxs="i", type="n", xlab="", ylab="", ...)

    contour(x$data$lon, x$data$lat, x$data$z,
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
            water.col <- gebco.colors(nz, "water", "line")
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
        contour(x$data$lon, x$data$lat, x$data$z,
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
        if (missing(land.col))
            land.col <- gebco.colors(nz, "land", "line")
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
        contour(x$data$lon, x$data$lat, x$data$z,
                levels=land.z, lwd=land.lwd, lty=land.lty, col=land.col,
                drawlabels=FALSE, add=TRUE, ...)
    }
    if (!is.null(legend.loc)) {
        o <- rev(order(legend))
        legend(legend.loc, lwd=lwd[o], lty=lty[o],
               bg="white", legend=legend[o], col=col[o])
    }
}
read.topo <- function(file, log.action)
{
    nh <- 6
    header <- readLines(file, n=nh)
    ncols <- as.numeric(strsplit(header[1],"[ ]+",perl=TRUE)[[1]][2])
    nrows <- as.numeric(strsplit(header[2],"[ ]+",perl=TRUE)[[1]][2])
    lon.ll <- as.numeric(strsplit(header[3],"[ ]+",perl=TRUE)[[1]][2])
    lat.ll <- as.numeric(strsplit(header[4],"[ ]+",perl=TRUE)[[1]][2])
    cellsize <- as.numeric(strsplit(header[5],"[ ]+",perl=TRUE)[[1]][2])
    zz <- as.matrix(read.table(file, header=FALSE, skip=nh),byrow=TRUE)
    z <- t(zz[dim(zz)[1]:1,])
    lon <- lon.ll + cellsize * seq(0, ncols-1)
    lat <- lat.ll + cellsize * seq(0, nrows-1)
    data <- list(lon=lon, lat=lat, z=z)
    metadata <- list(filename=file, cellsize=cellsize, ncols=ncols, nrows=nrows, lon.ll=lon.ll, lat.ll=lat.ll)
    if (missing(log.action)) log.action <- paste(deparse(match.call()), sep="", collapse="")
    log.item <- processing.log.item(log.action)
    res <- list(data=data, metadata=metadata, processing.log=log.item)
    class(res) <- c("topo", "oce")
    res
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
    print(x$processing.log)
    invisible(x)
}
