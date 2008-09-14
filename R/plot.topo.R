plot.topo <- function(x,
                      water.z, water.col, water.lty, water.lwd,
                      land.z,   land.col,  land.lty,  land.lwd,
                      legend.loc="topright",
                      ...)
{
    if (!inherits(x, "topo")) stop("method is only for topo objects")
    lat.range <- range(x$data$lat)
    asp <- 1 / cos(mean(lat.range)*pi/180)
    zr <- range(x$data$z)

    ## auto-scale based on data in window, if window provided
    args <- list(...)
    if (!is.null(args$xlim) && !is.null(args$ylim)) {
        xf <- (args$xlim[1] <= x$data$lon) & (x$data$lon <= args$xlim[2])
        yf <- (args$ylim[1] <= x$data$lat) & (x$data$lat <= args$ylim[2])
        zr <- range(x$data$z[xf, yf], na.rm=TRUE)
    } else {
        zr <- range(x$data$z, na.rm=TRUE)
    }

    plot(range(x$data$lon), range(x$data$lat), asp=asp, xaxs="i", yaxs="i",
         type="n", xlab="", ylab="", ...)

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
