plot.topo <- function(x,
                      water.z, water.col, water.lty, water.lwd,
                      land.z,  land.col,   land.lty,  land.lwd,
                      legend.loc="topright",
                      ...)
{
    if (!inherits(x, "topo")) stop("method is only for topo objects")
    lat.range <- range(x$data$lat)
    asp <- 1 / cos(mean(lat.range)*pi/180)
    zr <- range(x$data$z)

    plot(range(x$data$lon), range(x$data$lat), asp=asp, xaxs="i", yaxs="i",
         type="n", xlab="", ylab="", ...)

    contour(x$data$lon, x$data$lat, x$data$z,
            levels=0, drawlabels=FALSE, add=TRUE,
            col="black")                # coastline is always black
    if (missing(water.lty)) {
        water.lty <- par("lty")
    }
    if (missing(water.lwd)) {
        water.lwd <- par("lwd")
    }
    if (missing(land.lty)) {
        land.lty <- par("lty")
    }
    if (missing(land.lwd)) {
        land.lwd <- par("lwd")
    }
    if (zr[1] < 0) {
        if (missing(water.z)) {
            if (zr[2] > 0) {
                water.z <- pretty(c(zr[1], 0))
                water.z <- water.z[water.z!=0]
            } else {
                water.z <- pretty(zr)
            }
            water.z <- rev(sort(water.z))
        }
        if (missing(water.col))
            water.col <- gebco.colors(length(water.z), "water", "line")
        contour(x$data$lon, x$data$lat, x$data$z,
                levels=water.z, lwd=water.lwd, lty=water.lty, col=water.col,
                drawlabels=FALSE,
                add=TRUE, ...)
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
        if (missing(land.col))
            land.col <- gebco.colors(length(land.z), "land", "line")
        contour(x$data$lon, x$data$lat, x$data$z,
                levels=land.z, lwd=land.lwd, lty=land.lty, col=land.col,
                drawlabels=FALSE,
                add=TRUE, ...)
    }
    if (!is.null(legend.loc)) {
        nl <- length(land.z) + length(water.z)
        legend(legend.loc,
               lwd=c(land.lwd, water.lwd),
               lty=c(land.lty, water.lty),
               bg="white",
               legend=c(rev(land.z),0, water.z),
               col=c(land.col, "black", water.col))
    }
}
