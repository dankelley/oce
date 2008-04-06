plot.etopo2 <- function(x, legend.loc="topright", ...)
{
    if (!inherits(x, "etopo2")) stop("method is only for etop2 objects")
    zr <- range(x$data$z)
    contour(x$data$lon, x$data$lat, x$data$z, levels=0, drawlabels=FALSE,
            asp=1/cos(mean(x$data$lat*pi/180)))
    w <- c()
    wc <- c()
    if (zr[1] < 0) {
        w <- pretty(c(zr[1], 0))
        w <- rev(w[1:(length(w)-1)])
        wc <- gebco.colors(length(w), "water", "line")
        contour(x$data$lon, x$data$lat, x$data$z, levels=w,
                col=wc,
                drawlabels=FALSE,
                add=TRUE)
    }
    l <- c()
    lc <- c()
    if (zr[2] > 0) {
        l <- pretty(c(0, zr[2]))
        lc <- gebco.colors(length(w), "land", "line")
        contour(x$data$lon, x$data$lat, x$data$z, levels=l,
                col=lc,
                drawlabels=FALSE,
                add=TRUE)
    }
    if (!is.null(legend.loc)) {
        nl <- length(w) + length(l)
        legend(legend.loc, lwd=par("lwd"),legend=c(rev(l), w), col=c(rev(lc), wc))
    }
}
