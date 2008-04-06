plot.etopo2 <- function(x, water.z, water.colors, land.z, land.colors, legend.loc="topright", ...)
{
    if (!inherits(x, "etopo2")) stop("method is only for etop2 objects")
    zr <- range(x$data$z)
    lat.range <- range(x$data$lat)
    contour(x$data$lon, x$data$lat, x$data$z, levels=0, drawlabels=FALSE,
            col="black",
            asp=1/cos(mean(lat.range)*pi/180), ...)
    w <- c()
    wc <- c()
    if (zr[1] < 0) {
        if (missing(water.z)) {
            if (zr[2] > 0) {
                w <- pretty(c(zr[1], 0))
                w <- w[w!=0]
            } else {
                w <- pretty(zr)
            }
            w <- rev(w)
        } else {
            w <- water.z
        }
        if (missing(water.colors))
            wc <- gebco.colors(length(w), "water", "line")
        else
            wc <- water.colors
        contour(x$data$lon, x$data$lat, x$data$z, levels=w,
                col=wc,
                drawlabels=FALSE,
                add=TRUE, ...)
    }
    l <- c()
    lc <- c()
    if (zr[2] > 0) {
        if (missing(land.z)) {
            if (zr[1] < 0) {
                l <- pretty(c(0, zr[2]))
                l <- l[l!=0]
            } else {
                l <- pretty(zr)
            }
        } else {
            l <- land.z
        }
        if (missing(land.colors))
            lc <- gebco.colors(length(l), "land", "line")
        else
            lc <- land.colors
        contour(x$data$lon, x$data$lat, x$data$z, levels=l,
                col=lc,
                drawlabels=FALSE,
                add=TRUE, ...)
    }
    if (!is.null(legend.loc)) {
        nl <- length(w) + length(l)
        legend(legend.loc, lwd=par("lwd"),
               legend=c(rev(l),0, w),
               col=c(rev(lc), "black", wc), ...)
    }
}
