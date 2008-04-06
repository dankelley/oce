plot.etopo2 <- function(x, legend.loc="topright", ...)
{
    if (!inherits(x, "etopo2")) stop("method is only for etop2 objects")
    zr <- range(x$data$z)
    contour(x$data$lon, x$data$lat, x$data$z, levels=0, drawlabels=FALSE,
            col="black",
            asp=1/cos(mean(x$data$lat*pi/180)))
    w <- c()
    wc <- c()
    if (zr[1] < 0) {
        w <- pretty(c(zr[1], 0))
        w <- w[w!=0]
        w <- rev(w)
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
        l <- l[l!=0]
        lc <- gebco.colors(length(l), "land", "line")
        contour(x$data$lon, x$data$lat, x$data$z, levels=l,
                col=lc,
                drawlabels=FALSE,
                add=TRUE)
    }
    if (!is.null(legend.loc)) {
        nl <- length(w) + length(l)
        print(l)
        print(c(rev(l),0))
        print(w)
        legend(legend.loc, lwd=par("lwd"),
               legend=c(rev(l),0, w),
               col=c(rev(lc), "black", wc))
    }
}
