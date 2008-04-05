plot.etopo2 <- function(x, ...)
{
    if (!inherits(x, "etopo2")) stop("method is only for etop2 objects")
    contour(x$data$lon, x$data$lat, x$data$z, levels=0, drawlabels=FALSE,
            asp=1/cos(mean(x$data$lat*pi/180)))
}
