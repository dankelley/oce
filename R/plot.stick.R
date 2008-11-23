plot.stick <- function(t, x, y, ...)
{
    ylim <- range(y, na.rm=TRUE)
    plot(range(t), ylim, type="n")
    tstart <- t[1]
    t.isPOSIXlt <- inherits(t, "POSIXlt")
    t.isPOSIXct <- inherits(t, "POSIXct")
    if (t.isPOSIXct) t <- unclass(t)
    if (t.isPOSIXlt) t <- unclass(as.POSIXct(t))
    usr <- par("usr")
    pin <- par("pin")
    tx.scale <- (usr[2]-usr[1]) / (usr[4]-usr[3]) * pin[2] / pin[1]
    n <- length(x)
    xx <- array(dim = 3 * n)
    yy <- array(dim = 3 * n)
    ones <- seq(1, 3*n, 3)
    twos <- seq(2, 3*n, 3)
    threes <- seq(3, 3*n, 3)
    xx[ones] <- t
    yy[ones] <- 0
    xx[twos] <- t + x * tx.scale
    yy[twos] <- y
    xx[threes] <- NA
    yy[threes] <- NA
    lines(xx, yy, type='l', ...)
}
