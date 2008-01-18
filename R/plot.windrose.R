plot.windrose <- function(x, type=c("count","mean", "median", "fivenum"), ...)
{
    if (!inherits(x, "windrose")) stop("method is only for wind-rose objects")
    type <- match.arg(type)
    nt <- length(x$theta)
    t <- x$theta * pi / 180            # in radians
    dt <- t[2] - t[1]
    dt2 <- dt / 2
    count.max <- max(x$count, na.rm=TRUE)
    mean.max <- max(x$mean, na.rm=TRUE)
    median.max <- max(x$fivenum[,3], na.rm=TRUE)
    plot.new()
    pin <- par("pin")
    xlim <- ylim <- c(-1, 1)
    if (pin[1] > pin[2])
        xlim <- (pin[1]/pin[2]) * xlim
    else ylim <- (pin[2]/pin[1]) * ylim
    plot.window(xlim, ylim, "", asp = 1)
    if (type == "count") {
        max <- max(x$count, na.rm=TRUE)
        for (i in 1:nt) {
            r <- x$count[i] / max
            ##cat("t=", t[i], " r=", r, "\n")
            xlist <- c(0, r * cos(t[i] - dt2), r * cos(t[i] + dt2), 0)
            ylist <- c(0, r * sin(t[i] - dt2), r * sin(t[i] + dt2), 0)
            polygon(xlist, ylist,col="red")
        }
        title(paste("Counts (max ", max, ")", sep=""))
    } else if (type == "mean") {
        max <- max(x$mean, na.rm=TRUE)
        for (i in 1:nt) {
            r <- x$mean[i] / max
            ##cat("t=", t[i], " r=", r, "\n")
            xlist <- c(0, r * cos(t[i] - dt2), r * cos(t[i] + dt2), 0)
            ylist <- c(0, r * sin(t[i] - dt2), r * sin(t[i] + dt2), 0)
            polygon(xlist, ylist,col="red")
        }
        title(paste("Means (max ", sprintf(max, fmt="%.3g"), ")", sep=""))
    } else if (type == "median") {
        max <- max(x$fivenum[,3], na.rm=TRUE)
        for (i in 1:nt) {
            r <- x$fivenum[i,3] / max
            ##cat("t=", t[i], " r=", r, "\n")
            xlist <- c(0, r * cos(t[i] - dt2), r * cos(t[i] + dt2), 0)
            ylist <- c(0, r * sin(t[i] - dt2), r * sin(t[i] + dt2), 0)
            polygon(xlist, ylist,col="red")
        }
        title(paste("Medians (max ", sprintf(max,fmt="%.3g"), ")", sep=""))
    } else if (type == "fivenum") {
        max <- max(x$fivenum[,5], na.rm=TRUE)
        for (i in 1:nt) {
            r <- x$fivenum[i,5] / max   # outside end
            ##cat("t=", t[i], " r=", r, "\n")
            xlist <- c(0, r * cos(t[i] - dt2), r * cos(t[i] + dt2), 0)
            ylist <- c(0, r * sin(t[i] - dt2), r * sin(t[i] + dt2), 0)
            polygon(xlist, ylist,col="red")
            for (j in 1:4) {
                r <- x$fivenum[i, j] / max
                xlist <- c(r * cos(t[i] - dt2), r * cos(t[i] + dt2))
                ylist <- c(r * sin(t[i] - dt2), r * sin(t[i] + dt2))
                if (j==3)
                    lines(xlist, ylist, col="blue", lwd=2, lty=1)
                else
                    lines(xlist, ylist, col="blue", lwd=1, lty=3)
            }
        }
        title(paste("Fiveum (max ", sprintf(max,fmt="%.3g"), ")", sep=""))
    }
    tt <- seq(0, 2*pi, length.out=100)
    px <- cos(tt)
    py <- sin(tt)
    lines(px, py, col="pink")
}
