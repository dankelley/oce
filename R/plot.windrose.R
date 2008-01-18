plot.windrose <- function(x, type=c("count","mean", "median", "fivenum"), col,  ...)
{
    if (!inherits(x, "windrose")) stop("method is only for wind-rose objects")
    type <- match.arg(type)
    nt <- length(x$data$theta)
    t <- x$data$theta * pi / 180            # in radians
    dt <- t[2] - t[1]
    dt2 <- dt / 2
    plot.new()
    pin <- par("pin")
    xlim <- ylim <- c(-1, 1)
    if (pin[1] > pin[2])
        xlim <- (pin[1]/pin[2]) * xlim
    else ylim <- (pin[2]/pin[1]) * ylim
    plot.window(xlim, ylim, "", asp = 1)
                                        # Draw circle and radii
    if (missing(col))
        col <- c("red", "pink", "blue", "darkgray")
    tt <- seq(0, 2*pi, length.out=100)
    px <- cos(tt)
    py <- sin(tt)
    lines(px, py, col=col[4])
    for (i in 1:nt) {
        lines(c(0, cos(t[i] - dt2)), c(0, sin(t[i] - dt2)), lwd=0.5, col=col[4])
    }
    text( 0, -1, "S", pos=1)
    text(-1,  0, "W", pos=2)
    text( 0,  1, "N", pos=3)
    text( 1,  0, "E", pos=4)
                                        # Draw rose in a given type
    if (type == "count") {
        max <- max(x$data$count, na.rm=TRUE)
        for (i in 1:nt) {
            r <- x$data$count[i] / max
            ##cat("t=", t[i], " r=", r, "\n")
            xlist <- c(0, r * cos(t[i] - dt2), r * cos(t[i] + dt2), 0)
            ylist <- c(0, r * sin(t[i] - dt2), r * sin(t[i] + dt2), 0)
            polygon(xlist, ylist,col=col[1], border=col[3])
        }
        title(paste("Counts (max ", max, ")", sep=""))
    } else if (type == "mean") {
        max <- max(x$data$mean, na.rm=TRUE)
        for (i in 1:nt) {
            r <- x$data$mean[i] / max
            ##cat("t=", t[i], " r=", r, "\n")
            xlist <- c(0, r * cos(t[i] - dt2), r * cos(t[i] + dt2), 0)
            ylist <- c(0, r * sin(t[i] - dt2), r * sin(t[i] + dt2), 0)
            polygon(xlist, ylist,col=col[1], border=col[3])
        }
        title(paste("Means (max ", sprintf(max, fmt="%.3g"), ")", sep=""))
    } else if (type == "median") {
        max <- max(x$data$fivenum[,3], na.rm=TRUE)
        for (i in 1:nt) {
            r <- x$data$fivenum[i,3] / max
            ##cat("t=", t[i], " r=", r, "\n")
            xlist <- c(0, r * cos(t[i] - dt2), r * cos(t[i] + dt2), 0)
            ylist <- c(0, r * sin(t[i] - dt2), r * sin(t[i] + dt2), 0)
            polygon(xlist, ylist,col=col[1], border=col[3])
        }
        title(paste("Medians (max ", sprintf(max,fmt="%.3g"), ")", sep=""))
    } else if (type == "fivenum") {
        max <- max(x$data$fivenum[,5], na.rm=TRUE)
        for (i in 1:nt) {
            for (j in 2:5) {
                tm <- t[i] - dt2
                tp <- t[i] + dt2
                r0 <- x$data$fivenum[i, j-1] / max
                r  <- x$data$fivenum[i, j  ] / max
                xlist <- c(r0 * cos(tm), r * cos(tm), r * cos(tp), r0 * cos(tp))
                ylist <- c(r0 * sin(tm), r * sin(tm), r * sin(tp), r0 * sin(tp))
                thiscol <- col[c(2,1,1,2)][j-1]
                polygon(xlist, ylist, col=thiscol, border=col[4])
            }
            r <- x$data$fivenum[i, 3] / max
            lines(c(r * cos(tm), r * cos(tp)), c(r * sin(tm), r * sin(tp)), col="blue", lwd=2)
        }
        title(paste("Fiveum (max ", sprintf(max,fmt="%.3g"), ")", sep=""))
    }
    invisible()
}
