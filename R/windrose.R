as.windrose <- function(x, y, dtheta = 15)
{
    dt <- dtheta * pi / 180
    dt2 <- dt / 2
    R <- sqrt(x^2 + y^2)
    angle <- atan2(y, x)
    L <- max(R, na.rm=TRUE)
    nt <- 2 * pi / dt
    theta <- count <- mean <- vector("numeric", nt)
    fives <- matrix(0, nt, 5)
    for (i in 1:nt) {
        theta[i] <- i * dt
        if (theta[i] <= pi)
            inside <- (angle < (theta[i] + dt2)) & ((theta[i] - dt2) <= angle)
        else {
            inside <- ((2*pi+angle) < (theta[i] + dt2)) & ((theta[i] - dt2) <= (2*pi+angle))
        }
        count[i] <- sum(inside)
        mean[i] <- mean(R[inside], na.rm=TRUE)
        fives[i,] <- fivenum(R[inside])
    }
    data <- list(n=length(x), x.mean=mean(x, na.rm=TRUE), y.mean=mean(y, na.rm=TRUE), theta=theta*180/pi,
                 count=count, mean=mean, fives=fives)
    metadata <- list(dtheta=dtheta)
    log <- historyItem(paste(deparse(match.call()), sep="", collapse=""))
    res <- list(data=data, metadata=metadata, history=log)
    class(res) <- c("windrose", "oce")
    res
}

plot.windrose <- function(x,
                          type=c("count","mean", "median", "fivenum"),
                          mgp=getOption("oceMgp"),
                          mar=c(mgp[1], mgp[1], 1+mgp[1], mgp[1]),
                          col,
                          ...)
{
    if (!inherits(x, "windrose"))
        stop("method is only for wind-rose objects")
    type <- match.arg(type)
    nt <- length(x$data$theta)
    t <- x$data$theta * pi / 180        # in radians
    dt <- t[2] - t[1]
    dt2 <- dt / 2
                                        # Plot setup
    par(mgp=mgp, mar=mar)
    plot.new()
    pin <- par("pin")
    xlim <- ylim <- c(-1, 1)
    if (pin[1] > pin[2])
        xlim <- (pin[1]/pin[2]) * xlim
    else ylim <- (pin[2]/pin[1]) * ylim
    plot.window(xlim, ylim, "", asp = 1)
    if (missing(col))
        col <- c("red", "pink", "blue", "darkgray")
    else {
        if (length(col) != 4)
            stop("'col' should be a list of 4 colours")
    }
                                        # Draw circle and radii
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
        max <- max(x$data$fives[,5], na.rm=TRUE)
        for (i in 1:nt) {
            r <- x$data$fives[i,3] / max
            ##cat("t=", t[i], " r=", r, "\n")
            xlist <- c(0, r * cos(t[i] - dt2), r * cos(t[i] + dt2), 0)
            ylist <- c(0, r * sin(t[i] - dt2), r * sin(t[i] + dt2), 0)
            polygon(xlist, ylist,col=col[1], border=col[3])
        }
        title(paste("Medians (max ", sprintf(max,fmt="%.3g"), ")", sep=""))
    } else if (type == "fivenum") {
        max <- max(x$data$fives[,3], na.rm=TRUE)
        for (i in 1:nt) {
            for (j in 2:3) {
                tm <- t[i] - dt2
                tp <- t[i] + dt2
                r0 <- x$data$fives[i, j-1] / max
                r  <- x$data$fives[i, j  ] / max
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

summary.windrose <- function(object, ...)
{
    if (!inherits(object, "windrose"))
        stop("method is only for windrose objects")
    n <- length(object$data$theta)
    threes <- matrix(nrow=n, ncol=3)
    res <- list(n=n,
                dtheta=object$metadata$dtheta,
                threes=threes,
                history=object$history)
    for (i in 1:n) {
        threes[i,] <- c(object$data$fivenum[i,1], object$data$mean[i], object$data$fivenum[i, 5])
    }
    colnames(threes) <- c("Min.", "Mean", "Max.")
    rownames(threes) <- object$data$theta
    res$threes <- threes
    class(res) <- "summary.windrose"
    res
}

print.summary.windrose <- function(x, digits=max(6, getOption("digits") - 1), ...)
{
    cat("Windrose data\n-------------\n\n")
    cat("* Have n=", x$n, "angles, separated by dtheta=", x$dtheta,"\n\n")
    cat("* Statistics by angle::\n\n", ...)
    cat(showThrees(x, indent='     '), ...)
    print(summary(x$history))
    invisible(x)
}
