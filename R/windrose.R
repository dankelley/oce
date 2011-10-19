setMethod(f="initialize",
          signature="windrose",
          definition=function(.Object) {
              .Object@processingLog$time=c(.Object@processingLog$time, Sys.time())
              .Object@processingLog$value=c(.Object@processingLog$value, "create 'windrose' object")
              return(.Object)
          })

setMethod(f="[[",
          signature="windrose",
          definition=function(x, i, j, drop) {
              ## 'j' can be for times, as in OCE
              ##if (!missing(j)) cat("j=", j, "*****\n")
              i <- match.arg(i, c("theta", "count", "fives"))
              if (i == "theta") return(x@data$theta)
              else if (i == "count") return(x@data$count)
              else if (i == "fives") return(x@data$fives)
              else stop("cannot access \"", i, "\"") # cannot get here
          })

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
    res <- new('windrose')
    res@data <- list(n=length(x), x.mean=mean(x, na.rm=TRUE), y.mean=mean(y, na.rm=TRUE), theta=theta*180/pi,
                     count=count, mean=mean, fives=fives)
    res@metadata <- list(dtheta=dtheta)
    res@processingLog <- unclass(processingLog(res@processingLog, paste(deparse(match.call()), sep="", collapse="")))
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
    nt <- length(x@data$theta)
    t <- x@data$theta * pi / 180        # in radians
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
        max <- max(x@data$count, na.rm=TRUE)
        for (i in 1:nt) {
            r <- x@data$count[i] / max
            ##cat("t=", t[i], " r=", r, "\n")
            xlist <- c(0, r * cos(t[i] - dt2), r * cos(t[i] + dt2), 0)
            ylist <- c(0, r * sin(t[i] - dt2), r * sin(t[i] + dt2), 0)
            polygon(xlist, ylist,col=col[1], border=col[3])
        }
        title(paste("Counts (max ", max, ")", sep=""))
    } else if (type == "mean") {
        max <- max(x@data$mean, na.rm=TRUE)
        for (i in 1:nt) {
            r <- x@data$mean[i] / max
            ##cat("t=", t[i], " r=", r, "\n")
            xlist <- c(0, r * cos(t[i] - dt2), r * cos(t[i] + dt2), 0)
            ylist <- c(0, r * sin(t[i] - dt2), r * sin(t[i] + dt2), 0)
            polygon(xlist, ylist,col=col[1], border=col[3])
        }
        title(paste("Means (max ", sprintf(max, fmt="%.3g"), ")", sep=""))
    } else if (type == "median") {
        max <- max(x@data$fives[,5], na.rm=TRUE)
        for (i in 1:nt) {
            r <- x@data$fives[i,3] / max
            ##cat("t=", t[i], " r=", r, "\n")
            xlist <- c(0, r * cos(t[i] - dt2), r * cos(t[i] + dt2), 0)
            ylist <- c(0, r * sin(t[i] - dt2), r * sin(t[i] + dt2), 0)
            polygon(xlist, ylist,col=col[1], border=col[3])
        }
        title(paste("Medians (max ", sprintf(max,fmt="%.3g"), ")", sep=""))
    } else if (type == "fivenum") {
        max <- max(x@data$fives[,3], na.rm=TRUE)
        for (i in 1:nt) {
            for (j in 2:3) {
                tm <- t[i] - dt2
                tp <- t[i] + dt2
                r0 <- x@data$fives[i, j-1] / max
                r  <- x@data$fives[i, j  ] / max
                xlist <- c(r0 * cos(tm), r * cos(tm), r * cos(tp), r0 * cos(tp))
                ylist <- c(r0 * sin(tm), r * sin(tm), r * sin(tp), r0 * sin(tp))
                thiscol <- col[c(2,1,1,2)][j-1]
                polygon(xlist, ylist, col=thiscol, border=col[4])
            }
            r <- x@data$fivenum[i, 3] / max
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
    cat("Windrose data\n-------------\n\n")
    n <- length(object@data$theta)
    dtheta <- abs(diff(object@data$theta[1:2]))
    cat("* Have n=", n, "angles, separated by dtheta=", dtheta,"\n\n")
    ##cat("* Statistics by angle::\n\n", ...)
    ##threes <- matrix(nrow=2, ncol=3)
    ##threes[1,] <- threenum(object@data$theta)
    ##threes[2,] <- threenum(object@data$count)
    ##colnames(threes) <- c("Min.", "Mean", "Max.")
    ##rownames(threes) <- c("theta", "count")
    ##print(threes)
    ##cat('\n')
    processingLogShow(object)
}

