plot.coastline <- function (x, asp=NA, ...)
{
    debug <- FALSE
    if (!inherits(x, "coastline"))
        stop("method is only for coastline objects")
    asp.middle <- 1 / cos(mean(range(x$data$latitude,na.rm=TRUE)) * pi / 180) # dy/dx
    if (debug) cat("asp.middle=", asp.middle, "\n")
    if (is.na(asp))
        asp <- asp.middle
    ## The following is a somewhat provisional hack, necessiated by the tendency
    ## of plot() to produce latitudes past the poles.
    ## BUG: the use of par("pin") seems to mess up resizing in aqua windows.
    xr <- range(x$data$longitude, na.rm=TRUE)
    yr <- range(x$data$latitude, na.rm=TRUE)
    asp.page <- par("pin")[2] / par("pin")[1] # dy / dx
    if (debug) cat("asp.page=", asp.page, "\n")
    gamma <- asp.middle / asp.page
    if (debug) cat("asp.middle/asp.page=", asp.middle / asp.page, "\n")
    if ((asp.middle / asp.page) < 1) {
        if (debug) cat("type 1\n")
        xr[2] <- xr[1] + (xr[2] - xr[1]) * (asp.middle / asp.page)
    } else {
        if (debug) cat("type 2\n")
        yr[2] <- yr[1] + (yr[2] - yr[1]) / (asp.middle / asp.page)
    }
    plot(xr, yr, asp=asp, xlab="", ylab="", type="n", xaxs="i", yaxs="i", ...)
#    par(new=TRUE)
    if (debug) points(xr, yr, col="blue", pch=20, cex=3)
    if (debug)     abline(v=xr, col="red")
    yaxp <- par("yaxp")
    if (debug) cat("par(pin)",par("pin"),"\n")
    if (yaxp[1] < -90 | yaxp[2] > 90) {
        opin <- par("pin")
        if (debug) cat("inside pin=", par("pin"), " yaxp=",yaxp,"\n")
        yscale <- 180 / (yaxp[2] - yaxp[1])
        if (debug) cat("yscale",yscale," new opin[2]", yscale*opin[2],"\n")
        par(pin=c(opin[1], yscale*opin[2]))
    	lines(x$data$longitude, x$data$latitude, asp=asp, yaxp=c(-90,90,6), yaxs="i", xlab="", ylab="", type="l", ...)
        par("pin"=opin)
    } else {
    	lines(x$data$longitude, x$data$latitude, asp=asp, xlab="", ylab="", ...)
    }
    if (debug) {
        cat("par(pin)",par("pin"),"\n")
        cat("lon lim:");print(range(x$data$longitude,na.rm=TRUE))
        cat("lat lim:");print(range(x$data$latitude,na.rm=TRUE))
        cat("par:");print(par())
    }
}
