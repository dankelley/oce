plot.coastline <- function (x, asp=NA, ...)
{
    debug <- FALSE
    if (!inherits(x, "coastline"))
        stop("method is only for coastline objects")
    if (is.na(asp))
        asp <- 1 / cos(mean(range(x$data$latitude,na.rm=TRUE))*pi/180)
    ## The following is a somewhat provisional hack, necessiated by the tendency
    ## of plot() to produce latitudes past the poles.
    ## BUG: the use of par("pin") seems to mess up resizing in aqua windows.
    xr <- range(x$data$longitude, na.rm=TRUE)
    yr <- range(x$data$latitude, na.rm=TRUE)
    plot(xr, yr, asp=asp, xlab="", ylab="", type="n", axes=FALSE, ...)
    par(new=TRUE)
    yaxp <- par("yaxp")
    if (debug) cat("par(pin)",par("pin"),"\n")
    if (yaxp[1] < -90 | yaxp[2] > 90) {
        opin <- par("pin")
        if (debug) cat("inside pin=", par("pin"), " yaxp=",yaxp,"\n")
        yscale <- 180 / (yaxp[2] - yaxp[1])
        if (debug) cat("yscale",yscale," new opin[2]", yscale*opin[2],"\n")
        par(pin=c(opin[1], yscale*opin[2]))
    	plot(x$data$longitude, x$data$latitude, asp=asp, yaxp=c(-90,90,6), yaxs="i", xlab="", ylab="", type="l", ...)
        par("pin"=opin)
    } else {
    	plot(x$data$longitude, x$data$latitude, asp=asp, xlab="", ylab="", type="l", ...)
    }
    if (debug) {
        cat("par(pin)",par("pin"),"\n")
        cat("lon lim:");print(range(x$data$longitude,na.rm=TRUE))
        cat("lat lim:");print(range(x$data$latitude,na.rm=TRUE))
        cat("par:");print(par())
    }
}
