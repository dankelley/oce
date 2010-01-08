as.coastline <- function(latitude, longitude)
{
    n <- length(latitude)
    if (n != length(longitude)) stop("Lengths of longitude and latitude must be equal")
    data <- data.frame(longitude=longitude, latitude=latitude)
    log.item <- processing.log.item(paste(deparse(match.call()), sep="", collapse=""))
    res <- list(data=data, metadata=NULL, processing.log=log.item)
    class(res) <- c("coastline", "oce")
    res
}

plot.coastline <- function (x,
                            asp,
                            mgp=getOption("oce.mgp"),
                            mar=c(mgp[1], mgp[1], par("cex"), par("cex")),
                            bg,
                            axes=TRUE,
                            ...)
{
    if (!inherits(x, "coastline")) stop("method is only for coastline objects")
    par(mgp=mgp, mar=mar)
    dots <- list(...)
    debug <- FALSE
    if (missing(asp)) {
        if ("ylim" %in% names(dots))
            asp <- 1 / cos(mean(range(dots$ylim, na.rm=TRUE)) * pi / 180) # dy/dx
        else
            asp <- 1 / cos(mean(range(x$data$latitude,na.rm=TRUE)) * pi / 180) # dy/dx
    }
    ## The following is a somewhat provisional hack, to get around a
    ## tendency of plot() to produce latitudes past the poles.
    ## BUG: the use of par("pin") seems to mess up resizing in aqua windows.
    xr <- range(x$data$longitude, na.rm=TRUE)
    yr <- range(x$data$latitude, na.rm=TRUE)
    if (debug) cat("par('pin') is", par("pin"), "\n")
    asp.page <- par("pin")[2] / par("pin")[1] # dy / dx
    if (debug) cat("asp.page=", asp.page, "\n")
    gamma <- asp / asp.page
    if (debug) cat("asp/asp.page=", asp / asp.page, "\n")
    if ((asp / asp.page) < 1) {
        if (debug) cat("type 1\n")
        xr[2] <- xr[1] + (xr[2] - xr[1]) * (asp / asp.page)
    } else {
        if (debug) cat("type 2\n")
        yr[2] <- yr[1] + (yr[2] - yr[1]) / (asp / asp.page)
    }
    if (!missing(bg)) {
        plot.window(xr, yr, asp=asp, xlab="", ylab="", xaxs="i", yaxs="i", log="", ...)
        usr <- par("usr")
        polygon(usr[c(1,2,2,1)], usr[c(3,3,4,4)], col=bg)
        par(new=TRUE)
    }
    plot(xr, yr, asp=asp, xlab="", ylab="", type="n", xaxs="i", yaxs="i",
         axes=axes, ...)
    if (debug) points(xr, yr, col="blue", pch=20)
    if (debug)     abline(v=xr, col="red")
    yaxp <- par("yaxp")
    if (debug) cat("par(pin)",par("pin"),"\n")
    if (yaxp[1] < -90 | yaxp[2] > 90) {
        opin <- par("pin")
        if (debug) cat("inside pin=", par("pin"), " yaxp=",yaxp,"\n")
        yscale <- 180 / (yaxp[2] - yaxp[1])
        if (debug) cat("yscale",yscale," new opin[2]", yscale*opin[2],"\n")
        par(pin=c(opin[1], yscale*opin[2]))
    	lines(x$data$longitude, x$data$latitude, asp=asp, yaxp=c(-90,90,6), yaxs="i", xlab="", ylab="", ...)
        par("pin"=opin)
    } else {
    	lines(x$data$longitude, x$data$latitude, asp=asp, yaxs="i", xaxs="i", xlab="", ylab="", ...)
    }
    if (debug) {
        cat("par(pin)",par("pin"),"\n")
        cat("lon lim:");print(range(x$data$longitude,na.rm=TRUE))
        cat("lat lim:");print(range(x$data$latitude,na.rm=TRUE))
        cat("par:");print(par())
    }
}

read.coastline <- function(file,type=c("R","S","mapgen"),debug=getOption("oce.debug"),log.action)
{
    type <- match.arg(type)
    if (missing(log.action)) log.action <- paste(deparse(match.call()), sep="", collapse="")
    log.item <- processing.log.item(log.action)
    if (type == "R" || type == "S") {
        ##
        ## e.g. data from http://rimmer.ngdc.noaa.gov/coast/
        ## e.g. "~/data/Coastline/wcl_1_5000000.dat")
        if (is.character(file)) {
            file <- file(file, "r")
            on.exit(close(file))
        }
        if (!inherits(file, "connection")) stop("'file' must be a character string or connection")
        if (!isOpen(file)) {
            open(file, "r")
            on.exit(close(file))
        }
        data <- read.table(file, header=FALSE, col.names=c("longitude","latitude"))
        res <- list(data=data, metadata=NULL, processing.log=log.item)
    } else if (type == "mapgen") {
        header <- scan(file, what=character(0), nlines=1, quiet=TRUE);
        if (debug) {
            cat("method is mapgen\n")
            cat("header ")
            cat(header)
        }
        separator <- NULL
                                        # mapgen    # -b
                                        # matlab	nan nan
                                        # Splus     NA NA
                                        # mapgen...
                                        #	1
                                        #	...
                                        #	END
                                        #	2
                                        #   ...
                                        #   END
        if (all.equal(header, c("#","-b"))) {
            lonlat <- scan(file,what=double(0),na.strings=c("#","-b"), quiet=TRUE)
        } else {
            if (all.equal(header, c("nan","nan"))) {
                lonlat <- scan(file,what=double(0),na.strings=c("nan","nan"), quiet=TRUE)
            } else {
                if (all.equal(header, c("NA","NA"))) {
                    lonlat <- scan(file,what=double(0), quiet=TRUE)
                } else {
                    stop(cat("Unknown file type; the unrecognized header line is '",header,"'\n",sep=" "))
                }
            }
        }
        lonlat <- matrix(lonlat, ncol=2,byrow=TRUE)
        data <- data.frame(longitude=lonlat[,1], latitude=lonlat[,2])
        res <- list(data=data, metadata=NULL, processing.log=log.item)
    } else {
        stop("unknown method.  Should be \"R\", \"S\", or \"mapgen\"")
    }
    class(res) <- c("coastline", "oce")
    res
}

summary.coastline <- function(object, ...)
{
    if (!inherits(object, "coastline")) stop("method is only for coastline objects")
    fives <- matrix(nrow=2, ncol=5)
    res <- list(length=length(object$data$longitude),
                missing=sum(is.na(object$data$longitude)),
                fives=fives,
                processing.log=processing.log.summary(object))
    fives[1,] <- fivenum(object$data$latitude, na.rm=TRUE)
    fives[2,] <- fivenum(object$data$longitude, na.rm=TRUE)
    colnames(fives) <- c("Min.", "1st Qu.", "Median", "3rd Qu.", "Max.")
    rownames(fives) <- c("Latitude", "Longitude")
    res$fives <- fives
    class(res) <- "summary.coastline"
    res
}

print.summary.coastline <- function(x, digits=max(6, getOption("digits") - 1),...)
{
    cat("\nCoastline has", x$length, "points, of which", x$missing, "are NA (e.g. separating islands).\n")
    cat("Statistics:\n")
    print(x$fives, digits=digits)
    print(x$processing.log)
    cat("\n")
    invisible(x)
}
