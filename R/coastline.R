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
                            center, span,
                            mgp=getOption("oce.mgp"),
                            mar=c(mgp[1]+1,mgp[1]+1,1,1),
                            bg,
                            axes=TRUE,
                            expand=1.5,
                            debug=getOption("oce.debug"),
                            ...)
{
    oce.debug(debug, "\b\bplot.coastline() {\n")
    if (is.list(x) && "latitude" %in% names(x)) {
        if (!("longitude" %in% names(x)))
            stop("list must contain item named 'longitude'")
        x <- as.coastline(x$latitude, x$longitude)
    } else {
        if (!inherits(x, "coastline"))
            stop("method is only for coastline objects, or lists that contain 'latitude' and 'longitude'")
    }
    par(mgp=mgp, mar=mar)
    gave.center <- !missing(center)
    gave.span <- !missing(span)
    if (gave.center != gave.span)
        stop("must give both 'center' and 'span', or neither one")
    if (gave.center) {
        if (length(center) != 2)
            stop("'center' must contain two values, latitude in deg N and longitude in deg E")
        asp <- 1 / cos(center[1] * pi / 180) #  ignore any provided asp
        yr <- center[1] + span * c(-1/2, 1/2) / 111.11
        xr <- center[2] + span * c(-1/2, 1/2) / 111.11 / asp
    } else {
        if (missing(asp)) {
            asp <- 1 / cos(mean(range(x$data$latitude,na.rm=TRUE)) * pi / 180) # dy/dx
        }
        ## Expand
        xr0 <- range(x$data$longitude, na.rm=TRUE)
        yr0 <- range(x$data$latitude, na.rm=TRUE)
        if (expand >= 0) {
            xr <- mean(xr0) + expand * diff(xr0) * c(-1/2, 1/2)
            yr <- mean(yr0) + expand * diff(yr0) * c(-1/2, 1/2)
        }
    }
    ## The following is a somewhat provisional hack, to get around a
    ## tendency of plot() to produce latitudes past the poles.
    ## BUG: the use of par("pin") seems to mess up resizing in aqua windows.
    oce.debug(debug, "asp=", asp, "\n")
    oce.debug(debug, "par('pin') is", par("pin"), "\n")
    asp.page <- par("pin")[2] / par("pin")[1] # dy / dx
    oce.debug(debug, "asp.page=", asp.page, "\n")
    gamma <- asp / asp.page
    oce.debug(debug, "asp/asp.page=", asp / asp.page, "\n")
    if ((asp / asp.page) < 1) {
        oce.debug(debug, "type 1 (will narrow x range)\n")
        d <- asp / asp.page * diff(xr)
        xr <- mean(xr) + d * c(-1/2, 1/2)
        ## xr[2] <- xr[1] + (xr[2] - xr[1]) * (asp / asp.page)
    } else {
        oce.debug(debug, "type 2 (will narrow y range)\n")
        d <- asp / asp.page * diff(yr)
        yr <- mean(yr) + d * c(-1/2, 1/2)
        ##yr[2] <- yr[1] + (yr[2] - yr[1]) / (asp / asp.page)
    }
    if (!missing(bg)) {
        plot.window(xr, yr, asp=asp, xlab="", ylab="", xaxs="i", yaxs="i", log="", ...)
        usr <- par("usr")
        polygon(usr[c(1,2,2,1)], usr[c(3,3,4,4)], col=bg)
        par(new=TRUE)
    }
    plot(xr, yr, asp=asp, xlab="", ylab="", type="n", xaxs="i", yaxs="i",
         axes=axes, ...)
    if (debug > 0) {
        points(xr, yr, col="blue", pch=20, cex=3)
        abline(v=xr, col="red")
    }
    yaxp <- par("yaxp")
    oce.debug(debug, "par(pin)",par("pin"),"\n")
    if (yaxp[1] < -90 | yaxp[2] > 90) {
        opin <- par("pin")
        oce.debug(debug, "inside pin=", par("pin"), " yaxp=",yaxp,"\n")
        yscale <- 180 / (yaxp[2] - yaxp[1])
        oce.debug(debug, "yscale",yscale," new opin[2]", yscale*opin[2],"\n")
        par(pin=c(opin[1], yscale*opin[2]))
    	lines(x$data$longitude, x$data$latitude, asp=asp, yaxp=c(-90,90,6), yaxs="i", xlab="", ylab="", ...)
        par("pin"=opin)
    } else {
    	lines(x$data$longitude, x$data$latitude, asp=asp, yaxs="i", xaxs="i", xlab="", ylab="", ...)
    }
    oce.debug(debug, "lat lim:", range(x$data$latitude,na.rm=TRUE), "\n")
    oce.debug(debug, "lon lim:", range(x$data$longitude,na.rm=TRUE), "\n")
    oce.debug(debug, "\b\b} # plot.coastline()\n")
}

read.coastline <- function(file,type=c("R","S","mapgen"),debug=getOption("oce.debug"),log.action)
{
    file <- full.filename(file)
    type <- match.arg(type)
    if (missing(log.action)) log.action <- paste(deparse(match.call()), sep="", collapse="")
    log.item <- processing.log.item(log.action)
    if (type == "R" || type == "S") {
        ##
        ## e.g. data from http://rimmer.ngdc.noaa.gov/coast/
        ## e.g. "~/data/Coastline/wcl_1_5000000.dat")
        if (is.character(file)) {
            file <- full.filename(file)
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
        oce.debug(debug, "method is mapgen\nheader:", header, "\n")
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
    cat("Coastline Summary\n-----------------\n\n")
    cat("* Number of points:", x$length, ", of which", x$missing, "are NA (e.g. separating islands).\n")
    cat("\n",...)
    cat("* Statistics of subsample::\n\n", ...)
    cat(show.fives(x, indent='     '), ...)
    cat("\n* Processing log::\n\n", ...)
    cat(x$processing.log, ...)
    invisible(x)
}
