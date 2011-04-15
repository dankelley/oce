as.coastline <- function(latitude, longitude)
{
    n <- length(latitude)
    if (n != length(longitude))
        stop("Lengths of longitude and latitude must be equal")
    data <- data.frame(longitude=longitude, latitude=latitude)
    log.item <- processingLogItem(paste(deparse(match.call()), sep="", collapse=""))
    res <- list(data=data, metadata=NULL, processingLog=log.item)
    class(res) <- c("coastline", "oce")
    res
}

plot.coastline <- function (x,
                            xlab="", ylab="",
                            asp,
                            center, span,
                            expand=1.5,
                            mgp=getOption("oce.mgp"),
                            mar=c(mgp[1]+1,mgp[1]+1,1,1),
                            bg,
                            fill='lightgray',
                            axes=TRUE,
                            debug=getOption("oce.debug"),
                            ...)
{
    opar <- par(no.readonly = TRUE)
    oce.debug(debug, "\b\bplot.coastline(...,",
              "center=", if(missing(center)) "(missing)" else paste("c(", paste(center, collapse=","), ")"),
              "span=", if(missing(span)) "(missing)" else span, ", ...) {\n")
    if (is.list(x) && "latitude" %in% names(x)) {
        if (!("longitude" %in% names(x)))
            stop("list must contain item named 'longitude'")
        x <- as.coastline(x$latitude, x$longitude)
    } else {
        if (!inherits(x, "coastline"))
            stop("method is only for coastline objects, or lists that contain 'latitude' and 'longitude'")
    }
    dots <- list(...)
    names.dots <- names(dots)
    par(mgp=mgp, mar=mar)
    if ("xlim" %in% names.dots) {
        stop("cannot supply 'xlim'; please use 'center' and 'span' instead")
    }
    if ("ylim" %in% names.dots) {
        stop("cannot supply 'ylim'; please use 'center' and 'span' instead")
    }
    gave.center <- !missing(center)
    gave.span <- !missing(span)
    if (gave.center != gave.span)
        stop("must give both 'center' and 'span', or neither one")
    if (gave.center) {
        if (length(center) != 2)
            stop("'center' must contain two values, latitude in deg N and longitude in deg E")
        if (!missing(asp))
            warning("argument 'asp' being ignored, because argument 'center' was given")
        asp <- 1 / cos(center[1] * pi / 180) #  ignore any provided asp
        yr <- center[1] + span * c(-1/2, 1/2) / 111.11
        xr <- center[2] + span * c(-1/2, 1/2) / 111.11 * asp
        oce.debug(debug, "gave center and span; calculated xr=",xr," and yr=",yr,"\n")
    } else {
        if (missing(asp)) {
            if ("ylim" %in% names(dots))
                asp <- 1 / cos(mean(range(dots$ylim, na.rm=TRUE)) * pi / 180) # dy/dx
            else
                asp <- 1 / cos(mean(range(x$data$latitude,na.rm=TRUE)) * pi / 180) # dy/dx
        }
        ## Expand
        xr0 <- range(x$data$longitude, na.rm=TRUE)
        yr0 <- range(x$data$latitude, na.rm=TRUE)
        oce.debug(debug, "xr0=", xr0, "\n")
        oce.debug(debug, "yr0=", yr0, "\n")
        if (expand >= 0 && max(abs(xr0)) < 100 && max(abs(yr0) < 70)) { # don't expand if full map
            xr <- mean(xr0) + expand * diff(xr0) * c(-1/2, 1/2)
            yr <- mean(yr0) + expand * diff(yr0) * c(-1/2, 1/2)
        } else {
            xr <- xr0
            yr <- yr0
        }
    }
    ## The following is a somewhat provisional hack, to get around a
    ## tendency of plot() to produce latitudes past the poles.
    ## BUG: the use of par("pin") seems to mess up resizing in aqua windows.
    asp.page <- par("pin")[2] / par("pin")[1] # dy / dx
    oce.debug(debug, "par('pin')=",par('pin'), "asp=",asp,"asp.page=", asp.page, "\n")
    if (asp < asp.page) {
        oce.debug(debug, "type 1 (will narrow x range)\n")
        d <- asp / asp.page * diff(xr)
        xr <- mean(xr) + d * c(-1/2, 1/2)
        oce.debug(debug, "xr narrowed to:", xr, "\n")
        ## xr[2] <- xr[1] + (xr[2] - xr[1]) * (asp / asp.page)
    } else {
        oce.debug(debug, "type 2 (will narrow y range)\n")
        d <- asp / asp.page * diff(yr)
        yr <- mean(yr) + d * c(-1/2, 1/2)
        oce.debug(debug, "yr narrowed to:", yr, "\n")
        ##yr[2] <- yr[1] + (yr[2] - yr[1]) / (asp / asp.page)
    }
    if (xr[1] < (-180))
        xr[1] <- (-180)
    if (xr[2] >  180)
        xr[2] <- 180
    if (yr[1] <  (-90))
        yr[1] <- (-90)
    if (yr[2] >  90)
        yr[2] <- 90
    oce.debug(debug, "xr:", xr, "\n")
    oce.debug(debug, "yr:", yr, "\n")
    if (!missing(bg)) {
        plot.window(xr, yr, asp=asp, xlab=xlab, ylab=ylab, xaxs="i", yaxs="i", log="", ...)
        usr <- par("usr")
        polygon(usr[c(1,2,2,1)], usr[c(3,3,4,4)], col=bg)
        par(new=TRUE)
    }
    oce.debug(debug, "xr=", xr, "yr=",yr,"\n")
    plot(xr, yr, asp=asp, xlab=xlab, ylab=ylab, type="n", xaxs="i", yaxs="i",
         axes=FALSE, ...)
    if (axes) {
        if (1 == prod(par('mfrow')) * prod(par('mfcol'))) {
            if (FALSE) { ## FIXME: why does this not work?
                ## Construct axes "manually" because axis() does not know the physical range
                if (debug > 0) {
                    points(xr, yr, col="blue", pch=20, cex=3)
                }
                xr.pretty <- pretty(xr)
                yr.pretty <- pretty(yr)
                if (!(min(yr.pretty) > -80 && max(yr.pretty) < 80))
                    yr.pretty <- seq(-90, 90, 45)
                if (!(min(xr.pretty) > -150 && max(xr.pretty) < 150))
                    xr.pretty <- seq(-180, 180, 45)
                oce.debug(debug, "xr.pretty=", xr.pretty, "\n")
                oce.debug(debug, "yr.pretty=", yr.pretty, "\n")
                axis(1, at=xr.pretty, pos=yr.pretty[1])
                axis(3, at=xr.pretty, pos=max(yr.pretty), labels=FALSE)
                axis(2, at=yr.pretty, pos=xr.pretty[1])
                axis(4, at=yr.pretty, pos=max(xr.pretty), labels=FALSE)
            } else {
                box()
                axis(1)
                axis(2)
            }
        } else {
            box()
            axis(1)
            axis(2)
        }
    }
    yaxp <- par("yaxp")
    oce.debug(debug, "par(yaxp)",par("yaxp"),"\n")
    oce.debug(debug, "par(pin)",par("pin"),"\n")
    if (yaxp[1] < -90 | yaxp[2] > 90) {
        ##opin <- par("pin")
        oce.debug(debug, "inside pin=", par("pin"), " yaxp=",yaxp,"\n")
        yscale <- 180 / (yaxp[2] - yaxp[1])
        ##oce.debug(debug, "yscale",yscale," new opin[2]", yscale*opin[2],"\n")
        ##        par(pin=c(opin[1], yscale*opin[2]))
        if (!is.null(fill) && !is.null(x$metadata$fillable) && x$metadata$fillable)
            polygon(x$data$longitude, x$data$latitude, col=fill, ...)
        else
            lines(x$data$longitude, x$data$latitude, ...)
        ##par("pin"=opin)
    } else {
        if (!is.null(fill) && !is.null(x$metadata$fillable))
            polygon(x$data$longitude, x$data$latitude, col=fill, ...)
        else
            lines(x$data$longitude, x$data$latitude, ...)
    }
    oce.debug(debug, "lat lim:", range(x$data$latitude,na.rm=TRUE), "\n")
    oce.debug(debug, "lon lim:", range(x$data$longitude,na.rm=TRUE), "\n")
    oce.debug(debug, "\b\b} # plot.coastline()\n")
    invisible()
}

read.coastline <- function(file,type=c("R","S","mapgen","shapefile"),
                           debug=getOption("oce.debug"),
                           monitor=FALSE,
                           logAction)
{
    file <- full.filename(file)
    type <- match.arg(type)
    if (missing(logAction)) logAction <- paste(deparse(match.call()), sep="", collapse="")
    log.item <- processingLogItem(logAction)
    if (type == "shapefile")
        return(read.coastline.shapefile(file, monitor=monitor, debug=debug))
    if (type == "R" || type == "S") {
        ##
        ## e.g. data from http://rimmer.ngdc.noaa.gov/coast/
        ## e.g. "~/data/Coastline/wcl_1_5000000.dat")
        if (is.character(file)) {
            file <- full.filename(file)
            on.exit(close(file))
        }
        if (!inherits(file, "connection"))
            stop("'file' must be a character string or connection")
        if (!isOpen(file)) {
            open(file, "r")
            on.exit(close(file))
        }
        data <- read.table(file, header=FALSE, col.names=c("longitude","latitude"))
        res <- list(data=data, metadata=list(fillable=FALSE), processingLog=log.item)
    } else if (type == "mapgen") {
        header <- scan(file, what=character(0), nlines=1, quiet=TRUE) # slow, but just one line
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
            lonlat <- scan(file,what=double(0),na.strings=c("#","-b"), quiet=TRUE) # slow, but just one line
        } else {
            if (all.equal(header, c("nan","nan"))) {
                lonlat <- scan(file,what=double(0),na.strings=c("nan","nan"), quiet=TRUE) # fast because whole file
            } else {
                if (all.equal(header, c("NA","NA"))) {
                    lonlat <- scan(file,what=double(0), quiet=TRUE) # fast because whole file
                } else {
                    stop(cat("Unknown file type; the unrecognized header line is '",header,"'\n",sep=" "))
                }
            }
        }
        lonlat <- matrix(lonlat, ncol=2,byrow=TRUE)
        data <- data.frame(longitude=lonlat[,1], latitude=lonlat[,2])
        res <- list(data=data, metadata=list(fillable=FALSE), processingLog=log.item)
    } else {
        stop("unknown method.  Should be \"R\", \"S\", or \"mapgen\"")
    }
    class(res) <- c("coastline", "oce")
    res
}

read.coastline.shapefile <- function(file, lonlim=c(-180,180), latlim=c(-90,90),
                                     debug=getOption("oce.debug"),
                                     monitor=FALSE,
                                     logAction)
{
    ## References:
    ## [1] ESRI Shapefile Technical Description. March 1998.
    ##     http://dl.maptools.org/dl/shapelib/shapefile.pdf
    ## [2] Wikipedia page on shapefile format
    ##     http://en.wikipedia.org/wiki/Shapefile#Shapefile_shape_format_.28.shp.29
    ## Shapefile for Canada:
    ## http://coastalmap.marine.usgs.gov/GISdata/basemaps/canada/shoreline/canada_wvs_geo_wgs84.htm
    oce.debug(debug, "\b\bread.shape() {\n")
    shape.type.list <- c("nullshape",
                         "point", "not used",
                         "polyline", "not used",
                         "polygon", "not used", "not used",
                         "multipoint", "not used", "not used",
                         "pointz", "not used",
                         "polylinez", "not used",
                         "polygonz", "not used", "not used",
                         "multipointz", "not used", "not used",
                         "pointm", "not used",
                         "polylinem", "not used",
                         "polygonm", "not used", "not used",
                         "multipointm", "not used", "not used",
                         "multipatch")

    lonlim <- sort(lonlim)
    latlim <- sort(latlim)

    oce.debug(debug, "\b\bread.shapefile(file=\"", file, "\", ...) {\n", sep="")
    if (is.character(file)) {
        filename <- full.filename(file)
        file <- file(file, "rb")
        on.exit(close(file))
    }
    if (!inherits(file, "connection"))
        stop("argument `file' must be a character string or connection")
    if (!isOpen(file)) {
        filename <- "(connection)"
        open(file, "rb")
        on.exit(close(file))
    }
    seek(file, 0, "end")
    file.size <- seek(file, 0, "start")
    oce.debug(debug, "file.size=", file.size, "as determined from the operating system\n")
    buf <- readBin(file, "raw", file.size)
    ## main header is always 100 bytes [ESRI White paper page 3]
    header <- buf[1:100]
    field.code <- readBin(header[1:4], "integer", n=1, size=4, endian="big")
    if (field.code != 9994)
        stop("first four bytes of file must yield 9994 (as a big-endian integer) but yield ", field.code, "\n")
    file.size.header <- 2*readBin(buf[25:28], "integer", n=1, size=4, endian="big") # it's in 2-byte words
    oce.debug(debug, "file.size.header=", file.size.header, "as interpreted from header\n")
    if (file.size.header != file.size)
        warning("file size is ", file.size, " but the header suggests it to be ", file.size.header, "; using the former")
    shape.type.file <- readBin(buf[33:36], "integer", n=1, size=4, endian="little")
    oce.debug(debug, "shape.type.file=", shape.type.file, "\n")
    if (shape.type.file != 5)
        stop("can only deal with shape-type 5 (polygon) in this version of the software\n")
    xmin <- readBin(buf[37+0:7], "double", n=1, size=8, endian="little")
    ymin <- readBin(buf[45+0:7], "double", n=1, size=8, endian="little")
    xmax <- readBin(buf[53+0:7], "double", n=1, size=8, endian="little")
    ymax <- readBin(buf[61+0:7], "double", n=1, size=8, endian="little")
    metadata <- list(filename=filename, fillable=TRUE)
    oce.debug(debug, sprintf("xmin=%.4f xmax=%.4f ymin=%.4f ymax=%.4f\n", xmin, xmax, ymin, ymax))
    ##
    ## RECORD BY RECORD
    ##
    buf <- buf[101:length(buf)]         # now we just have data
    o <- 0                              # offset for chunk
    record <- 0
    latitude <- longitude <- NULL
    segment <- 0
    while (TRUE) {
        oce.debug(debug, "\n")
        record <- record + 1
        if ((o + 53) > file.size)       # FIXME could be more clever on eof
            break
        ## each record has an 8-byte header followed by data
        record.number <- readBin(buf[o + 1:4], "integer", n=1, size=4, endian="big")
        oce.debug(debug, "record.number=", record.number, "\n")
        record.length <- readBin(buf[o + 5:8], "integer", n=1, size=4, endian="big")
        oce.debug(debug, "record.length=", record.length, "\n")
        shape.type <- readBin(buf[o + 9:12], "integer", n=1, size=4, endian="little")
        if (shape.type == 0)
            break                       # all done
        if (shape.type != shape.type.file)
            stop("record ", record, " has shape type ", shape.type, ", which does not match file value ", shape.type.file)
        ##print(data.frame(0:31, shape.type.list))
        oce.debug(debug, "shape.type=", shape.type, "i.e.", shape.type.list[1+shape.type], "\n")
        ## minimum bounding rectangle, number of parts, number of points, parts, points
        ## MBR is xmin ymin xmax ymax
        mbr <- readBin(buf[o + 13:44], "double", n=4, size=8, endian="little", signed=TRUE)
        oce.debug(debug, "mbr=", paste(mbr, collapse=" "), "\n")
        ## ignore if not in focus box
        intersects.box <- !(mbr[1] > lonlim[2] | mbr[2] > latlim[2] | mbr[3] < lonlim[1] | mbr[4] < latlim[1])
        oce.debug(debug, "intersects.box=", intersects.box, "\n")
        number.parts <- readBin(buf[o + 45:48], "integer", n=1, size=4, endian="little")
        oce.debug(debug, "number.parts=", number.parts, "\n")
        number.points <- readBin(buf[o + 49:52], "integer", n=1, size=4, endian="little", signed=FALSE)
        oce.debug(debug, "number.points=", number.points, "\n")
        if (intersects.box) {
            part.offset <- readBin(buf[o + 53+0:(-1+4*number.parts)],
                                   "integer", n=number.parts, size=4, endian="little", signed=FALSE)
            xy <- matrix(readBin(buf[o + 53 + 4 * number.parts + 0:(-1 + 2 * number.points * 8)],
                                 "double", n=number.points*2, size=8), ncol=2, byrow=TRUE)
            look <- c(1 + part.offset, number.points)
            for (part in 1:number.parts) {
                segment <- segment + 1
                if (monitor){
                    segment <- segment + 1
                    cat(".")
                    if (!(segment %% 50))
                        cat(segment, "\n")
                }
                rows <- look[part]:(-1 + look[part+1])
                latitude <- c(latitude, NA, xy[rows,2])
                longitude <- c(longitude, NA, xy[rows,1])
            }
        }
        o <- o + 53 + 4 * number.parts + 2 * number.points * 8 - 1
    }
    data <- list(longitude=longitude, latitude=latitude)
    oce.debug(debug, "\b\b} # read.shapefile()\n")
    if (missing(logAction))
        logAction <- paste(deparse(match.call()), sep="", collapse="")
    log.item <- processingLogItem(logAction)
    res <- list(data=data, metadata=metadata, processingLog=log.item)
    class(res) <- c("coastline", "oce")
    oce.debug(debug, "\b\b} # read.shape()\n")
    res
}


summary.coastline <- function(object, ...)
{
    if (!inherits(object, "coastline"))
        stop("method is only for coastline objects")
    fives <- matrix(nrow=2, ncol=5)
    res <- list(length=length(object$data$longitude),
                missing=sum(is.na(object$data$longitude)),
                fives=fives,
                processingLog=processingLog.summary(object))
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
    cat("\n* processingLog::\n\n", ...)
    cat(x$processingLog, ...)
    invisible(x)
}
