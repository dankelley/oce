setMethod(f="initialize",
          signature="coastline",
          definition=function(.Object, latitude, longitude, filename="", fillable=FALSE) {
              if (!missing(latitude)) .Object@data$latitude <- latitude
              if (!missing(longitude)) .Object@data$longitude <- longitude
              .Object@metadata$filename <- filename
              .Object@metadata$fillable <- fillable
              .Object@processingLog$time <- as.POSIXct(Sys.time())
              .Object@processingLog$value <- "create 'coastline' object"
              return(.Object)
          })
## the default 'oce' object is sufficient for other methods

as.coastline <- function(latitude, longitude, fillable=FALSE)
{
    n <- length(latitude)
    if (n != length(longitude))
        stop("Lengths of longitude and latitude must be equal")
    rval <- new("coastline", latitude=latitude, longitude=longitude, fillable=fillable)
    rval@processingLog <- processingLog(rval@processingLog, paste(deparse(match.call()), sep="", collapse=""))
    rval
}

setMethod(f="plot",
          signature=signature("coastline"),
          definition=function (x,
                               xlab="", ylab="",
                               asp,
                               clatitude, clongitude, span,
                               ## center, span,
                               expand=1,
                               mgp=getOption("oceMgp"),
                               mar=c(mgp[1]+1,mgp[1]+1,1,1),
                               bg,
                               fill='lightgray',
                               axes=TRUE, cex.axis=par('cex.axis'),
                               add=FALSE,
                               debug=getOption("oceDebug"),
                               ...)
          {
              oceDebug(debug, "\bplot.coastline(...,",
                       "clatitude=", if(missing(clatitude)) "(missing)" else paste("c(", paste(clatitude, collapse=","), ")"),
                       "clongitude=", if(missing(clongitude)) "(missing)" else paste("c(", paste(clongitude, collapse=","), ")"),
                       ", span=", if(missing(span)) "(missing)" else span,
                       ", cex.axis=", cex.axis, 
                       ", ...) {\n", sep="")
              ##cat("top of plot(ctd, which=", which, "...)   mai=", par('mai'), "\n") # FIXME
              if (is.list(x) && "latitude" %in% names(x)) {
                  if (!("longitude" %in% names(x)))
                      stop("list must contain item named 'longitude'")
                  x <- as.coastline(x$latitude, x$longitude)
              } else {
                  if (!inherits(x, "coastline"))
                      stop("method is only for coastline objects, or lists that contain 'latitude' and 'longitude'")
              }
              longitude <- x[["longitude"]]
              latitude <- x[["latitude"]]
              dots <- list(...)
              dotsNames <- names(dots)
              gave.center <- !missing(clatitude) && !missing(clongitude)
              if ("center" %in% dotsNames)
                  stop("please use 'clatitude' and 'clongitude' instead of 'center'")
              if ("xlim" %in% dotsNames) stop("cannot supply 'xlim'; please use 'center' and 'span' instead")
              if ("ylim" %in% dotsNames) stop("cannot supply 'ylim'; please use 'center' and 'span' instead")
              if (4 == length(mar)) par(mar=mar)
              if (3 == length(mgp)) par(mgp=mgp)
              if (add) {
                  if (!is.null(fill) && !is.null(x@metadata$fillable) && x@metadata$fillable) {
                      polygon(longitude, latitude, col=fill, ...)
                      box()                      # clean up edges
                  } else {
                      lines(longitude, latitude, ...)
                  }
              } else {
                  gave.center <- !missing(clatitude) && !missing(clongitude)
                  gave.span <- !missing(span)
                  if (gave.center != gave.span) stop("must give all of 'clatitude', 'clongitude' and 'span', or none of them FIXME")
                  if (gave.center) {
                      if (!missing(asp))
                          warning("argument 'asp' being ignored, because argument 'center' was given")
                      asp <- 1 / cos(clatitude * atan2(1, 1) / 45) #  ignore any provided asp, because lat from center over-rides it
                      xr <- clongitude + span * c(-1/2, 1/2) / 111.11 / asp
                      yr <- clatitude + span * c(-1/2, 1/2) / 111.11
                      oceDebug(debug, "xr=", xr," yr=", yr, " asp=", asp, "\n")
                  } else {
                      xr0 <- range(longitude, na.rm=TRUE)
                      yr0 <- range(latitude, na.rm=TRUE)
                      oceDebug(debug, "xr0=", xr0, " yr0=", yr0, "\n")
                      if (missing(asp)) {
                          if ("ylim" %in% dotsNames)
                              asp <- 1 / cos(mean(range(dots$ylim, na.rm=TRUE)) * atan2(1, 1) / 45) # dy/dx
                          else
                              asp <- 1 / cos(mean(yr0) * atan2(1, 1) / 45) # dy/dx
                      }
                      ## Expand
                      if (expand >= 0 && max(abs(xr0)) < 100 && max(abs(yr0) < 70)) { # don't expand if full map
                          xr <- mean(xr0) + expand * diff(xr0) * c(-1/2, 1/2)
                          yr <- mean(yr0) + expand * diff(yr0) * c(-1/2, 1/2)
                      } else {
                          xr <- xr0
                          yr <- yr0
                      }
                      oceDebug(debug, "xr=", xr, " yr=", yr, "\n")
                  }
                  ## Trim lat or lon, to avoid empty margin space
                  asp.page <- par("pin")[2] / par("pin")[1] # dy / dx
                  oceDebug(debug, "par('pin')=", par('pin'), "\n")
                  oceDebug(debug, "asp=", asp, "\n")
                  oceDebug(debug, "asp.page=", asp.page, "\n")
                  if (asp < asp.page) {
                      oceDebug(debug, "type 1 (will narrow x range)\n")
                      d <- asp.page / asp * diff(xr)
                      oceDebug(debug, "  xr original:", xr, "\n")
                      xr <- mean(xr) + d * c(-1/2, 1/2)
                      oceDebug(debug, "  xr narrowed:", xr, "\n")
                  } else {
                      oceDebug(debug, "type 2 (will narrow y range)\n")
                      d <- asp.page / asp * diff(yr)
                      oceDebug(debug, "  yr original:", yr, "\n")
                      yr <- mean(yr) + d * c(-1/2, 1/2)
                      oceDebug(debug, "  yr narrowed:", yr, "\n")
                  }
                  ## Avoid looking beyond the poles, or the dateline
                  if (xr[1] < (-180))
                      xr[1] <- (-180)
                  if (xr[2] >  180)
                      xr[2] <- 180
                  if (yr[1] <  (-90))
                      yr[1] <- (-90)
                  if (yr[2] >  90)
                      yr[2] <- 90
                  oceDebug(debug, "after range trimming, xr=", xr, " yr=", yr, "\n")
                  ## Draw underlay, if desired
                  if (!missing(bg)) {
                      plot.window(xr, yr, asp=asp, xlab=xlab, ylab=ylab, xaxs="i", yaxs="i", log="", ...)
                      usr <- par("usr")
                      polygon(usr[c(1,2,2,1)], usr[c(3,3,4,4)], col=bg)
                      par(new=TRUE)
                  }
                  ## Ranges
                  plot(xr, yr, asp=asp, xlab=xlab, ylab=ylab, type="n", xaxs="i", yaxs="i", axes=FALSE, ...)
                  usrTrimmed <- par('usr')
                  ## Construct axes "manually" because axis() does not know the physical range
                  if (axes) {
                      xr.pretty <- pretty(xr)
                      yr.pretty <- pretty(yr)
                      if (!(min(yr.pretty) > -80 && max(yr.pretty) < 80))
                          yr.pretty <- seq(-90, 90, 45)
                      if (!(min(xr.pretty) > -150 && max(xr.pretty) < 150))
                          xr.pretty <- seq(-180, 180, 45)
                      oceDebug(debug, "xr.pretty=", xr.pretty, "\n")
                      oceDebug(debug, "yr.pretty=", yr.pretty, "\n")
                      usrTrimmed[1] <- max(-180, usrTrimmed[1])
                      usrTrimmed[2] <- min( 180, usrTrimmed[2])
                      usrTrimmed[3] <- max( -90, usrTrimmed[3])
                      usrTrimmed[4] <- min(  90, usrTrimmed[4])
                      oceDebug(debug, "par('usr')", par('usr'), "\n")
                      oceDebug(debug, "usrTrimmed", usrTrimmed, "\n")
                      axis(1, at=xr.pretty, pos=usrTrimmed[3], cex.axis=cex.axis)
                      oceDebug(debug, "putting bottom y axis at", usrTrimmed[3], "\n")
                      axis(2, at=yr.pretty, pos=usrTrimmed[1], cex.axis=cex.axis, cex=cex.axis)
                      oceDebug(debug, "putting left x axis at", usrTrimmed[1], "\n")
                      ##axis(3, at=xr.pretty, pos=usrTrimmed[4], labels=FALSE)
                      ##oceDebug(debug, "putting top x axis at", usrTrimmed[4], "\n")
                      axis(4, at=yr.pretty, pos=usrTrimmed[2], labels=FALSE, cex.axis=cex.axis)
                      oceDebug(debug, "putting right y axis at", usrTrimmed[2], "\n")
                  }
                  yaxp <- par("yaxp")
                  oceDebug(debug, "par('yaxp')", par("yaxp"), "\n")
                  oceDebug(debug, "par('pin')", par("pin"), "\n")
                  if (yaxp[1] < -90 | yaxp[2] > 90) {
                      oceDebug(debug, "trimming latitude; pin=", par("pin"), "FIXME: not working\n")
                      oceDebug(debug, "trimming latitdue; yaxp=", yaxp, "FIXME: not working\n")
                      yscale <- 180 / (yaxp[2] - yaxp[1])
                      if (!is.null(fill) && ("fillable" %in% names(x@metadata)) && x[["fillable"]]) {
                          polygon(x[["longitude"]], x[["latitude"]], col=fill, ...)
                      } else {
                          lines(x[["longitude"]], x[["latitude"]], ...)
                      }
                  } else {
                      if (!is.null(fill) && !is.null(x@metadata$fillable) && x@metadata$fillable) {
                          polygon(longitude, latitude, col=fill, ...)
                          rect(usrTrimmed[1], usrTrimmed[3], usrTrimmed[2], usrTrimmed[4])
                      } else {
                          lines(longitude, latitude, ...)
                          rect(usrTrimmed[1], usrTrimmed[3], usrTrimmed[2], usrTrimmed[4])
                      }
                  }
              }
              oceDebug(debug, "par('usr')=", par('usr'), "\n")
              oceDebug(debug, "\b\b} # plot.coastline()\n")
              invisible()
          })

read.coastline <- function(file,
                           type=c("R","S","mapgen","shapefile"),
                           debug=getOption("oceDebug"),
                           monitor=FALSE,
                           processingLog)
{
    type <- match.arg(type)
    oceDebug(debug, "\b\bread.coastline(file=\"", file, "\", type=\"", type, "\", ...) {\n", sep="")
    file <- fullFilename(file)
    if (is.character(file)) {
        filename <- file
    } else {
        filename <- "(unknown)"
    }
    if (type == "shapefile") {
        res <- read.coastline.shapefile(file, monitor=monitor, debug=debug)
    } else if (type == "R" || type == "S") {
        ##
        ## e.g. data from http://rimmer.ngdc.noaa.gov/coast/
        ## e.g. "~/data/Coastline/wcl_1_5000000.dat")
        if (is.character(file)) {
            file <- fullFilename(file)
            file <- file(file, "r")
            on.exit(close(file))
        }
        if (!inherits(file, "connection"))
            stop("'file' must be a character string or connection")
        if (!isOpen(file)) {
            open(file, "r")
            on.exit(close(file))
        }
        data <- read.table(file, col.names=c("longitude", "latitude"))
        res <- new("coastline", latitude=data$latitude, longitude=data$longitude, fillable=FALSE, filename=filename)
    } else if (type == "mapgen") {
        header <- scan(file, what=character(0), nlines=1, quiet=TRUE) # slow, but just one line
        oceDebug(debug, "method is mapgen\nheader:", header, "\n")
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
        res <- new("coastline", latitude=lonlat[,2], longitude=lonlat[,1], fillable=FALSE)
    } else {
        stop("unknown method.  Should be \"R\", \"S\", or \"mapgen\"")
    }
    if (missing(processingLog))
        processingLog <- paste(deparse(match.call()), sep="", collapse="")
    res@processingLog <- processingLog(res@processingLog, processingLog)
    oceDebug(debug, "\b\b} # read.coastline()\n")
    res
}

read.coastline.shapefile <- function(file, lonlim=c(-180,180), latlim=c(-90,90),
                                     debug=getOption("oceDebug"),
                                     monitor=FALSE,
                                     processingLog)
{
    ## References:
    ## [1] ESRI Shapefile Technical Description. March 1998.
    ##     http://dl.maptools.org/dl/shapelib/shapefile.pdf
    ## [2] Wikipedia page on shapefile format
    ##     http://en.wikipedia.org/wiki/Shapefile#Shapefile_shape_format_.28.shp.29
    ## Shapefile for Canada:
    ## http://coastalmap.marine.usgs.gov/GISdata/basemaps/canada/shoreline/canada_wvs_geo_wgs84.htm
    oceDebug(debug, "\b\bread.shape() {\n")
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

    oceDebug(debug, "\b\bread.shapefile(file=\"", file, "\", ...) {\n", sep="")
    if (is.character(file)) {
        filename <- fullFilename(file)
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
    oceDebug(debug, "file.size=", file.size, "as determined from the operating system\n")
    buf <- readBin(file, "raw", file.size)
    ## main header is always 100 bytes [ESRI White paper page 3]
    header <- buf[1:100]
    field.code <- readBin(header[1:4], "integer", n=1, size=4, endian="big")
    if (field.code != 9994)
        stop("first four bytes of file must yield 9994 (as a big-endian integer) but yield ", field.code, "\n")
    file.size.header <- 2*readBin(buf[25:28], "integer", n=1, size=4, endian="big") # it's in 2-byte words
    oceDebug(debug, "file.size.header=", file.size.header, "as interpreted from header\n")
    if (file.size.header != file.size)
        warning("file size is ", file.size, " but the header suggests it to be ", file.size.header, "; using the former")
    shape.type.file <- readBin(buf[33:36], "integer", n=1, size=4, endian="little")
    oceDebug(debug, "shape.type.file=", shape.type.file, "\n")
    if (shape.type.file != 5)
        stop("can only deal with shape-type 5 (polygon) in this version of the software\n")
    xmin <- readBin(buf[37+0:7], "double", n=1, size=8, endian="little")
    ymin <- readBin(buf[45+0:7], "double", n=1, size=8, endian="little")
    xmax <- readBin(buf[53+0:7], "double", n=1, size=8, endian="little")
    ymax <- readBin(buf[61+0:7], "double", n=1, size=8, endian="little")
    metadata <- list(filename=filename, fillable=TRUE)
    oceDebug(debug, sprintf("xmin=%.4f xmax=%.4f ymin=%.4f ymax=%.4f\n", xmin, xmax, ymin, ymax))
    ##
    ## RECORD BY RECORD
    ##
    buf <- buf[101:length(buf)]         # now we just have data
    o <- 0                              # offset for chunk
    record <- 0
    latitude <- longitude <- NULL
    segment <- 0
    while (TRUE) {
        oceDebug(debug, "\n")
        record <- record + 1
        if ((o + 53) > file.size)       # FIXME could be more clever on eof
            break
        ## each record has an 8-byte header followed by data
        record.number <- readBin(buf[o + 1:4], "integer", n=1, size=4, endian="big")
        oceDebug(debug, "record.number=", record.number, "\n")
        record.length <- readBin(buf[o + 5:8], "integer", n=1, size=4, endian="big")
        oceDebug(debug, "record.length=", record.length, "\n")
        shape.type <- readBin(buf[o + 9:12], "integer", n=1, size=4, endian="little")
        if (shape.type == 0)
            break                       # all done
        if (shape.type != shape.type.file)
            stop("record ", record, " has shape type ", shape.type, ", which does not match file value ", shape.type.file)
        ##print(data.frame(0:31, shape.type.list))
        oceDebug(debug, "shape.type=", shape.type, "i.e.", shape.type.list[1+shape.type], "\n")
        ## minimum bounding rectangle, number of parts, number of points, parts, points
        ## MBR is xmin ymin xmax ymax
        mbr <- readBin(buf[o + 13:44], "double", n=4, size=8, endian="little", signed=TRUE)
        oceDebug(debug, "mbr=", paste(mbr, collapse=" "), "\n")
        ## ignore if not in focus box
        intersects.box <- !(mbr[1] > lonlim[2] | mbr[2] > latlim[2] | mbr[3] < lonlim[1] | mbr[4] < latlim[1])
        oceDebug(debug, "intersects.box=", intersects.box, "\n")
        number.parts <- readBin(buf[o + 45:48], "integer", n=1, size=4, endian="little")
        oceDebug(debug, "number.parts=", number.parts, "\n")
        number.points <- readBin(buf[o + 49:52], "integer", n=1, size=4, endian="little")
        oceDebug(debug, "number.points=", number.points, "\n")
        if (intersects.box) {
            part.offset <- readBin(buf[o + 53+0:(-1+4*number.parts)],
                                   "integer", n=number.parts, size=4, endian="little")
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
    res <- new("coastline", latitude=latitude, longitude=longitude, fillable=TRUE)
    res@metadata <- metadata
    if (missing(processingLog))
        processingLog <- paste(deparse(match.call()), sep="", collapse="")
    res@processingLog <- processingLog(res@processingLog, paste(deparse(match.call()), sep="", collapse=""))
    oceDebug(debug, "\b\b} # read.shape()\n")
    res
}


summary.coastline <- function(object, ...)
{
    if (!inherits(object, "coastline"))
        stop("method is only for coastline objects")
    threes <- matrix(nrow=2, ncol=3)
    threes[1,] <- threenum(object@data$latitude)
    threes[2,] <- threenum(object@data$longitude)
    colnames(threes) <- c("Min.", "Mean", "Max.")
    rownames(threes) <- c("Latitude", "Longitude")
    cat("Coastline Summary\n-----------------\n\n")
    cat("* Number of points:", length(object@data$latitude), ", of which", 
        sum(is.na(object@data$latitude)), "are NA (e.g. separating islands).\n")
    cat("\n",...)
    cat("* Statistics of subsample::\n\n", ...)
    print(threes)
    cat("\n")
    processingLogShow(object)
}

