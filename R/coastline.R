setMethod(f="initialize",
          signature="coastline",
          definition=function(.Object, longitude=NULL, latitude=NULL, filename="", fillable=FALSE) {
              .Object@data$longitude <- longitude
              .Object@data$latitude <- latitude
              .Object@metadata$filename <- filename
              .Object@metadata$fillable <- fillable
              .Object@processingLog$time <- as.POSIXct(Sys.time())
              .Object@processingLog$value <- "create 'coastline' object"
              return(.Object)
          })


setMethod(f="subset",
          signature="oce",
          definition=function(x, subset, ...) {
              if (missing(subset))
                  stop("must give 'subset'")
              ## FIXME: need the stuff that's below??
              ###   subsetString <- paste(deparse(substitute(subset)), collapse=" ")
              ###   if (!length(grep("latitude", subsetString)) && !length(grep("longitude", subsetString)))
              ###       stop("can only subset a coastline by 'latitude' or 'longitude'")
              keep <- eval(substitute(subset), x@data, parent.frame(2))
              rval <- x
              rval@data$latitude[!keep] <- NA
              rval@data$longitude[!keep] <- NA
              rval@processingLog <- processingLogAppend(rval@processingLog, paste(deparse(match.call()), sep="", collapse=""))
              rval
          })

setMethod(f="summary",
          signature="coastline",
          definition=function(object, ...) {
              cat("Coastline Summary\n-----------------\n\n")
              cat("* Number of points:", length(object@data$latitude), ", of which", 
                  sum(is.na(object@data$latitude)), "are NA (e.g. separating islands).\n")
              cat("\n",...)
              cat("* Statistics of subsample::\n\n", ...)
              ndata <- length(object@data)
              threes <- matrix(nrow=ndata, ncol=3)
              for (i in 1:ndata)
                  threes[i,] <- threenum(object@data[[i]])
              rownames(threes) <- paste("   ", names(object@data))
              colnames(threes) <- c("Min.", "Mean", "Max.")
              print(threes, indent='  ')
              processingLogShow(object)
          })

 
as.coastline <- function(longitude, latitude, fillable=FALSE)
{
    if (missing(longitude)) stop("must provide longitude")
    if (missing(latitude)) stop("must provide latitude")
    names <- names(longitude)
    if ("longitude" %in% names && "latitude" %in% names) {
        latitude <- longitude[["latitude"]]
        longitude <- longitude[["longitude"]]
    }
    n <- length(latitude)
    if (n != length(longitude))
        stop("Lengths of longitude and latitude must be equal")
    rval <- new("coastline", longitude=longitude, latitude=latitude, fillable=fillable)
    rval@processingLog <- processingLogAppend(rval@processingLog, paste(deparse(match.call()), sep="", collapse=""))
    rval
}

setMethod(f="plot",
          signature=signature("coastline"),
          definition=function (x,
                               xlab="", ylab="", showHemi=TRUE,
                               asp,
                               clongitude, clatitude, span,
                               lonlabel=NULL, latlabel=NULL, sides=NULL,
                               projection=NULL, parameters=NULL, orientation=NULL,
                               expand=1,
                               mgp=getOption("oceMgp"), mar=c(mgp[1]+1,mgp[1]+1,1,1),
                               bg, fill='lightgray',
                               axes=TRUE, cex.axis=par('cex.axis'),
                               add=FALSE, inset=FALSE,
                               geographical=0,
                               longitudelim, latitudelim, # for old usage
                               debug=getOption("oceDebug"),
                               ...)
          {
              oceDebug(debug, "plot.coastline(...",
                       ", clongitude=", if(missing(clongitude)) "(missing)" else clongitude,
                       ", clatitude=", if(missing(clatitude)) "(missing)" else clatitude, 
                       ", span=", if(missing(span)) "(missing)" else span,
                       ", geographical=", geographical,
                       ", projection=", if (is.null(projection)) "NULL" else projection,
                       ", cex.axis=", cex.axis, 
                       ", inset=", inset, 
                       ", ...) {\n", sep="", unindent=1)
              if (!missing(longitudelim) || !missing(latitudelim)) {
                  if (missing(longitudelim) || missing(latitudelim))
                      stop("if longitudelim or latitudelim are given, both must be given")
                  if (!missing(clongitude) || !missing(clatitude) || !missing(span))
                      stop("if longitudelim or latitudelim are given, must not supply clongitude, clatitude, or span")
                  clongitude <- mean(longitudelim)
                  clatitude <- mean(latitudelim)
                  span <- geodDist(min(longitudelim), min(latitudelim), max(longitudelim), max(latitudelim))
                  warning("plot.coastline() converting longitudelim and latitudelim to clongitude=",
                          round(clongitude, 4),
                          ", clatitude=", round(clatitude, 4), " and span=", round(span, 0), "\n")
              }
              if (!is.null(projection)) {
                  if (missing(span))
                      span <- 1000
                  if (missing(clongitude))
                      longitudelim <- c(-180, 180)
                  else
                      longitudelim <- clongitude + c(-1, 1) * span / 111 / 2
                  if (missing(clatitude))
                      latitudelim <- c(-90, 90)
                  else
                      latitudelim <- clatitude + c(-1, 1) * span / 111 / 2
                  mapPlot(x[['longitude']], x[['latitude']], longitudelim, latitudelim,
                          showHemi=showHemi,
                          mgp=mgp, mar=mar,
                          bg="white", fill=fill, type='l', axes=TRUE,
                          lonlabel=lonlabel, latlabel=latlabel, sides=sides,
                          projection=projection, parameters=parameters, orientation=orientation,
                          debug=debug-1, ...)

                  oceDebug(debug, "} # plot.coastline()\n", unindent=1)
                  return(invisible())
              }
              geographical <- round(geographical)
              if (geographical < 0 || geographical > 2)
                  stop("argument geographical must be 0, 1, or 2")
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
              gave.center <- !missing(clongitude) && !missing(clatitude)
              if ("center" %in% dotsNames)
                  stop("use 'clongitude' and 'clatitude' instead of 'center'")
              if ("xlim" %in% dotsNames) stop("do not specify 'xlim'; give 'clongitude' and 'span' instead")
              if ("ylim" %in% dotsNames) stop("do not specify 'ylim'; give 'clatitude' and 'span' instead")
              if (!inset)
                  par(mar=mar)
              par(mgp=mgp)
              if (add) {
                  if ((is.logical(fill) && fill || is.character(fill)) && (!is.null(x@metadata$fillable) && x@metadata$fillable)) {
                      polygon(longitude, latitude, col=fill, ...)
                      if (axes)
                          box()                      # clean up edges
                  } else {
                      lines(longitude, latitude, ...)
                  }
              } else {
                  gaveSpan <- !missing(span)
                  if (!missing(clatitude) && !missing(clongitude)) {
                      if (!missing(asp))
                          warning("argument 'asp' being ignored, because argument 'clatitude' and 'clongitude' were given")
                      asp <- 1 / cos(clatitude * atan2(1, 1) / 45) #  ignore any provided asp, because lat from center over-rides it
                      xr <- clongitude + sqrt(1/2) * span * c(-1/2, 1/2) / 111.11 / asp
                      yr <- clatitude + sqrt(1/2) * span * c(-1/2, 1/2) / 111.11
                      xr0 <- xr
                      yr0 <- yr
                      oceDebug(debug, "xr=", xr," yr=", yr, "span=", span, "\n")
                      oceDebug(debug, "corner-to-corner span=", geodDist(xr[1], yr[1], xr[2], yr[2]), " km\n")
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
                      if (missing(span)) {
                          if (expand >= 0 && max(abs(xr0)) < 100 && max(abs(yr0) < 70)) { # don't expand if full map
                              xr <- mean(xr0) + expand * diff(xr0) * c(-1/2, 1/2)
                              yr <- mean(yr0) + expand * diff(yr0) * c(-1/2, 1/2)
                          } else {
                              xr <- xr0
                              yr <- yr0
                          }
                      } else {
                          xr <- mean(xr0) + span * c(-1/2, 1/2) / 111.11 / asp
                          yr <- mean(yr0)+ span * c(-1/2, 1/2) / 111.11
                      }
                      oceDebug(debug, "xr=", xr, " yr=", yr, "\n")
                  }
                  ## Trim lat or lon, to avoid empty margin space
                  if (FALSE) { # disable for issue 677 (as a test, or maybe permanently)
                      asp.page <- par("fin")[2] / par("fin")[1] # dy / dx
                      oceDebug(debug, "par('pin')=", par('pin'), "\n")
                      oceDebug(debug, "par('fin')=", par('fin'), "\n")
                      oceDebug(debug, "asp=", asp, "\n")
                      oceDebug(debug, "asp.page=", asp.page, "\n")
                      if (!is.finite(asp))
                          asp <- 1 / cos(clatitude * atan2(1, 1) / 45)
                      if (asp < asp.page) {
                          oceDebug(debug, "type 1 (will narrow x range)\n")
                          d <- asp.page / asp * diff(xr)
                          oceDebug(debug, "  xr original:", xr, "\n")
                          xr <- mean(xr) + d * c(-1/2, 1/2)
                          oceDebug(debug, "  xr narrowed:", xr, "\n")
                      } else {
                          oceDebug(debug, "type 2 (will narrow y range)\n")
                          d <- asp.page / asp * diff(yr)
                          oceDebug(debug, "  yr original:", yr, ", yielding approx span", 111*diff(yr),
                                   "km\n")
                          yr <- mean(yr) + d * c(-1/2, 1/2)
                          oceDebug(debug, "  yr narrowed:", yr, "\n")
                          oceDebug(debug, "corner-to-corner span=", geodDist(xr[1], yr[1], xr[2], yr[2]), " km\n")
                      }
                  }
                  ## Avoid looking beyond the poles, or the dateline
                  if (xr[1] < (-180)) {
                      xr[1] <- (-180)
                  }
                  if (xr[2] >  180) {
                      xr[2] <- 180
                  }
                  if (yr[1] <  (-90)) {
                      yr[1] <- (-90)
                  }
                  if (yr[2] >  90) {
                      yr[2] <- 90
                  }
                  oceDebug(debug, "after range trimming, xr=", xr, " yr=", yr, "\n")
                  oceDebug(debug, "corner-to-corner span=", geodDist(xr[1], yr[1], xr[2], yr[2]), " km\n")
                  ## Draw underlay, if desired
                  plot(xr, yr, asp=asp, xlab=xlab, ylab=ylab, type="n", xaxs="i", yaxs="i", axes=FALSE, ...)
                  if (!missing(bg)) {
                      plot.window(xr, yr, asp=asp, xlab=xlab, ylab=ylab, xaxs="i", yaxs="i", log="", ...)
                      usr <- par("usr")
                      oceDebug(debug, "drawing background; usr=", par('usr'), "bg=", bg, "\n")
                      ## polygon(usr[c(1,2,2,1)], usr[c(3,3,4,4)], col=bg)
                      rect(usr[1], usr[3], usr[2], usr[4], col=bg)
                      par(new=TRUE)
                  }
                  ## Ranges
                  ##plot(xr, yr, asp=asp, xlab=xlab, ylab=ylab, type="n", xaxs="i", yaxs="i", axes=FALSE, ...)
                  usrTrimmed <- par('usr')
                  ## Construct axes "manually" because axis() does not know the physical range
                  if (axes) {
                      prettyLat <- function(yr, ...)
                      {
                          res <- pretty(yr, ...)
                          if (diff(yr) > 100)
                              res <- seq(-90, 90, 45)
                          res
                      }
                      prettyLon <- function(xr, ...)
                      {
                          res <- pretty(xr, ...)
                          if (diff(xr) > 100)
                              res <- seq(-180, 180, 45)
                          res
                      }
                      oceDebug(debug, "xr:", xr, ", yr:", yr, ", xr0:", xr0, ", yr0:", yr0, "\n")
                      ##xr.pretty <- prettyLon(xr, n=if (geographical)3 else 5, high.u.bias=20)
                      xr.pretty <- prettyLon(par('usr')[1:2], n=if (geographical)3 else 5, high.u.bias=20)
                      ##yr.pretty <- prettyLat(yr, n=if (geographical)3 else 5, high.u.bias=20)
                      yr.pretty <- prettyLat(par('usr')[3:4], n=if (geographical)3 else 5, high.u.bias=20)
                      oceDebug(debug, "xr.pretty=", xr.pretty, "\n")
                      oceDebug(debug, "yr.pretty=", yr.pretty, "\n")
                      oceDebug(debug, "usrTrimmed", usrTrimmed, "(original)\n")
                      usrTrimmed[1] <- max(-180, usrTrimmed[1])
                      usrTrimmed[2] <- min( 180, usrTrimmed[2])
                      usrTrimmed[3] <- max( -90, usrTrimmed[3])
                      usrTrimmed[4] <- min(  90, usrTrimmed[4])
                      oceDebug(debug, "usrTrimmed", usrTrimmed, "\n")
                      oceDebug(debug, "par('usr')", par('usr'), "\n")
                      xlabels <- format(xr.pretty)
                      ylabels <- format(yr.pretty)
                      if (geographical >= 1) {
                          xlabels <- sub("-", "", xlabels)
                          ylabels <- sub("-", "", ylabels)
                      }
                      if (geographical == 2) {
                          xr.pretty <- prettyPosition(xr.pretty, debug=debug-1)
                          yr.pretty <- prettyPosition(yr.pretty, debug=debug-1)
                          xlabels <- formatPosition(xr.pretty, type='expression')
                          ylabels <- formatPosition(yr.pretty, type='expression')
                      }
                      axis(1, at=xr.pretty, labels=xlabels, pos=usrTrimmed[3], cex.axis=cex.axis)
                      oceDebug(debug, "putting bottom x axis at", usrTrimmed[3], "with labels:", xlabels, "\n")
                      axis(2, at=yr.pretty, labels=ylabels, pos=usrTrimmed[1], cex.axis=cex.axis, cex=cex.axis)
                      oceDebug(debug, "putting left y axis at", usrTrimmed[1], "\n")
                      axis(3, at=xr.pretty, labels=rep("", length.out=length(xr.pretty)), pos=usrTrimmed[4], cex.axis=cex.axis)
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
                      if ((is.logical(fill) && fill || is.character(fill)) && (!is.null(x@metadata$fillable) && x@metadata$fillable)) {
                          polygon(x[["longitude"]], x[["latitude"]], col=fill, ...)
                      } else {
                          lines(x[["longitude"]], x[["latitude"]], ...)
                      }
                  } else {
                      if ((is.logical(fill) && fill || is.character(fill)) && (!is.null(x@metadata$fillable) && x@metadata$fillable)) {
                          polygon(longitude, latitude, col=fill, ...)
                          if (axes)
                              rect(usrTrimmed[1], usrTrimmed[3], usrTrimmed[2], usrTrimmed[4])
                      } else {
                          lines(longitude, latitude, ...)
                          if (axes)
                              rect(usrTrimmed[1], usrTrimmed[3], usrTrimmed[2], usrTrimmed[4])
                      }
                  }
              }
              ##box()
              oceDebug(debug, "par('usr')=", par('usr'), "\n")
              oceDebug(debug, "} # plot.coastline()\n", unindent=1)
              invisible()
          })

read.coastline <- function(file,
                           type=c("R","S","mapgen","shapefile","openstreetmap"),
                           debug=getOption("oceDebug"),
                           monitor=FALSE,
                           processingLog)
{
    type <- match.arg(type)
    oceDebug(debug, "read.coastline(file=\"", file, "\", type=\"", type, "\", ...) {\n", sep="", unindent=1)
    file <- fullFilename(file)
    if (is.character(file)) {
        filename <- file
    } else {
        filename <- "(unknown)"
    }
    if (type == "shapefile") {
        res <- read.coastline.shapefile(file, monitor=monitor, debug=debug, processingLog=processingLog)
    } else if (type == "openstreetmap") {
        res <- read.coastline.openstreetmap(file, monitor=monitor, debug=debug, processingLog=processingLog)
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
        data <- read.table(file, col.names=c("longitude", "latitude"), stringsAsFactors=FALSE)
        res <- new("coastline", longitude=data$longitude, latitude=data$latitude, fillable=FALSE, filename=filename)
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
        res <- new("coastline", longitude=lonlat[,1], latitude=lonlat[,2], fillable=FALSE)
    } else {
        stop("unknown method.  Should be \"R\", \"S\", or \"mapgen\"")
    }
    if (missing(processingLog))
        processingLog <- paste(deparse(match.call()), sep="", collapse="")
    res@processingLog <- processingLogAppend(res@processingLog, processingLog)
    oceDebug(debug, "} # read.coastline()\n", unindent=1)
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
    oceDebug(debug, "read.shapefile(file=\"", file, "\", ...) {\n", sep="", unindent=1)
    shapeTypeList <- c("nullshape",    # 0
                       "point",        # 1
                       "not used",     # 2
                       "polyline",     # 3
                       "not used",     # 4
                       "polygon",      # 5
                       "not used",     # 6
                       "not used",     # 7
                       "multipoint",   # 8
                       "not used",     # 9
                       "not used",     # 10
                       "pointz",       # 11
                       "not used",     # 12
                       "polylinez",    # 13
                       "not used",     # 14
                       "polygonz",     # 15
                       "not used",     # 16
                       "not used",     # 17
                       "multipointz",  # 18
                       "not used",     # 19
                       "not used",     # 20
                       "pointm",       # 21
                       "not used",     # 22
                       "polylinem",    # 23
                       "not used",     # 24
                       "polygonm",     # 25
                       "not used",     # 26
                       "not used",     # 27
                       "multipointm",  # 28
                       "not used",     # 29
                       "not used",     # 30
                       "multipatch")   # 31

    lonlim <- sort(lonlim)
    latlim <- sort(latlim)

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
    fileSize <- seek(file, 0, "start")
    oceDebug(debug, "file.size:", fileSize, "as determined from the operating system\n")
    buf <- readBin(file, "raw", fileSize)
    ## main header is always 100 bytes [ESRI White paper page 3]
    header <- buf[1:100]
    fieldCode <- readBin(header[1:4], "integer", n=1, size=4, endian="big")
    if (fieldCode != 9994)
        stop("first four bytes of file must yield 9994 (as a big-endian integer) but yield ", fieldCode, "\n")
    fileSizeHeader <- 2*readBin(buf[25:28], "integer", n=1, size=4, endian="big") # it's in 2-byte words
    oceDebug(debug, "fileSizeHeader:", fileSizeHeader, "as interpreted from header\n")
    if (fileSizeHeader != fileSize)
        warning("file size is ", fileSize, " but the header suggests it to be ", fileSizeHeader, "; using the former")
    shapeTypeFile <- readBin(buf[33:36], "integer", n=1, size=4, endian="little")
    oceDebug(debug, "shapeTypeFile:", shapeTypeFile, "(", shapeTypeList[shapeTypeFile+1], ")\n")
    if (shapeTypeFile != 5 && shapeTypeFile != 3 && shapeTypeFile != 15) {
        warning("can handle shape-type 3 (", shapeTypeList[4], ") and 5 (",
             shapeTypeList[6], "), but not ", shapeTypeFile, " (",
            shapeTypeList[shapeTypeFile+1], ")\n")
        return(NULL)
    }
    if (3 == shapeTypeFile) {
        oceDebug(debug, "shapeTypeFile == 3, so assuming a depth-contour file\n")
        dbfName <- paste(gsub(".shp$", "", filename), ".dbf", sep="")
        oceDebug(debug, " reading DBF file '", dbfName, "'\n", sep="")
        if (requireNamespace("foreign", quietly=TRUE)) {
            depths <- foreign::read.dbf(dbfName)[[1]]
        } else {
            stop("cannot read shapeFile element of type 3 without the 'foreign' package being installed")
        }
    }
    xmin <- readBin(buf[37+0:7], "double", n=1, size=8, endian="little")
    ymin <- readBin(buf[45+0:7], "double", n=1, size=8, endian="little")
    xmax <- readBin(buf[53+0:7], "double", n=1, size=8, endian="little")
    ymax <- readBin(buf[61+0:7], "double", n=1, size=8, endian="little")
    metadata <- list(filename=filename, fillable=TRUE)
    oceDebug(debug, sprintf("xmin: %.4f, xmax: %.4f, ymin: %.4f, ymax: %.4f\n", xmin, xmax, ymin, ymax))
    ##
    ## RECORD BY RECORD
    ##
    buf <- buf[101:length(buf)]         # now we just have data
    o <- 0                              # offset for chunk
    record <- 0
    latitude <- longitude <- NULL
    segment <- 0
    res <- new("coastline", fillable=shapeTypeFile==5)
    while (TRUE) {
        record <- record + 1
        if ((o + 53) > fileSize) {      # FIXME could be more clever on eof
            oceDebug(debug, "o:", o, ", fileSize:", fileSize, " ... so finished\n")
            break
        }
        ## each record has an 8-byte header followed by data [1 table 2] BIG endian
        recordNumber <- readBin(buf[o + 1:4], "integer", n=1, size=4, endian="big")
        recordLength <- readBin(buf[o + 5:8], "integer", n=1, size=4, endian="big")
        ## first part of data is shape type [1 table 3 for null, etc] LITTLE endian
        shapeType <- readBin(buf[o + 9:12], "integer", n=1, size=4, endian="little")
        if (shapeType < 0) stop("cannot have shapeType < 0, but got ", shapeType, " (programming error)")
        if (shapeType > 31) stop("cannot have shapeType > 31, but got ", shapeType, " (programming error)")
        if (shapeType == 0) { # NULL record; just skip 4 bytes (I guess; [1] table 3)
            o <- o + 12
        } else {
            if (shapeType != shapeTypeFile)
                stop("record ", record, " has shape type ", shapeType, ", which does not match file value ", shapeTypeFile)
            ## minimum bounding rectangle, number of parts, number of points, parts, points
            ## MBR is xmin ymin xmax ymax
            mbr <- readBin(buf[o + 13:44], "double", n=4, size=8, endian="little", signed=TRUE)
            ## ignore if not in focus box
            intersectsBox <- !(mbr[1] > lonlim[2] | mbr[2] > latlim[2] | mbr[3] < lonlim[1] | mbr[4] < latlim[1])
            numberParts <- readBin(buf[o + 45:48], "integer", n=1, size=4, endian="little")
            numberPoints <- readBin(buf[o + 49:52], "integer", n=1, size=4, endian="little")
            oceDebug(debug, " recordNUmber:", recordNumber,
                     ", shapeType:", shapeType,
                     " (", shapeTypeList[1+shapeType], ")",
                     ", numberPoints:", numberPoints,
                     ", numberParts:", numberParts,
                     ", intersectsBox:", intersectsBox,
                     "\n", sep="")
            if (intersectsBox) {
                partOffset <- readBin(buf[o + 53+0:(-1+4*numberParts)],
                                      "integer", n=numberParts, size=4, endian="little")
                xy <- matrix(readBin(buf[o + 53 + 4 * numberParts + 0:(-1 + 2 * numberPoints * 8)],
                                     "double", n=numberPoints*2, size=8), ncol=2, byrow=TRUE)
                look <- c(1 + partOffset, numberPoints)
                for (part in 1:numberParts) {
                    segment <- segment + 1
                    if (monitor){
                        segment <- segment + 1
                        cat(".")
                        if (!(segment %% 50))
                            cat(segment, "\n")
                    }
                    rows <- seq.int(look[part], -1 + look[part+1])
                    latitude <- c(latitude, NA, xy[rows,2]) # FIXME: this is slow; can we know size at start?
                    longitude <- c(longitude, NA, xy[rows,1])
                }
            }
            o <- o + 53 + 4 * numberParts + 2 * numberPoints * 8 - 1
        }
    }
    res@data$latitude <- latitude
    res@data$longitude <- longitude
    res@metadata <- metadata
    if (shapeTypeFile == 3) {
        res@metadata$depths <- depths
    }
    if (missing(processingLog))
        processingLog <- paste(deparse(match.call()), sep="", collapse="")
    res@processingLog <- processingLogAppend(res@processingLog, processingLog)
    oceDebug(debug, "} # read.coastline.shapefile()\n", unindent=1)
    res
}

read.coastline.openstreetmap <- function(file, lonlim=c(-180,180), latlim=c(-90,90),
                                     debug=getOption("oceDebug"),
                                     monitor=FALSE,
                                     processingLog)
{
    oceDebug(debug, "read.coastline.openstreetmap(file=\"", file, "\", ...) {\n", sep="", unindent=1)
    ## FIXME: ignoring lonlim and latlim
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
    res <- new("coastline", fillable=FALSE, filename=filename)
    d <- readLines(file)
    ## get all <nd> (even if only using some)
    nodeLines <- d[grep("^ *<node", d)]
    nodeIds <- as.numeric(sub('".*$', '', sub('^.* id="', '', nodeLines)))
    nodeLats <- as.numeric(sub('".*$', '', sub('^.* lat="', '', nodeLines)))
    nodeLons <- as.numeric(sub('".*$', '', sub('^.* lon="', '', nodeLines)))
    ## get all <way>
    wayStart <- grep("<way ", d)
    wayEnd <- grep("</way ", d)
    coastlineWayEnd <- grep('k="natural" v="coastline"', d)
    ncoastline <- length(coastlineWayEnd)
    coastlineWayStart <- vector("integer", ncoastline)
    for (i in 1:ncoastline) {
        coastlineWayStart[i] <- wayStart[max(which(wayStart < coastlineWayEnd[i]))]
    }
    oceDebug(debug, "ncoastline:", ncoastline, "\n")
    latitude <- longitude <- NULL
    for (i in 1:ncoastline) {
        oceDebug(debug, "coastline chunk #", i, "\n")
        look <- d[seq.int(coastlineWayStart[i]+1, coastlineWayEnd[i]-1)]
        look <- look[grep("ref=", look)]
        refs <- as.numeric(sub('\"/>', '', sub('.*=\"', '', look)))
        ## following is 10% slower than using match
        ##for (r in refs) {
        ##    w <- which(r == nodeIds)   # FIXME: for speed, perhaps use match(r, nodeIds)
        ##    latitude <- c(latitude, nodeLats[w])
        ##    longitude <- c(longitude, nodeLons[w])
        ##}
        m <- match(refs, nodeIds)
        longitude <- c(longitude, nodeLons[m], NA)
        latitude <- c(latitude, nodeLats[m], NA)
    }
    res@data$latitude <- latitude
    res@data$longitude <- longitude
    if (missing(processingLog))
        processingLog <- paste(deparse(match.call()), sep="", collapse="")
    res@processingLog <- processingLogAppend(res@processingLog, processingLog)
    oceDebug(debug, "} # read.coastline.openstreetmap()\n", unindent=1)
    res
}

coastlineBest <- function(lonRange, latRange, span, debug=getOption("oceDebug"))
{
    oceDebug(debug, "coastlineBest(lonRange=c(", paste(round(lonRange, 2), collapse=","),
             "), latRange=c(", paste(round(latRange, 2), collapse=","),
             "), span=", if (missing(span)) "(missing)" else span,
             ", debug=", debug, ") {\n", sep="", unindent=1)
    if (missing(span)) {
        if (any(lonRange > 180)) {
            lonRange <- lonRange - 360 # FIXME: does this always work?
            oceDebug(debug, "adjusted lonRange:", lonRange, "\n")
        }
        lonRange <- sort(lonRange)
        latRange <- sort(latRange)
        ## Set scale as the max of the distances along four sides of box
        ## NB. all distance used here are in km.
        l <- geodDist(lonRange[1], latRange[1], lonRange[1], latRange[2])
        r <- geodDist(lonRange[2], latRange[1], lonRange[2], latRange[2])
        b <- geodDist(lonRange[1], latRange[1], lonRange[2], latRange[1])
        t <- geodDist(lonRange[1], latRange[2], lonRange[2], latRange[2])
        oceDebug(debug, "l:", l, ", r:", r, ", b:", b, ", t:", t, "\n")
        span <- max(l, r, b, t)
    }
    C <- 2 * 3.14 * 6.4e3              # circumferance of earth
    oceDebug(debug, "span:", span, ", C:", C, "\n")
    if (span < 500) {
        rval <- "coastlineWorldFine"
    } else if (span < C / 4) {
        rval <- "coastlineWorldMedium"
    } else {
        rval <- "coastlineWorld"
    }
    oceDebug(debug, "}\n", unindent=1)
    return(rval)
}

#' Cut a coastline file at specified longitude
#'
#' @details
#' This can be helpful in preventing \code{\link{mapPlot}} from producing ugly
#' horizontal lines in world maps. These lines occur when a coastline segment
#' is intersected by longitude lon_0+180.  Since the coastline files in the oce
#' and ocedata packages are already "cut" at longitudes of -180 and 180, the present
#' function is not needed for default maps, which have \code{+lon_0=0}. However,
#' may help with other values of \code{lon_0}.
#'
#' This function is provisional. Its behaviour, name and very existence
#' may change through the late months of 2015.  One part of the
#' development plan is to see if there is common ground between this
#' and the \code{clipPolys} function in the \CRANpkg{PBSmapping} package.
#' 
#' @param coastline original coastline object
#' @param lon_0 longitude as would be given in a \code{+lon_0=} item in a proj.4 string
#' 
#' @examples
#' library(oce)
#' data(coastlineWorld)
#' \dontrun{
#' mapPlot(coastlineCut(coastlineWorld, lon_0=100), proj="+proj=robin +lon_0=100", fill='gray')
#' }
#'
#' @return a new coastline object
coastlineCut <- function(coastline, lon_0=0)
{
    if (lon_0 == 0)
        return(coastline)
    cleanAngle <- function(a)
        ifelse(a<(-180), a+360, ifelse(a>180, a-360, a))
    loncut <- cleanAngle(lon_0+180)
    lon <- coastline[["longitude"]]
    lat <- coastline[["latitude"]]
    nlon <- length(lon)
    e <- 4                             # a bit over 2 should be more than enough for any coastline
    cut <- .C("polygon_subdivide_vertically_smash_1",
              n=as.integer(nlon), x=as.double(lon), y=as.double(lat), x0=as.double(loncut),
              nomax=as.integer(e*nlon), no=integer(1), xo=double(e*nlon), yo=double(e*nlon),
              NAOK=TRUE)
    cut$xo <- cut$xo[1:cut$no]
    cut$yo <- cut$yo[1:cut$no]
    warning("coastlineCut() may change name or behaviour through the summer of 2015")
    as.coastline(longitude=cut$xo, latitude=cut$yo)
}
