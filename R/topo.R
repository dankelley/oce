setMethod(f="initialize",
          signature="topo",
          definition=function(.Object,longitude,latitude,z,filename="", units) {
              if (!missing(longitude)) .Object@data$longitude <- longitude
              if (!missing(latitude)) .Object@data$latitude <- latitude
              if (!missing(z)) .Object@data$z <- z
              if (!missing(units)) .Object@metadata$units <- units
              .Object@metadata$filename <- filename
              .Object@processingLog$time <- as.POSIXct(Sys.time())
              .Object@processingLog$value <- "create 'topo' object"
              return(.Object)
          })


setMethod(f="summary",
          signature="topo",
          definition=function(object, ...) {
              digits <- 4
              latRange <- range(object[["latitude"]], na.rm=TRUE)
              lonRange <- range(object[["longitude"]], na.rm=TRUE)
              zRange <- range(object[["z"]], na.rm=TRUE)
              cat("\nTopo dataset\n------------\n")
              cat("* Source:          ", object[["filename"]], "\n")
              cat("* Latitude range:   ", format(latRange[1], digits),
                  " to ", format(latRange[2], digits), ", in steps of ", format(diff(object[["latitude"]][1:2]), digits), " deg\n", sep="")
              cat("* Longitude range:  ", format(lonRange[1], digits),
                  " to ", format(lonRange[2], digits), ", in steps of ", format(diff(object[["longitude"]][1:2]), digits), " deg\n", sep="")
              cat("* Elevation range:  ", format(zRange[1], digits=digits),
                  " to ", format(zRange[2], digits), " m\n", sep="")
              processingLogShow(object)
              invisible(NULL)
          })


setMethod(f="[[",
          signature(x="topo", i="ANY", j="ANY"),
          definition=function(x, i, j, drop) {
              ## 'j' can be for times, as in OCE
              ##if (!missing(j)) cat("j=", j, "*****\n")
              i <- match.arg(i, c("longitude","latitude","z", "filename"))
              if (i == "longitude") return(x@data$longitude)
              else if (i == "latitude") return(x@data$latitude)
              else if (i == "z") return(x@data$z)
              else if (i == "filename") return(x@metadata$filename)
              else return(as(x, "oce")[[i]])
          })


setMethod(f="subset",
          signature="topo",
          definition=function(x, subset, ...) {
              subsetString <- paste(deparse(substitute(subset)), collapse=" ")
              res <- x
              dots <- list(...)
              debug <- getOption("oceDebug")
              if (length(dots) && ("debug" %in% names(dots)))
                  debug <- dots$debug
              if (missing(subset))
                  stop("must give 'subset'")
              if (length(grep("longitude", subsetString))) {
                  oceDebug(debug, "subsetting a topo object by longitude\n")
                  keep <- eval(substitute(subset), x@data, parent.frame(2))
                  oceDebug(debug, "keeping", 100*sum(keep)/length(keep), "% of longitudes\n")
                  res[["longitude"]] <- x[["longitude"]][keep]
                  res[["z"]] <- x[["z"]][keep,]
              } else if (length(grep("latitude", subsetString))) {
                  oceDebug(debug, "subsetting a topo object by latitude\n")
                  keep <- eval(substitute(subset), x@data, parent.frame(2))
                  oceDebug(debug, "keeping", 100*sum(keep)/length(keep), "% of latitudes\n")
                  res[["latitude"]] <- x[["latitude"]][keep]
                  res[["z"]] <- x[["z"]][,keep]
              } else {
                  stop("the subset must be based on longitude or latitude")
              }
              res@processingLog <- processingLogAppend(res@processingLog, paste("subset.topo(x, subset=", subsetString, ")", sep=""))
              res
          })




topoInterpolate <- function(longitude, latitude, topo)
{
    if (missing(longitude)) stop("must supply longitude")
    if (missing(latitude)) stop("must supply latitude")
    if (missing(topo)) stop("must supply topo")
    if (length(latitude) != length(longitude)) stop("lengths of latitude and longitude must match")
    .Call("topo_interpolate", latitude, longitude, topo[["latitude"]], topo[["longitude"]], topo[["z"]])
}


setMethod(f="plot",
          signature=signature("topo"),
          definition=function(x,
                              xlab="", ylab="",
                              asp,
                              clongitude, clatitude, span,
                              ##center, span,
                              expand=1.5,
                              water.z,
                              col.water,
                              lty.water,
                              lwd.water,
                              land.z,
                              col.land,
                              lty.land,
                              lwd.land,
                              geographical=FALSE,
                              location="topright",
                              mgp=getOption("oceMgp"),
                              mar=c(mgp[1]+1,mgp[1]+1,1,1),
                              debug=getOption("oceDebug"),
                              ...)
          {
              if (!inherits(x, "topo"))
                  stop("method is only for objects of class '", "topo", "'")
              oceDebug(debug, "plot.topo() {\n", unindent=1)

              ##opar <- par(no.readonly = TRUE)
              ##on.exit(par(opar))
              par(mgp=mgp, mar=mar)
              dots <- list(...)
              dotsNames <- names(dots)
              if ("center" %in% dotsNames) stop("please use 'clatitude' and 'clongitude' instead of 'center'")
              gave.center <- !missing(clatitude) && !missing(clongitude)

              gave.span <- !missing(span)
              if (gave.center != gave.span) stop("must give all of 'clatitude', 'clongitude' and 'span', or none of them")
              if (!missing(clongitude) && clongitude > 180)
                  clongitude <- clongitude - 360
              if (!missing(clongitude) && clongitude < -180)
                  clongitude <- clongitude + 360
              if (gave.center) {
                  if (!missing(asp))
                      warning("argument 'asp' being ignored, because argument 'center' was given")
                  asp <- 1 / cos(clatitude * atan2(1, 1) / 45) #  ignore any provided asp, because lat from center over-rides it
                  xr <- clongitude + span * c(-1/2, 1/2) / 111.11 / asp
                  yr <- clatitude  + span * c(-1/2, 1/2) / 111.11
                  oceDebug(debug, "gave center; calculated xr=", xr," yr=", yr, " asp=", asp, "\n")
              } else {
                  if (missing(asp)) {
                      if ("ylim" %in% dotsNames)
                          asp <- 1 / cos(mean(range(dots$ylim, na.rm=TRUE)) * pi / 180) # dy/dx
                      else
                          asp <- 1 / cos(mean(range(x[["latitude"]],na.rm=TRUE)) * pi / 180) # dy/dx
                  }
                  ## Expand
                  xr0 <- range(x[["longitude"]], na.rm=TRUE)
                  yr0 <- range(x[["latitude"]], na.rm=TRUE)
                  oceDebug(debug, "xr0=", xr0, "\n")
                  oceDebug(debug, "yr0=", yr0, "\n")
                  if (expand >= 0 && max(abs(xr0)) < 100 && max(abs(yr0) < 70)) { # don't expand if full map
                      xr <- mean(xr0) + expand * diff(xr0) * c(-1/2, 1/2)
                      yr <- mean(yr0) + expand * diff(yr0) * c(-1/2, 1/2)
                  } else {
                      xr <- xr0
                      yr <- yr0
                  }
              }
              zr <- range(x[["z"]], na.rm=TRUE)
              if (gave.center && !is.null(dots$xlim))
                  stop("cannot give 'xlim' argument if the 'center' argument was given")
              if (gave.center && !is.null(dots$ylim))
                  stop("cannot give 'ylim' argument if the 'center' argument was given")
              ## auto-scale based on data in window, if window provided
              if (!is.null(dots$xlim) && !is.null(dots$ylim)) {
                  xr <- dots$xlim
                  yr <- dots$ylim
              }

              ## The following is a somewhat provisional hack, to get around a
              ## tendency of plot() to produce latitudes past the poles.
              ## BUG: the use of par("pin") seems to mess up resizing in aqua windows.
              asp.page <- par("pin")[2] / par("pin")[1] # dy / dx
              oceDebug(debug, "par('pin')=",par('pin'), "asp=",asp,"asp.page=", asp.page, "\n")
              if (asp > asp.page) {               # FIXME: this seems to have x and y mixed up (asp=dy/dx)
                  oceDebug(debug, "type 1 (will narrow x range)\n")
                  d <- asp / asp.page * diff(xr)
                  xr <- mean(xr) + d * c(-1/2, 1/2)
                  oceDebug(debug, "xr narrowed to:", xr, "\n")
                  ## xr[2] <- xr[1] + (xr[2] - xr[1]) * (asp / asp.page)
              } else {
                  oceDebug(debug, "type 2 (will narrow y range)\n")
                  d <- asp / asp.page * diff(yr)
                  yr <- mean(yr) + d * c(-1/2, 1/2)
                  oceDebug(debug, "yr narrowed to:", yr, "\n")
                  ##yr[2] <- yr[1] + (yr[2] - yr[1]) / (asp / asp.page)
              }

              oceDebug(debug, "xr:", xr, "(before trimming)\n")
              oceDebug(debug, "yr:", yr, "(before trimming)\n")
                                        #    if (xr[1] < -180) xr[1] <- -180
                                        #    if (xr[2] >  180) xr[2] <- 180
              if (yr[1] < -90)  yr[1] <- -90
              if (yr[2] >  90)  yr[2] <-  90
              oceDebug(debug, "xr:", xr, "(after trimming)\n")
              oceDebug(debug, "yr:", yr, "(after trimming)\n")

              X <- x[["longitude"]]
              Y <- x[["latitude"]]
              Z <- x[["z"]]
              ## check for prime meridian
              if (sign(prod(xr)) < 0) {
                  Z <- rbind(Z, Z)
                  X <- c(X - 360, X)
              }

              ## Data may not extend across plot region
              ##lon.range <- range(x[["longitude"]], na.rm=TRUE)
              ##lat.range <- range(x[["latitude"]], na.rm=TRUE)
              lon.range <- range(X, na.rm=TRUE)
              lat.range <- range(Y, na.rm=TRUE)
              if (xr[1] < lon.range[1]) xr[1] <- lon.range[1]
              if (xr[2] > lon.range[2]) xr[2] <- lon.range[2]
              if (yr[1] < lat.range[1]) yr[1] <- lat.range[1]
              if (yr[2] > lat.range[2]) yr[2] <- lat.range[2]

              plot(xr, yr, asp=asp, xlab="", ylab="", type="n", xaxs="i", yaxs="i", axes=FALSE, ...)
              if (debug > 0)
                  points(xr, yr, col="blue", pch=20, cex=3)

              xr.pretty <- pretty(xr)
              yr.pretty <- pretty(yr)
              oceDebug(debug, "xr.pretty=", xr.pretty, "(before trimming)\n")
              xr.pretty <- subset(xr.pretty, xr.pretty >= xr[1] & xr.pretty <= xr[2])
              oceDebug(debug, "xr.pretty=", xr.pretty, "(after trimming)\n")
              oceDebug(debug, "yr.pretty=", yr.pretty, "(before trimming)\n")
              yr.pretty <- subset(yr.pretty, yr.pretty >= yr[1] & yr.pretty <= yr[2])
              oceDebug(debug, "yr.pretty=", yr.pretty, "(after trimming)\n")

              lines(c(xr[1], xr[2], xr[2], xr[1], xr[1]), c(yr[1], yr[1], yr[2], yr[2], yr[1])) # axis box
              xlabels <- format(xr.pretty)
              ylabels <- format(yr.pretty)
              if (geographical) {
                  xlabels <- sub("-", "", xlabels)
                  ylabels <- sub("-", "", ylabels)
              }
              axis(1, at=xr.pretty, pos=yr[1], labels=xlabels)
              axis(3, at=xr.pretty, pos=max(yr), labels=FALSE)
              axis(2, at=yr.pretty, pos=xr[1], labels=ylabels)
              axis(4, at=yr.pretty, pos=max(xr), labels=FALSE)
              ## Use either mtext() or text() to position the label, depending on
              ## whether the extra margin space has been placed to the sides
              ## of the graph, or above and below it.
              if (0 != nchar(xlab)) {
                  if (asp > asp.page) {
                      mtext(xlab, side=1, line=mgp[1])
                  } else {
                      text(mean(par('usr')[1:2]), yr[1], xlab, pos=1, offset=mgp[1]+mgp[2])
                  }
              }
              if (0 != nchar(ylab)) {
                  if (asp > asp.page) {
                      text(xr[1], mean(par('usr')[3:4]), ylab, pos=2, offset=mgp[1]+mgp[2], srt=90)
                  } else {
                      mtext(ylab, side=2, line=mgp[1])
                  }
              }

              oceDebug(debug, "xr=", xr, "yr=",yr,"\n")
              ##yaxp <- par("yaxp")
              oceDebug(debug, "par(yaxp)",par("yaxp"),"\n")
              oceDebug(debug, "par(pin)",par("pin"),"\n")

              ## need to clip because contour() does not do so
              xx <- X                            # x[["longitude"]]
              yy <- Y                            # x[["latitude"]]
              xclip <- xx < xr[1] | xr[2] < xx
              yclip <- yy < yr[1] | yr[2] < yy
              xx <- xx[!xclip]
              if (length(xx) < 1)
                  stop("there are no topographic data within the longitudes of the plot region.")
              yy <- yy[!yclip]
              if (length(yy) < 1)
                  stop("there are no topographic data within the latitudes of the plot region.")
              ##zz <- x[["z"]][!xclip, !yclip]
              zz <- Z[!xclip, !yclip]
              zr <- range(zz)

              contour(xx, yy, zz,
                      levels=0, drawlabels=FALSE, add=TRUE,
                      col="black")                # coastline is always black

              legend <- lwd <- lty <- col <- NULL
              if (zr[1] < 0) {
                  if (missing(water.z)) {
                      if (zr[2] > 0) {
                          water.z <- pretty(c(zr[1], 0))
                          water.z <- water.z[water.z!=0]
                                        #cat("water.z=");print(water.z)
                          ## Do some tricks to get shelf water as well as deep
                          if (max(water.z) == -1000)
                              water.z <- c(water.z, -500, -250, -100, -50)
                          else if (max(water.z) == -500)
                              water.z <- c(water.z, -400, -300, -200, -150, -100, -50)
                                        #cat("after tricks, water.z=");print(water.z)
                      } else {
                          water.z <- pretty(zr)
                      }
                      water.z <- sort(water.z)
                  }
                  nz <- length(water.z)
                  if (missing(col.water))
                      col.water <- oce.colorsGebco(nz, "water", "line")
                  if (missing(lty.water))
                      lty.water <- rep(par("lty"), nz)
                  else if (length(lty.water) == 1)
                      lty.water <- rep(lty.water, nz)
                  if (missing(lwd.water))
                      lwd.water <- rep(par("lwd"), nz)
                  else if (length(lwd.water) == 1)
                      lwd.water <- rep(lwd.water, nz)
                  legend <- c(legend, water.z)
                  lwd    <- c(lwd,    lwd.water)
                  lty    <- c(lty,    lty.water)
                  col    <- c(col,    col.water)
                  contour(xx, yy, zz,
                          levels=water.z, lwd=lwd.water, lty=lty.water, col=col.water,
                          drawlabels=FALSE, add=TRUE, ...)
              }
              if (zr[2] > 0) {
                  if (missing(land.z)) {
                      if (zr[1] < 0) {
                          land.z <- pretty(c(0, zr[2]))
                          land.z <- land.z[land.z!=0]
                      } else {
                          land.z <- pretty(zr)
                      }
                  }
                  nz <- length(land.z)
                  if (nz > 0) {
                      if (missing(col.land))
                          col.land <- oce.colorsGebco(nz, "land", "line")
                      if (missing(lty.land))
                          lty.land <- rep(par("lty"), nz)
                      else if (length(lty.land) == 1)
                          lty.land <- rep(lty.land, nz)
                      if (missing(lwd.land))
                          lwd.land <- rep(par("lwd"), nz)
                      else if (length(lwd.land) == 1)
                          lwd.land <- rep(lwd.land, nz)
                      legend <- c(legend, land.z)
                      lwd    <- c(lwd,    lwd.land)
                      lty    <- c(lty,    lty.land)
                      col    <- c(col,    col.land)
                      contour(xx, yy, zz,
                              levels=land.z, lwd=lwd.land, lty=lty.land, col=col.land,
                              drawlabels=FALSE, add=TRUE, ...)
                  }
              }
              if (!is.null(location) && location != "none") {
                  o <- rev(order(legend))
                  legend(location, lwd=lwd[o], lty=lty[o], bg="white", legend=legend[o], col=col[o])
              }
              oceDebug(debug, "} # plot.topo()\n", unindent=1)
              invisible()
          })

read.topo <- function(file, ...)
{
    ## handle GEBCO netcdf files or an ascii format
    if (is.character(file) && length(grep(".nc$", file))) {
        if (!requireNamespace("ncdf4", quietly=TRUE)) {
            stop('must install.packages("ncdf4") to read topo data from a netCDF file')
        } else {
            ## GEBCO netcdf
            ## NOTE: need to name ncdf4 package because otherwise R checks give warnings.
            ncdf <- ncdf4::nc_open(file)
            xrange <- ncdf4::ncvar_get(ncdf, "x_range")
            yrange <- ncdf4::ncvar_get(ncdf, "y_range")
            ##zrange <- ncdf4::ncvar_get(ncdf, "z_range")
            spacing <- ncdf4::ncvar_get(ncdf, "spacing")
            longitude <- seq(xrange[1], xrange[2], by=spacing[1])
            latitude <- seq(yrange[1], yrange[2], by=spacing[2])
            z <- ncdf4::ncvar_get(ncdf, "z")
            dim <- ncdf4::ncvar_get(ncdf, "dimension")
            z <- t(matrix(z, nrow=dim[2], ncol=dim[1], byrow=TRUE))
            z <- z[,dim[2]:1]
            res <- as.topo(longitude, latitude, z, filename=file)
        }
    } else {
        ## ASCII
        ## NOTE: on 2014-11-13 it came to light that the old dataset website 
        ##          http://www.ngdc.noaa.gov/mgg/gdas/gd_designagrid.html 
        ## was no longer working, and that the new one
        ##          http://maps.ngdc.noaa.gov/viewers/wcs-client/ 
        ## seemed to have headers 5 lines long.  However,
        ## the code below has a trick to (perhaps) auto-detect whether the header
        ## length is 5 or 6.
        nh <- 6
        header <- readLines(file, n=nh)
        if (nchar(header[length(header)]) > 50) {
            ## the header is only 5 long, if the last header line is long.
            nh <- nh - 1
            header <- header[1:nh]
        }
        ncol <- as.numeric(strsplit(header[1],"[ ]+",perl=TRUE)[[1]][2])
        nrow <- as.numeric(strsplit(header[2],"[ ]+",perl=TRUE)[[1]][2])
        longitudeLowerLeft <- as.numeric(strsplit(header[3],"[ ]+",perl=TRUE)[[1]][2])
        latitudeLowerLeft <- as.numeric(strsplit(header[4],"[ ]+",perl=TRUE)[[1]][2])
        cellSize <- as.numeric(strsplit(header[5],"[ ]+",perl=TRUE)[[1]][2])
        missingValue <- NA
        if (length(i <- grep("nodata", header)))
            missingValue <- as.numeric(strsplit(header[i],"[ ]+",perl=TRUE)[[1]][2])
        zz <- as.matrix(read.table(file, header=FALSE, skip=nh), byrow=TRUE)
        rownames(zz) <- NULL
        colnames(zz) <- NULL
        longitude <- longitudeLowerLeft + cellSize * seq(0, ncol-1)
        latitude <- latitudeLowerLeft + cellSize * seq(0, nrow-1)
        z <- t(zz[dim(zz)[1]:1,])
        if (!is.na(missingValue))
            z[z == missingValue] <- NA
        res <- as.topo(longitude, latitude, z, filename=file) # FIXME: add units here
    }
    res@processingLog <- processingLogAppend(res@processingLog,
                                              paste(deparse(match.call()), sep="", collapse=""))
    res
}

as.topo <- function(longitude, latitude, z, units=NULL, filename="")
{
    if (inherits(longitude, "bathy")) {
        bathy <- longitude
        longitude <- as.numeric(rownames(bathy))
        latitude <- as.numeric(colnames(bathy))
        z <- as.matrix(bathy)
        if ("units" %in% names(bathy@metadata))
            units <- bathy@metadata$units
    }
    ncols <- length(longitude)
    nrows <- length(latitude)
    ## longitudeLowerLeft <- min(longitude, na.rm=TRUE)
    ## latitudeLowerLeft <- min(latitude, na.rm=TRUE)
    dim <- dim(z)
    if (dim[1] != ncols)
        stop("longitude vector has length ", ncols, ", which does not match matrix width ", dim[1])
    if (dim[2] != nrows)
        stop("latitude vector has length ", ncols, ", which does not match matrix height ", dim[2])
    res <- new("topo", latitude=latitude, longitude=longitude, z=z, filename=filename, units=units)
    res@processingLog <- processingLogAppend(res@processingLog,
                                              paste(deparse(match.call()), sep="", collapse=""))
    res
}

