setMethod(f="initialize",
          signature="section",
          definition=function(.Object) {
              .Object@processingLog$time <- as.POSIXct(Sys.time())
              .Object@processingLog$value <- "create 'section' object"
              return(.Object)
          })

setMethod(f="[[",
          signature="section",
          definition=function(x, i, j, drop) {
              if (i %in% names(x@metadata)) {
                  if (i %in% c("latitude", "longitude")) {
                      if (!missing(j) && "byStation" == j) {
                          return(x@metadata[[i]])
                      } else {
                          rval <- NULL
                          for (stn in seq_along(x@data$station))
                              rval <- c(rval, rep(x@data$station[[stn]]@metadata[[i]], length(x@data$station[[stn]]@data$salinity)))
                          return(rval)
                      }
                  } else {
                      return(x@metadata[[i]])
                  }
              } else if (i %in% names(x@data$station[[1]]@data)) {
                  rval <- NULL
                  for (stn in seq_along(x@data$station))
                      rval <- c(rval, x@data$station[[stn]]@data[[i]])
              } else if ("station" == i) {
                  if (missing(j)) {
                      rval <- x@data$station
                  } else {
                      nj <- length(j)
                      if (nj == 1) {
                          rval <- x@data$station[[j]]
                      } else {
                          rval <- vector("list", nj)
                          for (jj in j)
                              rval[[jj]] <- x@data$station[[jj]]
                      }
                  }
              } else if ("station ID" == i) {
                  rval <- NULL
                  for (stn in x[['station']])
                      rval <- c(rval, stn[['station']])
              } else if ("dynamic height" == i) {
                  rval <- swDynamicHeight(x)
              } else {
                  stop("cannot access item named \"", i, "\" in this section object")
              }
              rval
          })

setMethod(f="show",
          signature="section",
          definition=function(object) {
              id <- object@metadata$sectionId
              if (id == "")
                  cat("Section has stations:\n", sep="")
              else
                  cat("Section named '", id, "' has stations:\n", sep="")
              for (i in seq_along(object@data$station)) {
                  thisStn <- object@data$station[[i]]
                  cat("    ")
                  if (!is.null(thisStn@metadata$station) && "" != thisStn@metadata$station)
                      cat(thisStn@metadata$station, " ")
                  cat(sprintf("%.5f N   %.5f E   %.0f m", object@data$station[[i]]@metadata$latitude,
                              object@data$station[[i]]@metadata$longitude,
                              object@data$station[[i]]@metadata$waterDepth))
                  cat("\n")
              }
          })

sectionSort <- function(section, by=c("stationId", "distance"))
{
    by <- match.arg(by)
    rval <- section
    if (by == "stationId") {
	o <- order(section@metadata$stationId)
	rval@metadata$stationId <- rval@metadata$stationId[o]
	rval@metadata$latitude <- rval@metadata$latitude[o]
	rval@metadata$longitude <- rval@metadata$longitude[o]
	rval@data$station <- rval@data$station[o]
    } else if (by == "distance") {
	warning("sort.section() cannot yet handle argument by=\"distance\"")
    } else {
	stop("argument 'by' is incorrect")
    }
    rval@processingLog <- processingLog(rval@processingLog, paste(deparse(match.call()), sep="", collapse=""))
    rval
}

makeSection <- function(item, ...)
{
    if (inherits(item, "ctd")) {
	extra.args <- list(...)
	numStations <- 1 + length(extra.args)
	station <- vector("list", numStations)
	stn <- vector("character", numStations)
	lon <- vector("numeric", numStations)
	lat <- vector("numeric", numStations)
	stn[1] <- item@metadata$station
	lat[1] <- item@metadata$latitude
	lon[1] <- item@metadata$longitude
	station[[1]] <- item
	if (numStations > 1) {
	    for (i in 2:numStations) {
                ##cat("adding station i=", i, "\n")
                thisStn <- extra.args[[i-1]]
		stn[i] <- thisStn@metadata$station
		lat[i] <- thisStn@metadata$latitude
		lon[i] <- thisStn@metadata$longitude
		station[[i]] <- thisStn
	    }
	}
    } else if (inherits(item, "list")) {
	numStations <- length(item)
	station <- vector("list", numStations)
	stn <- vector("character", numStations)
	lon <- vector("numeric", numStations)
	lat <- vector("numeric", numStations)
	if (numStations > 1) {
	    for (i in 1:numStations) {
                thisItem <- item[[i]]
		stn[i] <- thisItem@metadata$station
		lat[i] <- thisItem@metadata$latitude
		lon[i] <- thisItem@metadata$longitude
		station[[i]] <- thisItem
	    }
	} else {
	    stop("need more than 1 station to make a section")
	}
    } else if (class(item) == "character") {
	numStations <- length(item)
	station <- vector("list", numStations)
	stn <- vector("character", numStations)
	lon <- vector("numeric", numStations)
	lat <- vector("numeric", numStations)
	if (numStations <= 1)
	    stop("need more than 1 station to make a section")
	if (exists(item[1])) {
	    ## ctd objects
	    ##oceDebug(1, "ctd objects\n")
	    for (i in 1:numStations) {
                thisItem <- get(item[[i]])
		stn[i] <- thisItem@metadata$station
		lat[i] <- thisItem@metadata$latitude
		lon[i] <- thisItem@metadata$longitude
		station[[i]] <- thisItem
	    }
	} else {
	    ## ctd filenames
	    ##oceDebug(1, "ctd files\n")
	    for (i in 1:numStations) {
		##oceDebug(1, "file named", item[i], "\n")
		ctd <- read.ctd(item[i])
		stn[i] <- ctd@metadata$station
		lat[i] <- ctd@metadata$latitude
		lon[i] <- ctd@metadata$longitude
		station[[i]] <- ctd
	    }
	}
    } else {
	stop("first argument must be of a \"ctd\" object, a \"list\" of ctd objects, or a vector of character strings naming ctd objects")
    }
    res <- new("section")
    res@metadata <- list(sectionId="", stationId=stn, latitude=lat, longitude=lon)
    res@data <- list(station=station)
    res@processingLog <- processingLog(res@processingLog, paste(deparse(match.call()), sep="", collapse=""))
    res
}

"+.section" <- function(section, station)
{
    if (missing(station)) return(section) # not sure this can happen
    if (!inherits(section, "section"))
        stop("'section' is not a section")
    if (!inherits(station, "ctd"))
        stop("'station' is not a station")
    res <- section
    n.orig <- length(section@data$station)
    s <- vector("list", n.orig + 1)
    for (i in 1:n.orig)
	s[[i]] <- section@data$station[[i]]
    s[[n.orig + 1]] <- station
    res@data$station <- s
    res@metadata$latitude <- c(res@metadata$latitude, station@metadata$latitude)
    res@metadata$longitude <- c(res@metadata$longitude, station@metadata$longitude)
    res@metadata$stationId <- c(res@metadata$stationId, station@metadata$station)
    res@processingLog <- processingLog(res@processingLog, paste(deparse(match.call()), sep="", collapse=""))
    res
}

setMethod(f="plot",
          signature=signature("section"),
          definition=function(x,
                              which=c("salinity", "temperature", "sigmaTheta", "map"),
                              eos=getOption("eos", default='unesco'),
                              at=NULL,
                              labels=TRUE,
                              grid = FALSE,
                              contourLevels=NULL,
                              contourLabels=NULL,
                              stationIndices,
                              coastline="coastlineWorld",
                              xlim=NULL, ylim=NULL,
                              map.xlim=NULL, map.ylim=NULL,
                              xtype="distance",
                              ytype="depth",
                              legend.loc="bottomright",
                              adorn=NULL,
                              showStations=FALSE,
                              showBottom=TRUE,
                              mgp=getOption("oceMgp"),
                              mar=c(mgp[1]+1, mgp[1]+1, mgp[2], mgp[2]+0.5),
                              col=par("col"), cex=par("cex"), pch=par("pch"),
                              debug=getOption("oceDebug"),
                              ...)
          {
              debug <- if (debug > 2) 2 else floor(0.5 + debug)
              oceDebug(debug, "\bplot.section(..., which=c(", paste(which, collapse=","), "), eos=\"", eos, "\", ...) {\n")
              plotSubsection <- function(variable="temperature", vtitle="T",
                                         eos=getOption("eos", default='unesco'),
                                         indicate.stations=TRUE, contourLevels=NULL, contourLabels=NULL,
                                         xlim=NULL,
                                         ylim=NULL,
                                         legend=TRUE,
                                         debug=0,
                                         col=par("col"),
                                         ...)
              {
                  oceDebug(debug, "\bplotSubsection(variable=", variable, ", eos=\"", eos, "\", ...) {\n")
                  if (variable == "map") {
                      lat <- array(NA, numStations)
                      lon <- array(NA, numStations)
                      for (i in 1:numStations) {
                          thisStation <- x[["station", stationIndices[i]]]
                          lat[i] <- thisStation[["latitude"]]
                          lon[i] <- thisStation[["longitude"]]
                      }
                      lon[lon<0] <- lon[lon<0] + 360
                      asp <- 1 / cos(mean(range(lat,na.rm=TRUE))*pi/180)
                      latm <- mean(lat, na.rm=TRUE)
                      lonm <- mean(lon, na.rm=TRUE)
                      lonr <- lonm + sqrt(2) * (range(lon, na.rm=TRUE) - mean(lon, na.rm=TRUE)) # expand range
                      latr <- latm + sqrt(2) * (range(lat, na.rm=TRUE) - mean(lat, na.rm=TRUE))
                      if (!is.null(map.xlim)) {
                          map.xlim <- sort(map.xlim)
                          plot(lonr, latr, xlim=map.xlim, asp=asp, type='n', xlab="Longitude", ylab="Latitude")
                      } else if (!is.null(map.ylim)) {
                          map.ylim <- sort(map.ylim)
                          plot(lonr, latr, ylim=map.ylim, asp=asp, type='n', xlab="Longitude", ylab="Latitude")
                      } else {
                          plot(lonr, latr, asp=asp, type='n', xlab="Longitude", ylab="Latitude")
                      }
                      if (!is.null(coastline)) {
                          if (is.character(coastline)) {
                              if (coastline == "none") {
                                  next
                              } else { # named coastline
                                  if (!exists(paste("^", coastline, "$", sep=""))) { # load it, if necessary
                                      oceDebug(debug, " loading coastline file \"", coastline, "\"\n", sep="")
                                      if (coastline == "coastlineWorld") {
                                          data(coastlineWorld)
                                          coastline <- coastlineWorld
                                      } else if (coastline == "coastlineMaritimes") {
                                          data(coastlineMaritimes)
                                          coastline <- coastlineMaritimes
                                      } else if (coastline == "coastlineHalifax") {
                                          data(coastlineHalifax)
                                          coastline <- coastlineHalifax
                                      } else if (coastline == "coastlineSLE") {
                                          data(coastlineSLE)
                                          coastline <- coastlineSLE
                                      } else {
                                          stop("there is no built-in coastline file of name \"", coastline, "\"")
                                      }
                                  }
                              }
                          }
                          if (!is.null(coastline@metadata$fillable) && coastline@metadata$fillable) {
                              polygon(coastline[["longitude"]], coastline[["latitude"]], col="lightgray", lwd=3/4)
                              polygon(coastline[["longitude"]]+360, coastline[["latitude"]], col="lightgray", lwd=3/4)
                          } else {
                              lines(coastline[["longitude"]], coastline[["latitude"]], col="darkgray")
                              lines(coastline[["longitude"]]+360, coastline[["latitude"]], col="darkgray")
                          }
                      }
                      lines(lon, lat, col="lightgray")
                      ## FIXME: possibly should figure out the offset, instead of just replotting shifted lon
                      col <- if("col" %in% names(list(...))) list(...)$col else "black"
                      points(lon, lat, col=col, pch=3, lwd=1/2)
                      points(lon - 360, lat, col=col, pch=3, lwd=1/2)
                      if (showStations) {
                          stationId <- x[['station ID']]
                          text(lon, lat, stationId, pos=2)
                          text(lon-360, lat, stationId, pos=2)
                      }
                      if (xtype == "distance") {
                          points(lon[1], lat[1], col=col, pch=22, cex=3*par("cex"), lwd=1/2)
                          points(lon[1] - 360, col=col, lat[1], pch=22, cex=3*par("cex"), lwd=1/2)
                      }
                      if (indicate.stations) {
                          dx <- 5 * mean(diff(sort(x@metadata$longitude)),na.rm=TRUE)
                          dy <- 5 * mean(diff(sort(x@metadata$latitude)),na.rm=TRUE)
                          xlab <- x@metadata$longitude[1] - dx * sign(x@metadata$longitude[2] - x@metadata$longitude[1])
                          ylab <- x@metadata$latitude[1]  - dy * sign(x@metadata$latitude[2]  - x@metadata$latitude[1])
                          text(xlab, ylab, x@metadata$stationId[1])
                          xlab <- x@metadata$longitude[numStations] -
                          dx * sign(x@metadata$longitude[numStations-1] - x@metadata$longitude[numStations])
                          ylab <- x@metadata$latitude[numStations]  -
                          dy * sign(x@metadata$latitude[numStations-1]  - x@metadata$latitude[numStations])
                          text(xlab, ylab, x@metadata$stationId[numStations])
                      }
                 } else {                        # not a map
                      if (!(variable %in% names(x@data$station[[1]]@data)) && variable != "salinity gradient" && variable != "data") {
                          stop("this section does not contain a variable named '", variable, "'")
                      }
                      ## FIXME: contours don't get to plot edges
                      xxrange <- range(xx, na.rm=TRUE)
                      yyrange <- range(yy, na.rm=TRUE)
                      ##yyrange[1] <- -1

                      ylim <- if (!is.null(ylim)) sort(-abs(ylim)) else yyrange
                      par(xaxs="i", yaxs="i")
                      ylab <- if ("ylab" %in% names(list(...))) list(...)$ylab else { if (which.ytype==1) resizableLabel("p") else "Depth [m]" }

                      if (is.null(at)) {
                          plot(xxrange, yyrange,
                               xaxs="i", yaxs="i",
                               xlim=xlim,
                               ylim=ylim,
                               col="white",
                               xlab=switch(which.xtype, "Distance [ km ]", "Along-track Distance [km]", "Latitude", "Longitude"),
                               ylab=ylab,
                               axes=FALSE)
                          axis(4, labels=FALSE)
                          ytics <- axis(2, labels=FALSE)
                          axis(2, at=ytics, labels=-ytics)
                          axis(1)
                          box()
                      } else {
                          plot(xxrange, yyrange,
                               xaxs="i", yaxs="i",
                               ##                     ylim=rev(yyrange),
                               xlim=xlim, ylim=ylim,
                               col="white",
                               xlab="", ylab=ylab, axes=FALSE)
                          axis(1, at=at, labels=labels)
                          axis(2)
                          axis(4, labels=FALSE)
                          box()
                      }
                      ## Bottom trace
                      usr <- par("usr")
                      graph.bottom <- usr[3]
                      waterDepth <- NULL
                      for (i in 1:numStations) {
                          if (variable == "salinity gradient") {
                              dSdp <- rev(diff(x@data$station[[stationIndices[i]]]@data[["salinity"]]) 
                                          / diff(x@data$station[[stationIndices[i]]]@data[["pressure"]]))
                              zz[i,] <- -c(dSdp[1], dSdp) # repeat first, to make up length
                          } else if (variable != "data") {
                              if (eos == "teos") {
                                  if (variable == "salinity") {
                                      zz[i,] <- rev(swAbsoluteSalinity(x@data$station[[stationIndices[i]]]))
                                  } else if (variable == "temperature") {
                                      zz[i,] <- rev(swConservativeTemperature(x@data$station[[stationIndices[i]]]))
                                  } else {
                                      zz[i,] <- rev(x@data$station[[stationIndices[i]]]@data[[variable]])
                                  }
                              } else {
                                  zz[i,] <- rev(x@data$station[[stationIndices[i]]]@data[[variable]])
                              }
                          }
                          if (grid) points(rep(xx[i], length(yy)), yy, col="gray", pch=20, cex=1/3)
                          temp <- x@data$station[[stationIndices[i]]]@data$temperature
                          len <- length(temp)
                          if (is.finite(x@data$station[[stationIndices[i]]]@metadata$waterDepth)) {
                              wd <- x@data$station[[stationIndices[i]]]@metadata$waterDepth
                              oceDebug(debug, "known waterDepth", wd, "for station i=", i, "\n")
                          } else {
                              wd <- NA
                              if (is.na(temp[len])) {
                                  wdi <- len - which(!is.na(rev(temp)))[1] + 1
                                  wd <- max(x@data$station[[stationIndices[i]]]@data$pressure, na.rm=TRUE)
                                  oceDebug(debug, "inferred waterDepth", wd, "for station i=", i, "\n")
                              } else {
                                  oceDebug(debug, "cannot infer waterDepth for station i=", i, "\n")
                              }
                          }
                          in.land <- which(is.na(x@data$station[[stationIndices[i]]]@data$temperature[-3])) # skip first 3 points
                          if (!is.na(wd)) {
                              waterDepth <- c(waterDepth, wd)
                          } else {
                              waterDepth <- c(waterDepth, max(x@data$station[[stationIndices[i]]]@data$pressure, na.rm=TRUE))
                          }
                      }

                      oceDebug(debug, "waterDepth=c(", paste(waterDepth, collapse=","), ")\n")
                      ##waterDepth <- -waterDepth
                      if (!grid)
                          Axis(side=3, at=xx, labels=FALSE, tcl=-1/3, lwd=0.5) # station locations
                      bottom.x <- c(xx[1], xx, xx[length(xx)])
                      bottom.y <- c(graph.bottom, -waterDepth, graph.bottom)
                      ##cat("bottom.x: (length", length(bottom.x),")");print(bottom.x)
                      ##cat("bottom.y: (length", length(bottom.y),")");print(bottom.y)

                      dots <- list(...) # adjust plot parameter labcex, unless user did

                      ##par(xaxs="i", yaxs="i")


                      ## Put x in order, if it's not already
                      ox <- order(xx)
                      xxOrig <- xx
                      if (any(xx[ox] != xx)) {
                          xx <- xx[ox]
                          zz <- zz[ox,] ## FIXME keep this???
                          ##warning("plot.section() reordered the stations to make x monotonic")
                          bottom.x <- c(min(xxOrig), xxOrig[ox], max(xxOrig))
                          bottom.y <- c(graph.bottom, -waterDepth[ox], graph.bottom)
                      }

                      ## cannot contour with duplicates in x or y; the former is the only problem
                      xx.unique <- c(TRUE, 0 != diff(xx))
                      yy.unique <- c(TRUE, 0 != diff(yy))
                      if (variable == "data") {
                          for (i in 1:numStations) {
                              thisStation <- x[["station", i]]
                              pressure <- thisStation[["pressure"]]
                              if (which.xtype == 4) {
                                  longitude <- thisStation[["longitude"]]
                                  points(rep(longitude, length(pressure)), -pressure, cex=cex, pch=pch, col=col)
                              } else {
                                  ## FIXME: shouldn't the next line work for all types??
                                  points(rep(xx[i], length(pressure)), -pressure, cex=cex, pch=pch, col=col)
                              }
                          }
                      } else {
                          if (!is.null(contourLevels) && !is.null(contourLabels)) {
                              oceDebug(debug, "user-supplied contourLevels: ", contourLevels, "\n")
                              if (!("labcex" %in% dots$labcex)) {
                                  contour(x=xx[xx.unique], y=yy[yy.unique], z=zz[xx.unique,yy.unique],
                                          axes=FALSE, add=TRUE, labcex=0.8,
                                          levels=contourLevels, labels=contourLabels,
                                          xaxs="i", yaxs="i",
                                          ...)
                              } else {
                                  contour(x=xx[xx.unique], y=yy[yy.unique], z=zz[xx.unique,yy.unique],
                                          axes=FALSE, add=TRUE,
                                          levels=contourLevels, labels=contourLabels,
                                          xaxs="i", yaxs="i",
                                          ...)
                              }
                          } else {
                              oceDebug(debug, "automatically-calculated contourLevels\n")
                              if (is.null(dots$labcex)) {
                                  contour(x=xx[xx.unique], y=yy[yy.unique], z=zz[xx.unique,yy.unique],
                                          axes=FALSE, labcex=0.8,
                                          add=TRUE,
                                          xaxs="i", yaxs="i",
                                          ...)
                              } else {
                                  contour(x=xx[xx.unique], y=yy[yy.unique], z=zz[xx.unique,yy.unique],
                                          axes=FALSE,
                                          add=TRUE,
                                          xaxs="i", yaxs="i",
                                          ...)
                              }
                          }
                      }
                      if (is.character(showBottom) || showBottom) {
                          type <- "polygon"
                          if (is.character(showBottom)) {
                              type <- showBottom
                          }
                          if (length(bottom.x) == length(bottom.y)) {
                              bottom <- par('usr')[3]
                              if (type == "polygon") {
                                  polygon(bottom.x, bottom.y, col="lightgray")
                              } else if (type == "lines") {
                                  for (s in seq_along(bottom.x))
                                      lines(rep(bottom.x[s], 2), c(bottom.y[s], bottom), col="lightgray")
                               } else if (type == "points") {
                                  for (s in seq_along(bottom.x))
                                      points(rep(bottom.x[s], 2), c(bottom.y[s], bottom), col="lightgray")
                              }
                          }
                          box()
                      }
                      ##axis(1, pretty(xxOrig))
                      axis(1)
                      ##lines(xx, -waterDepth[ox], col='red')
                      if (legend)
                          legend(legend.loc, legend=vtitle, bg="white", x.intersp=0, y.intersp=0.5,cex=1)
                  }
                  oceDebug(debug, "\b} # plotSubsection()\n")
              }                                   # plotSubsection
              if (!inherits(x, "section"))
                  stop("method is only for section objects")
              opar <- par(no.readonly = TRUE)
              if (length(which) > 1) on.exit(par(opar))
              which.xtype <- pmatch(xtype, c("distance", "track", "latitude", "longitude"), nomatch=0)
              if (0 == which.xtype)
                  stop('xtype must be one of: "distance", "track", "latitude", or "longitude"')
              xtype <- c("distance", "track", "latitude", "longitude")[which.xtype]
              which.ytype <- pmatch(ytype, c("pressure", "depth"), nomatch=0)
              if (missing(stationIndices)) {
                  numStations <- length(x@data$station)
                  stationIndices <- 1:numStations
              } else {
                  numStations <- length(stationIndices)
              }
              if (numStations < 2)
                  stop("cannot plot a section containing fewer than 2 stations")

              firstStation <- x@data$station[[stationIndices[1]]]
              num.depths <- length(firstStation@data$pressure)

              ## Check that pressures coincide
              if (length(which) > 1 || which != "data") {
                  p1 <- firstStation@data$pressure
                  for (ix in 2:numStations) {
                      thisStation <- x@data$station[[stationIndices[ix]]]
                      if (any(p1 != x@data$station[[stationIndices[ix]]]@data$pressure))
                          stop("This section has stations with different pressure levels.\n  Please use e.g.\n\tsectionGridded <- sectionGrid(section)\n  to create a uniform grid, and then you'll be able to plot the section.")
                  }
              }
              zz <- matrix(nrow=numStations, ncol=num.depths)
              xx <- array(NA, numStations)
              yy <- array(NA, num.depths)
              if (is.null(at)) {
                  lat0 <- firstStation@metadata$latitude
                  lon0 <- firstStation@metadata$longitude
                  for (ix in 1:numStations) {
                      j <- stationIndices[ix]
                      if (which.xtype == 1) {
                          xx[ix] <- geodDist(lat0, lon0, x@data$station[[j]]@metadata$latitude, x@data$station[[j]]@metadata$longitude)
                      } else if (which.xtype == 2) {
                          if (ix == 1) {
                              xx[ix] <- 0
                          } else {
                              xx[ix] <- xx[ix-1] + geodDist(x@data$station[[stationIndices[ix-1]]]@metadata$latitude,
                                                            x@data$station[[stationIndices[ix-1]]]@metadata$longitude,
                                                            x@data$station[[j]]@metadata$latitude,
                                                            x@data$station[[j]]@metadata$longitude)
                          }
                      } else if (which.xtype == 3) {
                          xx[ix] <- x@data$station[[j]]@metadata$latitude
                      } else if (which.xtype == 4) {
                          xx[ix] <- x@data$station[[j]]@metadata$longitude
                      } else {
                          stop('unkown xtype; it must be one of: "distance", "track", "latitude", or "longitude"')
                      }
                  }
              } else {
                  xx <- at
              }

              if (which.ytype == 1) yy <- rev(-x@data$station[[stationIndices[1]]]@data$pressure)
              else if (which.ytype == 2) yy <- rev(-swDepth(x@data$station[[stationIndices[1]]]@data$pressure))
              else stop("unknown ytype")

              oceDebug(debug, "before nickname-substitution, which=c(", paste(which, collapse=","), ")\n")
              lw <- length(which)
              which2 <- vector("numeric", lw)
              for (w in 1:lw) {
                  ww <- which[w]
                  if (is.numeric(ww)) {
                      which2[w] <- ww
                  } else {
                      if (     ww == "temperature") which2[w] <- 1
                      else if (ww == "salinity") which2[w] <- 2
                      else if (ww == "salinity gradient") which2[w] <- 2.5
                      else if (ww == "sigmaTheta") which2[w] <- 3
                      else if (ww == "nitrate") which2[w] <- 4
                      else if (ww == "nitrite") which2[w] <- 5
                      else if (ww == "oxygen") which2[w] <- 6
                      else if (ww == "phosphate") which2[w] <- 7
                      else if (ww == "silicate") which2[w] <- 8
                      else if (ww == "data") which2[w] <- 20
                      else if (ww == "map") which2[w] <- 99
                      else stop("unknown 'which':", ww)
                  }
              }
              which <- which2
              oceDebug(debug, "after nickname-substitution, which=c(", paste(which, collapse=","), ")\n")
              par(mgp=mgp, mar=mar)
              if (lw > 1) {
                  if (lw > 2)
                      layout(matrix(1:4, nrow=2, byrow=TRUE))
                  else
                      layout(matrix(1:2, nrow=2, byrow=TRUE))
              }
              adorn.length <- length(adorn)
              if (adorn.length == 1) {
                  adorn <- rep(adorn, lw)
                  adorn.length <- lw
              }
              for (w in 1:length(which)) {
                  oceDebug(debug, "w=", w, "\n")
                  if (!missing(contourLevels)) {
                      if (missing(contourLabels))
                          contourLabels <- format(contourLevels)
                      if (which[w] == 1)
                          plotSubsection("temperature", if (eos == "unesco") "T" else expression(Theta), eos=eos, levels=contourLevels, labels=contourLabels,  xlim=xlim, ylim=ylim, debug=debug-1, ...)
                      if (which[w] == 2)
                          plotSubsection("salinity",    if (eos == "unesco") "S" else expression(S[A]), eos=eos, ylab="", levels=contourLevels, labels=contourLabels,  xlim=xlim, ylim=ylim, debug=debug-1, ...)
                      if (which[w] > 2 && which[w] < 3)
                          plotSubsection("salinity gradient","dS/dz", ylab="", levels=contourLevels, xlim=xlim, ylim=ylim, debug=debug-1, ...)
                      if (which[w] == 3)
                          plotSubsection("sigmaTheta",  expression(sigma[theta]), levels=contourLevels, labels=contourLabels, xlim=xlim, ylim=ylim, debug=debug-1, ...)
                      if (which[w] == 4)
                          plotSubsection("nitrate",     "nitrate", levels=contourLevels, labels=contourLabels,  xlim=xlim, ylim=ylim, debug=debug-1, ...)
                      if (which[w] == 5)
                          plotSubsection("nitrite",     "nitrite", levels=contourLevels, labels=contourLabels,  xlim=xlim, ylim=ylim, debug=debug-1, ...)
                      if (which[w] == 6)
                          plotSubsection("oxygen",      "oxygen", levels=contourLevels, labels=contourLabels,  xlim=xlim, ylim=ylim, debug=debug-1, ...)
                      if (which[w] == 7)
                          plotSubsection("phosphate",   "phosphate", levels=contourLevels, labels=contourLabels,  xlim=xlim, ylim=ylim, debug=debug-1, ...)
                      if (which[w] == 8)
                          plotSubsection("silicate",    "silicate", levels=contourLevels, labels=contourLabels,  xlim=xlim, ylim=ylim, debug=debug-1, ...)
                  } else {
                      if (which[w] == 1)
                          plotSubsection("temperature", if (eos == "unesco") "T" else expression(Theta), eos=eos, xlim=xlim, ylim=ylim, debug=debug-1, ...)
                      if (which[w] == 2)
                          plotSubsection("salinity",    if (eos == "unesco") "S" else expression(S[A]), eos=eos, ylab="", xlim=xlim, ylim=ylim, debug=debug-1, ...)
                      if (which[w] > 2 && which[w] < 3)
                          plotSubsection("salinity gradient","dS/dz", ylab="", xlim=xlim, ylim=ylim, debug=debug-1, ...)
                      if (which[w] == 3)
                          plotSubsection("sigmaTheta", expression(sigma[theta]), xlim=xlim, ylim=ylim, debug=debug-1, ...)
                      if (which[w] == 4)
                          plotSubsection("nitrate",     "nitrate", xlim=xlim, ylim=ylim, debug=debug-1, ...)
                      if (which[w] == 5)
                          plotSubsection("nitrite",     "nitrite", xlim=xlim, ylim=ylim, debug=debug-1, ...)
                      if (which[w] == 6)
                          plotSubsection("oxygen",      "oxygen", xlim=xlim, ylim=ylim, debug=debug-1, ...)
                      if (which[w] == 7)
                          plotSubsection("phosphate",   "phosphate", xlim=xlim, ylim=ylim, debug=debug-1, ...)
                      if (which[w] == 8)
                          plotSubsection("silicate",    "silicate", xlim=xlim, ylim=ylim, debug=debug-1, ...)
                  }
                  if (which[w] == 20)
                      plotSubsection("data", "", xlim=xlim, ylim=ylim, col=col, debug=debug-1, legend=FALSE, ...)
                  if (which[w] == 99)
                      plotSubsection("map", indicate.stations=FALSE, debug=debug-1, ...)
                  if (w <= adorn.length) {
                      t <- try(eval(adorn[w]), silent=TRUE)
                      if (class(t) == "try-error") warning("cannot evaluate adorn[", w, "]\n")
                  }
              }
              oceDebug(debug, "\b} # plot.section()\n")
              invisible()
          })

read.section <- function(file, directory, sectionId="", flags,
			 ship="", scientist="", institute="",
                         missingValue=-999,
			 debug=getOption("oceDebug"), processingLog)
{
    if (!missing(directory)) {
        if (!missing(file))
            stop("cannot specify both 'file' and 'directory'")
        files <- list.files(directory)
        nstations <- length(files)
        stations <- vector("list", nstations)
        for (i in seq_along(files)) {
            name <- paste(directory, files[i], sep='/')
            stations[[i]] <- ctdTrim(read.oce(name))
        }
        return(makeSection(stations))
    }
    if (is.character(file)) {
	filename <- file
	file <- file(file, "r")
	on.exit(close(file))
    }
    if (!inherits(file, "connection")) {
	stop("argument `file' must be a character string or connection")
    }
    if (!isOpen(file)) {
	filename <- "(connection)"
	open(file, "r")
	on.exit(close(file))
    }
    ## flag=2 for good data [WOCE]
    if (missing(flags))
	flags <- c(2)
    # Skip header
    lines <- readLines(file)
    if ("BOTTLE" != substr(lines[1], 1, 6))
	stop("only type \"BOTTLE\" understood, but got header line\n", lines[1],"\n")
    if (nchar(sectionId) < 1)
        sectionId <- substr(lines[1], 8, nchar(lines[1]))
    n <- length(lines)
    header <- lines[1]
    for (l in (2:n)) {
	oceDebug(debug, lines[l], "\n")
	if ("#" != substr(lines[l], 1, 1)) {
	    header <- c(header, lines[l])
	    break
	}
    }
    header.length <- l + 1
    ccc <- textConnection(lines[header.length - 1])
    var.names <- scan(ccc, sep=",", what="", quiet=TRUE)
    close(ccc)
    ccc <- textConnection(lines[header.length])
    var.units <- scan(ccc, sep=",", what="", quiet=TRUE)
    close(ccc)
    if (length(var.units) != length(var.names))
        stop("length mismatch in variable lists")
    header <- lines[1:header.length]
    nd <- n - header.length - 1
    nv <- length(var.names)
    data <- array(dim=c(nd, nv - 2))
    stnSectionId <- vector("character", nd)
    stnId <- vector("character", nd)
    col.start <- 3
    for (l in ((header.length + 1):(n-1))) { # last line is END_DATA
	contents <- strsplit(lines[l], split=",")[[1]]
	stnSectionId[l - header.length] <- sub(" *","", contents[2])
	stnId[l - header.length] <- sub("^ *","", contents[3])
	data[l - header.length,] <- contents[col.start:nv]
	## FIXME: maybe should just scan this thing; it might work better anyway
    }
    if (length(which(var.names=="CTDPRS")))
	pressure <- as.numeric(data[,which(var.names=="CTDPRS") - col.start + 1])
    else
	stop("no column named \"CTDPRS\"")
    if (length(which(var.names=="CTDTMP")))
	temperature <- as.numeric(data[,which(var.names=="CTDTMP") - col.start + 1])
    else
	stop("no column named \"CTDTMP\"")
    ## Salinity is tricky.  There are two possibilities, in WOCE
    ## files, and each has a flag.  Here, we prefer CTDSAL, but if it
    ## has a bad flag value, we try SALNTY as a second option.  But
    ## if both CTDSAL and SALNTY are flagged, we just give up on the
    ## depth.
    if (length(which(var.names=="CTDSAL")))
	ctdsal <- as.numeric(data[,which(var.names=="CTDSAL") - col.start + 1])
    else
	stop("no column named \"CTDSAL\"")
    if (length(which(var.names=="CTDSAL_FLAG_W")))
	ctdsal.flag <- as.numeric(data[,which(var.names=="CTDSAL_FLAG_W") - col.start + 1])
    else
	stop("no column named \"CTDSAL_FLAG_W\"")
    if (length(which(var.names=="SALNTY")))
	salnty <- as.numeric(data[,which(var.names=="SALNTY") - col.start + 1])
    else
	stop("no column named \"SALNTY\"")
    if (length(which(var.names=="SALNTY_FLAG_W")))
	salnty.flag <- as.numeric(data[,which(var.names=="SALNTY_FLAG_W") - col.start + 1])
    else
	stop("no column named \"SALNTY_FLAG_W\"")
    if (length(which(var.names=="DATE")))
	stn.date <- as.character(data[,which(var.names=="DATE") - col.start + 1])
    else
	stop("no column named \"DATE\"")
    if (length(which(var.names=="TIME")))
	stn.time <- as.character(data[,which(var.names=="TIME") - col.start + 1])
    else
	stop("no column named \"TIME\"")
    ## EXPOCODE,SECT_ID,STNNBR,CASTNO,SAMPNO,BTLNBR,BTLNBR_FLAG_W,DATE,TIME,LATITUDE,LONGITUDE,DEPTH,CTDPRS,CTDTMP,CTDSAL,CTDSAL_FLAG_W,SALNTY,SALNTY_FLAG_W,OXYGEN,OXYGEN_FLAG_W,SILCAT,SILCAT_FLAG_W,NITRIT,NITRIT_FLAG_W,NO2+NO3,NO2+NO3_FLAG_W,PHSPHT,PHSPHT_FLAG_W

    if (length(which(var.names=="OXYGEN"))) {
        oxygen <- as.numeric(data[,which(var.names=="OXYGEN") - col.start + 1])
        oxygen[oxygen == missingValue] <- NA
    } else oxygen <- NULL
    if (length(which(var.names=="SILCAT"))) {
        silicate <- as.numeric(data[,which(var.names=="SILCAT") - col.start + 1])
        silicate[silicate == missingValue] <- NA
    } else silicate <- NULL
    if (length(which(var.names=="NITRIT"))) {
        nitrite <- as.numeric(data[,which(var.names=="NITRIT") - col.start + 1])
        nitrite[nitrite == missingValue] <- NA
    } else nitrite <- NULL
    if (length(which(var.names=="NITRAT"))) {
        nitrate <- as.numeric(data[,which(var.names=="NITRAT") - col.start + 1])
        nitrate[nitrate == missingValue] <- NA
    } else nitrate <- NULL
    if (length(which(var.names=="NO2+NO3"))) {
        no2plusno3 <- as.numeric(data[,which(var.names=="NO2+NO3") - col.start + 1])
        no2plusno3[no2plusno3 == missingValue] <- NA
        if (is.null(nitrate)) {
            nitrate <- no2plusno3 - nitrite
            rm(no2plusno3)
        } else {
            if (is.null(nitrite)) {
                nitrite <- no2plusno3 - nitrate
                rm(no2plusno3)
            } else {
                warning("cannot determine nitrate and nitrite")
                nitrite <- nitrate <- NULL
            }
        }
        ## http://woce.nodc.noaa.gov/woce_v3/wocedata_1/whp/exchange/exchange_format_desc.htm
    }
    if (length(which(var.names=="PHSPHT"))) {
        phosphate <- as.numeric(data[,which(var.names=="PHSPHT") - col.start + 1])
        phosphate[phosphate == missingValue] <- NA
    } else phosphate <- NULL

    waterDepth  <- as.numeric(data[,which(var.names=="DEPTH") - col.start + 1])
    ## FIXME: we have both 'latitude' and 'lat'; this is too confusing
    latitude  <- as.numeric(data[,which(var.names=="LATITUDE") - col.start + 1])
    longitude <- as.numeric(data[,which(var.names=="LONGITUDE") - col.start + 1])
    stationId <- data[,which(var.names=="STNNBR") - col.start + 1]
    stationId <- sub(" *$","",sub("^ *","",stationId)) #remove blanks
    stationList <- unique(stationId)
    numStations <- length(stationList)
    station <- vector("list", numStations)
    stn <- vector("character", numStations)
    lon <- vector("numeric", numStations)
    lat <- vector("numeric", numStations)
    time <- vector("numeric", numStations)
    tref <- as.POSIXct("2000-01-01 00:00:00", tz="UTC")
    trefn <- as.numeric(tref)
    for (i in 1:numStations) {
	oceDebug(debug, "processing station ", i, "\n")
	select <- which(stationId == stationList[i])
	# "199309232222"
	# "1993-09-23 22:22:00"
	time[i] <- as.numeric(strptime(paste(stn.date[select[1]], stn.time[select[1]], sep=""),
				       "%Y%m%d%H%M", tz="UTC")) - trefn
	stn[i] <- sub("^ *", "", stationId[select[1]])
	lat[i] <- latitude[select[1]]
	lon[i] <- longitude[select[1]]
	## Prefer CTDSAL, but also try SALNTY if no CTDSAL is ok
	goodSalinity <- ifelse(ctdsal.flag[select] %in% flags,
                               ctdsal[select],
                               ifelse(salnty.flag[select] %in% flags, salnty[select], NA))
	ok <- !is.na(goodSalinity)
	ok <- ok & pressure[select] >= 0
	thisStation <- as.ctd(salinity=goodSalinity[ok],
			       temperature=temperature[select[ok]],
			       pressure=pressure[select[ok]],

                               oxygen=if(!is.null(oxygen))oxygen[select[ok]],
                               nitrate=if(!is.null(nitrate))nitrate[select[ok]],
                               nitrite=if(!is.null(nitrite))nitrite[select[ok]],
                               phosphate=if(!is.null(phosphate))phosphate[select[ok]],
                               silicate=if(!is.null(silicate))silicate[select[ok]],

			       quality=ctdsal.flag[select[ok]],
			       ship=ship,
			       date=time[i] + tref,
			       scientist=scientist,
			       institute=institute,
			       latitude=lat[i],
			       longitude=lon[i],
			       cruise=stnSectionId[select[1]],
			       station=stn[i],
			       waterDepth=waterDepth[select[1]],
			       src=filename)
	oceDebug(debug, "  ", length(select[ok]), "levels @ ", lat[i], "N ", lon[i], "W\n")
	station[[i]] <- thisStation
    }
    data <- list(station=station)
    metadata <- list(header=header,sectionId=sectionId,stationId=stn,latitude=lat,longitude=lon,date=time+tref,filename=filename)
    if (missing(processingLog))
	processingLog <- paste(deparse(match.call()), sep="", collapse="")
    hitem <- processingLogItem(processingLog)
    res <- new("section")
    res@metadata <- metadata
    res@data <- data
    res@processingLog <- processingLog(res@processingLog, paste(deparse(match.call()), sep="", collapse=""))
    res
}

sectionGrid <- function(section, p, method=c("approx","boxcar","lm"),
			 debug=getOption("oceDebug"), ...)
{
    oceDebug(debug, "\bsectionGrid(section, p, method=\"", method, "\", ...) {\n", sep="")
    method <- match.arg(method)
    n <- length(section@data$station)
    oceDebug(debug, "have", n, "stations in this section\n")
    dp.list <- NULL
    if (missing(p)) {
	oceDebug(debug, "argument 'p' not given\n")
	p.max <- 0
	for (i in 1:n) {
	    p <- section@data$station[[i]]@data$pressure
	    dp.list <- c(dp.list, mean(diff(p)))
	    p.max <- max(c(p.max, p), na.rm=TRUE)
	}
	dp <- mean(dp.list, na.rm=TRUE) / 1.5 # make it a little smaller
	pt <- pretty(c(0, p.max), n=min(200, floor(p.max / dp)))
	oceDebug(debug, "p.max=", p.max, "; dp=", dp, "\n")
	oceDebug(debug, "pt=", pt, "\n")
    } else {
	if (length(p) == 1) {
	    if (p=="levitus") {
		pt <- c(0,   10,   20,   30,   50,   75,  100,  125,  150,  200,
			250,  300,  400,  500,  600,  700,  800,  900, 1000, 1100,
			1200, 1300, 1400, 1500, 1750, 2000, 2500, 3000, 3500, 4000,
			4500, 5000, 5500)
	    } else { # FIXME should insist numeric
		p.max <- 0
		for (i in 1:n) {
		    p <- section@data$station[[i]]@data$pressure
		    p.max <- max(c(p.max, p))
		}
		pt <- seq(0, p.max, p)
	    }
	} else {
	    pt <- p
	}
    }
    ## BUG should handle all variables (but how to interpolate on a flag?)
    res <- section
    for (i in 1:n) {
	res@data$station[[i]] <- ctdDecimate(section@data$station[[i]], p=pt, method=method, debug=debug-1, ...)
    }
    res@processingLog <- processingLog(res@processingLog, paste(deparse(match.call()), sep="", collapse=""))
    oceDebug(debug, "\b\b} # sectionGrid\n")
    res
}

sectionSmooth <- function(section, method=c("spline", "barnes"), debug=getOption("oceDebug"), ...)
{
    method <- match.arg(method)
    ## bugs: should ensure that every station has identical pressures
    ## FIXME: should have smoothing in the vertical also ... and is spline what I want??
    oceDebug(debug, "\bsectionSmooth(section,method=\"", method, "\", ...) {\n", sep="")
    if (!inherits(section, "section"))
        stop("method is only for section objects")
    nstn <- length(section@data$station)
    if (method == "spline") {
        stn1pressure <- section[["station", 1]][["pressure"]]
        npressure <- length(stn1pressure)
        for (istn in 1:nstn) {
            thisp <- section[["station", istn]][["pressure"]]
            if (length(thisp) != npressure)
                stop("pressure mismatch between station 1 and station", istn)
            if (any(thisp != stn1pressure))
                stop("pressure mismatch between station 1 and station", istn)
        }
        oceDebug(debug, "nstn=", nstn, "npressure=", npressure, "\n")
        res <- section
        ## reorder stations by distance from first (this
        ## is crucial if the files have been ordered by a
        ## directory listing, and they are not named e.g. 01
        ## to 10 etc but 1 to 10 etc.
        x <- geodDist(section)
        o <- order(x)
        res@metadata$latitude <- section@metadata$latitude[o]
        res@metadata$longitude <- section@metadata$longitude[o]
        res@metadata$stationId <- section@metadata$stationId[o]
        res@data$station <- section@data$station[o]
        x <- geodDist(res)
        temperatureMat <- array(dim=c(npressure, nstn))
        salinityMat <- array(dim=c(npressure, nstn))
        sigmaThetaMat <- array(dim=c(npressure, nstn))
        for (s in 1:nstn) {
            thisStation <- res@data$station[[s]]
            temperatureMat[,s] <- thisStation@data$temperature
            salinityMat[,s] <- thisStation@data$salinity
            sigmaThetaMat[,s] <- thisStation@data$sigmaTheta
        }
        ## turn off warnings about df being too small
        o <- options('warn')
        options(warn=-1) 
        for (p in 1:npressure) {
            ok <- !is.na(temperatureMat[p,]) ## FIXME: ok to infer missingness from temperature alone?
            nok <- sum(ok)
            iok <- (1:nstn)[ok]
            if (nok > 4) { ## Only fit spline if have 4 or more values; ignore bad values in fitting.
                temperatureMat[p,] <- predict(smooth.spline(x[ok], temperatureMat[p,ok], ...), x)$y
                salinityMat[p,]    <- predict(smooth.spline(x[ok],    salinityMat[p,ok], ...), x)$y
                sigmaThetaMat[p,]  <- predict(smooth.spline(x[ok],  sigmaThetaMat[p,ok], ...), x)$y
                ##oceDebug(debug, p, "dbar: smoothing, based on", nok, "good values\n")
            } else {
                ##oceDebug(debug, "pessure index=", p, ": not smoothing, since have only", nok, "good values\n")
            }
        }
        options(warn=o$warn) 
        for (s in 1:nstn) {
            res@data$station[[s]]@data$temperature <- temperatureMat[,s]
            res@data$station[[s]]@data$salinity <- salinityMat[,s]
            res@data$station[[s]]@data$sigmaTheta <- sigmaThetaMat[,s]
        }
    } else if (method == "barnes") {
        vars <- names(section[["station", 1]]@data)
        res <- section
        x <- geodDist(section)
        X <- p <- S <- NULL
        stn1pressure <- section[["station", 1]][["pressure"]]
        npressure <- length(stn1pressure)
        for (istn in 1:nstn) {
            stn <- section[["station", istn]]
            if (length(stn[["pressure"]]) != npressure)
                stop("pressure mismatch between station 1 and station", istn)
            if (any(stn[["pressure"]] != stn1pressure))
                stop("pressure mismatch between station 1 and station.", istn)
        }
        P <- rep(stn1pressure, nstn)
        X <- rep(x, each=npressure)
        for (var in vars) {
            if (var == "scan" || var == "time" || var == "pressure"
                || var == "depth" || var == "flag" || var == "quality")
                next
            v <- NULL
            oceDebug(debug, "smoothing", var, "\n")
            ## collect data
            if (FALSE) {
                for (istn in 1:nstn) {
                    oceDebug(debug, "station", istn, "\n")
                                        #browser()
                    stn <- section[["station", istn]]
                    v <- c(v, stn[[var]])
                }
            }
            ## grid overall, then deposit into stations (trimming for NA)
            v <- section[[var]]
            smu <- interpBarnes(X, P, v, xg=x, yg=stn1pressure, ..., debug=debug-1)
            for (istn in 1:nstn) {
                res@data$station[[istn]]@data[[var]] <- smu$zg[istn,]
                na <- is.na(section@data$station[[istn]][[var]])
                res@data$station[[istn]]@data[[var]][na] <- NA
            }
        }
    } else {
        stop("unknown method \"", method, "\"") # cannot reach here
    }

    res@processingLog <- processingLog(res@processingLog, paste(deparse(match.call()), sep="", collapse=""))
    oceDebug(debug, "\b\b} # sectionSmooth()\n")
    res
}

summary.section <- function(object, ...)
{
    if (!inherits(object, "section"))
        stop("method is only for section objects")
    numStations <- length(object@data$station)
    lon1 <- object@data$station[[1]]@metadata$longitude
    lat1 <- object@data$station[[1]]@metadata$latitude
    cat("Section Summary\n---------------\n\n")
    cat("* Source: \"", object@metadata$filename, "\"\n", sep="")
    cat("* ID:     \"", object@metadata$sectionId, "\"\n",sep="")
    stn.sum <- matrix(nrow=numStations, ncol=5)
    if (numStations > 0) {
        cat("* Station summary (first column is station ID):\n")
        for (i in 1:numStations) {
            stn <- object@data$station[[i]]
            stn.sum[i, 1] <- stn@metadata$longitude
            stn.sum[i, 2] <- stn@metadata$latitude
            stn.sum[i, 3] <- length(stn@data$pressure)
            if (is.finite(stn@metadata$waterDepth)) {
                stn.sum[i, 4] <- stn@metadata$waterDepth
            } else {
                temp <- stn@data$temperature
                wdi <- length(temp) - which(!is.na(rev(temp)))[1] + 1
                stn.sum[i, 4] <- stn@data$pressure[wdi]
            }
            stn.sum[i, 5] <- geodDist(lat1, lon1, stn@metadata$latitude, stn@metadata$longitude)
        }
        colnames(stn.sum) <- c("Long.", "Lat.", "Levels", "Depth", "Distance")
        rownames(stn.sum) <- object@metadata$stationId
        print(stn.sum, indent="    ")
    } else {
        cat("* No stations\n")
    }
    ##processingLogShow(object)
}

