## vim:textwidth=128:expandtab:shiftwidth=4:softtabstop=4

setMethod(f="initialize",
          signature="argo",
          definition=function(.Object,time,longitude,latitude,salinity,temperature,pressure,filename) {
              if (!missing(time)) .Object@data$time <- time
              if (!missing(longitude)) .Object@data$longitude <- longitude
              if (!missing(latitude)) .Object@data$latitude <- latitude
              if (!missing(salinity)) .Object@data$salinity <- salinity
              if (!missing(temperature)) .Object@data$temperature <-temperature 
              if (!missing(pressure)) .Object@data$pressure <- pressure
              .Object@metadata$filename <- if (missing(filename)) "" else filename
              .Object@processingLog$time <- as.POSIXct(Sys.time())
              .Object@processingLog$value <- "create 'argo' object"
              return(.Object)
          })

setMethod(f="subset",
          signature="argo",
          definition=function(x, subset, ...) {
              subsetString <- paste(deparse(substitute(subset)), collapse=" ")
              res <- x
              if (length(grep("time", subsetString)) ||
                  length(grep("longitude", subsetString)) || length(grep("latitude", subsetString))) {
                  keep <- eval(substitute(subset), x@data, parent.frame(2))
              } else if (length(grep("profile", subsetString))) {
                  ## add profile into the data, then do as usual
                  x@data$profile <- 1:length(x@data$time)
                  keep <- eval(substitute(subset), x@data, parent.frame(2))
              } else if (length(grep("pressure", subsetString))) {
                  ## check that it is a "gridded" argo
                  gridded <- ifelse(all(apply(x@data$pressure, 1, diff) == 0, na.rm=TRUE), TRUE, FALSE)
                  if (gridded) {
                      x@data$pressure <- x@data$pressure[,1] ## FIXME: have to convert pressure to vector
                      keep <- eval(substitute(subset), x@data, parent.frame(2))
                      x@data$pressure <- res@data$pressure ## FIXME: convert back to original for subsetting below
                  } else {
                      stop("cannot subset ungridded argo by pressure -- use argoGrid() first", call.=FALSE)
                  }
              } else {
                  stop("can only subset by time, longitude, latitude, pressure, and not by combinations", call.=FALSE)
              }
              ## Now do the subset
              if (length(grep("pressure", subsetString))) {
                  res@data$salinity <- x@data$salinity[keep,]
                  res@data$temperature <- x@data$temperature[keep,]
                  res@data$pressure <- x@data$pressure[keep,]
                  res@processingLog <- processingLogAppend(res@processingLog, paste("subset.ctd(x, subset=", subsetString, ")", sep=""))
              } else {
                  res@data$time <- x@data$time[keep]
                  res@data$longitude <- x@data$longitude[keep]
                  res@data$latitude <- x@data$latitude[keep]
                  res@data$salinity <- x@data$salinity[,keep]
                  res@data$temperature <- x@data$temperature[,keep]
                  res@data$pressure <- x@data$pressure[,keep]
                  res@processingLog <- processingLogAppend(res@processingLog, paste("subset.ctd(x, subset=", subsetString, ")", sep=""))
              }
              res
          })



setMethod(f="summary",
          signature="argo",
          definition=function(object, ...) {
              cat("Drifter Summary\n---------------\n\n")
              cat("* source:     \"", object@metadata$filename, "\"\n", sep="")
              cat("* id:         \"", object@metadata$id, "\"\n", sep="")
              callNextMethod()
          })

ncdfFixMatrix <- function(x)
{
    if (length(dim(x)) == 1)
        x <- as.vector(x)
    x
}

argoGrid <- function(argo, p, debug=getOption("oceDebug"), ...)
{
    oceDebug(debug, "argoGrid() {\n", sep="", unindent=1)
    dim <- dim(argo@data$pressure)
    ## ndepth <- dim[1]
    nprofile <- dim[2]
    ## FIXME: modify sal, temp, and pre.  In the end, pre constant along first index
    res <- argo
    salinity <- argo[["salinity"]]
    temperature <- argo[["temperature"]]
    pressure <- argo[["pressure"]]
    if (missing(p)) {
        pt <- apply(pressure, 1, median, na.rm=TRUE)
    } else if (length(p) == 1 && p == "levitus") {
        pt <- standardDepths()
        pt <- pt[pt < max(pressure, na.rm=TRUE)]
    } else if (is.numeric(p)) {
        if (length(p) == 1) {
            if (p < 1)
                stop("'p' must exceed 1")
            pt <- seq(0, max(pressure, na.rm=TRUE), length.out=p)
        } else {
            pt <- p
        }
    } else {
        stop("value of 'p' must be numeric, or \"levitus\"")
    }
    ##message("pt=c(", paste(round(pt), collapse=","), ")")
    npt <- length(pt)
    res@data$salinity <- matrix(0.0, ncol=nprofile, nrow=npt)
    res@data$temperature <- matrix(0.0, ncol=nprofile, nrow=npt)
    res@data$pressure <- matrix(0.0, ncol=nprofile, nrow=npt)
    for (profile in 1:nprofile) {
        ndata <- sum(!is.na(salinity[,profile]))
        if (ndata > 2 && 0 < max(abs(diff(pressure[,profile])),na.rm=TRUE)) {
            res@data$salinity[,profile] <- approx(pressure[,profile], salinity[,profile], pt, ...)$y
            res@data$temperature[,profile] <- approx(pressure[,profile], temperature[,profile], pt, ...)$y
            res@data$pressure[,profile] <- pt
        } else {
            res@data$salinity[,profile] <- rep(NA, npt)
            res@data$temperature[,profile] <- rep(NA, npt)
            res@data$pressure[,profile] <- pt
        }
    }
    res
}

read.argo <- function(file, debug=getOption("oceDebug"), processingLog, ...)
{
    if (!requireNamespace("ncdf4", quietly=TRUE))
        stop('must install.packages("ncdf4") to read argo data')
    if (missing(processingLog)) processingLog <- paste(deparse(match.call()), sep="", collapse="")
    ## ofile <- file
    filename <- ""
    ## NOTE: need to name ncdf4 package because otherwise R checks give warnings.
    if (is.character(file)) {
        filename <- fullFilename(file)
        file <- ncdf4::nc_open(file)
        on.exit(ncdf4::nc_close(file))
    } else {
        if (!inherits(file, "connection"))
            stop("argument `file' must be a character string or connection")
        if (!isOpen(file)) {
            file <- ncdf4::nc_open(file)
            on.exit(ncdf4::nc_close(file))
        }
    }
    id <- ncdf4::ncvar_get(file, "PLATFORM_NUMBER")[1]
    id <- gsub(" *$", "", id)
    id <- gsub("^ *", "", id)
    t0s <- as.vector(ncdf4::ncvar_get(file, "REFERENCE_DATE_TIME"))
    t0 <- strptime(t0s, "%Y%m%d%M%H%S", tz="UTC")
    julianDayTime <- as.vector(ncdf4::ncvar_get(file, "JULD"))
    time <- t0 + julianDayTime * 86400
    longitude <- ncdfFixMatrix(ncdf4::ncvar_get(file, "LONGITUDE"))
    longitudeNA <- ncdf4::ncatt_get(file, "LONGITUDE","_FillValue")$value
    longitude[longitude == longitudeNA] <- NA
    latitude <- ncdfFixMatrix(ncdf4::ncvar_get(file, "LATITUDE"))
    latitudeNA <- ncdf4::ncatt_get(file, "LATITUDE","_FillValue")$value
    latitude[latitude == latitudeNA] <- NA
    salinity <- ncdf4::ncvar_get(file, "PSAL")
    salinityNA <- ncdf4::ncatt_get(file, "PSAL","_FillValue")$value
    salinity[salinity == salinityNA] <- NA
    temperature <- ncdf4::ncvar_get(file, "TEMP")
    temperatureNA <- ncdf4::ncatt_get(file, "TEMP","_FillValue")$value
    temperature[temperature == temperatureNA] <- NA


    pressure <- ncdf4::ncvar_get(file, "PRES")
    pressureNA <- ncdf4::ncatt_get(file, "PRES","_FillValue")$value
    pressure[pressure == pressureNA] <- NA
    ## make things into matrices, even for a single profile
    if (1 == length(dim(salinity))) {
        dim <- c(length(salinity), 1)
        dim(salinity) <- dim
        dim(temperature) <- dim
        dim(pressure) <- dim
    }
    res <- new("argo", time=time,
               longitude=longitude, latitude=latitude, salinity=salinity, 
               temperature=temperature, pressure=pressure, filename=filename)
    res@metadata$filename <- filename
    res@metadata$id <- id
    if (1 == length(grep("ITS-90", ncdf4::ncatt_get(file, "TEMP", "long_name")$value, ignore.case=TRUE)))
        res@metadata$units$temperature <- c("\u00B0C", "ITS-90")
    if (1 == length(grep("PRACTICAL", ncdf4::ncatt_get(file, "PSAL", "long_name")$value, ignore.case=TRUE)))
        res@metadata$units$salinity <- c("", "PSS-78")
    if (1 == length(grep("east", ncdf4::ncatt_get(file, "LONGITUDE", "units")$value, ignore.case=TRUE)))
        res@metadata$units$longitude <- "\u00B0E"
    if (1 == length(grep("north", ncdf4::ncatt_get(file, "LATITUDE", "units")$value, ignore.case=TRUE)))
        res@metadata$units$latitude <- "\u00B0N"
    if (1 == length(grep("decibar", ncdf4::ncatt_get(file, "PRES", "units")$value, ignore.case=TRUE)))
        res@metadata$units$pressure <- "decibar"
    res@metadata$id <- if (!missing(id)) id else NA
    res@processingLog <- processingLogAppend(res@processingLog, paste(deparse(match.call()), sep="", collapse=""))
    res
}

as.argo <- function(time, longitude, latitude,
                       salinity, temperature, pressure, 
                       units=NULL,
                       id, filename="",
                       missingValue)
{
    if (inherits(class, "data.frame")) {
        df <- time 
        names <- names(df)
        time <- if ("time" %in% names) df$time else NULL
        salinity <- if ("salinity" %in% names) df$salinity else NULL
        temperature <- if ("temperature" %in% names) df$temperature else NULL
        pressure <- if ("pressure" %in% names) df$pressure else NULL
        longitude <- if ("longitude" %in% names) df$longitude else NULL
        latitude <- if ("latitude" %in% names) df$latitude else NULL
    } else {
        if (missing(time)) stop("must give time")
        if (missing(longitude)) stop("must give longitude")
        if (missing(latitude)) stop("must give latitude")
        if (missing(temperature)) stop("must give temperature")
        if (missing(salinity)) stop("must give salinity")
        if (missing(pressure)) stop("must give pressure")
    }
    res <- new("argo", time=time,
               longitude=longitude, latitude=latitude, salinity=salinity, 
               temperature=temperature, pressure=pressure, filename=filename)
    res@metadata$id <- if (!missing(id)) id else NA
    res@metadata$units <- if (!is.null(units)) units else
        list(longitude="\u00B0E", latitude="\u00B0N",
             salinity=c("", "PSS-78"), # assuming a particular scale
             temperature=c("\u00B0C", "ITS-90"), # assuming a particular scale
             pressure="dbar") # assuming a particular unit
    res@processingLog <- processingLogAppend(res@processingLog, paste(deparse(match.call()), sep="", collapse=""))
    res
}

setMethod(f="plot",
          signature=signature("argo"),
          definition=function (x, which = 1, level,
                               coastline=c("best", "coastlineWorld", "coastlineWorldMedium",
                                           "coastlineWorldFine", "none"),
                               cex=1, pch=1, type='p', col, fill=FALSE, 
                               adorn=NULL,
                               projection=NULL, parameters=NULL, orientation=NULL,
                               mgp=getOption("oceMgp"), mar=c(mgp[1]+1.5, mgp[1]+1.5, 1.5, 1.5),
                               tformat,
                               debug=getOption("oceDebug"),
                               ...)
          {
              if (!inherits(x, "argo"))
                  stop("method is only for objects of class '", "argo", "'")
              oceDebug(debug, "plot.argo(x, which=c(", paste(which,collapse=","), "),",
                      " mgp=c(", paste(mgp, collapse=","), "),",
                      " mar=c(", paste(mar, collapse=","), "),",
                      " ...) {\n", sep="", unindent=1)
              coastline <- match.arg(coastline)
              #opar <- par(no.readonly = TRUE)
              lw <- length(which)
              ##if (lw > 1) on.exit(par(opar))
              ##if (length(type) < lw) type <- rep(type, lw) # FIXME: recycle more sensibly
              ##if (length(pch) < lw) pch <- rep(pch, lw) # FIXME: recycle more sensibly
              ##if (length(cex) < lw) cex <- rep(cex, lw) # FIXME: recycle more sensibly
              adorn.length <- length(adorn)
              if (adorn.length == 1) {
                  adorn <- rep(adorn, lw)
                  adorn.length <- lw
              }
              ## omar <- par('mar')
              nw  <- length(which)
              if (nw > 1) {
                  par(mfcol=c(1, nw), mgp=mgp, mar=mar)
              } else {
                  par(mgp=mgp, mar=mar)
              }
              if (missing(level) || level == "all")
                  level <- seq(1L, dim(x@data$temperature)[1])
              ctd <- as.ctd(x@data$salinity, x@data$temperature, x@data$pressure,
                            units=list(temperature=c("\u00B0C", "ITS-90"), conductivity="ratio")) # guess on units
              which <- oce.pmatch(which,
                                  list(trajectory=1,
                                       "salinity ts"=2,
                                       "temperature ts"=3,
                                       "TS"=4,
                                       "salinity profile"=5,
                                       "temperature profile"=6))
              for (w in 1:nw) {
                  if (which[w] == 1) {
                      oceDebug(debug, "which[", w, "] ==1, so plotting a map\n")
                      ## map
                      ## FIXME: coastline selection should be DRY
                      haveCoastline <- FALSE
                      haveOcedata <- requireNamespace("ocedata", quietly=TRUE)
                      lonr <- range(x[["longitude"]], na.rm=TRUE)
                      latr <- range(x[["latitude"]], na.rm=TRUE)
                      if (coastline == "best") {
                          if (haveOcedata) {
                              bestcoastline <- coastlineBest(lonRange=lonr, latRange=latr)
                              oceDebug(debug, " 'best' coastline is: \"", bestcoastline, '\"\n', sep="")
                              if (bestcoastline == "coastlineWorld") {
                                  data(list=bestcoastline, package="oce", envir=environment())
                              } else {
                                  data(list=bestcoastline, package="ocedata", envir=environment())
                              }
                              coastline <- get(bestcoastline)
                          } else {
                              bestcoastline <- coastlineBest(lonRange=lonr, latRange=latr)
                              oceDebug(debug, " using \"coastlineWorld\" because ocedata package not installed\n")
                              data("coastlineWorld", package="oce", envir=environment())
                              coastline <- get("coastlineWorld")
                          }
                          haveCoastline <- TRUE
                      } else {
                          if (coastline != "none") {
                              if (coastline == "coastlineWorld") {
                                  data("coastlineWorld", package="oce", envir=environment())
                                  coastline <- get("coastlineWorld")
                              } else if (haveOcedata && coastline == "coastlineWorldFine") {
                                  data("coastlineWorldFine", package="ocedata", envir=environment())
                                  coastline <- get("coastlineWorldFine")
                              } else if (haveOcedata && coastline == "coastlineWorldMedium") {
                                  data("coastlineWorldMedium", package="ocedata", envir=environment())
                                  coastline <- get("coastlineWorldMedium")
                              }  else {
                                  stop("there is no built-in coastline file of name \"", coastline, "\"")
                              }
                              haveCoastline <- TRUE
                          }
                      }
                      ## if (!is.character(coastline)) stop("coastline must be a character string")

                      if (!is.null(projection)) {
                          meanlat <- mean(x[['latitude']], na.rm=TRUE)
                          meanlon <- mean(x[['longitude']], na.rm=TRUE)
                          ## id <- pmatch(projection, "automatic")
                          if (!is.na(pmatch(projection, "automatic"))) {
                              projection <- if (meanlat > 70)
                                  paste("+proj=stere +lon_0=", meanlon, sep="") else "+proj=merc"
                              oceDebug(debug, "using", projection, "projection (chosen automatically)\n")
                          } else {
                              oceDebug(debug, "using", projection, "projection (specified)\n")
                          }
                          mapPlot(x[["longitude"]], x[["latitude"]],
                                  projection=projection, orientation=orientation, parameters=parameters,
                                  type='p', cex=cex, pch=pch,
                                  col=if (missing(col)) "black" else col,
                                  debug=debug-1)
                          if (is.logical(fill) && fill) {
                              mapPolygon(coastline[['longitude']], coastline[['latitude']], col='lightgray')
                          } else {
                              if (is.character(fill)) {
                                  mapPolygon(coastline[['longitude']], coastline[['latitude']], col=fill) 
                              } else {
                                  mapPolygon(coastline[['longitude']], coastline[['latitude']])
                              }
                          }
                      } else {
                          asp <- 1 / cos(mean(range(x@data$latitude, na.rm=TRUE)) * atan2(1,1) / 45)
                          plot(x@data$longitude, x@data$latitude, asp=asp, 
                               type=type, cex=cex, pch=pch,
                               col=if (missing(col)) "black" else col,
                               xlab=resizableLabel("longitude"), ylab=resizableLabel("latitude"), ...)

                          if (haveCoastline) {
                              if (!is.null(coastline@metadata$fillable) && coastline@metadata$fillable) {
                                  polygon(coastline[["longitude"]], coastline[["latitude"]], col="lightgray", lwd=3/4)
                                  polygon(coastline[["longitude"]]+360, coastline[["latitude"]], col="lightgray", lwd=3/4)
                              } else {
                                  lines(coastline[["longitude"]], coastline[["latitude"]], col="darkgray")
                                  lines(coastline[["longitude"]]+360, coastline[["latitude"]], col="darkgray")
                              }
                          }
                          if (!missing(coastline)) {
                              polygon(coastline[["longitude"]], coastline[["latitude"]], col='lightgray')
                              if (type[w] == 'l')
                                  lines(x@data$longitude, x@data$latitude)
                              else
                                  points(x@data$longitude, x@data$latitude, cex=cex, pch=pch, col=if(!missing(col))col)
                          }
                      }
                      par(mar=mar)
                  } else if (which[w] == 2) {    # salinity timeseries
                      if (0 != sum(!is.na(x@data$salinity))) {
                          nlevels <- dim(x@data$salinity)[1]
                          t <- if (length(level) > 1)
                              numberAsPOSIXct(t(matrix(rep(x@data$time, nlevels), byrow=FALSE, ncol=nlevels)))
                          else
                              x@data$time
                          oce.plot.ts(t, as.vector(x@data$salinity[level,]),
                                      ylab=resizableLabel("S", "y"), type=type, 
                                      col=if (missing(col)) "black" else col,
                                      tformat=tformat, ...)
                      } else {
                          warning("no non-missing salinity data")
                      }
                  } else if (which[w] == 3) {    # temperature timeseries
                      if (0 != sum(!is.na(x@data$temperature))) {
                          nlevels <- dim(x@data$temperature)[1]
                          t <- if (length(level) > 1)
                              numberAsPOSIXct(t(matrix(rep(x@data$time, nlevels), byrow=FALSE, ncol=nlevels)))
                          else
                              x@data$time
                          oce.plot.ts(t, x@data$temperature[level,],
                                      ylab=resizableLabel("T", "y"), type=type,
                                      col=if (missing(col)) "black" else col,
                                      tformat=tformat, ...)
                      } else {
                          warning("no non-missing temperature data")
                      }
                  } else if (which[w] == 4) {    # TS
                      if (0 != sum(!is.na(x@data$temperature)) && 0 != sum(!is.na(x@data$salinity))) {
                          plotTS(ctd, col=if (missing(col)) "black" else col, type=type, ...)
                     } else {
                          warning("no non-missing salinity data")
                      }
                  } else if (which[w] == 5) {    # S profile
                      ## FIXME: how to handle the noise; if as below, document it
                      plotProfile(ctd, xtype="salinity",
                           Slim=quantile(x@data$salinity, c(0.01, 0.99), na.rm=TRUE),
                           ylim=quantile(x@data$pressure, c(0.99, 0.01), na.rm=TRUE), type=type)
                  } else if (which[w] == 6) {    # T profile
                      ## FIXME: how to handle the noise; if as below, document it
                      plotProfile(ctd, xtype="temperature",
                           Tlim=quantile(x@data$temperature, c(0.01, 0.99), na.rm=TRUE),
                           ylim=quantile(x@data$pressure, c(0.99, 0.01), na.rm=TRUE), type=type)
                  } else {
                      stop("plot.difter() given unknown value of which=", which[w], "\n", call.=FALSE)
                  }
              }
              oceDebug(debug, "} # plot.argo()\n", unindent=1)
              invisible()
          })

