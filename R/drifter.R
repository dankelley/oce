## vim:textwidth=128:expandtab:shiftwidth=4:softtabstop=4

setMethod(f="initialize",
          signature="drifter",
          definition=function(.Object,time,longitude,latitude,salinity,temperature,pressure,filename) {
              if (!missing(time)) .Object@data$time <- time
              if (!missing(longitude)) .Object@data$longitude <- longitude
              if (!missing(latitude)) .Object@data$latitude <- latitude
              if (!missing(salinity)) .Object@data$salinity <- salinity
              if (!missing(temperature)) .Object@data$temperature <-temperature 
              if (!missing(pressure)) .Object@data$pressure <- pressure
              .Object@metadata$filename <- if (missing(filename)) "" else filename
              .Object@processingLog$time <- as.POSIXct(Sys.time())
              .Object@processingLog$value <- "create 'drifter' object"
              return(.Object)
          })

setMethod(f="subset",
          signature="drifter",
          definition=function(x, subset, ...) {
              subsetString <- paste(deparse(substitute(subset)), collapse=" ")
              rval <- x
              if (length(grep("time", subsetString))) {
                  keep <- eval(substitute(subset), x@data, parent.frame())
                  rval@data$time <- x@data$time[keep]
                  rval@data$longitude <- x@data$longitude[keep]
                  rval@data$latitude <- x@data$latitude[keep]
                  rval@data$salinity <- x@data$salinity[,keep]
                  rval@data$temperature <- x@data$temperature[,keep]
                  rval@data$pressure <- x@data$pressure[,keep]
              } else if (length(grep("longitude", subsetString)) ||
                         length(grep("latitude", subsetString))) {
                  keep <- eval(substitute(subset), x@data, parent.frame())
                  rval@data$time <- x@data$time[keep]
                  rval@data$longitude <- x@data$longitude[keep]
                  rval@data$latitude <- x@data$latitude[keep]
                  rval@data$salinity <- x@data$salinity[,keep]
                  rval@data$temperature <- x@data$temperature[,keep]
                  rval@data$pressure <- x@data$pressure[,keep]
              } else {
                  stop("may only subset by time, longitude, or latitude, and not by combinations")
              }
              rval@processingLog <- processingLog(rval@processingLog, paste("subset.ctd(x, subset=", subsetString, ")", sep=""))
              rval
          })



setMethod(f="summary",
          signature="drifter",
          definition=function(object, ...) {
              cat("Drifter Summary\n---------------\n\n")
              cat("* source:     \"", object@metadata$filename, "\"\n", sep="")
              cat("* id:         \"", object@metadata$id, "\"\n", sep="")
              ndata <- length(object@data)
              threes <- matrix(nrow=ndata-1, ncol=3) # skipping time
              for (i in 2:ndata)
                  threes[i-1,] <- threenum(object@data[[i]])
              colnames(threes) <- c("Min.", "Mean", "Max.")
              rownames(threes) <- names(object@data)[-1]
              print(threes)
              processingLogShow(object)
          })

##setMethod(f="[[",
##          signature="drifter",
##          definition=function(x, i, j, drop) {
##              as(x, "oce")[[i, j, drop]]
##          })


read.drifter <- function(file, debug=getOption("oceDebug"), processingLog, ...)
{
    if (!require("ncdf4"))
        stop('must install.packages("ncdf4") to read drifter data')
    if (missing(processingLog)) processingLog <- paste(deparse(match.call()), sep="", collapse="")
    ofile <- file
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
    t0s <- ncdf4::ncvar_get(file, "REFERENCE_DATE_TIME")
    t0 <- strptime(t0s, "%Y%m%d%M%H%S", tz="UTC")
    julianDayTime <- ncdf4::ncvar_get(file, "JULD")
    time <- t0 + julianDayTime * 86400
    longitude <- ncdf4::ncvar_get(file, "LONGITUDE")
    longitudeNA <- ncdf4::ncatt_get(file, "LONGITUDE","_FillValue")$value
    longitude[longitude == longitudeNA] <- NA
    latitude <- ncdf4::ncvar_get(file, "LATITUDE")
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
    metadata <- list(filename=filename, id=id)
    res <- new("drifter", time=time,
               longitude=longitude, latitude=latitude, salinity=salinity, 
               temperature=temperature, pressure=pressure, filename=filename)
    res@metadata$id <- if (!missing(id)) id else NA
    res@processingLog <- processingLog(res@processingLog, paste(deparse(match.call()), sep="", collapse=""))
    res
}

as.drifter <- function(time, longitude, latitude,
                       salinity, temperature, pressure, 
                       id, filename="",
                       missingValue)
{
    if (class(time) == "data.frame") {
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
    res <- new("drifter", time=time,
               longitude=longitude, latitude=latitude, salinity=salinity, 
               temperature=temperature, pressure=pressure, filename=filename)
    res@metadata$id <- if (!missing(id)) id else NA
    res@processingLog <- processingLog(res@processingLog, paste(deparse(match.call()), sep="", collapse=""))
    res
}

setMethod(f="plot",
          signature=signature("drifter"),
          definition=function (x, which = 1, level,
                               coastline=c("best", "coastlineWorld", "coastlineWorldMedium",
                                           "coastlineWorldFine", "none"),
                               cex=1,
                               pch=1,
                               type='p',
                               col,
                               adorn=NULL,
                               mgp=getOption("oceMgp"),
                               mar=c(mgp[1]+1.5, mgp[1]+1.5, 1.5, 1.5),
                               tformat,
                               debug=getOption("oceDebug"),
                               ...)
          {
              if (!inherits(x, "drifter"))
                  stop("method is only for objects of class '", "drifter", "'")
              oceDebug(debug, "plot.drifter(x, which=c(", paste(which,collapse=","), "),",
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
              omar <- par('mar')
              nw  <- length(which)
              if (nw > 1) {
                  par(mfcol=c(1, nw), mgp=mgp, mar=mar)
              } else {
                  par(mgp=mgp, mar=mar)
              }
              if (missing(level) || level == "all")
                  level <- seq(1L, dim(x@data$temperature)[1])
              ctd <- as.ctd(x@data$salinity, x@data$temperature, x@data$pressure)

              oceDebug(debug, "which:", which, "\n")
              which <- ocePmatch(which,
                                 list(trajectory=1,
                                      "salinity ts"=2,
                                      "temperature ts"=3,
                                      "TS"=4,
                                      "salinity profile"=5,
                                      "temperature profile"=6))
              oceDebug(debug, "which:", which, "(after conversion to numerical form)\n")

              for (w in 1:nw) {
                  oceDebug(debug, "which[", w, "] = ", which[w], "\n")
                  if (which[w] == 1) {
                      asp <- 1 / cos(mean(range(x@data$latitude, na.rm=TRUE)) * atan2(1,1) / 45)
                      plot(x@data$longitude, x@data$latitude, asp=asp, 
                           type=type, cex=cex, pch=pch,
                           col=if (missing(col)) "black" else col,
                           xlab=resizableLabel("longitude"), ylab=resizableLabel("latitude"), ...)
                      ## FIXME: this coastline code is reproduced in section.R; it should be DRY
                      ## figure out coastline
                      haveCoastline <- FALSE
                      if (!is.character(coastline)) 
                          stop("coastline must be a character string")
                      haveOcedata <- require("ocedata", quietly=TRUE)
                      lonr <- range(x[["longitude"]], na.rm=TRUE)
                      latr <- range(x[["latitude"]], na.rm=TRUE)
                      if (coastline == "best") {
                          if (haveOcedata) {
                              bestcoastline <- coastlineBest(lonRange=lonr, latRange=latr)
                              oceDebug(debug, " 'best' coastline is: \"", bestcoastline, '\"\n', sep="")
                              data(list=bestcoastline, package="ocedata", envir=environment())
                              coastline <- get(bestcoastline)
                          } else {
                              oceDebug(debug, " using \"coastlineWorld\" because ocedata package not installed\n")
                              data(coastlineWorld, envir=environment())
                              coastline <- coastlineWorld
                          }
                          haveCoastline <- TRUE
                      } else {
                          if (coastline != "none") {
                              if (coastline == "coastlineWorld") {
                                  data("coastlineWorld", envir=environment())
                                  coastline <- coastlineWorld
                              } else if (haveOcedata && coastline == "coastlineWorldFine") {
                                  data("coastlineWorldFine", package="ocedata", envir=environment())
                                  coastline <- coastlineWorldFine
                              } else if (haveOcedata && coastline == "coastlineWorldMedium") {
                                  data("coastlineWorldMedium", package="ocedata", envir=environment())
                                  coastline <- coastlineWorldMedium
                              }  else {
                                  stop("there is no built-in coastline file of name \"", coastline, "\"")
                              }
                              haveCoastline <- TRUE
                          }
                      }
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
              oceDebug(debug, "} # plot.drifter()\n", unindent=1)
              invisible()
          })

