## vim: tw=120 shiftwidth=4 softtabstop=4 expandtab:
setMethod(f="initialize",
          signature="adp",
          definition=function(.Object,time,u,a,q,filename) {
              if (!missing(time)) .Object@data$time <- time
              if (!missing(u)) .Object@data$u <- u
              if (!missing(a)) .Object@data$a <- a 
              if (!missing(q)) .Object@data$q <- q
              .Object@metadata$filename <- if (missing(filename)) "" else filename
              .Object@processingLog$time <- as.POSIXct(Sys.time())
              .Object@processingLog$value <- "create 'adp' object"
              return(.Object)
          })

setMethod(f="summary",
          signature="adp",
          definition=function(object, ...) {
              cat("ADP Summary\n-----------\n\n", ...)
              cat(paste("* Instrument:         ", object@metadata$instrumentType, "\n", sep=""), ...)
              cat("* Manufacturer:      ", object@metadata$manufacturer, "\n")
              cat(paste("* Serial number:      ", object@metadata$serialNumber, "\n", sep=""), ...)
              cat(paste("* Firmware version:   ", object@metadata$firmwareVersion, "\n", sep=""), ...)
              cat(paste("* Source filename:    ``", object@metadata$filename, "``\n", sep=""), ...)
              if ("latitude" %in% names(object@metadata)) {
                  cat(paste("* Location:           ",
                            if (is.na(object@metadata$latitude)) "unknown latitude" else sprintf("%.5f N", object@metadata$latitude), ", ",
                            if (is.na(object@metadata$longitude)) "unknown longitude" else sprintf("%.5f E",
                                                                                                   object@metadata$longitude),
                            "\n", sep=''))
              }
              v.dim <- dim(object@data$v)
              cat("* Number of profiles:", v.dim[1], "\n")
              cat("* Number of cells:   ", v.dim[2], "\n")
              cat("* Number of beams:   ", v.dim[3], "\n")
              cat("* Cell size:         ", object@metadata$cellSize, "m\n")
              if (1 == length(agrep("nortek", object@metadata$manufacturer, ignore.case=TRUE))) {
                  resSpecific <- list(internalCodeVersion=object@metadata$internalCodeVersion,
                                      hardwareRevision=object@metadata$hardwareRevision,
                                      recSize=object@metadata$recSize*65536/1024/1024,
                                      velocityRange=object@metadata$velocityRange,
                                      firmwareVersion=object@metadata$firmwareVersion,
                                      config=object@metadata$config,
                                      configPressureSensor=object@metadata$configPressureSensor,
                                      configMagnetometerSensor=object@metadata$configMagnetometerSensor,
                                      configTiltSensor=object@metadata$configPressureSensor,
                                      configPressureSensor=object@metadata$configTiltSensor,
                                      serialNumberHead=object@metadata$serialNumberHead,
                                      blankingDistance=object@metadata$blankingDistance,
                                      measurementInterval=object@metadata$measurementInterval,
                                      deploymentName=object@metadata$deploymentName,
                                      velocityScale=object@metadata$velocityScale)
              } else if (1 == length(agrep("rdi", object@metadata$manufacturer, ignore.case=TRUE))) {
                  resSpecific <- list(instrumentSubtype=object@metadata[["instrumentSubtype"]],
                                      manufacturer=object@metadata$manufacturer,
                                      numberOfDataTypes=object@metadata$numberOfDataTypes,
                                      headingAlignment=object@metadata$headingAlignment,
                                      headingBias=object@metadata$headingBias,
                                      pingsPerEnsemble=object@metadata$pingsPerEnsemble,
                                      bin1Distance=object@metadata$bin1Distance,
                                      xmitPulseLength=object@metadata$xmitPulseLength,
                                      oceBeamSpreaded=object@metadata$oceBeamSpreaded,
                                      beamConfig=object@metadata$beamConfig)
              } else if (1 == length(agrep("sontek", object@metadata$manufacturer, ignore.case=TRUE))) {
                  resSpecific <- list(cpuSoftwareVerNum=object@metadata$cpuSoftwareVerNum,
                                      dspSoftwareVerNum=object@metadata$dspSoftwareVerNum,
                                      boardRev=object@metadata$boardRev,
                                      adpType=object@metadata$adpType,
                                      slantAngle=object@metadata$slantAngle,
                                      orientation=object@metadata$orientation)
              } else {
                  stop("can only summarize ADP objects of sub-type \"rdi\", \"sontek\", or \"nortek\", not class ", paste(class(object),collapse=","))
              }
              cat(sprintf("* Measurements:       %s %s to %s %s sampled at %.4g Hz\n",
                          format(object@metadata$measurementStart), attr(object@metadata$measurementStart, "tzone"),
                          format(object@metadata$measurementEnd), attr(object@metadata$measurementEnd, "tzone"),
                          1 / object@metadata$measurementDeltat))
              subsampleStart <- object@data$time[1]
              subsampleDeltat <- as.numeric(object@data$time[2]) - as.numeric(object@data$time[1])
              subsampleEnd <- object@data$time[length(object@data$time)]
              cat(sprintf("* Subsample:          %s %s to %s %s sampled at %.4g Hz\n",
                          format(subsampleStart), attr(subsampleStart, "tzone"),
                          format(subsampleEnd),  attr(subsampleEnd, "tzone"),
                          1 / subsampleDeltat))
              if (object@metadata$numberOfCells > 1)
                  cat(sprintf("* Cells:              %d, centered at %.3f m to %.3f m, spaced by %.3f m\n",
                              object@metadata$numberOfCells, object@data$distance[1],  tail(object@data$distance, 1), diff(object@data$distance[1:2])),  ...)
              else
                  cat(sprintf("* Cells:              one cell, centered at %.3f m\n", object@data$distance[1]), ...)

              cat("* Coordinate system: ", object@metadata$originalCoordinate, "[originally],", object@metadata$oceCoordinate, "[presently]\n", ...)
              cat("* Frequency:         ", object@metadata$frequency, "kHz\n", ...)
              cat("* Beams:             ", object@metadata$numberOfBeams, if (!is.null(object@metadata$oceBeamUnspreaded) &
                                                                              object@metadata$oceBeamUnspreaded) "beams (attenuated)" else "beams (not attenuated)",
                  "oriented", object@metadata$orientation, "with angle", object@metadata$beamAngle, "deg to axis\n", ...)
              if (!is.null(object@metadata$transformationMatrix)) {
                  digits <- 4
                  cat("* Transformation matrix::\n\n")
                  cat("  ", format(object@metadata$transformationMatrix[1,], width=digits+4, digits=digits, justify="right"), "\n")
                  cat("  ", format(object@metadata$transformationMatrix[2,], width=digits+4, digits=digits, justify="right"), "\n")
                  cat("  ", format(object@metadata$transformationMatrix[3,], width=digits+4, digits=digits, justify="right"), "\n")
                  if (object@metadata$numberOfBeams > 3)
                      cat("  ", format(object@metadata$transformationMatrix[4,], width=digits+4, digits=digits, justify="right"), "\n")
              }
              cat("\n")
              ## start building res from the header information
              haveData <- !is.null(object@data)
              res <- resSpecific
              res$measurementStart <- object@metadata$measurementStart
              res$measurementEnd <- object@metadata$measurementEnd
              res$measurementDeltat <- object@metadata$measurementDeltat
              res$frequency <- object@metadata$frequency
              res$numberOfDataTypes <- object@metadata$numberOfDataType
              res$bin1Distance <- object@metadata$bin1Distance
              res$xmitPulseLength <- object@metadata$xmitPulseLength
              res$oceBeamUnspreaded <- object@metadata$oceBeamUnspreaded
              res$beamAngle <- object@metadata$beamAngle
              res$beamConfig <- object@metadata$beamConfig
              res$transformationMatrix <- object@metadata$transformationMatrix
              res$orientation <- object@metadata$orientation
              res$originalCoordinate <- object@metadata$originalCoordinate
              res$oceCoordinate <- object@metadata$oceCoordinate
              res$processingLog <- object@processingLog
              dataNames <- names(object@data)
              threes <- matrix(nrow=(-2+length(dataNames)), ncol=3)
              ii <- 1
              for (i in 1:length(dataNames)) {
                  if (dataNames[i] != "time" && dataNames[i] != "distance") {
                      threes[ii,] <- threenum(object@data[[dataNames[i]]])
                      ii <- ii + 1
                  }
              }
              rownames(threes) <- c(dataNames[dataNames != "time" & dataNames != "distance"])
              colnames(threes) <- c("Min.", "Mean", "Max.")
              cat("* Statistics of subsample::\n\n")
              print(threes)
              processingLogShow(object)
          })

setMethod(f="[[",
          signature(x="adp", i="ANY", j="ANY"),
          definition=function(x, i, j, drop) {
              if (i == "a") {
                  if (!missing(j) && j == "numeric") {
                      rval <- x@data$a
                      dim <- dim(rval)
                      rval <- as.numeric(rval)
                      dim(rval) <- dim
                  } else {
                      rval <- x@data$a
                  }
                  rval
              } else if (i == "q") {
                  if (!missing(j) && j == "numeric") {
                      rval <- x@data$q
                      dim <- dim(rval)
                      rval <- as.numeric(rval)
                      dim(rval) <- dim
                  } else {
                      rval <- x@data$q
                  }
                  rval
              } else if (i == "g") {
                  if (!missing(j) && j == "numeric") {
                      rval <- x@data$g
                      dim <- dim(rval)
                      rval <- as.numeric(rval)
                      dim(rval) <- dim
                  } else {
                      rval <- x@data$g
                  }
                  rval
              } else {
                  as(x, "oce")[[i]]
              }
          })

setMethod(f="[[<-",
          signature="adp",
          definition=function(x, i, j, value) { # FIXME: use j for e.g. times
              if (i %in% names(x@metadata)) {
                  x@metadata[[i]] <- value
              } else if (i %in% names(x@data)) {
                  x@data[[i]] <- value
              } else {
                  stop("there is no item named \"", i, "\" in this ", class(x), " object")
              }
              ## Not checking validity because user may want to shorten items one by one, and check validity later.
              ## validObject(x)
              invisible(x)
          })

setValidity("adp",
            function(object) {
                if (!("v" %in% names(object@data))) {
                    cat("object@data$v is missing")
                    return(FALSE)
                }
                if (!("a" %in% names(object@data))) {
                    cat("object@data$a is missing")
                    return(FALSE)
                }
                if (!("q" %in% names(object@data))) {
                    cat("object@data$q is missing")
                    return(FALSE)
                }
                mdim <- dim(object@data$v)
                if ("a" %in% names(object@data) && !all.equal(mdim, dim(object@data$a))) {
                    cat("dimension of 'a' is (", dim(object@data$a), "), which does not match that of 'v' (", mdim, ")\n")
                    return(FALSE)
                }
                if ("q" %in% names(object@data) && !all.equal(mdim, dim(object@data$q))) {
                    cat("dimension of 'a' is (", dim(object@data$a), "), which does not match that of 'v' (", mdim, ")\n")
                    return(FALSE)
                }
                if ("time" %in% names(object@data)) {
                    n <- length(object@data$time)
                    for (item in c("pressure", "temperature", "salinity", "depth", "heading", "pitch", "roll")) {
                        if (item %in% names(object@data) && length(object@data[[item]]) != n) {
                            cat("length of time vector is ", n, " but the length of ", item, " is ", 
                                length(object@data[[item]]), "\n")
                            return(FALSE)
                        }
                    }
                    return(TRUE)
                }
            })


setMethod(f="subset",
          signature="adp",
          definition=function(x, subset, ...) {
              subsetString <- paste(deparse(substitute(subset)), collapse=" ")
              rval <- x
              dots <- list(...)
              debug <- getOption("oceDebug")
              if (length(dots) && ("debug" %in% names(dots)))
                  debug <- dots$debug
              if (missing(subset))
                  stop("must give 'subset'")
              if (length(grep("time", subsetString))) {
                  oceDebug(debug, "subsetting an adp by time\n")
                  if (length(grep("distance", subsetString)))
                      stop("cannot subset by both time and distance; split into multiple calls")
                  keep <- eval(substitute(subset), x@data, parent.frame(2))
                  names <- names(x@data)
                  haveDia <- "timeDia" %in% names
                  if (haveDia) {
                      subsetDiaString <- gsub("time", "timeDia", subsetString)
                      keepDia <- eval(parse(text=subsetDiaString), x@data)
                      oceDebug(debug, "for diagnostics, keeping ", 100*sum(keepDia) / length(keepDia), "% of data\n")
                  }
                  oceDebug(debug, vectorShow(keep, "keeping bins:"))
                  oceDebug(debug, "number of kept bins:", sum(keep), "\n")
                  if (sum(keep) < 2)
                      stop("must keep at least 2 profiles")
                  rval <- x
                  ## FIXME: are we handling slow timescale data?
                  for (name in names(x@data)) {
                      if (length(grep("Dia$", name))) {
                          if ("distance" == name)
                              next
                          if (name == "timeDia" || is.vector(x@data[[name]])) {
                              oceDebug(debug, "subsetting x@data$", name, ", which is a vector\n", sep="")
                              rval@data[[name]] <- x@data[[name]][keepDia]
                          } else if (is.matrix(x@data[[name]])) {
                              oceDebug(debug, "subsetting x@data$", name, ", which is a matrix\n", sep="")
                              rval@data[[name]] <- x@data[[name]][keepDia,]
                          } else if (is.array(x@data[[name]])) {
                              oceDebug(debug, "subsetting x@data$", name, ", which is an array\n", sep="")
                              rval@data[[name]] <- x@data[[name]][keepDia,,, drop=FALSE]
                          }
                      } else {
                          if (name == "time" || is.vector(x@data[[name]])) {
                              if ("distance" == name)
                                  next
                              oceDebug(debug, "subsetting x@data$", name, ", which is a vector\n", sep="")
                              rval@data[[name]] <- x@data[[name]][keep] # FIXME: what about fast/slow
                          } else if (is.matrix(x@data[[name]])) {
                              oceDebug(debug, "subsetting x@data$", name, ", which is a matrix\n", sep="")
                              rval@data[[name]] <- x@data[[name]][keep,]
                          } else if (is.array(x@data[[name]])) {
                              oceDebug(debug, "subsetting x@data$", name, ", which is an array\n", sep="")
                              rval@data[[name]] <- x@data[[name]][keep,,, drop=FALSE]
                          }
                      }
                  }
              } else if (length(grep("distance", subsetString))) {
                  oceDebug(debug, "subsetting an adp by distance\n")
                  if (length(grep("time", subsetString)))
                      stop("cannot subset by both time and distance; split into multiple calls")
                  keep <- eval(substitute(subset), x@data, parent.frame(2))
                  oceDebug(debug, vectorShow(keep, "keeping bins:"), "\n")
                  if (sum(keep) < 2)
                      stop("must keep at least 2 bins")
                  rval <- x
                  rval@data$distance <- x@data$distance[keep]
                  for (name in names(x@data)) {
                      if (name == "time")
                          next
                      if (is.array(x@data[[name]]) && 3 == length(dim(x@data[[name]]))) {
                          oceDebug(debug, "subsetting array data[[", name, "]] by distance\n")
                          oceDebug(debug, "before, dim(", name, ") =", dim(rval@data[[name]]), "\n")
                          rval@data[[name]] <- x@data[[name]][,keep,, drop=FALSE]
                          oceDebug(debug, "after, dim(", name, ") =", dim(rval@data[[name]]), "\n")
                      }
                  }
              } else if (length(grep("pressure", subsetString))) {
                  keep <- eval(substitute(subset), x@data, parent.frame(2))
                  rval <- x
                  rval@data$v <- rval@data$v[keep,,]
                  rval@data$a <- rval@data$a[keep,,]
                  rval@data$q <- rval@data$q[keep,,]
                  rval@data$time <- rval@data$time[keep]
                  ## the items below may not be in the dataset
                  names <- names(rval@data)
                  if ("bottomRange" %in% names) rval@data$bottomRange <- rval@data$bottomRange[keep,]
                  if ("pressure" %in% names) rval@data$pressure <- rval@data$pressure[keep]
                  if ("temperature" %in% names) rval@data$temperature <- rval@data$temperature[keep]
                  if ("salinity" %in% names) rval@data$salinity <- rval@data$salinity[keep]
                  if ("depth" %in% names) rval@data$depth <- rval@data$depth[keep]
                  if ("heading" %in% names) rval@data$heading <- rval@data$heading[keep]
                  if ("pitch" %in% names) rval@data$pitch <- rval@data$pitch[keep]
                  if ("roll" %in% names) rval@data$roll <- rval@data$roll[keep]
              } else {
                  stop("should express the subset in terms of distance or time")
              }
              rval@metadata$numberOfSamples <- dim(rval@data$v)[1]
              rval@metadata$numberOfCells <- dim(rval@data$v)[2]
              rval@processingLog <- processingLogAppend(rval@processingLog, paste("subset.adp(x, subset=", subsetString, ")", sep=""))
              rval
          })



head.adp <- function(x, n = 6L, ...)
{
    numberOfProfiles <- dim(x@data$v)[1]
    if (n < 0)
        look <- seq.int(max(1, (1 + numberOfProfiles + n)), numberOfProfiles)
    else
        look <- seq.int(1, min(n, numberOfProfiles))
    rval <- x
    for (name in names(x@data)) {
        if ("distance" == name)
            next
        if (is.vector(x@data[[name]])) {
            rval@data[[name]] <- x@data[[name]][look]
        } else if (is.matrix(x@data[[name]])) {
            rval@data[[name]] <- x@data[[name]][look,]
        } else if (is.array(x@data[[name]])) {
            rval@data[[name]] <- x@data[[name]][look,,]
        } else {
            rval@data[[name]] <- x@data[[name]][look] # for reasons unknown, 'time' is not a vector
        }
    }
    rval@processingLog <- processingLogAppend(rval@processingLog, paste(deparse(match.call()), sep="", collapse=""))
    rval
}

tail.adp <- function(x, n = 6L, ...)
{
    numberOfProfiles <- dim(x@data$v)[1]
    if (n < 0)
        look <- seq.int(1, min(numberOfProfiles, numberOfProfiles + n))
    else
        look <- seq.int(max(1, (1 + numberOfProfiles - n)), numberOfProfiles)
    rval <- x
    for (name in names(x@data)) {
        if (is.vector(x@data[[name]])) {
            rval@data[[name]] <- x@data[[name]][look]
        } else if (is.matrix(x@data[[name]])) {
            rval@data[[name]] <- x@data[[name]][look,]
        } else if (is.array(x@data[[name]])) {
            rval@data[[name]] <- x@data[[name]][look,,]
        } else {
            rval@data[[name]] <- x@data[[name]][look] # for reasons unknown, 'time' is not a vector
        }
    }
    rval@processingLog <- processingLogAppend(rval@processingLog, paste(deparse(match.call()), sep="", collapse=""))
    rval 
}

coordinate <- function(x)
{
    if (inherits(x, "adp") || inherits(x, "adv"))
        x@metadata$oceCoordinate
    else {
        warning("unknown object type; it must inherit from either \"adv\" or \"adp\"")
        NULL
    }
}

is.beam <- function(x)
{
    if (inherits(x, "adp") || inherits(x, "adv"))
        return(x@metadata$oceCoordinate == "beam")
    else {
        warning("unknown file type; the object must inherit from either \"adv\" or \"adp\"")
        return(FALSE)
    }
}

is.xyz <- function(x)
{
    if (inherits(x, "adp") || inherits(x, "adv"))
        return(x@metadata$oceCoordinate == "xyz")
    else {
        warning("unknown file type; the object must inherit from either \"adv\" or \"adp\"")
        return(FALSE)
    }
}
is.enu <- function(x)
{
    if (inherits(x, "adp") || inherits(x, "adv"))
        return(x@metadata$oceCoordinate == "enu")
    else {
        warning("unknown file type; the object must inherit from either \"adv\" or \"adp\"")
        return(FALSE)
    }
}

beamName <- function(x, which)
{
    if (x@metadata$oceCoordinate == "beam") {
        paste(gettext("beam", domain="R-oce"), 1:4)[which]
    } else if (x@metadata$oceCoordinate == "enu") {
        c(gettext("east", domain="R-oce"),
          gettext("north", domain="R-oce"),
          gettext("up", domain="R-oce"),
          gettext("error", domain="R-oce"))[which]
    } else if (x@metadata$oceCoordinate == "xyz") {
        c("u", "v", "w", "e")[which]
    } else if (x@metadata$oceCoordinate == "other") {
        c("u'", "v'", "w'", "e")[which]
    } else {
        " "
    }
}

read.adp <- function(file, from=1, to, by=1, tz=getOption("oceTz"),
                     longitude=NA, latitude=NA, 
                     manufacturer=c("rdi", "nortek", "sontek"),
                     monitor=FALSE, despike=FALSE, processingLog,
                     debug=getOption("oceDebug"),
                     ...)
{
    oceDebug(debug, "read.adp(...,from=", from,
             ",to=", if (missing(to)) "(missing)" else to,
             ",by=", by,
             ",manufacturer=", if (missing(manufacturer)) "(missing)" else manufacturer, ",...)\n")
    manufacturer <- match.arg(manufacturer)
    if (monitor)
        cat(file, "\n", ...)
    if (manufacturer == "rdi")
        read.adp.rdi(file=file, from=from, to=to, by=by, tz=tz,
                     longitude=longitude, latitude=latitude,
                     debug=debug-1, monitor=monitor, despike=despike,
                     processingLog=processingLog, ...)
    else if (manufacturer == "nortek")
        read.adp.nortek(file=file, from=from, to=to, by=by, tz=tz,
                        longitude=longitude, latitude=latitude,
                        debug=debug-1, monitor=monitor, despike=despike,
                        processingLog=processingLog, ...)
    else if (manufacturer == "sontek")
        read.adp.sontek(file=file, from=from, to=to, by=by, tz=tz,
                        longitude=longitude, latitude=latitude,
                        debug=debug-1, monitor=monitor, despike=despike,
                        processingLog=processingLog, ...)
}


setMethod(f="plot",
          signature=signature("adp"),
          definition=function(x, which=1:dim(x@data$v)[3], mode=c("normal", "diagnostic"),
                              col, breaks, zlim,
                              titles,
                              lwd=par('lwd'),
                              type='l',
                              ytype=c("profile", "distance"),
                              adorn=NULL,
                              drawTimeRange=getOption("oceDrawTimeRange"),
                              useSmoothScatter,
                              missingColor="gray",
                              mgp=getOption("oceMgp"),
                              mar=c(mgp[1]+1.5,mgp[1]+1.5,1.5,1.5),
                              mai.palette=rep(0, 4), #c(0, 1/8, 0, 3/8),
                              tformat,
                              marginsAsImage=FALSE,
                              cex=par("cex"), cex.axis=par("cex.axis"), cex.main=par("cex.main"),
                              xlim, ylim,
                              control,
                              useLayout=FALSE,
                              coastline="coastlineWorld", span=300,
                              main="",
                              grid=FALSE, grid.col='darkgray', grid.lty='dotted', grid.lwd=1,
                              debug=getOption("oceDebug"),
                              ...)
          {
              debug <- max(0, min(debug, 4))
              colGiven <- !missing(col)
              breaksGiven <- !missing(breaks)
              zlimGiven <- !missing(zlim)
              if (breaksGiven && zlimGiven)
                  stop("cannot supply both zlim and breaks")
              rval <- list(xat=NULL, yat=NULL)
              mode <- match.arg(mode)
              if (mode == "diagnostic") {
                  if (x@metadata$instrumentType != "aquadopp") {
                      warning("This instrument is not a Nortek Aquadopp, so mode=\"diagnostic\" is being ignored")
                      mode <- 'normal'
                  }
                  if (x@metadata$numberOfCells != 1) {
                      warning("This instrument seems to be a Nortek Aquadopp, but it has more than 1 cell, so it must not be; so mode=\"diagnostic\" is being ignored")
                      mode <- 'normal'
                  }
                  if (!("timeDia" %in% names(x@data))) {
                      warning("This instrument did not record Diagnostic data, so mode=\"diagnostic\" is being ignored")
                      mode <- 'normal'
                  }
              }
              oceDebug(debug, "plot.adp(x, which=\"", paste(which, collapse=","),
                       "\", breaks=", if (missing(breaks)) "(missing)" else 
                           paste("c(", paste(breaks, collapse=", "), ")", sep=""),
                       ", mode=\"", mode, "\", ...) {\n", sep="", unindent=1)
              oceDebug(debug, "par(mar)=", paste(par('mar'), collapse=" "), "\n")
              oceDebug(debug, "par(mai)=", paste(par('mai'), collapse=" "), "\n")
              oceDebug(debug, "par(mfg)=", paste(par('mfg'), collapse=" "), "\n")
              oceDebug(debug, "mai.palette=", paste(mai.palette, collapse=" "), "\n")
              if (!missing(ylim))
                  oceDebug(debug, "ylim=c(", paste(ylim, collapse=", "), ")\n")
              if (!inherits(x, "adp"))
                  stop("method is only for objects of class '", "adp", "'")
              if (!(is.null(x@metadata$haveActualData) || x@metadata$haveActualData)) {
                  warning("there are no profiles in this dataset")
                  return
              }
              opar <- par(no.readonly = TRUE)
              nw <- length(which)
              nbeams  <- x@metadata$numberOfBeams
              if (nw == 1) {
                  pm <- pmatch(which, c("velocity","amplitude","quality","hydrography", "angles"))
                  if (!is.na(pm)) {
                      if (pm == 1)
                          which <- 0 + seq(1, nbeams)
                      else if (pm == 2)
                          which <- 4 + seq(1, nbeams)
                      else if (pm == 3)
                          which <- 8 + seq(1, nbeams)
                      else if (pm == 4)
                          which <- 14:15
                      else if (pm == 5)
                          which <- 16:18
                      nw <- length(which)
                  }
              }
              if (!missing(titles) && length(titles) != nw)
                  stop("length of 'titles' must equal length of 'which'")
              if (nw > 1)
                  on.exit(par(opar))
              if (is.numeric(which)) {
                  whichFraction <- which - floor(which)
                  which <- floor(which)
              } else {
                  whichFraction <- rep(0, length(which))
              }
              par(mgp=mgp, mar=mar, cex=cex)
              dots <- list(...)
              ytype <- match.arg(ytype)
              ## user may specify a matrix for xlim and ylim
              ylimGiven <- !missing(ylim)
              oceDebug(debug, 'ylimGiven=', ylimGiven, '\n')
              if (ylimGiven) {
                  if (is.matrix(ylim)) {
                      if (dim(ylim)[2] != nw) {
                          ylim2 <- matrix(ylim, ncol=2, nrow=nw, byrow=TRUE) # FIXME: is this what I want?
                      }
                  } else {
                      ylim2 <- matrix(ylim, ncol=2, nrow=nw, byrow=TRUE) # FIXME: is this what I want?
                  }
                  class(ylim2) <- class(ylim)
                  ylim <- ylim2
              }
              xlimGiven <- !missing(xlim)
              if (xlimGiven) {
                  if (is.matrix(xlim)) {
                      if (dim(xlim)[2] != nw) {
                          xlim2 <- matrix(xlim, ncol=2, nrow=nw) # FIXME: is this what I want?
                      }
                  } else {
                      if (length(xlim) != 2)
                          stop("xlim must be a vector of length 2, or a 2-column matrix")
                      xlim2 <- matrix(xlim[1:2], ncol=2, nrow=nw, byrow=TRUE)
                  }
                  class(xlim2) <- class(xlim)
                  attr(xlim2, "tzone") <- attr(xlim, "tzone")
                  xlim <- xlim2
              }
              if (missing(zlim)) {
                  zlimGiven <- FALSE
                  zlimAsGiven <- NULL
              } else {
                  zlimGiven <- TRUE
                  if (is.vector(zlim)) {
                      if (length(zlim) == 2) {
                          zlimAsGiven <- matrix(rep(zlim, length(which)),ncol=2,byrow=TRUE)
                      } else {
                          stop("zlim must be a vector of length 2, or a matrix with 2 columns")
                      }
                  } else {
                      ## FIXME: should this be made into a matrix?
                      zlimAsGiven <- zlim
                  }
              }

              ylimAsGiven <- if (ylimGiven) ylim else NULL
              if (missing(lwd))
                  lwd <- rep(par('lwd'), length.out=nw)
              else
                  lwd <- rep(lwd, length.out=nw)
              if (missing(main))
                  main <- rep('', length.out=nw)
              else
                  main <- rep(main, length.out=nw)
              oceDebug(debug, "later on in plot.adp:\n")
              oceDebug(debug, "  par(mar)=", paste(par('mar'), collapse=" "), "\n")
              oceDebug(debug, "  par(mai)=", paste(par('mai'), collapse=" "), "\n")

              oceDebug(debug, "which:", which, "\n")
              which <- oce.pmatch(which,
                                  list(u1=1, u2=2, u3=3, u4=4,
                                       a1=5, a2=6, a3=7, a4=8,
                                       q1=9, q2=10, q3=11, q4=12,
                                       g1=70, g2=71, g3=72, g4=73,
                                       salinity=13,
                                       temperature=14,
                                       pressure=15,
                                       heading=16,
                                       pitch=17,
                                       roll=18,
                                       progressivevector=23,
                                       uv=28,
                                       "uv+ellipse"=29,
                                       "uv+ellipse+arrow"=30,
                                       bottomrange=40,
                                       bottomrange1=41, bottomrange2=42, bottomrange3=43, bottomrange4=44,
                                       bottomvelocity=50,
                                       bottomvelocity1=51, bottomvelocity2=52, bottomvelocity3=53, bottomvelocity4=54,
                                       heaving=55,
                                       map=60,
                                       soundSpeed=100,
                                       velocity=1:3,
                                       amplitude=5:7,
                                       quality=9:11,
                                       hydrography=14:15,
                                       angles=16:18))
              nw <- length(which) # may be longer with e.g. which='velocity'
              oceDebug(debug, "which:", which, "(after conversion to numerical codes)\n")
              images <- c(1:12, 70:73)
              timeseries <- c(13:22, 40:44, 50:54, 55, 100)
              spatial <- 23:27
              speed <- 28

              adorn.length <- length(adorn)
              if (adorn.length == 1) {
                  adorn <- rep(adorn, nw)
                  adorn.length <- nw
              }

              tt <- x@data$time
              ttDia <- x@data$timeDia  # may be null
              class(tt) <- "POSIXct"              # otherwise image() gives warnings
              if (!zlimGiven && all(which %in% 5:8)) { # single scale for all 'a' (amplitude) data
                  zlim <- range(abs(as.numeric(x[["a"]][,,which[1]-4])), na.rm=TRUE) # FIXME name of item missing, was ma
                  if (length(which) > 1) {
                      for (w in 2:length(which)) {
                          zlim <- range(abs(c(zlim, x[["a"]][,,which[w]-4])), na.rm=TRUE) # FIXME: check name
                      }
                  }
              }
              ##oceDebug(debug, "useLayout=", useLayout, "\n")
              showBottom <- ("bottomRange" %in% names(x@data)) && !missing(control) && !is.null(control["drawBottom"])
              if (showBottom)
                  bottom <- apply(x@data$bottomRange, 1, mean)
              oceDebug(debug, "showBottom=", showBottom, "\n")
              if (useLayout) {
                  if (any(which %in% images) || marginsAsImage) {
                      w <- 1.5
                      lay <- layout(matrix(1:(2*nw), nrow=nw, byrow=TRUE), widths=rep(c(1, lcm(w)), nw))
                      oceDebug(debug, "calling layout(matrix...)\n")
                      oceDebug(debug, "using layout, since this is an image, or has marginsAsImage\n")
                  } else {
                      if (nw != 1 || which != 23) {
                          lay <- layout(cbind(1:nw))
                          oceDebug(debug, "calling layout(cbind(1:", nw, ")\n")
                          oceDebug(debug, "using layout\n")
                      }
                  }
              } else {
                  if (nw > 1) {
                      par(mfrow=c(nw, 1))
                      oceDebug(debug, "calling par(mfrow=c(", nw, ", 1)\n")
                  }
              }
              flipy <- ytype == "profile" && x@metadata$orientation == "downward"
              numberOfCells <- x[["numberOfCells"]]
              haveTimeImages <- any(which %in% images) && 1 < numberOfCells
              oceDebug(debug, 'haveTimeImages=', haveTimeImages, '(if TRUE, it means any timeseries graphs get padding on RHS)\n')
              for (w in 1:nw) {
                  oceDebug(debug, "which[", w, "]=", which[w], "; drawTimeRange=", drawTimeRange, "\n")
                  if (which[w] %in% images) {                   # image types
                      skip <- FALSE
                      if (which[w] %in% 1:(x@metadata$numberOfBeams)) {    #velocity
                          if (mode == "diagnostic") {
                              oceDebug(debug, "a diagnostic velocity component image/timeseries\n")
                              z <- x@data$vDia[,,which[w]]
                              zlab <- if (missing(titles)) paste(beamName(x, which[w]), "Dia", sep="") else titles[w]
                              y.look <- if (ylimGiven) ylimAsGiven[w, 1] <= x@data$distance & x@data$distance <= ylimAsGiven[w, 2] else rep(TRUE, length(x@data$distance))
                              zlim <- if (zlimGiven) zlimAsGiven[w,] else max(abs(x@data$vDia[,y.look,which[w]]), na.rm=TRUE) * c(-1,1)
                          } else {
                              oceDebug(debug, "a velocity component image/timeseries\n")
                              z <- x@data$v[,,which[w]]
                              zlab <- if (missing(titles)) beamName(x, which[w]) else titles[w]
                              y.look <- if (ylimGiven) ylimAsGiven[w, 1] <= x@data$distance & x@data$distance <= ylimAsGiven[w, 2] else rep(TRUE, length(x@data$distance))
                              if (0 == sum(y.look))
                                  stop("no data in the provided ylim=c(", paste(ylimAsGiven[w,], collapse=","), ")")
                              zlim <- if (zlimGiven) zlimAsGiven[w,] else {
                                  if (breaksGiven) NULL else max(abs(x@data$v[,y.look,which[w]]), na.rm=TRUE) * c(-1,1)
                              }
                          }
                          oceDebug(debug, 'flipy=', flipy, '\n')
                      } else if (which[w] %in% 5:(4+x@metadata$numberOfBeams)) { # amplitude
                          if (mode == "diagnostic" && "aDia" %in% names(x@data)) {
                              oceDebug(debug, "a diagnostic amplitude component image/timeseries\n")
                              z <- as.numeric(x@data$aDia[,,which[w]-4])
                              dim(z) <- dim(x@data$aDia)[1:2]
                              y.look <- if (ylimGiven) ylimAsGiven[1] <= x@data$distance & x@data$distance <= ylimAsGiven[2] else rep(TRUE, length(x@data$distance))
                              zlim <- if (zlimGiven) zlimAsGiven[w,] else {
                                  if (breaksGiven) NULL else range(as.numeric(x@data$aDia[,y.look,]), na.rm=TRUE) 
                              }
                              zlab <- c(expression(aDia[1]),expression(a[2]),expression(aDia[3]),expression(aDia[4]))[which[w]-4]
                          } else {
                              oceDebug(debug, "an amplitude component image/timeseries\n")
                              z <- as.numeric(x@data$a[,,which[w]-4])
                              dim(z) <- dim(x@data$a)[1:2]
                              y.look <- if (ylimGiven) ylimAsGiven[1] <= x@data$distance & x@data$distance <= ylimAsGiven[2] else rep(TRUE, length(x@data$distance))
                              zlim <- if (zlimGiven) zlimAsGiven[w,] else {
                                  if (breaksGiven) NULL else range(as.numeric(x@data$a[,y.look,]), na.rm=TRUE) 
                              }
                              zlab <- c(expression(a[1]),expression(a[2]),expression(a[3]),expression(a[4]))[which[w]-4]
                          }
                      } else if (which[w] %in% 9:(8+x@metadata$numberOfBeams)) { # correlation
                          if ("q" %in% names(x@data)) {
                              z <- as.numeric(x@data$q[,,which[w]-8])
                              dim(z) <- dim(x@data$q)[1:2]
                              zlim <- c(0, 256)
                              zlab <- c(expression(q[1]),expression(q[2]),expression(q[3]))[which[w]-8]
                          } else if ("amp" %in% names(x@data)) {
                              z <- as.numeric(x@data$amp[,,which[w]-8])
                              dim(z) <- dim(x@data$amp)[1:2]
                              zlim <- c(0, max(as.numeric(x@data$amp)))
                              zlab <- c(expression(amp[1]),expression(amp[2]),expression(amp[3]))[which[w]-8]
                          }
                      } else if (which[w] %in% 70:(69+x@metadata$numberOfBeams)) { # correlation
                          if ("g" %in% names(x@data)) {
                              z <- as.numeric(x@data$g[,,which[w]-69])
                              dim(z) <- dim(x@data$g)[1:2]
                              zlim <- c(0, 100)
                              zlab <- c(expression(g[1]),expression(g[2]),expression(g[3]))[which[w]-8]
                          } else {
                              warning("ADP object lacks a 'g' data item")
                          }
                      } else {
                          skip <- TRUE
                      }
                      if (!skip) {
                          if (numberOfCells > 1) {
                              if (xlimGiven) {
                                  ats <- imagep(x=tt, y=x@data$distance, z=z,
                                                xlim=xlim[w,],
                                                zlim=zlim,
                                                flipy=flipy,
                                                col=if (colGiven) col else {
                                                    if (missing(breaks)) oce.colorsPalette(128, 1)
                                                    else oce.colorsPalette(length(breaks)-1, 1)
                                                },
                                                breaks=breaks,
                                                ylab=resizableLabel("distance m"),
                                                xlab="Time",
                                                zlab=zlab,
                                                tformat=tformat,
                                                drawTimeRange=drawTimeRange,
                                                drawContours=FALSE,
                                                missingColor=missingColor,
                                                adorn=adorn[w],
                                                mgp=mgp,
                                                mar=mar,
                                                mai.palette=mai.palette,
                                                cex=cex*(1 - min(nw / 8, 1/4)), # FIXME: should emulate par(mfrow)
                                                main=main[w],
                                                debug=debug-1,
                                                ...)
                              } else {
                                  oceDebug(debug, "issue 585: about to call imagep()\n")
                                  ats <- imagep(x=tt, y=x@data$distance, z=z,
                                                zlim=zlim,
                                                flipy=flipy,
                                                ylim=if (ylimGiven) ylim[w,] else
                                                    range(x@data$distance, na.rm=TRUE),
                                                    col=if (colGiven) col else {
                                                        if (missing(breaks)) oce.colorsPalette(128, 1)
                                                        else oce.colorsPalette(length(breaks)-1, 1)
                                                    },
                                                    breaks=breaks,
                                                    ylab=resizableLabel("distance"),
                                                    xlab="Time",
                                                    zlab=zlab,
                                                    tformat=tformat,
                                                    drawTimeRange=drawTimeRange,
                                                    drawContours=FALSE,
                                                    missingColor=missingColor,
                                                    adorn=adorn[w],
                                                    mgp=mgp,
                                                    mar=mar,
                                                    mai.palette=mai.palette,
                                                    cex=cex*(1 - min(nw / 8, 1/4)),
                                                    main=main[w],
                                                    debug=debug-1,
                                                    ...)
                              }
                              if (showBottom)
                                  lines(x@data$time, bottom)
                          } else {
                              col <- if (colGiven) rep(col, length.out=nw) else rep("black", length.out=nw)
                              time  <- if (mode== "diagnostic") x@data$timeDia else x@data$time
                              tlim <- range(time)
                              ats <- oce.plot.ts(time, z, ylab=zlab,
                                                 xlim=if(xlimGiven) xlim[w,] else tlim,
                                                 ylim=if(ylimGiven) ylim[w,],
                                                 xaxs="i",
                                                 col=col[w],
                                                 lwd=lwd[w],
                                                 cex=cex*(1 - min(nw / 8, 1/4)),
                                                 cex.axis=cex*(1 - min(nw / 8, 1/4)),
                                                 main=main[w],
                                                 type=type,
                                                 mgp=mgp,
                                                 mar=c(mgp[1], mgp[1]+1.5, 1.5, 1.5),
                                                 tformat=tformat,
                                                 adorn=adorn[w],
                                                 debug=debug-1)
                              rval$xat <- grid$xat
                              rval$yat <- grid$yat
                          }
                      }
                      drawTimeRange <- FALSE
                  } else if (which[w] %in% timeseries) { # time-series types
                      col <- if (colGiven) rep(col, length.out=nw) else rep("black", length.out=nw)
                      oceDebug(debug, "graph", w, "is a timeseries\n")
                      ##par(mgp=mgp, mar=mar, cex=cex)
                      tlim <- range(x@data$time)
                      if (which[w] == 13) {
                          if (haveTimeImages) drawPalette(debug=debug-1)
                          ats <- oce.plot.ts(x@data$time, x@data$salinity,
                                             xlim=if(xlimGiven) xlim[w,] else tlim,
                                             ylim=if(ylimGiven) ylim[w,],
                                             xaxs="i",
                                             col=col[w],
                                             lwd=lwd[w],
                                             cex=cex*(1 - min(nw / 8, 1/4)),
                                             cex.axis=cex*(1 - min(nw / 8, 1/4)),
                                             main=main[w],
                                             ylab=resizableLabel("S"),
                                             type=type,
                                             mgp=mgp,
                                             mar=if(haveTimeImages) par('mar') else c(mgp[1], mgp[1]+1.5, 1.5, 1.5),
                                             drawTimeRange=drawTimeRange,
                                             tformat=tformat,
                                             adorn=adorn[w],
                                             debug=debug-1)
                      } else if (which[w] == 14) {
                          if (haveTimeImages) drawPalette(debug=debug-1, mai=mai.palette)
                          if (mode == "diagnostic" && "temperatureDia" %in% names(x@data)) {
                              ats <- oce.plot.ts(x@data$timeDia, x@data$temperatureDia,
                                                 xlim=if(xlimGiven) xlim[w,] else tlim,
                                                 ylim=if(ylimGiven) ylim[w,],
                                                 xaxs="i",
                                                 col=col[w],
                                                 lwd=lwd[w],
                                                 cex=cex*(1 - min(nw / 8, 1/4)),
                                                 cex.axis=cex*(1 - min(nw / 8, 1/4)),
                                                 main=main[w],
                                                 ylab=expression(paste("Diagnostic T [ ", degree, "C ]")),
                                                 type=type,
                                                 mgp=mgp,
                                                 mar=if(haveTimeImages) par('mar') else c(mgp[1], mgp[1]+1.5, 1.5, 1.5),
                                                 tformat=tformat,
                                                 adorn=adorn[w],
                                                 debug=debug-1)
                          } else {
                              ats <- oce.plot.ts(x@data$time, x@data$temperature,
                                                 xlim=if(xlimGiven) xlim[w,] else tlim,
                                                 ylim=if(ylimGiven) ylim[w,],
                                                 xaxs="i",
                                                 col=col[w],
                                                 lwd=lwd[w],
                                                 cex=cex*(1 - min(nw / 8, 1/4)),
                                                 cex.axis=cex*(1 - min(nw / 8, 1/4)),
                                                 main=main[w],
                                                 ylab=expression(paste("T [ ", degree, "C ]")),
                                                 type=type,
                                                 mgp=mgp,
                                                 mar=if(haveTimeImages) par('mar') else c(mgp[1], mgp[1]+1.5, 1.5, 1.5),
                                                 tformat=tformat,
                                                 adorn=adorn[w],
                                                 debug=debug-1)
                          }
                      } else if (which[w] == 15) {
                          if (haveTimeImages) drawPalette(debug=debug-1, mai=mai.palette)
                          if (mode == "diagnostic" && "pressureDia" %in% names(x@data)) {
                              ats <- oce.plot.ts(x@data$timeDia, x@data$pressureDia,
                                                 xlim=if(xlimGiven) xlim[w,] else tlim,
                                                 ylim=if(ylimGiven) ylim[w,],
                                                 xaxs="i",
                                                 col=col[w],
                                                 lwd=lwd[w],
                                                 cex=cex*(1 - min(nw / 8, 1/4)),
                                                 cex.axis=cex*(1 - min(nw / 8, 1/4)),
                                                 main=main[w],
                                                 ylab="pDia",
                                                 type=type,
                                                 mgp=mgp,
                                                 mar=if(haveTimeImages) par('mar') else c(mgp[1], mgp[1]+1.5, 1.5, 1.5),
                                                 drawTimeRange=drawTimeRange,
                                                 tformat=tformat,
                                                 adorn=adorn[w],
                                                 debug=debug-1)
                          } else {
                              ats <- oce.plot.ts(x@data$time, x@data$pressure,
                                                 xlim=if(xlimGiven) xlim[w,] else tlim,
                                                 ylim=if(ylimGiven) ylim[w,],
                                                 xaxs="i",
                                                 col=col[w],
                                                 lwd=lwd[w],
                                                 cex=cex*(1 - min(nw / 8, 1/4)),
                                                 cex.axis=cex*(1 - min(nw / 8, 1/4)),
                                                 main=main[w],
                                                 ylab=resizableLabel("p"),
                                                 type=type,
                                                 mgp=mgp,
                                                 mar=if(haveTimeImages) par('mar') else c(mgp[1], mgp[1]+1.5, 1.5, 1.5),
                                                 drawTimeRange=drawTimeRange,
                                                 tformat=tformat,
                                                 adorn=adorn[w],
                                                 debug=debug-1)
                          }
                      } else if (which[w] == 16) {
                          if (haveTimeImages) drawPalette(debug=debug-1, mai=mai.palette)
                          if (mode == "diagnostic" && "headingDia" %in% names(x@data)) {
                              ats <- oce.plot.ts(x@data$timeDia, x@data$headingDia,
                                                 xlim=if(xlimGiven) xlim[w,] else tlim,
                                                 ylim=if(ylimGiven) ylim[w,],
                                                 xaxs="i",
                                                 col=col[w],
                                                 lwd=lwd[w],
                                                 cex=cex*(1 - min(nw / 8, 1/4)),
                                                 cex.axis=cex*(1 - min(nw / 8, 1/4)),
                                                 main=main[w],
                                                 ylab="headingDia",
                                                 type=type,
                                                 mgp=mgp,
                                                 mar=if(haveTimeImages) par('mar') else c(mgp[1], mgp[1]+1.5, 1.5, 1.5),
                                                 drawTimeRange=drawTimeRange,
                                                 tformat=tformat,
                                                 adorn=adorn[w],
                                                 debug=debug-1)
                          } else {
                              ats <- oce.plot.ts(x@data$time, x@data$heading,
                                                 xlim=if(xlimGiven) xlim[w,] else tlim,
                                                 ylim=if(ylimGiven) ylim[w,],
                                                 xaxs="i",
                                                 col=col[w],
                                                 lwd=lwd[w],
                                                 cex=cex*(1 - min(nw / 8, 1/4)),
                                                 cex.axis=cex*(1 - min(nw / 8, 1/4)),
                                                 main=main[w],
                                                 ylab=resizableLabel("heading"),
                                                 type=type,
                                                 mgp=mgp,
                                                 mar=if(haveTimeImages) par('mar') else c(mgp[1], mgp[1]+1.5, 1.5, 1.5),
                                                 drawTimeRange=drawTimeRange,
                                                 tformat=tformat,
                                                 adorn=adorn[w],
                                                 debug=debug-1)
                          }
                      } else if (which[w] == 17) {
                          if (haveTimeImages) drawPalette(debug=debug-1, mai=mai.palette)
                          if (mode == "diagnostic" && "pitchDia" %in% names(x@data)) {
                              ats <- oce.plot.ts(x@data$timeDia, x@data$pitchDia,
                                                 xlim=if(xlimGiven) xlim[w,] else tlim,
                                                 ylim=if(ylimGiven) ylim[w,],
                                                 xaxs="i",
                                                 col=col[w],
                                                 lwd=lwd[w],
                                                 cex=cex*(1 - min(nw / 8, 1/4)),
                                                 cex.axis=cex*(1 - min(nw / 8, 1/4)),
                                                 main=main[w],
                                                 ylab="pitchDia",
                                                 type=type,
                                                 mgp=mgp,
                                                 mar=if(haveTimeImages) par('mar') else c(mgp[1], mgp[1]+1.5, 1.5, 1.5),
                                                 drawTimeRange=drawTimeRange,
                                                 tformat=tformat,
                                                 adorn=adorn[w],
                                                 debug=debug-1)
                          } else {
                              ats <- oce.plot.ts(x@data$time, x@data$pitch,
                                                 xlim=if(xlimGiven) xlim[w,] else tlim,
                                                 ylim=if(ylimGiven) ylim[w,],
                                                 xaxs="i",
                                                 col=col[w],
                                                 lwd=lwd[w],
                                                 cex=cex*(1 - min(nw / 8, 1/4)),
                                                 cex.axis=cex*(1 - min(nw / 8, 1/4)),
                                                 main=main[w],
                                                 ylab=resizableLabel("pitch"),
                                                 type=type,
                                                 mgp=mgp,
                                                 mar=if(haveTimeImages) par('mar') else c(mgp[1], mgp[1]+1.5, 1.5, 1.5),
                                                 drawTimeRange=drawTimeRange,
                                                 tformat=tformat,
                                                 adorn=adorn[w],
                                                 debug=debug-1)
                          }
                      } else if (which[w] == 18) {
                          if (haveTimeImages) drawPalette(debug=debug-1, mai=mai.palette)
                          if (mode == "diagnostic" && "rollDia" %in% names(x@data)) {
                              ats <- oce.plot.ts(x@data$timeDia, x@data$rollDia,
                                                 xlim=if(xlimGiven) xlim[w,] else tlim,
                                                 ylim=if(ylimGiven) ylim[w,],
                                                 xaxs="i",
                                                 col=col[w],
                                                 lwd=lwd[w],
                                                 cex=cex*(1 - min(nw / 8, 1/4)),
                                                 cex.axis=cex*(1 - min(nw / 8, 1/4)),
                                                 main=main[w],
                                                 ylab="rollDia",
                                                 type=type,
                                                 mgp=mgp,
                                                 mar=if(haveTimeImages) par('mar') else c(mgp[1], mgp[1]+1.5, 1.5, 1.5),
                                                 drawTimeRange=drawTimeRange,
                                                 tformat=tformat,
                                                 adorn=adorn[w],
                                                 debug=debug-1)
                          } else {
                              ats <- oce.plot.ts(x@data$time, x@data$roll,
                                                 xlim=if(xlimGiven) xlim[w,] else tlim,
                                                 ylim=if(ylimGiven) ylim[w,],
                                                 xaxs="i",
                                                 col=col[w],
                                                 lwd=lwd[w],
                                                 cex=cex*(1 - min(nw / 8, 1/4)),
                                                 cex.axis=cex*(1 - min(nw / 8, 1/4)),
                                                 main=main[w],
                                                 ylab=resizableLabel("roll"),
                                                 type=type,
                                                 mgp=mgp,
                                                 mar=if(haveTimeImages) par('mar') else c(mgp[1], mgp[1]+1.5, 1.5, 1.5),
                                                 drawTimeRange=drawTimeRange,
                                                 tformat=tformat,
                                                 adorn=adorn[w],
                                                 debug=debug-1)
                          }
                      } else if (which[w] == 19) {
                          if (x@metadata$numberOfBeams > 0) {
                              if (haveTimeImages) drawPalette(debug=debug-1, mai=mai.palette)
                              ats <- oce.plot.ts(x@data$time, apply(x@data$v[,,1], 1, mean, na.rm=TRUE),
                                                 xlim=if(xlimGiven) xlim[w,] else tlim,
                                                 ylim=if(ylimGiven) ylim[w,],
                                                 xaxs="i",
                                                 col=col[w],
                                                 lwd=lwd[w],
                                                 cex=cex*(1 - min(nw / 8, 1/4)),
                                                 cex.axis=cex*(1 - min(nw / 8, 1/4)),
                                                 main=main[w],
                                                 ylab=beamName(x, 1),
                                                 type=type,
                                                 mgp=mgp,
                                                 mar=if(haveTimeImages) par('mar') else c(mgp[1], mgp[1]+1.5, 1.5, 1.5),
                                                 mai.palette=mai.palette,
                                                 drawTimeRange=drawTimeRange,
                                                 tformat=tformat,
                                                 adorn=adorn[w],
                                                 debug=debug-1)
                          } else {
                              warning("cannot plot beam/velo 1 because the device no beams")
                          }
                      } else if (which[w] == 20) {
                          if (x@metadata$numberOfBeams > 1) {
                              if (haveTimeImages) drawPalette(debug=debug-1, mai=mai.palette)
                              ats <- oce.plot.ts(x@data$time, apply(x@data$v[,,2], 1, mean, na.rm=TRUE),
                                                 xlim=if(xlimGiven) xlim[w,] else tlim,
                                                 ylim=if(ylimGiven) ylim[w,],
                                                 xaxs="i",
                                                 col=col[w],
                                                 lwd=lwd[w],
                                                 cex=cex*(1 - min(nw / 8, 1/4)),
                                                 cex.axis=cex*(1 - min(nw / 8, 1/4)),
                                                 main=main[w],
                                                 ylab=beamName(x, 2),
                                                 type=type,
                                                 mgp=mgp,
                                                 mar=if(haveTimeImages) par('mar') else c(mgp[1], mgp[1]+1.5, 1.5, 1.5),
                                                 mai.palette=mai.palette,
                                                 drawTimeRange=drawTimeRange,
                                                 tformat=tformat,
                                                 adorn=adorn[w],
                                                 debug=debug-1)
                          } else {
                              warning("cannot plot beam/velo 2 because the device has only ", x@metadata$numberOfBeams, " beams")
                          }
                      } else if (which[w] == 21) {
                          if (x@metadata$numberOfBeams > 2) {
                              if (haveTimeImages) drawPalette(debug=debug-1, mai=mai.palette)
                              ats <- oce.plot.ts(x@data$time, apply(x@data$v[,,3], 1, mean, na.rm=TRUE),
                                                 xlim=if(xlimGiven) xlim[w,] else tlim,
                                                 ylim=if(ylimGiven) ylim[w,],
                                                 xaxs="i",
                                                 col=col[w],
                                                 lwd=lwd[w],
                                                 cex=cex*(1 - min(nw / 8, 1/4)),
                                                 cex.axis=cex*(1 - min(nw / 8, 1/4)),
                                                 main=main[w],
                                                 ylab=beamName(x, 3),
                                                 type=type,
                                                 mgp=mgp,
                                                 mar=if(haveTimeImages) par('mar') else c(mgp[1], mgp[1]+1.5, 1.5, 1.5),
                                                 mai.palette=mai.palette,
                                                 drawTimeRange=drawTimeRange,
                                                 tformat=tformat,
                                                 adorn=adorn[w],
                                                 debug=debug-1)
                          } else {
                              warning("cannot plot beam/velo 3 because the device has only", x@metadata$numberOfBeams, "beams")
                          }
                      } else if (which[w] == 22) {
                          if (x@metadata$numberOfBeams > 3) {
                              if (haveTimeImages) drawPalette(debug=debug-1, mai=mai.palette)
                              ats <- oce.plot.ts(x@data$time, apply(x@data$v[,,4], 1, mean, na.rm=TRUE),
                                                 xlim=if(xlimGiven) xlim[w,] else tlim,
                                                 ylim=if(ylimGiven) ylim[w,],
                                                 xaxs="i",
                                                 col=col[w],
                                                 lwd=lwd[w],
                                                 cex=cex*(1 - min(nw / 8, 1/4)),
                                                 cex.axis=cex*(1 - min(nw / 8, 1/4)),
                                                 main=main[w],
                                                 ylab=beamName(x, 4),
                                                 type=type,
                                                 mgp=mgp,
                                                 mar=if(haveTimeImages) par('mar') else c(mgp[1], mgp[1]+1.5, 1.5, 1.5),
                                                 mai.palette=mai.palette,
                                                 drawTimeRange=drawTimeRange,
                                                 tformat=tformat,
                                                 adorn=adorn[w],
                                                 debug=debug-1)
                          } else {
                              warning("cannot plot beam/velo 4 because the device has only", x@metadata$numberOfBeams, "beams")
                          }
                      } else  if (which[w] == 55) { # heaving
                          if (haveTimeImages) drawPalette(debug=debug-1, mai=mai.palette)
                          dt <- as.numeric(x@data$time[2]) - as.numeric(x@data$time[1])
                          ats <- oce.plot.ts(x@data$time, dt * cumsum(apply(x@data$v[,,3], 1, mean)),
                                             xlim=if(xlimGiven) xlim[w,] else tlim,
                                             ylim=if(ylimGiven) ylim[w,],
                                             xaxs="i",
                                             col=col[w],
                                             lwd=lwd[w],
                                             cex=cex*(1 - min(nw / 8, 1/4)),
                                             cex.axis=cex*(1 - min(nw / 8, 1/4)),
                                             main=main[w],
                                             ylab="Heaving [m]",
                                             type=type,
                                             mgp=mgp,
                                             mar=if(haveTimeImages) par('mar') else c(mgp[1], mgp[1]+1.5, 1.5, 1.5),
                                             mai.palette=mai.palette,
                                             drawTimeRange=drawTimeRange,
                                             tformat=tformat,
                                             adorn=adorn[w],
                                             debug=debug-1)
                          drawTimeRange <- FALSE
                      } else if (which[w] == 100) {
                          oceDebug(debug, "draw(ctd, ...) of type 'soundSpeed'\n")
                          if (haveTimeImages) drawPalette(debug=debug-1, mai=mai.palette)
                          ats <- oce.plot.ts(x[["time"]], x[["soundSpeed"]],
                                             xlim=if(xlimGiven) xlim[w,] else tlim,
                                             ylim=if(ylimGiven) ylim[w,],
                                             xaxs="i",
                                             col=col[w],
                                             lwd=lwd[w],
                                             cex=cex*(1 - min(nw / 8, 1/4)),
                                             cex.axis=cex*(1 - min(nw / 8, 1/4)),
                                             main=main[w],
                                             ylab="Sound Speed [m/s]",
                                             type=type,
                                             mgp=mgp,
                                             mar=if(haveTimeImages) par('mar') else c(mgp[1], mgp[1]+1.5, 1.5, 1.5),
                                             tformat=tformat,
                                             adorn=adorn[w],
                                             debug=debug-1)
                      } else if (which[w] %in% 40:44) { # bottomRange
                          par(mar=c(mgp[1]+1,mgp[1]+1,1,1))
                          n <- prod(dim(x@data$v)[1:2])
                          if ("br" %in% names(x@data)) {
                              if (which[w] == 40) {
                                  ats <- oce.plot.ts(x@data$time, apply(x@data$br, 1, mean, na.rm=TRUE), ylab="Range [m]",
                                                     type=type,
                                                     xlim=if(xlimGiven) xlim[w,] else tlim,
                                                     ylim=if(ylimGiven) ylim[w,] else range(x@data$br, na.rm=TRUE),
                                                     tformat=tformat,
                                                     debug=debug-1)
                              } else {
                                  ats <- oce.plot.ts(x@data$time, x@data$br[,which[w]-40], ylab=c("Beam", which[w]-40, "range [m]"),
                                                     type=type,
                                                     xlim=if(xlimGiven) xlim[w,] else tlim,
                                                     ylim=if(ylimGiven) ylim[w,] else range(x@data$br[,1], na.rm=TRUE),
                                                     tformat=tformat,
                                                     debug=debug-1)
                              }
                          } else {
                              warning("cannot handle which= ", which[w], " because this instrument lacked bottom tracking")
                          }
                      } else if (which[w] %in% 50:54) { # bottom velocity
                          par(mar=c(mgp[1]+1,mgp[1]+1,1,1))
                          n <- prod(dim(x@data$v)[1:2])
                          if ("bv" %in% names(x@data)) {
                              if (which[w] == 50) {
                                  ats <- oce.plot.ts(x@data$time, apply(x@data$bv, 1, mean, na.rm=TRUE), ylab="Range [m]",
                                                     type=type,
                                                     xlim=if(xlimGiven) xlim[w,] else tlim,
                                                     ylim=if(ylimGiven) ylim[w,] else range(x@data$bv, na.rm=TRUE),
                                                     tformat=tformat,
                                                     debug=debug-1)
                              } else {
                                  ats <- oce.plot.ts(x@data$time, x@data$bv[,which[w] - 50], ylab=c("Beam",which[w]-50,"velocity [m/s]"),
                                                     type=type,
                                                     xlim=if(xlimGiven) xlim[w,] else tlim,
                                                     ylim=if(ylimGiven) ylim[w,] else range(x@data$bv[,1], na.rm=TRUE),
                                                     tformat=tformat,
                                                     debug=debug-1)
                              }
                          } else {
                              warning("cannot handle which= ", which[w], " because this instrument lacked bottom tracking")
                          }
                      }

                      ## FIXME delete the next block, after testing.
                      if (marginsAsImage && useLayout)  { # FIXME: I think this should be deleted
                          ## blank plot, to get axis length same as for images
                          omar <- par("mar")
                          par(mar=c(mar[1], 1/4, mgp[2]+1/2, mgp[2]+1))
                          plot(1:2, 1:2, type='n', axes=FALSE, xlab="", ylab="")
                          par(mar=omar)
                      }
                  } else if (which[w] %in% spatial) {                   # various spatial types
                      if (which[w] == 23) {    # progressive vector
                          par(mar=c(mgp[1]+1,mgp[1]+1,1,1))
                          if (mode == 'diagnostic')
                              dt <- as.numeric(difftime(x@data$timeDia[2], x@data$timeDia[1],units="sec")) # FIXME: should not assume all equal
                          else
                              dt <- as.numeric(difftime(x@data$time[2], x@data$time[1],units="sec")) # FIXME: should not assume all equal
                          m.per.km <- 1000
                          if (mode == 'diagnostic') {
                              U <- x@data$vDia[,1,1]
                              V <- x@data$vDia[,1,2]
                              ttt <- x@data$timeDia
                          } else {
                              U <- x@data$v[,,1]
                              V <- x@data$v[,,2]
                              ttt <- x@data$time
                          }
                          if (!missing(control) && !is.null(control$bin)) {
                              if (control$bin < 1)
                                  stop("cannot have control$bin less than 1, but got ", control$bin)
                              max.bin <- dim(x@data$v)[2]
                              if (control$bin > max.bin)
                                  stop("cannot have control$bin larger than ", max.bin," but got ", control$bin)
                              u <- U[,control$bin,1]
                              v <- V[,control$bin,2]
                          } else {
                              if (x@metadata$numberOfCells > 1) {
                                  u <- apply(U, 1, mean, na.rm=TRUE)
                                  v <- apply(V, 1, mean, na.rm=TRUE)
                              } else {
                                  u <- U
                                  v <- V
                              }
                          }
                          u[is.na(u)] <- 0        # zero out missing
                          v[is.na(v)] <- 0
                          ##xDist <- cumsum(u) * dt / m.per.km
                          ##yDist <- cumsum(v) * dt / m.per.km
                          xDist <- integrateTrapezoid(ttt, u, 'cA') / m.per.km
                          yDist<- integrateTrapezoid(ttt, v, 'cA') / m.per.km
                          plot(xDist, yDist, xlab="km", ylab="km", type='l', asp=1, col=if (colGiven) col else "black", ...)
                          xaxp <- par("xaxp")
                          xat <- seq(xaxp[1], xaxp[2], length.out=1+xaxp[3])
                          yaxp <- par("yaxp")
                          yat <- seq(yaxp[1], yaxp[2], length.out=1+yaxp[3])
                          ats <- list(xat=xat, yat=yat)
                      } else if (which[w] == 24) {
                          par(mar=c(mgp[1]+1,mgp[1]+1,1,1))
                          value <- apply(x@data$v[,,1], 2, mean, na.rm=TRUE)
                          plot(value, x@data$distance, xlab=beamName(x, 1),
                               ylab=resizableLabel("distance"), type='l', ...)
                          xaxp <- par("xaxp")
                          xat <- seq(xaxp[1], xaxp[2], length.out=1+xaxp[3])
                          yaxp <- par("yaxp")
                          yat <- seq(yaxp[1], yaxp[2], length.out=1+yaxp[3])
                          ats <- list(xat=xat, yat=yat)
                      } else if (which[w] == 25) {
                          par(mar=c(mgp[1]+1,mgp[1]+1,1,1))
                          value <- apply(x@data$v[,,2], 2, mean, na.rm=TRUE)
                          plot(value, x@data$distance, xlab=beamName(x, 2),
                               ylab=resizableLabel("distance"), type='l', ...)
                          xaxp <- par("xaxp")
                          xat <- seq(xaxp[1], xaxp[2], length.out=1+xaxp[3])
                          yaxp <- par("yaxp")
                          yat <- seq(yaxp[1], yaxp[2], length.out=1+yaxp[3])
                          ats <- list(xat=xat, yat=yat)
                      } else if (which[w] == 26) {
                          par(mar=c(mgp[1]+1,mgp[1]+1,1,1))
                          value <- apply(x@data$v[,,3], 2, mean, na.rm=TRUE)
                          plot(value, x@data$distance, xlab=beamName(x, 3),
                               ylab=resizableLabel("distance"), type='l', ...)
                          xaxp <- par("xaxp")
                          xat <- seq(xaxp[1], xaxp[2], length.out=1+xaxp[3])
                          yaxp <- par("yaxp")
                          yat <- seq(yaxp[1], yaxp[2], length.out=1+yaxp[3])
                          ats <- list(xat=xat, yat=yat)
                      } else if (which[w] == 27) {
                          if (x@metadata$numberOfBeams > 3) {
                              par(mar=c(mgp[1]+1,mgp[1]+1,1,1))
                              value <- apply(x@data$v[,,4], 2, mean, na.rm=TRUE)
                              plot(value, x@data$distance, xlab=beamName(x, 4),
                                   ylab=resizableLabel("distance"), type='l', ...)
                              xaxp <- par("xaxp")
                              xat <- seq(xaxp[1], xaxp[2], length.out=1+xaxp[3])
                              yaxp <- par("yaxp")
                              yat <- seq(yaxp[1], yaxp[2], length.out=1+yaxp[3])
                              ats <- list(xat=xat, yat=yat)
                          } else {
                              warning("cannot use which=27 because this device did not have 4 beams")
                          }
                      }
                      if (w <= adorn.length) {
                          t <- try(eval(adorn[w]), silent=TRUE)
                          if (class(t) == "try-error") warning("cannot evaluate adorn[", w, "]\n")
                      }
                  } else if (which[w] %in% 28:30) { # "uv", "uv+ellipse", or "uv+ellipse+arrow"
                      par(mar=c(mgp[1]+1,mgp[1]+1,1,1))
                      n <- dim(x@data$v)[1]
                      if (!missing(control) && !is.null(control$bin)) {
                          if (control$bin < 1)
                              stop("cannot have control$bin less than 1, but got ", control$bin)
                          max.bin <- dim(x@data$v)[2]
                          if (control$bin > max.bin)
                              stop("cannot have control$bin larger than ", max.bin," but got ", control$bin)
                          u <- x@data$v[,control$bin,1]
                          v <- x@data$v[,control$bin,2]
                      } else {
                          if (x@metadata$numberOfCells > 1) {
                              u <- apply(x@data$v[,,1], 1, mean, na.rm=TRUE)
                              v <- apply(x@data$v[,,2], 1, mean, na.rm=TRUE)
                          } else {
                              u <- x@data$v[,1,1]
                              v <- x@data$v[,1,2]
                          }
                      }
                      oceDebug(debug, "uv type plot\n")
                      if (n < 5000 || (!missing(useSmoothScatter) && !useSmoothScatter)) {
                          if ("type" %in% names(dots)) {
                              plot(u, v,
                                   xlab=resizableLabel("u"),
                                   ylab=resizableLabel("v"),
                                   asp=1, col=if (colGiven) col else "black",
                                   xlim=if(xlimGiven) xlim[w,] else range(u, na.rm=TRUE),
                                   ylim=if(ylimGiven) ylim[w,] else range(v, na.rm=TRUE),
                                   ...)
                          } else {
                              plot(u, v,
                                   xlab=resizableLabel("u"),
                                   ylab=resizableLabel("v"),
                                   type='n', asp=1,
                                   xlim=if(xlimGiven) xlim[w,] else range(u, na.rm=TRUE),
                                   ylim=if(ylimGiven) ylim[w,] else range(v, na.rm=TRUE),
                                   ...)
                              points(u, v, cex=cex/2, col=if (colGiven) col else "black")
                          }
                      } else {
                          smoothScatter(u, v,
                                        xlab=resizableLabel("u"),
                                        ylab=resizableLabel("v"),
                                        asp=1,
                                        xlim=if(xlimGiven) xlim[w,] else range(u, na.rm=TRUE),
                                        ylim=if(ylimGiven) ylim[w,] else range(v, na.rm=TRUE),
                                        ...)
                      }
                      xaxp <- par("xaxp")
                      xat <- seq(xaxp[1], xaxp[2], length.out=1+xaxp[3])
                      yaxp <- par("yaxp")
                      yat <- seq(yaxp[1], yaxp[2], length.out=1+yaxp[3])
                      ats <- list(xat=xat, yat=yat)

                      if (main[w] != "")
                          mtext(main[w], adj=1)
                      if (which[w] >= 29 && which[w] < 40) {
                          ok <- !is.na(u) & !is.na(v)
                          e <- eigen(cov(data.frame(u[ok],v[ok])))
                          major <- sqrt(e$values[1])  # major
                          minor <- sqrt(e$values[2])  # minor
                          theta <- seq(0, 2*pi, length.out=360/5)
                          xx <- major * cos(theta)
                          yy <- minor * sin(theta)
                          theta0 <- atan2(e$vectors[2,1], e$vectors[1,1])
                          ##cat("major", major, "minor", minor, "theta0", theta0, "\n")
                          rotate <- rbind(c(cos(theta0), -sin(theta0)),
                                          c(sin(theta0), cos(theta0)))
                          xxyy <- rotate %*% rbind(xx, yy)
                          col <- if (colGiven) col else "darkblue"
                          lines(xxyy[1,], xxyy[2,], lwd=5, col="yellow")
                          lines(xxyy[1,], xxyy[2,], lwd=2, col=col)
                          rval$ellipseMajor <- major
                          rval$ellipseMinor <- minor
                          rval$ellipseAngle <- theta
                          if (which[w] >= 30) {
                              if (!missing(control) && !is.null(control$bin)) {
                                  if (control$bin < 1)
                                      stop("cannot have control$bin less than 1, but got ", control$bin)
                                  max.bin <- dim(x@data$v)[2]
                                  if (control$bin > max.bin)
                                      stop("cannot have control$bin larger than ", max.bin," but got ", control$bin)
                                  umean <- mean(x@data$v[,control$bin,2], na.rm=TRUE)
                                  vmean <- mean(x@data$v[,control$bin,2], na.rm=TRUE)
                              } else {
                                  umean <- mean(x@data$v[,,1], na.rm=TRUE)
                                  vmean <- mean(x@data$v[,,2], na.rm=TRUE)
                              }
                              rval$meanU <- umean
                              rval$meanV <- vmean
                              arrows(0, 0, umean, vmean, lwd=5, length=1/10, col="yellow")
                              arrows(0, 0, umean, vmean, lwd=2, length=1/10, col=col)
                          }
                      }
                  } else if (which[w] == 60) {
                      oceDebug(debug, "draw(adp, ...) of type MAP\n")
                      ## get coastline file
                      if (is.character(coastline)) {
                          if (coastline == "none") {
                              if (!is.null(x@metadata$station) && !is.na(x@metadata$station)) {
                                  plot(x@metadata$longitude, x@metadata$latitude, xlab="", ylab="")
                              } else {
                                  warning("no latitude or longitude in object's metadata, so cannot draw map")
                              }
                          } else { # named coastline
                              if (!exists(paste("^", coastline, "$", sep=""))) { # load it, if necessary
                                  if (requireNamespace("ocedata", quietly=TRUE)) {
                                      if (coastline == "best") {
                                          best <- coastlineBest(span=span, debug=debug-1)
                                          data(list=best, package="oce", envir=environment())
                                          coastline <- get(best)
                                      } else if (coastline == "coastlineWorld") {
                                          data("coastlineWorld", package="oce", envir=environment())
                                          coastline <- get("coastlineWorld")
                                      } else if (coastline == "coastlineWorldFine") {
                                          data("coastlineWorldFine", package="ocedata", envir=environment())
                                          coastline <- get("coastlineWorldFine")
                                      } else if (coastline == "coastlineWorldMedium") {
                                          data("coastlineWorldMedium", package="ocedata", envir=environment())
                                          coastline <- get("coastlineWorldMedium")
                                      }  else {
                                          stop("there is no built-in coastline file of name \"", coastline, "\"")
                                      }
                                  }
                              }
                          }
                          ## FIXME: span should be an arg
                          if ("slatitude" %in% names(x@data)) {
                              lat <- x[["slatitude"]]
                              lon <- x[["slongitude"]]
                              asp <- 1 / cos(mean(lat, na.rm=TRUE) * pi / 180)
                              plot(coastline, clatitude=mean(lat, na.rm=TRUE), clongitude=mean(lon, na.rm=TRUE), span=span)
                              points(lon, lat)
                              #plot(lon, lat, asp=asp, xlab="Latitude", ylab="Longitude")
                              #lines(coastline[["longitude"]], coastline[["latitude"]], col='gray')
                          } else if ("latitude" %in% names(x@metadata)) {
                              lat <- x[["latitude"]]
                              lon <- x[["longitude"]]
                              if (is.finite(lat) && is.finite(lon)) {
                                  plot(coastline, clatitude=lat, clongitude=lon, span=50)
                                  points(x[["longitude"]], x[["latitude"]], cex=2*par('cex'))
                              } else {
                                  warning("nothing to map\n")
                              }
                          } else {
                              warning("nothing to map\n")
                          }
                      }
                  } else {
                      stop("unknown value of which (", which[w], ")")
                  }
                  if (grid)
                      grid(col=grid.col, lty=grid.lty, lwd=grid.lwd)
                  if (w <= adorn.length) {
                      t <- try(eval(adorn[w]), silent=TRUE)
                      if (class(t) == "try-error")
                          warning("cannot evaluate adorn[", w, "]\n")
                  }
              }
              par(cex=opar$cex)
              oceDebug(debug, "} # plot.adp()\n", unindent=1)
              if (exists("ats")) {
                  rval$xat <- ats$xat
                  rval$yat <- ats$yat
              }
              invisible(rval)
          })

toEnuAdp <- function(x, declination=0, debug=getOption("oceDebug"))
{
    oceDebug(debug, "toEnuAdp() {\n", unindent=1)
    coord <- x@metadata$oceCoordinate
    if (coord == "beam") {
        x <- xyzToEnuAdp(beamToXyzAdp(x, debug=debug-1), declination=declination, debug=debug-1)
    } else if (coord == "xyz") {
        x <- xyzToEnuAdp(x, declination=declination, debug=debug-1)
    } else if (coord == "enu") {
        ;
    } else {
        warning("toEnuAdp cannot convert from coordinate system ", coord, " to ENU, so returning argument as-is")
    }
    oceDebug(debug, "} # toEnuAdp()\n", unindent=1)
    x
}

beamUnspreadAdp <- function(x, count2db=c(0.45, 0.45, 0.45, 0.45), asMatrix=FALSE, debug=getOption("oceDebug"))
{
    oceDebug(debug, "beamUnspreadAdp(...) {\n", unindent=1)
    if (!inherits(x, "adp"))
        stop("method is only for objects of class '", "adp", "'")
    ## make compatible with old function name (will remove in Jan 2013)
    if (!is.null(x@metadata$oceBeamUnattenuated) && x@metadata$oceBeamUnattenuated) {
        warning("the beams are already unspreaded in this dataset.")
        return(x)
    }
    if (!is.null(x@metadata$oceBeamUnspreaded) && x@metadata$oceBeamUnspreaded) {
        warning("the beams are already unspreaded in this dataset")
        return(x)
    }
    numberOfProfiles <- dim(x@data$a)[1]
    oceDebug(debug, "numberOfProfiles=", numberOfProfiles, "\n")
    correction <- matrix(rep(20 * log10(x@data$distance), numberOfProfiles),
                         nrow=numberOfProfiles, byrow=TRUE)
    if (asMatrix) {
        res <- array(double(), dim=dim(x@data$a))
        for (beam in 1:x@metadata$numberOfBeams) {
            oceDebug(debug, "beam=",beam,"\n")
            res[,,beam] <- count2db[beam] * as.numeric(x@data$a[,,beam]) + correction
        }
    } else {
        res <- x
        for (beam in 1:x@metadata$numberOfBeams) {
            oceDebug(debug, "beam=",beam,"\n")
            tmp <- floor(count2db[beam] * as.numeric(x@data$a[,,beam]) + correction)
            tmp[tmp < 0] <- 0
            tmp[tmp > 255] <- 255
            res@data$a[,,beam] <- as.raw(tmp)
        }
        res@metadata$oceBeamUnspreaded <- TRUE
        res@processingLog <- processingLogAppend(res@processingLog, paste(deparse(match.call()), sep="", collapse=""))
    }
    oceDebug(debug, "} # beamUnspreadAdp()\n", unindent=1)
    res
}

beamToXyzAdp <- function(x, debug=getOption("oceDebug"))
{
    debug <- if (debug > 0) 1 else 0
    oceDebug(debug, "beamToXyzAdp(x, debug=", debug, ") {\n", sep="", unindent=1)
    if (!inherits(x, "adp"))
        stop("method is only for objects of class \"adp\"")
    if (x@metadata$oceCoordinate != "beam")
        stop("input must be in beam coordinates")
    if (length(grep(".*rdi.*", x@metadata$manufacturer))) {
        if (x@metadata$numberOfBeams != 4)
            stop("can only handle 4-beam ADP units from RDI")
        res <- x
        if (is.null(x@metadata$transformationMatrix))
            stop("missing x@metadata$transformationMatrix")
        oceDebug(debug, "manufacturer: rdi\n")
        tm <- x@metadata$transformationMatrix
        if (!all.equal(dim(tm), c(4,4)))
            stop("x@metadata$transformationMatrix must be a 4x4 matrix")
        if (debug) {
            cat("Transformation matrix:\n")
            print(tm)
        }
        V <- x@data$v[,,1:4]
        res@data$v[,,1] <- tm[1,1] * V[,,1] + tm[1,2] * V[,,2] + tm[1,3] * V[,,3] + tm[1,4] * V[,,4]
        res@data$v[,,2] <- tm[2,1] * V[,,1] + tm[2,2] * V[,,2] + tm[2,3] * V[,,3] + tm[2,4] * V[,,4]
        res@data$v[,,3] <- tm[3,1] * V[,,1] + tm[3,2] * V[,,2] + tm[3,3] * V[,,3] + tm[3,4] * V[,,4]
        res@data$v[,,4] <- tm[4,1] * V[,,1] + tm[4,2] * V[,,2] + tm[4,3] * V[,,3] + tm[4,4] * V[,,4]
        if ("bv" %in% names(x@data)) { # bottom velocity
            V <- x@data$bv
            res@data$bv[,1] <- tm[1,1] * V[,1] + tm[1,2] * V[,2] + tm[1,3] * V[,3] + tm[1,4] * V[,4]
            res@data$bv[,2] <- tm[2,1] * V[,1] + tm[2,2] * V[,2] + tm[2,3] * V[,3] + tm[2,4] * V[,4]
            res@data$bv[,3] <- tm[3,1] * V[,1] + tm[3,2] * V[,2] + tm[3,3] * V[,3] + tm[3,4] * V[,4]
            res@data$bv[,4] <- tm[4,1] * V[,1] + tm[4,2] * V[,2] + tm[4,3] * V[,3] + tm[4,4] * V[,4]
        }
    } else if (length(grep(".*nortek.*", x@metadata$manufacturer))) {
        if (x@metadata$numberOfBeams != 3)
            stop("can only handle 3-beam ADP units from nortek")
        if (is.null(x@metadata$transformationMatrix))
            stop("missing x@metadata$transformationMatrix")
        tm <- x@metadata$transformationMatrix
        if (!all.equal(dim(tm), c(3, 3)))
            stop("x@metadata$transformationMatrix must be a 3x3 matrix")
        oceDebug(debug, "manufacturer: nortek; transformationMatrix is as given below\n")
        if (debug > 0)
            print(tm)
        res <- x
        V <- x@data$v[,,1:3]
        res@data$v[,,1] <- tm[1,1] * V[,,1] + tm[1,2] * V[,,2] + tm[1,3] * V[,,3]
        res@data$v[,,2] <- tm[2,1] * V[,,1] + tm[2,2] * V[,,2] + tm[2,3] * V[,,3]
        res@data$v[,,3] <- tm[3,1] * V[,,1] + tm[3,2] * V[,,2] + tm[3,3] * V[,,3]
        if ("bv" %in% names(x@data)) { # bottom velocity
            V <- x@data$bv
            res@data$bv[,1] <- tm[1,1] * V[,1] + tm[1,2] * V[,2] + tm[1,3] * V[,3]
            res@data$bv[,2] <- tm[2,1] * V[,1] + tm[2,2] * V[,2] + tm[2,3] * V[,3]
            res@data$bv[,3] <- tm[3,1] * V[,1] + tm[3,2] * V[,2] + tm[3,3] * V[,3]
        }
    } else if (length(grep(".*sontek.*", x@metadata$manufacturer))) {
        if (x@metadata$numberOfBeams != 3)
            stop("can only handle 3-beam ADP units from sontek")
        if (is.null(x@metadata$transformationMatrix))
            stop("missing x@metadata$transformationMatrix")
        tm <- x@metadata$transformationMatrix
        if (!all.equal(dim(tm), c(3, 3)))
            stop("x@metadata$transformationMatrix must be a 3x3 matrix")
        oceDebug(debug, "manufacturer: sontek; transformationMatrix is as given below\n")
        if (debug > 0)
            print(tm)
        res <- x
        V <- x@data$v[,,1:3]
        res@data$v[,,1] <- tm[1,1] * V[,,1] + tm[1,2] * V[,,2] + tm[1,3] * V[,,3]
        res@data$v[,,2] <- tm[2,1] * V[,,1] + tm[2,2] * V[,,2] + tm[2,3] * V[,,3]
        res@data$v[,,3] <- tm[3,1] * V[,,1] + tm[3,2] * V[,,2] + tm[3,3] * V[,,3]
        if ("bv" %in% names(x@data)) { # bottom velocity
            V <- x@data$bv
            res@data$bv[,1] <- tm[1,1] * V[,1] + tm[1,2] * V[,2] + tm[1,3] * V[,3]
            res@data$bv[,2] <- tm[2,1] * V[,1] + tm[2,2] * V[,2] + tm[2,3] * V[,3]
            res@data$bv[,3] <- tm[3,1] * V[,1] + tm[3,2] * V[,2] + tm[3,3] * V[,3]
        }
    } else {
        stop("adp type must be either \"rdi\" or \"nortek\" or \"sontek\"")
    }
    res@metadata$oceCoordinate <- "xyz"
    res@processingLog <- processingLogAppend(res@processingLog, paste(deparse(match.call()), sep="", collapse=""))
    oceDebug(debug, "} # beamToXyzAdp()\n", unindent=1)
    res
}

xyzToEnuAdp <- function(x, declination=0, debug=getOption("oceDebug"))
{
    ##cat("adp.R:xyzToEnuAdp(): called as", paste(deparse(match.call()), sep="", collapse=""), "\n")
    debug <- if (debug > 0) 1 else 0
    oceDebug(debug, "xyzToEnuAdp(x, declination=", declination, ", debug=", debug, ") {\n", sep="", unindent=1)
    if (!inherits(x, "adp"))
        stop("method is only for objects of class '", "adp", "'")
    if (x@metadata$oceCoordinate != "xyz")
        stop("input must be in xyz coordinates")
    res <- x
    heading <- res@data$heading
    pitch <- res@data$pitch
    roll <- res@data$roll
    ## Case-by-case alteration of heading, pitch and roll, so we can use one formula for all.
    ## There are three instrumentType values, ("teledyn rdi", "nortek", and "sontek"), and
    ## three orientation values ("upward", "downward", and "sideward").
    haveBv <- "bv" %in% names(x@data)
    if (1 == length(agrep("rdi", x@metadata$manufacturer, ignore.case=TRUE))) { # "teledyn rdi"
        ## h/p/r and s/f/m from Clark Richards pers. comm. 2011-03-14, revised 2011-03-15
        if (res@metadata$orientation == "upward") {
            oceDebug(debug, "Case 1: RDI ADCP with upward-pointing sensor.\n")
            oceDebug(debug, "        Using S=-X, F=Y, and M=-Z.\n")
            ## As an alternative to the next three lines, could just add 180 degrees to roll
            starboard <- -res@data$v[,,1] # p11 "RDI Coordinate Transformation Manual" (July 1998)
            forward <- res@data$v[,,2] # p11 "RDI Coordinate Transformation Manual" (July 1998)
            mast <- -res@data$v[,,3] # p11 "RDI Coordinate Transformation Manual" (July 1998)
            if (haveBv) {              # bottom velocity
                starboardBv <- -res@data$bv[,1]
                forwardBv <- res@data$bv[,2]
                mastBv <- -res@data$bv[,3]
            }
        } else if (res@metadata$orientation == "downward") {
            oceDebug(debug, "Case 2: RDI ADCP with downward-pointing sensor.\n")
            oceDebug(debug, "        Using roll=-roll, S=X, F=Y, and M=Z.\n")
            roll <- -roll
            starboard <- res@data$v[,,1] # p11 "RDI Coordinate Transformation Manual" (July 1998)
            forward <- res@data$v[,,2] # p11 "RDI Coordinate Transformation Manual" (July 1998)
            mast <- res@data$v[,,3] # p11 "RDI Coordinate Transformation Manual" (July 1998)
            if (haveBv) {              # bottom velocity
                starboardBv <- res@data$bv[,1]
                forwardBv <- res@data$bv[,2]
                mastBv <- res@data$bv[,3]
            }
        } else {
            stop("need metadata$orientation='upward' or 'downward', not '",x@metadata$orientation,"'")
        }
    } else if (1 == length(agrep("nortek", x@metadata$manufacturer))) { # "nortek"
        ## h/p/r and s/f/m from Clark Richards pers. comm. 2011-03-14
        if (res@metadata$orientation == "upward") {
            oceDebug(debug, "Case 3: Nortek ADP with upward-pointing sensor.\n")
            oceDebug(debug, "        Using heading=heading-90, pitch=roll, roll=-pitch, S=X, F=Y, and M=Z.\n")
            heading <- heading - 90
            tmp <- pitch
            pitch <- roll
            roll <- -tmp
            starboard <- res@data$v[,,1]
            forward <- res@data$v[,,2]
            mast <- res@data$v[,,3]
            if (haveBv) {              # bottom velocity
                starboardBv <- res@data$bv[,1]
                forwardBv <- res@data$bv[,2]
                mastBv <- res@data$bv[,3]
            }
        } else if (res@metadata$orientation == "downward") {
            oceDebug(debug, "Case 4: Nortek ADP with downward-pointing sensor.\n")
            oceDebug(debug, "        Using heading=heading-90, pitch=roll, roll=-pitch, S=X, F=-Y, and M=-Z.\n")
            heading <- heading - 90
            tmp <- pitch
            pitch <- roll
            roll <- -tmp
            starboard <- res@data$v[,,1]
            forward <- -res@data$v[,,2]
            mast <- -res@data$v[,,3]
            if (haveBv) {              # bottom velocity
                starboardBv <- res@data$bv[,1]
                forwardBv <- -res@data$bv[,2]
                mastBv <- res@data$bv[,3]
            }
        } else {
            stop("need metadata$orientation='upward' or 'downward', not '",x@metadata$orientation,"'")
        }
    } else if (1 == length(agrep("sontek", x@metadata$manufacturer))) { # "sontek"
        if (res@metadata$orientation == "upward") {
            oceDebug(debug, "Case 5: Sontek ADP with upward-pointing sensor.\n")
            oceDebug(debug, "        Using heading=heading-90, pitch=-pitch, roll=-roll, S=X, F=Y, and M=Z.\n")
            heading <- heading - 90
            pitch <- -pitch
            roll <- -roll
            starboard <- res@data$v[,,1]
            forward <- res@data$v[,,2]
            mast <- res@data$v[,,3]
            if (haveBv) {              # bottom velocity
                starboardBv <- res@data$bv[,1]
                forwardBv <- res@data$bv[,2]
                mastBv <- res@data$bv[,3]
            }
        } else if (res@metadata$orientation == "downward") {
            oceDebug(debug, "Case 6: Sontek ADP with downward-pointing sensor.\n")
            oceDebug(debug, "        Using heading=heading-90, pitch=-pitch, roll=-roll, S=X, F=Y, and M=Z.\n")
            heading <- heading - 90
            pitch <- -pitch
            roll <- -roll
            starboard <- res@data$v[,,1]
            forward <- res@data$v[,,2]
            mast <- res@data$v[,,3]
            if (haveBv) {              # bottom velocity
                starboardBv <- res@data$bv[,1]
                forwardBv <- res@data$bv[,2]
                mastBv <- res@data$bv[,3]
            }
        } else {
            stop("need metadata$orientation='upward' or 'downward', not '",x@metadata$orientation,"'")
        }
    } else {
        stop("unrecognized manufacturer; should be 'teledyne rdi', 'sontek', or 'nortek', but is '",
             x@metadata$manufacturer, "'")
    }
    oceDebug(debug, vectorShow(heading, "heading (after adjustment)"))
    oceDebug(debug, vectorShow(pitch, "pitch (after adjustment)"))
    oceDebug(debug, vectorShow(roll, "roll (after adjustment)"))
    np <- dim(x@data$v)[1]           # number of profiles
    nc <- dim(x@data$v)[2]           # numberOfCells
    ## ADP and ADV calculations are both handled by sfm_enu
    for (c in 1:nc) {
        enu <- .C("sfm_enu",
                  as.integer(length(x@data$heading)), # need not equal np
                  as.double(heading + declination),
                  as.double(pitch),
                  as.double(roll),
                  as.integer(np),
                  as.double(starboard[,c]),
                  as.double(forward[,c]),
                  as.double(mast[,c]),
                  east = double(np),
                  north = double(np),
                  up = double(np),
                  NAOK=TRUE,
                  PACKAGE="oce")
        res@data$v[,c,1] <- enu$east
        res@data$v[,c,2] <- enu$north
        res@data$v[,c,3] <- enu$up
    }
    if (haveBv) {
        enu <- .C("sfm_enu",
                  as.integer(length(x@data$heading)), # need not equal np
                  as.double(heading + declination),
                  as.double(pitch),
                  as.double(roll),
                  as.integer(np),
                  as.double(starboardBv),
                  as.double(forwardBv),
                  as.double(mastBv),
                  east = double(np),
                  north = double(np),
                  up = double(np),
                  NAOK=TRUE,
                  PACKAGE="oce")
        res@data$bv[,1] <- enu$east
        res@data$bv[,2] <- enu$north
        res@data$bv[,3] <- enu$up
    }
    res@metadata$oceCoordinate <- "enu"
    res@processingLog <- processingLogAppend(res@processingLog,
                                       paste("xyzToEnu(x", ", declination=", declination, ", debug=", debug, ")", sep=""))
    oceDebug(debug, "} # xyzToEnuAdp()\n", unindent=1)
    res
}

enuToOtherAdp <- function(x, heading=0, pitch=0, roll=0)
{
    if (!inherits(x, "adp"))
        stop("method is only for objects of class '", "adp", "'")
    if (x@metadata$oceCoordinate != "enu")
        stop("input must be in enu coordinates, but it is in ", x@metadata$oceCoordinate, " coordinates")
    res <- x
    np <- dim(x@data$v)[1]           # number of profiles
    nc <- dim(x@data$v)[2]           # numberOfCells
    for (c in 1:nc) {
        other <- .C("sfm_enu",
                    as.integer(length(heading)),
                    as.double(heading),
                    as.double(pitch),
                    as.double(roll),
                    as.integer(np),
                    as.double(x@data$v[,c,1]),
                    as.double(x@data$v[,c,2]),
                    as.double(x@data$v[,c,3]),
                    v1new = double(np),
                    v2new = double(np),
                    v3new = double(np),
                    NAOK=TRUE,
                    PACKAGE="oce")
        res@data$v[,c,1] <- other$v1new
        res@data$v[,c,2] <- other$v2new
        res@data$v[,c,3] <- other$v3new
    }
    if ("bv" %in% names(x@data)) {
        other <- .C("sfm_enu",
                    as.integer(length(heading)),
                    as.double(heading),
                    as.double(pitch),
                    as.double(roll),
                    as.integer(np),
                    as.double(x@data$bv[,1]),
                    as.double(x@data$bv[,2]),
                    as.double(x@data$bv[,3]),
                    v1new = double(np),
                    v2new = double(np),
                    v3new = double(np),
                    NAOK=TRUE,
                    PACKAGE="oce")
        res@data$bv[,1] <- other$v1new
        res@data$bv[,2] <- other$v2new
        res@data$bv[,3] <- other$v3new
    }
    res@metadata$oceCoordinate <- "other"
    res@processingLog <- processingLogAppend(res@processingLog, paste(deparse(match.call()), sep="", collapse=""))
    res
}

peek.ahead <- function(file, bytes=2, debug=!TRUE)
{
    pos <- seek(file)
    res <- readBin(file, "raw", n=bytes, size=1)
    oceDebug(debug, "peeked at", paste("0x", paste(res, sep=" "), sep=""), "\n")
    seek(file, pos)
    res
}

display.bytes <- function(b, label="", ...)
{
    n <- length(b)
    cat("\n", label, " (", n, "bytes)\n", sep="", ...)
    print(b, ...)
}

subtractBottomVelocity <- function(x, debug=getOption("oceDebug"))
{
    oceDebug(debug, "subtractBottomVelocity(x) {\n", unindent=1)
    if (!("bv" %in% names(x@data))) {
        warning("there is no bottom velocity in this object")
        return(x)
    }
    rval <- x
    numberOfBeams <- dim(x@data$v)[3] # could also get from metadata but this is less brittle
    for (beam in 1:numberOfBeams) {
        oceDebug(debug, "beam #", beam, "\n")
        rval@data$v[,,beam] <- x@data$v[,,beam] - x@data$bv[,beam] 
    }
    oceDebug(debug, "} # subtractBottomVelocity()\n", unindent=1)
    rval@processingLog <- processingLogAppend(rval@processingLog, paste(deparse(match.call()), sep="", collapse=""))
    rval
}

binmapAdp <- function(x, debug=getOption("oceDebug"))
{
    oceDebug(debug, "binmap(x, debug) {\n", unindent=1)
    if (!inherits(x, "adp"))
        stop("x must be an \"adp\" object")
    v <- x[["v"]]
    a <- x[["a"]] ## FIXME: should ensure that this exist
    q <- x[["q"]]
    g <- x[["g"]]
    if (4 != dim(v)[3])
        stop("binmap() only works for 4-beam instruments")
    theta <- x[['beamAngle']]           # FIXME: check that not missing or weird
    distance <- x[["distance"]]
    roll <- x[["roll"]]
    pitch <- x[["pitch"]]
    ## Below, we loop through the profiles.  I tried an experiment in
    ## vectorizing across the loop, by combining into a single vector
    ## for (distance, cr, ...), but it was no faster, and the code was
    ## more complicated to read.
    vbm <- array(double(), dim=dim(v))
    abm <- array(raw(), dim=dim(v))
    qbm <- array(raw(), dim=dim(v))
    gbm <- array(raw(), dim=dim(v))
    nprofile <- dim(v)[1]
    rval <- x
    for (profile in 1:nprofile) {
        r <- roll[profile]
        p <- pitch[profile]
        cr <- cos(r * pi / 180)
        sr <- sin(r * pi / 180)
        cp <- cos(p * pi / 180)
        sp <- sin(p * pi / 180)
        tt <- tan(theta * pi / 180)
        z1 <- distance * (cr - tt * sr) * cp

        ##if (profile == 1) {
        ##    cat('R : r', r, 'p', p, 'cr', cr, 'sr', sr, 'cp', cp, 'sp', sp, 'tt', tt, '\n') 
        ##    cat("R : z1      ", format(z1[1:8], width=11, digits=7), '\n')
        ##}

        z2 <- distance * (cr + tt * sr) * cp
        z3 <- distance * (cp + tt * sp) * cr
        z4 <- distance * (cp - tt * sp) * cr
        ## FIXME: check on whether we can speed things up by using e.g. x[["v"]]
        ## instead of v, which would lower the memory requirements.

        ## v=velocity
        ## Need to check all four beams that there are more than 2
        ## non-NA values in the profiles, otherwise set to 0
        checkNA <- sum(!is.na(v[profile,,1])) > 1 & sum(!is.na(v[profile,,2])) > 1 & sum(!is.na(v[profile,,3])) > 1 & sum(!is.na(v[profile,,4])) > 1
        if (checkNA) {
            vbm[profile,,1] <- approx(z1, v[profile,,1], distance)$y
            vbm[profile,,2] <- approx(z2, v[profile,,2], distance)$y
            vbm[profile,,3] <- approx(z3, v[profile,,3], distance)$y
            vbm[profile,,4] <- approx(z4, v[profile,,4], distance)$y
        } else {
            vbm[profile,,1] <- NA
            vbm[profile,,2] <- NA
            vbm[profile,,3] <- NA
            vbm[profile,,4] <- NA
        }
        ## a
        rule <- 2                      # FIXME: is is OK to extend data to edges?
        abm[profile,,1] <- oce.as.raw(approx(z1, as.numeric(a[profile,,1], rule=rule), distance)$y)
        abm[profile,,2] <- oce.as.raw(approx(z2, as.numeric(a[profile,,2], rule=rule), distance)$y)
        abm[profile,,3] <- oce.as.raw(approx(z3, as.numeric(a[profile,,3], rule=rule), distance)$y)
        abm[profile,,4] <- oce.as.raw(approx(z4, as.numeric(a[profile,,4], rule=rule), distance)$y)
        ## q
        qbm[profile,,1] <- oce.as.raw(approx(z1, as.numeric(q[profile,,1], rule=rule), distance)$y)
        qbm[profile,,2] <- oce.as.raw(approx(z2, as.numeric(q[profile,,2], rule=rule), distance)$y)
        qbm[profile,,3] <- oce.as.raw(approx(z3, as.numeric(q[profile,,3], rule=rule), distance)$y)
        qbm[profile,,4] <- oce.as.raw(approx(z4, as.numeric(q[profile,,4], rule=rule), distance)$y)
        ## g
        gbm[profile,,1] <- oce.as.raw(approx(z1, as.numeric(g[profile,,1], rule=rule), distance)$y)
        gbm[profile,,2] <- oce.as.raw(approx(z2, as.numeric(g[profile,,2], rule=rule), distance)$y)
        gbm[profile,,3] <- oce.as.raw(approx(z3, as.numeric(g[profile,,3], rule=rule), distance)$y)
        gbm[profile,,4] <- oce.as.raw(approx(z4, as.numeric(g[profile,,4], rule=rule), distance)$y)
    }
    rval@data$v <- vbm
    ##cat("R : v1      ", format(v[1,1:8,1], width=11, digits=7), '\n')
    ##cat("R : V1      ", format(vbm[1,1:8,1], width=11, digits=7), '\n')
    rval@data$a <- abm
    rval@data$q <- qbm
    rval@data$g <- gbm
    rval
}

