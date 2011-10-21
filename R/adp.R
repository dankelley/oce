## vim: tw=120 shiftwidth=4 softtabstop=4 expandtab:
setMethod(f="initialize",
          signature="adp",
          definition=function(.Object,time,u,a,q,filename) {
              if (!missing(time)) .Object@data$time <- time
              if (!missing(u)) .Object@data$u <- u
              if (!missing(a)) .Object@data$a <- a 
              if (!missing(q)) .Object@data$q <- q
              .Object@metadata$filename <- if (missing(filename)) "" else filename
              .Object@processingLog$time=c(.Object@processingLog$time, Sys.time())
              .Object@processingLog$value=c(.Object@processingLog$value, "create 'ctd' object")
              return(.Object)
          })

setMethod(f="[[",
          signature="adp",
          definition=function(x, i, j, drop) {
              if (i == "filename") return(x@metadata$filename)
              else if (i == "u1") return(x@data$v[,,1])
              else if (i == "u2") return(x@data$v[,,2])
              else if (i == "u3") return(x@data$v[,,3])
              else if (i == "u4") return(x@data$v[,,4])
              else if (i == "time") return(x@data$time)
              else if (i == "pressure") return(x@data$pressure)
              else if (i == "temperature") return(x@data$temperature)
              else if (i == "distance") return(x@data$distance)
              else if (i == "latitude") return(x@metadata$latitude)
              else if (i == "longitude") return(x@metadata$longitude)
              else stop("cannot access \"", i, "\"") # cannot get here
          })

setMethod(f="show",
          signature="adp",
          definition=function(object) {
              filename <- object[["filename"]]
              if (is.null(filename) || filename == "")
                  cat("ADP has column data\n", sep="")
              else
                  cat("ADP from file '", object[["filename"]], "' has column data\n", sep="")
              names <- names(object@data)
              ncol <- length(names)
              for (i in 1:ncol) {
                  cat(vectorShow(object@data[[i]], paste("  ", names[i])))
                  dim <- dim(object@data[[i]])
                  if (!is.null(dim)) {
                      if (length(dim) == 2)
                          cat("      (actually, the above is a matrix of dimension ", dim[1], " by ", dim[2], ")\n", sep="")
                      else if (length(dim) == 3)
                          cat("      (actually, the above is a matrix of dimension ", dim[1], " by ", dim[2], " by ", dim[3], ")\n", sep="")
                  }
              }
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
    rval@processingLog <- unclass(processingLog(rval@processingLog, paste(deparse(match.call()), sep="", collapse="")))
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
    rval@processingLog <- unclass(processingLog(rval@processingLog, paste(deparse(match.call()), sep="", collapse="")))
    rval 
}

removeShipMotion <- function(x)
{
    rval <- x
    if (!("bottomRange" %in% names(x@data)))
        return(rval)
    numberOfBeams <- dim(x@data$v)[3] # could also get from metadata but this is less brittle
    for (beam in 1:numberOfBeams) {
        rval@data$v[,,beam] <- rval@data$v[,,beam] - rval@data$bottomVelocity[,beam]
    }
    rval@processingLog <- unclass(processingLog(rval@processingLog, paste(deparse(match.call()), sep="", collapse="")))
    rval
}

coordinate <- function(x)
{
    if (inherits(x, "adp") || inherits(x, "adv"))
        x@metadata$oceCoordinate
    else {
        warning("unknown file type; the object must inherit from either \"adv\" or \"adp\"")
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
    if (x@metadata$oceCoordinate == "beam")
        c("beam 1", "beam 2", "beam 3", "beam 4")[which]
    else if (x@metadata$oceCoordinate == "enu")
        c("east", "north", "up", "error")[which]
    else if (x@metadata$oceCoordinate == "xyz")
        c("u", "v", "w", "e")[which]
    else if (x@metadata$oceCoordinate == "other")
        c("u'", "v'", "w'", "e")[which]
    else " "
}

read.adp <- function(file, from=1, to, by=1, tz=getOption("oceTz"),
                     latitude=NA, longitude=NA,
                     manufacturer=c("rdi", "nortek", "sontek"),
                     debug=getOption("oceDebug"), monitor=FALSE, despike=FALSE, processingLog,
                     ...)
{
    oceDebug(debug, "read.adp(...,from=",from,",to=",if (missing(to)) "(missing)" else to,",by=",by,"type=",type,",...)\n")
    type <- match.arg(type)
    if (monitor)
        cat(file, "\n", ...)
    if (manufacturer == "rdi")
        read.adp.rdi(file=file, from=from, to=to, by=by, tz=tz,
                     latitude=latitude, longitude=longitude,
                     debug=debug-1, monitor=monitor, despike=despike,
                     processingLog=processingLog, ...)
    else if (type == "nortek")
        read.adp.nortek(file=file, from=from, to=to, by=by, tz=tz,
                        latitude=latitude, longitude=longitude,
                        debug=debug-1, monitor=monitor, despike=despike,
                        processingLog=processingLog, ...)
    else if (type == "sontek")
        read.adp.sontek(file=file, from=from, to=to, by=by, tz=tz,
                        latitude=latitude, longitude=longitude,
                        debug=debug-1, monitor=monitor, despike=despike,
                        processingLog=processingLog, ...)
}

summary.adp <- function(object, ...)
{
    if (!inherits(object, "adp"))
        stop("method is only for adp objects")
    cat("ADP Summary\n-----------\n\n", ...)
    cat(paste("* Instrument:         ", object@metadata$instrumentType, "\n", sep=""), ...)
    cat("* Manufacturer:      ", object@metadata$manufacturer, "\n")
    cat(paste("* Serial number:      ", object@metadata$serialNumber, "\n", sep=""), ...)
    cat(paste("* Source filename:    ``", object@metadata$filename, "``\n", sep=""), ...)
    if ("latitude" %in% names(object@metadata)) {
        cat(paste("* Location:           ",
                  if (is.na(object@metadata$latitude)) "unknown latitude" else sprintf("%.5f N", object@metadata$latitude), ", ",
                  if (is.na(object@metadata$longitude)) "unknown longitude" else sprintf("%.5f E", object@metadata$longitude), "\n"))
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
                            oceBeamUnattenuated=object@metadata$oceBeamUnattenuated,
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
    cat(sprintf("* Cells:              %d, centered at %.3f m to %.3f m, spaced by %.3f m\n",
                object@metadata$numberOfCells, object@data$distance[1],  object@data$distance[length(object@data$distance)], diff(object@data$distance[1:2])),  ...)
    cat("* Coordinate system: ", object@metadata$coordinateSystem, "[originally],", object@metadata$oceCoordinate, "[presently]\n", ...)
    cat("* Frequency:         ", object@metadata$frequency, "kHz\n", ...)
    cat("* Beams:             ", object@metadata$numberOfBeams, if (!is.null(object@metadata$oceBeamUnattenuated) & object@metadata$oceBeamUnattenuated) "beams (attenuated)" else "beams (not attenuated)",
        "oriented", object@metadata$orientation, "with angle", object@metadata$beam.angle, "deg to axis\n", ...)
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
    res$oceBeamUnattenuated <- object@metadata$oceBeamUnattenuated
    res$beamAngle <- object@metadata$beamAngle
    res$beamConfig <- object@metadata$beamConfig
    res$transformationMatrix <- object@metadata$transformationMatrix
    res$orientation <- object@metadata$orientation
    res$coordinateSystem <- object@metadata$coordinateSystem
    res$oceCoordinate <- object@metadata$oceCoordinate
    res$processingLog <- object@processingLog
    dataNames <- names(object@data)
    threes <- matrix(nrow=(-1+length(dataNames)), ncol=3)
    ii <- 1
    for (i in 1:length(dataNames)) {
        if (names(object@data)[i] != "time") {
            threes[ii,] <- threenum(object@data[[dataNames[i]]])
            ii <- ii + 1
        }
    }
    rownames(threes) <- c(dataNames[dataNames != "time"])
    colnames(threes) <- c("Min.", "Mean", "Max.")
    cat("* Statistics of subsample::\n\n")
    print(threes)
}

plot.adp <- function(x, which=1:dim(x@data$v)[3],
                     col,
                     zlim,
                     titles,
                     lwd=par('lwd'),
                     type='l',
                     ytype=c("profile", "distance"),
                     adorn=NULL,
                     drawTimeRange=getOption("oceDrawTimeRange"),
                     useSmoothScatter,
                     mgp=getOption("oceMgp"),
                     mar=c(mgp[1]+1.5,mgp[1]+1.5,1.5,1.5),
                     marginsAsImage=FALSE,
                     cex=par("cex"), cex.axis=par("cex.axis"), cex.main=par("cex.main"),
                     xlim, ylim,
                     control,
                     useLayout=FALSE,
                     main="",
                     debug=getOption("oceDebug"),
                     ...)
{
    debug <- max(0, min(debug, 4))
    rval <- NULL
    oceDebug(debug, "\b\bplot.adp(x, which=", paste(which, collapse=","), ") {\n", sep="")
    oceDebug(debug, "early in plot.adp:\n")
    oceDebug(debug, "  par(mar)=", paste(par('mar'), collapse=" "), "\n")
    oceDebug(debug, "  par(mai)=", paste(par('mai'), collapse=" "), "\n")
    gave.col <- !missing(col)
    if (!missing(ylim))
        oceDebug(debug, "ylim=c(", paste(ylim, collapse=", "), ")\n")
    if (!inherits(x, "adp"))
        stop("method is only for adp objects")
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
    par(mgp=mgp, mar=mar, cex=cex)
    dots <- list(...)
    ytype <- match.arg(ytype)
    ## user may specify a matrix for xlim and ylim
    gave.ylim <- !missing(ylim)
    oceDebug(debug, 'gave.ylim=', gave.ylim, '\n')
    if (gave.ylim) {
        if (is.matrix(ylim)) {
            if (dim(ylim)[2] != nw) {
                ylim2 <- matrix(ylim, ncol=2, nrow=nw) # FIXME: is this what I want?
            }
        } else {
            ylim2 <- matrix(ylim, ncol=2, nrow=nw) # FIXME: is this what I want?
        }
        class(ylim2) <- class(ylim)
        ylim <- ylim2
    }
    gave.xlim <- !missing(xlim)
    if (gave.xlim) {
        if (is.matrix(xlim)) {
            if (dim(xlim)[2] != nw) {
                xlim2 <- matrix(xlim, ncol=2, nrow=nw) # FIXME: is this what I want?
            }
        } else {
            if (length(xlim) != 2)
                stop("xlim must be a vector of length 2, or a 2-column matrix")
            xlim2 <- matrix(xlim[1:2], ncol=2, nrow=nw, byrow=TRUE)
        }
        xlim <- xlim2
    }
    if (missing(zlim)) {
        gave.zlim <- FALSE
        zlim.given <- NULL
    } else {
        gave.zlim <- TRUE
        if (is.vector(zlim)) {
            if (length(zlim) == 2) {
                zlim.given <- matrix(rep(zlim, length(which)),ncol=2,byrow=TRUE)
            } else {
                stop("zlim must be a vector of length 2, or a matrix with 2 columns")
            }
        } else {
            ## FIXME: should this be made into a matrix?
            zlim.given <- zlim
        }
    }
    ylim.given <- if (gave.ylim) dots[["ylim"]] else NULL
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

    ## Translate word-style (FIXME: ugly coding)
    which2 <- vector("numeric", length(which))
    for (w in 1:nw) {
        ww <- which[w]
        if (is.numeric(ww) || 1 == length(grep("^[0-9]*$", ww))) {
            which2[w] <- as.numeric(ww)
        } else {
            if (     ww == "u1") which2[w] <- 1
            else if (ww == "u2") which2[w] <- 2
            else if (ww == "u3") which2[w] <- 3
            else if (ww == "u4") which2[w] <- 4
            else if (ww == "a1") which2[w] <- 5
            else if (ww == "a2") which2[w] <- 6
            else if (ww == "a3") which2[w] <- 7
            else if (ww == "a4") which2[w] <- 8
            else if (ww == "q1") which2[w] <- 9
            else if (ww == "q2") which2[w] <- 10
            else if (ww == "q3") which2[w] <- 11
            else if (ww == "q4") which2[w] <- 12
            else if (ww == "salinity") which2[w] <- 13
            else if (ww == "temperature") which2[w] <- 14
            else if (ww == "pressure") which2[w] <- 15
            else if (ww == "heading") which2[w] <- 16
            else if (ww == "pitch") which2[w] <- 17
            else if (ww == "roll") which2[w] <- 18
            ## 19 beam-1 correlation-amplitude diagnostic plot
            ## 20 beam-2 correlation-amplitude diagnostic plot
            ## 21 beam-3 correlation-amplitude diagnostic plot
            ## 22 beam-4 correlation-amplitude diagnostic plot
            else if (ww == "progressive vector") which2[w] <- 23
            else if (ww == "uv") which2[w] <- 28
            else if (ww == "uv+ellipse") which2[w] <- 29
            else if (ww == "uv+ellipse+arrow") which2[w] <- 30
            ## 40 to 44 only work for bottom-tracking devices
            else if (ww == "bottom range" ) which2[w] <- 40 # average of all beams
            else if (ww == "bottom range1") which2[w] <- 41 # beam1
            else if (ww == "bottom range2") which2[w] <- 42 # beam2
            else if (ww == "bottom range3") which2[w] <- 43 # beam3
            else if (ww == "bottom range4") which2[w] <- 44 # beam4 (if there is one)
            ## 50 to 54 only work for bottom-tracking devices
            else if (ww == "bottom velocity" ) which2[w] <- 50 # average of all beams
            else if (ww == "bottom velocity1") which2[w] <- 51 # beam1
            else if (ww == "bottom velocity2") which2[w] <- 52 # beam2
            else if (ww == "bottom velocity3") which2[w] <- 53 # beam3
            else if (ww == "bottom velocity4") which2[w] <- 54 # beam4 (if there is one)
            else if (ww == "heaving") which2[w] <- 55
            else stop("unknown 'which':", ww)
        }
    }
    which <- which2
    images <- 1:12
    timeseries <- c(13:22, 40:44, 50:54, 55)
    spatial <- 23:27
    speed <- 28

    adorn.length <- length(adorn)
    if (adorn.length == 1) {
        adorn <- rep(adorn, nw)
        adorn.length <- nw
    }

    tt <- x@data$time
    class(tt) <- "POSIXct"              # otherwise image() gives warnings
    if (gave.zlim && all(which %in% 5:8)) { # single scale for all
        zlim <- range(abs(as.numeric(x@data[,,which[1]])), na.rm=TRUE) # FIXME name of item missing, was ma
        for (w in 2:length(which)) {
            zlim <- range(abs(c(zlim, x@data[[which[w]]])), na.rm=TRUE) # FIXME: check name
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
    flip.y <- ytype == "profile" && x@metadata$orientation == "downward"
    haveTimeImages <- any(which %in% images)
    oceDebug(debug, 'haveTimeImages=', haveTimeImages, '(if TRUE, it means any timeseries graphs get padding on RHS)\n')
    for (w in 1:nw) {
        oceDebug(debug, "which[", w, "]=", which[w], "; drawTimeRange=", drawTimeRange, "\n")
        if (which[w] %in% images) {                   # image types
            skip <- FALSE
            if (which[w] %in% 1:(x@metadata$numberOfBeams)) {    #velocity
                oceDebug(debug, "a velocity component image")
                z <- x@data$v[,,which[w]]
                y.look <- if (gave.ylim) ylim.given[1] <= x@data$distance & x@data$distance <= ylim.given[2] else rep(TRUE, length(x@data$distance))
                zlim <- if (gave.zlim) zlim.given[w,] else max(abs(x@data$v[,y.look,which[w]]), na.rm=TRUE) * c(-1,1)
                zlab <- if (missing(titles)) beamName(x, which[w]) else titles[w]
            } else if (which[w] %in% 5:(4+x@metadata$numberOfBeams)) { # amplitude
                z <- as.numeric(x@data$a[,,which[w]-4])
                dim(z) <- dim(x@data$a)[1:2]
                y.look <- if (gave.ylim)
                    ylim.given[1] <= x@data$distance & x@data$distance <= ylim.given[2]
                else
                    rep(TRUE, length(x@data$distance))
                zlim <- range(as.numeric(x@data$a[,y.look,]), na.rm=TRUE)
                zlab <- c(expression(a[1]),expression(a[2]),expression(a[3]),expression(a[4]))[which[w]-4]
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
            } else {
                skip <- TRUE
            }
            if (!skip) {
                imagep(x=tt, y=x@data$distance, z=z,
                       zlim=zlim,
                       flip.y=flip.y,
                       col=if (gave.col) col else oceColorsPalette(128, 1),
                       ylab=resizableLabel("distance"),
                       xlab="Time",
                       zlab=zlab,
                       drawTimeRange=drawTimeRange,
                       drawContours=FALSE,
                       adorn=adorn[w],
                       mgp=mgp,
                       mar=mar,
                       cex=cex*(1 - min(nw / 8, 1/4)), # FIXME: should emulate par(mfrow)
                       main=main[w],
                       debug=debug-1,
                       ...)
            }
            if (showBottom)
                lines(x@data$time, bottom)
            drawTimeRange <- FALSE
        } else if (which[w] %in% timeseries) { # time-series types
            col <- if (gave.col) rep(col, length.out=nw) else rep("black", length.out=nw)
            oceDebug(debug, "graph", w, "is a timeseries\n")
            ##par(mgp=mgp, mar=mar, cex=cex)
            tlim <- range(x@data$time)
            if (which[w] == 13) {
                if (haveTimeImages)
                    drawPalette(debug=debug-1)
                oce.plot.ts(x@data$time, x@data$salinity,
                            xlim=if(gave.xlim) xlim[w,] else tlim,
                            ylim=if(gave.ylim) ylim[w,],
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
                            drawTimeRange=drawTimeRange, adorn=adorn[w])
            }
            if (which[w] == 14) {
                if (haveTimeImages)
                    drawPalette(debug=debug-1)
                oce.plot.ts(x@data$time, x@data$temperature,
                            xlim=if(gave.xlim) xlim[w,] else tlim,
                            ylim=if(gave.ylim) ylim[w,],
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
                            drawTimeRange=drawTimeRange,
                            adorn=adorn[w],
                            debug=debug-1)
            }
            if (which[w] == 15) {
                if (haveTimeImages)
                    drawPalette(debug=debug-1)
                oce.plot.ts(x@data$time, x@data$pressure,
                            xlim=if(gave.xlim) xlim[w,] else tlim,
                            ylim=if(gave.ylim) ylim[w,],
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
                            drawTimeRange=drawTimeRange, adorn=adorn[w])
            }
            if (which[w] == 16) {
                if (haveTimeImages)
                    drawPalette(debug=debug-1)
                oce.plot.ts(x@data$time, x@data$heading,
                            xlim=if(gave.xlim) xlim[w,] else tlim,
                            ylim=if(gave.ylim) ylim[w,],
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
                            drawTimeRange=drawTimeRange, adorn=adorn[w])
            }
            if (which[w] == 17) {
                if (haveTimeImages)
                    drawPalette(debug=debug-1)
                oce.plot.ts(x@data$time, x@data$pitch,
                            xlim=if(gave.xlim) xlim[w,] else tlim,
                            ylim=if(gave.ylim) ylim[w,],
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
                            drawTimeRange=drawTimeRange, adorn=adorn[w])
            }
            if (which[w] == 18) {
                if (haveTimeImages)
                    drawPalette(debug=debug-1)
                oce.plot.ts(x@data$time, x@data$roll,
                            xlim=if(gave.xlim) xlim[w,] else tlim,
                            ylim=if(gave.ylim) ylim[w,],
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
                            drawTimeRange=drawTimeRange, adorn=adorn[w])
            }
            if (which[w] == 19) {
                if (haveTimeImages)
                    drawPalette(debug=debug-1)
                if (x@metadata$numberOfBeams > 0)
                    oce.plot.ts(x@data$time, apply(x@data$v[,,1], 1, mean, na.rm=TRUE),
                                xlim=if(gave.xlim) xlim[w,] else tlim,
                                ylim=if(gave.ylim) ylim[w,],
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
                                drawTimeRange=drawTimeRange,
                                adorn=adorn[w], ...)
                    else
                        warning("cannot plot beam/velo 1 because the device no beams")
            }
            if (which[w] == 20) {
                if (haveTimeImages)
                    drawPalette(debug=debug-1)
                if (x@metadata$numberOfBeams > 1)
                    oce.plot.ts(x@data$time, apply(x@data$v[,,2], 1, mean, na.rm=TRUE),
                                xlim=if(gave.xlim) xlim[w,] else tlim,
                                ylim=if(gave.ylim) ylim[w,],
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
                                drawTimeRange=drawTimeRange,
                                adorn=adorn[w], ...)
                    else
                        warning("cannot plot beam/velo 2 because the device has only ", x@metadata$numberOfBeams, " beams")
            }
            if (which[w] == 21) {
                if (haveTimeImages)
                    drawPalette(debug=debug-1)
                if (x@metadata$numberOfBeams > 2)
                    oce.plot.ts(x@data$time, apply(x@data$v[,,3], 1, mean, na.rm=TRUE),
                                xlim=if(gave.xlim) xlim[w,] else tlim,
                                ylim=if(gave.ylim) ylim[w,],
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
                                drawTimeRange=drawTimeRange,
                                adorn=adorn[w], ...)
                    else
                        warning("cannot plot beam/velo 3 because the device has only", x@metadata$numberOfBeams, "beams")
            }
            if (which[w] == 22) {
                if (haveTimeImages)
                    drawPalette(debug=debug-1)
                if (x@metadata$numberOfBeams > 3)
                    oce.plot.ts(x@data$time, apply(x@data$v[,,4], 1, mean, na.rm=TRUE),
                                xlim=if(gave.xlim) xlim[w,] else tlim,
                                ylim=if(gave.ylim) ylim[w,],
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
                                drawTimeRange=drawTimeRange,
                                adorn=adorn[w], ...)
                    else
                        warning("cannot plot beam/velo 4 because the device has only", x@metadata$numberOfBeams, "beams")
            }
            if (which[w] %in% 55) { # heaving
                if (haveTimeImages)
                    drawPalette(debug=debug-1)
                dt <- as.numeric(x@data$time[2]) - as.numeric(x@data$time[1])
                oce.plot.ts(x@data$time, dt * cumsum(apply(x@data$v[,,3], 1, mean)),
                            xlim=if(gave.xlim) xlim[w,] else tlim,
                            ylim=if(gave.ylim) ylim[w,],
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
                            drawTimeRange=drawTimeRange,
                            adorn=adorn[w], ...)
                drawTimeRange <- FALSE
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
                dt <- as.numeric(difftime(x@data$time[2], x@data$time[1],units="sec")) # FIXME: should not assume all equal
                m.per.km <- 1000
                if (!missing(control) && !is.null(control$bin)) {
                    if (control$bin < 1)
                        stop("cannot have control$bin less than 1, but got ", control$bin)
                    max.bin <- dim(x@data$v)[2]
                    if (control$bin > max.bin)
                        stop("cannot have control$bin larger than ", max.bin," but got ", control$bin)
                    u <- x@data$v[,control$bin,1]
                    v <- x@data$v[,control$bin,2]
                } else {
                    u <- apply(x@data$v[,,1], 1, mean, na.rm=TRUE)
                    v <- apply(x@data$v[,,2], 1, mean, na.rm=TRUE)
                }
                u[is.na(u)] <- 0        # zero out missing
                v[is.na(v)] <- 0
                x.dist <- cumsum(u) * dt / m.per.km
                y.dist <- cumsum(v) * dt / m.per.km
                plot(x.dist, y.dist, xlab="km", ylab="km", type='l', asp=1, col=if (gave.col) col else "black", ...)
            } else if (which[w] == 24) {
                par(mar=c(mgp[1]+1,mgp[1]+1,1,1))
                value <- apply(x@data$v[,,1], 2, mean, na.rm=TRUE)
                plot(value, x@data$distance, xlab=beamName(x, 1), ylab="Distance [m]", type='l', ...)
            } else if (which[w] == 25) {
                par(mar=c(mgp[1]+1,mgp[1]+1,1,1))
                value <- apply(x@data$v[,,2], 2, mean, na.rm=TRUE)
                plot(value, x@data$distance, xlab=beamName(x, 2), ylab="Distance [m]", type='l', ...)
            } else if (which[w] == 26) {
                par(mar=c(mgp[1]+1,mgp[1]+1,1,1))
                value <- apply(x@data$v[,,3], 2, mean, na.rm=TRUE)
                plot(value, x@data$distance, xlab=beamName(x, 3), ylab="Distance [m]", type='l', ...)
                ##grid()
            } else if (which[w] == 27) {
                if (x@metadata$numberOfBeams > 3) {
                    par(mar=c(mgp[1]+1,mgp[1]+1,1,1))
                    value <- apply(x@data$v[,,4], 2, mean, na.rm=TRUE)
                    plot(value, x@data$distance, xlab=beamName(x, 4), ylab="Distance [m]", type='l', ...)
                    ##grid()
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
                u <- apply(x@data$v[,,1], 1, mean, na.rm=TRUE)
                v <- apply(x@data$v[,,2], 1, mean, na.rm=TRUE)
            }
            oceDebug(debug, "uv type plot\n")
            if (n < 5000 || (!missing(useSmoothScatter) && !useSmoothScatter)) {
                if ("type" %in% names(dots)) {
                    plot(u, v, xlab="u [m/s]", ylab="v [m/s]", asp=1, col=if (gave.col) col else "black",
                         xlim=if(gave.xlim) xlim[w,] else range(u, na.rm=TRUE),
                         ylim=if(gave.ylim) ylim[w,] else range(v, na.rm=TRUE),
                         ...)
                } else {
                    plot(u, v, xlab="u [m/s]", ylab="v [m/s]", type='n', asp=1,
                         xlim=if(gave.xlim) xlim[w,] else range(u, na.rm=TRUE),
                         ylim=if(gave.ylim) ylim[w,] else range(v, na.rm=TRUE),
                         ...)
                    points(u, v, cex=cex/2, col=if (gave.col) col else "black")
                }
            } else {
                smoothScatter(u, v, xlab="u [m/s]", ylab="v [m/s]", asp=1,
                              xlim=if(gave.xlim) xlim[w,] else range(u, na.rm=TRUE),
                              ylim=if(gave.ylim) ylim[w,] else range(v, na.rm=TRUE),
                              ...)
            }
            if (main[w] != "")
                mtext(main[w], adj=1)
            if (which[w] >= 29) {
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
                col <- if (gave.col) col else "darkblue"
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
        } else if (which[w] %in% 40:44) { # bottomRange
            par(mar=c(mgp[1]+1,mgp[1]+1,1,1))
            n <- prod(dim(x@data$v)[1:2])
            if ("bottomRange" %in% names(x@data)) {
                if (which[w] == 40)
                    oce.plot.ts(x@data$time, apply(x@data$bottomRange, 1, mean, na.rm=TRUE), ylab="Range [m]")
                else if (which[w] == 41)
                    oce.plot.ts(x@data$time, x@data$bottomRange[,1], ylab="Beam 1 range [m]")
                else if (which[w] == 42)
                    oce.plot.ts(x@data$time, x@data$bottomRange[,2], ylab="Beam 1 range [m]")
                else if (which[w] == 43)
                    oce.plot.ts(x@data$time, x@data$bottomRange[,3], ylab="Beam 1 range [m]")
                else if (which[w] == 44)
                    oce.plot.ts(x@data$time, x@data$bottomRange[,4], ylab="Beam 1 range [m]")
            } else {
                warning("cannot handle which= ", which[w], " because this instrument lacked bottom tracking")
            }
        } else if (which[w] %in% 50:54) { # bottom velocity
            par(mar=c(mgp[1]+1,mgp[1]+1,1,1))
            n <- prod(dim(x@data$v)[1:2])
            if ("bottom velocity" %in% names(x@data)) {
                if (which[w] == 50)
                    oce.plot.ts(x@data$time, apply(x@data$bottomVelocity, 1, mean, na.rm=TRUE), ylab="Range [m]")
                else if (which[w] == 51)
                    oce.plot.ts(x@data$time, x@data$bottomVelocity[,1], ylab="Beam 1 velocity [m/s]")
                else if (which[w] == 52)
                    oce.plot.ts(x@data$time, x@data$bottomVelocity[,2], ylab="Beam 2 velocity [m/s]")
                else if (which[w] == 53)
                    oce.plot.ts(x@data$time, x@data$bottomVelocity[,3], ylab="Beam 3 velocity [m/s]")
                else if (which[w] == 54)
                    oce.plot.ts(x@data$time, x@data$bottomVelocity[,4], ylab="Beam 4 velocity [m/s]")
            } else {
                warning("cannot handle which= ", which[w], " because this instrument lacked bottom tracking")
            }
        } else {
            stop("unknown value of which (", which[w], ")")
        }
        if (w <= adorn.length) {
            t <- try(eval(adorn[w]), silent=TRUE)
            if (class(t) == "try-error")
                warning("cannot evaluate adorn[", w, "]\n")
        }
    }
    par(cex=opar$cex)
    oceDebug(debug, "\b\b\b} # plot.adp()\n")
    invisible(rval)
}

toEnuAdp <- function(x, declination=0, debug=getOption("oceDebug"))
{
    oceDebug(debug, "\b\badp.2enu() {\n")
    coord <- x@metadata$oceCoordinate
    if (coord == "beam") {
        x <- xyzToEnuAdp(beamToXyzAdp(x, debug=debug-1), declination=declination, debug=debug-1)
    } else if (coord == "xyz") {
        x <- xyzToEnuAdp(x, declination=declination, debug=debug-1)
    } else if (coord == "enu") {
        ;
    } else {
        warning("adp.2enu cannot convert from coordinate system ", coord, " to ENU, so returning argument as-is")
    }
    oceDebug(debug, "\b\b} # adp.2enu()\n")
    x
}

beamUnattenuateAdp <- function(x, count2db=c(0.45, 0.45, 0.45, 0.45), debug=getOption("oceDebug"))
{
    oceDebug(debug, "\b\bbeamUnattenuateAdp(...) {\n")
    if (!inherits(x, "adp"))
        stop("method is only for adp objects")
    if (x@metadata$oceBeamUnattenuated)
        stop("the beams are already unattenuated in this dataset")
    res <- x
    numberOfProfiles <- dim(x@data$a)[1]
    oceDebug(debug, "numberOfProfiles=", numberOfProfiles, "\n")
    correction <- matrix(rep(20 * log10(x@data$distance), numberOfProfiles),
                         nrow=numberOfProfiles, byrow=TRUE)
    for (beam in 1:x@metadata$numberOfBeams) {
        oceDebug(debug, "beam=",beam,"\n")
        tmp <- floor(count2db[beam] * as.numeric(x@data$a[,,beam]) + correction)
        tmp[tmp < 0] <- 0
        tmp[tmp > 255] <- 255
        res@data$a[,,beam] <- as.raw(tmp)
    }
    res@metadata$oceBeamUnattenuated <- TRUE
    res@processingLog <- unclass(processingLog(res@processingLog, paste(deparse(match.call()), sep="", collapse="")))
    oceDebug(debug, "\b\b} # beamUnattenuateAdp()\n")
    res
}

beamToXyzAdp <- function(x, debug=getOption("oceDebug"))
{
    debug <- if (debug > 0) 1 else 0
    oceDebug(debug, "\b\bbeamToXyzAdp(x, debug=", debug, ") {\n", sep="")
    if (!inherits(x, "adp"))
        stop("method is only for objects of class \"adp\"")
    if (x@metadata$oceCoordinate != "beam")
        stop("input must be in beam coordinates")
    if (grep(".*rdi.*", x@metadata$manufacturer)) {
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
    } else if (grep(".*nortek.*", x@metadata$manufacturer)) {
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
    } else if (grep(".*sontek.*", x@metadata$manufacturer)) {
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
    } else {
        stop("adp type must be either \"rdi\" or \"nortek\" or \"sontek\"")
    }
    res@metadata$oceCoordinate <- "xyz"
    res@processingLog <- unclass(processingLog(res@processingLog, paste(deparse(match.call()), sep="", collapse="")))
    oceDebug(debug, "\b\b\b} # beamToXyzAdp()\n")
    res
}

xyzToEnuAdp <- function(x, declination=0, debug=getOption("oceDebug"))
{
    debug <- if (debug > 0) 1 else 0
    oceDebug(debug, "\b\bxyzToEnuAdp(x, declination=", declination, ", debug=", debug, ") {\n", sep="")
    if (!inherits(x, "adp"))
        stop("method is only for adp objects")
    if (x@metadata$oceCoordinate != "xyz")
        stop("input must be in xyz coordinates")
    res <- x
    heading <- res@data$heading
    pitch <- res@data$pitch
    roll <- res@data$roll
    ## Case-by-case alteration of heading, pitch and roll, so we can use one formula for all.
    ## There are three instrumentType values, ("teledyn rdi", "nortek", and "sontek"), and
    ## three orientation values ("upward", "downward", and "sideward").
    if (1 == length(agrep("rdi", x@metadata$manufacturer, ignore.case=TRUE))) { # "teledyn rdi"
        ## h/p/r and s/f/m from Clark Richards pers. comm. 2011-03-14, revised 2011-03-15
        if (res@metadata$orientation == "upward") {
            oceDebug(debug, "Case 1: RDI ADCP with upward-pointing sensor.\n")
            oceDebug(debug, "        Using S=-X, F=Y, and M=-Z.\n")
            ## As an alternative to the next three lines, could just add 180 degrees to roll
            starboard <- -res@data$v[,,1] # p11 "RDI Coordinate Transformation Manual" (July 1998)
            forward <- res@data$v[,,2] # p11 "RDI Coordinate Transformation Manual" (July 1998)
            mast <- -res@data$v[,,3] # p11 "RDI Coordinate Transformation Manual" (July 1998)
        } else if (res@metadata$orientation == "downward") {
            oceDebug(debug, "Case 2: RDI ADCP with downward-pointing sensor.\n")
            oceDebug(debug, "        Using roll=-roll, S=X, F=Y, and M=Z.\n")
            roll <- -roll
            starboard <- res@data$v[,,1] # p11 "RDI Coordinate Transformation Manual" (July 1998)
            forward <- res@data$v[,,2] # p11 "RDI Coordinate Transformation Manual" (July 1998)
            mast <- res@data$v[,,3] # p11 "RDI Coordinate Transformation Manual" (July 1998)
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
        } else if (res@metadata$orientation == "downward") {
            oceDebug(debug, "Case 6: Sontek ADP with downward-pointing sensor.\n")
            oceDebug(debug, "        Using heading=heading-90, pitch=-pitch, roll=-roll, S=X, F=Y, and M=Z.\n")
            heading <- heading - 90
            pitch <- -pitch
            roll <- -roll
            starboard <- res@data$v[,,1]
            forward <- res@data$v[,,2]
            mast <- res@data$v[,,3]
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
    res@metadata$oceCoordinate <- "enu"
    res@processingLog <- unclass(processingLog(res@processingLog, paste(deparse(match.call()), sep="", collapse="")))
    oceDebug(debug, "\b\b\b} # xyzToEnuAdp()\n")
    res
}

enuToOtherAdp <- function(x, heading=0, pitch=0, roll=0)
{
    if (!inherits(x, "adp"))
        stop("method is only for adp objects")
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
    res@metadata$oceCoordinate <- "other"
    res@processingLog <- unclass(processingLog(res@processingLog, paste(deparse(match.call()), sep="", collapse="")))
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
