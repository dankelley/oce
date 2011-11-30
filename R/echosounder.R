## vim:textwidth=128:expandtab:shiftwidth=4:softtabstop=4

setMethod(f="initialize",
          signature="echosounder",
          definition=function(.Object, filename="") {
              .Object@metadata$filename <- filename
              .Object@processingLog$time <- as.POSIXct(Sys.time())
              .Object@processingLog$value <- "create 'echosounder' object"
              return(.Object)
          })

setMethod(f="[[",
          signature="echosounder",
          definition=function(x, i, j, drop) {
              as(x, "oce")[[i, j, drop]]
          })

as.echosounder <- function(time, depth, a, src="") # FIXME change this, when read.echosounder() finalized
{
    res <- new('echosounder', filename=src)
    res@metadata$channel <- 1
    res@metadata$soundSpeed <- swSoundSpeed(35, 10, 50)
    res@metadata$samplingDeltat <- as.numeric(time[2]) - as.numeric(time[1])
    dim <- dim(a)
    res@metadata$pingsInFile <- dim[1]
    res@metadata$samplesPerPing <- dim[2]
    ## FIXME: what about timeLocation, latitude, and longitude?
    res@data$time <- time
    res@data$depth <- depth
    res@data$a<- a
    res@processingLog <- processingLog(res@processingLog, paste(deparse(match.call()), sep="", collapse=""))
    res
}

findBottom <- function(x, clean=despike) # FIXME: time, lat, lon, and then document (incl. for bathy!)
{
    a <- x[["a"]]
    wm <- clean(apply(a, 1, which.max))
    depth <- x[["depth"]][wm]
    list(time=x[["time"]], depth=depth, index=wm)
}

setMethod(f="plot",
          signature=signature("echosounder"),
          definition=function(x, which = 1, # 1=z-t section 2=dist-t section 3=map
                              newx, xlab="",
                              zlim,
                              type="l", col=oceColorsJet, lwd=2,
                              despike=FALSE, drawBottom,
                              adorn=NULL,
                              mgp=getOption("oceMgp"),
                              mar=c(mgp[1]+1, mgp[1]+1, mgp[1]+1, mgp[1]+1),
                              debug=getOption("oceDebug"),
                              ...)
          {
              dots <- list(...)
              dotsNames <- names(dots)
              oceDebug(debug, "\b\bplot() { # for echosounder\n")
              opar <- par(no.readonly = TRUE)
              lw <- length(which)
              adorn.length <- length(adorn)
              if (adorn.length == 1) {
                  adorn <- rep(adorn, lw)
                  adorn.length <- lw
              }
              par(mgp=mgp, mar=mar)
              if (lw > 1) {
                  on.exit(par=opar)
                  if (lw > 2)
                      lay <- layout(matrix(1:4, nrow=2, byrow=TRUE))
                  else
                      lay <- layout(matrix(1:2, nrow=2, byrow=TRUE))
              }
              for (w in 1:length(which)) {
                  oceDebug(debug, "this which:", which[w], "\n")
                  if (which[w] == 1 || which[w] == "zt image") {
                      time <- x[["time"]]
                      xInImage <- time
                      if (!length(time))
                          stop("plot.echosounder() cannot plot, because @data$time has zero length")
                      a <- x[["a"]]
                      newxGiven <- !missing(newx)
                      if (newxGiven) {
                          t <- as.numeric(time)
                          if (length(newx) != length(t))
                              stop("length of 'newx' must match that of time within the object")
                          if (any(diff(order(newx))<0))
                              stop("cannot handle misordered newx (FIXME)")
                          ndepth <- dim(a)[2]
                          for (i in 1:ndepth)
                              a[,i] <- approx(oce::rescale(t), a[,i], oce::rescale(newx))$y
                          xInImage <- newx
                      }
                      if (despike)
                          a <- apply(a, 2, smooth)
                      z <- log10(ifelse(a > 1, a, 1)) # FIXME: make an argument for this '1'
                      if (!missing(drawBottom)) {
                          if (is.logical(drawBottom) && drawBottom)
                              drawBottom <- "white"
                          waterDepth <- findBottom(x)$depth
                          axisBottom <- par('usr')[3]
                          deepestWater <- max(abs(waterDepth))
                          imagep(xInImage, y=-x[["depth"]], xlab=xlab, ylab="z [m]",
                                 ylim=c(-deepestWater,0),
                                 z=z, zlim=if(missing(zlim)) c(0, max(z)) else zlim,
                                 col=col, mar=mar, ...)
                          axisBottom <- par('usr')[3]
                          waterDepth <- c(axisBottom, -waterDepth, axisBottom)
                          time <-  x[["time"]]
                          if (newxGiven) {
                              newx2 <- approx(as.numeric(time), newx, as.numeric(time))$y
                              newx2 <- c(newx2[1], newx2, newx2[length(newx2)])
                              polygon(newx2, waterDepth, col=drawBottom)
                          } else {
                              time2 <- c(time[1], time, time[length(time)])
                              polygon(time2, waterDepth, col=drawBottom)
                          }
                      } else {
                          imagep(xInImage, y=-x[["depth"]], xlab=xlab, ylab="z [m]",
                                 ylim=c(-max(abs(x[["depth"]])), 0),
                                 z=z, zlim=if(missing(zlim)) c(0, max(z)) else zlim,
                                 col=col, mar=mar, ...)
                      }
                      if (newxGiven) {
                          pretty <- pretty(time)
                          labels <- format(pretty, format="%H:%M:%S")
                          at <- approx(as.numeric(time), newx, as.numeric(pretty))$y
                          axis(3, at=at, labels=labels)
                      }
                  } else if (which[w] == 2 || which[w] == "zx image") {
                      distance <- geodDist(x[["latitude"]], x[["longitude"]], alongPath=TRUE)
                      a <- x[["a"]]
                      if (despike)
                          a <- apply(a, 2, smooth)
                      z <- log10(ifelse(a > 1, a, 1)) # FIXME: make an argument for this '1'
                      imagep(distance, -x[["depth"]], xlab="Distance [km]", ylab="z [m]",
                             z, zlim=if (missing(zlim)) c(0, max(z)) else zlim,
                             col=col)
                      if (!missing(drawBottom)) {
                          if (is.logical(drawBottom) && drawBottom)
                              drawBottom <- "white"
                          b <- findBottom(x)
                          ndistance <- length(distance)
                          distance2 <- c(distance[1], distance, distance[ndistance])
                          depth <- x[["depth"]][b$index]
                          axisBottom <- par('usr')[3]
                          depth2 <- c(axisBottom, -depth, axisBottom)
                          polygon(distance2, depth2, col=drawBottom)
                      }
                  } else if (which[w] == 3 || which[w] == "map") { # map: optional extra arguments 'radius' and 'coastline'
                      lat <- x[["latitude"]]
                      lon <- x[["longitude"]]
                      asp <- 1 / cos(mean(range(lat, na.rm=TRUE))*pi/180)
                      latm <- mean(lat, na.rm=TRUE)
                      lonm <- mean(lon, na.rm=TRUE)
                      radius <- max(geodDist(latm, lonm, lat, lon))
                      if ("radius" %in% dotsNames) {
                          radius <- max(radius, dots$radius)
                      }
                      km_per_lat_deg <- geodDist(latm, lonm, latm+1, lonm) 
                      km_per_lon_deg <- geodDist(latm, lonm, latm, lonm+1) 
                      lonr <- lonm + radius / km_per_lon_deg * c(-1, 1)
                      latr <- latm + radius / km_per_lat_deg * c(-1, 1)
                      plot(lonr, latr, asp=asp, type='n', xlab="Longitude", ylab="Latitude")
                      if ("coastline" %in% dotsNames) {
                          coastline <- dots$coastline
                          if (!is.null(coastline@metadata$fillable) && coastline@metadata$fillable) {
                              polygon(coastline[["longitude"]], coastline[["latitude"]], col="lightgray", lwd=3/4)
                              polygon(coastline[["longitude"]]+360, coastline[["latitude"]], col="lightgray", lwd=3/4)
                          } else {
                              lines(coastline[["longitude"]], coastline[["latitude"]], col="darkgray")
                              lines(coastline[["longitude"]]+360, coastline[["latitude"]], col="darkgray")
                          }
                      }
                      lines(lon, lat, col=col, lwd=lwd)
                  } else {
                      stop("unknown value of which=", which, " (must be 1, 2, or 3)")
                  }
                  if (w <= adorn.length && nchar(adorn[w]) > 0) {
                      t <- try(eval(adorn[w]), silent=TRUE)
                      if (class(t) == "try-error")
                          warning("cannot evaluate adorn[", w, "]\n")
                  }
              }
              oceDebug(debug, "\b\b} # plot.echosounder()\n")
              invisible()
          })

read.echosounder <- function(file, channel=1, soundSpeed=swSoundSpeed(35, 10, 50),
                             tz=getOption("oceTz"), debug=getOption("oceDebug"))
{
    oceDebug(debug, "\b\bread.echosounder(file=\"", file, "\", tz=\"", tz, "\", debug=", debug, ") {\n", sep="")
    ofile <- file
    filename <- NULL
    if (is.character(file)) {
        filename <- fullFilename(file)
        file <- file(file, "rb")
        on.exit(close(file))
    }
    if (!inherits(file, "connection"))
        stop("argument `file' must be a character string or connection")
    if (!isOpen(file)) {
        open(file, "rb")
        on.exit(close(file))
    }
    res <- new("echosounder", filename=filename)
    seek(file, 0, "start")
    seek(file, where=0, origin="end")
    fileSize <- seek(file, where=0)
    oceDebug(debug, "fileSize=", fileSize, "\n")
    buf <- readBin(file, what="raw", n=fileSize, endian="little")

    ## Section 3.3 of the Biosonics doc (see ?echosounder-class) describes the
    ## file format.  Note that the files are little endian.
    ##
    ## Data are organized as a sequence of tuples, in the following format:
    ##   N = 2-byte unsigned int that indicates the size of the tuple.
    ##   code = 2-byte code (see table below)
    ##   data = N bytes (depends on code)
    ##   N6 = 2 bytes that must equal N+6, or the data are corrupted
    ##
    ## The codes, from the table in section 3.5 of Biosonics doc (see ?echosounder-class)
    ## are as follows.  The first tuple in a file must have code 0xFFFF, and the
    ## second must have code 001E, 0018, or 0001.
    ##
    ##   0xFFFF Signature (for start of file)
    ##   0x001E V3 File Header
    ##   0x0018 ￼V2 File Header
    ##   0x0001 ￼V1 File Header
    ##   0x0012 Channel Descriptor
    ##   0x0036 Extended Channel Descriptor
    ##   0x0015 ￼Single-Beam Ping
    ##   0x001C Dual-Beam Ping
    ##   0x001D Split-Beam Ping
    ##   0x000F or 0x0020 ￼ Time
    ##   0x000E ￼Position
    ##   0x0011 ￼Navigation String
    ##   0x0030 Timestamped Navigation String
    ##   0x0031 Transducer Orientation
    ##   0x0032 Bottom Pick
    ##   0x0033 ￼Single Echoes
    ##   0x0034 ￼Comment
    ##   ￼0xFFFE ￼End of File
    tuple <- 1
    offset <- 0
    timeSlow <- latitudeSlow <- longitudeSlow <- NULL # accumulate using c() because length unknown
    timeLast <- 0
    first <- TRUE
    scan <- 1
    intensity <- list()
    time <- list()
    samplingDeltat <- 2.4e-05 # a guess, to avoid being unknown if the header cannot be read
    channelNumber <- NULL
    channelID <- NULL
    channelDeltat <- NULL
    blankedSamples <- 0
    fileType <- "unknown" 
    timeBottom <- rangeBottom <- NULL
    while (offset < fileSize) {
        print <- debug && tuple < 200
        N <- .C("uint16_le", buf[offset+1:2], 1L, res=integer(1))$res
        code1 <- buf[offset+3]
        code2 <- buf[offset+4]
        code <- readBin(buf[offset+3:4],"integer", size=2, n=1, endian="small", signed=FALSE)
        if (print) cat("buf[", 1+offset, ", ...] (0x", code1, sep="")
        ## The ordering of the code1 tests is not too systematic here; frequently-encountered
        ## codes are placed first, but then it's a bit random.
        if (code1 == 0x15) {           # single-beam ping tuple (section 4.6.1)
            thisChannel <- .C("uint16_le", buf[offset+4+1:2], 1L, res=integer(1))$res
            pingNumber <- readBin(buf[offset+6+1:4], "integer", size=4, n=1, endian="little")
            pingElapsedTime <- readBin(buf[offset+10+1:4], "integer", size=4, n=1, endian="little") / 1000
            ns <- .C("uint16_le", buf[offset+14+1:2], 1L, res=integer(1))$res # number of samples
            if (thisChannel == channelNumber[channel]) {
                if (debug > 0) {
                    cat("buf[", 1+offset, ", ...] (0x", code1, " single-beam ping)", 
                        " scan=", scan,
                        " ping=", pingNumber,
                        " ns=", ns,
                        " channel=", thisChannel,
                        " elapsedTime=", pingElapsedTime,
                        "\n", sep="")
                }
                tmp <- .Call("biosonics_ping", buf[offset+16+1:(2*ns)], samplesPerPing)
                a[scan, ] <- rev(tmp) # note reversal in time
                time[[scan]] <- timeLast # FIXME many pings between times, so this is wrong
                scan <- scan + 1
           } else {
               if (debug > 0) {
                   cat("buf[", 1+offset, ", ...] (0x", code1, " single-beam ping)", 
                       " ping=", pingNumber,
                       " ns=", ns,
                        " channel=", thisChannel,
                        " IGNORED)\n", sep="")
                }
            }
        } else if (code1 == 0x0f || code == 0x20) { # time
            timeSec <- readBin(buf[offset+4 + 1:4], what="integer", endian="little", size=4, n=1)
            timeSubSec <- .C("biosonics_ss", buf[offset+10], res=numeric(1))$res
            timeFull <- timeSec + timeSubSec
            timeElapsedSec <- readBin(buf[offset+10+1:4], what="integer", endian="little", size=4, n=1)/1e3
            ## centisecond = ss & 0x7F (according to section 4.7)
            timeLast <- timeSec + timeSubSec # used for positioning
            if (print) cat(sprintf(" time) calendar: %s   elapsed %.2f\n", timeFull+as.POSIXct("1970-01-01 00:00:00", tz="UTC"), timeElapsedSec))
        } else if (code1 == 0x0e) { # position
            lat <- readBin(buf[offset + 4 + 1:4], "integer", endian="little", size=4, n=1) / 6e6
            lon <- readBin(buf[offset + 8 + 1:4], "integer", endian="little", size=4, n=1) / 6e6
            latitudeSlow <- c(latitudeSlow, lat)
            longitudeSlow <- c(longitudeSlow, lon)
            timeSlow <- c(timeSlow, timeLast)
            if (print) cat(" position)", lat, "N", lon, "E\n")
        } else if (code2 == 0xff) {
            if (tuple == 1) {
                if (print) cat(" file start code)\n")
            } else {
                break
            }
        } else if (code1 == 0x11) {
            if (print) cat(" navigation string) IGNORED\n")
        } else if (code1 == 0x12) {
            channelNumber <- c(channelNumber, .C("uint16_le", buf[offset+4+1:2], 1L, res=integer(1))$res)
            blankedSamples <- .C("uint16_le", buf[offset+20+1:2], 1L, res=integer(1))$res
            channelDeltat <- c(channelDeltat, 1e-9*.C("uint16_le", buf[offset+12+1:2], 1L, res=integer(1))$res)
            pingsInFile <- readBin(buf[offset+6+1:4], "integer", n=1, size=4, endian="little")
            samplesPerPing <- .C("uint16_le", buf[offset+10+1:2], 1L, res=integer(1))$res
            if (1 == length(channelNumber)) { # get space
                a <- matrix(1, nrow=pingsInFile, ncol=samplesPerPing)
            }
            if (print) cat(" channel descriptor)",
                           " number=", tail(channelNumber, 1),
                           " blankedSamples=", blankedSamples,
                           " dt=", tail(channelDeltat, 1),
                           " pingsInFile=", pingsInFile,
                           " samplesPerPing=", samplesPerPing, 
                           "\n")
        } else if (code1 == 0x30) {
            if (print) cat(" time-stamped navigation string) IGNORED\n")
        } else if (code1 == 0xff && tuple > 1) {
            if (print) cat("  EOF\n")
        } else if (code1 == 0x1E) {
            if (print) cat(" V3 file header)\n")
            fileType <- if (buf[offset + 1] == 0x10 & buf[offset + 2] == 0x00) "DT4 v2.3" else "DT4 pre v2.3"
        } else if (code1 == 0x18) {
            warning("Biosonics file of type 'V2' detected ... errors may crop up")
            fileType <- "V2"
        } else if (code1 == 0x01) {
            warning("Biosonics file of type 'V1' detected ... errors may crop up")
            fileType <- "V1"
        } else if (code1 == 0x1c) {
            warning("cannot handle dual-beam ping")
        } else if (code1 == 0x32) {
            if (print) cat(" bottom pick) ")
            ##thisChannel <- .C("uint16_le", buf[offset+4+1:2], 1L, res=integer(1))$res
            ##thisPing <- .C("uint16_le", buf[offset+4+1:2], 1L, res=integer(1))$res
            foundBottom <- .C("uint16_le", buf[offset+14+1:2], 1L, res=integer(1))$res
            if (foundBottom) {
                binBottom <- readBin(buf[offset+16+1:4], "integer", size=4, n=1, endian="little")
                ##timeBottom <- c(timeBottom, readBin(buf[offset+10+1:4], "integer", size=4, n=1, endian="little") / 1000)
                timeBottom <- c(timeBottom, timeLast) ## FIXME: maybe should use the elapsed time (as in prev line)
                if (print) cat(" bin ", binBottom, "... this value not used in oce package\n")
            } else {
                if (print) cat(" could not find bottom\n")
            }
        } else if (code1 == 0x36) {
            if (print) cat(" extended channel descriptor) IGNORED\n")
        } else if (code1 == 0x33) {
            if (print) cat(" single echo tuple) IGNORED ... see p26 of DT4_format_2010.pdf\n")
        } else if (code1 == 0x1D) {
            if (print) cat(" split-beam tuple) IGNORED ... see p8 of DT4_format_2010.pdf\n")
        } else {
            if (print) cat(" unknown code) IGNORED\n")
        }
        N6 <- .C("uint16_le", buf[offset+N+5:6], 1L, res=integer(1))$res
        if (N6 != N + 6)
            stop("error reading tuple number ", tuple, " (mismatch in redundant header-length flags)")
        offset <- offset + N + 6
        tuple <- tuple + 1
    }
    res@metadata$channel <- channel
    res@metadata$fileType <- fileType
    res@metadata$blankedSamples <- blankedSamples
    res@metadata$soundSpeed <- soundSpeed
    res@metadata$samplingDeltat <- channelDeltat[1] # nanoseconds
    res@metadata$pingsInFile <- pingsInFile
    res@metadata$samplesPerPing <- samplesPerPing
    range  <- rev(blankedSamples + seq(1,dim(a)[2])) * res@metadata$soundSpeed * res@metadata$samplingDeltat / 2

    time <- as.numeric(time)

    ## interpolate to "fast" latitude and longitude, after extending to ensure spans
    ## enclose the ping times.
    n <- length(latitudeSlow)
    t <- c(2*timeSlow[1]-timeSlow[2], timeSlow, 2*timeSlow[n] - timeSlow[n-1])
    approx2 <- function(x, y, xout)
    {
        nx <- length(x)
        nxout <- length(xout)
        before <- y[1] + (y[2] - y[1]) * (xout[1] - x[1]) / (x[2] - x[1])
        after <- y[n-1] + (y[n] - y[n-1]) * (xout[nxout] - x[n-1]) / (x[n] - x[n-1])
        approx(c(xout[1], x, xout[nxout]), c(before, y, after), xout)$y
    }
    latitude <- approx2(timeSlow, latitudeSlow, time)
    longitude <- approx2(timeSlow, longitudeSlow, time)

    ## TEST:
    ## par(mfrow=c(2,1))
    ## plot(timeSlow[1:5], latitudeSlow[1:5])
    ## lines(time, latitude)
    ## plot(timeSlow[length(latitudeSlow)+(-5:0)], latitudeSlow[length(latitudeSlow)+(-5:0)])
    ## lines(time, latitude)

    res@data <- list(timeSlow=timeSlow+as.POSIXct("1970-01-01 00:00:00", tz="UTC"),
                     latitudeSlow=latitudeSlow,
                     longitudeSlow=longitudeSlow,
                     depth=range,
                     time=time+as.POSIXct("1970-01-01 00:00:00", tz="UTC"),
                     latitude=latitude,
                     longitude=longitude,
                     a=a)
    if (!is.null(timeBottom) && !is.null(rangeBottom)) { # FIXME: not documented, or used
        res@data$timeBottom <- timeBottom
        res@data$rangeBottom <- rangeBottom
    }
    res@processingLog <- processingLog(res@processingLog,
                                       paste("read.echosounder(\"", filename, "\", tz=\"", tz, "\", debug=", debug, ")", sep=""))
    res
}

summary.echosounder <- function(object, ...)
{
    cat("Echosounder Summary\n-------------------\n\n")
    showMetadataItem(object, "filename", "File source:         ", quote=TRUE)
    time <- object[["time"]]
    tz <- attr(time[1], "tzone")
    nt <- length(time)
    cat(sprintf("* Channel:             %d\n", object[["channel"]]))
    cat(sprintf("* Measurements:        %s %s to %s %s\n", format(time[1]), tz, format(time[nt]), tz))
    cat(sprintf("* Assumed sound speed: %.2f m/s\n", object[["soundSpeed"]]))
    cat(sprintf("* Time between pings:  %.2e s\n", object[["samplingDeltat"]]))
    cat(sprintf("* Blanked samples:     %d\n", object[["blankedSamples"]]))
    cat(sprintf("* Pings in file:       %d\n", object[["pingsInFile"]]))
    cat(sprintf("* Samples per ping:    %d\n", object[["samplesPerPing"]]))
    cat(sprintf("* File type:           \"%s\"\n", object[["fileType"]]))
    cat("* Statistics::\n")
    dataNames <- names(object@data)
    ndata <- length(dataNames)
    threes <- matrix(nrow=ndata-length(grep("^time", dataNames)), ncol=3)
    ii <- 1
    for (i in 1:ndata) {
        if (0 == length(grep("^time", dataNames[i]))) {
            threes[ii,] <- threenum(object@data[[i]])
            ii <- ii + 1
        }
    }
    rownames(threes) <- paste("    ", dataNames[-grep("^time", dataNames)])
    colnames(threes) <- c("Min.", "Mean", "Max.")
    print(threes)
    processingLogShow(object)
}

