## vim:textwidth=128:expandtab:shiftwidth=4:softtabstop=4

## REFERENCES:
##   [1] "DT4 Data File Format Specification" [July, 2010] DT4_format_2010.pdf

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

as.echosounder <- function(time, depth, a, src="") # FIXME add lots of new args, when read.echosounder() finalized
{
    res <- new('echosounder', filename=src)
    res@metadata$channel <- 1
    res@metadata$soundSpeed <- swSoundSpeed(35, 10, 1)
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

findBottom <- function(x, ignore=5, clean=despike)
{
    a <- x[["a"]]
    keep <- x[["depth"]] >= ignore
    wm <- clean(apply(a[,keep], 1, which.max))
    depth <- x[["depth"]][wm]
    list(time=x[["time"]], depth=depth, index=wm)
}

setMethod(f="plot",
          signature=signature("echosounder"),
          definition=function(x, which = 1, # 1=z-t section 2=dist-t section 3=map
                              beam="a",
                              newx,
                              xlab, ylab,
                              xlim, ylim, zlim,
                              type="l", col=oceColorsJet, lwd=2,
                              despike=FALSE,
                              drawBottom, ignore=5,
                              radius, coastline,
                              adorn=NULL,
                              mgp=getOption("oceMgp"),
                              mar=c(mgp[1]+1, mgp[1]+1, mgp[1]+1, mgp[1]+1),
                              atTop, labelsTop,
                              tformat,
                              debug=getOption("oceDebug"),
                              ...)
          {
              dots <- list(...)
              dotsNames <- names(dots)
              oceDebug(debug, "\b\bplot() { # for echosounder\n")
              opar <- par(no.readonly = TRUE)
              lw <- length(which)
              if (length(beam) < lw)
                  beam <- rep(beam, lw)
              adorn.length <- length(adorn)
              if (adorn.length == 1) {
                  adorn <- rep(adorn, lw)
                  adorn.length <- lw
              }
              opar <- par(no.readonly = TRUE)
              par(mgp=mgp, mar=mar)
              if (lw > 1) {
                  ##on.exit(par(opar))
                  if (lw > 2)
                      lay <- layout(matrix(1:4, nrow=2, byrow=TRUE))
                  else
                      lay <- layout(matrix(1:2, nrow=2, byrow=TRUE))
              }

              oceDebug(debug, "which:", which, "\n")
              which <- ocePmatch(which, list("zt image"=1, "zx image"=2, map=3))
              oceDebug(debug, "which:", which, "\n")
              for (w in 1:length(which)) {
                  oceDebug(debug, "this which:", which[w], "\n")
                  if (which[w] == 1) {
                      time <- x[["time"]]
                      xInImage <- time
                      if (!length(time))
                          stop("plot.echosounder() cannot plot, because @data$time has zero length")
                      signal <- x[[beam[w]]]
                      newxGiven <- !missing(newx)
                      if (newxGiven) {
                          t <- as.numeric(time)
                          if (length(newx) != length(t))
                              stop("length of 'newx' must match that of time within the object")
                         xInImage <- newx
                      }
                      if (despike)
                          signal <- apply(signal, 2, smooth)
                      if (beam[w] == "Sv") {
                          z <- signal
                      } else {
                          z <- log10(ifelse(signal > 1, signal, 1)) # FIXME: make an argument for this '1'
                      }
                      if (!missing(drawBottom)) {
                          if (is.logical(drawBottom) && drawBottom)
                              drawBottom <- "lightgray"
                          waterDepth <- findBottom(x, ignore=ignore)$depth
                          axisBottom <- par('usr')[3]
                          deepestWater <- max(abs(waterDepth))
                          imagep(xInImage, y=-x[["depth"]], z=z,
                                 xlab=if (missing(xlab)) "" else xlab, # time
                                 ylab=if (missing(ylab)) "z [m]" else ylab, # depth
                                 xlim=xlim,
                                 ylim=if (missing(ylim)) c(-deepestWater,0) else ylim,
                                 zlim=if (missing(zlim)) c(0, max(z)) else zlim,
                                 col=col,
                                 mgp=mgp, mar=mar,
                                 tformat=tformat,
                                 debug=debug-1, ...)
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
                          imagep(xInImage, y=-x[["depth"]], z=z,
                                 xlab=if (missing(xlab)) "" else xlab, # time
                                 ylab=if (missing(ylab)) "z [m]" else ylab, # depth
                                 xlim=xlim,
                                 ylim=if (missing(ylim)) c(-max(abs(x[["depth"]])), 0) else ylim,
                                 zlim=if (missing(zlim)) c(0, max(z, na.rm=TRUE)) else zlim,
                                 col=col,
                                 mgp=mgp, mar=mar,
                                 tformat=tformat,
                                 debug=debug-1,
                                 zlab=beam[w],
                                 ...)
                      }
                      if (newxGiven) {
                          if (!missing(atTop)) {
                              at <- approx(as.numeric(x[["time"]]), newx, as.numeric(atTop))$y
                              if (missing(labelsTop))
                                  labelsTop <- format(atTop, format=if ("format" %in% dotsNames)  dots$format else "%H:%M:%S")
                              axis(side=3, at=at, labels=labelsTop, cex.axis=par('cex'))
                          } else {
                              pretty <- pretty(time)
                              labels <- format(pretty, format="%H:%M:%S")
                              at <- approx(as.numeric(time), newx, as.numeric(pretty))$y
                              axis(3, at=at, labels=labels, cex.axis=par('cex'))
                          }
                      }
                  } else if (which[w] == 2) {
                      latitude <- x[["latitude"]]
                      longitude <- x[["longitude"]]
                      jitter <- rnorm(length(latitude), sd=1e-8) # jitter lat by equiv 1mm to avoid colocation
                      distance <- geodDist(jitter + latitude, longitude, alongPath=TRUE) ## FIXME: jitter should be in imagep
                      depth <- x[["depth"]]
                      a <- x[["a"]]
                      if (despike)
                          a <- apply(a, 2, smooth)
                      z <- log10(ifelse(a > 1, a, 1)) # FIXME: make an argument for this '1'
                      deepestWater <- max(abs(depth))
                      if (!missing(drawBottom)) {
                          if (is.logical(drawBottom) && drawBottom)
                              drawBottom <- "lightgray"
                          waterDepth <- findBottom(x, ignore=ignore)
                          axisBottom <- par('usr')[3]
                          deepestWater <- max(abs(waterDepth$depth))
                      }
                      imagep(distance, -depth, z,
                             xlab=if (missing(xlab)) "Distance [km]" else xlab,
                             ylab=if (missing(ylab)) "z [m]" else ylab,
                             ylim=if (missing(ylim)) c(-deepestWater,0) else ylim,
                             zlim=if (missing(zlim)) c(0, max(z)) else zlim,
                             mgp=mgp, mar=mar,
                             tformat=tformat,
                             col=col,
                             debug=debug-1)
                      if (!missing(drawBottom)) {
                          if (is.logical(drawBottom) && drawBottom)
                              drawBottom <- "white"
                          ndistance <- length(distance)
                          distance2 <- c(distance[1], distance, distance[ndistance])
                          axisBottom <- par('usr')[3]
                          depth2 <- c(axisBottom, -depth[waterDepth$index], axisBottom)
                          polygon(distance2, depth2, col=drawBottom)
                      }
                      if (!missing(atTop)) {
                          at <- approx(as.numeric(x[["time"]]), distance, as.numeric(atTop))$y
                          if (missing(labelsTop))
                              labelsTop <- format(atTop, format=if ("format" %in% dotsNames)  dots$format else "%H:%M:%S")
                          axis(side=3, at=at, labels=labelsTop, cex.axis=par('cex'))
                      }
                  } else if (which[w] == 3) {
                      lat <- x[["latitude"]]
                      lon <- x[["longitude"]]
                      asp <- 1 / cos(mean(range(lat, na.rm=TRUE))*pi/180)
                      latm <- mean(lat, na.rm=TRUE)
                      lonm <- mean(lon, na.rm=TRUE)
                      if (missing(radius))
                          radius <- max(geodDist(latm, lonm, lat, lon))
                      else
                          radius <- max(radius, geodDist(latm, lonm, lat, lon))
                      km_per_lat_deg <- geodDist(latm, lonm, latm+1, lonm) 
                      km_per_lon_deg <- geodDist(latm, lonm, latm, lonm+1) 
                      lonr <- lonm + radius / km_per_lon_deg * c(-2, 2)
                      latr <- latm + radius / km_per_lat_deg * c(-2, 2)
                      plot(lonr, latr, asp=asp, type='n',
                           xlab=if (missing(xlab)) "Longitude" else xlab,
                           ylab=if (missing(ylab)) "Latitude" else ylab)
                      if (!missing(coastline)) {
                          coastline <- coastline
                          if (!is.null(coastline@metadata$fillable) && coastline@metadata$fillable) {
                              polygon(coastline[["longitude"]], coastline[["latitude"]], col="lightgray", lwd=3/4)
                              polygon(coastline[["longitude"]]+360, coastline[["latitude"]], col="lightgray", lwd=3/4)
                              box()
                          } else {
                              lines(coastline[["longitude"]], coastline[["latitude"]], col="darkgray")
                              lines(coastline[["longitude"]]+360, coastline[["latitude"]], col="darkgray")
                          }
                      }
                      lines(lon, lat, col=if(!is.function(col)) col else "black", lwd=lwd)
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

    ## [1 p9]: "After this comes the main data section of the file.
    ## This will typically contain some or all of the following tuples:
    ## ping tuples (type 0x0015, 0x001C or 0x001D),
    ## time tuples (type 0x000F or 0x0020),
    ## position tuples (type 0x000E),
    ## navigation string tuples (type 0x0011 or 0x0030),
    ## transducer orientation tuples (type 0x0031),
    ## bottom pick tuples (type 0x0032),
    ## single echoes tuples (type 0x0033), and
    ## comment tuples (type 0x0034).

    ## [1 sec 3.3 ] describes the file format.  NB: files are little endian.
    ##
    ## Data are organized as a sequence of tuples, in the following format:
    ##   N = 2-byte unsigned int that indicates the size of the tuple.
    ##   code = 2-byte code (see table below)
    ##   data = N bytes (depends on code)
    ##   N6 = 2 bytes that must equal N+6, or the data are corrupted
    ##
    ## The codes, from the table in [1 sec 3.5] are as follows. 
    ## The first tuple in a file must have code 0xFFFF, and the
    ## second must have code 001E, 0018, or 0001.
    ##
    ##   0xFFFF Signature (for start of file)
    ##   0x001E V3 File Header
    ##   0x0018 V2 File Header
    ##   0x0001 V1 File Header
    ##   0x0012 Channel Descriptor
    ##   0x0036 Extended Channel Descriptor
    ##   0x0015 Single-Beam Ping
    ##   0x001C Dual-Beam Ping
    ##   0x001D Split-Beam Ping
    ##   0x000F or 0x0020 Time
    ##   0x000E Position
    ##   0x0011 Navigation String
    ##   0x0030 Timestamped Navigation String
    ##   0x0031 Transducer Orientation
    ##   0x0032 Bottom Pick
    ##   0x0033 Single Echoes
    ##   0x0034 Comment
    ##   0xFFFE End of File
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
    range <- NULL
    beamType <- "unknown"
    while (offset < fileSize) {
        print <- debug && tuple < 200
        N <- .C("uint16_le", buf[offset+1:2], 1L, res=integer(1), NAOK=TRUE, PACKAGE="oce")$res
        code1 <- buf[offset+3]
        code2 <- buf[offset+4]
        code <- readBin(buf[offset+3:4],"integer", size=2, n=1, endian="small", signed=FALSE)
        if (debug > 3) cat("buf[", 3+offset, "] = code1 = 0x", code1, sep="")
        ## The ordering of the code1 tests is not too systematic here; frequently-encountered
        ## codes are placed first, but then it's a bit random.
        if (code1 == 0x15 || code1 == 0x1c || code1 == 0x1d) {           # single-beam, dual-beam, or split-beam tuple
            thisChannel <- .C("uint16_le", buf[offset+4+1:2], 1L, res=integer(1), NAOK=TRUE, PACKAGE="oce")$res
            pingNumber <- readBin(buf[offset+6+1:4], "integer", size=4L, n=1L, endian="little")
            pingElapsedTime <- 0.001 * readBin(buf[offset+10+1:4], "integer", size=4L, n=1L, endian="little")
            ns <- .C("uint16_le", buf[offset+14+1:2], 1L, res=integer(1), NAOK=TRUE, PACKAGE="oce")$res # number of samples
            if (thisChannel == channelNumber[channel]) {
                if (debug > 3) {
                    cat("buf[", 1+offset, ", ...] (0x", code1, " single-beam ping)", 
                        " scan=", scan,
                        " ping=", pingNumber,
                        " ns=", ns,
                        " channel=", thisChannel,
                        " elapsedTime=", pingElapsedTime,
                        "\n", sep="")
                }
                ## Note the time reversal in the assignment to the data matrix 'a'
                ## FIXME: is it faster to flip the data matrix later?
                if (code1 == 0x15) {
                    tmp <- .Call("biosonics_ping", buf[offset+16+1:(2*ns)], samplesPerPing, ns, 0)
                    beamType <- "single-beam"
                } else if (code1 == 0x1c) {
                    tmp <- .Call("biosonics_ping", buf[offset+16+1:(4*ns)], samplesPerPing, ns, 1)
                    beamType <- "dual-beam"
                } else if (code1 == 0x1d) {
                    ## e.g. 01-Fish.dt4 sample file from Biosonics
                    tmp <- .Call("biosonics_ping", buf[offset+16+1:(4*ns)], samplesPerPing, ns, 2)
                    beamType <- "split-beam"
                } else {
                    stop("unknown 'tuple' 0x", code1, sep="")
                }
                a[scan, ] <- rev(tmp$a)
                b[scan, ] <- rev(tmp$b)
                c[scan, ] <- rev(tmp$c)
                time[[scan]] <- timeLast # FIXME many pings between times, so this is wrong
                scan <- scan + 1
                if (debug > 3) cat("channel:", thisChannel, "ping:", pingNumber, "pingElapsedTime:", pingElapsedTime, "\n")
           } else {
               if (debug > 0) {
                   cat("buf[", 1+offset, ", ...] = 0x", code1, 
                       " ping=", pingNumber, " ns=", ns, " channel=", thisChannel, " IGNORED since wrong channel)\n", sep="")
                }
            }
        } else if (code1 == 0x0f || code == 0x20) { # time
            timeSec <- readBin(buf[offset+4 + 1:4], what="integer", endian="little", size=4, n=1)
            timeSubSec <- .C("biosonics_ss", buf[offset+10], res=numeric(1), NAOK=TRUE, PACKAGE="oce")$res
            timeFull <- timeSec + timeSubSec
            timeElapsedSec <- readBin(buf[offset+10+1:4], what="integer", endian="little", size=4, n=1)/1e3
            ## centisecond = ss & 0x7F [1 sec 4.7]
            timeLast <- timeSec + timeSubSec # used for positioning
            if (debug > 3) cat(sprintf(" time calendar: %s   elapsed %.2f\n", timeFull+as.POSIXct("1970-01-01 00:00:00", tz="UTC"), timeElapsedSec))
        } else if (code1 == 0x0e) { # position
            lat <- readBin(buf[offset + 4 + 1:4], "integer", endian="little", size=4, n=1) / 6e6
            lon <- readBin(buf[offset + 8 + 1:4], "integer", endian="little", size=4, n=1) / 6e6
            latitudeSlow <- c(latitudeSlow, lat)
            longitudeSlow <- c(longitudeSlow, lon)
            timeSlow <- c(timeSlow, timeLast)
            if (debug > 3) cat(" position", lat, "N", lon, "E\n")
        } else if (code2 == 0xff) {
            if (tuple == 1) {
                if (debug > 1) cat(" file start code\n")
            } else {
                break
            }
        } else if (code1 == 0x11) {
            if (debug > 3) cat(" navigation string IGNORED\n")
        } else if (code1 == 0x12) {
            channelNumber <- c(channelNumber, .C("uint16_le", buf[offset+4+1:2], 1L, res=integer(1), NAOK=TRUE, PACKAGE="oce")$res)
            ib <- .C("uint16_le", buf[offset+20+1:2], 1L, res=integer(1), NAOK=TRUE, PACKAGE="oce")$res
            blankedSamples <- ib
            channelDeltat <- c(channelDeltat, 1e-9*.C("uint16_le", buf[offset+12+1:2], 1L, res=integer(1), NAOK=TRUE,
                                                      PACKAGE="oce")$res)
            pingsInFile <- readBin(buf[offset+6+1:4], "integer", n=1, size=4, endian="little")
            samplesPerPing <- .C("uint16_le", buf[offset+10+1:2], 1L, res=integer(1), NAOK=TRUE, PACKAGE="oce")$res # ssp [p13 1]
            sp <- 1e-9 * readBin(buf[offset+12+1:2], "integer", n=1L, size=2L, endian="little") # [p13 1] time between samples (ns)
            if (debug > 1) cat('sp: ', sp, ' sample period in ns\n', sep='')
            pud <- readBin(buf[offset+16+1:2], "integer", n=1L, size=2L, endian="little")
            if (debug > 1) cat('pud: ', pud, ' pulse duration in us (expect 400 for 01-Fish.dt4)\n', sep='')
            pp <- .C("uint16_le", buf[offset+18+1:2], 1L, res=integer(1), NAOK=TRUE, PACKAGE="oce")$res # [p13 1] ping period (ms)
            if (debug > 1) cat('pp: ', pp, '\n', sep='')
            ib <- .C("uint16_le", buf[offset+20+1:2], 1L, res=integer(1), NAOK=TRUE, PACKAGE="oce")$res # [p13 1]
            if (debug > 1) cat('ib: ', ib, '\n', sep='')
            ## next 2 bytes contain u2, ignored
            th <- 0.01 * readBin(buf[offset+24+1:2], "integer", n=1L, size=2L, endian="little") # [p13 1]
            if (debug > 1) cat('th: ', th, ' data collection threshold in dB (expect -65 for 01-Fish.dt4)\n', sep='')
            rxee <- buf[offset+26+1:128] # [1 p13] # receiver EEPROM image (FIXME: why is serialnum out by 1? INDEX question)
            ##corr <- .C("int16_le", buf[offset+282+1:2], 1L, res=integer(1), NAOK=TRUE, PACKAGE="oce")$res # [p13 1]
            corr <- 0.01 * readBin(buf[offset+282+1:2], "integer", n=1L, size=2L, endian="little") # [p13 1]
            if (debug > 1) cat('corr: ', corr, ' user-defined calibration correction in dB (expect 0 for 01-Fish.dt4)\n', sep='')

            if (1 == length(channelNumber)) { # get space
                a <- matrix(NA, nrow=pingsInFile, ncol=samplesPerPing)
                b <- matrix(NA, nrow=pingsInFile, ncol=samplesPerPing)
                c <- matrix(NA, nrow=pingsInFile, ncol=samplesPerPing)
            }
            if (debug > 3) cat(" channel descriptor ",
                           " number=", tail(channelNumber, 1),
                           " blankedSamples=", blankedSamples,
                           " dt=", tail(channelDeltat, 1),
                           " pingsInFile=", pingsInFile,
                           " samplesPerPing=", samplesPerPing, 
                           "\n")
        } else if (code1 == 0x30) {
            if (debug > 3) cat(" time-stamped navigation string IGNORED\n")
        } else if (code1 == 0xff && tuple > 1) {
            if (debug > 3) cat("  EOF\n")
        } else if (code1 == 0x1e) {
            if (debug > 1) cat(" V3 file header\n")
            fileType <- if (buf[offset + 1] == 0x10 & buf[offset + 2] == 0x00) "DT4 v2.3" else "DT4 pre v2.3"
            res@metadata$temperature <- 0.01*.C("uint16_le", buf[offset+8+1:2], 1L, res=integer(1), NAOK=TRUE, PACKAGE="oce")$res
            if (debug > 1) cat("temperature=", res@metadata$temperature, "degC (expect 14 for 1-Fish.dt4)\n")
            res@metadata$salinity <- 0.01*.C("uint16_le", buf[offset+10+1:2], 1L, res=integer(1), NAOK=TRUE, PACKAGE="oce")$res
            if (debug > 1) cat("salinity=", res@metadata$salinity, "PSU (expect 30 for 1-Fish.dt4)\n")
            res@metadata$soundSpeed <- swSoundSpeed(res@metadata$salinity, res@metadata$temperature, 30)
            res@metadata$tpow <- 0.01*.C("uint16_le", buf[offset+12+1:2], 1L, res=integer(1), NAOK=TRUE, PACKAGE="oce")$res
            if (debug > 1) cat("tpow=", res@metadata$tpow, "(expect 0 for 1-Fish.dt4; called mTransmitPower)\n")
            res@metadata$tz <- readBin(buf[offset+16+1:2],"integer", size=2)
            if (debug > 1) cat("tz=", res@metadata$tz, "(expect -420 for 1-Fish.dt4)\n")
            dst <- readBin(buf[offset+18+1:2],"integer", size=2)
            res@metadata$dst <- dst != 0
            if (debug > 1) cat("dst=", res@metadata$dst, "(daylight saings time) expect TRUE for 1-Fish.dt4)\n")
        } else if (code1 == 0x18) {
            warning("Biosonics file of type 'V2' detected ... errors may crop up")
            fileType <- "V2"
        } else if (code1 == 0x01) {
            warning("Biosonics file of type 'V1' detected ... errors may crop up")
            fileType <- "V1"
        } else if (code1 == 0x32) {
            if (debug > 3) cat(" bottom pick tuple [1 sec 4.12] ")
            ##thisChannel <- .C("uint16_le", buf[offset+4+1:2], 1L, res=integer(1))$res
            ##thisPing <- .C("uint16_le", buf[offset+4+1:2], 1L, res=integer(1))$res
            foundBottom <- .C("uint16_le", buf[offset+14+1:2], 1L, res=integer(1), NAOK=TRUE, PACKAGE="oce")$res
            if (foundBottom) {
                thisRange <- readBin(buf[offset+20+1:4], "numeric", size=4, n=1, signed=TRUE, endian="little")
            } else {
                thisRange <- NA
            }
            range <- c(range, thisRange)
            if (debug > 3) cat(" thisRange:", thisRange)
        } else if (code1 == 0x36) {
            if (debug > 3) cat(" extended channel descriptor IGNORED\n")
        } else if (code1 == 0x33) {
            if (debug > 3) cat(" single echo tuple IGNORED ... see p26 of DT4_format_2010.pdf\n")
        } else if (code1 == 0x34) {
            if (debug > 1) cat(" comment tuple [1 sec 4.14 p28]\n")
            ## FIXME: other info could be gleaned from the comment, if needed
            numbytes <- .C("uint16_le", buf[offset+34:35], 1L, res=integer(1), NAOK=TRUE, PACKAGE="oce")$res
            if (debug > 1) cat('numbytes:', numbytes, ' ... NOTHING ELSE DECODED in this verion of oce.\n')
        } else {
            if (debug > 3) cat(" unknown code IGNORED\n")
        }
        N6 <- .C("uint16_le", buf[offset+N+5:6], 1L, res=integer(1), NAOK=TRUE, PACKAGE="oce")$res
        if (N6 != N + 6)
            stop("error reading tuple number ", tuple, " (mismatch in redundant header-length flags)")
        offset <- offset + N + 6
        tuple <- tuple + 1
    }
    res@metadata$beamType <- beamType
    res@metadata$channel <- channel
    res@metadata$fileType <- fileType
    res@metadata$blankedSamples <- blankedSamples
    res@metadata$soundSpeed <- soundSpeed
    res@metadata$samplingDeltat <- channelDeltat[1] # nanoseconds
    res@metadata$pingsInFile <- pingsInFile
    res@metadata$samplesPerPing <- samplesPerPing
    ## channel info, with names matching [1 p13]
    ##res@metadata$samplingDeltat <- sp
    res@metadata$pulseDuration <- pud
    res@metadata$pp <- pp
    res@metadata$ib <- ib
    res@metadata$th <- th
    res@metadata$rxee <- rxee
    res@metadata$corr <- corr

    depth <- rev(blankedSamples + seq(1,dim(a)[2])) * res@metadata$soundSpeed * res@metadata$samplingDeltat / 2
    ## test: for 01-Fish.dt4, have as follows:
    ##     <mPingBeginRange_m>0.988429069519043</mPingBeginRange_m>
    ##     <mPingEndRange_m>64.787033081054688</mPingEndRange_m>
    ## but we get as follows:
    ##     range(d[['depth']])
    ##     [1]  1.001714 64.485323
    ## i.e. a 12cm error at top and a 20cm error at bottom.  Note that
    ##    > diff(d[['depth']][2:1])
    ##    [1] 0.01788775
    ## FIXME: check depth mismatch relates to (a) sound speed or (b) geometry.  (Small error; low priority.)

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
    
    res@metadata$transducerSerialNumber <- readBin(rxee[2+1:8], "character") # [1 p16] offset=2 length 8
    if (debug > 1) cat("transducerSerialNumber '", res@metadata$transducerSerialNumber, "' (expect DT600085 for 01-Fish.dt4)\n", sep="")
    res@metadata$calibrationTime <- numberAsPOSIXct(readBin(rxee[36+1:4], 'integer'), tz="UTC") # [1 p16] offset=36
    if (debug > 1) cat("calibrationTime: ", format(res@metadata$calibrationTime), " (expect Thu Apr 13 02:36:38 2000 for 01-Fish.dt4)\n", sep="")
    ##res@metadata$sl <- 0.1 * .C("int16_le", rxee[58+1:2], 1L, res=integer(1), NAOK=TRUE, PACKAGE="oce")$res # [p16 1]
    res@metadata$sl <- 0.1 * readBin(rxee[58+1:2], "integer", n=1L, size=2L, endian="little") # [p16 1]
    if (debug > 1) cat('sl=', res@metadata$sl, ' (expect 220 for 01-Fish.dt4 source level SourceLevel_dBuPa)\n', sep='')
    #res@metadata$rs <- 0.1 * .C("int16_le", rxee[1+64+1:2], 1L, res=integer(1), NAOK=TRUE, PACKAGE="oce")$res # [p16 1]
    res@metadata$rs <- 0.1 * readBin(rxee[64+1:2], "integer", n=1L, size=2) # [p16 1]
    if (debug > 1) cat('rs=', res@metadata$rs, ' receiver sensitivity in dB (counts/micro Pa) (expect -58.8 for 01-Fish.dt4)\n', sep='')

    res@metadata$trtype <- .C("uint16_le", rxee[84+1:2], 1L, res=integer(1), NAOK=TRUE, PACKAGE="oce")$res # [p16 1]
    if (debug > 1) cat('trtype=', res@metadata$trtype, ' hardware [not deployment] transducer type (0 single, 3 dual, 4 split) expect 1 for 01-Fish.dt4\n', sep='')
    res@metadata$fq <- readBin(rxee[86+1:4], "integer", n=1L, size=4) # [p16 1]
    if (debug > 1) cat('fq=', res@metadata$fq, ' transducer frequency, Hz (expect 208000 for 01-Fish.dt4)\n', sep='')


    res@metadata$bwy <- 0.1 * as.numeric(rxee[1+100]) # [1 p16] offset=100
    res@metadata$bwx <- 0.1 * as.numeric(rxee[1+101]) # [1 p16] offset=101
    if (debug > 1) cat("bwx=", res@metadata$bwx, " (expect 6.5 for 01-Fish.dt4; called BeamWidthX_deg)\n", sep="")
    if (debug > 1) cat("bwy=", res@metadata$bwy, " (expect 6.5 for 01-Fish.dt4; called BeamWidthY_deg)\n", sep="")
    psi <- res@metadata$bwy / 20.0 * res@metadata$bwx / 20.0 * 10^(-3.16)
    if (debug > 1) cat("psi=", psi, "\n", sep="")
    ## c = sound speed (inferred from sal and tem FIXME could use pressure I suppose)
    ## r = range
    ##Sv <- 20*log10(a) -(sl+rs+tpow)/10.0 +20*log10(r) +2*a*r -10*log10(c*pud/1000000.0*psi/2.0) +corr/100.0
    range <- rev(depth)
    absorption <- swSoundAbsorption(35, 10, 100, 1000) # FIXME: just a placeholder
    Sv <- 20*log10(a) - (res@metadata$sl + res@metadata$rs + res@metadata$tpow)/10.0 + 20*log10(range) + 2*absorption*range - 10*log10(soundSpeed*res@metadata$pulseDuration/1e6*psi/2) + corr/100
    Sv[!is.finite(Sv)] <- NA

    res@data <- list(timeSlow=timeSlow+as.POSIXct("1970-01-01 00:00:00", tz="UTC"),
                     latitudeSlow=latitudeSlow,
                     longitudeSlow=longitudeSlow,
                     depth=depth,
                     range=range, # FIXME: is it a coincidence that time is of same length?
                     time=time+as.POSIXct("1970-01-01 00:00:00", tz="UTC"),
                     latitude=latitude,
                     longitude=longitude,
                     Sv=Sv, # FIXME: not tested
                     a=a, b=b, c=c)

    res@processingLog <- processingLog(res@processingLog,
                                       paste("read.echosounder(\"", filename, "\", tz=\"", tz, "\", debug=", debug, ")", sep=""))
    .C("biosonics_free_storage", package="oce") # clear temporary storage space
    res
}

summary.echosounder <- function(object, ...)
{
    cat("Echosounder Summary\n-------------------\n\n")
    showMetadataItem(object, "filename",               "File source:         ", quote=TRUE)
    showMetadataItem(object, "transducerSerialNumber", "Transducer serial #: ", quote=FALSE)
    metadataNames <- names(object@metadata)
    cat(sprintf("* File type:           \"%s\"\n", object[["fileType"]]))
    if ("beamType" %in% metadataNames)
        cat(sprintf("* Beam type:           %s\n", object[["beamType"]]))
    time <- object[["time"]]
    tz <- attr(time[1], "tzone")
    nt <- length(time)
    cat(sprintf("* Channel:             %d\n", object[["channel"]]))
    cat(sprintf("* Measurements:        %s %s to %s %s\n", format(time[1]), tz, format(time[nt]), tz))
    cat(sprintf("* Sound speed:         %.2f m/s\n", object[["soundSpeed"]]))
    ##cat(sprintf("* Time between pings:  %.2e s\n", object[["samplingDeltat"]]))
    if ("pulseDuration" %in% metadataNames) cat(sprintf("* Pulse duration:      %g s\n", object[["pulseDuration"]]/1e6))
    cat(sprintf("* Blanked samples:     %d\n", object[["blankedSamples"]]))
    cat(sprintf("* Pings in file:       %d\n", object[["pingsInFile"]]))
    cat(sprintf("* Samples per ping:    %d\n", object[["samplesPerPing"]]))
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

