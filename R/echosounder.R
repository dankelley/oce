## vim:textwidth=128:expandtab:shiftwidth=4:softtabstop=4

## FIXME I need to read this quirky format with more care.  The docs say:
## FIXME    The amplitude counts value is in a custom floating- point format,
## FIXME    with a 4-bit exponent and a 12-bit mantissa.
## FIXME and also that a run-length-encoding method is used.  I handle neither here.

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

as.echosounder <- function(time, depth, data, src="") # FIXME change this, when read.echosounder() finalized
{
    warning("not doing much yet!")
    res <- new('echosounder')
    data <- data.frame(time=time,
                       depth=depth,
                       data=data)
    metadata <- list(src=src)
    res@metadata <- metadata
    res@data <- data
    res@processingLog <- processingLog(res@processingLog, paste(deparse(match.call()), sep="", collapse=""))
    res
}

setMethod(f="plot",
          signature=signature("echosounder"),
          definition=function(x, which = 1, # 1=z-t section 2=dist-t section 3=map
                              type="l", col="black", lwd=2,
                              adorn=NULL,
                              mgp=getOption("oceMgp"),
                              mar=c(mgp[1]+1,mgp[1]+1,mgp[1]+1,mgp[1]+1),
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
                  if (which[w] == 1) { # z-t graph
                      oce.plot.ts(x[["time"]], geodDist(x[["latitude"]], x[["longitude"]], alongPath=TRUE),
                                  type=type, col=col, lwd=lwd, ylab="Distance [km]")
                  } else if (which[w] == 2) {
                      imagep(x=d@data$timePing, y=-rev(d@data$depth), ylab="z [m]",
                             z=log10(ifelse(d@data$amplitude > 0, d@data$amplitude, 1)),
                             col=oceColorsJet, ...)
                  } else if (which[w] == 3) { # map: optional extra arguments 'radius' and 'coastline'
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

read.echosounder <- function(file, soundSpeed=swSoundSpeed(35, 10, 50),
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
    Ntime <- 0
    time <- latitude <- longitude <- NULL
    timeLast <- 0
    first <- TRUE
    scan <- 1
    intensity <- list()
    timePing <- list()
    samplingDeltat <- 2.4e-05 # a guess, to avoid being unknown if the header cannot be read
    channelNumber <- NULL
    channelID <- NULL
    channelDeltat <- NULL
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
            ## FIXME must handle RLE in biosonics_ping
            channel <- .C("uint16_le", buf[offset+4+1:2], 1L, res=integer(1))$res
            pingNumber <- readBin(buf[offset+6+1:4], "integer", size=4, n=1, endian="little")
            pingElapsedTime <- readBin(buf[offset+10+1:4], "integer", size=4, n=1, endian="little") / 1000
            ns <- .C("uint16_le", buf[offset+14+1:2], 1L, res=integer(1))$res # number of samples
            if (channel == channelNumber[1]) { ## FIXME: only plotting first channel, as a test
                #intensity[[scan]] <- .C("uint16_le", buf[offset+16+1:(2*ns)], as.integer(ns), res=integer(ns))$res
                intensity[[scan]] <-.Call("biosonics_ping", buf[offset+16+1:(2*ns)], ns) # FIXME: not ns in both places
                timePing[[scan]] <- timeLast # FIXME many pings between times, so this is wrong
                scan <- scan + 1
                if (debug > 0) {
                    cat("buf[", 1+offset, ", ...] (0x", code1, " single-beam ping)", 
                        " ping=", pingNumber,
                        " ns=", ns,
                        " channel=", channel,
                        " elapsedTime=", pingElapsedTime,
                        "\n", sep="")
                }
            }
        } else if (code1 == 0x0f || code == 0x20) { # time
            timeSec <- readBin(buf[offset+4 + 1:4], what="integer", endian="little", size=4, n=1)
            timeSubSec <- .C("biosonics_ss", buf[offset+10], res=numeric(1))$res
            timeFull <- timeSec + timeSubSec
            timeElapsedSec <- readBin(buf[offset+10+1:4], what="integer", endian="little", size=4, n=1)/1e3
            ## centisecond = ss & 0x7F (according to section 4.7)
            timeLast <- timeSec + timeSubSec
            if (print) cat(sprintf(" time) calendar: %s   elapsed %.2f\n", timeFull+as.POSIXct("1970-01-01 00:00:00", tz="UTC"), timeElapsedSec))
            Ntime <- Ntime + 1
        } else if (code1 == 0x0e) { # position
            lat <- readBin(buf[offset + 4 + 1:4], "integer", endian="little", size=4, n=1) / 6e6
            lon <- readBin(buf[offset + 8 + 1:4], "integer", endian="little", size=4, n=1) / 6e6
            latitude <- c(latitude, lat)
            longitude <- c(longitude, lon)
            time <- c(time, timeLast)
            if (print) cat(" position)", lat, "N", lon, "E\n")
        } else if (code2 == 0xff) {
            if (tuple == 1) {
                if (print) cat(" file start code)\n")
            } else {
                break
            }
        } else if (code1 == 0x11) {
            if (print) cat(" navigation string) ... ignored\n")
        } else if (code1 == 0x12) {
            channelNumber <- c(channelNumber, .C("uint16_le", buf[offset+4+1:2], 1L, res=integer(1))$res)
            channelDeltat <- c(channelDeltat, 1e-9*.C("uint16_le", buf[offset+12+1:2], 1L, res=integer(1))$res)
            if (print) cat(" channel descriptor) number=", tail(channelNumber, 1), " dt=", tail(channelDeltat, 1), "\n")
        } else if (code1 == 0x30) {
            if (print) cat(" time-stamped navigation string, which is ignored)\n")
        } else if (code1 == 0xff && tuple > 1) {
            if (print) cat("  EOF\n")
        } else if (code1 == 0x1e) {
            if (print) cat(" V3 file header)\n")
        } else if (code1 == 0x18) {
            warning("Biosonics file of type 'V2' detected ... errors may crop up")
        } else if (code1 == 0x01) {
            warning("Biosonics file of type 'V1' detected ... errors may crop up")
        } else if (code1 == 0x1c) {
            warning("cannot handle dual-beam ping")
        } else {
            if (print) cat(" unknown code)\n")
        }
        N6 <- .C("uint16_le", buf[offset+N+5:6], 1L, res=integer(1))$res
        if (N6 != N + 6)
            stop("error reading tuple number ", tuple, " (mismatch in redundant header-length flags)")
        offset <- offset + N + 6
        tuple <- tuple + 1
    }
    oceDebug(debug, "tuples:", tuple, " times:", Ntime, "\n")
    ncol <- max(sapply(intensity, length))
    nrow <- length(intensity)
    amplitude <- matrix(1, nrow=nrow, ncol=ncol) # FIXME using 1 for missing
    for (row in seq_along(intensity)) {
        amplitude[row,1:length(intensity[[row]])] <- rev(intensity[[row]])
    }
    res@metadata$soundSpeed <- soundSpeed
    res@metadata$samplingDeltat <- channelDeltat[1] # nanoseconds
    res@data <- list(time=time + as.POSIXct("1970-01-01 00:00:00", tz="UTC"),
                     depth=seq(0,-1+dim(amplitude)[2]) * res@metadata$soundSpeed * res@metadata$samplingDeltat / 2,
                     timePing=unlist(timePing) + as.POSIXct("1970-01-01 00:00:00", tz="UTC"),
                     latitude=latitude,
                     longitude=longitude,
                     amplitude=amplitude)
    res@processingLog <- processingLog(res@processingLog,
                                       paste("read.echosounder(\"", filename, ", tz=\"", tz, "\", debug=", debug, ")"))
    res
}

summary.echosounder <- function(object, ...)
{
    cat("Echosounder Summary\n-------------------\n\n")
    showMetadataItem(object, "filename", "File source:         ", quote=TRUE)
    time <- object[["timePing"]]
    tz <- attr(time[1], "tzone")
    nt <- length(time)
    cat(sprintf("* Measurements:        %s %s to %s %s\n", format(time[1]), tz, format(time[nt]), tz))
    cat(sprintf("* Assumed sound speed: %.2f m/s\n", object[["soundSpeed"]]))
    cat(sprintf("* Time between pings:  %.2e s\n", object[["samplingDeltat"]]))
}


## NOTES
##
## on our test file, RP's code gives as follows.  Although
## this is not too useful, I guess I may as well read it
## from the file.
##        absorb: 0
##            sv: 1.4466e+03
##   temperature: 10
##      salinity: 0
##         power: 0
##          nsdr: 2
