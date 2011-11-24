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

as.echosounder <- function(time, depth, data, src="") # just guessing on args
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
                              col="black", lwd=2,
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
                  if (which[w] == 1) {
                      cat("should plot z-t graph\n")
                  } else if (which[w] == 2) {
                      cat("should plot z-distance graph\n")
                  } else if (which[w] == 3) {
                      ## scale in km
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

read.echosounder <- function(file, debug=getOption("oceDebug"))
{
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
    buf <<- readBin(file, what="raw", n=fileSize, endian="little")

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
    code1Vec <<- NULL
    pingLengthVec <<- NULL
    ## > unique(pingLengthVec)
    ## [1] 6808 6810 6794 6806 6804 6800 6802 6792 6798
    Ntime <- 0
    NsingleBeamPing <- 0
    latitude <- longitude <- NULL
    while (offset < fileSize) {
        print <- tuple < 15 || tuple > 4400
        N <- .C("uint16_le", buf[offset+1], buf[offset+2], res=integer(1))$res
        code1 <- buf[offset+3]
        code2 <- buf[offset+4]
        code <- readBin(buf[offset+3:4],"integer", size=2, n=1, endian="small", signed=FALSE)
        if (print) cat("tuple=", tuple, " offset=", offset, " N=", N, " code 0x", code1, " 0x", code2, sep="")
        if (tuple > 1) {
            if (code2 == 0xff) {
                cat("\n")
                break
            }
            if (code2 != 0x00)
                stop("tuple code should start with 0x00, but it starts with 0x", code1)
        }
        if (code1 == 0x15) {
            if (print) cat("  single-beam ping\n")
            pingLengthVec <<- c(pingLengthVec, N)
            NsingleBeamPing <- NsingleBeamPing + 1 
        } else if (code1 == 0x0f || code == 0x20) {
            if (print) cat("  time\n")
            Ntime <- Ntime + 1
        } else if (code1 == 0x11) {
            if (print) cat("  navigation string\n")
        } else if (code1 == 0x0e) {
            if (print) cat("  position\n")
            lat <- readBin(buf[offset + 4 + 1:4], "integer", signed=FALSE, size=4, n=1) / 6e6
            lon <- readBin(buf[offset + 8 + 1:4], "integer", signed=FALSE, size=4, n=1) / 6e6
            latitude <- c(latitude, lat)
            longitude <- c(longitude, lon)
        } else if (code1 == 0x12) {
            if (print) cat("  channel descriptor\n")
        } else if (code1 == 0x13) {
            if (print) cat("  13: unknown\n")
        } else if (code1 == 0xff && tuple > 1) {
            if (print) cat("  EOF\n")
        } else if (code1 == 0x1e) {
            if (print) cat("  V3 file header\n")
        } else if (code1 == 0x18) {
            if (print) cat("  V2 file header\n")
        } else if (code1 == 0x01) {
            if (print) cat("  V1 file header\n")
        } else {
            if (print) cat("\n")
        }
        code1Vec <<- c(code1Vec, code1)
        ## FIXME: read the data!
        N6 <- .C("uint16_le", buf[offset+N+5], buf[offset+N+6], res=integer(1))$res
        if (N6 != N + 6)
            stop("error reading tuple number ", tuple, " (mismatch in redundant header-length flags)")
        offset <- offset + N + 6
        tuple <- tuple + 1
    }
    cat("pings:", NsingleBeamPing, " times:", Ntime, "\n")
    res@data <- list(latitude=latitude, longitude=longitude)
    res@processingLog <- processingLog(res@processingLog,
                                       paste("read.echosounder(\"", filename, ", debug=", debug, ")"))
    ## FIXME insert data, pl etc
    res
}
## test (until I can get a dataset)
##   writeBin(as.raw(c(0x00, 0x00, 0xFF, 0xFF, 0x00, 0x06)), file("test", "wb"))
##   source('~/src/R-kelley/oce/R/echosounder.R'); read.echosounder('test')

summary.echosounder <- function(object, ...)
{
    cat("Echosounder Summary\n-------------------\n\n")
    showMetadataItem(object, "filename", "File source:        ")
    warning("FIXME: should summarize now")
}
