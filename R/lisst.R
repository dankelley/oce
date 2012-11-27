setMethod(f="initialize",
          signature="lisst",
          definition=function(.Object, filename="", latitude=NA, longitude=NA) {
              .Object@metadata$filename <- filename
              .Object@metadata$latitude <- latitude
              .Object@metadata$longitude <- longitude
              .Object@processingLog$time <- as.POSIXct(Sys.time())
              .Object@processingLog$value <- paste("create 'lisst' object with", 
                                                   " filename=\"", filename, "\"", 
                                                   ", latitude=", latitude,
                                                   ", longitude=", longitude, sep="")
              return(.Object)
          })

setMethod(f="plot",
          signature="lisst",
          definition=function(x, which = c(16, 37, 38), tformat, debug=getOption("oceDebug"), ...) {
              oceDebug(debug, "\b\bplot.lisst(..., which=c(", paste(which, collapse=","), "),...) {\n", sep="")
              nw <- length(which)
              which2 <- vector("numeric", nw)
              oceDebug(debug, "which=c(", paste(which, collapse=","), ")\n", sep="")
              for (w in 1:nw) {
                  ww <- which[w]
                  if (is.numeric(which[w])) which2[w] <- which[w]
                  else if (length(grep("^C[123]", which[w]))) which2[w] <- as.numeric(substr(which[1], 2, 4))
                  else if (ww == "lts")  which2[w] <- 33
                  else if (ww == "voltage")  which2[w] <- 34
                  else if (ww == "aux") which2[w] <- 35
                  else if (ww == "lrs") which2[w] <- 36
                  else if (ww == "pressure") which2[w] <- 37
                  else if (ww == "temperature") which2[w] <- 38
                  else if (ww == "transmission") which2[w] <- 41
                  else if (ww == "beam") which2[w] <- 42
                  else stop("cannot handle which value \"", which[w], "\"")
              }
              oceDebug(debug, "which2=c(", paste(which2, collapse=","), ")\n", sep="")
              par(mfrow=c(nw, 1))
              time <- x[["time"]]
              for (w in 1:nw) {
                  ww <- which2[w]
                  if      (ww <= 32) oce.plot.ts(time, x@data[[which2[w]]], ylab=paste("Size Class #", ww, sep=""), tformat=tformat, debug=debug-1, ...)
                  else if (ww == 33) oce.plot.ts(time, x[["lts"]], ylab="Laser Trans. Sensor", tformat=tformat, debug=debug-1, ...)
                  else if (ww == 34) oce.plot.ts(time, x[["voltage"]], ylab="Voltage", tformat=tformat, debug=debug-1, ...)
                  else if (ww == 35) oce.plot.ts(time, x[["aux"]], ylab="Ext. Aux. Input", tformat=tformat, debug=debug-1, ...)
                  else if (ww == 36) oce.plot.ts(time, x[["lrs"]], ylab="Laser Ref. Sensor", tformat=tformat, debug=debug-1, ...)
                  else if (ww == 37) oce.plot.ts(time, x[["pressure"]], ylab=resizableLabel("p"), tformat=tformat, debug=debug-1, ...)
                  else if (ww == 38) oce.plot.ts(time, x[["temperature"]], ylab=resizableLabel("T"), tformat=tformat, debug=debug-1, ...)
                  else if (ww == 41) oce.plot.ts(time, x[["transmission"]], ylab="Transmission [%]", tformat=tformat, debug=debug-1, ...)
                  else if (ww == 42) oce.plot.ts(time, x[["beam"]], ylab="Beam-C [1/m]", tformat=tformat, debug=debug-1, ...)
              }
          })

as.lisst <- function(data, filename="", year=0, tz="UTC", latitude=NA, longitude=NA)
{
    rval <- new("lisst", filename=filename, latitude=latitude, longitude=longitude)
    ncols <- ncol(data)
    if (ncols < 42)
        stop("data file must hold at least 42 space-separated columns")
    if (ncols > 42) {
        warning("data file has more than 42 columns; only first 42 are used")
        data <- data[,1:42]
    }
    data <- data.frame(data)
    names <- rep("", length.out=42)
    names[1:32] <-  paste("C", 1:32, sep="")
    names[33] <- "lts"                  # laserTransmissionSensor
    names[34] <- "voltage"
    names[35] <- "aux"
    names[36] <- "lrs"                  # laserReferenceSensor
    names[37] <- "pressure"
    names[38] <- "temperature"
    names[39] <- "dayhour"
    names[40] <- "minutesecond"
    names[41] <- "transmission"
    names[42] <- "beam"
    names(data) <- names
    day <- floor(data$dayhour/100)
    hour <- data$dayhour - 100 * day
    minute <- floor(data$minutesecond/100)
    second <- data$minutesecond - 100 * minute
    decimalday <- day + hour / 24 + minute / 60 / 24 + second / 24 / 60 / 60
    t0 <- as.POSIXct(paste(year, "-01-01 00:00:00", sep=""), tz=tz)
    data$time <- t0 + 86400 * decimalday / 365.25
    rval@data <- data
    rval@processingLog <- processingLog(rval@processingLog, paste(deparse(match.call()), sep="", collapse=""))
    rval
}

read.lisst <- function(file, year=0, tz="UTC", latitude=NA, longitude=NA)
{
    processingLog <- paste(deparse(match.call()), sep="", collapse="")

    filename <- NULL
    if (is.character(file)) {
        filename <- fullFilename(file)
        file <- file(file, "r")
        on.exit(close(file))
    }
    if (!inherits(file, "connection"))
        stop("argument `file' must be a character string or connection")
    if (!isOpen(file)) {
        open(file, "r")
        on.exit(close(file))
    }
    data <- read.table(file, header=FALSE)
    as.lisst(data, filename, year, tz, latitude, longitude)
}

summary.lisst <- function(object, ...)
{
    cat("LISST Summary\n-------------\n\n")
    showMetadataItem(object, "filename", "File source:        ")
    start <- object@data$time[1]
    dt <- as.numeric(object@data$time[2]) - as.numeric(object@data$time[1])
    end <- object@data$time[length(object@data$time)]
    cat(sprintf("* Measurements:       %s %s to %s %s sampled at %.4g Hz\n",
                format(start), attr(start, "tzone"),
                format(end), attr(end, "tzone"),
                1 / dt))
    cat("* Statistics of subsample::\n")
    ndata <- length(object@data)
    threes <- matrix(nrow=ndata-1, ncol=3)
    names <- names(object@data)
    for (i in 1:ndata) {
        if (names[i] != "time") {
            threes[i,] <- threenum(object@data[[i]])
        }
    }
    rownames(threes) <- paste("   ", names[seq.int(1, -1 + length(names))])
    colnames(threes) <- c("Min.", "Mean", "Max.")
    print(threes, indent='  ')
    processingLogShow(object)
}

