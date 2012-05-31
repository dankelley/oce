setMethod(f="initialize",
          signature="lisst",
          definition=function(.Object, filename="") {
              .Object@metadata$filename <- filename
              .Object@processingLog$time <- as.POSIXct(Sys.time())
              .Object@processingLog$value <- "create 'lisst' object"
              return(.Object)
          })

setMethod(f="plot",
          signature="lisst",
          definition=function(x, which = c(1, 37, 38), debug=getOption("oceDebug"), ...) {
              oceDebug(debug, "\b\bplot.lisst(..., which=c(", paste(which, collapse=","), "),...) {\n", sep="")
              nw <- length(which)
              which2 <- vector("numeric", nw)
              for (w in 1:nw) {
                  ww <- which[w]
                  if (is.numeric(which[w])) {
                      which2[w] <- which[w]
                  } else if (length(grep("^c[123]", which[w]))) {
                      which2[w] <- as.numeric(substr(which[1], 2, 4))
                  } else if (ww == "pressure") {
                      which2[w] <- 37
                  } else if (ww == "temperature") {
                      which2[w] <- 38
                  }
              }
              par(mfrow=c(nw, 1))
              time <- x[["time"]]
              for (w in 1:nw) {
                  ww <- which2[w]
                  if (ww <= 32) {
                      oce.plot.ts(time, x@data[[which2[w]]], ylab=paste("C", ww, sep=""))
                  } else if (ww == 37) {
                      oce.plot.ts(time, x[["pressure"]], ylab=resizableLabel("p"))
                  } else if (ww == 38) {
                      oce.plot.ts(time, x[["temperature"]], ylab=resizableLabel("T"))
                  }
              }
          })

read.lisst <- function(file, year, tz="UTC")
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
    ncols <- ncols(data)
    if (ncols < 42)
        stop("data file must hold at least 42 space-separated columns")
    if (ncols > 42) {
        warning("data file has more than 42 columns; only first 42 are used")
        data <- data[,1:42]
    }
    names(data)[1:32] <-  paste("C", 1:32, sep="")
    names(data)[33] <- "lts"                  # laserTransmissionSensor
    names(data)[34] <- "voltage"
    names(data)[35] <- "aux"
    names(data)[36] <- "lrs"                  # laserReferenceSensor
    names(data)[37]<-"pressure"
    names(data)[38]<-"temperature"
    names(data)[39] <- "dayhour"
    names(data)[40] <- "minutesecond"
    names(data)[41] <- "transmission"
    names(data)[42] <- "beam"
    day <- floor(data$dayhour/100)
    hour <- data$dayhour - 100 * day
    minute <- floor(data$minutesecond/100)
    second <- data$minutesecond - 100 * minute
    decimalday <- day + hour / 24 + minute / 60 / 24 + second / 24 / 60 / 60
    if (missing(year)) 
        year <- strtrim(Sys.Date(),4) # a somewhat whacky default
    t0 <- as.POSIXct(paste(year, "-01-01 00:00:00", sep=""), tz=tz)
    data$time <- t0 + 86400 * decimalday / 365.25
    rval <- new('lisst', filename)
    rval@data <- data
    rval@processingLog <- processingLog(rval@processingLog, paste(deparse(match.call()), sep="", collapse=""))
    rval
}

summary.lisst <- function(object, ...)
{
    cat("LISST Summary\n-----------\n\n")
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
    threes <- matrix(nrow=ndata, ncol=3)
    for (i in 1:ndata) {
        if (names(object@data)[i] != "time") {
            threes[i,] <- threenum(object@data[[i]])
        }
    }
    rownames(threes) <- paste("   ", names(object@data))
    colnames(threes) <- c("Min.", "Mean", "Max.")
    print(threes, indent='  ')
    processingLogShow(object)
}

