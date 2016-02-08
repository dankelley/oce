setMethod(f="initialize",
          signature="lisst",
          definition=function(.Object, filename="", longitude=NA, latitude=NA) {
              .Object@metadata$filename <- filename
              .Object@metadata$longitude <- longitude
              .Object@metadata$latitude <- latitude
              .Object@processingLog$time <- as.POSIXct(Sys.time())
              .Object@processingLog$value <- paste("create 'lisst' object with", 
                                                   " filename=\"", filename, "\"", 
                                                   ", longitude=", longitude,
                                                   ", latitude=",
                                                   latitude, sep="")
              return(.Object)
          })


setMethod(f="summary",
          signature="lisst",
          definition=function(object, ...) {
              cat("LISST Summary\n-------------\n\n")
              showMetadataItem(object, "filename", "File source:        ")
              callNextMethod()
          })


setMethod(f="plot",
          signature="lisst",
          definition=function(x, which = c(16, 37, 38), tformat, debug=getOption("oceDebug"), ...) {
              oceDebug(debug, "plot.lisst(..., which=c(", paste(which, collapse=","), "),...) {\n", sep="", unindent=1)
              nw <- length(which)
              oceDebug(debug, "which:", which, "\n")
              which <- oce.pmatch(which,
                                  list(C1=1, C2=2, C3=3, C4=4, C5=5, C6=6, C7=7, C8=8, C9=9, C10=10,
                                      C11=11, C12=12, C13=13, C14=14, C15=15, C16=16, C17=17, C18=18, C19=19, C20=20,
                                      C21=21, C22=22, C23=23, C24=24, C25=25, C26=26, C27=27, C28=28, C29=29, C30=30,
                                      C31=31, C32=32,
                                      lts=33, voltage=34, aux=35, lrs=36,
                                      pressure=37, temperature=38, transmission=41, beam=42))
              oceDebug(debug, "which:", which, "\n")
              opar <- par(no.readonly = TRUE)
              if (length(which) > 1) on.exit(par(opar))
              par(mfrow=c(nw, 1))
              time <- x[["time"]]
              for (w in 1:nw) {
                  ww <- which[w]
                  if      (ww <= 32) oce.plot.ts(time, x@data[[which[w]]],
                                                 ylab=paste(gettext("Size Class #", domain="R-oce"), ww, sep=""),
                                                 tformat=tformat, debug=debug-1, ...)
                  else if (ww == 33) oce.plot.ts(time, x[["lts"]],
                                                 ylab=gettext("Laser Trans. Sensor", domain="R-oce"),
                                                 tformat=tformat, debug=debug-1, ...)
                  else if (ww == 34) oce.plot.ts(time, x[["voltage"]],
                                                 ylab=gettext("Voltage", domain="R-oce"),
                                                 tformat=tformat, debug=debug-1, ...)
                  else if (ww == 35) oce.plot.ts(time, x[["aux"]],
                                                 ylab=gettext("Ext. Aux. Input", domain="R-oce"),
                                                 tformat=tformat, debug=debug-1, ...)
                  else if (ww == 36) oce.plot.ts(time, x[["lrs"]],
                                                 ylab=gettext("Laser Ref. Sensor", domain="R-oce"),
                                                 tformat=tformat, debug=debug-1, ...)
                  else if (ww == 37) oce.plot.ts(time, x[["pressure"]],
                                                 ylab=resizableLabel("p"),
                                                 tformat=tformat, debug=debug-1, ...)
                  else if (ww == 38) oce.plot.ts(time, x[["temperature"]],
                                                 ylab=resizableLabel("T"),
                                                 tformat=tformat, debug=debug-1, ...)
                  else if (ww == 41) oce.plot.ts(time, x[["transmission"]],
                                                 ylab=gettext("Transmission %", domain="R-oce"),
                                                 tformat=tformat, debug=debug-1, ...)
                  else if (ww == 42) oce.plot.ts(time, x[["beam"]],
                                                 ylab=gettext("Beam-C [1/m]", domain="R-oce"),
                                                 tformat=tformat, debug=debug-1, ...)
              }
          })



as.lisst <- function(data, filename="", year=0, tz="UTC", longitude=NA, latitude=NA)
{
    res <- new("lisst", filename=filename, latitude=latitude, longitude=longitude)
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
    res@data <- data
    res@processingLog <- processingLogAppend(res@processingLog, paste(deparse(match.call()), sep="", collapse=""))
    names <- names(data)
    if ("pressure" %in% names) res@metadata$units$pressure <- list(unit=expression(dbar), scale="")
    if ("temperature" %in% names) res@metadata$units$temperature <- list(unit=expression(degree*C), scale="")
    res
}

read.lisst <- function(file, year=0, tz="UTC", longitude=NA, latitude=NA)
{
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
    res <- as.lisst(data, filename=filename, year=year, tz=tz, longitude=longitude, latitude=latitude)
    res@processingLog <- processingLogAppend(res@processingLog, paste(deparse(match.call()), sep="", collapse=""))
    names <- names(data)
    if ("pressure" %in% names) res@metadata$units$pressure <- list(unit=expression(dbar), scale="")
    if ("temperature" %in% names) res@metadata$units$temperature <- list(unit=expression(degree*C), scale="")
    res
}


