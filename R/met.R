setMethod(f="initialize",
          signature="met",
          definition=function(.Object, time, temperature, pressure, u, v, filename="") {
              if (!missing(time)) .Object@data$time <- time
              if (!missing(temperature)) .Object@data$temperature <-temperature 
              if (!missing(pressure)) .Object@data$pressure <- pressure
              if (!missing(u)) .Object@data$u <- u
              if (!missing(v)) .Object@data$v <- v
              .Object@metadata$filename <- filename
              .Object@processingLog$time=c(.Object@processingLog$time, Sys.time())
              .Object@processingLog$value=c(.Object@processingLog$value, "create 'met' object")
              return(.Object)
          })

as.met <- function(time, temperature, pressure, u, v, filename="(constructed from data)")
{
    if (missing(time)) stop("must provide time")
    time <- as.POSIXct(time) # in case it's POSIXlt or a string
    n <- length(time)
    if (missing(temperature)) temperature <- rep(NA, n)
    else if (length(temperature) != n) stop("length of 'temperature' must match length of 'time'")
    if (missing(pressure)) pressure <- rep(NA, n)
    else if (length(pressure) != n) stop("length of 'pressure' must match length of 'time'")
    if (missing(u)) u <- rep(NA, n)
    else if (length(u) != n) stop("length of 'u' must match length of 'time'")
    if (missing(v)) v <- rep(NA, n)
    else if (length(v) != n) stop("length of 'v' must match length of 'time'")
    new('met', time=time, temperature=temperature, pressure=pressure, u=u, v=v, filename=filename)
}

read.met <- function(file, type=NULL, skip, 
                     tz=getOption("oceTz"),
                     debug=getOption("oceDebug"), processingLog, ...)
{
    oceDebug(debug, "\b\bread.met() {\n")
    if (is.character(file)) {
        filename <- fullFilename(file)
        file <- file(file, "r")
        on.exit(close(file))
    } else {
        filename <- ""
    }
    if (!inherits(file, "connection"))
        stop("argument `file' must be a character string or connection")
    if (!isOpen(file)) {
        open(file, "r")
        on.exit(close(file))
    }
    header <- readLines(file, encoding="latin1")
    ##print(header[1:19])
    headerItem <- function(header, name, numeric=TRUE) {
        if (length(i <- grep(name, header))) {
            if (numeric)
                as.numeric(sub("[^d](.*)[^d]$", "\\1", strsplit(header[i], ",")[[1]][2]))
            else
                sub("[^d](.*)[^d]$", "\\1", strsplit(header[i], ",")[[1]][2])
        } else {
            NA
        }
    }
    elevation <- headerItem(header, "Elevation")
    latitude <- headerItem(header, "Latitude")
    longitude <- headerItem(header, "Longitude")
    station <- headerItem(header, "Station Name", FALSE)
    province <- headerItem(header, "Province", FALSE) # is this too specific to Canada??
    climateIdentifier <- headerItem(header, "Climate Identifier", FALSE)
    WMOIdentifier <- headerItem(header, "WMO Identifier", FALSE)
    TCIdentifier <- headerItem(header, "TC Identifier", FALSE)
    Identifier <- headerItem(header, "Climate Identifier", FALSE)
    if (missing(skip)) {
        skip <- grep("^\"Date/Time\"", header)[1] - 1
    }
    res <- new('met', time=1)
    res@metadata <- list(latitude=latitude,
                         longitude=longitude,
                         elevation=elevation,
                         climateIdentifier=climateIdentifier,
                         WMOIdentifier=WMOIdentifier,
                         TCIdentifier=TCIdentifier,
                         filename=filename)
    rawData <- read.csv(filename, skip=skip, encoding="latin1", header=TRUE)
    time <- strptime(paste(rawData$Year, rawData$Month, rawData$Day, rawData$Time), "%Y %m %d %H:%M", tz=tz)
    temperature <- as.numeric(rawData[["Temp...C."]])
    pressure <- as.numeric(rawData[["Stn.Press..kPa."]])
    speed <- as.numeric(rawData[["Wind.Spd..km.h."]]) * 1000 / 3600
    direction <- 10 * as.numeric(rawData[["Wind.Dir..10.s.deg."]])
    u <- speed * cos(-direction * atan2(1, 1) / 45)
    v <- speed * sin(-direction * atan2(1, 1) / 45)
    res@data <- list(time=time, temperature=temperature, pressure=pressure, u=u, v=v)
    res
}

setMethod(f="plot",
           signature=signature("met"),
           definition=function(x, which = 1:4,
                               mgp=getOption("oceMgp"),
                               mar=c(mgp[1]+1,mgp[1]+1,mgp[1]+1,mgp[1]+1),
                               debug=getOption("oceDebug"),
                               ...)
           {
               oceDebug(debug, "\b\bplot.met() {\n")
               opar <- par(no.readonly = TRUE)
               nw <- length(which)
               if (nw > 1) on.exit(par(opar))
               par(mfrow=c(nw, 1), mgp=mgp, mar=mar)
               for (w in 1:nw) {
                   oceDebug(debug, "which=", w, "\n")
                   if (which[w] == 1) {
                       oce.plot.ts(x@data$time, x@data$temperature, ylab=resizableLabel("T", "y"))
                   } else if (which[w] == 2) {
                       oce.plot.ts(x@data$time, x@data$pressure, ylab="Pressure [kPa]")
                   } else if (which[w] == 3) {
                       oce.plot.ts(x@data$time, x@data$u, ylab=resizableLabel("eastward", "y"))
                   } else if (which[w] == 4) {
                       oce.plot.ts(x@data$time, x@data$v, ylab=resizableLabel("northward", "y"))
                   }
               }
           })


summary.met <- function(object, ...)
{
    cat("Met Summary\n-----------\n\n")
    showMetadataItem(object, "latitude", "Latitude   ")
    showMetadataItem(object, "longitude", "Longitude ")
    showMetadataItem(object, "elevation", "Elevation   ")
    showMetadataItem(object, "climateIdentifier", "Climate Identifer ")
    showMetadataItem(object, "WMOIdentifier", "WMO Identifer ")
    showMetadataItem(object, "TCIdentifier", "TC Identifer ")
    cat("* Statistics of subsample::\n")
    ndata <- length(object@data)
    threes <- matrix(nrow=ndata-1, ncol=3)
    for (i in 2:ndata) { # skip time
        threes[i-1,] <- threenum(object@data[[i]])
    }
    rownames(threes) <- paste("   ", names(object@data)[-1])
    colnames(threes) <- c("Min.", "Mean", "Max.")
    print(threes, indent='  ')
    processingLogShow(object)
} 

 
