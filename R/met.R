setMethod(f="initialize",
          signature="met",
          definition=function(.Object, time, temperature, pressure, u, v, filename="") {
              if (!missing(time)) .Object@data$time <- time
              if (!missing(temperature)) .Object@data$temperature <-temperature 
              if (!missing(pressure)) .Object@data$pressure <- pressure
              if (!missing(u)) .Object@data$u <- u
              if (!missing(v)) .Object@data$v <- v
              .Object@metadata$filename <- filename
              .Object@processingLog$time <- as.POSIXct(Sys.time())
              .Object@processingLog$value <- "create 'met' object"
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
    res <- new('met', time=1)
    text <- readLines(file, encoding="latin1")
    ##print(header[1:19])
    textItem <- function(text, name, numeric=TRUE) {
        if (length(i <- grep(name, text))) {
            if (numeric)
                as.numeric(sub("[^d](.*)[^d]$", "\\1", strsplit(text[i], ",")[[1]][2]))
            else
                sub("[^d](.*)[^d]$", "\\1", strsplit(text[i], ",")[[1]][2])
        } else {
            NA
        }
    }
    elevation <- textItem(text, "Elevation")
    latitude <- textItem(text, "Latitude")
    longitude <- textItem(text, "Longitude")
    station <- textItem(text, "Station Name", FALSE)
    province <- textItem(text, "Province", FALSE) # is this too specific to Canada??
    climateIdentifier <- textItem(text, "Climate Identifier", FALSE)
    WMOIdentifier <- textItem(text, "WMO Identifier", FALSE)
    TCIdentifier <- textItem(text, "TC Identifier", FALSE)
    Identifier <- textItem(text, "Climate Identifier", FALSE)
    if (missing(skip)) {
        skip <- grep("^\"Date/Time\"", text)[1] - 1
    }
    res@metadata <- list(latitude=latitude,
                         longitude=longitude,
                         elevation=elevation,
                         climateIdentifier=climateIdentifier,
                         WMOIdentifier=WMOIdentifier,
                         TCIdentifier=TCIdentifier,
                         filename=filename)
    rawData <- read.csv(text=text, skip=skip, encoding="latin1", header=TRUE)
    time <- strptime(paste(rawData$Year, rawData$Month, rawData$Day, rawData$Time), "%Y %m %d %H:%M", tz=tz)
    ntime <- length(time)
    names <- names(rawData)
    ## Must use grep to identify columns, because the names are not fixed.  In some
    ## test files, temperature was in a column named "..Temp...C.", but in others
    ## it was in a column named "..Temp[*]C.", where "[*]" contains accented
    ## symbols.  The other columns may need similar treatment at some point,
    ## if files are encountered with e.g. a special symbol used for the degree
    ## sign in a wind direction, but for now they are accessed directly,
    ## partly to indicate the possibilities of coding, for those who find
    ## it necessary to adjust this code to work with other files.
    ##
    ## It would be good if someone from Environment Canada would take pity on a
    ## poor user, and convince the powers-that-be to settle on a single format
    ## and even (gasp) to document it.
    Tcol <- grep("^Temp.*C\\.", names) # sometimes they use a degree symbol in this name
    if (length(Tcol)) temperature <- as.numeric(rawData[,Tcol[1]])
    else temperature <- rep(NA, ntime)
    pressure <- as.numeric(rawData[["Stn.Press..kPa."]])
    speed <- as.numeric(rawData[["Wind.Spd..km.h."]]) * 1000 / 3600
    direction <- 10 * as.numeric(rawData[["Wind.Dir..10.s.deg."]])
    u <- -speed * sin(direction * atan2(1, 1) / 45)
    v <- -speed * cos(direction * atan2(1, 1) / 45)
    res@data <- list(time=time, temperature=temperature, pressure=pressure, u=u, v=v)
    if (missing(processingLog))
        processingLog <- paste(deparse(match.call()), sep="", collapse="")
    res@processingLog <- processingLog(res@processingLog, processingLog)
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
    showMetadataItem(object, "filename", "Source     ", quote=TRUE)
    showMetadataItem(object, "latitude", "Latitude     ")
    showMetadataItem(object, "longitude", "Longitude   ")
    showMetadataItem(object, "elevation", "Elevation   ")
    showMetadataItem(object, "climateIdentifier", "Climate Identifer          ")
    showMetadataItem(object, "WMOIdentifier", "World Met Office Identifer ")
    showMetadataItem(object, "TCIdentifier", "Transport Canada Identifer ")
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

 
