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


setMethod(f="summary",
          signature="met",
          definition=function(object, ...) {
              cat("Met Summary\n-----------\n\n")
              showMetadataItem(object, "filename", "Source     ", quote=TRUE)
              showMetadataItem(object, "latitude", "Latitude     ")
              showMetadataItem(object, "longitude", "Longitude   ")
              showMetadataItem(object, "elevation", "Elevation   ")
              showMetadataItem(object, "climateIdentifier", "Climate Identifer          ")
              showMetadataItem(object, "WMOIdentifier", "World Met Office Identifer ")
              showMetadataItem(object, "TCIdentifier", "Transport Canada Identifer ")
              ndata <- length(object@data)
              threes <- matrix(nrow=ndata-1, ncol=3)
              isTime <- names(object@data) == "time"
              if (any(isTime))
                  cat("* Time ranges from", format(object@data$time[1]), "to", format(tail(object@data$time, 1)), "\n")
              ii <- 1
              for (i in 2:ndata) { # skip time
                  if (isTime[i])
                      next
                  threes[ii,] <- if (is.factor(object@data[[i]][1])) rep(NA, 3) else threenum(object@data[[i]])
                  ii <- ii + 1
              }
              rownames(threes) <- paste("   ", names(object@data)[-1])
              colnames(threes) <- c("Min.", "Mean", "Max.")
              print(threes, indent='  ')
              processingLogShow(object)
              invisible(NULL)
          })

setMethod(f="subset",
          signature="met",
          definition=function(x, subset, ...) {
              res <- new("met") # start afresh in case x@data is a data.frame
              res@metadata <- x@metadata
              res@processingLog <- x@processingLog
              for (i in seq_along(x@data)) {
                  r <- eval(substitute(subset), x@data, parent.frame(2))
                  r <- r & !is.na(r)
                  res@data[[i]] <- x@data[[i]][r]
              }
              names(res@data) <- names(x@data)
              subsetString <- paste(deparse(substitute(subset)), collapse=" ")
              res@processingLog <- processingLogAppend(res@processingLog, paste("subset.met(x, subset=", subsetString, ")", sep=""))
              res
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
    oceDebug(debug, "read.met() {\n", unindent=1)
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
    res@metadata$latitude <- latitude
    res@metadata$longitude <- longitude
    res@metadata$elevation <- elevation
    res@metadata$climateIdentifier <- climateIdentifier
    res@metadata$WMOIdentifier <- WMOIdentifier
    res@metadata$TCIdentifier <- TCIdentifier
    res@metadata$filename <- filename
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
    j <- grep("^Temp.*C.*$", names(rawData))[1]
    temperature <- if (1 == length(j)) as.numeric(rawData[,j]) else rep(NA, ntime)
    j <- grep("^Stn.*Press.*kPa.*$", names(rawData))[1]
    pressure <- if (1 == length(j)) as.numeric(rawData[,j]) else rep(NA, ntime)
    j <- grep("^Wind.*Spd.*km.*$", names(rawData))[1]
    wind <- if (1 == length(j)) as.numeric(rawData[,j]) else rep(NA, ntime)
    speed <- wind * 1000 / 3600        # convert from km/h to m/s
    j <- grep("^Wind.*deg.*$", names(rawData))[1]
    direction <- if (1 == length(j)) as.numeric(rawData[,j]) else rep(NA, ntime)
    rpd <- atan2(1, 1) / 45            # radian/degree

    names(rawData) <- decodeDataNames(names, "met")
    rawData[["speed"]] <- rawData[["wind"]] * 1000 / 3600 # convert km/h to m/s


    ## Note (90 - ) to get from "clockwise from north" to "anticlockwise from east"
    theta <- (90 - 10 * rawData[["direction"]]) * rpd 
    ## Note the (-) to get from "wind from" to "wind speed towards"
    rawData[["u"]] <- -rawData[["speed"]] * sin(theta)
    rawData[["v"]] <- -rawData[["speed"]] * cos(theta)
    zero <- is.na(rawData[["direction"]]) & rawData[["wind"]] == 0
    rawData[["u"]][zero] <- 0
    rawData[["v"]][zero] <- 0
    rawData[["time"]] <- time
    res@data <- rawData #list(time=time, temperature=temperature, pressure=pressure, u=u, v=v,
                     #wind=wind, direction=direction)
    if (missing(processingLog))
        processingLog <- paste(deparse(match.call()), sep="", collapse="")
    res@processingLog <- processingLogAppend(res@processingLog, processingLog)
    res
}

setMethod(f="plot",
           signature=signature("met"),
           definition=function(x, which = 1:4,
                               mgp=getOption("oceMgp"),
                               mar=c(mgp[1]+1,mgp[1]+1,mgp[1]+1,mgp[1]+1),
                               tformat,
                               debug=getOption("oceDebug"),
                               ...)
           {
               oceDebug(debug, "plot.met() {\n", unindent=1)
               opar <- par(no.readonly = TRUE)
               nw <- length(which)
               if (nw > 1) on.exit(par(opar))
               if (nw > 1)
                   par(mfrow=c(nw, 1), mgp=mgp, mar=mar)
               else
                   par(mgp=mgp, mar=mar)
               for (w in 1:nw) {
                   oceDebug(debug, "which=", w, "\n")
                   if (which[w] == 1 && any(!is.na(x@data$temperature))) {
                       oce.plot.ts(x@data$time, x@data$temperature, ylab=resizableLabel("T", "y"), tformat=tformat)
                   } else if (which[w] == 2 && any(!is.na(x@data$pressure))) {
                       oce.plot.ts(x@data$time, x@data$pressure, ylab="Pressure [kPa]", tformat=tformat)
                   } else if (which[w] == 3 && any(!is.na(x@data$u))) {
                       oce.plot.ts(x@data$time, x@data$u, ylab=resizableLabel("eastward", "y"), tformat=tformat)
                   } else if (which[w] == 4 && any(!is.na(x@data$v))) {
                       oce.plot.ts(x@data$time, x@data$v, ylab=resizableLabel("northward", "y"), tformat=tformat)
                   }
               }
               oceDebug(debug, "} # plot.met()\n", unindent=1)
           })



 
