## vim:textwidth=128:expandtab:shiftwidth=4:softtabstop=4


#' Class to store meteorological data
#' 
#' Class to store meteorological data, with standard slots \code{metadata},
#' \code{data} and \code{processingLog}.  For objects created with
#' \code{\link{read.met}}, the \code{data} slot will contain all the columns
#' within the original file (with some guesses as to units) in addition to
#' several calculated quantities such as \code{u} and \code{v}, which are
#' velocities in m/s (not the km/h stored in typical data files), and which
#' obey the oceanographic convention that \code{u>0} is a wind towards the
#' east.
#' 
#' @section Methods:
#' 
#' \emph{Accessing values.} For an object named \code{m}, temperature (in degC)
#' may be accessed as \code{m[["temperature"]]}, dew point (in degC) as
#' \code{m[["dewPoint"]]}, pressure (in kPa) as \code{m[["pressure"]]},
#' eastward wind component (in m/s) as \code{m[["u"]]}, northward wind
#' component (in m/s) as \code{m[["v"]]}.  \strong{Caution:} the other elements
#' stored in the dataset are mainly in the format of the source file, and thus
#' their use requires some extra knowledge; for example,
#' \code{m[["direction"]]} yields the wind direction, measured in 10-degree
#' units positive clockwise from North.  The filename from which the data came
#' (if any) may be found with \code{m[["filename"]]}.  Items in \code{metadata}
#' must be specifield by full name, but those in \code{data} may be
#' abbreviated, so long as the abbreviation is unique.
#' 
#' \emph{Assigning values.} Everything that may be accessed may also be
#' assigned, e.g.
#' \preformatted{
#' m[["temperature"]] <- 1 + m[["temperature"]]
#' }
#' increases temperature by 1C.
#' 
#' @author Dan Kelley
#' @family classes provided by \code{oce}
#' @family functions that handle \code{met} data
setClass("met", contains="oce")


#' Sample meteorological object
#' 
#' This is sample \code{met} object containing data for Halifax, Nova Scotia,
#' during September of 2003 (the period during which Hurricane Juan struck the
#' city).
#' 
#' @name met
#' @docType data
#' @source Downloaded from the Environment Canada website on May 29, 2014, and
#' processed as follows.
#' \preformatted{
#' met <- read.met('http://climate.weather.gc.ca/climateData/bulkdata_e.html?format=csv&stationID=6358&Year=2003&Month=9&Day=17&timeframe=1&submit=Download+Data')
#' met[['time']] <- met[['time']] + 4 * 3600
#'}
#' (Note the conversion from local standard time to UTC.)
#'
#' @family datasets provided with \code{oce}
NULL

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


#' Summarize a met object
#' 
#' Summarizes some of the data in a \code{met} object.
#' 
#' Pertinent summary information is presented, including the sampling location,
#' data ranges, etc.
#' 
#' @param object A \code{met} object, i.e. one inheriting from \code{\link{met-class}}.
#' @param \dots further arguments passed to or from other methods.
#' @author Dan Kelley
#' @family functions that handle \code{met} data
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
              callNextMethod()
          })


#' Subset a met object
#' 
#' This function is somewhat analogous to \code{\link{subset.data.frame}}.
#' 
#' @param x An object inheriting from \code{\link{met-class}}.
#' @param subset An expression indicating how to subset \code{x}.
#' @param \dots ignored.
#' @return A new \code{met} object.
#' @author Dan Kelley
#' @examples
#' library(oce)
#' data(met)
#' # Few days surrounding Hurricane Juan
#' plot(subset(met, time > as.POSIXct("2003-09-27", tz="UTC")))
#' 
#' @family functions that handle \code{met} data
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
 


#' Coerce data into met dataset
#' 
#' Coerces a dataset into a met dataset.
#' 
#' This function is used by \code{\link{read.met}}, and may be used to
#' construct objects that behave as though read by that function.
#' 
#' @param time Vector of obseration times (or character strings that can be
#' coerced into times).
#' @param temperature vector of temperatures.
#' @param pressure vector of pressures.
#' @param u vector of eastward wind speed in m/s.
#' @param v vector of northward wind speed in m/s.
#' @param filename optional string indicating data source
#' @return An object of \code{\link{met-class}}.
#' @author Dan Kelley
#' @family functions that handle \code{met} data
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



#' Read a meteorological data file
#' 
#' Read a meteorological data file
#' 
#' Reads a comma-separated value file in the format used by the Meteorological
#' Service of Canada (MSC).  The agency does not publish a format for these
#' files, so this function was based on a study of a few sample files, and it
#' may fail for other files, if MSC changes the format.
#' 
#' @param file a connection or a character string giving the name of the file
#' to load.
#' @param type if \code{NULL}, then the first line is studied, in order to
#' determine the file type.  If \code{type="msc"}, then a file as formatted by
#' the Meteorological Service of Canada is assumed.
#' @param skip optional number of lines of header that occur before the actual
#' data.  If this is not supplied, \code{read.met} scans the file until it
#' finds a line starting with \code{"Date/Time"}, and considers all lines above
#' that to be header.
#' @param tz timezone assumed for time data
#' @param debug a flag that turns on debugging.  Set to 1 to get a moderate
#' amount of debugging information, or to 2 to get more.
#' @param processingLog if provided, the action item to be stored in the log.
#' (Typically only provided for internal calls; the default that it provides is
#' better for normal calls by a user.)
#' @param \dots additional arguments, passed to called routines.
#' @return An object of \code{\link[base]{class}} \code{"met"}, of which the
#' \code{data} slot contains vectors \code{time}, \code{temperature},
#' \code{pressure}, \code{u}, and \code{v}.  The velocity components have units
#' m/s and are the components of the vector of wind direction.  In other words,
#' the oceanographic convention on velocity is employed, not the meteorological
#' one; the weather forecaster's "North wind" has positive \code{v} and zero
#' \code{u}.  In addition to these things, \code{data} also contains items
#' called \code{wind} (in km/h) and \code{direction} (in tenths of a degree),
#' taken straight from the data file.
#' @section Note: There seem to be several similar formats in use, so this
#' function may not work in all cases.
#' @author Dan Kelley
#' @examples
#' \dontrun{
#' library(oce)
#' met <- read.met("ile-rouge-eng-hourly-06012008-06302008.csv")
#' plot(met, which=3:4)
#' }
#' 
#' @family functions that handle \code{met} data
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
    ##province <- textItem(text, "Province", FALSE) # is this too specific to Canada??
    climateIdentifier <- textItem(text, "Climate Identifier", FALSE)
    WMOIdentifier <- textItem(text, "WMO Identifier", FALSE)
    TCIdentifier <- textItem(text, "TC Identifier", FALSE)
    ##Identifier <- textItem(text, "Climate Identifier", FALSE)
    if (missing(skip)) {
        skip <- grep("^\"Date/Time\"", text)[1] - 1
    }
    res@metadata$latitude <- latitude
    res@metadata$longitude <- longitude
    res@metadata$elevation <- elevation
    res@metadata$station <- station
    res@metadata$climateIdentifier <- climateIdentifier
    res@metadata$WMOIdentifier <- WMOIdentifier
    res@metadata$TCIdentifier <- TCIdentifier
    res@metadata$filename <- filename
    rawData <- read.csv(text=text, skip=skip, encoding="latin1", header=TRUE)
    time <- strptime(paste(rawData$Year, rawData$Month, rawData$Day, rawData$Time), "%Y %m %d %H:%M", tz=tz)
    ##ntime <- length(time)
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
    ##j <- grep("^Temp.*C.*$", names(rawData))[1]
    ##temperature <- if (1 == length(j)) as.numeric(rawData[,j]) else rep(NA, ntime)
    ##j <- grep("^Stn.*Press.*kPa.*$", names(rawData))[1]
    ##pressure <- if (1 == length(j)) as.numeric(rawData[,j]) else rep(NA, ntime)
    ##j <- grep("^Wind.*Spd.*km.*$", names(rawData))[1]
    ##wind <- if (1 == length(j)) as.numeric(rawData[,j]) else rep(NA, ntime)
    ##speed <- wind * 1000 / 3600        # convert from km/h to m/s
    ##j <- grep("^Wind.*deg.*$", names(rawData))[1]
    ##direction <- if (1 == length(j)) as.numeric(rawData[,j]) else rep(NA, ntime)
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
    names <- names(res@data)
    if ("wind" %in% names) res@metadata$units$wind <- list(unit=expression(km/h), scale="")
    if ("speed" %in% names) res@metadata$units$speed <- list(unit=expression(m/s), scale="")
    if ("u" %in% names) res@metadata$units$u <- list(unit=expression(m/s), scale="")
    if ("v" %in% names) res@metadata$units$v <- list(unit=expression(m/s), scale="")
    if ("pressure" %in% names) res@metadata$units$pressure <- list(unit=expression(kPa), scale="")
    if ("temperature" %in% names) res@metadata$units$temperature <- list(unit=expression(degree*C), scale="ITS-90")
    if ("dewPoint" %in% names) res@metadata$units$dewPoint <- list(unit=expression(degree*C), scale="ITS-90")
    if ("visibility" %in% names) res@metadata$units$visibility <- list(unit=expression(km), scale="")
    res
}


#' Plot meteorological data
#' 
#' Plot meteorological data
#' 
#' Creates a multi-panel summary plot of data measured in a meteorological data
#' set.  cast. The panels are controlled by the \code{which} argument.
#' Normally, 4 panels are specified with the \code{which}, but it can also be
#' useful to specify less than 4 panels, and then to draw other panels after
#' this call.
#' 
#' If more than one panel is drawn, then on exit from \code{plot.met}, the
#' value of \code{par} will be reset to the value it had before the function
#' call.  However, if only one panel is drawn, the adjustments to \code{par}
#' made within \code{plot.met} are left in place, so that further additions may
#' be made to the plot.
#' 
#' @aliases plot.met plot,met,missing-method plot,met-method
#' @param x A \code{met} object, e.g. as read by \code{\link{read.met}}, or a
#' list containing items named \code{salinity} and \code{temperature}.
#' @param which list of desired plot types.  \itemize{ \item \code{which=1}
#' gives a time-series plot of temperature \item \code{which=2} gives a
#' time-series plot of pressure \item \code{which=3} gives a time-series plot
#' of the x (eastward) component of velocity \item \code{which=4} gives a
#' time-series plot of the y (northward) component of velocity }
#' @param tformat optional argument passed to \code{\link{oce.plot.ts}}, for
#' plot types that call that function.  (See \code{\link{strptime}} for the
#' format used.)
#' @param \dots optional arguments passed to plotting functions.
#' @template mgpTemplate
#' @template marTemplate
#' @template debugTemplate
#' @author Dan Kelley
#' @examples
#' library(oce)
#' data(met)
#' plot(met, which=3:4)
#' 
#' @family functions that plot \code{oce} data
#' @family functions that handle \code{met} data
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



 
