## vim:textwidth=128:expandtab:shiftwidth=4:softtabstop=4

#' @title Class to Store Meteorological Data
#'
#' @description
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
#' component (in m/s) as \code{m[["v"]]}, and wind direction (in degrees
#' clockwise of North) as \code{"direction"}.
#' The filename from which the data came
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
#' @family things related to \code{met} data
setClass("met", contains="oce")

#' @title Extract Something From a Met Object
#' @param x A met object, i.e. one inheriting from \code{\link{met-class}}.
#' @template sub_subTemplate
#' @family things related to \code{met} data
setMethod(f="[[",
          signature(x="met", i="ANY", j="ANY"),
          definition=function(x, i, j, ...) {
              callNextMethod()         # [[
          })

#' @title Replace Parts of a Met Object
#' @param x An \code{met} object, i.e. inheriting from \code{\link{met-class}}
#' @template sub_subsetTemplate
#' @family things related to \code{met} data
setMethod(f="[[<-",
          signature(x="met", i="ANY", j="ANY"),
          definition=function(x, i, j, ..., value) {
              callNextMethod(x=x, i=i, j=j, ...=..., value=value) # [[<-
          })


#' @title Sample Met Object
#'
#' @description
#' This is sample \code{met} object containing data for Halifax, Nova Scotia,
#' during September of 2003 (the period during which Hurricane Juan struck the
#' city).
#'
#' @name met
#' @docType data
#' @source Downloaded from the Environment Canada website on February 1, 2017, and
#' processed as follows. (Note that the URL changes from time to time, and that it
#' was discovered by reverse-engineering the website, since this agency provides
#' no documentation on how to create URLs.)
#' \preformatted{
#' met <- read.met('http://climate.weather.gc.ca/climate_data/bulk_data_e.html?format=csv&stationID=6358&Year=2003&Month=9&timeframe=1&submit=Download+Data')
#' met <- oceSetData(met, "time", met[["time"]] + 4 * 3600, note="add 4h to local time to get UTC time")
#'}
#' Note the conversion from local standard time to UTC, done with 
#' \code{\link{oceSetData}} so that a comment will appear in the output
#' from \code{summary(met)}.
#'
#' @family datasets provided with \code{oce}
#' @family things related to \code{met} data
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


#' @title Summarize a Met Object
#'
#' @description
#' Pertinent summary information is presented, including the sampling location,
#' data ranges, etc.
#'
#' @param object A \code{met} object, i.e. one inheriting from \code{\link{met-class}}.
#' @param \dots further arguments passed to or from other methods.
#' @author Dan Kelley
#' @family things related to \code{met} data
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
              callNextMethod()         # summary
          })


#' @title Subset a Met Object
#'
#' @description
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
#' @family things related to \code{met} data
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



#' @title Coerce Data into Met Object
#'
#' @description
#' Coerces a dataset into a met dataset.
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
#' @family things related to \code{met} data
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


#' Download and Cache a met File
#'
#' Data are downloaded from \url{http://climate.weather.gc.ca} and cached locally.
#'
#' @details
#' The data are downloaded with \code{\link[utils]{download.file}}
#' pointed to the Environment Canada website \url{http://climate.weather.gc.ca},
#' using queries that had to be devised by reverse-engineering, since the agency
#' provides no documentation about the queries. Changes to query format are not
#' by any means unheard-of; queries that worked in 2016 failed in early 2017, for
#' example.
#'
#' The Station ID that is provided in the \code{id} argument
#' becomes part of the query used to download the data.
#' Confusingly, this is \emph{neither} the "Climate ID"
#' \emph{nor} the "WMO ID". Instead, it seems to be a creation of Environment
#' Canada.  Even worse, the Environment Canada documents state that the ID for 
#' a particular location may be changed at any time. Users are therefore advised to
#' look up the codes at
#' \code{ftp://ftp.tor.ec.gc.ca/Pub/Get_More_Data_Plus_de_donnees//Station Inventory EN.csv}
#' before using this function. This file can be searched by city name, and in other
#' ways.  (A future version of \code{download.met} might use this file, if enough
#' users request this feature.)
#'
#' @param id A number giving the "Station ID" of the station of interest. If not
#' provided, \code{id} defaults to 6358, for Halifax International Airport. See
#' \dQuote{Details}.
#'
#' @param year A number giving the year of interest. This defaults to the present
#' year, if not given.
#'
#' @param month A number giving the month of interest. This defaults to the present
#' month, if not given.
#'
#' @template downloadDestTemplate
#'
#' @template debugTemplate
#'
#' @return String indicating the full pathname to the downloaded file.
#'
#' @author Dan Kelley
#'
#' @examples
#'\dontrun{
#' library(oce)
#' ## Download data for Halifax International Airport, in September
#' ## of 2003. (This dataset is used for data(met) provided with oce.)
#' metFileName <- download.met(6358, 2003, 9)
#'}
#'
#' @seealso The work is done with \code{\link[utils]{download.file}}.
#'
#' @template downloadWarningTemplate
#'
#' @family functions that download files
#' @family things related to \code{met} data
download.met <- function(id, year, month, destdir="~/data/met", destfile, debug=getOption("oceDebug"))
{
    if (missing(id))
        id <- 6358
    id <- as.integer(id)
    today <- as.POSIXlt(Sys.time())
    if (missing(year))
        year <- today$year + 1900
    if (missing(month)) {
        month <- today$mon + 1         # so 1=jan etc
        month <- month - 1             # we want *previous* month, which should have data
        if (month == 1) {
            year <- year - 1
            month <- 12
        }
    }
    ## Next line is an example that worked as of Feb 2, 2017
    ## http://climate.weather.gc.ca/climate_data/bulk_data_e.html?format=csv&stationID=6358&Year=2003&Month=9&timeframe=1&submit=Download+Data
    url <- paste("http://climate.weather.gc.ca/climate_data/bulk_data_e.html?format=csv&stationID=",
                 id, "&Year=", year, "&Month=", month, "&timeframe=1&submit=Download+Data", sep="")
    oceDebug(debug, "url:", url, "\n")
    if (missing(destfile))
        destfile <- sprintf("met_%d_%d_%02d_%02d.csv", id, year, month, 1)
    oceDebug(debug, "destdir:", destdir, "\n")
    oceDebug(debug, "destfile:", destfile, "\n")
    destination <- paste(destdir, destfile, sep="/")
    oceDebug(debug, "destination:", destination, "\n")
    if (1 == length(list.files(path=destdir, pattern=paste("^", destfile, "$", sep="")))) {
        oceDebug(debug, "Not downloading", destfile, "because it is already present in", destdir, "\n")
    } else {
        download.file(url, destination)
        oceDebug(debug, "Downloaded file stored as '", destination, "'\n", sep="")
    }
    destination
}

#' Convert met Data Name to Oce Name
#'
#' @details
#' Interoperability between oce functions requires that standardized data names
#' be used, e.g. \code{"temperature"} for in-situ temperature. Very few
#' data-file headers name the temperature column in exactly that way, however,
#' and this function is provided to try to guess the names. The task is complicated
#' by the fact that Environment Canada seems to change the names of the columns,
#' e.g. sometimes a symbol is used for the degree sign, other times not.
#'
#' Several quantities in the returned object differ from their values in the source
#' file. For example, speed is converted from km/h to m/s, and angles are converted
#' from tens of degres to degrees. Also, some items are created from scratch, e.g.
#' \code{u} and \code{v}, the eastward and northward velocity, are computed from speed
#' and direction. (Note that e.g. u is positive if the wind blows to the east; the
#' data are thus in the normal Physics convention.)
#'
#' @param names a vector of character strings with original names
#' @param scheme an optional indication of the scheme that is employed. This may
#' be \code{"ODF"}, in which case \code{\link{ODFNames2oceNames}} is used,
#' or \code{"met"}, in which case some tentative code for met files is used.
#'
#' @return
#' Vector of strings for the decoded names. If an unknown scheme is provided,
#' this will just be \code{names}.
metNames2oceNames <- function(names, scheme)
{
    ##schemeGiven <- !missing(scheme)
    res <- names
    if (!missing(scheme)) {
        if (scheme == "ODF") {
            res <- ODFNames2oceNames(ODFnames=names, ODFunits=NULL)
        } else if (scheme == "met") {
            if (1 == length(i <- grep("^Data\\.Quality$", res))) res[i] <- "dataQuality"
            if (1 == length(i <- grep("^Dew\\.Point\\.Temp\\.\\.\\.C\\.$", res))) res[i] <- "dewPoint"
            if (1 == length(i <- grep("^Dew\\.Point\\.Temp\\.Flag$", res))) res[i] <- "dewPointFlag"
            if (1 == length(i <- grep("^Hmdx$", res))) res[i] <- "humidex"
            if (1 == length(i <- grep("^Hmdx\\.Flag$", res))) res[i] <- "humidexFlag"
            if (1 == length(i <- grep("^Rel\\.Hum\\.\\.\\.\\.$", res))) res[i] <- "humidity"
            if (1 == length(i <- grep("^Rel\\.Hum\\.Flag$", res))) res[i] <- "humidityFlag"
            if (1 == length(i <- grep("^Stn.*Press.*kPa.*$", res))) res[i] <- "pressure"
            if (1 == length(i <- grep("^Stn.Press.Flag$", res))) res[i] <- "pressureFlag"
            if (1 == length(i <- grep("^Temp.*C.*$", res))) res[i] <- "temperature"
            if (1 == length(i <- grep("^Temp.*Flag$", res))) res[i] <- "temperatureFlag"
            if (1 == length(i <- grep("^Visibility.*km.*$", res))) res[i] <- "visibility"
            if (1 == length(i <- grep("^Visibility.*Flag$", res))) res[i] <- "visibilityFlag"
            if (1 == length(i <- grep("^Wind.*Spd.*km.*$", res))) res[i] <- "wind"
            if (1 == length(i <- grep("^Wind.*Spd.*Flag$", res))) res[i] <- "windFlag"
            if (1 == length(i <- grep("^Wind\\.Dir\\.\\.10s\\.deg\\.$", res))) res[i] <- "direction"
            if (1 == length(i <- grep("^Wind\\.Dir\\.Flag$", res))) res[i] <- "directionFlag"
            if (1 == length(i <- grep("^Wind\\.Chill$", res))) res[i] <- "windChill"
            if (1 == length(i <- grep("^Wind\\.Chill\\.Flag$", res))) res[i] <- "windChillFlag"
            if (1 == length(i <- grep("^Weather$", res))) res[i] <- "weather"
        } else {
            warning("unknown scheme ", scheme)
        }
    } else {
        ## temperature
        col <- grep("temp", names, ignore.case=TRUE, useBytes=TRUE)
        if (1 == length(col))
            res[col] <- "temperature"
    }
    ## message("names,res >>")
    ## print(data.frame(names, res))
    ## message("<<")
    res
}



#' @title Read a Met File
#'
#' @description
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
#' @return An object of \code{\link[base]{class}} \code{"met"}, of which the
#' \code{data} slot contains vectors \code{time}, \code{temperature},
#' \code{pressure}, \code{u}, and \code{v}.  The velocity components have units
#' m/s and are the components of the vector of wind direction.  In other words,
#' the oceanographic convention on velocity is employed, not the meteorological
#' one; the weather forecaster's "North wind" has positive \code{v} and zero
#' \code{u}.  In addition to these things, \code{data} also contains 
#' \code{wind} (in km/h), taken straight from the data file.
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
#' @family things related to \code{met} data
read.met <- function(file, type=NULL, skip, tz=getOption("oceTz"), debug=getOption("oceDebug"))
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
    text <- readLines(file, encoding="UTF-8")
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
    names <- names(rawData)
    names(rawData) <- metNames2oceNames(names, "met")
    ## Quite a lot of things ae in weird units (km/h instead of m/s etc), so we will need to do some conversions.
    rawData[["speed"]] <- rawData[["wind"]] * 1000 / 3600 # convert km/h to m/s
    rawData[["direction"]] <- 10 * rawData[["direction"]] # convert 10s of degrees to degrees

    ## Note (90 - ) to get from "clockwise from north" to "anticlockwise from east"
    rpd <- atan2(1, 1) / 45            # radian/degree
    theta <- (90 - rawData[["direction"]]) * rpd
    ## Note the (-) to get from "wind from" to "wind speed towards"
    rawData[["u"]] <- -rawData[["speed"]] * sin(theta)
    rawData[["v"]] <- -rawData[["speed"]] * cos(theta)
    zero <- is.na(rawData[["direction"]]) & rawData[["wind"]] == 0
    rawData[["u"]][zero] <- 0
    rawData[["v"]][zero] <- 0
    rawData[["time"]] <- time
    res@data <- rawData
    pl <- paste("read.met(\"", filename, "\", type=", if (is.null(type)) "NULL" else type, ", tz=\"", tz, "\")", sep="")
    res@processingLog <- processingLogAppend(res@processingLog, pl)
    names <- names(res@data)
    res@metadata$dataNamesOriginal <- list()
    res@metadata$flags <- list()
    if ("dewPoint" %in% names) {
        res@metadata$units$dataQuality <- list(unit=expression(), scale="")
        res@metadata$dataNamesOriginal$dataQuality <- "Data Quality"
    }
    if ("dewPoint" %in% names) {
        res@metadata$units$dewPoint <- list(unit=expression(degree*C), scale="ITS-90")
        res@metadata$dataNamesOriginal$dewPoint <- "Dew Point Temp (\u00B0C)"
    }
    if ("direction" %in% names) {
        res@metadata$units$direction <- list(unit=expression(degree), scale="")
        res@metadata$dataNamesOriginal$direction <- "-" # we use deg, they use 10deg, so no original name
    }
    if ("humidex" %in% names) {
        res@metadata$units$humidex <- list(unit=expression(degree*C), scale="")
        res@metadata$dataNamesOriginal$humidex <- "Hmdx"
    }
    if ("humidity" %in% names) {
        res@metadata$units$humidity <- list(unit=expression(degree*C), scale="ITS-90")
        res@metadata$dataNamesOriginal$humidity <- "Rel Hum (%)"
    }
    if ("pressure" %in% names) {
        res@metadata$units$pressure <- list(unit=expression(kPa), scale="")
        res@metadata$dataNamesOriginal$pressure <- "Stn Press (kPa)"
    }
    if ("speed" %in% names) {
        res@metadata$units$speed <- list(unit=expression(m/s), scale="")
        res@metadata$dataNamesOriginal$speed <- "-"
    }
    if ("temperature" %in% names) {
        res@metadata$units$temperature <- list(unit=expression(degree*C), scale="ITS-90")
        res@metadata$dataNamesOriginal$temperature <- "Temp (\u00B0C)"
    }
    if ("u" %in% names) res@metadata$units$u <- list(unit=expression(m/s), scale="")
    if ("v" %in% names) res@metadata$units$v <- list(unit=expression(m/s), scale="")
    if ("visibility" %in% names) {
        res@metadata$units$visibility <- list(unit=expression(km), scale="")
        res@metadata$dataNamesOriginal$visibility <- "Visibility (km)"
    }
    if ("weather" %in% names) {
        res@metadata$units$wind <- list(unit=expression(), scale="")
        res@metadata$dataNamesOriginal$weather <- "Weather"
    }

    if ("wind" %in% names) {
        res@metadata$units$wind <- list(unit=expression(km/h), scale="")
        res@metadata$dataNamesOriginal$wind <- "Wind Spd (km/h)"
    }
    if ("windChill" %in% names) {
        res@metadata$units$windChill <- list(unit=expression(degree*C), scale="ITS-90")
        res@metadata$dataNamesOriginal$windChill <- "Wind Chill"
    }
    ## move flags from data to metadata@flags
    for (flagType in c("dewPoint", "direction", "humidex", "humidity", "pressure", "temperature", "visibility", "wind",
                       "windChill")) {
        flagName <- paste(flagType, "Flag", sep="")
        if (flagName %in% names) {
            res@metadata$flags[[flagType]] <- res@data[[flagName]]
            res@data[[flagName]] <- NULL
        }
    }
    ## Remove various date things; we have time in our object so there is no need for these things,
    ## and just because the agency repeats things, that's no reason for us to do the same.
    ## (I would listen to argumetns to retain these, however.)
    res@data$Date.Time <- NULL # no need for this
    res@data$Year <- NULL # no need for this
    res@data$Month <- NULL # no need for this
    res@data$Day <- NULL # no need for this
    res@data$Time <- NULL # no need for this
    res
}


#' @title Plot met Data
#'
#' @description
#' Creates a multi-panel summary plot of data measured in a meteorological data
#' set.  cast. The panels are controlled by the \code{which} argument.
#'
#' @details
#' If more than one panel is drawn, then on exit from \code{plot.met}, the
#' value of \code{par} will be reset to the value it had before the function
#' call.  However, if only one panel is drawn, the adjustments to \code{par}
#' made within \code{plot.met} are left in place, so that further additions may
#' be made to the plot.
#'
#' @param x A \code{met} object, e.g. as read by \code{\link{read.met}}, or a
#' list containing items named \code{salinity} and \code{temperature}.
#'
#' @param which list of desired plot types.
#' \itemize{
#' \item \code{which=1} gives a time-series plot of temperature
#' \item \code{which=2} gives a time-series plot of pressure
#' \item \code{which=3} gives a time-series plot of the x (eastward) component of velocity
#' \item \code{which=4} gives a time-series plot of the y (northward) component of velocity
#' \item \code{which=5} gives a time-series plot of speed
#' \item \code{which=6} gives a time-series plot of direction (degrees clockwise from north;
#' note that the values returned by \code{met[["direction"]]} must be multiplied by 10
#' to get the direction plotted)
#' }
#'
#' @param tformat optional argument passed to \code{\link{oce.plot.ts}}, for
#' plot types that call that function.  (See \code{\link{strptime}} for the
#' format used.)
#'
#' @template mgpTemplate
#' @template marTemplate
#' @template debugTemplate
#'
#' @author Dan Kelley
#'
#' @examples
#' library(oce)
#' data(met)
#' plot(met, which=3:4)
#'
#' ## Wind speed and direction during Hurricane Juan
#' ## Compare with the final figure in a white paper by Chris Fogarty
#' ## (available at http://www.novaweather.net/Hurricane_Juan_files/McNabs_plot.pdf 
#' ## downloaded 2017-01-02).
#' library(oce)
#' data(met)
#' t0 <- as.POSIXct("2003-09-29 04:00:00", tz="UTC")
#' dt <- 12 * 3600 
#' juan <- subset(met, t0 - dt <= time & time <= t0 + dt)
#' par(mfrow=c(2,1))
#' plot(juan, which=5)
#' abline(v=t0)
#' plot(juan, which=6)
#' abline(v=t0)
#'
#' @family functions that plot \code{oce} data
#' @family things related to \code{met} data
setMethod(f="plot",
           signature=signature("met"),
           definition=function(x, which = 1:4, mgp, mar, tformat, debug=getOption("oceDebug"))
           {
               oceDebug(debug, "plot.met() {\n", unindent=1)
               if (missing(mgp))
                   mgp <- getOption("oceMgp")
               if (missing(mar))
                   mar <- c(mgp[1]+1, mgp[1]+1, mgp[1]+1, mgp[1]+1)
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
                       oce.plot.ts(x@data$time, x@data$u, ylab=resizableLabel("u [m/s]", "y"), tformat=tformat)
                   } else if (which[w] == 4 && any(!is.na(x@data$v))) {
                       oce.plot.ts(x@data$time, x@data$v, ylab=resizableLabel("v [m/s]", "y"), tformat=tformat)
                   } else if (which[w] == 5 && any(!is.na(x@data$v))) {
                       oce.plot.ts(x@data$time, x@data$speed, ylab=resizableLabel("Speed [m/s]", "y"), tformat=tformat)
                   } else if (which[w] == 6) {
                       oce.plot.ts(x@data$time, x@data$direction, ylab=resizableLabel("Direction [deg]", "y"), tformat=tformat)
                   }
               }
               oceDebug(debug, "} # plot.met()\n", unindent=1)
           })
