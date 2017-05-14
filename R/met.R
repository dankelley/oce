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

#' @title Extract Something From a met Object
#' @param x A met object, i.e. one inheriting from \code{\link{met-class}}.
#' @template sub_subTemplate
#' @family things related to \code{met} data
setMethod(f="[[",
          signature(x="met", i="ANY", j="ANY"),
          definition=function(x, i, j, ...) {
              callNextMethod()         # [[
          })

#' @title Replace Parts of a met Object
#' @param x An \code{met} object, i.e. inheriting from \code{\link{met-class}}
#' @template sub_subsetTemplate
#' @family things related to \code{met} data
setMethod(f="[[<-",
          signature(x="met", i="ANY", j="ANY"),
          definition=function(x, i, j, ..., value) {
              callNextMethod(x=x, i=i, j=j, ...=..., value=value) # [[<-
          })


#' @title Sample met Object
#'
#' @description
#' This is sample \code{met} object containing data for Halifax, Nova Scotia,
#' during September of 2003 (the period during which Hurricane Juan struck the
#' city).
#'
#' @details
#' The data file was downloaded with
#' \preformatted{
#'metFile <- download.met(id=6358, year=2003, month=9, destdir=".")
#'met <- read.met(metFile)
#'met <- oceSetData(met, "time", met[["time"]]+4*3600,
#'                  note="add 4h to local time to get UTC time")
#'}
#' Using \code{\link{download.met}} avoids having to navigate the
#' the awkward Environment Canada website, but it imposes the burden
#' of having to know the station number. See the documentation for
#' \code{\link{download.met}} for more details on station numbers.
#'
#' @name met
#' @docType data
#' @source Environment Canada website on February 1, 2017
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


#' @title Summarize a met Object
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


#' @title Subset a met Object
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
#' @family functions that subset \code{oce} objects
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



#' @title Coerce Data into met Object
#'
#' @description
#' Coerces a dataset into a met dataset. This fills in only a few of the typical
#' data fields, so the returned object is much
#' sparser than the output from \code{\link{read.met}}. Also, almost no
#' metadata fields are filled in, so the resultant object does not store
#' station location, units of the data, data-quality flags, etc. Anyone working
#' with data from Environment Canada [2] is advised to use \code{\link{read.met}}
#' instead of the present function.
#'
#' @param time Either a vector of observation times (or character strings that can be
#' coerced into times) or the output from \code{canadaHCD::hcd_hourly} (see [1]).
#' @param temperature vector of temperatures.
#' @param pressure vector of pressures.
#' @param u vector of eastward wind speed in m/s.
#' @param v vector of northward wind speed in m/s.
#' @param filename optional string indicating data source
#' @return An object of \code{\link{met-class}}.
#' @author Dan Kelley
#'
#' @references
#' 1. The \code{canadaHCD} package is in development by Gavin Simpson; see
#' \url{https://github.com/gavinsimpson/canadaHCD} for instructions on how
#' to download and install from GitHub.
#'
#' 2. Environment Canada website for Historical Climate Data
#' \url{http://climate.weather.gc.ca/index_e.html}
#'
#' @family things related to \code{met} data
as.met <- function(time, temperature, pressure, u, v, filename="(constructed from data)")
{
    if (missing(time)) stop("must provide time")
    if (inherits(time, "data.frame")) {
        ## Try to see whether this was created by a function in the canadaHCL package
        ## Copy the data, renaming some things that we know are named differently
        ## in canadaHSD::hcd_hourly().
        res <- new("met")
        ## Extract Station ID to the metadata
        names <- names(time)
        if ("Station" %in% names) {
            res@metadata$station <- time$Station[1]
            time$Station <- NULL
            names <- names(time)
        }
        ## Change the following names.
        ## DateTime Temp DewPointTemp RelHumidity WindDir WindSpeed Visibility Pressure Humidex WindChill Weather 
        if ("WindDir" %in% names)
            time$WindDir <- 10 * time$WindDir
        if ("WindSpeed" %in% names)
            time$WindSpeed <- (1000 / 3600) * time$WindSpeed
        names[names=="DateTime"] <- "time"
        names[names=="Temp"] <- "temperature"
        names[names=="DewPointTemp"] <- "dewPoint"
        names[names=="RelHumidity"] <- "humidity"
        names[names=="WindDir"] <- "direction"
        names[names=="WindSpeed"] <- "speed"
        names[names=="Visibility"] <- "visibility"
        names[names=="Pressure"] <- "pressure"
        names[names=="Humidex"] <- "humidex"
        names[names=="WindChill"] <- "windChill"
        names[names=="Weather"] <- "weather"
        names(time) <- names
        for (item in names) {
            res@data[[item]] <- time[[item]]
        }
        if (!("u" %in% names) && !("v" %in% names)) {
            rpd <- atan2(1, 1) / 45            # radian/degree
            theta <- (90 - time[["direction"]]) * rpd
            u <- -time[["speed"]] * sin(theta)
            v <- -time[["speed"]] * cos(theta)
            zero <- is.na(time[["direction"]]) | time[["speed"]] == 0
            u[zero] <- 0
            v[zero] <- 0
            res@data$u <- u
            res@data$v <- v
        }
    } else {
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
        res <- new("met", time=time, temperature=temperature, pressure=pressure, u=u, v=v, filename=filename)
    }
    res
}


#' Download and Cache a met File
#'
#' Data are downloaded from \url{http://climate.weather.gc.ca} and cached locally.
#'
#' @details
#' The data are downloaded with \code{\link[utils]{download.file}}
#' pointed to the Environment Canada website [1]
#' using queries that had to be devised by reverse-engineering, since the agency
#' does not provide documentation about how to construct queries. Caution: the
#' query format changes from time to time, so \code{download.met} may work one
#' day, and fail the next.
#'
#' The constructed query contains Station ID, as provided in the \code{id} argument.
#' Note that this seems to be a creation of Environment Canada, alone;
#' it is distinct from the more standard "Climate ID" and "WMO ID".
#' To make things more difficult, Environment Canada states that the
#' Station ID is subject to change over time. (Whether this applies to existing
#' data is unclear.)
#'
#' Given these difficulties with Station ID, users are advised to consult
#' the Environment Canada website [1] before downloading any data,
#' and to check it from time to time
#' during the course of a research project, to see if the Station ID has changed.
#' Another approach would be to use Gavin Simpson's
#' \code{canadaHCD} package [2] to look up Station IDs. This package maintains
#' a copy of the Environment Canada listing of stations, and its
#' \code{find_station} function provides an easy way to determine Station IDs.
#' After that, its \code{hcd_hourly} function (and related functions) make
#' it easy to read data. These data can then be converted to the 
#' \code{met} class with \code{\link{as.met}}, although doing so leaves
#' many important metadata blank.
#'
#' @param id A number giving the "Station ID" of the station of interest. If not
#' provided, \code{id} defaults to 6358, for Halifax International Airport. See
#' \dQuote{Details}.
#'
#' @param year A number giving the year of interest. Ignored unless \code{deltat}
#' is \code{"hour"}. If \code{year} is not given, it defaults to the present year.
#'
#' @param month A number giving the month of interest. Ignored unless \code{deltat}
#' is \code{"hour"}. If \code{month} is not given, it defaults to the present
#' month.
#'
#' @param deltat Optional character string indicating the time step of the
#' desired dataset. This may be \code{"hour"} or \code{"month"}.
#' If \code{deltat} is not given, it defaults to \code{"hour"}.
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
#' metFile <- download.met(6358, 2003, 9, destdir=".")
#' met <- read.met(metFile)
#'}
#'
#' @seealso The work is done with \code{\link[utils]{download.file}}.
#'
#' @template downloadWarningTemplate
#'
#' @references
#' 1. Environment Canada website for Historical Climate Data
#' \url{http://climate.weather.gc.ca/index_e.html}
#'
#' 2. Gavin Simpon's \code{canadaHCD} package on GitHub
#' \url{https://github.com/gavinsimpson/canadaHCD}
#'
#' @family functions that download files
#' @family things related to \code{met} data
download.met <- function(id, year, month, deltat, destdir="~/data/met", destfile,
                         debug=getOption("oceDebug"))
{
    if (missing(id))
        id <- 6358
    id <- as.integer(id)
    if (missing(deltat))
        deltat <- "hour"
    deltatChoices <- c("hour", "month") # FIXME: add "day"
    deltatIndex <- pmatch(deltat, deltatChoices)
    if (is.na(deltatIndex))
        stop("deltat=\"", deltat, "\" is not supported; try \"hour\" or \"month\"")
    deltat <- deltatChoices[deltatIndex]
    if (deltat == "hour") {
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
        if (missing(destfile))
            destfile <- sprintf("met_%d_hourly_%04d_%02d_%02d.csv", id, year, month, 1)
    } else if (deltat == "month") {
        ## Next line reverse engineered from monthly data at Resolute. I don't imagine we
        ## need Year and Month and Day.
        url <- paste("http://climate.weather.gc.ca/climate_data/bulk_data_e.html?stationID=",
                     id, "&format=csv&timeframe=3&submit=Download+Data", sep="")
                     ##id, "&Year=2000&Month=1&Day=14&format=csv&timeframe=3&submit=%20Download+Data", sep="")
        if (missing(destfile))
            destfile <- sprintf("met_%d_monthly.csv", id)
    } else {
        stop("deltat must be \"hour\" or \"month\"")
    }
    destination <- paste(destdir, destfile, sep="/")
    oceDebug(debug, "url:", url, "\n")
    if (1 == length(list.files(path=destdir, pattern=paste("^", destfile, "$", sep="")))) {
        oceDebug(debug, "Not downloading \"", destfile, "\" because it is already present in the \"", destdir, "\" directory\n", sep="")
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
            ## next block handles monthly data
            if (1 == length(i <- grep("^Date\\.Time$", res))) res[i] <- "DateTime"
            if (1 == length(i <- grep("^Year$", res))) res[i] <- "Year"
            if (1 == length(i <- grep("^Month$", res))) res[i] <- "Month"
            if (1 == length(i <- grep("^Mean\\.Max\\.Temp\\.\\.\\.C\\.$", res))) res[i] <- "temperatureMaximum"
            if (1 == length(i <- grep("^Extr\\.Max\\.Temp\\.\\.\\.C\\.$", res))) res[i] <- "temperatureExtraMaximum"
            if (1 == length(i <- grep("^Extr\\.Max\\.Temp\\.Flag$", res))) res[i] <- "temperatureExtraMaximumFlag"
            if (1 == length(i <- grep("^Mean\\.Min\\.Temp\\.\\.\\.C\\.$", res))) res[i] <- "temperatureMinimum"
            if (1 == length(i <- grep("^Extr\\.Min\\.Temp\\.\\.\\.C\\.$", res))) res[i] <- "temperatureExtraMinimum"
            if (1 == length(i <- grep("^Extr\\.Min\\.Temp\\.Flag$", res))) res[i] <- "temperatureExtraMinimumFlag"
            if (1 == length(i <- grep("^Mean\\.Temp\\.\\.\\.C\\.$", res))) res[i] <- "temperature"
            if (1 == length(i <- grep("^Mean\\.Max\\.Temp\\.Flag$", res))) res[i] <- "temperatureMaximumFlag"
            if (1 == length(i <- grep("^Mean\\.Min\\.Temp\\.Flag$", res))) res[i] <- "temperatureMinimumFlag"
            if (1 == length(i <- grep("^Mean\\.Temp\\.Flag$", res))) res[i] <- "temperatureFlag"
            if (1 == length(i <- grep("^Total\\.Rain\\.\\.mm\\.$", res))) res[i] <- "rain"
            if (1 == length(i <- grep("^Total\\.Rain\\.Flag$", res))) res[i] <- "rainFlag"
            if (1 == length(i <- grep("^Total\\.Snow\\.\\.cm\\.$", res))) res[i] <- "snow"
            if (1 == length(i <- grep("^Snow\\.Grnd\\.Last\\.Day\\.\\.cm\\.$", res))) res[i] <- "snowGroundLastDay"
            if (1 == length(i <- grep("^Snow\\.Grnd\\.Last\\.Day\\.Flag$", res))) res[i] <- "snowGroundLastDayFlag"
            if (1 == length(i <- grep("^Total\\.Snow\\.Flag$", res))) res[i] <- "snowFlag"
            if (1 == length(i <- grep("^Total\\.Precip\\.\\.mm", res))) res[i] <- "precipitation"
            if (1 == length(i <- grep("^Total\\.Precip\\.Flag", res))) res[i] <- "precipitationFlag"
            if (1 == length(i <- grep("^Dir\\.of\\.Max\\.Gust\\.\\.10\\.s\\.deg\\.$", res))) res[i] <- "directionMaximumGust"
            if (1 == length(i <- grep("^Dir\\.of\\.Max\\.Gust\\.Flag$", res))) res[i] <- "directionMaximumGustFlag"
            if (1 == length(i <- grep("^Spd\\.of\\.Max\\.Gust\\.\\.km\\.h\\.$", res))) res[i] <- "speedMaximumGust"
            if (1 == length(i <- grep("^Spd\\.of\\.Max\\.Gust\\.Flag$", res))) res[i] <- "speedMaximumGustFlag"
            ## next block handles hourly data
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
            ## some files have "10s" and others "10.s" (I think)
            if (1 == length(i <- grep("^Wind\\.Dir\\.\\.10\\.*s\\.deg\\.$", res))) res[i] <- "direction"
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



#' @title Read a met File
#'
#' @description
#' Reads a comma-separated value file in the format used by the Environment
#' Canada [1].  The agency does not publish a format for these
#' files, so this function was based on a study of a few sample files, and it
#' may fail for other files, if Environment Canada changes the format.
#'
#' @param file a connection or a character string giving the name of the file
#' to load.
#' @param type if \code{NULL}, then the first line is studied, in order to
#' determine the file type.  If \code{type="msc"}, then a file as formatted by
#' Environment Canada is assumed.
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
#'\dontrun{
#'library(oce)
#'# Recreate data(met) and plot u(t) and v(t)
#'metFile <- download.met(id=6358, year=2003, month=9, destdir=".")
#'met <- read.met(metFile)
#'met <- oceSetData(met, "time", met[["time"]]+4*3600,
#'                  note="add 4h to local time to get UTC time")
#'plot(met, which=3:4)
#'}
#'
#' @references
#' 1. Environment Canada website for Historical Climate Data
#' \url{http://climate.weather.gc.ca/index_e.html}
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
    text <- readLines(file, encoding="UTF-8", warn=FALSE)
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
    rawData <- read.csv(text=text, skip=skip, encoding="UTF-8", header=TRUE)
    names <- names(rawData)
    ## FIXME: handle daily data, if the column names differ
    if ("Day" %in% names && "Time" %in% names) {
        ## hourly data
        time <- strptime(paste(rawData$Year, rawData$Month, rawData$Day, rawData$Time),
                         "%Y %m %d %H:%M", tz=tz)
    } else {
        ## monthly data
        time <- ISOdatetime(rawData$Year, rawData$Month, 15, 0, 0, 0, tz="UTC")
    }
    ## deltat <- if ("Date.Time" %in% names) "monthly" else "hourly"
    ## print(data.frame(old=names, new=metNames2oceNames(names, "met")))
    names(rawData) <- metNames2oceNames(names, "met")
    names <- names(rawData)            # now names is in oce convention
    ## add a proper time column
    #browser()
    ## Quite a lot of things ae in weird units (km/h instead of m/s etc), so we will need to do some conversions.
    if ("wind" %in% names)
        rawData[["speed"]] <- rawData[["wind"]] * 1000 / 3600 # convert km/h to m/s
    if ("direction" %in% names)
        rawData[["direction"]] <- 10 * rawData[["direction"]] # convert 10s of degrees to degrees
    if ("directionMaximumGust" %in% names)
        rawData[["directionMaximumGust"]] <- 10 * rawData[["directionMaximumGust"]] # convert 10s of degrees to degrees

    ## Note (90 - ) to get from "clockwise from north" to "anticlockwise from east"
    rpd <- atan2(1, 1) / 45            # radian/degree
    ## message("names: ", paste(names, collapse=" "))
    if ("direction" %in% names && "wind" %in% names) {
        theta <- (90 - rawData[["direction"]]) * rpd
        ## Note the (-) to get from "wind from" to "wind speed towards"
        rawData[["u"]] <- -rawData[["wind"]] * 1000 / 3600 * sin(theta)
        rawData[["v"]] <- -rawData[["wind"]] * 1000 / 3600 * cos(theta)
        zero <- is.na(rawData[["direction"]]) & rawData[["wind"]] == 0
        rawData[["u"]][zero] <- 0
        rawData[["v"]][zero] <- 0
    }
    rawData$time <- time
    res@data <- rawData
    pl <- paste("read.met(\"", filename, "\", type=", if (is.null(type)) "NULL" else type, ", tz=\"", tz, "\")", sep="")
    res@processingLog <- processingLogAppend(res@processingLog, pl)
    names <- names(res@data)
    res@metadata$dataNamesOriginal <- list()
    res@metadata$flags <- list()
    if ("dataQuality" %in% names) {
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
    if ("directionMaximumGust" %in% names) {
        res@metadata$units$directionMaximumGust <- list(unit=expression(degree), scale="")
        res@metadata$dataNamesOriginal$directionMaximumGust <- "-" # we use deg, they use 10deg, so no original name
    }
    if ("humidex" %in% names) {
        res@metadata$units$humidex <- list(unit=expression(degree*C), scale="")
        res@metadata$dataNamesOriginal$humidex <- "Hmdx"
    }
    if ("humidity" %in% names) {
        res@metadata$units$humidity <- list(unit=expression(degree*C), scale="ITS-90")
        res@metadata$dataNamesOriginal$humidity <- "Rel Hum (%)"
    }
    if ("precipitation" %in% names) {
        res@metadata$units$precipitation <- list(unit=expression(mm), scale="")
        res@metadata$dataNamesOriginal$precipitation <- "Total Precip (mm)"
    }
    if ("pressure" %in% names) {
        res@metadata$units$pressure <- list(unit=expression(kPa), scale="")
        res@metadata$dataNamesOriginal$pressure <- "Stn Press (kPa)"
    }
    if ("rain" %in% names) {
        res@metadata$units$rain <- list(unit=expression(mm), scale="")
        res@metadata$dataNamesOriginal$rain <- "Total Rain (mm)"
    }
    if ("snow" %in% names) {
        res@metadata$units$snow <- list(unit=expression(cm), scale="")
        res@metadata$dataNamesOriginal$snow<- "Total Snow (cm)"
    }
    if ("snowGroundLastDay" %in% names) {
        res@metadata$units$snowGroundLastDay <- list(unit=expression(cm), scale="")
        res@metadata$dataNamesOriginal$snowGroundLastDay <- "Snow Grnd Last Day (cm)"
    }
    if ("speed" %in% names) {
        res@metadata$units$speed <- list(unit=expression(m/s), scale="")
        res@metadata$dataNamesOriginal$speed <- "-"
    }
    if ("speedMaximumGust" %in% names) {
        res@metadata$units$speedMaximumGust <- list(unit=expression(km/h), scale="")
        res@metadata$dataNamesOriginal$speedMaximumGust <- "Spd of Max Gust (km/h)"
    }
    if ("temperature" %in% names) {
        res@metadata$units$temperature <- list(unit=expression(degree*C), scale="ITS-90")
        res@metadata$dataNamesOriginal$temperature <- "Temp (\u00B0C)"
    }
    if ("temperatureExtraMaximum" %in% names) {
        res@metadata$units$temperatureExtraMaximum <- list(unit=expression(degree*C), scale="ITS-90")
        res@metadata$dataNamesOriginal$temperatureExtraMaximum <- "Extr Max Temp (\u00B0C)"
    }
    if ("temperatureExtraMinimum" %in% names) {
        res@metadata$units$temperatureExtraMinimum <- list(unit=expression(degree*C), scale="ITS-90")
        res@metadata$dataNamesOriginal$temperatureExtraMinimum <- "Extr Min Temp (\u00B0C)"
    }
     if ("temperatureMaximum" %in% names) {
        res@metadata$units$temperatureMaximum <- list(unit=expression(degree*C), scale="ITS-90")
        res@metadata$dataNamesOriginal$temperatureMaximum <- "Mean Max Temp (\u00B0C)"
    }
    if ("temperatureMinimum" %in% names) {
        res@metadata$units$temperatureMinimum <- list(unit=expression(degree*C), scale="ITS-90")
        res@metadata$dataNamesOriginal$temperatureMinimum <- "Mean Min Temp (\u00B0C)"
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
    for (flagType in c("dewPoint",
                       "direction", "directionMaximumGust",
                       "humidex", "humidity", "pressure",
                       "temperature", "temperatureMinimum", "temperatureMaximum",
                       "temperatureExtra", "temperatureExtraMinimum", "temperatureExtraMaximum",
                       "precipitation", "rain", "snow", "snowGroundLastDay",
                       "speed", "speedMaximumGust",
                       "visibility", "wind",
                       "windChill")) {
        flagName <- paste(flagType, "Flag", sep="")
        if (flagName %in% names) {
            res@metadata$flags[[flagType]] <- res@data[[flagName]]
            res@data[[flagName]] <- NULL
        }
    }
    ## Discard any flags that are all NA (which, we presume, means the data are OK)
    for (flagName in names(res@metadata$flags)) {
        if (all(is.na(res@metadata$flags[[flagName]])))
            res@metadata$flags[[flagName]] <- NULL
    }
    ## Remove various date things; we have time in our object so there is no need for these things,
    ## and just because the agency repeats things, that's no reason for us to do the same.
    ## (I would listen to argumetns to retain these, however.)
    res@data$Date.Time <- NULL # no need for this
    res@data$DateTime <- NULL # no need for this
    res@data$Year <- NULL # no need for this
    res@data$Month <- NULL # no need for this
    res@data$Day <- NULL # no need for this
    res@data$Time <- NULL # no need for this
    ## Remove non-ascii characters in original data names, since they caused a
    ## build-check NOTE on CRAN.  (Actually, those characters are in the units
    ## that are embedded within the names, e.g. the degree character.)
    for (dno in seq_along(res@metadata$dataNamesOriginal)) {
        o <- res@metadata$dataNamesOriginal[[dno]]
        Encoding(o) <- "latin1"
        o <- iconv(o, "latin1", "ASCII", sub="")
        res@metadata$dataNamesOriginal[[dno]] <- o
    }
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
               dnames <- names(x@data)
               for (w in 1:nw) {
                   oceDebug(debug, "which=", w, "\n")
                   if (which[w] == 1 && any(!is.na(x@data$temperature))) {
                       oce.plot.ts(x@data$time, x@data$temperature, ylab=resizableLabel("T", "y"), tformat=tformat)
                   } else if (which[w] == 2 && "pressure" %in% dnames && any(!is.na(x@data$pressure))) {
                       oce.plot.ts(x@data$time, x@data$pressure, ylab="Pressure [kPa]", tformat=tformat)
                   } else if (which[w] == 3 && "u" %in% dnames && any(!is.na(x@data$u))) {
                       oce.plot.ts(x@data$time, x@data$u, ylab=resizableLabel("u [m/s]", "y"), tformat=tformat)
                   } else if (which[w] == 4 && "v" %in% dnames && any(!is.na(x@data$v))) {
                       oce.plot.ts(x@data$time, x@data$v, ylab=resizableLabel("v [m/s]", "y"), tformat=tformat)
                   } else if (which[w] == 5 && "speed" %in% dnames && any(!is.na(x@data$speed))) {
                       oce.plot.ts(x@data$time, x@data$speed, ylab=resizableLabel("Speed [m/s]", "y"), tformat=tformat)
                   } else if (which[w] == 6) {
                       oce.plot.ts(x@data$time, x@data$direction, ylab=resizableLabel("Direction [deg]", "y"), tformat=tformat)
                   }
               }
               oceDebug(debug, "} # plot.met()\n", unindent=1)
           })
