# vim:textwidth=80:expandtab:shiftwidth=4:softtabstop=4

#' Class to Store Meteorological Data
#'
#' This class stores meteorological data. For objects created with
#' [read.met()], the `data` slot will contain all the columns
#' within the original file (with some guesses as to units) in addition to
#' several calculated quantities such as `u` and `v`, which are
#' velocities in m/s (not the km/h stored in typical data files), and which
#' obey the oceanographic convention that `u>0` is a wind towards the
#' east.
#'
#' @templateVar class met
#'
#' @templateVar dataExample {}
#'
#' @templateVar metadataExample {}
#'
#' @template slot_summary
#'
#' @template slot_put
#'
#' @template slot_get
#'
#' @author Dan Kelley
#'
#' @family classes provided by oce
#' @family things related to met data
setClass("met", contains="oce")


#' Extract Something From a met Object
#'
#' @param x a [met-class] object.
#'
#' @section Details of the Specialized Method:
#'
#' * If `i` is `"?"`, then the return value is a list
#' containing four items, each of which is a character vector
#' holding the names of things that can be accessed with `[[`.
#' The `data` and `metadata` items hold the names of
#' entries in the object's data and metadata
#' slots, respectively. The `dataDerived`
#' and `metadataDerived` items are each NULL, because
#' no derived values are defined by [met-class] objects.
#'
#' @template sub_subTemplate
#'
#' @author Dan Kelley
#'
#' @family things related to met data
setMethod(f="[[",
          signature(x="met", i="ANY", j="ANY"),
          definition=function(x, i, j, ...) {
              if (i == "?")
                  return(list(metadata=sort(names(x@metadata)),
                          metadataDerived=NULL,
                          data=sort(names(x@data)),
                          dataDerived=NULL))
              callNextMethod()         # [[
          })

#' Replace Parts of a met Object
#'
#' @param x a [met-class] object.
#'
#' @template sub_subsetTemplate
#'
#' @family things related to met data
setMethod(f="[[<-",
          signature(x="met", i="ANY", j="ANY"),
          definition=function(x, i, j, ..., value) {
              callNextMethod(x=x, i=i, j=j, ...=..., value=value) # [[<-
          })


#' Sample met Object
#'
#' This is sample [met-class] object containing data for Halifax, Nova Scotia,
#' during September of 2003 (the period during which Hurricane Juan struck the
#' city).
#'
#' The data file was downloaded
#'```R
#'metFile <- download.met(id=6358, year=2003, month=9, destdir=".", type="xml")
#'```
#' Note that using [download.met()] avoids having to navigate the
#' the awkward Environment Canada website, but it imposes the burden
#' of having to know the station ID number.  With the data in-hand,
#' the object was then created (and its timezone adjusted) with
#'```R
#'met <- read.met(metFile)
#'met <- oceSetData(met, "time", met[["time"]]+4*3600,
#'                  note="add 4h to local time to get UTC time")
#'```
#'
#' *Historical note.* The `data(met)` object was changed on October 19,
#' 2019, based on the data provided by Environment
#' Canada at that time. The previous version of `data(met)`,
#' created in 2017, had been based on a data format that
#' Environment Canada no longer provided in 2019.  See the
#' notes on the `type` argument of [read.met()] for more on this
#' shift in the Environment Canada data format.
#'
#' @name met
#'
#' @docType data
#'
#' @source Environment Canada website on October 19, 2019.
#'
#' @family datasets provided with oce
#' @family things related to met data
NULL

setMethod(f="initialize",
          signature="met",
          definition=function(.Object, time, temperature, pressure, u, v, filename="", ...) {
              .Object <- callNextMethod(.Object, ...)
              if (!missing(time)) .Object@data$time <- time
              if (!missing(temperature)) .Object@data$temperature <-temperature
              if (!missing(pressure)) .Object@data$pressure <- pressure
              if (!missing(u)) .Object@data$u <- u
              if (!missing(v)) .Object@data$v <- v
              .Object@processingLog$time <- presentTime()
              .Object@processingLog$value <- "create 'met' object"
              return(.Object)
          })


#' Summarize a met Object
#'
#' Pertinent summary information is presented, including the sampling location,
#' data ranges, etc.
#'
#' @param object a [met-class] object.
#'
#' @param \dots further arguments passed to or from other methods.
#'
#' @author Dan Kelley
#'
#' @family things related to met data
setMethod(f="summary",
          signature="met",
          definition=function(object, ...) {
              cat("Met Summary\n-----------\n\n")
              showMetadataItem(object, "filename",          "Source                     ", quote=TRUE)
              showMetadataItem(object, "name",              "Name                       ")
              showMetadataItem(object, "province",          "Province                   ")
              showMetadataItem(object, "stationOperator",   "Station Operator           ")
              showMetadataItem(object, "latitude",          "Latitude                   ")
              showMetadataItem(object, "longitude",         "Longitude                  ")
              showMetadataItem(object, "elevation",         "Elevation                  ")
              showMetadataItem(object, "climateIdentifier", "Climate Identifer          ")
              showMetadataItem(object, "WMOIdentifier",     "World Met Office Identifer ")
              showMetadataItem(object, "TCIdentifier",      "Transport Canada Identifer ")
              showMetadataItem(object, "note",              "Note                       ")
              invisible(callNextMethod()) # summary
          })


#' Subset a met Object
#'
#' This function is somewhat analogous to [subset.data.frame()].
#'
#' @param x a [met-class] object.
#'
#' @param subset An expression indicating how to subset `x`.
#'
#' @param \dots ignored.
#'
#' @return A [met-class] object.
#'
#' @author Dan Kelley
#'
#' @examples
#' library(oce)
#' data(met)
#' # Few days surrounding Hurricane Juan
#' plot(subset(met, time > as.POSIXct("2003-09-27", tz="UTC")))
#'
#' @family things related to met data
#' @family functions that subset oce objects
setMethod(f="subset",
          signature="met",
          definition=function(x, subset, ...) {
              res <- new("met") # start afresh in case x@data is a data.frame
              res@metadata <- x@metadata
              res@processingLog <- x@processingLog
              for (i in seq_along(x@data)) {
                  r <- eval(substitute(expr=subset, env=environment()), x@data, parent.frame(2))
                  r <- r & !is.na(r)
                  res@data[[i]] <- x@data[[i]][r]
              }
              names(res@data) <- names(x@data)
              subsetString <- paste(deparse(substitute(expr=subset, env=environment())), collapse=" ")
              res@processingLog <- processingLogAppend(res@processingLog, paste("subset.met(x, subset=", subsetString, ")", sep=""))
              res
          })



#' Coerce Data into met Object
#'
#' Coerces a dataset into a met dataset. This fills in only a few of the typical
#' data fields, so the returned object is much
#' sparser than the output from [read.met()]. Also, almost no
#' metadata fields are filled in, so the resultant object does not store
#' station location, units of the data, data-quality flags, etc. Anyone working
#' with data from Environment Canada (reference 2) is advised to use [read.met()]
#' instead of the present function.
#'
#' @param time Either a vector of observation times (or character strings that can be
#' coerced into times) or the output from `canadaHCD::hcd_hourly` (see reference 1).
#'
#' @param temperature vector of temperatures.
#'
#' @param pressure vector of pressures.
#'
#' @param u vector of eastward wind speed in m/s.
#'
#' @param v vector of northward wind speed in m/s.
#'
#' @param filename optional string indicating data source
#'
#' @return A [met-class] object.
#'
#' @author Dan Kelley
#'
#' @references
#' 1. The `canadaHCD` package is in development by Gavin Simpson; see
#' \url{https://github.com/gavinsimpson/canadaHCD} for instructions on how
#' to download and install from GitHub.
#'
#' 2. Environment Canada website for Historical Climate Data
#' `https://climate.weather.gc.ca/index_e.html`
#'
#' @family things related to met data
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
    res@processingLog <- processingLogAppend(res@processingLog, paste(deparse(match.call()), sep="", collapse=""))
    res
}


#' Download and Cache a met File
#'
#' Data are downloaded from the Environment Canada's historical data
#' website and cached locally.
#'
#' The data are downloaded
#' using [utils::download.file()] based on a query devised by reverse-engineering
#' queries create by the Environment Canada interface
#' `https://climate.weather.gc.ca/historical_data/search_historic_data_e.html`.
#' The constructed query contains Station ID, as provided in the `id` argument.
#' Note that this seems to be a creation of Environment Canada, alone;
#' it is distinct from the more standard "Climate ID" and "WMO ID".
#' To make things more difficult, Environment Canada states that the
#' Station ID is subject to change over time. (Whether this applies to existing
#' data is unclear.)
#'
#' Given these difficulties with Station ID, users are advised to consult
#' the Environment Canada website (reference 1) before downloading any data,
#' and to check it from time to time
#' during the course of a research project, to see if the Station ID has changed.
#' Another approach would be to use Gavin Simpson's
#' `canadaHCD` package (reference 2) to look up Station IDs. This package maintains
#' a copy of the Environment Canada listing of stations, and its
#' `find_station` function provides an easy way to determine Station IDs.
#' After that, its `hcd_hourly` function (and related functions) make
#' it easy to read data. These data can then be converted to the
#' `met` class with [as.met()], although doing so leaves
#' many important metadata blank.
#'
#' @param id A number giving the "Station ID" of the station of interest. If not
#' provided, `id` defaults to 6358, for Halifax International Airport. See
#' \dQuote{Details}.
#'
#' @param year A number giving the year of interest. Ignored unless `deltat`
#' is `"hour"`. If `year` is not given, it defaults to the present year.
#'
#' @param month A number giving the month of interest. Ignored unless `deltat`
#' is `"hour"`. If `month` is not given, it defaults to the present
#' month.
#'
#' @param deltat Optional character string indicating the time step of the
#' desired dataset. This may be `"hour"` or `"month"`.
#' If `deltat` is not given, it defaults to `"hour"`.
#'
#' @param type String indicating which type of file to download, either
#' `"xml"` (the default) for an XML file or `"csv"` for a CSV file.
#'
#' @template downloadDestTemplate
#'
#' @param force Logical value indicating whether to force a download, even
#' if the file already exists locally.
#'
#' @param quiet Logical value passed to [download.file()]; a `TRUE` value
#' silences output.
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
#' @seealso The work is done with [utils::download.file()].
#'
#' @references
#' 1. Environment Canada website for Historical Climate Data
#' `https://climate.weather.gc.ca/index_e.html`
#'
#' 2. Gavin Simpson's `canadaHCD` package on GitHub
#' \url{https://github.com/gavinsimpson/canadaHCD}
#'
#' @family functions that download files
#' @family things related to met data
download.met <- function(id, year, month, deltat, type="xml",
                         destdir=".", destfile, force=FALSE, quiet=FALSE,
                         debug=getOption("oceDebug"))
{
    if (missing(id))
        id <- 6358
    id <- as.integer(id)
    if (missing(deltat))
        deltat <- "hour"
    deltatChoices <- c("hour", "month") # FIXME: add "day"
    deltatIndex <- pmatch(deltat, deltatChoices)
    if (!(type %in% c("csv", "xml")))
        stop("type '", type, "' not permitted; try 'csv' or 'xml'")
    if (is.na(deltatIndex))
        stop("deltat=\"", deltat, "\" is not supported; try \"hour\" or \"month\"")
    deltat <- deltatChoices[deltatIndex]
    if (deltat == "hour") {
        today <- as.POSIXlt(presentTime())
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
        url <- paste("http://climate.weather.gc.ca/climate_data/bulk_data_e.html?",
                     "format=", type,
                     "&stationID=", id,
                     "&Year=", year,
                     "&Month=", month,
                     "&timeframe=1&submit=Download+Data", sep="")
        if (missing(destfile))
            destfile <- sprintf("met_%d_hourly_%04d_%02d_%02d.%s", id, year, month, 1, type)
    } else if (deltat == "month") {
        ## Next line reverse engineered from monthly data at Resolute. I don't imagine we
        ## need Year and Month and Day.
        url <- paste("http://climate.weather.gc.ca/climate_data/bulk_data_e.html?stationID=",
                     id, "&format=", type, "&timeframe=3&submit=Download+Data", sep="")
                     ##id, "&Year=2000&Month=1&Day=14&format=csv&timeframe=3&submit=%20Download+Data", sep="")
        if (missing(destfile))
            destfile <- sprintf("met_%d_monthly.%s", id, type)
    } else {
        stop("deltat must be \"hour\" or \"month\"")
    }
    destination <- paste(destdir, destfile, sep="/")
    oceDebug(debug, "url:", url, "\n")
    if (!force && 1 == length(list.files(path=destdir, pattern=paste("^", destfile, "$", sep="")))) {
        oceDebug(debug, "Not downloading \"", destfile, "\" because it is already present in the \"", destdir, "\" directory\n", sep="")
    } else {
        ##?owarn <- options()$warn # this, and the capture.output, quieten the processing
        ##?options(warn=-1)
        capture.output({download.file(url, destination, quiet=TRUE)})
        ##?options(warn=owarn)
        oceDebug(debug, "Downloaded file stored as '", destination, "'\n", sep="")
    }
    ## NOTE: if the format=csv part of the URL is changed to format=txt we get
    ## the metadata file. But dealing with that is a bit of coding, both at the
    ## download stage and at the read.met() stage, and I don't think this is
    ## worthwhile.  The better scheme may be for users to move to the XML
    ## format, instead of sticking with the CSV format.
    destination
}

#' Convert met Data Name to Oce Name
#'
#' Interoperability between oce functions requires that standardized data names
#' be used, e.g. `"temperature"` for in-situ temperature. Very few
#' data-file headers name the temperature column in exactly that way, however,
#' and this function is provided to try to guess the names. The task is complicated
#' by the fact that Environment Canada seems to change the names of the columns,
#' e.g. sometimes a symbol is used for the degree sign, other times not.
#'
#' Several quantities in the returned object differ from their values in the source
#' file. For example, speed is converted from km/h to m/s, and angles are converted
#' from tens of degrees to degrees. Also, some items are created from scratch, e.g.
#' `u` and `v`, the eastward and northward velocity, are computed from speed
#' and direction. (Note that e.g. u is positive if the wind blows to the east; the
#' data are thus in the normal Physics convention.)
#'
#' @param names a vector of character strings with original names
#'
#' @param scheme an optional indication of the scheme that is employed. This may
#' be `"ODF"`, in which case [ODFNames2oceNames()] is used,
#' or `"met"`, in which case some tentative code for met files is used.
#'
#' @return
#' Vector of strings for the decoded names. If an unknown scheme is provided,
#' this will just be `names`.
metNames2oceNames <- function(names, scheme)
{
    ##schemeGiven <- !missing(scheme)
    res <- names
    if (!missing(scheme)) {
        if (scheme == "ODF") {
            res <- ODFNames2oceNames(ODFnames=names)
        } else if (scheme == "met") {
            ## next block handles monthly data
            res[grep("^Date.Time$", res)] <- "DateTime"
            res[grep("^Year$", res)] <- "Year"
            res[grep("^Month$", res)] <- "Month"
            res[grep("^Mean.Max.Temp...C.$", res)] <- "temperatureMaximum"
            res[grep("^Extr.Max.Temp...C.$", res)] <- "temperatureExtraMaximum"
            res[grep("^Extr.Max.Temp.Flag$", res)] <- "temperatureExtraMaximumFlag"
            res[grep("^Mean.Min.Temp...C.$", res)] <- "temperatureMinimum"
            res[grep("^Extr.Min.Temp...C.$", res)] <- "temperatureExtraMinimum"
            res[grep("^Extr.Min.Temp.Flag$", res)] <- "temperatureExtraMinimumFlag"
            res[grep("^Mean.Temp...C.$", res)] <- "temperature"
            res[grep("^Mean.Max.Temp.Flag$", res)] <- "temperatureMaximumFlag"
            res[grep("^Mean.Min.Temp.Flag$", res)] <- "temperatureMinimumFlag"
            res[grep("^Mean.Temp.Flag$", res)] <- "temperatureFlag"
            res[grep("^Total.Rain..mm.$", res)] <- "rain"
            res[grep("^Total.Rain.Flag$", res)] <- "rainFlag"
            res[grep("^Total.Snow..cm.$", res)] <- "snow"
            res[grep("^Snow.Grnd.Last.Day..cm.$", res)] <- "snowGroundLastDay"
            res[grep("^Snow.Grnd.Last.Day.Flag$", res)] <- "snowGroundLastDayFlag"
            res[grep("^Total.Snow.Flag$", res)] <- "snowFlag"
            res[grep("^Total.Precip..mm", res)] <- "precipitation"
            res[grep("^Total.Precip.Flag", res)] <- "precipitationFlag"
            res[grep("^Dir.of.Max.Gust..10.s.deg.$", res)] <- "directionMaximumGust"
            res[grep("^Dir.of.Max.Gust.Flag$", res)] <- "directionMaximumGustFlag"
            res[grep("^Spd.of.Max.Gust..km.h.$", res)] <- "speedMaximumGust"
            res[grep("^Spd.of.Max.Gust.Flag$", res)] <- "speedMaximumGustFlag"
            ## next block handles hourly data
            res[grep("^Data.Quality$", res)] <- "dataQuality"
            res[grep("^Dew.Point.Temp.*C.$", res)] <- "dewPoint"
            res[grep("^Dew.Point.Temp.Flag$", res)] <- "dewPointFlag"
            res[grep("^Hmdx$", res)] <- "humidex"
            res[grep("^Hmdx.Flag$", res)] <- "humidexFlag"
            res[grep("^Rel.Hum....$", res)] <- "humidity"
            res[grep("^Rel.Hum.Flag$", res)] <- "humidityFlag"
            res[grep("^Stn.*Press.*kPa.*$", res)] <- "pressure"
            res[grep("^Stn.Press.Flag$", res)] <- "pressureFlag"
            res[grep("^Temp.*C.*$", res)] <- "temperature"
            res[grep("^Temp.*Flag$", res)] <- "temperatureFlag"
            res[grep("^Visibility.*km.*$", res)] <- "visibility"
            res[grep("^Visibility.*Flag$", res)] <- "visibilityFlag"
            res[grep("^Wind.*Spd.*km.*$", res)] <- "wind"
            res[grep("^Wind.*Spd.*Flag$", res)] <- "windFlag"
            ## some files have "10s" and others "10.s" (I think)
            res[grep("^Wind.Dir..10.*s.deg.$", res)] <- "direction"
            res[grep("^Wind.Dir.Flag$", res)] <- "directionFlag"
            res[grep("^Wind.Chill$", res)] <- "windChill"
            res[grep("^Wind.Chill.Flag$", res)] <- "windChillFlag"
            res[grep("^Weather$", res)] <- "weather"
        } else {
            warning("unknown scheme ", scheme)
        }
    } else {
        ## temperature
        col <- grep("temp", names, ignore.case=TRUE, useBytes=TRUE)
        if (1 == length(col))
            res[col] <- "temperature"
    }
    ## cat("in metNames2oceNames:\n")
    ## print(data.frame(names=names,res=res))
    res
}



#' Read a met File
#'
#' Reads some meteorological file formats used by the Environment
#' Canada (see reference 1).  Since the agency does not publish the
#' data formats, this function has to be adjusted every few years,
#' when a user finds that the format has changed.
#'
#' @param file a character string naming a file that holds met data.
#'
#' @param type if `NULL`, which is the default, then an attempt is
#' made to infer the type from the file contents. If this fails, it
#' will be necessary for the user to provide a value for the \code{type}
#' argument.  The permitted choices are: (a) `"csv"` or `"csv1"` for an
#' old CSV format no longer provided as of October 2019,
#' (b) `"csv2"` for a CSV format noticed on the Environment Canada
#' website in October 2019 (but note that the paired metadata file
#' is ignored), (c) `"csv3"` for a CSV format noticed on the
#' Environment Canada website in January 2020,
#' and (d) `"xml2"` for an XML format that was noticed
#' on the Environment Canada website in October 2019.
#'
#' @param skip number of lines of header that occur before the actual
#' data.  This is ignored unless `type` is `"csv"` or `"csv1"`, in which case
#' a non-`NULL` value of `skip` is taken as the number of lines preceding
#' the columnar data ... and this is only needed if [read.met()] cannot
#' find a line starting with `"Date/Time"` (for csv2 format)
#' or `"Date/Time (LTC)"` (for csv3 format).
#'
#' @param tz timezone assumed for time data.  This defaults to
#' `getOption("oceTz")`, which is very likely to be wrong.  In
#' a scientific context, where UTC is typically used for oceanographic
#' measurement, it makes sense to set `tz="UTC"`.  Note that these
#' data files do not contain timezone information, instead giving
#' data in Local Standard Time (LST).  Since LST differs from city
#' to city, users must make corrections to the time, as
#' shown in the \dQuote{Examples}, which use data for
#' Halifax Nova Scotia, where LST is UTC-4.
#'
#' @param debug a flag that turns on debugging.  Set to 1 to get a moderate
#' amount of debugging information, or to 2 to get more.
#'
#' @return A [met-class] object.
#'
#' @author Dan Kelley
#'
#' @examples
#' # Example 1: "csv1" Environment Canada format (found to be obsolete as of Oct 2019)
#' csv1 <- read.met(system.file("extdata", "test_met_vsn1.csv", package="oce"))
#' csv1 <- oceSetData(csv1, "time", csv1[["time"]]+4*3600,
#'     note="add 4h to local time to get UTC time")
#'
#' # Example 2: "csv2" Environment Canada format (found to be obsolete as of Jan 2022)
#' csv2 <- read.met(system.file("extdata", "test_met_vsn2.csv", package="oce"))
#' csv2 <- oceSetData(csv2, "time", csv2[["time"]]+4*3600,
#'     note="add 4h to local time to get UTC time")
#'
#' # Example 3: "csv3" Environment Canada format. Note timezone correction
#' csv3 <- read.met(system.file("extdata", "test_met_vsn3.csv", package="oce"))
#' csv3 <- oceSetData(csv3, "time", csv3[["time"]]+4*3600,
#'     note="add 4h to local time to get UTC time")
#'
#' # Example 4: "xml2" format. (Uncertain timezone, so not corrected.)
#' if (requireNamespace("XML", quietly=TRUE))
#'     xml2 <- read.met(system.file("extdata", "test_met_xml2.xml", package="oce"))
#'
#' # Example 5: download and plot
#' \dontrun{
#' library(oce)
#' # Recreate data(met) and plot u(t) and v(t)
#' metFile <- download.met(id=6358, year=2003, month=9, destdir=".")
#' met <- read.met(metFile)
#' met <- oceSetData(met, "time", met[["time"]]+4*3600,
#'     note="add 4h to local time to get UTC time")
#' plot(met)
#' }
#'
#' @references
#' 1. Environment Canada website for Historical Climate Data
#' `https://climate.weather.gc.ca/index_e.html`
#'
#' @family things related to met data
read.met <- function(file, type=NULL, skip=NULL, tz=getOption("oceTz"), debug=getOption("oceDebug"))
{
    if (missing(file))
        stop("must supply 'file'")
    oceDebug(debug, "read.met(file=\"", file, "\", ...) {\n", sep="", unindent=1, style="bold")
    ## We avoid some file() problems by insisting this is a string
    if (!is.character(file))
        stop("'file' must be a character string")
    someLines <- readLines(file, 30, encoding="UTF-8", warn=FALSE)
    if (length(someLines) == 0L)
        stop("no data in file")
    if (!is.null(type) && !(type %in% c("csv", "csv1", "csv2", "xml2")))
        stop("type='", type, "' not allowed; try 'csv', 'csv1', 'csv2' or 'xml2'")
    if (is.null(type)) {
        if (grepl("xml$", file) || 1 == grepl('xml version', someLines[1])) {
            ## an xml file
            if (grepl(".weather.gc.ca", someLines[1]))
                type <- "xml2"
        } else {
            ## must be a csv
            if (1 == length(grep('^.?"WMO Identifier",', someLines))) {
                type <- "csv1"
            } else if (grepl('^.?"Longitude.[^"]*","Latitude[^"]*","Station Name","Climate ID"', someLines[1])) {
                type <- if (grepl("Time \\(LST\\)", someLines[1])) "csv3" else "csv2"
            } else {
                stop("cannot determine type from file contents; the first line is '", someLines[1], "'")
            }
            oceDebug(debug, "file contents suggest type=\"", type, "\"\n", sep="")
        }
    }
    if (type == "csv" || type == "csv1")
        res <- read.met.csv1(file, skip=skip, tz=tz, debug=debug-1)
    else if (type == "csv2" || type == "csv3")
        res <- read.met.csv2(file, skip=skip, tz=tz, debug=debug-1)
    else if (type == "xml2")
        res <- read.met.xml2(file, skip=skip, tz=tz, debug=debug-1)
    else
        stop("cannot handle file type '", type, "'")
    oceDebug(debug, "} # read.met()\n", unindent=1, style="bold")
    res
}

read.met.csv1 <- function(file, skip=NULL, tz=getOption("oceTz"), debug=getOption("oceDebug"))
{
    if (missing(file))
        stop("must supply 'file'")
    if (!is.character(file))
        stop("'file' must be a character string")
    oceDebug(debug, "read.met.csv2(\"", file, "\") {\n", sep="", unindent=1, style="bold")
    res <- new("met", time=1)
    text <- readLines(file, encoding="UTF-8", warn=FALSE)
    oceDebug(debug, "file has", length(text), "lines\n")
    ##print(header[1:19])
    textItem <- function(text, name, numeric=TRUE) {
        i <- grep(name, text)
        if (length(i)) {
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
    if (is.null(skip)) {
        skip <- grep("^\"Date/Time\"", text)[1] - 1
    }
    res@metadata$latitude <- latitude
    res@metadata$longitude <- longitude
    res@metadata$elevation <- elevation
    res@metadata$station <- station
    res@metadata$climateIdentifier <- climateIdentifier
    res@metadata$WMOIdentifier <- WMOIdentifier
    res@metadata$TCIdentifier <- TCIdentifier
    res@metadata$filename <- file
    ## Use stringsAsFactors=TRUE to compact weather conditions somewhat ... note that flags are converted to character type
    ## later on, when they are moved from 'data' into 'metadata$flags'.
    owarn <- options()$warn
    options(warn=-1)
    rawData <- read.csv(text=text, skip=skip, encoding="UTF-8", header=TRUE, stringsAsFactors=TRUE)
    options(warn=owarn)
    names <- names(rawData)
    ## FIXME: handle daily data, if the column names differ
    time <- if ("Day" %in% names && "Time" %in% names) {
        ## hourly data
        as.POSIXct(strptime(paste(rawData$Year, rawData$Month, rawData$Day, rawData$Time),
                            "%Y %m %d %H:%M", tz=tz))
    } else {
        ## monthly data
        ISOdatetime(rawData$Year, rawData$Month, 15, 0, 0, 0, tz="UTC")
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
    res@data <- as.list(rawData)
    pl <- paste("read.met(\"", file, "\", type=\"csv1\", tz=\"", tz, "\")", sep="")
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
        res@metadata$units$humidity <- list(unit=expression("%"), scale="")
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
            ## The check on being logical type handles the case where a flag consists entirely of empty strings in the .csv
            ## file. I think that in that case, all the values end up being NA, so we just ignore this and make a bunch of
            ## zero-length strings.
            res@metadata$flags[[flagType]] <- if (is.logical(res@data[[flagName]])) rep("", length(res@data[[flagName]]))
                else as.character(res@data[[flagName]])
            res@data[[flagName]] <- NULL
        }
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
    res@data <- res@data[order(names(res@data))] # put in alphabetical order for easier scanning in summary() views
    res@processingLog <- processingLogAppend(res@processingLog, paste(deparse(match.call()), sep="", collapse=""))
    oceDebug(debug, "} # read.met1()\n", unindent=1, style="bold")
    res
}

read.met.csv2 <- function(file, skip=NULL, tz=getOption("oceTz"), debug=getOption("oceDebug"))
{
    if (!is.character(file))
        stop("'file' must be a character string")
    oceDebug(debug, "read.met.csv2(\"", file, "\") {\n", sep="", unindent=1, style="bold")
    ## Sample first two lines (as of 2019 oct 12)
    ## "Longitude (x)","Latitude (y)","Station Name","Climate ID","Date/Time","Year","Month","Day","Time","Temp (°C)","Temp Flag","Dew Point Temp (°C)","Dew Point Temp Flag","Rel Hum (%)","Rel Hum Flag","Wind Dir (10s deg)","Wind Dir Flag","Wind Spd (km/h)","Wind Spd Flag","Visibility (km)","Visibility Flag","Stn Press (kPa)","Stn Press Flag","Hmdx","Hmdx Flag","Wind Chill","Wind Chill Flag","Weather"
    ## "-94.97","74.72","RESOLUTE BAY A","2403497","2019-10-01 00:00","2019","10","01","00:00","-3.2","","-4.6","","90","","18","","36","","","M","100.35","","","","-11","","NA"
    res <- new("met", time=1)
    owarn <- options()$warn
    options(warn=-1)
    text <- readLines(file, 1, encoding="UTF-8", warn=FALSE)
    dataNames <- strsplit(gsub('"', '', text[1]), ",")[[1]]
    data <- read.csv(file, skip=1, encoding="UTF-8", header=FALSE)
    options(warn=owarn)
    if ("Dew Point Temp (\u00B0C)" %in% dataNames) {
        res@metadata$units$dewPoint <- list(unit=expression(degree*C), scale="ITS-90")
        res@metadata$dataNamesOriginal$dewPoint <- "Dew Point Temp (\u00B0C)"
        dataNames[dataNames == "Dew Point Temp (\u00B0C)"] <- "dewPoint"
    }
    if ("Hmdx" %in% dataNames) {
        res@metadata$units$humidex <- list(unit=expression(degree*C), scale="ITS-90")
        res@metadata$dataNamesOriginal$humidex <- "Hmdx"
        dataNames[dataNames == "Hmdx"] <- "humidex"
    }
    if ("Longitude (x)" %in% dataNames) {
        res@metadata$dataNamesOriginal$longitude <- "Longitude (x)"
        dataNames[dataNames == "Longitude (x)"] <- "longitude"
    }
    if ("Latitude (y)" %in% dataNames) {
        res@metadata$dataNamesOriginal$latitude <- "Latitude (y)"
        dataNames[dataNames == "Latitude (y)"] <- "latitude"
    }
    if ("Rel Hum (%)" %in% dataNames) {
        res@metadata$units$humidity <- list(unit=expression("%"), scale="")
        res@metadata$dataNamesOriginal$humidity <- "Rel Hum (%)"
        dataNames[dataNames == "Rel Hum (%)"] <- "humidity"
    }
    if ("Station Name" %in% dataNames) {
        res@metadata$dataNamesOriginal$latitude <- "Station Name"
        dataNames[dataNames == "Station Name"] <- "station"
    }
    if ("Stn Press (kPa)" %in% dataNames) {
        res@metadata$units$pressure <- list(unit=expression(kPa), scale="")
        res@metadata$dataNamesOriginal$pressure <- "Stn Press (kPa)"
        dataNames[dataNames == "Stn Press (kPa)"] <- "pressure"
    }
    if ("Temp (\u00B0C)" %in% dataNames) {
        res@metadata$units$temperature <- list(unit=expression(degree*C), scale="ITS-90")
        res@metadata$dataNamesOriginal$temperature <- "Temp (\u00B0C)"
        dataNames[dataNames == "Temp (\u00B0C)"] <- "temperature"
    }
    if ("Mean Temp (\u00B0C)" %in% dataNames) {
        res@metadata$units$temperature <- list(unit=expression(degree*C), scale="ITS-90")
        res@metadata$dataNamesOriginal$temperature <- "Mean Temp (\u00B0C)"
        dataNames[dataNames == "Mean Temp (\u00B0C)"] <- "temperature"
    }
     if ("Visibility (km)" %in% dataNames) {
        res@metadata$units$visibility <- list(unit=expression(km), scale="")
        res@metadata$dataNamesOriginal$visibility <- "Visibility (km)"
        dataNames[dataNames == "Visibility (km)"] <- "visibility"
    }
    if ("Wind Chill" %in% dataNames) {
        res@metadata$units$windChill <- list(unit=expression(degree*C), scale="ITS-90")
        res@metadata$dataNamesOriginal$windChill <- "Wind Chill"
        dataNames[dataNames == "Wind Chill"] <- "windChill"
    }
    if ("Weather" %in% dataNames) {
        res@metadata$units$weather <- list(unit=expression(), scale="")
        res@metadata$dataNamesOriginal$weather <- "Weather"
        dataNames[dataNames == "Weather"] <- "weather"
    }
    ##> print(dataNames)
    ##> head(data)
    ##> str(data)
    ##> browser()
    names(data) <- dataNames
    ##> print("DANNY")
    ##> print(dataNames)
    res@data <- data
    ## climateIdentifier
    if ("Climate ID" %in% dataNames) {
        res@metadata$climateIdentifier <- data[["Climate ID"]][1]
        res@data[["Climate ID"]] <- NULL
    }
    ## dataNames <- names(data)
    nsamples <- dim(data)[1]
    oceDebug(debug, vectorShow(nsamples))
    ## Time
    if ("Time" %in% dataNames) {
        hour <- as.numeric(lapply(as.character(data$Time), function(x) strsplit(x, ":")[[1]][1]))
        minute <- as.numeric(lapply(as.character(data$Time), function(x) strsplit(x, ":")[[1]][2]))
    } else if ("Time (LST)" %in% dataNames) {
        hour <- as.numeric(lapply(as.character(data[["Time (LST)"]]), function(x) strsplit(x, ":")[[1]][1]))
        minute <- as.numeric(lapply(as.character(data[["Time (LST)"]]), function(x) strsplit(x, ":")[[1]][2]))
    } else {
        hour <- rep(0, nsamples)
        minute <- rep(0, nsamples)
    }
    second <- 0
    day <- if ("Day" %in% dataNames) data[["Day"]] else 1
    time <- ISOdatetime(data[["Year"]], data[["Month"]], day, hour, minute, second, tz=tz)
    res@data$time <- time
    res@data[["Date/Time"]] <- NULL
    res@data[["Year"]] <- NULL
    res@data[["Month"]] <- NULL
    res@data[["Day"]] <- NULL
    res@data[["Time"]] <- NULL
    ## wind
    if ("Wind Spd (km/h)" %in% dataNames && "Wind Dir (10s deg)" %in% dataNames) {
        res@data$speed <- data[["Wind Spd (km/h)"]] * 1000 / 3600 # convert km/h to m/s
        res@metadata$dataNamesOriginal$speed <- "-"
        res@data[["Wind Spd (km/h)"]] <- NULL
        res@data$direction <- 10 * res@data[["Wind Dir (10s deg)"]] # convert 10s of degrees to degrees
        res@metadata$dataNamesOriginal$direction <- "-"
        res@data[["Wind Dir (10s deg)"]] <- NULL
        rpd <- atan2(1, 1) / 45            # radian/degree
        theta <- rpd * (90 - res@data$direction)
        ## Note the (-) to get from "wind from" to "wind speed towards"
        res@data$u <- -res@data$speed * sin(theta)
        res@data$v <- -res@data$speed * cos(theta)
        zero <- is.na(res@data$direction) & res@data$speed == 0
        res@data$u[zero] <- 0
        res@data$v[zero] <- 0
        res@metadata$units$direction  <- list(unit=expression(degree), scale="")
        res@metadata$units$speed <- list(unit=expression(m/s), scale="")
        res@metadata$units$u <- list(unit=expression(m/s), scale="")
        res@metadata$units$v <- list(unit=expression(m/s), scale="")
    }

    ## Move some things to metadata, if they are uni-valued. This is so
    ## code written for the csv1 style will work for csv2 style also.
    if (1 == length(unique(data$longitude))) {
        res@metadata$longitude <- data$longitude[1]
        res@data$longitude <- NULL
    }
    if (1 == length(unique(data$latitude))) {
        res@metadata$latitude <- data$latitude[1]
        res@data$latitude <- NULL
    }
    if (1 == length(unique(data$station))) {
        res@metadata$station <- as.character(data$station[1])
        res@data$station <- NULL
    }

    ## Flags
    res@metadata$flags <- list()
    knownFlags <- list(dewpoint="Dew Point Temp Flag",
                       humidex="Hmdx Flag",
                       direction="Wind Dir Flag",
                       humidity="Rel Hum Flag",
                       pressure="Stn Press Flag",
                       speed="Wind Spd Flag",
                       temperature="Temp Flag",
                       visibility="Visibility Flag",
                       windChill="Wind Chill Flag"
                       )
    knownFlagNames <- names(knownFlags)
    for (iflag in seq_along(knownFlags)) {
        ##message('iflag=',iflag,'->',knownFlags[[iflag]], '; ', knownFlagNames[[iflag]])
        res@metadata$flags[[knownFlagNames[iflag]]] <- res@data[[knownFlags[[iflag]]]]
        res@data[[knownFlags[[iflag]]]] <- NULL
    }
    res@data <- res@data[order(names(res@data))] # put in alphabetical order for easier scanning in summary() views
    res@processingLog <- processingLogAppend(res@processingLog, paste(deparse(match.call()), sep="", collapse=""))
    oceDebug(debug, "} # read.met.csv2()\n", unindent=1, style="bold")
    res
}

read.met.xml2 <- function(file, skip=NULL, tz=getOption("oceTz"), debug=getOption("oceDebug"))
{
    oceDebug(debug, "read.met.xml2(file=\"", file, "\", ...) {\n", sep="", unindent=1, style="bold")
    if (!requireNamespace("XML", quietly=TRUE))
        stop('must install.packages("XML") to read rsk data')
    xml <- XML::xmlToList(XML::xmlParse(file)) # a list
    ## The names of items in the list was discovered with
    ##     head(names(list))
    stationInformation <- xml$stationinformation
    ## Isolate station data. (I bet there's a function for this.)
    isStation <- unlist(lapply(names(xml), function(x) x=="stationdata"))
    stationData <- xml[isStation]
    n <- length(stationData)
    res <- new("met")
    res@metadata$filename <- file
    ## Fill in station metadata. The available entries are found with
    ##     names(stationInformation,1)
    ## the output of which yields as follows, for a file downloaded 2019 oct 13.
    ##     "name"
    ##     "province"
    ##     "stationoperator" (Not present in all files)
    ##     "latitude"
    ##     "longitude"
    ##     "elevation"
    ##     "climate_identifier"
    ##     "wmo_identifier"
    ##     "tc_identifier"
    ##     "note"
    res@metadata$name <- stationInformation$name
    res@metadata$province <- stationInformation$province
    res@metadata$stationOperator <- stationInformation$stationoperator
    res@metadata$latitude <- as.numeric(stationInformation$latitude)
    res@metadata$longitude <- as.numeric(stationInformation$longitude)
    res@metadata$elevation <- as.numeric(stationInformation$elevation)
    res@metadata$climateIdentifier <- stationInformation$climate_identifier
    res@metadata$WMOIdentifier <- stationInformation$wmo_identifier
    res@metadata$TCIdentifier <- stationInformation$tc_identifier
    res@metadata$note <- stationInformation$note
    ## Fill in data. The names of items are found with
    ##     str(stationData[[1]], 1)
    n <- length(stationData)
    oceDebug(debug, "number of data, n=", n, "\n")
    ## Get time-series data
    ##message("item 244 is ok, but item 245 has no pressure, viz.")
    ##str(stationData[[244]]$stnpress)
    ##str(stationData[[245]]$stnpress)
    fixNull <- function(x)
        ifelse(is.list(x), x$text, NA)
    extract <- function(name)
        lapply(1:n, function(i) fixNull(stationData[[i]][[name]]))
    res@metadata$dataNamesOriginal <- list()
    ## "temp" "dptemp" "relhum" "winddir" "windspd"
    ## "visibility" "stnpress" "humidex" "windchill" "weather"
    res@data$temperature <- as.numeric(extract("temp"))
    res@metadata$dataNamesOriginal$temperature <- "temp"
    res@metadata$units$temperature <- list(unit=expression(degree*C), scale="ITS-90")

    res@data$dewPoint <- as.numeric(extract("dptemp"))
    res@metadata$dataNamesOriginal$dewPoint <- "dptemp"
    res@metadata$units$dewPoint <- list(unit=expression(degree*C), scale="ITS-90")

    res@data$humidity <- as.numeric(extract("relhum"))
    res@metadata$dataNamesOriginal$humidity <- "relhum"
    res@metadata$units$humidity <- list(unit=expression("%"), scale="")

    res@data$direction <- 10 * as.numeric(extract("winddir")) # from 10deg to deg
    res@metadata$dataNamesOriginal$direction <- "-"
    res@metadata$units$direction <- list(unit=expression(degree), scale="")

    res@data$speed <- as.numeric(extract("windspd")) * 1000 / 3600 # from km/h to m/s
    res@metadata$dataNamesOriginal$speed <- "-"
    res@metadata$units$speed <- list(unit=expression(m/s), scale="")

    res@data$u <- -res@data$speed * cos(res@data$direction * pi / 180) # from met to ocean sign
    res@metadata$dataNamesOriginal$u  <- "-"
    res@metadata$units$u <- list(unit=expression(m/s), scale="")

    res@data$v <- -res@data$speed * sin(res@data$direction * pi / 180)
    res@metadata$dataNamesOriginal$v  <- "-"
    res@metadata$units$v <- list(unit=expression(m/s), scale="")

    ## fix up NA cases
    zero <- is.na(res@data$direction) & res@data$speed == 0
    res@data$u[zero] <- 0
    res@data$v[zero] <- 0

    res@data$visibility <- extract("visibility")
    res@metadata$dataNamesOriginal$visibility  <- "visibility"

    res@data$pressure <- as.numeric(extract("stnpress"))
    res@metadata$dataNamesOriginal$pressure  <- "stnpress"
    res@metadata$units$pressure <- list(unit=expression(kPa), scale="")

    res@data$humidex <- as.numeric(extract("humidex"))
    res@metadata$dataNamesOriginal$humidex  <- "humidex"
    res@metadata$units$humidex <- list(unit=expression(degree*C), scale="ITS-90")

    res@data$windChill <- as.numeric(extract("windchill"))
    res@metadata$dataNamesOriginal$windChill  <- "windchill"
    res@metadata$units$windChill <- list(unit=expression(degree*C), scale="ITS-90")

    res@data$weather <- extract("weather")
    res@metadata$dataNamesOriginal$weather  <- "weather"

    ## Time
    attrsNames <- names(stationData[[1]][[".attrs"]])
    oceDebug(debug, vectorShow(attrsNames, postscript=" (names relating to time)"))
    year <- if ("year" %in% attrsNames) as.numeric(lapply(stationData, function(sd) sd[[".attrs"]][["year"]]))
        else 2000
    month <- if ("month" %in% attrsNames) as.numeric(lapply(stationData, function(sd) sd[[".attrs"]][["month"]]))
        else 1
    day <- if ("day" %in% attrsNames) as.numeric(lapply(stationData, function(sd) sd[[".attrs"]][["day"]]))
        else 1
    hour <- if ("hour" %in% attrsNames) as.numeric(lapply(stationData, function(sd) sd[[".attrs"]][["hour"]]))
        else 0
    res@data$time <- ISOdatetime(year, month, day, hour, 0, 0, tz="UTC")
    res@metadata$dataNamesOriginal$time  <- "-"
    res@data <- res@data[order(names(res@data))] # put in alphabetical order for easier scanning in summary() views
    res@processingLog <- processingLogAppend(res@processingLog,
                                             paste("read.met.xml2(file=\"", file, "\"",
                                                   ", skip=", if(is.null(skip)) "NULL" else skip,
                                                   ", tz=\"", tz, "\")", sep=""))
    oceDebug(debug, "} # read.met.xml2()\n", unindent=1, style="bold")
    res
}


#' Plot a met Object
#'
#' Creates a multi-panel summary plot of data measured in a meteorological data
#' set.  cast. The panels are controlled by the `which` argument.
#'
#' If more than one panel is drawn, then on exit from `plot.met`, the
#' value of `par` will be reset to the value it had before the function
#' call.  However, if only one panel is drawn, the adjustments to `par`
#' made within `plot.met` are left in place, so that further additions may
#' be made to the plot.
#'
#' @param x a [met-class] object.
#'
#' @param which list of desired plot types.
#' * `which=1` gives a time-series plot of temperature
#'
#' * `which=2` gives a time-series plot of pressure
#'
#' * `which=3` gives a time-series plot of the x (eastward) component of velocity
#'
#' * `which=4` gives a time-series plot of the y (northward) component of velocity
#'
#' * `which=5` gives a time-series plot of speed
#'
#' * `which=6` gives a time-series plot of direction (degrees clockwise from north;
#' note that the values returned by `met[["direction"]]` must be multiplied by 10
#' to get the direction plotted)
#'
#' @param tformat optional argument passed to [oce.plot.ts()], for
#' plot types that call that function.  (See [strptime()] for the
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
#' @family functions that plot oce data
#' @family things related to met data
#'
#' @aliases plot.met
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
