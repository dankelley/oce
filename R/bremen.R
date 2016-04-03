## vim:textwidth=128:expandtab:shiftwidth=4:softtabstop=4

#' Class for data stored in a format used at Bremen
#'
#' Class for data stored in a format used at Bremen. This is somewhat unusual
#' amongst \code{oce} classes, in that it does not map to a particular
#' instrument. Although some functions are provided for dealing with these
#' data (see \dQuote{Details}), the most common action is to read the
#' data with \code{\link{read.bremen}}, and then to coerce the object to
#' another storage class (e.g. using \code{\link{as.ctd}} for CTD-style
#' data) so that specialized functions can be used thereafter.
#'
#' The main function is \code{\link{read.bremen}}.  A simple
#' plotting method is provided with \code{\link{plot,bremen-method}}, and
#' \code{\link{summary,bremen-method}} provides summaries. Data may be
#' retrieved with \code{\link{[[,bremen-method}} or replaced with
#' \code{\link{[[<-,bremen-method}}.
#'
#' @author Dan Kelley
#' @family classes provided by \code{oce}
setClass("bremen", contains="oce") # 20150528 may be called "geomar" or something later

setMethod(f="initialize",
          signature="bremen",
          definition=function(.Object,filename="") {
              ## Assign to some columns so they exist if needed later (even if they are NULL)
              .Object@metadata$filename <- filename
              .Object@processingLog$time <- as.POSIXct(Sys.time())
              .Object@processingLog$value <- "create 'bremen' object"
              return(.Object)
          })

#' @title Extract Something From a \code{bremen} Object
#' @param x A bremen object, i.e. one inheriting from \code{\link{bremen-class}}.
#' @template sub_subTemplate
#' @family functions that handle \code{bremen} data
setMethod(f="[[",
          signature(x="bremen", i="ANY", j="ANY"),
          definition=function(x, i, j, ...) {
              callNextMethod()
          })

#' @title Replace Parts of a \code{bremen} Object
#' @param x An \code{bremen} object, i.e. inheriting from \code{\link{bremen-class}}
#' @template sub_subsetTemplate
#' @family functions that handle \code{bremen} data
setMethod(f="[[<-",
          signature(x="bremen", i="ANY", j="ANY"),
          definition=function(x, i, j, value) {
              callNextMethod(x=x, i=i, j=j, value=value)
          })

#' Plot a bremen object
#'
#' Plot a \code{bremen} object, i.e. one inheriting from \code{\link{bremen-class}}.
#'
#' If \code{x} seems to be a CTD dataset, uses \code{\link{plot,ctd-method}};
#' otherwise, \code{x} is assumed to be a lowered-adp object, and a two-panel
#' plot is created with \code{\link{plot.ladp}} to show velocity varation with
#' pressure.
#'
#' @aliases plot.bremen plot,bremen,missing-method plot,bremen-method
#' @param x A \code{bremen} object, e.g. as read by \code{\link{read.bremen}}.
#' @param type Optional string indicating the type to which \code{x} should be
#' coerced before ploting. The choices are \code{ctd} and \code{ladp}.
#' @param ... Other arguments, passed to plotting functions.
#' @author Dan Kelley
#' @family functions that plot \code{oce} data
#' @family functions that handle \code{bremen} data
setMethod(f="plot",
          signature=signature("bremen"),
          definition=function(x, type, ...) {
              names <- names(x@data)
              ##n <- length(names)
              if (missing(type)) {
                  if ("salinity" %in% names) plot(as.ctd(x), ...)
                  else plot(as.ladp(x), ...)
              } else {
                  if (!is.na(pmatch(type, "ctd"))) plot(as.ctd(x), ...)
                  else if (!is.na(pmatch(type, "ladp"))) plot(as.ladp(x), ...)
              }
          })


#' Summarize a bremen object
#'
#' Summarizes some of the data in a \code{bremen} object.
#'
#' Pertinent summary information is presented, including the station name,
#' sampling location, data ranges, etc.
#'
#' @param object A \code{bremen} object, i.e. one inheriting from \code{\link{bremen-class}}.
#' call to \code{\link{read.bremen}}.
#' @param ... Further arguments passed to or from other methods.
#' @author Dan Kelley
#' @family functions that handle \code{bremen} data
setMethod(f="summary",
          signature="bremen",
          definition=function(object, ...) {
              cat("Bremen Summary\n--------------\n\n")
              #showMetadataItem(object, "type", "Instrument: ")
              showMetadataItem(object, "model", "Instrument model:    ")
              #showMetadataItem(object, "serialNumber", "Instrument serial number:  ")
              #showMetadataItem(object, "serialNumberTemperature", "Temperature serial number:  ")
              #showMetadataItem(object, "serialNumberConductivity", "Conductivity serial number:  ")
              showMetadataItem(object, "filename", "File source:         ")
              showMetadataItem(object, "hexfilename", "Original file source (hex):  ")
              showMetadataItem(object, "institute", "Institute:           ")
              showMetadataItem(object, "scientist", "Chief scientist:     ")
              showMetadataItem(object, "date", "Date:      ", isdate=TRUE)
              showMetadataItem(object, "startTime", "Start time:          ", isdate=TRUE)
              showMetadataItem(object, "systemUploadTime", "System upload time:  ", isdate=TRUE)
              showMetadataItem(object, "cruise",  "Cruise:              ")
              showMetadataItem(object, "ship",    "Vessel:              ")
              showMetadataItem(object, "station", "Station:             ")
              showMetadataItem(object, "profile", "Profile:             ")
              callNextMethod()
          })


findInHeaderBremen <- function(key, lines)
{
    i <- grep(paste("^", key, sep=""), lines)[1] # only take first -- may be problematic
    if (length(i) < 1) "" else gsub("^.*=[ ]*", "", lines[i])
}


#' Read a Bremen data file
#'
#' @param file a connection or a character string giving the name of the file
#' to load.
#' @return An object of \code{\link{bremen-class}}.
#' @section Issues: This may be renamed (or removed) without notice.
#' @author Dan Kelley
#' @family functions that handle \code{bremen} data
read.bremen <- function(file)
{
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
    res <- new("bremen")
    lines <- readLines(file)
    ## Discover header as lines starting with a letter
    headerLength <- max(grep("^[a-zA-Z]", lines))
    h <- lines[1:headerLength]
    res@metadata$filename <- filename
    res@metadata$header <- h
    lat <- strsplit(findInHeaderBremen("Latitude", h), " ")[[1]]
    latitude <- as.numeric(lat[1]) + as.numeric(lat[2]) / 60 # assume N hemi
    if (lat[3] == "S")
        latitude <- -latitude
    res@metadata$latitude <- latitude
    lon <- strsplit(findInHeaderBremen("Longitude", h), " ")[[1]]
    longitude <- as.numeric(lon[1]) + as.numeric(lon[2]) / 60 # assume N hemi
    if (lon[3] == "W")
        longitude <- -longitude
    res@metadata$longitude <- longitude
    date <- findInHeaderBremen("Date", h)
    time <- findInHeaderBremen("Time", h)
    datetime <- paste(date, " ", time, ":00", sep="")
    res@metadata$time <- as.POSIXct(datetime, tz="UTC")
    res@metadata$station <- findInHeaderBremen("Station", h)
    res@metadata$profile <- findInHeaderBremen("Profile", h)
    res@metadata$ship <- findInHeaderBremen("Shipname", h)
    res@metadata$cruise <- findInHeaderBremen("Cruise", h)
    res@metadata$scientist<- findInHeaderBremen("CruisePI", h)
    res@metadata$institute <- findInHeaderBremen("Affiliation", h)
    res@metadata$model <- findInHeaderBremen("CTD_Model", h)
    res@metadata$waterDepth <- as.numeric(findInHeaderBremen("WaterDepth", h))
    res@metadata$maxPress <- as.numeric(findInHeaderBremen("MaxPress", h))
    ## Columns have nicknames
    nicknames <- strsplit(gsub(" ", "", strsplit(h[grep("^(Columns)|(Fields)", h)], "=")[[1]][2]), ":")[[1]]
    names <- nicknames
    names[nicknames=="p"] <- "pressure"
    names[nicknames=="t"] <- "temperature"
    names[nicknames=="pt"] <- "theta"
    names[nicknames=="sth"] <- "sigmaTheta"
    names[nicknames=="s"] <- "salinity"
    names[nicknames=="o"] <- "oxygen"
    names[nicknames=="z"] <- "pressure" # NOTE: bremen files have positive z values
    ## infer column names from last line of header (guessing a bit)
    data <- read.table(text=lines[-seq.int(1, headerLength)], header=FALSE, col.names=names)
    for (name in names(data)) {
        ## FIXME: I have no idea what "uz" is, so I cannot guess the unit
        if (name == "u" || name == "v" || name == "uz" || name == "vz") {
            res@data[name] <- data[name] / 100 # velocities in cm/s
        } else if (name == "salinity" || name == "temperature") {
            #res@data[name] <- ifelse(data[[name]] == -9, NA, data[[name]])
            res@data[name] <- as.data.frame(ifelse(data[[name]] == -9, NA, data[[name]]))
        } else {
            res@data[name] <- data[name]
        }
    }
    res
}

