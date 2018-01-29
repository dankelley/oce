## vim:textwidth=128:expandtab:shiftwidth=4:softtabstop=4

#' @title Class for data stored in a format used at Bremen
#'
#' @description
#' Class for data stored in a format used at Bremen. This is somewhat unusual
#' amongst \code{oce} classes, in that it does not map to a particular
#' instrument. Although some functions are provided for dealing with these
#' data (see \dQuote{Details}), the most common action is to read the
#' data with \code{\link{read.bremen}}, and then to coerce the object to
#' another storage class (e.g. using \code{\link{as.ctd}} for CTD-style
#' data) so that specialized functions can be used thereafter.
#'
#' @description
#' The main function is \code{\link{read.bremen}}.  A simple
#' plotting method is provided with \code{\link{plot,bremen-method}}, and
#' \code{\link{summary,bremen-method}} provides summaries. Data may be
#' retrieved with \code{\link{[[,bremen-method}} or replaced with
#' \code{\link{[[<-,bremen-method}}.
#'
#' @author Dan Kelley
#' @family classes provided by \code{oce}
#' @family things related to \code{bremen} data
setClass("bremen", contains="oce") # 20150528 may be called "geomar" or something later

setMethod(f="initialize",
          signature="bremen",
          definition=function(.Object, filename="") {
              ## Assign to some columns so they exist if needed later (even if they are NULL)
              .Object@metadata$filename <- filename
              .Object@processingLog$time <- as.POSIXct(Sys.time())
              .Object@processingLog$value <- "create 'bremen' object"
              return(.Object)
          })

#' @title Extract Something From a Bremen Object
#' @param x A bremen object, i.e. one inheriting from \code{\link{bremen-class}}.
#' @template sub_subTemplate
#' @family things related to \code{bremen} data
setMethod(f="[[",
          signature(x="bremen", i="ANY", j="ANY"),
          definition=function(x, i, j, ...) {
              callNextMethod()         # [[
          })

#' @title Replace Parts of a Bremen Object
#' @param x An \code{bremen} object, i.e. inheriting from \code{\link{bremen-class}}
#' @template sub_subsetTemplate
#' @family things related to \code{bremen} data
setMethod(f="[[<-",
          signature(x="bremen", i="ANY", j="ANY"),
          definition=function(x, i, j, ..., value) {
              callNextMethod(x=x, i=i, j=j, ...=..., value=value) # [[<-
          })

#' @title Plot a Bremen Object
#'
#' @description
#' Plot a \code{bremen} object, i.e. one inheriting from \code{\link{bremen-class}}.
#' If \code{x} seems to be a CTD dataset, uses \code{\link{plot,ctd-method}};
#' otherwise, \code{x} is assumed to be a lowered-adp object, and a two-panel
#' plot is created with \code{\link{plot,ladp-method}} to show velocity variation with
#' pressure.
#'
#' @param x A \code{bremen} object, e.g. as read by \code{\link{read.bremen}}.
#' @param type Optional string indicating the type to which \code{x} should be
#' coerced before plotting. The choices are \code{ctd} and \code{ladp}.
#' @param ... Other arguments, passed to plotting functions.
#' @author Dan Kelley
#' @family functions that plot \code{oce} data
#' @family things related to \code{bremen} data
#' @aliases plot.bremen
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


#' @title Summarize a Bremen Object
#'
#' @description
#' Pertinent summary information is presented, including the station name,
#' sampling location, data ranges, etc.
#'
#' @param object A \code{bremen} object, i.e. one inheriting from \code{\link{bremen-class}}.
#' call to \code{\link{read.bremen}}.
#' @param ... Further arguments passed to or from other methods.
#' @author Dan Kelley
#' @family things related to \code{bremen} data
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
              invisible(callNextMethod()) # summary
          })


findInHeaderBremen <- function(key, lines)
{
    i <- grep(paste("^", key, sep=""), lines)[1] # only take first -- may be problematic
    if (length(i) < 1) "" else gsub("^.*=[ ]*", "", lines[i])
}


#' @title Read a Bremen File
#'
#' @description
#' Read a file in Bremen format, producing an object inheriting from
#' \code{\link{bremen-class}}.
#'
#' @details
#' Velocities are assumed to be in
#' cm/s, and are converted to m/s to follow the oce convention. Shears
#' (which is what the variables named \code{uz} and \code{vz} are assumed
#' to represent) are assumed to be in (cm/s)/m, although they could be in 1/s
#' or something else; the lack of documentation is a problem here. Also,
#' note that the assumed shears are not just first-difference estimates
#' of velocity, given the results of a sample dataset:
#' \preformatted{
#' > head(data.frame(b[["data"]]))
#'   pressure      u      v       uz       vz
#' 1        0  0.092 -0.191  0.00000  0.00000
#' 2       10  0.092 -0.191  0.02183 -0.35412
#' 3       20  0.092 -0.191  0.03046 -0.09458
#' 4       30  0.026 -0.246 -0.03948  0.02169
#' 5       40 -0.003 -0.212 -0.02614  0.03111
#' 6       50 -0.023 -0.169 -0.03791  0.01706
#' }
#'
#' @param file a connection or a character string giving the name of the file
#' to load.
#' @return An object of \code{\link{bremen-class}}.
#' @section Issues: This function may be renamed (or removed) without notice.
#' It was created to read some data being used in a particular research
#' project, and will be rendered useless if Bremen changes this data format.
#' @author Dan Kelley
#' @family things related to \code{bremen} data
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
