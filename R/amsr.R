## vim:textwidth=80:expandtab:shiftwidth=4:softtabstop=4

#' Class to Hold amsr Data
#'
#' The Advanced Microwave Scanning Radiometer (AMSR-2) is in current operation on
#' the Japan Aerospace Exploration Agency (JAXA) GCOM-W1 space craft, launched in
#' May 2012. Data are processed by Remote Sensing Systems. The satellite
#' completes an ascending and descending pass during local daytime and nightime
#' hours respectively. Each daily file contains 7 daytime and 7 nighttime
#' maps of variables named as follows within the \code{data}
#' slot of amsr objects: \code{timeDay}, 
#' \code{SSTDay}, \code{LFwindDay} (wind at 10m sensed in
#' the 10.7GHz band), \code{MFwindDay} (wind at 10m sensed at 18.7GHz),
#' \code{vaporDay}, \code{cloudDay}, and \code{rainDay}, along with
#' similarly-named items that end in \code{Night}.
#' See [1] for additional information on the instrument.
#'
#' @details
#' The bands are stored in \code{\link{raw}} form, to save storage. The accessor
#' function \code{\link{[[,amsr-method}} can provide these values in \code{raw}
#' form or in physical units; \code{\link{plot,amsr-method}}, and
#' \code{\link{summary,amsr-method}} work with physical units.
#'
#' @author Dan Kelley and Chantelle Layton
#' @concept satellite
#' @references
#' 1. \url{http://www.remss.com/missions/amsre}
#' @seealso \code{\link{landsat-class}} for handling data from the Landsat-8 satellite.
#'
#' @family things related to \code{amsr} data
setClass("amsr", contains="satellite")

setMethod(f="initialize",
          signature="amsr",
          definition=function(.Object, filename) {
              if (!missing(filename))
                  .Object@metadata$filename <- filename
              .Object@processingLog$time <- as.POSIXct(Sys.time())
              .Object@processingLog$value <- "create 'amsr' object"
              return(.Object)
          })

setMethod(f="show",
          signature="amsr",
          definition=function(object) {
              cat("Data (physical units):\n")
              dataNames <- names(object@data)
              for (b in seq_along(dataNames)) {
                  dim <- if (is.list(object@data[[b]])) dim(object@data[[b]]$lsb) else dim(object@data[[b]])
                  cat("  \"", dataNames[b], "\" has dimension c(", dim[1], ",", dim[2], ")\n", sep='')
              }
          })


#' Summarize an AMSR Object
#'
#' Although the data are stored in \code{\link{raw}} form, the summary
#' presents results in physical units.
#'
#' @param object The object to be summarized.
#' @param ... Ignored.
#' @author Dan Kelley
#' @concept satellite
#' @family things related to \code{amsr} data
setMethod(f="summary",
          signature="amsr",
          definition=function(object, ...) {
              cat("Amsr Summary\n------------\n\n")
              showMetadataItem(object, "filename",   "Data file:           ")
              cat(sprintf("* Longitude range:     %.4fE to %.4fE\n", object@metadata$longitude[1], tail(object@metadata$longitude,1))) 
              cat(sprintf("* Latitude range:      %.4fN to %.4fN\n", object@metadata$latitude[1], tail(object@metadata$latitude,1))) 
              for (name in names(object@data)) {
                  object@data[[name]] <- object[[name]] # translate to science units
              }
              callNextMethod()
          })

#' Extract Something From an amsr Object
#'
#' Extract something from the \code{metadata} or \code{data} slot of an
#' \code{\link{amsr-class}} object.
#'
#' @details
#' Partial matches for \code{i}
#' are permitted for \code{metadata}, and \code{j} is ignored for
#' \code{metadata}.
#'
#' Data within the \code{data} slot may be found directly, e.g.
#' \code{j="SSTDay"} will yield sea-surface temperature in the daytime
#' satellite, and \code{j="SSTNight"} is used to access the nighttime data. In
#' addition, \code{j="SST"} yields an average of the night and day values
#' (using just one of these, if the other is missing). This scheme works for
#' all the data stored in \code{amsr} objects, namely:
#' \code{time}, \code{SST}, \code{LFwind}, \code{MFwind},
#' \code{vapor}, \code{cloud} and \code{rain}.  In each case, the default
#' is to calculate values in scientific units, unless \code{j="raw"}, in
#' which case the raw data are returned.
#'
#' The \code{"raw"} mode can be useful
#' in decoding the various types of missing value that are used by \code{amsr}
#' data, namely \code{as.raw(255)} for land, \code{as.raw(254)} for
#' a missing observation, \code{as.raw(253)} for a bad observation,
#' \code{as.raw(252)} for sea ice, or \code{as.raw(251)} for missing SST
#' due to rain or missing water vapour due to heavy rain. Note that
#' something special has to be done for e.g. \code{d[["SST", "raw"]]}
#' because the idea is that this syntax (as opposed to specifying
#' \code{"SSTDay"}) is a request to try to find good
#' data by looking at both the Day and Night measurements. The scheme
#' employed is quite detailed. Denote by "A" the raw value of the desired field
#' in the daytime pass, and by "B" the corresponding value in the 
#' nighttime pass. If either A or B is 255, the code for land, then the
#' result will be 255. If A is 254 (i.e. there is no observation),
#' then B is returned, and the reverse holds also. Similarly, if either
#' A or B equals 253 (bad observation), then the other is returned.
#' The same is done for code 252 (ice) and code 251 (rain).
#'
#' @return
#' In all cases, the returned value is a matrix with 
#' with \code{NA} values inserted at locations where
#' the raw data equal \code{as.raw(251:255)}, as explained
#' in \dQuote{Details}.
#'
#' @param x An \code{amsr} object, i.e. one inheriting from \code{\link{amsr-class}}.
#' @author Dan Kelley
#' @template sub_subTemplate
#' @examples
#' \dontrun{
#' # Show a daytime SST image, along with an indication of whether
#' # the NA values are from rain.
#' library(oce)
#' earth <- read.amsr("f34_20160102v7.2.gz")
#' fclat <- subset(earth , 35 <= latitude & latitude <= 55)
#' fc <- subset(fclat , -70 <= longitude & longitude <= -30)
#' par(mfrow=c(2,1))
#' plot(fc, "SSTDay")
#' rainy <- fc[["SSTDay", "raw"]] == as.raw(0xfb)
#' lon <- fc[["longitude"]]
#' lat <- fc[["latitude"]]
#' asp <- 1 / cos(pi*mean(lat)/180)
#' imagep(lon, lat, rainy, asp=asp)
#' mtext("red: too rainy to sense SSTDay")
#' }
#' @family things related to \code{amsr} data
setMethod(f="[[",
          signature(x="amsr", i="ANY", j="ANY"),
          definition=function(x, i, j, ...) {
              debug <- getOption("oceDebug")
              oceDebug(debug, "amsr [[ {\n", unindent=1)
              if (missing(i))
                  stop("Must name a amsr item to retrieve, e.g. '[[\"panchromatic\"]]'", call.=FALSE)
              i <- i[1]                # drop extras if more than one given
              if (!is.character(i))
                  stop("amsr item must be specified by name", call.=FALSE)
              if (is.character(i) && !is.na(pmatch(i, names(x@metadata)))) {
                  oceDebug(debug, "} # amsr [[\n", unindent=1)
                  return(x@metadata[[i]])
              }
              namesAllowed <- c(names(x@data), "SST", "LFwindDay", "MFwindDay", "vaporDay", "cloudDay", "rainDay")
              if (!(i %in% namesAllowed))
                  stop("band '", i, "' is not available in this object; try one of: ",
                       paste(namesAllowed, collapse=" "))
              #' get numeric band, changing land, n-obs, bad-obs, sea-ice and windy to NA
              getBand<-function(b) {
                  bad <- b == as.raw(0xff)| # land mass
                  b == as.raw(0xfe)| # no observations
                  b == as.raw(0xfd)| # bad observations
                  b == as.raw(0xfc)| # sea ice
                  b == as.raw(0xfb) # missing SST or wind due to rain, or missing water vapour due to heavy rain
                  b <- as.numeric(b)
                  b[bad] <- NA
                  b
              }
              dim <- c(length(x@metadata$longitude), length(x@metadata$latitude))
              if (missing(j) || j != "raw") {
                  ## Apply units; see http://www.remss.com/missions/amsre
                  ## FIXME: the table at above link has two factors for time; I've no idea
                  ## what that means, and am extracting what seems to be seconds in the day.
                  if      (i == "timeDay") res <- 60*6*getBand(x@data[[i]]) # FIXME: guessing on amsr time units
                  else if (i == "timeNight") res <- 60*6*getBand(x@data[[i]]) # FIXME: guessing on amsr time units
                  else if (i == "time") res <- 60*6*getBand(.Call("amsr_average", x@data[["timeDay"]], x@data[["timeNight"]]))
                  else if (i == "SSTDay") res <- -3 + 0.15 * getBand(x@data[[i]])
                  else if (i == "SSTNight") res <- -3 + 0.15 * getBand(x@data[[i]])
                  else if (i == "SST") res <- -3 + 0.15 * getBand(.Call("amsr_average", x@data[["SSTDay"]], x@data[["SSTNight"]]))
                  else if (i == "LFwindDay") res <- 0.2 * getBand(x@data[[i]])
                  else if (i == "LFwindNight") res <- 0.2 * getBand(x@data[[i]])
                  else if (i == "LFwind") res <- 0.2 * getBand(.Call("amsr_average", x@data[["LFwindDay"]], x@data[["LFwindNight"]]))
                  else if (i == "MFwindDay") res <- 0.2 * getBand(x@data[[i]])
                  else if (i == "MFwindNight") res <- 0.2 * getBand(x@data[[i]])
                  else if (i == "MFwind") res <- 0.2 * getBand(.Call("amsr_average", x@data[["MFwindDay"]], x@data[["MFwindNight"]]))
                  else if (i == "vaporDay") res <- 0.3 * getBand(x@data[[i]])
                  else if (i == "vaporNight") res <- 0.3 * getBand(x@data[[i]])
                  else if (i == "vapor") res <- 0.3 * getBand(.Call("amsr_average", x@data[["vaporDay"]], x@data[["vaporNight"]]))
                  else if (i == "cloudDay") res <- -0.05 + 0.01 * getBand(x@data[[i]])
                  else if (i == "cloudNight") res <- -0.05 + 0.01 * getBand(x@data[[i]])
                  else if (i == "cloud") res <- -0.05 + 0.01 * getBand(.Call("amsr_average", x@data[["cloudDay"]], x@data[["cloudNight"]]))
                  else if (i == "rainDay") res <- 0.01 * getBand(x@data[[i]])
                  else if (i == "rainNight") res <- 0.01 * getBand(x@data[[i]])
                  else if (i == "rain") res <- 0.01 * getBand(.Call("amsr_average", x@data[["rainDay"]], x@data[["rainNight"]]))
              } else {
                  if      (i == "timeDay") res <- x@data[[i]]
                  else if (i == "timeNight") res <- x@data[[i]]
                  else if (i == "time") res <- getBand(.Call("amsr_average", x@data[["timeDay"]], x@data[["timeNight"]]))
                  else if (i == "SSTDay") res <- x@data[[i]]
                  else if (i == "SSTNight") res <- x@data[[i]]
                  else if (i == "SST") res <- .Call("amsr_average", x@data[["SSTDay"]], x@data[["SSTNight"]])
                  else if (i == "LFwindDay") res <- x@data[[i]]
                  else if (i == "LFwindNight") res <- x@data[[i]]
                  else if (i == "LFwind") res <- .Call("amsr_average", x@data[["LFwindDay"]], x@data[["LFwindNight"]])
                  else if (i == "MFwindDay") res <- x@data[[i]]
                  else if (i == "MFwindNight") res <- x@data[[i]]
                  else if (i == "MFwind") res <- .Call("amsr_average", x@data[["MFwindDay"]], x@data[["MFwindNight"]])
                  else if (i == "vaporDay") res <- x@data[[i]]
                  else if (i == "vaporNight") res <- x@data[[i]]
                  else if (i == "vapor") res <- .Call("amsr_average", x@data[["vaporDay"]], x@data[["vaporNight"]])
                  else if (i == "cloudDay") res <- x@data[[i]]
                  else if (i == "cloudNight") res <- x@data[[i]]
                  else if (i == "cloud") res <- .Call("amsr_average", x@data[["cloudDay"]], x@data[["cloudNight"]])
                  else if (i == "rainDay") res <- x@data[[i]]
                  else if (i == "rainNight") res <- x@data[[i]]
                  else if (i == "rain") res <- .Call("amsr_average", x@data[["rainDay"]], x@data[["rainNight"]])
              }
              dim(res) <- dim
              res
          })

#' @title Replace Parts of an AMSR Object
#' @param x An \code{amsr} object, i.e. inheriting from \code{\link{amsr-class}}
#' @family things related to \code{amsr} data
#' @template sub_subsetTemplate
setMethod(f="[[<-",
          signature(x="amsr", i="ANY", j="ANY"),
          definition=function(x, i, j, value) {
              callNextMethod(x=x, i=i, j=j, value=value)
          })

#' Subset an amsr Object
#'
#' @description
#' This function is somewhat analogous to
#' \code{\link{subset.data.frame}}, but only one independent variable may be
#' used in \code{subset} in any call to the function, which means that
#' repeated calls will be necessary to subset based on more than one
#' independent variable (e.g. latitude and longitude).
#'
#' @param x A \code{amsr} object, i.e. one inheriting from \code{\link{amsr-class}}.
#' @param subset An expression indicating how to subset \code{x}.
#' @param ... Ignored.
#' @return An \code{amsr} object.
#' @examples
#' \dontrun{
#' library(oce)
#' earth <- read.amsr("f34_20160102v7.2.gz") # not provided with oce
#' fclat <- subset(earth , 45<=latitude & latitude <= 49)
#' fc <- subset(fclat , longitude <= -47 & longitude <= -43)
#' plot(fc)
#' }
#' @author Dan Kelley
#'
#' @family things related to \code{amsr} data
setMethod(f="subset",
          signature="amsr",
          definition=function(x, subset, ...) {
              res <- x
              subsetString <- paste(deparse(substitute(subset)), collapse=" ")
              if (length(grep("longitude", subsetString))) {
                  if (length(grep("latitude", subsetString)))
                      stop("the subset must not contain both longitude and latitude. Call this twice, to combine these")
                  keep <- eval(substitute(subset),
                               envir=data.frame(longitude=x@metadata$longitude))
                  for (name in names(res@data))
                      res@data[[name]] <- res@data[[name]][keep,]
                  res@metadata$longitude <- x@metadata$longitude[keep]
              } else if (length(grep("latitude", subsetString))) {
                  keep <- eval(substitute(subset),
                               envir=data.frame(latitude=x@metadata$latitude))
                  for (name in names(res@data))
                      res@data[[name]] <- res@data[[name]][,keep]
                  res@metadata$latitude <- res@metadata$latitude[keep]
              } else {
                  stop("may only subset by longitude or latitude")
              }
              res@processingLog <- processingLogAppend(res@processingLog, paste("subset(x, subset=", subsetString, ")", sep=""))
              res
          })
 

#' Plot an amsr Object
#'
#' @param x An object inherting from \code{\link{amsr-class}}.
#' @param y String indicating the name of the band to plot; if not provided,
#' \code{SST} is used; see \code{\link{amsr-class}} for a list of bands.
#' @param asp Optional aspect ratio for plot.
#'
#' @param missingColor List of colours for problem cases. The names of the
#' elements in this list must be as in the default, but the colours may
#' be changed to any desired values. These default values work reasonably
#' well for SST images, which are the default image, and which employ a
#' blue-white-red blend of colours, no mixture of which matches the
#' default values in \code{missingColor}.
#'
#' @param debug A debugging flag, integer.
#'
#' @param ... extra arguments passed to \code{\link{imagep}}, e.g. set
#' \code{col} to control colours.
#'
#' @concept satellite
#'
#' @examples
#' \dontrun{
#' d <- read.amsr("f34_20160102v7.2.gz")
#' asp <- 1/cos(pi*40/180)
#' plot(d, "SST", col=oceColorsJet, xlim=c(-80,0), ylim=c(20,60), asp=asp)
#' data(coastlineWorldMedium, package="ocedata")
#' lines(coastlineWorldMedium[['longitude']], coastlineWorldMedium[['latitude']])
#' }
#'
#' @author Dan Kelley
#'
#' @family functions that plot \code{oce} data
#' @family things related to \code{amsr} data
setMethod(f="plot",
          signature=signature("amsr"),
          ## FIXME: how to let it default on band??
          definition=function(x, y, asp,
                              missingColor=list(land='papayawhip',none='gray',bad='orange',ice='plum',rain='mediumseagreen'),
                              debug=getOption("oceDebug"), ...)
          {
              oceDebug(debug, "plot.amsr(..., y=c(",
                       if (missing(y)) "(missing)" else y, ", ...) {\n", sep="", unindent=1)
              if (missing(y))
                  y <- "SST"
              lon <- x[["longitude"]]
              lat <- x[["latitude"]]
              if ("ylim" %in% names(list(...))) {
                  if (missing(asp)) asp <- 1/cos(pi/180*abs(mean(list(...)$ylim)))
              } else {
                  if (missing(asp)) asp <- 1/cos(pi/180*abs(mean(lat, na.rm=TRUE)))
              }
              z <- x[[y]]
              i <- if ("zlab" %in% names(list(...))) imagep(lon, lat, z, asp=asp, ...)
                  else imagep(lon, lat, z, zlab=y, asp=asp, ...)
              ## Handle missing-data codes by redrawing the (decimate) image.
              ## Perhaps imagep() should be able to do this, but imagep() is a
              ## long function with a lot of interlocking arguments so I'll
              ## start by doing this manually here, and, if I like it, I'll
              ## extend imagep() later. Note that I added a new element of the
              ## return value of imagep(), to get the decimation factor.
              missingColorLength <- length(missingColor)
              if (5 != missingColorLength) {
                  stop("must have 5 elements in the missingColor argument")
              }
              if (!all(sort(names(missingColor))==sort(c("land","none","bad","ice","rain"))))
                  stop("missingColor names must be: 'land', 'none', 'bad', 'ice' and 'rain'")
              lonDecIndices <- seq(1L, length(lon), by=i$decimate[1])
              latDecIndices <- seq(1L, length(lat), by=i$decimate[2])
              lon <- lon[lonDecIndices]
              lat <- lat[latDecIndices]
              codes <- list(land=as.raw(255), # land
                            none=as.raw(254), # missing data
                            bad=as.raw(253), # bad observation
                            ice=as.raw(252), # sea ice
                            rain=as.raw(251)) # heavy rain
              for (codeName in names(codes)) {
                  bad <- x[[y, "raw"]][lonDecIndices, latDecIndices] == as.raw(codes[[codeName]])
                  image(lon, lat, bad,
                        col=c("transparent", missingColor[[codeName]]), add=TRUE)
                  ##message("did code ", codes[[codeName]], " (colour ", missingColor[[codeName]], ")")
              }
              box()
              oceDebug(debug, "} # plot.amsr()\n", unindent=1)
          })


#' Read an amsr File
#'
#' Read a compressed amsr file, generating an object that inherits from
#' \code{\link{amsr-class}}.  Note that only compressed files are read in
#' this version.
#'
#' @section File sources:
#' AMSR files are provided at the FTP site
#' \code{ftp://ftp.ssmi.com/amsr2/bmaps_v07.2/} and login as "guest",
#' enter a year-based directory (e.g. \code{y2016} for the year 2016),
#' then enter a month-based directory (e.g. \code{m08} for August, the 8th
#' month), and then download a file for the present date, e.g.
#' \code{f34_20160803v7.2.gz} for August 3rd, 2016. Do not uncompress
#' this file, since \code{read.amsr} can only read uncompressed files.
#' If \code{read.amsr} reports an error on the number of chunks, try
#' downloading a similarly-named file (e.g. in the present example,
#' \code{read.amsr("f34_20160803v7.2_d3d.gz")} will report an error
#' about inability to read a 6-chunk file, but 
#' \code{read.amsr("f34_20160803v7.2.gz")} will work properly.
#'
#' @param file String indicating the name of a compressed file. See
#' \dQuote{File sources}.
#'
#' @param debug A debugging flag, integer.
#'
#' @concept satellite
#' @seealso \code{\link{plot,amsr-method}} for an example.
#' @author Dan Kelley and Chantelle Layton
#'
#' @family things related to \code{amsr} data
read.amsr <- function(file, debug=getOption("oceDebug"))
{
    oceDebug(debug, "read.amsr(file=\"", file, "\",",
             #if (length(band) > 1) paste("band=c(\"", paste(band, collapse="\",\""), "\")", sep="") else
                 ", debug=", debug, ") {\n", sep="", unindent=1)
    res <- new("amsr")
    filename <- file
    res@metadata$filename <- filename
    file <- if (length(grep(".*.gz$", filename))) gzfile(filename, "rb") else file(filename, "rb")
    on.exit(close(file))
    ## we can hard-code a max size because the satellite data size is not variable
    buf <- readBin(file, what="raw", n=50e6, endian="little")
    nbuf <- length(buf)
    dim <- c(1440, 720)
    nchunks <- nbuf / prod(dim)
    if (nchunks != round(nchunks)) 
        stop("error: the data length ", nbuf, " is not an integral multiple of ", dim[1], "*", dim[2])
    ## From an amsr webpage --
    ## Each binary data file available from our ftp site consists of fourteen (daily) or
    ## six (averaged) 0.25 x 0.25 degree grid (1440,720) byte maps. For daily files,
    ## seven daytime, ascending maps in the following order, Time (UTC), Sea Surface
    ## Temperature (SST), 10 meter Surface Wind Speed (WSPD-LF), 10 meter Surface
    ## Wind Speed (WSPD-MF), Atmospheric Water Vapor (VAPOR), Cloud Liquid Water (CLOUD),
    ## and Rain Rate (RAIN), are followed by seven nighttime maps in the same order.
    ## Time-Averaged files contain just the geophysical layers in the same order
    ## [SST, WSPD-LF, WSPD-MF,VAPOR, CLOUD, RAIN].
    select <- seq.int(1L, prod(dim))
    if (nchunks == 14) {
        oceDebug(debug, "14-chunk amsr file\n")
        timeDay <- buf[select]
        SSTDay <- buf[prod(dim) + select]
        LFwindDay <- buf[2*prod(dim) + select]
        MFwindDay <- buf[3*prod(dim) + select]
        vaporDay <- buf[4*prod(dim) + select]
        cloudDay <- buf[5*prod(dim) + select]
        rainDay <- buf[6*prod(dim) + select]
        dim(timeDay) <- dim
        dim(SSTDay) <- dim
        dim(LFwindDay) <- dim
        dim(MFwindDay) <- dim
        dim(vaporDay) <- dim
        dim(cloudDay) <- dim
        dim(rainDay) <- dim

        timeNight <- buf[7*prod(dim) + select]
        SSTNight <- buf[8*prod(dim) + select]
        LFwindNight <- buf[9*prod(dim) + select]
        MFwindNight <- buf[10*prod(dim) + select]
        vaporNight <- buf[11*prod(dim) + select]
        cloudNight <- buf[12*prod(dim) + select]
        rainNight <- buf[13*prod(dim) + select]
        dim(timeNight) <- dim
        dim(SSTNight) <- dim
        dim(LFwindNight) <- dim
        dim(MFwindNight) <- dim
        dim(vaporNight) <- dim
        dim(cloudNight) <- dim
        dim(rainNight) <- dim
        res@metadata$units$SSTDay <- list(unit=expression(degree*C), scale="ITS-90")
        res@metadata$units$SSTNight <- list(unit=expression(degree*C), scale="ITS-90")
        res@metadata$units$LFwindDay <- list(unit=expression(m/s), scale="")
        res@metadata$units$LFwindNight <- list(unit=expression(m/s), scale="")
        res@metadata$units$MFwindDay <- list(unit=expression(m/s), scale="")
        res@metadata$units$MFwindNight <- list(unit=expression(m/s), scale="")
        res@metadata$units$rainDay <- list(unit=expression(mm/h), scale="")
        res@metadata$units$rainNight <- list(unit=expression(mm/h), scale="")

        res@data <- list(timeDay=timeDay,
                         SSTDay=SSTDay, LFwindDay=LFwindDay, MFwindDay=MFwindDay,
                         vaporDay=vaporDay, cloudDay=cloudDay, rainDay=rainDay,
                         timeNight=timeNight,
                         SSTNight=SSTNight, LFwindNight=LFwindNight, MFwindNight=MFwindNight,
                         vaporNight=vaporNight, cloudNight=cloudNight, rainNight=rainNight)
        res@metadata$longitude  <- 0.25 * 1:dim[1] - 0.125
        res@metadata$latitude <- 0.25 * 1:dim[2] - 90.125
        ## rearrange matrices so that Greenwich is near the centre
        for (name in names(res@data)) {
            t <- matrixShiftLongitude(res@data[[name]], res@metadata$longitude)
            res@data[[name]] <- t$m
        }
        res@metadata$longitude <- t$longitude
    } else if (nchunks == 6) {
        stop("Cannot (yet) read 6-chunk data. Please contact the developers if you need this file (and be sure to send the file to them).")
    } else {
        stop("Can only handle 14-chunk data; this file has ", nchunks, " chunks. Please contact the developers if you need to read this file (and be sure to send the file to them).")
    }
    res@metadata$spacecraft <- "amsr"
    res@processingLog <- processingLogAppend(res@processingLog,
                                        paste(deparse(match.call()), sep="", collapse=""))
    oceDebug(debug, "} # read.amsr()\n", unindent=1)
    res
}

#' @title Create a composite of amsr satellite data
#' @details
#' Form averages for each item in the \code{data} slot of the supplied objects,
#' taking into account the bad-data codes. If none of the objects has good
#' data at any particular pixel (i.e. particular latitude and longitude),
#' the resultant will have the bad-data code of the last item in the argument
#' list.
#' The metadata in the result are taken directly from the metadata of the
#' final argument, except that the filename is set to a comma-separated list
#' of the component filenames.
#'
#' @param object An object inheriting from \link{amsr-class}.
#' @param ... Other amsr objects.
#'
#' @family things related to \code{amsr} data
#' @template compositeTemplate
setMethod("composite",
          c(object="amsr"),
          function(object, ...) {
              dots <- list(...)
              ndots <- length(dots)
              if (ndots < 2)
                  stop("need more than one argument")
              for (idot in 1:ndots)
                  if (!inherits(dots[[idot]], "amsr")) stop("argument ", 1+idot, " does not inherit from 'amsr'")
              ## inherit most of the metadata from the last argument
              res <- dots[[ndots]]
              filenames <- object[["filename"]]
              for (idot in 1:ndots)
                  filenames <- paste(filenames, ",", dots[[idot]][["filename"]], sep="")
              n <- 1 + ndots
              dim <- c(dim(object@data[[1]]), n)
              for (name in names(object@data)) {
                  a <- array(as.raw(0xff), dim=dim)
                  ##message("A name='", name, "'")
                  a[,,1] <- object@data[[name]]
                  ##message("B")
                  for (idot in 1:ndots) {
                      ##message("C idot=", idot)
                      a[,,1+idot] <- dots[[idot]]@data[[name]]
                      ##message("D idot=", idot)
                  }
                  ##message("E")
                  A <- .Call("amsr_composite", a)
                  ##message("F")
                  res@data[[name]] <- A
              }
              res@metadata$filename <- filenames
              res
          })
