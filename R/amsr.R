## vim:textwidth=80:expandtab:shiftwidth=4:softtabstop=4

setClass("satellite", contains="oce")

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
#' form or in physical units; \code{\link{plot.amsr}}, and
#' \code{\link{summary.amsr}} work with physical units.
#'
#' @author Dan Kelley and Chantelle Layton
#' @concept satellite
#' @family functions that handle amsr data
#' @family functions that handle satellite data
#' @references
#' 1. \url{http://www.remss.com/missions/amsre}
#' @aliases amsr-class
#' @seealso \code{\link{landsat-class}} for handling data from the Landsat-8 satellite.
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


#' Summarize an amsr Object
#'
#' Although the data are stored in \code{\link{raw}} form, the summary
#' presents results in physical units.
#'
#' @param object The object to be summarized.
#' @param ... Ignored.
#' @author Dan Kelley
#' @aliases summary.amsr
#' @concept satellite
#' @family functions that handle amsr data
#' @family functions that handle satellite data
setMethod(f="summary",
          signature="amsr",
          definition=function(object, ...) {
              cat("Amsr Summary\n------------\n\n")
              showMetadataItem(object, "filename",   "Data file:           ")
              for (name in names(object@data)) {
                  object@data[[name]] <- object[[name]] # translate to science units
              }
              callNextMethod()
          })

#' Extract Something From an amsr Object
#'
#' Extract something from the \code{metadata} or \code{data} slot of an
#' \code{\link{amsr-class}} object.

#' @details
#' Partial matches for \code{i}
#' are permitted for \code{metadata}, and \code{j} is ignored.
#'
#' Data within the \code{data} slot must be matched exactly by name,
#' and may be retrieved with units (the default) or as raw bytes (if
#' \code{j="raw"}.)  The available items are:
#' seconds from the start of day (\code{time}),
#' temperature in degC (\code{SST});
#' wind speed in m/s (\code{LFwind} and \code{MFwindDay});
#' water vapor content (\code{vaporDay});
#' cloudiness (\code{cloud}),
#' and rainfall in mm/h (\code{rain}).  Each of these is
#' an average across day-time and night-time passes; to get
#' the day/night data separately, use e.g. \code{SSTDay}
#' \code{SSTNight}, and similarly-named versions of all
#' bands.
#'
#' @return
#' In all cases, the returned value is a matrix with 
#' with dimension 1440 by 720, with \code{NA} values if the 
#' satellite data are over land (coded to \code{0x255}),
#' have no observations (coded to \code{0xfe}),
#' are bad observations (coded to \code{0xfd}),
#' indicate sea ice (coded to \code{0xfc}),
#' are are faulty owing to high rain (coded to \code{0xfb}).
#'
#' @param x An amsr object, i.e. one inheriting from \code{\link{amsr-class}}.
#' @param i The item to extract; see \dQuote{Details}
#' @param j Optional additional information on the \code{i} item (ignored).
#' @param ... Optional additional information (ignored).
#' @author Dan Kelley
#' @concept satellite
#' @family functions that handle amsr data
#' @family functions that handle satellite data
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
              getBand <- function(b) {
                  bad <- b == as.raw(0xff)| # land mass
                  b == as.raw(0xfe)| # no observations
                  b == as.raw(0xfd)| # bad observations
                  b == as.raw(0xfc)| # sea ice
                  b == as.raw(0xfb) # missing SST or wind due to rain, or missing water vapour due to heavy rain
                  b <- as.numeric(b)
                  b[bad] <- NA
                  dim(b) <- c(1440, 720)
                  b
              }
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
              res
          })

#' Plot an amsr Object
#'
#' @param x An object inherting from \code{\link{amsr-class}}.
#' @param y String indicating the name of the band to plot; if not provided,
#' \code{SST} is used; see \code{\link{amsr-class}} for a list of bands.
#' @param asp Optional aspect ratio for plot.
#' @param debug A debugging flag, integer.
#' @param ... extra arguments passed to \code{\link{imagep}}, e.g. set
#' \code{col} to control colours.
#'
#' @aliases plot.amsr
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
#' @family functions that handle amsr data
#' @family functions that handle satellite data
#' @family functions that plot oce data
setMethod(f="plot",
          signature=signature("amsr"),
          ## FIXME: how to let it default on band??
          definition=function(x, y, asp, debug=getOption("oceDebug"), ...)
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
              if ("zlab" %in% names(list(...))) imagep(lon, lat, x[[y]], asp=asp, ...)
              else imagep(lon, lat, x[[y]], zlab=y, asp=asp, ...)
              oceDebug(debug, "} # plot.amsr()\n", unindent=1)
          })


#' Read an amsr File
#'
#' Read a compressed amsr file, generating an object that inherits from
#' \code{\link{amsr-class}}.  Note that only compressed files are read in
#' this version
#'
#' @param file Sting indicating the name of a compressed file.
#' @param debug A debugging flag, integer.
#' @author Dan Kelley and Chantelle Layton
#' @concept satellite
#' @seealso \code{\link{plot.amsr}} for an example.
#' @family functions that handle amsr data
#' @family functions that handle satellite data
#' @examples
#' \dontrun{
#' d <- read.amsr("f34_20160102v7.2.gz")
#' summary(d)
#' plot(d, "SST", col=oceColorsJet, xlim=c(-80,0), ylim=c(20,60), asp=asp)
#' }
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
        stop("Cannot (yet) read 6-chunk data. Contact developers if you need this.")
    } else {
        stop("Can only handle 14-chunk data.")
    }
    res@metadata$satellite <- "amsr"
    res@processingLog <- processingLogAppend(res@processingLog,
                                        paste(deparse(match.call()), sep="", collapse=""))
    oceDebug(debug, "} # read.amsr()\n", unindent=1)
    res
}

