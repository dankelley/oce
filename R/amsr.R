## vim:textwidth=128:expandtab:shiftwidth=4:softtabstop=4

#' Class to hold AMSR data
#'
#' The bands are stored in \code{\link{raw}} form, to save storage. The accessor
#' function, \code{\link{plot.amsr}}, and \code{\link{summary.amsr}} all 
#' apply linear scaling to create values appropriate to the units of
#' the individual band.  The band names are \code{timeDay}, 
#' \code{SSTDay}, \code{LFwindDay}, \code{MFwindDay}, \code{vaporDay} (note
#' the U.S. spelling), \code{cloudDay}, and \code{rainDay}, along with
#' similarly-named items that end in \code{Night}.
#'
#' @author Dan Kelley
#' @concept satellite data
#' @references
#' \url{http://www.remss.com/missions/amsre} describes the data.
#' @aliases amsr-class
#' @seealso \code{\link{landsat-class}} for handling data from the Landsat-8 satellite.
setClass("amsr", contains="oce")

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


#' Summarize an amsr object
#'
#' Although the data are stored in \code{\link{raw}} form, the summary
#' presents results in physical units.
#'
#' @param object The object to be summarized.
#' @param ... Ignored.
#' @author Dan Kelley
#' @aliases summary.amsr
#' @concept satellite data
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

#' Extract something from an amsr object
#'
#' @param x An amsr object, i.e. one inheriting from \code{\link{amsr-class}}.
#' @param i The item to extract.
#' @param j Optional additional information on the \code{i} item (ignored).
#' @param ... Optional additional information (ignored).
#' @author Dan Kelley
#' @concept satellite data
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
              if (!(i %in% names(x@data)))
                  stop("band '", i, "' is not available in this object; try one of: ",
                       paste(names(x@data), collapse=" "))
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
              ## Apply units; see http://www.remss.com/missions/amsre
              ## FIXME: the table at above link has two factors for time; I've no idea
              ## what that means, and am extracting what seems to be seconds in the day.
              if      (i == "timeDay") res <- 60*6*getBand(x@data[[i]]) # seconds since midnight (?)
              else if (i == "timeNight") res <- 60*6*getBand(x@data[[i]]) # seconds since midnight (?)
              else if (i == "SSTDay") res <- -3 + 0.15 * getBand(x@data[[i]])
              else if (i == "SSTNight") res <- -3 + 0.15 * getBand(x@data[[i]])
              else if (i == "LFwindDay") res <- 0.2 * getBand(x@data[[i]])
              else if (i == "MFwindDay") res <- 0.2 * getBand(x@data[[i]])
              else if (i == "LFwindNight") res <- 0.2 * getBand(x@data[[i]])
              else if (i == "MFwindNight") res <- 0.2 * getBand(x@data[[i]])
              else if (i == "vaporDay") res <- 0.3 * getBand(x@data[[i]])
              else if (i == "vaporNight") res <- 0.3 * getBand(x@data[[i]])
              else if (i == "cloudDay") res <- -0.05 + 0.01 * getBand(x@data[[i]])
              else if (i == "cloudNight") res <- -0.05 + 0.01 * getBand(x@data[[i]])
              else if (i == "rainDay") res <- 0.01 * getBand(x@data[[i]])
              else if (i == "rainNight") res <- 0.01 * getBand(x@data[[i]])
              else stop("setting units for band '", i, "' is not coded yet -- ask developer for it!")
              dim(res) <- c(1440, 720)
              res
          })

#' Plot an amsr object
#'
#' @param x An object inherting from \code{\link{amsr-class}}.
#' @param y String indicating the name of the band to plot; if not provided, \code{SSTDay} is used..
#' @param col Color palette.
#' @param missingColor String indicating the colour to be used for missing data (land, clouds, etc)
#' @param debug A debugging flag, integer.
#' @param ... Ignored.
#'
#' @author Dan Kelley
#' @aliases plot.amsr
#' @concept satellite data
setMethod(f="plot",
          signature=signature("amsr"),
          ## FIXME: how to let it default on band??
          definition=function(x, y,
                              col=oce.colorsPalette,
                              missingColor="gray",
                              debug=getOption("oceDebug"), ...)
          {
              oceDebug(debug, "plot.amsr(..., y=c(",
                       if (missing(y)) "(missing)" else y, ", ...) {\n", sep="", unindent=1)
              if (missing(y))
                  y <- "SSTDay"
              imagep(x[["longitude"]], x[["latitude"]], x[[y]],
                     zlab=y, col=col, missingColor=missingColor)
              oceDebug(debug, "} # plot.amsr()\n", unindent=1)
          })


#' Read an amsr file
#'
#' Read a compressed amsr file, generating an object that inherits from
#' \code{\link{amsr-class}}.  Note that only compressed files are read in
#' this version
#'
#' @param file Sting indicating the name of a compressed file.
#' @param debug A debugging flag, integer.
#' @author Dan Kelley and Chantelle Layton
#' @concept satellite data
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
    } else if (nchunks == 6) {
        stop("Cannot (yet) read 6-chunk data. Contact developers if you need this.")
    } else {
        stop("Can only handle 14-chunk data.")
    }
    res@processingLog <- processingLogAppend(res@processingLog,
                                        paste(deparse(match.call()), sep="", collapse=""))
    oceDebug(debug, "} # read.amsr()\n", unindent=1)
    res
}

