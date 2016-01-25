## vim:textwidth=80:expandtab:shiftwidth=4:softtabstop=4

#' Class to hold satellite data
#'
#' This class holds satellite data of various types, including
#' \code{\link{amsr-class}} and \code{\link{g1sst-class}}.
#' @author Dan Kelley and Chantelle Layton
#' @concept satellite
setClass("satellite", contains="oce")

setMethod(f="initialize",
          signature="satellite",
          definition=function(.Object, filename, subclass) {
              if (!missing(filename))
                  .Object@metadata$filename <- filename
              .Object@processingLog$time <- as.POSIXct(Sys.time())
              .Object@processingLog$value <- if (missing(subclass))
                  "create 'satellite' object" else paste("create '", subclass, "' object")
              return(.Object)
          })

#' Summarize a satellite object
#'
#' @param object The object to be summarized.
#' @param ... Ignored.
#' @author Dan Kelley
#' @aliases summarize.satellite summarize.g1sst
#' @concept satellite
setMethod(f="summary",
          signature="satellite",
          definition=function(object, ...) {
              cat("Satellite Summary\n-----------------\n\n")
              showMetadataItem(object, "filename",   "Data file:           ")
              showMetadataItem(object, "satellite",  "Satellite:           ")
              lon <- object@metadata$longitude
              lat <- object@metadata$latitude
              if (length(lon) > 2) cat("* Longitude:           ", lon[1], ", ", lon[2],  ", ..., ", tail(lon, 1), "\n", sep="")
              else cat("* Longitude:           ", paste(lon, collapse=", "), "\n", sep="")
              if (length(lat) > 2) cat("* Latitude:            ", lat[1], ", ", lat[2],  ", ..., ", tail(lat, 1), "\n", sep="")
              else cat("* Latitude:            ", paste(lat, collapse=", "), "\n", sep="")
              cat("* Time:                ",
                  format(object@metadata$time, "%Y-%m-%d %H:%M:%S %z"), "\n", sep="")
              if (FALSE) { # FIXME: this is just for amsr, when we drop it's specific fcn
                  for (name in names(object@data))
                      object@data[[name]] <- object[[name]] # translate to science units
              }
              callNextMethod()
          })

#' Plot a satellite object
#'
#' For an example, see \code{\link{read.g1sst}}.
#'
#' @param x An object inherting from \code{\link{satellite-class}}.
#' @param y String indicating the quantity to be plotted.
#' @param asp Optional aspect ratio for plot.
#' @param debug A debugging flag, integer.
#' @param ... extra arguments passed to \code{\link{imagep}}, e.g. set
#' \code{col} to control colours.
#'
#' @author Dan Kelley
#' @aliases plot.satellite
#' @concept satellite
setMethod(f="plot",
          signature=signature("satellite"),
          definition=function(x, y, asp, debug=getOption("oceDebug"), ...)
          {
              oceDebug(debug, "plot.satellite(..., y=c(",
                       if (missing(y)) "(missing)" else y, ", ...) {\n", sep="", unindent=1)
              if (missing(y))
                  stop("must indicate what to plot")
              lon <- x[["longitude"]]
              lat <- x[["latitude"]]
              if (missing(asp)) asp <- 1/cos(pi/180*abs(mean(lat, na.rm=TRUE)))
              if ("zlab" %in% names(list(...))) {
                  imagep(lon, lat, x[[y]], asp=asp, ...)
              } else {
                  imagep(lon, lat, x[[y]], asp=asp, zlab=y, ...)
              }
              oceDebug(debug, "} # plot.satellite()\n", unindent=1)
          })

## g1sst subclass

#' Class to hold G1SST satellite-model data
#'
#' G1SST is an acronym for global 1-km sea surface temperature, a product
#' that combines satellite data with the model output. It is provided by
#' the JPO ROMS (Regional Ocean Modelling System) modelling group.
#' See the JPL website [1] to learn more about the data, and see
#' the \code{\link{read.g1sst}} documentation for an example
#' of downloading and plotting.
#'
#' It is important not to regard G1SST data in the same category as,
#' say, \code{\link{amsr-class}} data, because the two products
#' differ greatly with respect to cloud cover. The satellite used by
#' \code{\link{amsr-class}} has the ability to sense water temperature
#' even if there is cloud cover, whereas \code{g1sst} fills in cloud
#' gaps with model simulations.  It can be helpful to consult 
#' [1] for a given time, clicking and then unclicking the radio button
#' that turns off the model-based filling of cloud gaps.
#'
#' @author Dan Kelley
#' @concept satellite
#' @references
#' 1. JPO OurOcean Portal \url{http://ourocean.jpl.nasa.gov/SST/}
setClass("g1sst", contains="satellite")
 
#' Read G1SST satellite data
#'
#' This works with netcdf files as provided by the ERDAPP server [1].
#'
#' As noted in the documentation for \code{\link{g1sst-class}}, one
#' must be aware of the incorporation of model simulations in the 
#' \code{g1sst} product. For example, the code presented below
#' might lead one to believe that the mapped field represents
#' observatins, whereas in fact it can be verified by
#' consulting [2] (clicking and unclicking the radio button to
#' show just the data) that the field mostly derives from simulation.
#'
#' @param filename name of a netcdf file containing G1SST data.
#'
#' @return An object of \code{\link{g1sst-class}}.
#' @examples
#' \dontrun{
#' # Construct query, making it easier to understand and modify.
#' day <- "2016-01-02"
#' lon0 <- -67.5
#' lon1 <- -63.5
#' lat0 <- 44
#' lat1 <- 46
#' source <- paste("http://coastwatch.pfeg.noaa.gov/erddap/griddap/",
#'                 "jplG1SST.nc?",
#'                 "SST%5B(", day, "T12:00:00Z)",
#'                 "%5D%5B(", lat0, "):(", lat1, ")",
#'                 "%5D%5B(", lon0, "):(", lon1, ")",
#'                 "%5D", sep="")
#' if (!length(list.files(pattern="^a.nc$")))
#'     download.file(source, "a.nc")
#' d <- read.g1sst("a.nc")
#' plot(d, "SST", col=oceColorsJet)
#' data(coastlineWorldFine, package="ocedata")
#' lines(coastlineWorldFine[['longitude']],coastlineWorldFine[['latitude']])
#' }
#'
#' @references
#' 1. ERDDAP Portal \url{http://coastwatch.pfeg.noaa.gov/erddap/}
#' 2. JPO OurOcean Portal \url{http://ourocean.jpl.nasa.gov/SST/}
#' @concept satellite
#' @author Dan Kelley
read.g1sst <- function(filename)
{
    if (!requireNamespace("ncdf4", quietly=TRUE))
        stop('must install.packages("ncdf4") to read argo data')
    f <- ncdf4::nc_open("a.nc")
    res <- new("g1sst", filename=filename)
    res@metadata$longitude <- ncdf4::ncvar_get(f, "longitude")
    res@metadata$latitude <- ncdf4::ncvar_get(f, "latitude")
    res@metadata$time <- numberAsPOSIXct(ncdf4::ncvar_get(f, "time"))
    res@metadata$units$SST <- list(unit=expression(degree*C), scale="ITS-90")
    res@metadata$satellite <- "G1SST"
    res@data$SST <- ncdf4::ncvar_get(f, "SST")
    res
}

