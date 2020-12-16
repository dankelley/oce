## vim:textwidth=128:expandtab:shiftwidth=4:softtabstop=4


#' Class to Store Topographic Data
#'
#' This class stores topographic data, as read with
#' [read.topo()] or assembled with [as.topo()].
#' Plotting is handled with [plot,topo-method()]
#' and summaries with [summary,topo-method()].
#'
#' @templateVar class topo
#'
#' @templateVar dataExample The key items stored in this slot are: `longititude`, `latitude`, and `z`.
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
#'
#' @family things related to topo data
setClass("topo", contains="oce")

#' @title Global Topographic Dataset at Half-degree Resolution
#'
#' @description
#' Global topographic dataset at half-degree resolution, downloaded from
#' a NOAA server on May 18, 2019.  Longitude, accessible as
#' `topoWorld[["longitude"]]`, ranges from -179.75 to 129.75 degrees north.
#' Latitude (`topoWorld[["latitude"]]`) ranges from -89.75 to 89.75 degrees east.
#' Height (`topoWorld[["z"]]`) is measured in metres above nominal sea level.
#'
#' The coarse resolution can be a problem in plotting depth contours along with
#' coastlines in regions of steep topography. For example, near the southeast
#' corner of Newfoundland, a 200m contour will overlap a coastline drawn with
#' `coastlineWorldFine` from the \CRANpkg{ocedata} package. The solution in such cases is to
#' download a higher-resolution topography file, perhaps using
#' [download.topo()], and then use [read.topo()]
#' to create another `topo` object.  (With other data
#' sources, [as.topo()] may be helpful.)
#'
#' @section Historical note:
#' From late 2009 until May 18, 2019, the `topoWorld` dataset was created
#' with a fairly complicated code that read a binary file downloaded from NOAA
#' (\samp{http://www.ngdc.noaa.gov/mgg/global/relief/ETOPO5/TOPO/ETOPO5}),
#' decoded, decimated from 1/12th degree resolution to 1/2 degree resolution, and
#' passed through [matrixShiftLongitude()] to put longitude
#' between -180 and 180 degrees. The new scheme for creating the dataset,
#' (see \dQuote{Source}) is much simpler, and also a much better model
#' of how users are likely to deal with topography files in the more
#' modern netCDF format. Note that the new version differs from the old one
#' in longitude and latitude being shifted by 1/4 degree,
#' and by a mean  elevation difference of under 10m. The old and new
#' versions appear identical when plotted at the global scale that is
#' the recommended for such a coarse topographic file.
#'
#' @name topoWorld
#' @docType data
#'
#' @usage data(topoWorld)
#'
#' @source
#' This is created with [read.topo()], using a file downloaded with
#'\preformatted{
#'topoFile <- download.topo(west=-180, east=180, south=-90, north=90,
#'                          resolution=30, destdir=".")
#'}
#'
#' @examples
#'\dontrun{
#' library(oce)
#' data(topoWorld)
#' par(mfrow=c(2, 1))
#' plot(topoWorld, location=NULL)
#' imagep(topoWorld)
#'}
#'
#' @family datasets provided with oce
#' @family things related to topo data
NULL

#' High-resolution Topographic Dataset for Nova Scotia
#'
#' One-minute (under 2km) dataset in a region including Nova Scotia, Prince
#' Edward Island and part of New Brunswick, Canada.
#'
#' @name topoNS
#' @docType data
#'
#' @usage data(topoNS)
#'
#' @source
#' This is created with [read.topo()], using a file downloaded with
#' [download.topo]`(-67,-59.5,43.3,47.2,resolution=1)`.
#'
#' @examples
#'\donttest{
#' library(oce)
#' data(coastlineWorldFine, package="ocedata")
#' data(topoNS)
#' # 1. Contour plot (with no legend)
#' plot(topoNS, location="none")
#' # 2. Image plot (note that whitespace can occur if plot does not match aspect ratio)
#' H <- quantile(abs(topoNS[["z"]]), 0.98)
#' cm <- colormap(H*c(-1, 1),
#'                col=function(n) oceColorsGebco(region="both",n=n))
#' imagep(topoNS, colormap=cm, xlab="", ylab="")
#' lines(coastlineWorldFine[["longitude"]], coastlineWorldFine[["latitude"]])
#'}
#'
#' @family datasets provided with oce
#' @family things related to topo data
NULL


setMethod(f="initialize",
          signature="topo",
          definition=function(.Object, longitude, latitude, z, filename="", units, ...) {
              .Object <- callNextMethod(.Object, ...)
              if (!missing(longitude)) .Object@data$longitude <- longitude
              if (!missing(latitude)) .Object@data$latitude <- latitude
              if (!missing(z)) .Object@data$z <- z
              if (!missing(units)) .Object@metadata$units <- units
              .Object@metadata$filename <- filename
              .Object@processingLog$time <- presentTime()
              .Object@processingLog$value <- "create 'topo' object"
              return(.Object)
          })


#' @title Summarize A Topo Object
#'
#' @description
#' Pertinent summary information is presented, including the longitude and
#' latitude range, and the range of elevation.
#'
#' @param object A [topo-class] object.
#'
#' @param \dots Further arguments passed to or from other methods.
#'
#' @return A matrix containing statistics of the elements of the `data` slot.
#'
#' @examples
#' library(oce)
#' data(topoWorld)
#' summary(topoWorld)
#'
#' @author Dan Kelley
#'
#' @family things related to topo data
setMethod(f="summary",
          signature="topo",
          definition=function(object, ...) {
              cat("\nTopo dataset\n------------\n")
              cat("* Source:          ", object[["filename"]], "\n")
              invisible(callNextMethod()) # summary
          })

#' @title Extract Something From a Topo Object
#'
#' @param x a [topo-class] object.
#'
#' @examples
#' data(topoWorld)
#' dim(topoWorld[['z']])
#'
#' @section Details of the specialized topo method:
#' There are no special features for [topo-class] data;
#' the general method is used directly.
#' @template sub_subTemplate
#' @family things related to topo data
setMethod(f="[[",
          signature(x="topo", i="ANY", j="ANY"),
          definition=function(x, i, j, ...) {
              callNextMethod()         # [[
          })

#' @title Replace Parts of a Topo Object
#'
#' @param x a [topo-class] object.
#'
#' @family things related to topo data
#'
#' @template sub_subsetTemplate
setMethod(f="[[<-",
          signature(x="topo", i="ANY", j="ANY"),
          definition=function(x, i, j, ..., value) {
              callNextMethod(x=x, i=i, j=j, ..., value=value) # [[<-
          })

#' @title Subset a Topo Object
#'
#' @description
#' This function is somewhat analogous to [subset.data.frame()].
#' Subsetting can be by `time` or `distance`, but these may not be
#' combined; use a sequence of calls to subset by both.
#'
#' @param x a [topo-class] object.
#'
#' @param subset A condition to be applied to the `data` portion of `x`.
#' See \sQuote{Details}.
#'
#' @param ... Ignored.
#'
#' @return A new [topo-class] object.
#'
#' @examples
#' ## northern hemisphere
#' library(oce)
#' data(topoWorld)
#' plot(subset(topoWorld, latitude > 0))
#'
#' @author Dan Kelley
#'
#' @family things related to topo data
#' @family functions that subset oce objects
setMethod(f="subset",
          signature="topo",
          definition=function(x, subset, ...) {
              subsetString <- paste(deparse(substitute(expr=subset, env=environment())), collapse=" ")
              res <- x
              dots <- list(...)
              debug <- getOption("oceDebug")
              if (length(dots) && ("debug" %in% names(dots)))
                  debug <- dots$debug
              if (missing(subset))
                  stop("must give 'subset'")
              if (length(grep("longitude", subsetString))) {
                  oceDebug(debug, "subsetting a topo object by longitude\n")
                  keep <- eval(expr=substitute(expr=subset, env=environment()), envir=x@data, enclos=parent.frame(2))
                  oceDebug(debug, "keeping", 100*sum(keep)/length(keep), "% of longitudes\n")
                  res[["longitude"]] <- x[["longitude"]][keep]
                  res[["z"]] <- x[["z"]][keep, ]
              } else if (length(grep("latitude", subsetString))) {
                  oceDebug(debug, "subsetting a topo object by latitude\n")
                  keep <- eval(expr=substitute(expr=subset, env=environment()), envir=x@data, enclos=parent.frame(2))
                  oceDebug(debug, "keeping", 100*sum(keep)/length(keep), "% of latitudes\n")
                  res[["latitude"]] <- x[["latitude"]][keep]
                  res[["z"]] <- x[["z"]][, keep]
              } else {
                  stop("the subset must be based on longitude or latitude")
              }
              res@processingLog <- processingLogAppend(res@processingLog, paste("subset.topo(x, subset=", subsetString, ")", sep=""))
              res
          })


#' Download and Cache a topo File
#'
#' Topographic data are downloaded from a data server that holds the ETOPO1
#' dataset (Amante, C. and B.W. Eakins, 2009), and saved as a netCDF file whose
#' name specifies the data request, if a file of that name is not already
#' present on the local file system.  The return value is the name of the data
#' file, and it's typical use is as the filename for a call to [read.topo()].
#' Given the rules on file naming, subsequent calls to `download.topo()`
#' with identical parameters will simply return the name of the cached file,
#' assuming the user has not deleted it in the meantime.
#'
#' For years, the server used by `download.topo()` was capable of
#' returning netCDF files, but this was found to fail in late May of 2020.
#' Luckily, the [marmap::getNOAA.bathy()] function in the \CRANpkg{marmap}
#' package was updated to handle the changes in the server, and
#' `download.topo()` was able to pattern its URL construction on that function.
#' The server also now has an API, which might suggest it will be less subject
#' to change in the future.
#'
#' @param west,east Longitudes of the western and eastern sides of the box.
#'
#' @param south,north Latitudes of the southern and northern sides of the box.
#'
#' @param resolution numeric value of grid spacing, in geographical minutes.
#' The default value of 4 minutes corresponds to 4 nautical miles, or 7.4km.
#'
#' @template downloadDestTemplate
#'
#' @param format Deprecated, and ignored, as of June 2020.
#'
#' @param server character value specifying the base from which a
#' download URL will be constructed.  It is unlikely that any value
#' other than the default will work, unless it is a similarly-constructed
#' mirrored site.
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
#' topoFile <- download.topo(west=-66, east=-60, south=43, north=47,
#'                           resolution=1, destdir="~/data/topo")
#' topo <- read.topo(topoFile)
#' imagep(topo, zlim=c(-400, 400), drawTriangles=TRUE)
#' if (requireNamespace("ocedata", quietly=TRUE)) {
#'     data(coastlineWorldFine, package="ocedata")
#'     lines(coastlineWorldFine[["longitude"]], coastlineWorldFine[["latitude"]])
#' }
#'}
#'
#' @references
#' * Amante, C. and B.W. Eakins, 2009. ETOPO1 1 Arc-Minute Global Relief
#' Model: Procedures, Data Sources and Analysis. NOAA Technical Memorandum
#' NESDIS NGDC-24. National Geophysical Data Center, NOAA. \doi{doi:10.7289/V5C8276M}
#'
#' @family functions that download files
#' @family things related to topo data
download.topo <- function(west, east, south, north, resolution=4,
                           destdir=".", destfile, format,
                           server="https://gis.ngdc.noaa.gov",
                           debug=getOption("oceDebug"))
{
    oceDebug(debug, "download.topo(west=", west,
             ", east=", east,
             ", south=", south,
             ", north=", north,
             ", resolution=", resolution,
             ", destdir='", destdir,
             ", server='", server,
             "')\n",
             sep="", style="bold", unindent=1)
    if (resolution < 1)
        warning("resolution is < 1, which may cause errors or incorrect results\n")

    if (west > 180)
        west <- west - 360
    if (east > 180)
        east <- east - 360
    wName <- paste(abs(round(west,2)), if (west <= 0) "W" else "E", sep="")
    eName <- paste(abs(round(east,2)), if (east <= 0) "W" else "E", sep="")
    sName <- paste(abs(round(south,2)), if (south <= 0) "S" else "N", sep="")
    nName <- paste(abs(round(north,2)), if (north <= 0) "S" else "N", sep="")
    resolutionName <- paste(resolution, "min", sep="")
    if (missing(destfile))
        destfile <- paste0(paste("topo", wName, eName, sName, nName, resolutionName, sep="_"), ".nc")
    destination <- paste0(destdir, "/", destfile)
    oceDebug(debug, "destination='", destination, "'\n", sep="")
    if (file.exists(destination)) {
        oceDebug(debug, "using existing file \"", destination, "\"\n", sep="")
        oceDebug(debug, "} # download.topo\n", sep="", style="bold", unindent=1)
        return(destination)
    }
    nlon <- (east - west) * 60 / resolution
    nlat <- (north - south) * 60 / resolution
    url <- paste0(server, "/arcgis/rest/services/DEM_mosaics/ETOPO1_bedrock/ImageServer/exportImage",
                  "?bbox=", west, ",", south, ",", east, ",", north,
                  "&bboxSR=4326",
                  "&size=", nlon, ",", nlat,
                  "&imageSR=4326",
                  "&format=tiff",
                  "&pixelType=S16",
                  "&interpolation=+RSP_NearestNeighbor",
                  "&compression=LZW",
                  "&f=image")
    oceDebug(debug, "querying \"", url, "\"\n", sep="")
    if (!requireNamespace("raster", quietly=TRUE))
        stop('must install.packages("raster"), which is required to translate downloaded data')
    if (!requireNamespace("ncdf4", quietly=TRUE))
        stop('must install.packages("ncdf4"), which is required to save topo data')
    r <- raster::raster(x=url)
    oceDebug(debug, "converting data\n", sep="")
    longitude <- seq(r@extent@xmin, r@extent@xmax, length.out=r@ncols)
    latitude <- seq(r@extent@ymin, r@extent@ymax, length.out=r@nrows)
    z <- t(raster::as.matrix(raster::flip(r, direction="y")))
    oceDebug(debug, "saving to \"", destination, "\"\n", sep="")
    ## create netcdf file
    ## dimensions
    #side <- ncdf4::ncdim_def("side", units="", vals=2.0)
    fillvalue <- 1e32
    lonDim <- ncdf4::ncdim_def("lon", "degrees_east", as.double(longitude))
    latDim <- ncdf4::ncdim_def("lat", "degrees_north", as.double(latitude))
    Band1 <- ncdf4::ncvar_def("Band1", "m", list(lonDim, latDim), fillvalue, "elevation m", prec="double")
    nc <- ncdf4::nc_create(destination, list(Band1))
    ncdf4::ncvar_put(nc, "Band1", z)
    ncdf4::nc_close(nc)
    oceDebug(debug, "} # download.topo()\n", sep="", style="bold", unindent=1)
    destination
}

#' @title Interpolate Within a Topo Object
#'
#' @description
#' Bilinear interpolation is used so that values will vary smoothly within a
#' longitude-latitude grid cell. Note that the sign convention for
#' `longitude` and `latitude` must match that in `topo`.
#'
#' @param longitude Vector of longitudes (in the same sign convention as used in
#' `topo`).
#'
#' @param latitude Vector of latitudes (in the same sign convention as used in
#' `topo`).
#'
#' @param topo A [topo-class] object.
#'
#' @return Vector of heights giving the elevation of the earth above means sea
#' level at the indicated location on the earth.
#'
#' @examples
#' library(oce)
#' data(topoWorld)
#' # "The Gully", approx. 400m deep, connects Gulf of St Lawrence with North Atlantic
#' topoInterpolate(45, -57, topoWorld)
#'
#' @author Dan Kelley
#'
#' @family things related to topo data
topoInterpolate <- function(longitude, latitude, topo)
{
    if (missing(longitude)) stop("must supply longitude")
    if (missing(latitude)) stop("must supply latitude")
    if (missing(topo)) stop("must supply topo")
    if (length(latitude) != length(longitude)) stop("lengths of latitude and longitude must match")
    bilinearInterp(longitude, latitude, topo[["longitude"]], topo[["latitude"]], topo[["z"]])
}


#' Plot a topo Object
#'
#' This plots contours of topographic elevation.  The plot aspect ratio is set
#' based on the middle latitude in the plot.  The line properties, such as
#' `land.lwd`, may either be a single item, or a vector; in the latter case,
#' the length must match the length of the corresponding properties, e.g.
#' `land.z`.
#'
#' @param x a [topo-class] object.
#'
#' @param xlab,ylab Character strings giving a label for the x and y axes.
#'
#' @param asp Aspect ratio for plot.  The default is for `plot.coastline` to
#' set the aspect ratio to give natural latitude-longitude scaling somewhere near
#' the centre latitude on the plot. Often, it makes sense to set `asp`
#' yourself, e.g. to get correct shapes at 45N, use `asp=1/cos(45*pi/180)`.
#' Note that the land mass is not symmetric about the equator, so to get good
#' world views you should set `asp=1` or set `ylim` to be symmetric
#' about zero.  Any given value of `asp` is ignored, if `clongitude` and
#' `clatitude` are given.
#'
#' @param clatitude Optional center latitude of map, in degrees north.  If this
#' and `clongitude` are provided, then any provided value of `asp` is
#' ignored, and instead the plot aspect ratio is computed based on the center
#' latitude.  Also, if `clongitude` and `clatitude` are provided, then
#' `span` must be, also.
#'
#' @param clongitude Optional center longitude of map, in degrees east; see
#' `clatitude`.
#'
#' @param span Optional suggested span of plot, in kilometers (must be supplied,
#' if `clongitude` and `clatitude` are supplied).
#'
#' @param expand Numerical factor for the expansion of plot limits, showing area
#' outside the plot, e.g. if showing a ship track as a coastline, and then an
#' actual coastline to show the ocean boundary.  The value of `expand` is
#' ignored if either `xlim` or `ylim` is given.
#'
#' @param water.z Depths at which to plot water contours.  If not provided, these
#' are inferred from the data.
#'
#' @param col.water Colors corresponding to `water.z` values.  If not
#' provided, these will be `"fill"` colors from
#' [oce.colorsGebco()].
#'
#' @param lty.water Line type(s) for water contours.
#'
#' @param lwd.water Line width(s) for water contours.
#'
#' @param land.z Depths at which to plot land contours.  If not provided, these
#' are inferred from the data.  If set to `NULL`, no land contours will be
#' plotted.
#'
#' @param col.land Colors corresponding to `land.z` values.  If not
#' provided, these will be `"fill"` colors from
#' [oce.colorsGebco()].
#'
#' @param lty.land Line type(s) for land contours.
#'
#' @param lwd.land Line width(s) for land contours.
#'
#' @param geographical Logical, indicating whether to plot latitudes and
#' longitudes without minus signs.
#'
#' @param location Location for a legend (or `"none"`, for no legend).
#'
#' @param mgp 3-element numerical vector to use for `par(mgp)`, and also for
#' `par(mar)`, computed from this.  The default is tighter than the R
#' default, in order to use more space for the data and less for the axes.
#'
#' @param mar Four-element numerical vector to be used with
#' [`par`]`("mar")`.
#'
#' @param debug Numerical value, with positive values indicating higher levels of
#' debugging.
#'
#' @param ... Additional arguments passed on to plotting functions.
#'
#' @examples
#' library(oce)
#' data(topoWorld)
#' plot(topoWorld, clongitude=-60, clatitude=45, span=10000)
#'
#' @author Dan Kelley
#'
#' @family functions that plot oce data
#' @family things related to topo data
#' @aliases plot.topo
setMethod(f="plot",
          signature=signature("topo"),
          definition=function(x,
                              xlab="", ylab="",
                              asp,
                              clongitude, clatitude, span,
                              ##center, span,
                              expand=1.5,
                              water.z,
                              col.water,
                              lty.water,
                              lwd.water,
                              land.z,
                              col.land,
                              lty.land,
                              lwd.land,
                              geographical=FALSE,
                              location="topright",
                              mgp=getOption("oceMgp"),
                              mar=c(mgp[1]+1, mgp[1]+1, 1, 1),
                              debug=getOption("oceDebug"),
                              ...)
          {
              if (!inherits(x, "topo"))
                  stop("method is only for objects of class '", "topo", "'")
              oceDebug(debug, "plot.topo() {\n", unindent=1)

              ##opar <- par(no.readonly = TRUE)
              ##on.exit(par(opar))
              par(mgp=mgp, mar=mar)
              dots <- list(...)
              dotsNames <- names(dots)
              if ("center" %in% dotsNames) stop("please use 'clatitude' and 'clongitude' instead of 'center'")
              gave.center <- !missing(clatitude) && !missing(clongitude)

              gave.span <- !missing(span)
              if (gave.center != gave.span) stop("must give all of 'clatitude', 'clongitude' and 'span', or none of them")
              if (!missing(clongitude) && clongitude > 180)
                  clongitude <- clongitude - 360
              if (!missing(clongitude) && clongitude < -180)
                  clongitude <- clongitude + 360
              if (gave.center) {
                  if (!missing(asp))
                      warning("argument 'asp' being ignored, because argument 'center' was given")
                  asp <- 1 / cos(clatitude * atan2(1, 1) / 45) #  ignore any provided asp, because lat from center over-rides it
                  xr <- clongitude + span * c(-1/2, 1/2) / 111.11 / asp
                  yr <- clatitude  + span * c(-1/2, 1/2) / 111.11
                  oceDebug(debug, "gave center; calculated xr=", xr, " yr=", yr, " asp=", asp, "\n")
              } else {
                  if (missing(asp)) {
                      if ("ylim" %in% dotsNames)
                          asp <- 1 / cos(mean(range(dots$ylim, na.rm=TRUE)) * pi / 180) # dy/dx
                      else
                          asp <- 1 / cos(mean(range(x[["latitude"]], na.rm=TRUE)) * pi / 180) # dy/dx
                  }
                  ## Expand
                  xr0 <- range(x[["longitude"]], na.rm=TRUE)
                  yr0 <- range(x[["latitude"]], na.rm=TRUE)
                  oceDebug(debug, "xr0=", xr0, "\n")
                  oceDebug(debug, "yr0=", yr0, "\n")
                  if (expand >= 0 && max(abs(xr0)) < 100 && max(abs(yr0) < 70)) {
                      ## don't expand if full map
                      xr <- mean(xr0) + expand * diff(xr0) * c(-1/2, 1/2)
                      yr <- mean(yr0) + expand * diff(yr0) * c(-1/2, 1/2)
                  } else {
                      xr <- xr0
                      yr <- yr0
                  }
              }
              zr <- range(x[["z"]], na.rm=TRUE)
              if (gave.center && !is.null(dots$xlim))
                  stop("cannot give 'xlim' argument if the 'center' argument was given")
              if (gave.center && !is.null(dots$ylim))
                  stop("cannot give 'ylim' argument if the 'center' argument was given")
              ## auto-scale based on data in window, if window provided
              if (!is.null(dots$xlim) && !is.null(dots$ylim)) {
                  xr <- dots$xlim
                  yr <- dots$ylim
              }

              ## The following is a somewhat provisional hack, to get around a
              ## tendency of plot() to produce latitudes past the poles.
              ## BUG: the use of par("pin") seems to mess up resizing in aqua windows.
              asp.page <- par("pin")[2] / par("pin")[1] # dy / dx
              oceDebug(debug, "par('pin')=", par('pin'), "asp=", asp, "asp.page=", asp.page, "\n")
              if (asp > asp.page) {
                  ## FIXME: this seems to have x and y mixed up (asp=dy/dx)
                  oceDebug(debug, "type 1 (will narrow x range)\n")
                  d <- asp / asp.page * diff(xr)
                  xr <- mean(xr) + d * c(-1/2, 1/2)
                  oceDebug(debug, "xr narrowed to:", xr, "\n")
                  ## xr[2] <- xr[1] + (xr[2] - xr[1]) * (asp / asp.page)
              } else {
                  oceDebug(debug, "type 2 (will narrow y range)\n")
                  d <- asp / asp.page * diff(yr)
                  yr <- mean(yr) + d * c(-1/2, 1/2)
                  oceDebug(debug, "yr narrowed to:", yr, "\n")
                  ##yr[2] <- yr[1] + (yr[2] - yr[1]) / (asp / asp.page)
              }

              oceDebug(debug, "xr:", xr, "(before trimming)\n")
              oceDebug(debug, "yr:", yr, "(before trimming)\n")
                                        #    if (xr[1] < -180) xr[1] <- -180
                                        #    if (xr[2] >  180) xr[2] <- 180
              if (yr[1] < -90)  yr[1] <- -90
              if (yr[2] >  90)  yr[2] <-  90
              oceDebug(debug, "xr:", xr, "(after trimming)\n")
              oceDebug(debug, "yr:", yr, "(after trimming)\n")

              X <- x[["longitude"]]
              Y <- x[["latitude"]]
              Z <- x[["z"]]
              ## Handle repeats modulo 180
              if (X[1] == -180 && X[length(X)] == 180) {
                  ## warning("In plot() : trimming a longitude of 180 since longitude -180 is present",
                  ##         call.=FALSE)
                  keep <- seq.int(1L, length(X)-1)
                  X <- X[keep]
                  Z <- Z[keep, ]
              }
              ## check for prime meridian
              if (sign(prod(xr)) < 0) {
                  Z <- rbind(Z, Z)
                  X <- c(X - 360, X)
                  ## If X runs from -180 to 180, then subtracting 360 will duplicate the -180 value,
                  ## so we test for repeats. We don't test for a diff of exactly zero, for numerical
                  ## reasons, and the test for 0.001 times the mean is quite arbitrary, since we
                  ## are likely looking for a value of 1e-14 or so, which is FAR below the difference
                  ## we would get in realistic topographic data.
                  dX <- diff(X)
                  if (any(dX < 0.001*mean(dX))) {
                      delete <- which(dX < 0.001 * mean(dX))[1]
                      X <- X[-delete]
                      Z <- Z[-delete, ]
                  }
              }

              ## Data may not extend across plot region
              ##lon.range <- range(x[["longitude"]], na.rm=TRUE)
              ##lat.range <- range(x[["latitude"]], na.rm=TRUE)
              lon.range <- range(X, na.rm=TRUE)
              lat.range <- range(Y, na.rm=TRUE)
              if (xr[1] < lon.range[1]) xr[1] <- lon.range[1]
              if (xr[2] > lon.range[2]) xr[2] <- lon.range[2]
              if (yr[1] < lat.range[1]) yr[1] <- lat.range[1]
              if (yr[2] > lat.range[2]) yr[2] <- lat.range[2]

              plot(xr, yr, asp=asp, xlab="", ylab="", type="n", xaxs="i", yaxs="i", axes=FALSE, ...)
              if (debug > 0)
                  points(xr, yr, col="blue", pch=20, cex=3)

              xr.pretty <- pretty(xr)
              yr.pretty <- pretty(yr)
              oceDebug(debug, "xr.pretty=", xr.pretty, "(before trimming)\n")
              xr.pretty <- subset(xr.pretty, xr.pretty >= xr[1] & xr.pretty <= xr[2])
              oceDebug(debug, "xr.pretty=", xr.pretty, "(after trimming)\n")
              oceDebug(debug, "yr.pretty=", yr.pretty, "(before trimming)\n")
              yr.pretty <- subset(yr.pretty, yr.pretty >= yr[1] & yr.pretty <= yr[2])
              oceDebug(debug, "yr.pretty=", yr.pretty, "(after trimming)\n")

              lines(c(xr[1], xr[2], xr[2], xr[1], xr[1]), c(yr[1], yr[1], yr[2], yr[2], yr[1])) # axis box
              xlabels <- format(xr.pretty)
              ylabels <- format(yr.pretty)
              if (geographical) {
                  xlabels <- sub("-", "", xlabels)
                  ylabels <- sub("-", "", ylabels)
              }
              axis(1, at=xr.pretty, pos=yr[1], labels=xlabels)
              axis(3, at=xr.pretty, pos=max(yr), labels=FALSE)
              axis(2, at=yr.pretty, pos=xr[1], labels=ylabels)
              axis(4, at=yr.pretty, pos=max(xr), labels=FALSE)
              ## Use either mtext() or text() to position the label, depending on
              ## whether the extra margin space has been placed to the sides
              ## of the graph, or above and below it.
              if (0 != nchar(xlab)) {
                  if (asp > asp.page) {
                      mtext(xlab, side=1, line=mgp[1])
                  } else {
                      text(mean(par('usr')[1:2]), yr[1], xlab, pos=1, offset=mgp[1]+mgp[2])
                  }
              }
              if (0 != nchar(ylab)) {
                  if (asp > asp.page) {
                      text(xr[1], mean(par('usr')[3:4]), ylab, pos=2, offset=mgp[1]+mgp[2], srt=90)
                  } else {
                      mtext(ylab, side=2, line=mgp[1])
                  }
              }

              oceDebug(debug, "xr=", xr, "yr=", yr, "\n")
              ##yaxp <- par("yaxp")
              oceDebug(debug, "par(yaxp)", par("yaxp"), "\n")
              oceDebug(debug, "par(pin)", par("pin"), "\n")

              ## need to clip because contour() does not do so
              xx <- X                            # x[["longitude"]]
              yy <- Y                            # x[["latitude"]]
              xclip <- xx < xr[1] | xr[2] < xx
              yclip <- yy < yr[1] | yr[2] < yy
              xx <- xx[!xclip]
              if (length(xx) < 1)
                  stop("there are no topographic data within the longitudes of the plot region.")
              yy <- yy[!yclip]
              if (length(yy) < 1)
                  stop("there are no topographic data within the latitudes of the plot region.")
              ##zz <- x[["z"]][!xclip, !yclip]
              zz <- Z[!xclip, !yclip]
              zr <- range(zz)
              contour(xx, yy, zz,
                      levels=0, drawlabels=FALSE, add=TRUE,
                      col="black")                # coastline is always black

              legend <- lwd <- lty <- col <- NULL
              if (zr[1] < 0) {
                  if (missing(water.z)) {
                      if (zr[2] > 0) {
                          water.z <- pretty(c(zr[1], 0))
                          water.z <- water.z[water.z!=0]
                                        #cat("water.z=");print(water.z)
                          ## Do some tricks to get shelf water as well as deep
                          if (max(water.z) == -1000)
                              water.z <- c(water.z, -500, -250, -100, -50)
                          else if (max(water.z) == -500)
                              water.z <- c(water.z, -400, -300, -200, -150, -100, -50)
                                        #cat("after tricks, water.z=");print(water.z)
                      } else {
                          water.z <- pretty(zr)
                      }
                      water.z <- sort(water.z)
                  }
                  nz <- length(water.z)
                  if (missing(col.water))
                      col.water <- oce.colorsGebco(nz, "water", "line")
                  if (missing(lty.water))
                      lty.water <- rep(par("lty"), nz)
                  else if (length(lty.water) == 1)
                      lty.water <- rep(lty.water, nz)
                  if (missing(lwd.water))
                      lwd.water <- rep(par("lwd"), nz)
                  else if (length(lwd.water) == 1)
                      lwd.water <- rep(lwd.water, nz)
                  legend <- c(legend, water.z)
                  lwd    <- c(lwd,    lwd.water)
                  lty    <- c(lty,    lty.water)
                  col    <- c(col,    col.water)
                  contour(xx, yy, zz,
                          levels=water.z, lwd=lwd.water, lty=lty.water, col=col.water,
                          drawlabels=FALSE, add=TRUE, ...)
              }
              if (zr[2] > 0) {
                  if (missing(land.z)) {
                      if (zr[1] < 0) {
                          land.z <- pretty(c(0, zr[2]))
                          land.z <- land.z[land.z!=0]
                      } else {
                          land.z <- pretty(zr)
                      }
                  }
                  nz <- length(land.z)
                  if (nz > 0) {
                      if (missing(col.land))
                          col.land <- oce.colorsGebco(nz, "land", "line")
                      if (missing(lty.land))
                          lty.land <- rep(par("lty"), nz)
                      else if (length(lty.land) == 1)
                          lty.land <- rep(lty.land, nz)
                      if (missing(lwd.land))
                          lwd.land <- rep(par("lwd"), nz)
                      else if (length(lwd.land) == 1)
                          lwd.land <- rep(lwd.land, nz)
                      legend <- c(legend, land.z)
                      lwd    <- c(lwd,    lwd.land)
                      lty    <- c(lty,    lty.land)
                      col    <- c(col,    col.land)
                      contour(xx, yy, zz,
                              levels=land.z, lwd=lwd.land, lty=lty.land, col=col.land,
                              drawlabels=FALSE, add=TRUE, ...)
                  }
              }
              if (!is.null(location) && location != "none") {
                  o <- rev(order(legend))
                  legend(location, lwd=lwd[o], lty=lty[o], bg="white", legend=legend[o], col=col[o])
              }
              oceDebug(debug, "} # plot.topo()\n", unindent=1)
              invisible(NULL)
          })


#' Read a Topo File
#'
#' Read a file that contains topographic data in the ETOPO dataset, as was once provided by
#' the NOAA website (see [download.topo()] for a good server for such
#' files. (As of May, 2020, there does not seem to be a way to download these
#" fles from the NOAA website.)
#'
#' The three permitted file types are as follows.
#' 1. An ascii type
#' in which line 1 holds a label (which is ignored), whitespace, and then
#' the number of columns in the matrix (i.e. the number of longitude values),
#' line 2 is similar but for latitude, line 3 is similar but for the westernmost
#' longitude, line 4 is similar but for southernmost latitude, line 5
#' is similar but for cell size, and lines after that hold the grid.
#' 2. A NetCDF format that was once described by NOAA as "GMT NetCDF".
#' 3. A NetCDF format that was once described by NOAA as "NetCDF".
#'
#' @param file Name of a file containing an ETOPO-format dataset. Three
#' types are permitted; see \dQuote{Details}.
#'
#' @template debugTemplate
#'
#' @return
#' A [topo-class] object that.
#'
#' @examples
#'\dontrun{
#' library(oce)
#' topoMaritimes <- read.topo("topoMaritimes.asc")
#' plot(topographyMaritimes)
#'}
#'
#' @author Dan Kelley
#' @family things related to topo data
read.topo <- function(file, debug=getOption("oceDebug"))
{
    if (!missing(file) && is.character(file) && 0 == file.info(file)$size)
        stop("empty file")
    oceDebug(debug, "read.topo(file=\"", file, "\") {\n", sep="", style="bold", unindent=1)
    ## handle GEBCO netcdf files or an ascii format
    dataNamesOriginal <- list()
    if (is.character(file) && length(grep(".nc$", file))) {
        oceDebug(debug, "this is a netcdf file\n")
        if (!requireNamespace("ncdf4", quietly=TRUE)) {
            stop('must install.packages("ncdf4") to read topo data from a NetCDF file')
        } else {
            ##message("file: '", file, "'")
            ## "GEBCO NetCDF" (NOT the same as "NetCDF")
            ## NOTE: need to name ncdf4 package because otherwise R checks give warnings.
            ncdf <- ncdf4::nc_open(file)
            dataNamesOriginal <- list()
            if ("Band1" %in% names(ncdf$var)) {
                oceDebug(debug, "file has a variable named 'Band1', so reading longitude as 'lon', latitude as 'lat', and z as 'Band1'\n")
                z <- ncdf4::ncvar_get(ncdf, "Band1")
                longitude <- as.vector(ncdf4::ncvar_get(ncdf, "lon"))
                latitude <- as.vector(ncdf4::ncvar_get(ncdf, "lat"))
                dataNamesOriginal <- list(longitude="lon", latitude="lat", z="Band1")
                ##cat(vectorShow(longitude, "longitude in reading Band1"))
            } else {
                oceDebug(debug, "file has no variable named 'Band1', so computing longitude and latitude from 'x_range' and 'y_range' together with 'spacing', and reading z as 'z'\n")
                xrange <- ncdf4::ncvar_get(ncdf, "x_range")
                yrange <- ncdf4::ncvar_get(ncdf, "y_range")
                ##zrange <- ncdf4::ncvar_get(ncdf, "z_range")
                spacing <- ncdf4::ncvar_get(ncdf, "spacing")
                longitude <- seq(xrange[1], xrange[2], by=spacing[1])
                latitude <- seq(yrange[1], yrange[2], by=spacing[2])
                z <- ncdf4::ncvar_get(ncdf, "z")
                dim <- ncdf4::ncvar_get(ncdf, "dimension")
                z <- t(matrix(z, nrow=dim[2], ncol=dim[1], byrow=TRUE))
                z <- z[, dim[2]:1]
                dataNamesOriginal <- list(longitude="-", latitude="-", z="-")
            }
            ## FIXME(DK 2016-08-20): Sometimes length is off by 1. I'm not sure why, and
            ## FIXME(DK 2016-08-20): this should be figured out by inspection of files.
            if (length(longitude) == dim(z)[1]+1) {
                oceDebug(debug, "offsetting longitude of a netcdf topo file by half a step\n")
                warning("offsetting longitude of a netcdf topo file by half a step")
                longitude <- longitude[-1] - diff(longitude[1:2])/2
            }
            if (length(latitude) == dim(z)[2]+1) {
                oceDebug(debug, "offsetting latitude of a netcdf topo file by half a step")
                warning("offsetting latitude of a netcdf topo file by half a step")
                latitude <- latitude[-1] - diff(latitude[1:2])/2
            }
            res <- as.topo(longitude, latitude, z, filename=file)
        }
    } else {
        oceDebug(debug, "this is an ASCII (text) file\n")
        ## ASCII
        ## NOTE: on 2014-11-13 it came to light that the old dataset website
        ##          http://www.ngdc.noaa.gov/mgg/gdas/gd_designagrid.html
        ## was no longer working, and that the new one
        ##          http://maps.ngdc.noaa.gov/viewers/wcs-client/
        ## seemed to have headers 5 lines long.  However,
        ## the code below has a trick to (perhaps) auto-detect whether the header
        ## length is 5 or 6.
        nh <- 6
        header <- readLines(file, n=nh)
        if (nchar(header[length(header)]) > 50) {
            ## the header is only 5 long, if the last header line is long.
            nh <- nh - 1
            header <- header[1:nh]
        }
        ncol <- as.numeric(strsplit(header[1], "[ ]+", perl=TRUE)[[1]][2])
        nrow <- as.numeric(strsplit(header[2], "[ ]+", perl=TRUE)[[1]][2])
        longitudeLowerLeft <- as.numeric(strsplit(header[3], "[ ]+", perl=TRUE)[[1]][2])
        latitudeLowerLeft <- as.numeric(strsplit(header[4], "[ ]+", perl=TRUE)[[1]][2])
        cellSize <- as.numeric(strsplit(header[5], "[ ]+", perl=TRUE)[[1]][2])
        missingValue <- NA
        i <- grep("nodata", header)
        if (length(i))
            missingValue <- as.numeric(strsplit(header[i], "[ ]+", perl=TRUE)[[1]][2])
        zz <- as.matrix(read.table(file, header=FALSE, skip=nh), byrow=TRUE)
        rownames(zz) <- NULL
        colnames(zz) <- NULL
        longitude <- longitudeLowerLeft + cellSize * seq(0, ncol-1)
        latitude <- latitudeLowerLeft + cellSize * seq(0, nrow-1)
        z <- t(zz[dim(zz)[1]:1, ])
        if (!is.na(missingValue))
            z[z == missingValue] <- NA
        res <- as.topo(longitude, latitude, z, filename=file) # FIXME: add units here
    }
    res@metadata$dataNamesOriginal <- dataNamesOriginal
    res@processingLog <- processingLogAppend(res@processingLog,
                                              paste(deparse(match.call()), sep="", collapse=""))
    oceDebug(debug, "} # read.topo\n", sep="", style="bold", unindent=1)
    res
}


#' Coerce Data into Topo Object
#'
#' @param longitude Either a vector of longitudes (in degrees east, and bounded by
#' -180 and 180), or a `bathy` object created by `getNOAA.bathy()` from
#' the `marmap` package; in the second case, all other arguments are ignored.
#'
#' @param latitude A vector of latitudes.
#'
#' @param z A matrix of heights (positive over land).
#'
#' @param filename Name of data (used when called by [read.topo()].
#'
#' @return A [topo-class] object.
#'
#' @author Dan Kelley
#'
#' @family things related to topo data
as.topo <- function(longitude, latitude, z, filename="")
{
    if (inherits(longitude, "bathy")) {
        bathy <- longitude
        longitude <- as.numeric(rownames(bathy))
        latitude <- as.numeric(colnames(bathy))
        z <- as.matrix(bathy)
    }
    ncols <- length(longitude)
    nrows <- length(latitude)
    ## longitudeLowerLeft <- min(longitude, na.rm=TRUE)
    ## latitudeLowerLeft <- min(latitude, na.rm=TRUE)
    dim <- dim(z)
    if (dim[1] != ncols)
        stop("longitude vector has length ", ncols, ", which does not match matrix width ", dim[1])
    if (dim[2] != nrows)
        stop("latitude vector has length ", ncols, ", which does not match matrix height ", dim[2])
    units <- list(latitude=list(unit=expression(degree*E), scale=""),
                  longitude=list(unit=expression(degree*N), scale=""),
                  z=list(unit=expression(m), scale=""))
    res <- new("topo", latitude=latitude, longitude=longitude, z=z, filename=filename, units=units)
    res@processingLog <- processingLogAppend(res@processingLog,
                                              paste(deparse(match.call()), sep="", collapse=""))
    res
}
