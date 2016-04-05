## vim:textwidth=128:expandtab:shiftwidth=4:softtabstop=4


#' Class to store topographic data
#' 
#' Topographic data may be read with \code{\link{read.topo}} or assembled with
#' \code{\link{as.topo}}.  Plotting are handled with \code{\link{plot,topo-method}}
#' and summaries with \code{\link{summary,topo-method}}. Data retrieval may be
#' done with \code{\link{[[,topo-method}} and replacement with
#' \code{\link{[[<-,topo-method}}.  The key data, stored within the data slot,
#' are: \code{longititude}, \code{latitude}, and \code{z}.
#' 
#' @author Dan Kelley
#' 
#' @family classes provided by \code{oce}
#' @family things related to \code{topo} data
setClass("topo", contains="oce")

#' Global topographic dataset at half-degree resolution
#' 
#' Global topographic dataset at half-degree resolution, created by decimating the
#' ETOPO5 dataset.  Its longitude ranges from -179.5 to 180, and its latitude
#' ranges from -89.5 to 90.  Height is measured in metres above nominal sea level.
#' 
#' The coarse resolution can be a problem in plotting depth contours along with
#' coastlines in regions of steep topography. For example, near the southeast
#' corner of Newfoundland, a 200m contour will overlap a coastline drawn with
#' \code{\link[ocedata]{coastlineWorldFine}}. The solution in such cases is to
#' download a higher-resolution topography file and create a \code{topo} object
#' with \code{\link{read.topo}} or \code{\link{as.topo}}.
#' 
#' @name topoWorld
#' @docType data
#'
#' @usage data(topoWorld)
#' 
#' @source
#' The binary ETOPO5 dataset was downloaded in late 2009 from the NOAA website,
#' \url{http://www.ngdc.noaa.gov/mgg/global/relief/ETOPO5/TOPO/ETOPO5/}, decoded,
#' decimated from 1/12th degree resolution to 1/2 degree resolution, and shifted so
#' that longitude runs from negative to positive instead of from 0 to 360.
#' 
#' @examples
#' \dontrun{
#' library(oce)
#' data(topoWorld) 
#' par(mfrow=c(2,1))
#' plot(topoWorld, location=NULL)
#' imagep(topoWorld)
#' }
#'
#' @family datasets provided with \code{oce}
#' @family things related to \code{topo} data
NULL

setMethod(f="initialize",
          signature="topo",
          definition=function(.Object,longitude,latitude,z,filename="", units) {
              if (!missing(longitude)) .Object@data$longitude <- longitude
              if (!missing(latitude)) .Object@data$latitude <- latitude
              if (!missing(z)) .Object@data$z <- z
              if (!missing(units)) .Object@metadata$units <- units
              .Object@metadata$filename <- filename
              .Object@processingLog$time <- as.POSIXct(Sys.time())
              .Object@processingLog$value <- "create 'topo' object"
              return(.Object)
          })


#' Summarize a topography data object
#' 
#' Pertinent summary information is presented, including the longitude and
#' latitude range, and the range of elevation.
#' 
#' @param object A \code{topo} object, i.e. inheriting from \code{\link{topo-class}}.
#' 
#' @param \dots Further arguments passed to or from other methods.
#' 
#' @return A matrix containing statistics of the elements of the \code{data} slot.
#' 
#' @examples
#' library(oce)
#' data(topoWorld)
#' summary(topoWorld)
#' 
#' @author Dan Kelley
#' 
#' @family things related to \code{topo} data
setMethod(f="summary",
          signature="topo",
          definition=function(object, ...) {
              cat("\nTopo dataset\n------------\n")
              cat("* Source:          ", object[["filename"]], "\n")
              callNextMethod()
          })

#' @title Extract Something From a \code{topo} Object
#' @param x A topo object, i.e. one inheriting from \code{\link{topo-class}}.
#' @examples
#' data(topoWorld)
#' dim(topoWorld[['z']])
#'
#' @section Details of the specialized topo method:
#' There are no special features for \code{\link{topo-class}} data;
#' the general method is used directly.
#' @template sub_subTemplate
#' @family things related to \code{topo} data
setMethod(f="[[",
          signature(x="topo", i="ANY", j="ANY"),
          definition=function(x, i, j, ...) {
              callNextMethod()
          })

#' @title Replace Parts of a \code{topo} Object
#' @param x An \code{topo} object, i.e. inheriting from \code{\link{topo-class}}
#' @family things related to \code{topo} data
#' @template sub_subsetTemplate
setMethod(f="[[<-",
          signature(x="topo", i="ANY", j="ANY"),
          definition=function(x, i, j, value) {
              callNextMethod(x=x, i=i, j=j, value=value)
          })

#' Subset a topo object
#' 
#' @details
#' This function is somewhat analogous to \code{\link{subset.data.frame}}.
#' Subsetting can be by \code{time} or \code{distance}, but these may not be
#' combined; use a sequence of calls to subset by both.
#' 
#' @param x A \code{topo} object, i.e. inheriting from \code{\link{topo-class}}.
#' 
#' @param subset A condition to be applied to the \code{data} portion of \code{x}.
#' See \sQuote{Details}.
#' 
#' @param ... Ignored.
#' 
#' @return A new \code{\link{topo-class}} object.
#' 
#' @examples
#' ## northern hemisphere
#' library(oce)
#' data(topoWorld)
#' plot(subset(topoWorld, latitude > 0))
#' 
#' @author Dan Kelley
#' 
#' @family things related to \code{topo} data
setMethod(f="subset",
          signature="topo",
          definition=function(x, subset, ...) {
              subsetString <- paste(deparse(substitute(subset)), collapse=" ")
              res <- x
              dots <- list(...)
              debug <- getOption("oceDebug")
              if (length(dots) && ("debug" %in% names(dots)))
                  debug <- dots$debug
              if (missing(subset))
                  stop("must give 'subset'")
              if (length(grep("longitude", subsetString))) {
                  oceDebug(debug, "subsetting a topo object by longitude\n")
                  keep <- eval(substitute(subset), x@data, parent.frame(2))
                  oceDebug(debug, "keeping", 100*sum(keep)/length(keep), "% of longitudes\n")
                  res[["longitude"]] <- x[["longitude"]][keep]
                  res[["z"]] <- x[["z"]][keep,]
              } else if (length(grep("latitude", subsetString))) {
                  oceDebug(debug, "subsetting a topo object by latitude\n")
                  keep <- eval(substitute(subset), x@data, parent.frame(2))
                  oceDebug(debug, "keeping", 100*sum(keep)/length(keep), "% of latitudes\n")
                  res[["latitude"]] <- x[["latitude"]][keep]
                  res[["z"]] <- x[["z"]][,keep]
              } else {
                  stop("the subset must be based on longitude or latitude")
              }
              res@processingLog <- processingLogAppend(res@processingLog, paste("subset.topo(x, subset=", subsetString, ")", sep=""))
              res
          })

#' Interpolate within a topography dataset
#' 
#' Bilinear interpolation is used so that values will vary smoothly within a
#' longitude-latitude grid cell. Note that the sign convention for
#' \code{longitude} and \code{latitude} must match that in \code{topo}.
#' 
#' @param longitude Vector of longitudes (in the same sign convention as used in
#' \code{topo}).
#'   
#' @param latitude Vector of latitudes (in the same sign convention as used in
#' \code{topo}).
#'   
#' @param topo A \code{topo} object, i.e. inheriting from
#' \code{\link{topo-class}}.
#' 
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
#' @family things related to \code{topo} data
topoInterpolate <- function(longitude, latitude, topo)
{
    if (missing(longitude)) stop("must supply longitude")
    if (missing(latitude)) stop("must supply latitude")
    if (missing(topo)) stop("must supply topo")
    if (length(latitude) != length(longitude)) stop("lengths of latitude and longitude must match")
    .Call("topo_interpolate", latitude, longitude, topo[["latitude"]], topo[["longitude"]], topo[["z"]])
}


#' Plot topography data
#' 
#' This plots contours of topographic elevation.  The plot aspect ratio is set
#' based on the middle latitude in the plot.  The line properties, such as
#' \code{land.lwd}, may either be a single item, or a vector; in the latter case,
#' the length must match the length of the corresponding properties, e.g.
#' \code{land.z}.
#' 
#' @param x A \code{topo} object, i.e. inheriting from \code{\link{topo-class}}.
#' 
#' @param xlab,ylab Character strings giving a label for the x and y axes.
#' 
#' @param asp Aspect ratio for plot.  The default is for \code{plot.coastline} to
#' set the aspect ratio to give natural latitude-longitude scaling somewhere near
#' the centre latitude on the plot. Often, it makes sense to set \code{asp}
#' yourself, e.g. to get correct shapes at 45N, use \code{asp=1/cos(45*pi/180)}.
#' Note that the land mass is not symmetric about the equator, so to get good
#' world views you should set \code{asp=1} or set \code{ylim} to be symmetric
#' about zero.  Any given value of \code{asp} is ignored, if \code{clongitude} and
#' \code{clatitude} are given.
#' 
#' @param clatitude Optional center latitude of map, in degrees north.  If this
#' and \code{clongitude} are provided, then any provided value of \code{asp} is
#' ignored, and instead the plot aspect ratio is computed based on the center
#' latitude.  Also, if \code{clongitude} and \code{clatitude} are provided, then
#' \code{span} must be, also.
#' 
#' @param clongitude Optional center longitude of map, in degrees east; see
#' \code{clatitude}.
#' 
#' @param span Optional suggested span of plot, in kilometers (must be supplied,
#' if \code{clongitude} and \code{clatitude} are supplied).
#' 
#' @param expand Numerical factor for the expansion of plot limits, showing area
#' outside the plot, e.g. if showing a ship track as a coastline, and then an
#' actual coastline to show the ocean boundary.  The value of \code{expand} is
#' ignored if either \code{xlim} or \code{ylim} is given.
#' 
#' @param water.z Depths at which to plot water contours.  If not provided, these
#' are inferred from the data.
#' 
#' @param col.water Colours corresponding to \code{water.z} values.  If not
#' provided, these will be \code{"fill"} colours from
#' \code{\link{oce.colorsGebco}}.
#' 
#' @param lty.water Line type(s) for water contours.
#' 
#' @param lwd.water Line width(s) for water contours.
#' 
#' @param land.z Depths at which to plot land contours.  If not provided, these
#' are inferred from the data.  If set to \code{NULL}, no land contours will be
#' plotted.
#' 
#' @param col.land Colours corresponding to \code{land.z} values.  If not
#' provided, these will be \code{"fill"} colours from
#' \code{\link{oce.colorsGebco}}.
#' 
#' @param lty.land Line type(s) for land contours.
#' 
#' @param lwd.land Line width(s) for land contours.
#' 
#' @param geographical Logical, indicating whether to plot latitudes and
#' longitudes without minus signs.
#' 
#' @param location Location for a legend (or \code{"none"}, for no legend).
#' 
#' @param mgp 3-element numerical vector to use for \code{par(mgp)}, and also for
#' \code{par(mar)}, computed from this.  The default is tighter than the R
#' default, in order to use more space for the data and less for the axes.
#' 
#' @param mar Four-element numericl vector to be used with
#' \code{\link{par}("mar")}.
#' 
#' @param debug Numerical value, with positive values indicating higher levels of
#' debugging.
#' 
#' @param ... Additional arguments passed on to plotting functions.
#' 
#' 
#' @examples
#' library(oce)
#' data(topoWorld)
#' plot(topoWorld, clongitude=-60, clatitude=45, span=10000)
#' 
#' @author Dan Kelley
#' 
#' @family functions that plot \code{oce} data
#' @family things related to \code{topo} data
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
                              mar=c(mgp[1]+1,mgp[1]+1,1,1),
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
                  oceDebug(debug, "gave center; calculated xr=", xr," yr=", yr, " asp=", asp, "\n")
              } else {
                  if (missing(asp)) {
                      if ("ylim" %in% dotsNames)
                          asp <- 1 / cos(mean(range(dots$ylim, na.rm=TRUE)) * pi / 180) # dy/dx
                      else
                          asp <- 1 / cos(mean(range(x[["latitude"]],na.rm=TRUE)) * pi / 180) # dy/dx
                  }
                  ## Expand
                  xr0 <- range(x[["longitude"]], na.rm=TRUE)
                  yr0 <- range(x[["latitude"]], na.rm=TRUE)
                  oceDebug(debug, "xr0=", xr0, "\n")
                  oceDebug(debug, "yr0=", yr0, "\n")
                  if (expand >= 0 && max(abs(xr0)) < 100 && max(abs(yr0) < 70)) { # don't expand if full map
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
              oceDebug(debug, "par('pin')=",par('pin'), "asp=",asp,"asp.page=", asp.page, "\n")
              if (asp > asp.page) {               # FIXME: this seems to have x and y mixed up (asp=dy/dx)
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
              ## check for prime meridian
              if (sign(prod(xr)) < 0) {
                  Z <- rbind(Z, Z)
                  X <- c(X - 360, X)
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

              oceDebug(debug, "xr=", xr, "yr=",yr,"\n")
              ##yaxp <- par("yaxp")
              oceDebug(debug, "par(yaxp)",par("yaxp"),"\n")
              oceDebug(debug, "par(pin)",par("pin"),"\n")

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
              invisible()
          })


#' Read an topography file
#' 
#' Read a file that contains topographic data in the ETOPO dataset, as provided by
#' the NOAA website [1].
#' 
#' @param file Name of a file containing an ETOPO-format dataset.
#' 
#' @param \dots Additional arguments, passed to called routines.
#' 
#' 
#' @return
#' An object of type \code{\link{topo-class}} that which has the following slots.
#' \item{\code{data}}{: a data frame containing \code{lat}, \code{lon}, and
#'   \code{z}}
#' \item{\code{metadata}}{: a list containing the source filename}
#' \item{\code{processingLog}}{: a log, in the standard \code{oce} format.}
#' 
#' 
#' @examples
#' \dontrun{
#' library(oce)
#' topoMaritimes <- read.topo("topoMaritimes.asc")
#' plot(topographyMaritimes)
#' }
#' 
#' @references
#' 1. NOAA website that provides datasets:
#' \url{http://maps.ngdc.noaa.gov/viewers/wcs-client} with the \code{ArcGIS ASCII
#'   Grid} menu item selected.
#' 
#' @author Dan Kelley
#' @family things related to \code{topo} data
read.topo <- function(file, ...)
{
    ## handle GEBCO netcdf files or an ascii format
    if (is.character(file) && length(grep(".nc$", file))) {
        if (!requireNamespace("ncdf4", quietly=TRUE)) {
            stop('must install.packages("ncdf4") to read topo data from a netCDF file')
        } else {
            ## GEBCO netcdf
            ## NOTE: need to name ncdf4 package because otherwise R checks give warnings.
            ncdf <- ncdf4::nc_open(file)
            xrange <- ncdf4::ncvar_get(ncdf, "x_range")
            yrange <- ncdf4::ncvar_get(ncdf, "y_range")
            ##zrange <- ncdf4::ncvar_get(ncdf, "z_range")
            spacing <- ncdf4::ncvar_get(ncdf, "spacing")
            longitude <- seq(xrange[1], xrange[2], by=spacing[1])
            latitude <- seq(yrange[1], yrange[2], by=spacing[2])
            z <- ncdf4::ncvar_get(ncdf, "z")
            dim <- ncdf4::ncvar_get(ncdf, "dimension")
            z <- t(matrix(z, nrow=dim[2], ncol=dim[1], byrow=TRUE))
            z <- z[,dim[2]:1]
            res <- as.topo(longitude, latitude, z, filename=file)
        }
    } else {
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
        ncol <- as.numeric(strsplit(header[1],"[ ]+",perl=TRUE)[[1]][2])
        nrow <- as.numeric(strsplit(header[2],"[ ]+",perl=TRUE)[[1]][2])
        longitudeLowerLeft <- as.numeric(strsplit(header[3],"[ ]+",perl=TRUE)[[1]][2])
        latitudeLowerLeft <- as.numeric(strsplit(header[4],"[ ]+",perl=TRUE)[[1]][2])
        cellSize <- as.numeric(strsplit(header[5],"[ ]+",perl=TRUE)[[1]][2])
        missingValue <- NA
        if (length(i <- grep("nodata", header)))
            missingValue <- as.numeric(strsplit(header[i],"[ ]+",perl=TRUE)[[1]][2])
        zz <- as.matrix(read.table(file, header=FALSE, skip=nh), byrow=TRUE)
        rownames(zz) <- NULL
        colnames(zz) <- NULL
        longitude <- longitudeLowerLeft + cellSize * seq(0, ncol-1)
        latitude <- latitudeLowerLeft + cellSize * seq(0, nrow-1)
        z <- t(zz[dim(zz)[1]:1,])
        if (!is.na(missingValue))
            z[z == missingValue] <- NA
        res <- as.topo(longitude, latitude, z, filename=file) # FIXME: add units here
    }
    res@processingLog <- processingLogAppend(res@processingLog,
                                              paste(deparse(match.call()), sep="", collapse=""))
    res
}


#' Coerce data into topo dataset
#' 
#' @param longitude Either a vector of longitudes (in degrees east, and bounded by
#' -180 and 180), or a \code{bathy} object created by \code{getNOAA.bathy()} from
#' the \code{marmap} package; in the second case, all other arguments are ignored.
#' 
#' @param latitude A vector of latitudes.
#' 
#' @param z A matrix of heights (positive over land).
#' 
#' @param filename Name of data (used when called by \code{\link{read.topo}}.
#' 
#' @return An object of \code{\link{topo-class}}.
#' 
#' @author Dan Kelley
#'
#' @family things related to \code{topo} data
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

