# vim:textwidth=80:expandtab:shiftwidth=4:softtabstop=4

#' Class to Store AMSR-2 Satellite Data
#'
#' This class stores data from the AMSR-2 satellite.
#'
#' The Advanced Microwave Scanning Radiometer (AMSR-2) is in current operation on
#' the Japan Aerospace Exploration Agency (JAXA) GCOM-W1 space craft, launched in
#' May 2012. Data are processed by Remote Sensing Systems. The satellite
#' completes an ascending and descending pass during local daytime and nighttime
#' hours respectively. Each daily file contains 7 daytime and 7 nighttime
#' maps of variables named as follows within the `data`
#' slot of amsr objects: `timeDay`,
#' `SSTDay`, `LFwindDay` (wind at 10m sensed in
#' the 10.7GHz band), `MFwindDay` (wind at 10m sensed at 18.7GHz),
#' `vaporDay`, `cloudDay`, and `rainDay`, along with
#' similarly-named items that end in `Night`.
#' See reference 1 for additional information on the instrument, how
#' to cite the data source in a paper, etc.
#'
#' The bands are stored in [raw()] form, to save storage. The accessor
#' function \code{\link{[[,amsr-method}} can provide these values in `raw`
#' form or in physical units; [plot,amsr-method()], and
#' [summary,amsr-method()] work with physical units.
#'
#' @templateVar class amsr
#'
#' @templateVar dataExample {}
#'
#' @templateVar metadataExample Examples that are of common interest include  `longitude` and `latitude`, which define the grid.
#'
#' @template slot_summary
#'
#' @template slot_put
#'
#' @template slot_get
#'
#' @author Dan Kelley and Chantelle Layton
#'
#' @references
#' 1. Information on the satellite, how to cite the data, etc. is
#' provided at `http://www.remss.com/missions/amsr/`.
#'
#' 2. A simple interface for viewing and downloading data is at
#' `http://images.remss.com/amsr/amsr2_data_daily.html`.
#'
#' @family classes holding satellite data
#' @family things related to amsr data
setClass("amsr", contains="satellite")

setMethod(f="initialize",
          signature="amsr",
          definition=function(.Object, filename, ...) {
              .Object <- callNextMethod(.Object, ...)
              if (!missing(filename))
                  .Object@metadata$filename <- filename
              .Object@processingLog$time <- presentTime()
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


#' An amsr dataset for waters near Nova Scotia
#'
#' This is a composite satellite image combining views for
#' 2020 August 9, 10 and 11, trimmed from a world view to a view
#' spanning 30N to 60N and 80W to 40W; see \dQuote{Details}.
#'
#' The following code was used to create this dataset.
#'\preformatted{
#' library(oce)
#' data(coastlineWorldFine, package="ocedata")
#' d1 <- read.amsr(download.amsr(2020, 8,  9, "~/data/amsr"))
#' d2 <- read.amsr(download.amsr(2020, 8, 10, "~/data/amsr"))
#' d3 <- read.amsr(download.amsr(2020, 8, 11, "~/data/amsr"))
#' d <- composite(d1, d2, d3)
#' amsr <- subset(d,    -80 < longitude & longitude < -40)
#' amsr <- subset(amsr,  30 < latitude  &  latitude <  60)
#'}
#'
#' @name amsr
#' @docType data
#'
#' @usage data(amsr)
#'
#' @examples
#' library(oce)
#' data(coastlineWorld)
#' data(amsr)
#' plot(amsr, "SST")
#' lines(coastlineWorld[["longitude"]], coastlineWorld[["latitude"]])
#'
#' @family satellite datasets provided with oce
#' @family datasets provided with oce
#' @family things related to amsr data
NULL


#' Summarize an amsr Object
#'
#' Although the data are stored in [raw()] form, the summary
#' presents results in physical units.
#'
#' @param object an [amsr-class] object.
#'
#' @param ... ignored.
#'
#' @author Dan Kelley
#'
#' @family things related to amsr data
setMethod(f="summary",
          signature="amsr",
          definition=function(object, ...) {
              cat("Amsr Summary\n------------\n\n")
              showMetadataItem(object, "filename",   "Data file:           ")
              cat(sprintf("* Longitude range:     %.4fE to %.4fE\n", object@metadata$longitude[1], tail(object@metadata$longitude, 1)))
              cat(sprintf("* Latitude range:      %.4fN to %.4fN\n", object@metadata$latitude[1], tail(object@metadata$latitude, 1)))
              for (name in names(object@data)) {
                  object@data[[name]] <- object[[name]] # translate to science units
              }
              invisible(callNextMethod())        # summary
          })

#' Extract Something From an amsr Object
#'
#' Extract something from the `metadata` or `data` slot of an [amsr-class] object.
#' Partial matches for `i`
#' are permitted for `metadata`, and `j` is ignored for
#' `metadata`.
#'
#' Data within the `data` slot may be found directly, e.g.
#' `i="SSTDay"` will yield sea-surface temperature in the daytime
#' satellite, and `i="SSTNight"` is used to access the nighttime data. In
#' addition, `i="SST"` yields a computed average of the night and day values
#' (using just one of these, if the other is missing). This scheme of
#' providing computed averages works for
#' all the data stored in `amsr` objects, namely:
#' `time`, `SST`, `LFwind`, `MFwind`,
#' `vapor`, `cloud` and `rain`.  In each case, the default
#' is to calculate values in scientific units, unless `j="raw"`, in
#' which case the raw data are returned.
#'
#' The conversion from raw to scientific units is done with formulae
#' found at `http://www.remss.com/missions/amsre`, e.g. SST is
#' computed by converting the raw value to an integer (between 0 and 255),
#' multiplying by 0.15C, and subtracting 3C.
#'
#' The `"raw"` mode can be useful
#' in decoding the various types of missing value that are used by `amsr`
#' data, namely `as.raw(255)` for land, `as.raw(254)` for
#' a missing observation, `as.raw(253)` for a bad observation,
#' `as.raw(252)` for sea ice, or `as.raw(251)` for missing SST
#' due to rain or missing water vapour due to heavy rain. Note that
#' something special has to be done for e.g. `d[["SST","raw"]]`
#' because the idea is that this syntax (as opposed to specifying
#' `"SSTDay"`) is a request to try to find good
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
#' with `NA` values inserted at locations where
#' the raw data equal `as.raw(251:255)`, as explained
#' in \dQuote{Details}.
#'
#' @param x an [amsr-class] object.
#'
#' @author Dan Kelley
#'
#' @template sub_subTemplate
#'
#' @examples
#' # Histogram of SST values
#' library(oce)
#' data(amsr)
#' hist(amsr[["SST"]])
#'
#' @family things related to amsr data
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
              # get numeric band, changing land, n-obs, bad-obs, sea-ice and windy to NA
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
                  else if (i == "time") res <- 60*6*getBand(do_amsr_average(x@data[["timeDay"]], x@data[["timeNight"]]))
                  else if (i == "SSTDay") res <- -3 + 0.15 * getBand(x@data[[i]])
                  else if (i == "SSTNight") res <- -3 + 0.15 * getBand(x@data[[i]])
                  else if (i == "SST") res <- -3 + 0.15 * getBand(do_amsr_average(x@data[["SSTDay"]], x@data[["SSTNight"]]))
                  else if (i == "LFwindDay") res <- 0.2 * getBand(x@data[[i]])
                  else if (i == "LFwindNight") res <- 0.2 * getBand(x@data[[i]])
                  else if (i == "LFwind") res <- 0.2 * getBand(do_amsr_average(x@data[["LFwindDay"]], x@data[["LFwindNight"]]))
                  else if (i == "MFwindDay") res <- 0.2 * getBand(x@data[[i]])
                  else if (i == "MFwindNight") res <- 0.2 * getBand(x@data[[i]])
                  else if (i == "MFwind") res <- 0.2 * getBand(do_amsr_average(x@data[["MFwindDay"]], x@data[["MFwindNight"]]))
                  else if (i == "vaporDay") res <- 0.3 * getBand(x@data[[i]])
                  else if (i == "vaporNight") res <- 0.3 * getBand(x@data[[i]])
                  else if (i == "vapor") res <- 0.3 * getBand(do_amsr_average(x@data[["vaporDay"]], x@data[["vaporNight"]]))
                  else if (i == "cloudDay") res <- -0.05 + 0.01 * getBand(x@data[[i]])
                  else if (i == "cloudNight") res <- -0.05 + 0.01 * getBand(x@data[[i]])
                  else if (i == "cloud") res <- -0.05 + 0.01 * getBand(do_amsr_average(x@data[["cloudDay"]], x@data[["cloudNight"]]))
                  else if (i == "rainDay") res <- 0.01 * getBand(x@data[[i]])
                  else if (i == "rainNight") res <- 0.01 * getBand(x@data[[i]])
                  else if (i == "rain") res <- 0.01 * getBand(do_amsr_average(x@data[["rainDay"]], x@data[["rainNight"]]))
                  else if (i == "data") return(x@data)
              } else {
                  if      (i == "timeDay") res <- x@data[[i]]
                  else if (i == "timeNight") res <- x@data[[i]]
                  else if (i == "time") res <- getBand(do_amsr_average(x@data[["timeDay"]], x@data[["timeNight"]]))
                  else if (i == "SSTDay") res <- x@data[[i]]
                  else if (i == "SSTNight") res <- x@data[[i]]
                  else if (i == "SST") res <- do_amsr_average(x@data[["SSTDay"]], x@data[["SSTNight"]])
                  else if (i == "LFwindDay") res <- x@data[[i]]
                  else if (i == "LFwindNight") res <- x@data[[i]]
                  else if (i == "LFwind") res <- do_amsr_average(x@data[["LFwindDay"]], x@data[["LFwindNight"]])
                  else if (i == "MFwindDay") res <- x@data[[i]]
                  else if (i == "MFwindNight") res <- x@data[[i]]
                  else if (i == "MFwind") res <- do_amsr_average(x@data[["MFwindDay"]], x@data[["MFwindNight"]])
                  else if (i == "vaporDay") res <- x@data[[i]]
                  else if (i == "vaporNight") res <- x@data[[i]]
                  else if (i == "vapor") res <- do_amsr_average(x@data[["vaporDay"]], x@data[["vaporNight"]])
                  else if (i == "cloudDay") res <- x@data[[i]]
                  else if (i == "cloudNight") res <- x@data[[i]]
                  else if (i == "cloud") res <- do_amsr_average(x@data[["cloudDay"]], x@data[["cloudNight"]])
                  else if (i == "rainDay") res <- x@data[[i]]
                  else if (i == "rainNight") res <- x@data[[i]]
                  else if (i == "rain") res <- do_amsr_average(x@data[["rainDay"]], x@data[["rainNight"]])
                  else if (i == "data") return(x@data)
              }
              dim(res) <- dim
              res
          })

#' Replace Parts of an amsr Object
#'
#' @param x an [amsr-class] object.
#'
#' @template sub_subsetTemplate
#'
#' @family things related to amsr data
setMethod(f="[[<-",
          signature(x="amsr", i="ANY", j="ANY"),
          definition=function(x, i, j, ..., value) {
              callNextMethod(x=x, i=i, j=j, ...=..., value=value) # [[<-
          })

#' Subset an amsr Object
#'
#' Return a subset of a [amsr-class] object.
#'
#' This function is used to subset data within an [amsr-class]
#' object by `longitude` or by `latitude`.  These two methods cannot
#' be combined in a single call, so two calls are required, as shown
#' in the Example.
#'
#' @param x an [amsr-class] object.
#'
#' @param subset an expression indicating how to subset `x`.
#'
#' @param ... ignored.
#'
#' @return An [amsr-class] object.
#'
#' @examples
#' library(oce)
#' data(amsr) # see ?amsr for how to read and composite such objects
#' sub <- subset(amsr, -75 < longitude & longitude < -45)
#' sub <- subset(sub,   40 < latitude  &  latitude <  50)
#' plot(sub)
#' data(coastlineWorld)
#' lines(coastlineWorld[['longitude']], coastlineWorld[['latitude']])
#'
#' @author Dan Kelley
#'
#' @family things related to amsr data
#' @family functions that subset oce objects
setMethod(f="subset",
          signature="amsr",
          definition=function(x, subset, ...) {
              dots <- list(...)
              debug <- if ("debug" %in% names(dots)) dots$debug else 0
              oceDebug(debug, "subset,amsr-method() {\n", style="bold", sep="", unindent=1)
              res <- x
              ## subsetString <- paste(deparse(substitute(subset)), collapse=" ")
              subsetString <- paste(deparse(substitute(expr=subset, env=environment())), collapse=" ")
              if (length(grep("longitude", subsetString))) {
                  if (length(grep("latitude", subsetString)))
                      stop("the subset must not contain both longitude and latitude. Call this twice, to combine these")
                  ##keep <- eval(expr=substitute(expr=subset, env=environment()), envir=data.frame(longitude=x@metadata$longitude))
                  keep <- eval(expr=substitute(expr=subset, env=environment()),
                               envir=data.frame(longitude=x@metadata$longitude), enclos=parent.frame(2))
                  oceDebug(debug, "keeping", sum(keep), "of", length(keep), "longitudes\n")
                  for (name in names(res@data)) {
                      oceDebug(debug, "processing", name, "\n")
                      res@data[[name]] <- res[[name, "raw"]][keep, ]
                  }
                  res@metadata$longitude <- x@metadata$longitude[keep]
              } else if (length(grep("latitude", subsetString))) {
                  if (length(grep("longitude", subsetString)))
                      stop("the subset must not contain both longitude and latitude. Call this twice, to combine these")
                  keep <- eval(expr=substitute(expr=subset, env=environment()),
                               envir=data.frame(latitude=x@metadata$latitude), enclos=parent.frame(2))
                  oceDebug(debug, "keeping", sum(keep), "of", length(keep), "latitudes\n")
                  for (name in names(res@data)) {
                      oceDebug(debug, "processing", name, "\n")
                      res@data[[name]] <- x[[name, "raw"]][, keep]
                  }
                  res@metadata$latitude <- res@metadata$latitude[keep]
              } else {
                  stop("may only subset by longitude or latitude")
              }
              res@processingLog <- processingLogAppend(res@processingLog, paste("subset(x, subset=", subsetString, ")", sep=""))
              oceDebug(debug, "} # subset,amsr-method()\n", style="bold", sep="", unindent=1)
              res
          })

#' Plot an amsr Object
#'
#' Plot an image of a component of an [amsr-class] object.
#'
#' In addition to fields named directly in the object, such as `SSTDay` and
#' `SSTNight`, it is also possible to plot computed fields, such as `SST`,
#' which combines the day and night fields.
#'
#' @param x an [amsr-class] object.
#'
#' @param y character value indicating the name of the band to plot; if not provided,
#' `SST` is used; see the documentation for the [amsr-class] class for a list of bands.
#'
#' @param asp optional numerical value giving the aspect ratio for plot.  The
#' default value, `NULL`, means to use an aspect ratio of 1 for world views,
#' and a value computed from `ylim`, if the latter is specified in the
#' `...` argument.
#'
#' @param breaks optional numeric vector of the z values for breaks in the color scheme.
#' If `colormap` is provided, it takes precedence over `breaks` and `col`.
#'
#' @param col optional argument, either a vector of colors corresponding to the breaks, of length
#' 1 less than the number of breaks, or a function specifying colors.
#' If neither `col` or `colormap` is provided, then `col` defaults to
#' [oceColorsTemperature()].
#' If `colormap` is provided, it takes precedence over `breaks` and `col`.
#'
#' @param colormap a specification of the colormap to use, as created
#' with [colormap()].  If `colormap` is NULL, which is the default, then
#' a colormap is created to cover the range of data values, using
#' [oceColorsTemperature] colour scheme.
#' If `colormap` is provided, it takes precedence over `breaks` and `col`.
#' See \dQuote{Examples} for an example of using the "turbo" colour scheme.
#'
#' @param zlim optional numeric vector of length 2, giving the limits
#' of the plotted quantity.  A reasonable default is computed, if this
#' is not given.
#'
#' @param missingColor List of colors for problem cases. The names of the
#' elements in this list must be as in the default, but the colors may
#' be changed to any desired values. These default values work reasonably
#' well for SST images, which are the default image, and which employ a
#' blue-white-red blend of colors, no mixture of which matches the
#' default values in `missingColor`.
#'
#' @param debug A debugging flag, integer.
#'
#' @param ... extra arguments passed to [imagep()], e.g. to control
#' the view with `xlim` (for longitude) and `ylim` (for latitude).
#'
#' @examples
#' library(oce)
#' data(coastlineWorld)
#' data(amsr) # see ?amsr for how to read and composite such objects
#'
#' # Example 1: plot with default colour scheme, oceColorsTemperature()
#' plot(amsr, "SST")
#' lines(coastlineWorld[['longitude']], coastlineWorld[['latitude']])
#'
#' # Example 2: 'turbo' colour scheme
#' plot(amsr, "SST", col=oceColorsTurbo)
#' lines(coastlineWorld[['longitude']], coastlineWorld[['latitude']])
#'
#' @author Dan Kelley
#'
#' @family things related to amsr data
#' @family functions that plot oce data
#'
#' @aliases plot.amsr
setMethod(f="plot",
          signature=signature("amsr"),
          ## FIXME: how to let it default on band??
          definition=function(x, y, asp=NULL,
                              breaks, col, colormap, zlim,
                              missingColor=list(land='papayaWhip',
                                                none='lightGray',
                                                bad='gray',
                                                rain='plum',
                                                ice='mediumVioletRed'),
                              debug=getOption("oceDebug"), ...)
          {
              dots <- list(...)
              oceDebug(debug, "plot.amsr(..., y=c(",
                       if (missing(y)) "(missing)" else y, ", ...) {\n", sep="", style="bold", unindent=1)
              zlimGiven <- !missing(zlim)
              if (missing(y))
                  y <- "SST"
              lon <- x[["longitude"]]
              lat <- x[["latitude"]]
              ## Examine ylim (if asp is not NULL) and also at
              #' both xlim and ylim to compute zrange
              xlim <- dots$xlim
              ylim <- dots$ylim
              if (is.null(asp)) {
                  if (!is.null(ylim)) {
                      asp <- 1 / cos(pi/180*abs(mean(ylim, na.rm=TRUE)))
                      oceDebug(debug, "inferred asp=", asp, " from ylim argument\n", sep="")
                  } else {
                      asp <- 1 / cos(pi/180*abs(mean(lat, na.rm=TRUE)))
                      oceDebug(debug, "inferred asp=", asp, " from ylim argument\n", sep="")
                  }
              } else {
                  oceDebug(debug, "using supplied asp=", asp, "\n", sep="")
              }
              z <- x[[y]]
              ## Compute zrange for world data, or data narrowed to xlim and ylim.
              if (!is.null(xlim)) {
                  if (!is.null(ylim)) {
                      oceDebug(debug, "computing range based on z trimmed by xlim and ylim\n")
                      zrange <- range(z[xlim[1] <= lon & lon <= xlim[2], ylim[1] <= lat & lat <= ylim[2]], na.rm=TRUE)
                  } else {
                      oceDebug(debug, "computing range based on z trimmed by xlim alone\n")
                      zrange <- range(z[xlim[1] <= lon & lon <= xlim[2], ], na.rm=TRUE)
                  }
              } else {
                  if (!is.null(ylim)) {
                      oceDebug(debug, "computing range based on z trimmed by ylim alone\n")
                      zrange <- range(z[, ylim[1] <= lat & lat <= ylim[2]], na.rm=TRUE)
                  } else {
                      oceDebug(debug, "computing range based on whole-world data\n")
                      zrange <- range(z, na.rm=TRUE)
                  }
              }
              oceDebug(debug, "zrange: ", paste(zrange, collapse=" to "), "\n")
              ## Determine colormap, if not given as an argument.
              if (missing(colormap)) {
                  oceDebug(debug, "case 1: 'colormap' not given, so will be computed here\n")
                  if (!missing(breaks)) {
                      oceDebug(debug, "case 1.1: 'breaks' was specified\n")
                      cat("FYI breaks are as follows:\n");print(breaks)
                      if (!missing(col)) {
                          oceDebug(debug, "case 1.1.1: computing colormap from specified breaks and specified col\n")
                          colormap <- oce::colormap(zlim=if (zlimGiven) zlim else range(breaks), col=col)
                      } else {
                          oceDebug(debug, "case 1.1.2: computing colormap from specified breaks and computed col\n")
                          colormap <- oce::colormap(zlim=if (zlimGiven) zlim else range(breaks), col=oceColorsTemperature)
                      }
                  } else {
                      oceDebug(debug, "case 1.2: 'breaks' was not specified\n")
                      if (!missing(col)) {
                          oceDebug(debug, "case 1.2.1: computing colormap from and computed breaks and specified col\n")
                          colormap <- oce::colormap(zlim=if (zlimGiven) zlim else zrange, col=col)
                      } else {
                          oceDebug(debug, "case 1.2.2: computing colormap from computed breaks and computed col\n")
                          colormap <- oce::colormap(zlim=if (zlimGiven) zlim else zrange, col=oceColorsTemperature)
                      }
                  }
              } else {
                  oceDebug(debug, "using specified colormap, ignoring breaks and col, whether they were supplied or not\n")
              }
              i <- if ("zlab" %in% names(dots)) {
                  oceDebug(debug, "calling imagep() with asp=", asp, ", and zlab=\"", dots$zlab, "\"\n", sep="")
                  imagep(lon, lat, z, colormap=colormap, asp=asp, debug=debug-1, ...)
              } else {
                  oceDebug(debug, "calling imagep() with asp=", asp, ", and no zlab argument\n", sep="")
                  imagep(lon, lat, z, colormap=colormap, zlab=y, asp=asp, debug=debug-1, ...)
              }
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
              if (!all(sort(names(missingColor))==sort(c("land", "none", "bad", "ice", "rain"))))
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
                  ##message("did code ", codes[[codeName]], " (color ", missingColor[[codeName]], ")")
              }
              box()
              oceDebug(debug, "} # plot.amsr()\n", sep="", style="bold", unindent=1)
          })


#' Download and Cache an amsr File
#'
#' If the file is already present in `destdir`, then it is not
#' downloaded again. The default `destdir` is the present directory,
#' but it probably makes more sense to use something like `"~/data/amsr"`
#' to make it easy for scripts in other directories to use the cached data.
#' The file is downloaded with [download.file()].
#'
#' @param year,month,day Numerical values of the year, month, and day
#' of the desired dataset. Note that one file is archived per day,
#' so these three values uniquely identify a dataset.
#' If `day` and `month` are not provided but `day` is,
#' then the time is provided in a relative sense, based on the present
#' date, with `day` indicating the number of days in the past.
#' Owing to issues with timezones and the time when the data
#' are uploaded to the server, `day=3` may yield the
#' most recent available data. For this reason, there is a
#' third option, which is to leave `day` unspecified, which
#' works as though `day=3` had been given.
#'
#' @param destdir A string naming the directory in which to cache the downloaded file.
#' The default is to store in the present directory, but many users find it more
#' helpful to use something like `"~/data/amsr"` for this, to collect all
#' downloaded amsr files in one place.
#' @param server A string naming the server from which data
#' are to be acquired. See \dQuote{History}.
#'
#' @section History:
#' Until 25 March 2017, the default server was
#' `"ftp.ssmi.com/amsr2/bmaps_v07.2"`, but this was changed when the author
#' discovered that this FTP site had been changed to require users to create
#' accounts to register for downloads.  The default was changed to
#' `"http://data.remss.com/amsr2/bmaps_v07.2"` on the named date.
#' This site was found by a web search, but it seems to provide proper data.
#' It is assumed that users will do some checking on the best source.
#'
#' On 23 January 2018, it was noticed that the server-url naming convention
#' had changed, e.g.
#' `http://data.remss.com/amsr2/bmaps_v07.2/y2017/m01/f34_20170114v7.2.gz`
#' becoming
#' `http://data.remss.com/amsr2/bmaps_v08/y2017/m01/f34_20170114v8.gz`
#'
#' @return A character value indicating the filename of the result; if
#' there is a problem of any kind, the result will be the empty
#' string.
#'
#' @examples
#'\dontrun{
#' ## The download takes several seconds.
#' f <- download.amsr(2017, 1, 14) # Jan 14, 2017
#' d <- read.amsr(f)
#' plot(d)
#' mtext(d[["filename"]], side=3, line=0, adj=0)
#'}
#'
#' @family functions that download files
#' @family functions that plot oce data
#' @family things related to amsr data
#'
#' @references
#' `http://images.remss.com/amsr/amsr2_data_daily.html`
#' provides daily images going back to 2012. Three-day,
#' monthly, and monthly composites are also provided on that site.
download.amsr <- function(year, month, day, destdir=".", server="http://data.remss.com/amsr2/bmaps_v08")
{
    ## ftp ftp://ftp.ssmi.com/amsr2/bmaps_v07.2/y2016/m08/f34_20160804v7.2.gz
    if (missing(year) && missing(month)) {
        if (missing(day))
            day <- 3
        day <- abs(day)
        today <- as.POSIXlt(Sys.Date() - day)
        year <- 1900 + today$year
        month <- 1 + today$mon
        day <- today$mday
    }
    year <- as.integer(year)
    month <- as.integer(month)
    day <- as.integer(day)
    destfile <- sprintf("f34_%4d%02d%02dv8.gz", year, month, day)
    destpath <- paste(destdir, destfile, sep="/")
    ## example
    ## http://data.remss.com/amsr2/bmaps_v07.2/y2015/m11/f34_20151101v7.2.gz
    if (tail(destpath, 1)=="/") # remove trailing slash
        destpath <- substr(destpath, 1, length(destpath)-1)
    if (0 == length(list.files(path=destdir, pattern=paste("^", destfile, "$", sep="")))) {
        source <- sprintf("%s/y%4d/m%02d/%s", server, year, month, destfile)
        bad <- download.file(source, destfile)
        if (!bad && destdir != ".")
            system(paste("mv", destfile, destpath))
    } else {
        message("Not downloading ", destfile, " because it is already present in ", destdir)
    }
    if (destdir == ".") destfile else destpath
}



#' Read an amsr File
#'
#' Read a compressed amsr file, generating an [amsr-class] object.
#' Note that only compressed files are read in this version.
#'
#' AMSR files are provided at the FTP site
#' `ftp://ftp.ssmi.com/amsr2/bmaps_v07.2/` and login as "guest",
#' enter a year-based directory (e.g. `y2016` for the year 2016),
#' then enter a month-based directory (e.g. `m08` for August, the 8th
#' month), and then download a file for the present date, e.g.
#' `f34_20160803v7.2.gz` for August 3rd, 2016. Do not uncompress
#' this file, since `read.amsr` can only read uncompressed files.
#' If `read.amsr` reports an error on the number of chunks, try
#' downloading a similarly-named file (e.g. in the present example,
#' `read.amsr("f34_20160803v7.2_d3d.gz")` will report an error
#' about inability to read a 6-chunk file, but
#' `read.amsr("f34_20160803v7.2.gz")` will work properly.
#'
#' @param file String indicating the name of a compressed file. See
#' \dQuote{File sources}.
#'
#' @param debug A debugging flag, integer.
#'
#' @seealso [plot,amsr-method()] for an example.
#'
#' @author Dan Kelley and Chantelle Layton
#'
#' @family things related to amsr data
read.amsr <- function(file, debug=getOption("oceDebug"))
{
    if (!missing(file) && is.character(file) && 0 == file.info(file)$size)
        stop("empty file")
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

#' Create a composite of amsr satellite data
#'
#' Form averages for each item in the `data` slot of the supplied objects,
#' taking into account the bad-data codes.
#'
#' If none of the objects has good
#' data at any particular pixel (i.e. particular latitude and longitude),
#' the resultant will have the bad-data code of the last item in the argument
#' list.
#' The metadata in the result are taken directly from the metadata of the
#' final argument, except that the filename is set to a comma-separated list
#' of the component filenames.
#'
#' @param object An [amsr-class] object.
#'
#' @param ... Other amsr objects.
#'
#' @family things related to amsr data
#'
#' @template compositeTemplate
setMethod("composite",
          c(object="amsr"),
          function(object, ...) {
              dots <- list(...)
              ndots <- length(dots)
              if (ndots < 1)
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
                  a[, , 1] <- object@data[[name]]
                  ##message("B")
                  for (idot in 1:ndots) {
                      ##message("C idot=", idot)
                      a[, , 1+idot] <- dots[[idot]]@data[[name]]
                      ##message("D idot=", idot)
                  }
                  ##message("E")
                  A <- do_amsr_composite(a, dim(a))
                  ##message("F")
                  res@data[[name]] <- A
              }
              res@metadata$filename <- filenames
              res
          })
