# vim:textwidth=80:expandtab:shiftwidth=4:softtabstop=4

#' Class to Store Coastline Data
#'
#' This class stores coastline data.
#'
#' @templateVar class coastline
#'
#' @templateVar dataExample The key items stored in this slot are `longitude` and `latitude`.
#'
#' @templateVar metadataExample {}
#'
#' @template slot_summary
#'
#' @template slot_put
#'
#' @template slot_get
#'
#' @family classes provided by oce
#' @family things related to coastline data
#'
#' @author Dan Kelley
setClass("coastline", contains="oce")


#' World Coastline
#'
#' This is a coarse resolution coastline at scale 1:110M, with 10,696 points,
#' suitable for world-scale plots plotted at a small size, e.g. inset diagrams.
#' Finer resolution coastline files are provided in the
#' `ocedata` package.
#'
#' @name coastlineWorld
#'
#' @docType data
#'
#' @section Installing your own datasets: Follow the procedure along the lines
#' described in \dQuote{Details}, where of course your source file will differ.
#' Also, you should change the name of the coastline object from
#' `coastlineWorld`, to avoid conflicts with the built-in dataset. Save
#' the `.rda` file to some directory of your choosing, e.g. perhaps
#' `/data/coastlines` or `~/data/coastlines` on a unix-type machine.
#' Then, whenever you need the file, use [load()] to load it.  Most
#' users find it convenient to do the loading in an [Rprofile()]
#' startup file.
#'
#' @source Downloaded from `https://www.naturalearthdata.com`, in
#' `ne_110m_admin_0_countries.shp` in July 2015, with an
#' update on December 16, 2017.
#' @family datasets provided with oce
#' @family things related to coastline data
NULL

setMethod(f="initialize",
          signature="coastline",
          definition=function(.Object, longitude=NULL, latitude=NULL, filename="", fillable=FALSE, ...) {
              .Object <- callNextMethod(.Object, ...)
              .Object@data$longitude <- longitude
              .Object@data$latitude <- latitude
              .Object@metadata$filename <- filename
              .Object@metadata$fillable <- fillable
              .Object@processingLog$time <- presentTime()
              .Object@processingLog$value <- "create 'coastline' object"
              return(.Object)
          })

#' Extract Something From a Coastline Object
#'
#' @param x a [coastline-class] object.
#'
#' @templateVar class coastline
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
#' no derived values are defined for [coastline-class] objects.
#'
#' * In many cases, the focus will be on the coastline trace
#' in longitude-latitude space, so `x[["longitude"]]`
#' and `x[["latitude"]]` are commonly used.
#'
#' @template sub_subTemplate
#'
#' @family things related to coastline data
#'
#' @author Dan Kelley
setMethod(f="[[",
          signature(x="coastline", i="ANY", j="ANY"),
          definition=function(x, i, j, ...) {
              if (i == "?")
                  return(list(metadata=sort(names(x@metadata)),
                          metadataDerived=NULL,
                          data=sort(names(x@data)),
                          dataDerived=NULL))
              callNextMethod()         # [[
          })

#' Replace Parts of a Coastline Object
#'
#' @param x a [coastline-class] object.
#'
#' @family things related to coastline data
#'
#' @template sub_subsetTemplate
#'
#' @author Dan Kelley
setMethod(f="[[<-",
          signature(x="coastline", i="ANY", j="ANY"),
          definition=function(x, i, j, ..., value) {
              callNextMethod(x=x, i=i, j=j, ...=..., value=value) # [[<-
          })

#' Subset a Coastline Object
#'
#' Subsets a coastline object according to limiting values for longitude
#' and latitude. This uses functions in the \CRANpkg{raster} package
#' for some calculations, and so it will fail unless that package is
#' installed.
#'
#' As illustrated in the \dQuote{Examples}, `subset` must be an
#' expression that indicates limits on `latitude` and
#' `longitude`. The individual elements are provided in R notation,
#' not mathematical notation (i.e. `30<latitude<60` would not work).
#' Ampersands must be used to combine the limits.  The simplest way
#' to understand this is to copy the example directly, and then modify
#' the stated limits. Note that `>` comparison is not permitted,
#' and that `<` is converted to `<=` in the calculation.
#' Similarly, `&&` is converted to `&`. Spaces in the
#' expression are ignored. For convenience, `longitude` and
#' and `latitude` may be abbreviated as `lon` and `lat`,
#' as in the \dQuote{Examples}.
#'
#' @param x a [coastline-class] object.
#'
#' @param subset An expression indicating how to subset `x`. See \dQuote{Details}.
#'
#' @param ... optional additional arguments, the only one of which is considered
#' is one named `debug`, an integer that controls the level of debugging. If
#' this is not supplied, `debug` is assumed to be 0, meaning no debugging. If
#' it is 1, the steps of determining the bounding box are shown. If it is 2 or larger,
#' then additional processing steps are shown, including the extraction of every
#' polygon involved in the final result.
#'
#' @return A `coastline` object.
#'
#' @examples
#' library(oce)
#' data(coastlineWorld)
#' ## Subset to a box centred on Nova Scotia, Canada
#' if (requireNamespace("sf")) {
#'     cl <- subset(coastlineWorld, -80<lon & lon<-50 & 30<lat & lat<60)
#'     ## The plot demonstrates that the trimming is as requested.
#'     plot(cl, clon=-65, clat=45, span=6000)
#'     rect(-80, 30, -50, 60, bg="transparent", border="red")
#' }
#' @family things related to coastline data
#' @family functions that subset oce objects
#'
#' @author Dan Kelley
#'
#' @aliases subset.coastline
setMethod(f="subset",
          signature="coastline",
          definition=function(x, subset, ...) {
              if (missing(subset))
                  stop("must give 'subset'")
              dots <- list(...)
              debug <- dots$debug
              if (is.null(debug))
                  debug <- options("oceDebug")$debug
              if (is.null(debug))
                  debug <- 0
              ## 's0' is a character string that we decompose to find W, E, S
              ## and N.  This is done in small steps because that might help in
              ## locating any bugs that might crop up. Note that the elements
              ## of the string are broken down to get W, E, S and N, and so
              ## we must start with some cleanup (e.g. removal of spaces,
              ## conversion of && to & and <= to <) for the pattern matching
              ## to work simply.
              s0 <- deparse(substitute(expr=subset, env=environment()), width.cutoff=500)
              oceDebug(debug, "subset,coastline-method(..., ", s0, ") {\n", unindent=1, sep="", style="bold")
              if (length(grep(">", s0)))
                  stop("the 'subset' may not contain the character '>'")
              s1 <- gsub(" ", "", s0) # remove all spaces
              oceDebug(debug, "s1='", s1, "'\n", sep="")
              s2 <- gsub("&&", "&", gsub("=", "", gsub("[ ]*", "", s1))) # && becomes &
              oceDebug(debug, "s2='", s2, "'\n", sep="")
              s3 <- gsub("<=", "<", s2) # <= becomes <
              oceDebug(debug, "s3='", s3, "'\n", sep="")
              s4 <- strsplit(s3, "&")[[1]]
              oceDebug(debug, "s4='", paste(s4, collapse="' '"), "'\n", sep="")
              E <- W <- S <- N <- NA
              for (ss in s4) {
                  s4 <- gsub("<=", "<", ss)
                  oceDebug(debug, "ss='", ss, "'\n", sep="")
                  if (length(grep("<lon", s4))) {
                      oceDebug(debug, "looking for W in '", s4, "'\n", sep="")
                      W <- as.numeric(strsplit(s4, "<")[[1]][1])
                  } else if (length(grep("lon[a-z]*<", s4))) {
                      oceDebug(debug, "looking for E in '", s4, "'\n", sep="")
                      E <- as.numeric(strsplit(s4, "<")[[1]][2])
                  } else if (length(grep("<lat", s4))) {
                      oceDebug(debug, "looking for S in '", s4, "'\n", sep="")
                      S <- as.numeric(strsplit(s4, "<")[[1]][1])
                  } else if (length(grep("lat[a-z]*<", s4))) {
                      oceDebug(debug, "looking for N in '", s4, "'\n", sep="")
                      N <- as.numeric(strsplit(s4, "<")[[1]][2])
                  }
              }
              if (is.na(W)) stop("could not determine western longitude limit")
              if (is.na(E)) stop("could not determine eastern longitude limit")
              if (is.na(S)) stop("could not determine southern latitude limit")
              if (is.na(N)) stop("could not determine northern latitude limit")
              oceDebug(debug, "W=", W, ", E=", E, ", S=", S, ", N=", N, "\n", sep="")
              res <- x
              cllon <- x[["longitude"]]
              cllat <- x[["latitude"]]
              if (!requireNamespace("sf", quietly=TRUE))
                  stop("\"sf\" package must be installed for this to work")
              box <- sf::st_polygon(list(outer=cbind(c(W,W,E,E,W), c(S,N,N,S,S))))
              owarn <- options("warn")$warn
              options(warn=-1)
              na <- which(is.na(cllon))
              nseg <- length(na)
              ##OLD nnew <- 0
              outlon <- NULL
              outlat <- NULL
              for (iseg in 2:nseg) {
                  oceDebug(debug, "iseg=", iseg, "\n")
                  look <- seq.int(na[iseg-1]+1, na[iseg]-1)
                  lon <- cllon[look]
                  if (any(is.na(lon))) stop("step 1: double lon NA at iseg=", iseg) # checks ok on coastlineWorld
                  lat <- cllat[look]
                  if (any(is.na(lat))) stop("step 1: double lat NA at iseg=", iseg) # checks ok on coastlineWorld
                  n <- length(lon)
                  if (n < 1) stop("how can we have no data?")
                  ## was using sp and raster, but this caused assert() errors
                  ## see https://github.com/dankelley/oce/issues/1657
                  C <- sf::st_polygon(list(outer=cbind(c(lon, lon[1]), c(lat, lat[1]))))
                  inside <- sf::st_intersection(box, C)
                  if (1 == length(inside)) {
                      outlon <- c(outlon, NA, inside[[1]][,1])
                      outlat <- c(outlat, NA, inside[[1]][,2])
                  }
              }
              ## }}}
              res@data$longitude <- outlon
              res@data$latitude <- outlat
              options(warn=owarn)
              res@processingLog <- processingLogAppend(res@processingLog,
                                                       paste("subset(x, ", s0, ")", sep=""))
              oceDebug(debug, "} # subset,coastline-method\n", unindent=1, sep="", style="bold")
              res
          })


#' Summarize a Coastline Object
#'
#' Summarizes coastline length, bounding box, etc.
#'
#' @param object a [coastline-class] object.
#'
#' @param \dots further arguments passed to or from other methods.
#'
#' @family things related to coastline data
#'
#' @author Dan Kelley
#'
#' @aliases summary.coastline
setMethod(f="summary",
          signature="coastline",
          definition=function(object, ...) {
              cat("Coastline Summary\n-----------------\n\n")
              showMetadataItem(object, "filename",                  "File: ", quote=TRUE)
              cat("* Number of points: ", length(object@data$latitude), ", of which ",
                  sum(is.na(object@data$latitude)), " are NA (e.g. separating islands).\n", sep="")
              invisible(callNextMethod())        # summary
          })

#' Coerce Data into a Coastline Object
#'
#' Coerces a sequence of longitudes and latitudes into a coastline dataset.
#' This may be used when [read.coastline()] cannot read a file, or
#' when the data have been manipulated.
#'
#' @param longitude the longitude in decimal degrees, positive east of
#' Greenwich, or a data frame with columns named `latitude` and
#' `longitude`, in which case these values are extracted from the data
#' frame and the second argument is ignored.
#'
#' @param latitude the latitude in decimal degrees, positive north of the
#' Equator.
#'
#' @param fillable boolean indicating whether the coastline can be drawn as a
#' filled polygon.
#'
#' @return a [coastline-class] object.
#'
#' @family things related to coastline data
#'
#' @author Dan Kelley
as.coastline <- function(longitude, latitude, fillable=FALSE)
{
    if (missing(longitude)) stop("must provide longitude")
    if (missing(latitude)) stop("must provide latitude")
    names <- names(longitude)
    if ("longitude" %in% names && "latitude" %in% names) {
        latitude <- longitude[["latitude"]]
        longitude <- longitude[["longitude"]]
    }
    n <- length(latitude)
    if (n != length(longitude))
        stop("Lengths of longitude and latitude must be equal")
    res <- new("coastline", longitude=longitude, latitude=latitude, fillable=fillable)
    res@processingLog <- processingLogAppend(res@processingLog, paste(deparse(match.call()), sep="", collapse=""))
    res
}


#' Plot a coastline Object
#'
#' This function plots a coastline.  An attempt is made to fill the space of
#' the plot, and this is done by limiting either the longitude range or the
#' latitude range, as appropriate, by modifying the eastern or northern limit,
#' as appropriate.
#'
#' If `longitudelim`, `latitudelim` and `projection` are all given,
#' then these arguments are passed to [mapPlot()] to produce the plot.
#' (The call uses `bg` for `col`, and uses `col`, `fill`
#' and `border` directly.) If the results need further customization,
#' users should use [mapPlot()] directly.
#'
#' If `projection` is provided without `longitudelim` or `latitudelim`,
#' then [mapPlot()] is still called, but `longitudelim` and
#' `latitudelim` are computed from `clongitude`, `clatitude` and `span`.
#'
#' If `projection` is not provided, much simpler plots are produced. These are
#' Cartesian, with aspect ratio set to minimize shape distortion at the central latitude.
#' Although these are crude, they have the benefit of always working, which cannot
#' be said of true map projections, which can be problematic in various ways owing
#' to difficulties in inverting projection calculations.
#'
#' To get an inset map inside another map, draw the first map, do
#' [`par`]`(new=TRUE)`, and then call [plot,coastline-method()] with a value of
#' `mar` that moves the inset plot to a desired location on the existing
#' plot, and with `bg="white"`.
#'
#' @param x a [coastline-class] object.
#'
#' @param xlab label for x axis
#'
#' @param ylab label for y axis
#'
#' @param showHemi logical indicating whether to show the hemisphere in axis
#' tick labels.
#'
#' @param asp Aspect ratio for plot.  The default is for `plot,coastline-method`
#' to set the aspect ratio to give natural latitude-longitude scaling somewhere
#' near the centre latitude on the plot. Often, it makes sense to set
#' `asp` yourself, e.g. to get correct shapes at 45N, use
#' `asp=1/cos(45*pi/180)`.  Note that the land mass is not symmetric about
#' the equator, so to get good world views you should set `asp=1` or set
#' `ylim` to be symmetric about zero. Any given value of `asp` is
#' ignored, if `clongitude` and `clatitude` are given (or
#' if the latter two are inferred from `projection`.
#'
#' @param clongitude,clatitude optional center latitude of map, in decimal
#' degrees.  If both `clongitude` and `clatitude` are provided,
#' or alternatively if they can be inferred from substrings `+lon_0`
#' and `+lat_0` in `projection`, then
#' any provided value of `asp` is ignored, and instead the plot aspect
#' ratio is computed based on the center latitude.  If `clongitude` and
#' `clatitude` are known, then `span` must also be provided, and
#' in this case, it is not permitted to also specify `longitudelim` and
#' `latitudelim`.
#'
#' @param span optional suggested diagonal span of the plot, in kilometers.
#' The plotted span is usually close to the suggestion, although the details
#' depend on the plot aspect ratio and other factors, so some adjustment may be
#' required to fine-tune a plot.  A value for `span` must be supplied, if
#' `clongitude` and `clatitude` are supplied
#' (or inferred from `projection`).
#'
#' @param lonlabels,latlabels optional vectors of longitude and latitude to
#' label on the sides of plot, passed to [mapPlot()] to control
#' axis labelling, for plots done with map projections (i.e. for
#' cases in which `projection` is not `NULL`).
#'
#' @param projection optional map projection to use (see
#' the [mapPlot()] argument of the same name).
#' If set to `FALSE` then no projection is used,
#' and the data are plotted in a cartesion frame, with aspect ratio set to
#' reduce distortion near the middle of the plot.  This option is useful if the
#' coastline produces spurious horizontal lines owing to islands crossing the
#' plot edges (a problem that plagues map projections).  If `projection`
#' is not set, a Mercator projection is used for latitudes below about 70
#' degrees, as if `projection="+proj=merc"` had been supplied, or a
#' Stereopolar one is used as if `projection="+proj=stere"`.  Otherwise,
#' `projection` must be a character string identifying a projection
#' accepted by [mapPlot()].
#'
#' @param expand numerical factor for the expansion of plot limits, showing
#' area outside the plot, e.g. if showing a ship track as a coastline, and then
#' an actual coastline to show the ocean boundary.  The value of `expand`
#' is ignored if either `xlim` or `ylim` is given.
#'
#' @param mgp 3-element numerical vector to use for [`par`]`("mgp")`, and also
#' for `par(mar)`, computed from this.  The default is tighter than the R
#' default, in order to use more space for the data and less for the axes.
#'
#' @param mar value to be used with [`par`]`("mar")`.
#'
#' @param bg optional color to be used for the background of the map.  This
#' comes in handy for drawing insets (see \dQuote{details}).
#'
#' @param fill a legacy parameter that will be permitted only temporarily; see
#' \dQuote{History}.
#'
#' @param type indication of type; may be `"polygon"`, for a filled polygon,
#' `"p"` for points, `"l"` for line segments, or `"o"` for points
#' overlain with line segments. See `color` for a note on how
#' the the value of `type` alters the meaning of the `color`
#' argument.
#'
#' @param border color used to indicate land (if `type="polygon"`) or
#' the coastline and international borders (if `type="l"`).
#'
#' @param col either the color for filling polygons (if `type="polygon"`)
#' or the color of the points and line segments (if `type="p"`,
#' `type="l"`, or `type="o"`).
#'
#' @param axes boolean, set to `TRUE` to plot axes.
#'
#' @param cex.axis value for axis font size factor.
#'
#' @param add boolean, set to `TRUE` to draw the coastline on an existing
#' plot.  Note that this retains the aspect ratio of that existing plot, so it
#' is important to set that correctly, e.g. with \code{asp=1/cos(lat * pi /
#' 180)}, where `clat` is the central latitude of the plot.
#'
#' @param inset set to `TRUE` for use within [plotInset()].  The
#' effect is to prevent the present function from adjusting margins, which is
#' necessary because margin adjustment is the basis for the method used by
#' [plotInset()].
#'
#' @param geographical flag indicating the style of axes.  With
#' `geographical=0`, the axes are conventional, with decimal degrees as
#' the unit, and negative signs indicating the southern and western
#' hemispheres.  With `geographical=1`, the signs are dropped, with axis
#' values being in decreasing order within the southern and western
#' hemispheres.  With `geographical=2`, the signs are dropped and the axes
#' are labelled with degrees, minutes and seconds, as appropriate, and
#' hemispheres are indicated with letters. With `geographical=3`, things
#' are the same as for `geographical=2`, but the hemisphere indication
#' is omitted. Finally, with `geographical=4`, unsigned numbers are used,
#' followed by letters `N` in the northern hemisphere, `S` in the southern,
#' `E` in the eastern, and `W` in the western.
#'
#' @param longitudelim this and `latitudelim` provide a second way to
#' suggest plot ranges. Note that these may not be supplied if
#' `clongitude`, `clatitude` and `span` are given.
#'
#' @param latitudelim see `longitudelim`.
#'
#' @param debug set to `TRUE` to get debugging information during
#' processing.
#'
#' @param \dots optional arguments passed to plotting functions.  For example,
#' set `yaxp=c(-90,90,4)` for a plot extending from pole to pole.
#'
#' @return None.
#'
#' @section History: Until February, 2016, `plot,coastline-method` relied on a
#' now-defunct argument `fill` to control colors; `col` is to be
#' used now, instead.
#'
#' @seealso The documentation for the [coastline-class] class explains the
#' structure of coastline objects, and also outlines the other functions
#' dealing with them.
#'
#' @examples
#'\donttest{
#' library(oce)
#' par(mar=c(2, 2, 1, 1))
#' data(coastlineWorld)
#' plot(coastlineWorld)
#' plot(coastlineWorld, clongitude=-63.6, clatitude=44.6, span=1000)
#'
#' ## Canada in Lambert projection
#' plot(coastlineWorld, clongitude=-95, clatitude=65, span=5500,
#'      grid=10, projection='+proj=laea +lon_0=-100 +lat_0=55')
#'}
#'
#' @family functions that plot oce data
#' @family things related to coastline data
#'
#' @author Dan Kelley
#'
#' @aliases plot.coastline
setMethod(f="plot",
          signature=signature("coastline"),
          definition=function (x,
                               xlab="", ylab="", showHemi=TRUE,
                               asp,
                               clongitude, clatitude, span,
                               lonlabels=TRUE, latlabels=TRUE,
                               projection=NULL,
                               expand=1,
                               mgp=getOption("oceMgp"), mar=c(mgp[1]+1, mgp[1]+1, 1, 1),
                               bg,
                               fill, # just so we catch it's use ... will be removed at some later time
                               type="polygon",
                               border=NULL, col=NULL, # OLD: border and col did not exist
                               axes=TRUE, cex.axis=par('cex.axis'),
                               add=FALSE, inset=FALSE,
                               geographical=0,
                               longitudelim, latitudelim, # for old usage
                               debug=getOption("oceDebug"),
                               ...)
          {
              if (!missing(projection) && inherits(projection, "CRS"))
                  projection <- projection@projargs
              oceDebug(debug, "plot,coastline-method(...",
                       ", clongitude=", if (missing(clongitude)) "(missing)" else clongitude,
                       ", clatitude=", if (missing(clatitude)) "(missing)" else clatitude,
                       ", span=", if (missing(span)) "(missing)" else span,
                       ", type=\"", type, "\"",
                       ", border=\"", if (is.null(border)) "NULL" else border, "\"",
                       ", col=\"", if (is.null(col)) "NULL" else col, "\"",
                       ", geographical=", geographical,
                       ", projection=\"", if (is.null(projection)) "NULL" else projection, "\"",
                       ", cex.axis=", cex.axis,
                       ", inset=", inset,
                       ", ...) {\n", sep="", unindent=1, style="bold")
              if (missing(clongitude) && !missing(projection) && length(grep("+lon_0", projection))) {
                  clongitude <- as.numeric(gsub(".*\\+lon_0=([^ ]*).*", "\\1", projection))
                  oceDebug(debug, "inferred clongitude=", clongitude, " from projection\n")
              }
              if (missing(clatitude) && !missing(projection) && length(grep("+lat_0", projection))) {
                  clatitude <- as.numeric(gsub(".*\\+lat_0=([^ ]*).*", "\\1", projection))
                  oceDebug(debug, "inferred clatitude=", clatitude, " from projection\n")
              }
              dots <- list(...)
              ##> message("fill: ", if (missing(fill)) "MISSING" else fill)
              ##> message("col: ", if (missing(col)) "MISSING" else col)
              if (!missing(fill)) {
                  ## permit call as documented before 2016-02-03
                  ## Note: the code permitted fill=TRUE but this was never documented
                  if (is.character(fill)) {
                      col <- fill
                  } else {
                      if (is.logical(fill) && !fill) {
                          col <- NULL
                      }
                  }
                  warning("In plot,coastline-method() : 'fill' being accepted for backwards compatibility; please use 'border' and 'col' instead", call.=FALSE)
              }
              ##> message("fill: ", if (missing(fill)) "MISSING" else fill)
              ##> message("col: ", if (missing(col)) "MISSING" else col)
              ##>> if (is.character(col)) {
              ##>>     if (!("fillable" %in% names(x@metadata) && x@metadata$fillable)) {
              ##>>         col <- NULL
              ##>>         warning("In plot,coastlinemethod() : setting col=NULL because the coastline is not fillable", call.=FALSE)
              ##>>     }
              ##>> }
              if (inherits(x, "coastline") && !missing(longitudelim) && !missing(latitudelim) && !missing(projection)) {
                  oceDebug(debug, "plot,coastline-method calling mapPlot (code location 1)\n")
                  mapPlot(x[["longitude"]], x[["latitude"]], projection=projection,
                          longitudelim=longitudelim, latitudelim=latitudelim,
                          bg=col, col=if (missing(fill)) "lightgray" else fill, border=border, debug=debug-1)
                  return(invisible(NULL))
              }
              if (!missing(clongitude))
                  if (clongitude > 180) clongitude <- clongitude - 360
              if (!missing(longitudelim) || !missing(latitudelim)) {
                  if (missing(longitudelim) || missing(latitudelim))
                      stop("In plot,coastline-method() : if either longitudelim or latitudelim are given, both must be given", call.=FALSE)
                  if (!missing(clongitude) || !missing(clatitude) || !missing(span))
                      stop("In plot,coastline-method() : if longitudelim or latitudelim are given, then clongitude, clatitude, and span may not be given", call.=FALSE)
                  ##message("A")
                  clongitude <- mean(longitudelim)
                  clatitude <- mean(latitudelim)
                  span <- geodDist(min(longitudelim), min(latitudelim), max(longitudelim), max(latitudelim))
                  warning("In plot,coastline-method() : converting longitudelim and latitudelim to clongitude=",
                          round(clongitude, 4),
                          ", clatitude=", round(clatitude, 4), " and span=", round(span, 0), "\n", call.=FALSE)
              }
              if (!is.null(projection)) {
                  if (missing(span))
                      span <- 1000
                  if (missing(clongitude))
                      longitudelim <- c(-180, 180)
                  else {
                      if (abs(clatitude) > 80)
                          longitudelim <- c(-180, 180)
                      else {
                          longitudelim <- clongitude + c(-1, 1) * span / 111 / 2 / cos(abs(clatitude)*pi/180) # FIXME: makes no sense
                          #browser()
                      }
                  }
                  ## if (longitudelim[1] < -180) longitudelim[1] <- -180
                  ## if (longitudelim[2] >  180) longitudelim[2] <- 180
                  if (missing(clatitude))
                      latitudelim <- c(-90, 90)
                  else
                      latitudelim <- clatitude + c(-1, 1) * span / 111 / 2
                  oceDebug(debug, "longitudelim=", paste(longitudelim, collapse=" "), "\n")
                  oceDebug(debug, "latitudelim=", paste(latitudelim, collapse=" "), "\n")
                  oceDebug(debug, "plot,coastline-method calling mapPlot (code location 2)\n")
                  ## BOOKMARK 1: can we longitudelim better?
                  mapPlot(x[['longitude']], x[['latitude']], longitudelim, latitudelim,
                          showHemi=showHemi,
                          mgp=mgp, mar=mar,
                          bg="white", border=border, col=col, type=type, axes=TRUE, ## FIXME: use bg and col here; delete fill
                          lonlabels=lonlabels, latlabels=latlabels,
                          projection=projection,
                          debug=debug-1, ...)
                  oceDebug(debug, "} # plot.coastline()\n", unindent=1, style="bold")
                  return(invisible())
              }
              geographical <- round(geographical)
              if (geographical < 0 || geographical > 4)
                  stop("argument geographical must be an integer between 0 to 4, inclusive")
              if (is.list(x) && "latitude" %in% names(x)) {
                  if (!("longitude" %in% names(x)))
                      stop("list must contain item named 'longitude'")
                  x <- as.coastline(x$latitude, x$longitude)
              } else {
                  if (!inherits(x, "coastline"))
                      stop("method is only for coastline objects, or lists that contain 'latitude' and 'longitude'")
              }
              longitude <- x[["longitude"]]
              latitude <- x[["latitude"]]
              ##dots <- list(...)
              dotsNames <- names(dots)
              ##gave.center <- !missing(clongitude) && !missing(clatitude)
              if ("center" %in% dotsNames)
                  stop("use 'clongitude' and 'clatitude' instead of 'center'")
              if ("xlim" %in% dotsNames) stop("do not specify 'xlim'; give 'clongitude' and 'span' instead")
              if ("ylim" %in% dotsNames) stop("do not specify 'ylim'; give 'clatitude' and 'span' instead")
              if (!inset)
                  par(mar=mar)
              par(mgp=mgp)
              if (add) {
                  ##> if (is.character(fill) && (!is.null(x@metadata$fillable) && x@metadata$fillable)) {
                  ## FIXME: handle 'type' values 'p', 'l' and 'o' here
                  warning("BUG: ignoring 'type' because add=TRUE (FIXME)\n")
                  polygon(longitude, latitude, border=border, col=col, ...)
                  if (axes) {
                      b <- par("usr")
                      b[1] <- max(-180, b[1])
                      b[2] <- min(180, b[2])
                      b[3] <- max(-90, b[3])
                      b[4] <- min(90, b[4])
                      rect(b[1], b[3], b[2], b[4])
                  }
                  ##> } else {
                  ##>     lines(longitude, latitude, ...)
                  ##> }
              } else {
                  ##gaveSpan <- !missing(span)
                  if (!missing(clatitude) && !missing(clongitude)) {
                      if (!missing(asp))
                          warning("argument 'asp' being ignored, because argument 'clatitude' and 'clongitude' were given")
                      asp <- 1 / cos(clatitude * atan2(1, 1) / 45) #  ignore any provided asp, because lat from center over-rides it
                      xr <- clongitude + sqrt(1/2) * span * c(-1/2, 1/2) / 111.11 / asp
                      yr <- clatitude + sqrt(1/2) * span * c(-1/2, 1/2) / 111.11
                      xr0 <- xr
                      yr0 <- yr
                      oceDebug(debug, "xr=", xr, " yr=", yr, "span=", span, "\n")
                      oceDebug(debug, "corner-to-corner span=", geodDist(xr[1], yr[1], xr[2], yr[2]), " km\n")
                  } else {
                      xr0 <- range(longitude, na.rm=TRUE)
                      yr0 <- range(latitude, na.rm=TRUE)
                      oceDebug(debug, "xr0=", xr0, " yr0=", yr0, "\n")
                      if (missing(asp)) {
                          if ("ylim" %in% dotsNames)
                              asp <- 1 / cos(mean(range(dots$ylim, na.rm=TRUE)) * atan2(1, 1) / 45) # dy/dx
                          else
                              asp <- 1 / cos(mean(yr0) * atan2(1, 1) / 45) # dy/dx
                      }
                      ## Expand
                      if (missing(span)) {
                          if (expand >= 0 && max(abs(xr0)) < 100 && max(abs(yr0) < 70)) {
                              ## don't expand if full map
                              xr <- mean(xr0) + expand * diff(xr0) * c(-1/2, 1/2)
                              yr <- mean(yr0) + expand * diff(yr0) * c(-1/2, 1/2)
                          } else {
                              xr <- xr0
                              yr <- yr0
                          }
                      } else {
                          xr <- mean(xr0) + span * c(-1/2, 1/2) / 111.11 / asp
                          yr <- mean(yr0)+ span * c(-1/2, 1/2) / 111.11
                      }
                      oceDebug(debug, "xr=", xr, " yr=", yr, "\n")
                  }
                  ## Trim lat or lon, to avoid empty margin space
                  ##> if (FALSE) {
                  ##>     ## disable for issue 677 (as a test, or maybe permanently)
                  ##>     asp.page <- par("fin")[2] / par("fin")[1] # dy / dx
                  ##>     oceDebug(debug, "par('pin')=", par('pin'), "\n")
                  ##>     oceDebug(debug, "par('fin')=", par('fin'), "\n")
                  ##>     oceDebug(debug, "asp=", asp, "\n")
                  ##>     oceDebug(debug, "asp.page=", asp.page, "\n")
                  ##>     if (!is.finite(asp))
                  ##>         asp <- 1 / cos(clatitude * atan2(1, 1) / 45)
                  ##>     if (asp < asp.page) {
                  ##>         oceDebug(debug, "type 1 (will narrow x range)\n")
                  ##>         d <- asp.page / asp * diff(xr)
                  ##>         oceDebug(debug, "  xr original:", xr, "\n")
                  ##>         xr <- mean(xr) + d * c(-1/2, 1/2)
                  ##>         oceDebug(debug, "  xr narrowed:", xr, "\n")
                  ##>     } else {
                  ##>         oceDebug(debug, "type 2 (will narrow y range)\n")
                  ##>         d <- asp.page / asp * diff(yr)
                  ##>         oceDebug(debug, "  yr original:", yr, ", yielding approx span", 111*diff(yr),
                  ##>                  "km\n")
                  ##>         yr <- mean(yr) + d * c(-1/2, 1/2)
                  ##>         oceDebug(debug, "  yr narrowed:", yr, "\n")
                  ##>         oceDebug(debug, "corner-to-corner span=", geodDist(xr[1], yr[1], xr[2], yr[2]), " km\n")
                  ##>     }
                  ##> }
                  ## Avoid looking beyond the poles, or the dateline
                  if (xr[1] < (-180)) {
                      xr[1] <- (-180)
                  }
                  if (xr[2] >  180) {
                      xr[2] <- 180
                  }
                  if (yr[1] <  (-90)) {
                      yr[1] <- (-90)
                  }
                  if (yr[2] >  90) {
                      yr[2] <- 90
                  }
                  oceDebug(debug, "after range trimming, xr=", xr, " yr=", yr, "\n")
                  oceDebug(debug, "corner-to-corner span=", geodDist(xr[1], yr[1], xr[2], yr[2]), " km\n")
                  ## Draw underlay, if desired
                  plot(xr, yr, asp=asp, xlab=xlab, ylab=ylab, type="n", xaxs="i", yaxs="i", axes=FALSE, ...)
                  if (!missing(bg)) {
                      plot.window(xr, yr, asp=asp, xlab=xlab, ylab=ylab, xaxs="i", yaxs="i", log="", ...)
                      usr <- par("usr")
                      oceDebug(debug, "drawing background bg=", bg, vectorShow(par('usr')))
                      ## polygon(usr[c(1,2,2,1)], usr[c(3,3,4,4)], col=bg)
                      rect(usr[1], usr[3], usr[2], usr[4], col=bg)
                      par(new=TRUE)
                  }
                  ## Ranges
                  ##plot(xr, yr, asp=asp, xlab=xlab, ylab=ylab, type="n", xaxs="i", yaxs="i", axes=FALSE, ...)
                  usrTrimmed <- par('usr')
                  ## Construct axes "manually" because axis() does not know the physical range
                  if (axes) {
                      prettyLat<-function(yr, ...)
                      {
                          res <- pretty(yr, ...)
                          if (diff(yr) > 100)
                              res <- seq(-90, 90, 45)
                          res
                      }
                      prettyLon<-function(xr, ...)
                      {
                          res <- pretty(xr, ...)
                          if (diff(xr) > 300)
                              res <- seq(-180, 180, 45)
                          res
                      }
                      oceDebug(debug, vectorShow(par('usr')))
                      oceDebug(debug, "xr:", xr, ", yr:", yr, ", xr0:", xr0, ", yr0:", yr0, "\n")
                      ##xr.pretty <- prettyLon(xr, n=if (geographical)3 else 5, high.u.bias=20)
                      xr.pretty <- prettyLon(par('usr')[1:2], n=if (geographical>0) 3 else 5, high.u.bias=20)
                      ##yr.pretty <- prettyLat(yr, n=if (geographical)3 else 5, high.u.bias=20)
                      yr.pretty <- prettyLat(par('usr')[3:4], n=if (geographical>0) 3 else 5, high.u.bias=20)
                      oceDebug(debug, vectorShow(xr.pretty))
                      oceDebug(debug, vectorShow(yr.pretty))
                      oceDebug(debug, vectorShow(usrTrimmed, postscript=" (original)\n"))
                      usrTrimmed[1] <- max(-180, usrTrimmed[1])
                      usrTrimmed[2] <- min( 180, usrTrimmed[2])
                      usrTrimmed[3] <- max( -90, usrTrimmed[3])
                      usrTrimmed[4] <- min(  90, usrTrimmed[4])
                      oceDebug(debug, vectorShow(usrTrimmed, postscript=" (after trimming)\n"))
                      ## Go to full-world, if we are close to full world.  This ensures we will
                      ## get ticks at the extrema, which is useful because the coastline doesn't go
                      ## close enough to the north pole to get a +90 tick.
                      usr12r <- range(usrTrimmed[1:2])
                      if (diff(usrTrimmed[1:2]) > 340) {
                          xr.pretty <- seq(-180, 180, 45)
                      } else {
                          xr.pretty <- prettyLon(usrTrimmed[1:2], n=if (geographical>0) 3 else 5, high.u.bias=20)
                      }
                      if (diff(usrTrimmed[3:4]) > 160) {
                          yr.pretty <- seq(-90, 90, 45)
                      } else {
                          yr.pretty <- prettyLon(usrTrimmed[3:4], n=if (geographical>0) 3 else 5, high.u.bias=20)
                      }
                      if (geographical >= 1) {
                          xlabels <- sub("-", "", xr.pretty)
                          ylabels <- sub("-", "", yr.pretty)
                      }
                      if (geographical == 0) {
                          xlabels <- xr.pretty
                          ylabels <- yr.pretty
                      } else if (geographical == 1) {
                          xlabels <- abs(xr.pretty)
                          ylabels <- abs(yr.pretty)
                      } else if (geographical == 2 || geographical == 3) {
                          xlabels <- formatPosition(xr.pretty, isLat=FALSE, type='expression',
                                                    showHemi=geographical==3)
                          ylabels <- formatPosition(yr.pretty, isLat=TRUE, type='expression',
                                                    showHemi=geographical==3)
                      } else if (geographical == 4) {
                          xlabels <- paste0(abs(xr.pretty),
                                            ifelse(xr.pretty > 0, "E",
                                                   ifelse(xr.pretty < 0, "W", "")))
                          ylabels <- paste0(abs(yr.pretty),
                                            ifelse(yr.pretty > 0, "N",
                                                   ifelse(yr.pretty < 0, "S", "")))
                      }
                      ##cat("xr.pretty:", paste(xr.pretty, collapse=","),"\n",sep="")
                      ##cat("xlabels: '", paste(xlabels, collapse="','"),"'\n",sep="")
                      axis(1, at=xr.pretty, labels=xlabels, pos=usrTrimmed[3], cex.axis=cex.axis)
                      oceDebug(debug, "putting bottom x axis at", usrTrimmed[3], "with labels:", xlabels, "\n")
                      axis(2, at=yr.pretty, labels=ylabels, pos=usrTrimmed[1], cex.axis=cex.axis, cex=cex.axis)
                      oceDebug(debug, "putting left y axis at", usrTrimmed[1], "\n")
                      axis(3, at=xr.pretty, labels=rep("", length.out=length(xr.pretty)), pos=usrTrimmed[4], cex.axis=cex.axis)
                      ##axis(3, at=xr.pretty, pos=usrTrimmed[4], labels=FALSE)
                      ##oceDebug(debug, "putting top x axis at", usrTrimmed[4], "\n")
                      axis(4, at=yr.pretty, pos=usrTrimmed[2], labels=FALSE, cex.axis=cex.axis)
                      oceDebug(debug, "putting right y axis at", usrTrimmed[2], "\n")
                  }
                  yaxp <- par("yaxp")
                  oceDebug(debug, vectorShow(par("yaxp")))
                  oceDebug(debug, vectorShow(par("pin")))
                  if (yaxp[1] < -90 | yaxp[2] > 90) {
                      oceDebug(debug, "should trim poles\n")
                  }
                  if (type == "polygon") {
                      if (is.null(border))
                          border <- "black"
                      if (is.null(col))
                          col <- "lightgray"
                      polygon(longitude, latitude, border=border, col=col, ...)
                      if (axes) {
                          b <- par("usr")
                          b[1] <- max(-180, b[1])
                          b[2] <- min(180, b[2])
                          b[3] <- max(-90, b[3])
                          b[4] <- min(90, b[4])
                          rect(b[1], b[3], b[2], b[4])
                      }
                  } else {
                      if (is.null(col))
                          col <- "black"
                      if (type == "l") {
                          lines(longitude, latitude, col=col, ...)
                      } else if (type == "p") {
                          points(longitude, latitude, col=col, ...)
                      } else if (type == "o") {
                          points(longitude, latitude, col=col, ...)
                          lines(longitude, latitude, col=col, ...)
                      }
                  }
              }
              ##box()
              oceDebug(debug, vectorShow(par('usr')))
              oceDebug(debug, "} # plot.coastline()\n", unindent=1, style="bold")
              invisible()
          })

#' Download a coastline File
#'
#' Constructs a query to the NaturalEarth server (see reference 1) to download coastline
#' data (or lake data, river data, etc) in any of three resolutions.
#'
#' @param resolution A character value specifying the desired resolution. The permitted
#' choices are `"10m"` (for 1:10M resolution, the most detailed),
#' `"50m"` (for 1:50M resolution)
#' and `"110m"` (for 1:110M resolution). If `resolution` is not supplied,
#' `"50m"` will be used.
#'
#' @param item A character value indicating the quantity to be downloaded.
#' This is normally one of `"coastline"`, `"land"`, `"ocean"`,
#' `"rivers_lakes_centerlines"`, or `"lakes"`, but the NaturalEarth
#' server has other types, and advanced users can discover their names by inspecting
#' the URLs of links on the NaturalEarth site, and use them for `item`.
#' If `item` is not supplied, it defaults to `"coastline"`.
#'
#' @template downloadDestTemplate
#'
#' @param server A character value specifying the server that is to supply
#' the data. At the moment, the only permitted value is `"naturalearth"`,
#' which is the default if `server` is not supplied.
#'
#' @template debugTemplate
#'
#' @return A character value indicating the filename of the result; if
#' there is a problem of any kind, the result will be the empty
#' string.
#'
#' @seealso The work is done with [utils::download.file()].
#'
#' @examples
#'\dontrun{
#' library(oce)
#' # User must create directory ~/data/coastline first.
#' # As of September 2016, the downloaded file, named
#' # "ne_50m_coastline.zip", occupies 443K bytes.
#' filename <- download.coastline(destdir="~/data/coastline")
#' coastline <- read.coastline(filename)
#' plot(coastline)
#'}
#'
#' @references
#' 1. The NaturalEarth server is at `https://www.naturalearthdata.com`
#'
#' @family functions that download files
#' @family things related to coastline data
#'
#' @author Dan Kelley
download.coastline <- function(resolution, item="coastline",
                           destdir=".", destfile,
                           server="naturalearth",
                           debug=getOption("oceDebug"))
{
    if (missing(resolution))
        resolution <- "50m"
    resolutionChoices <- c("10m", "50m", "110m")
    if (!(resolution %in% resolutionChoices))
        stop("'resolution' must be one of: '", paste(resolutionChoices, collapse="' '"), "'")
    if (server == "naturalearth")
        urlBase <- "https://www.naturalearthdata.com/downloads"
    else
        stop("the only server that works is naturalearth")
    filename <- paste("ne_", resolution, "_", item, ".zip", sep="")
    if (missing(destfile))
        destfile <- filename
    url <- paste(urlBase, "/", resolution, "/physical/", filename, sep="")
    destination <- paste(destdir, destfile, sep="/")
    if (1 == length(list.files(path=destdir, pattern=paste("^", destfile, "$", sep="")))) {
        oceDebug(debug, "Not downloading", destfile, "because it is already present in", destdir, "\n")
    } else {
        download.file(url, destination)
        oceDebug(debug, "Downloaded file stored as '", destination, "'\n", sep="")
    }
    ## The following is a sample URL, from which I reverse-engineered the URL construction.
    ##    https://www.naturalearthdata.com/http//www.naturalearthdata.com/download/50m/physical/ne_50m_lakes.zip
    destination
}

#' Read a Coastline File
#'
#' Read a coastline file in R, Splus, mapgen, shapefile, or openstreetmap
#' format.
#' The S and R formats are identical, and consist of two columns, lon and lat,
#' with land-jump segments separated by lines with two NAs.
#' The MapGen format is of the form \preformatted{ # -b -16.179081 28.553943
#' -16.244793 28.563330 } BUG: the 'arc/info ungenerate' format is not yet
#' understood.
#'
#' @param file name of file containing coastline data.
#'
#' @param type type of file, one of `"R"`, `"S"`, `"mapgen"`,
#' `"shapefile"` or `"openstreetmap"`.
#'
#' @param debug set to TRUE to print information about the header, etc.
#'
#' @param monitor print a dot for every coastline segment read (ignored except
#' for reading "shapefile" type)
#'
#' @param processingLog if provided, the action item to be stored in the log.
#' (Typically only provided for internal calls; the default that it provides is
#' better for normal calls by a user.)
#'
#' @return a [coastline-class] object.
#'
#' @author Dan Kelley
read.coastline <- function(file,
    type=c("R", "S", "mapgen", "shapefile", "openstreetmap"),
    debug=getOption("oceDebug"),
    monitor=FALSE,
    processingLog)
{
    if (missing(file))
        stop("must supply 'file'")
    if (is.character(file)) {
        if (!file.exists(file))
            stop("cannot find file '", file, "'")
        if (0L == file.info(file)$size)
            stop("empty file '", file, "'")
    }
    type <- match.arg(type)
    oceDebug(debug, "read.coastline(file=\"", file, "\", type=\"", type, "\", ...) {\n", sep="", unindent=1, style="bold")
    file <- fullFilename(file)
    if (is.character(file)) {
        if (1 == length(grep(".zip$", file)))
            return(read.coastline.shapefile(file, debug=debug))
        else
            filename <- file
    } else {
        filename <- "(unknown)"
    }
    if (type == "shapefile") {
        res <- read.coastline.shapefile(file, monitor=monitor, debug=debug, processingLog=processingLog)
    } else if (type == "openstreetmap") {
        res <- read.coastline.openstreetmap(file, monitor=monitor, debug=debug, processingLog=processingLog)
    } else if (type == "R" || type == "S") {
        ##
        ## e.g. data from http://rimmer.ngdc.noaa.gov/coast/
        ## e.g. "~/data/Coastline/wcl_1_5000000.dat")
        if (is.character(file)) {
            file <- fullFilename(file)
            file <- file(file, "r")
            on.exit(close(file))
        }
        if (!inherits(file, "connection"))
            stop("'file' must be a character string or connection")
        if (!isOpen(file)) {
            open(file, "r")
            on.exit(close(file))
        }
        data <- read.table(file, col.names=c("longitude", "latitude"), stringsAsFactors=FALSE)
        res <- new("coastline", longitude=data$longitude, latitude=data$latitude, fillable=FALSE, filename=filename)
    } else if (type == "mapgen") {
        header <- scan(file, what=character(0), nlines=1, quiet=TRUE) # slow, but just one line
        oceDebug(debug, "method is mapgen\nheader:", header, "\n")
        ##separator <- NULL
                                        # mapgen    # -b
                                        # matlab    nan nan
                                        # Splus     NA NA
                                        # mapgen...
                                        # 1
                                        # ...
                                        # END
                                        # 2
                                        #   ...
                                        #   END
        if (all.equal(header, c("#", "-b"))) {
            lonlat <- scan(file, what=double(0), na.strings=c("#", "-b"), quiet=TRUE) # slow, but just one line
        } else {
            if (all.equal(header, c("nan", "nan"))) {
                lonlat <- scan(file, what=double(0), na.strings=c("nan", "nan"), quiet=TRUE) # fast because whole file
            } else {
                if (all.equal(header, c("NA", "NA"))) {
                    lonlat <- scan(file, what=double(0), quiet=TRUE) # fast because whole file
                } else {
                    stop(cat("Unknown file type; the unrecognized header line is '", header, "'\n", sep=" "))
                }
            }
        }
        res <- new("coastline", longitude=lonlat[, 1], latitude=lonlat[, 2], fillable=FALSE)
    } else {
        stop("unknown method.  Should be \"R\", \"S\", or \"mapgen\"")
    }
    if (missing(processingLog))
        processingLog <- paste(deparse(match.call()), sep="", collapse="")
    res@processingLog <- processingLogAppend(res@processingLog, processingLog)
    oceDebug(debug, "} # read.coastline()\n", unindent=1, style="bold")
    res
}

#' Read a Coastline File in Shapefile Format
#'
#' Read coastline data stored in the shapefile format (see reference 1).
#'
#' @param file name of file containing coastline data (a file ending in `.shp`)
#' or a zipfile that contains such a file, with a corresponding name.
#' The second scheme is useful for files downloaded from the NaturalEarth
#' website (see reference 2).
#'
#' @param lonlim,latlim numerical vectors specifying the
#' west and east edges (and south and north edges) of a focus window.
#' Coastline polygons that do not intersect the defined box are
#' skipped, which can be useful in narrowing high-resolution world-scale
#' data to a local application.
#'
#' @param debug set to TRUE to print information about the header, etc.
#'
#' @param monitor Logical indicating whether to print an indication of progress through
#' the file.
#'
#' @param processingLog if provided, the action item to be stored in the log.
#' (Typically only provided for internal calls; the default that it provides is
#' better for normal calls by a user.)
#'
#' @return x a [coastline-class] object.
#'
#' @section A hack for depth contours: The following demonstrates that this
#' code is getting close to working with depth contours.  This should be
#' handled more internally, and a new object for depth contours should be
#' constructed, of which coastlines could be a subset.
#'
#' @references
#' 1. The ``shapefile'' format is described in
#' *ESRI Shapefile Technical Description*, March 1998, available at
#' `https://www.esri.com/content/dam/esrisites/sitecore-archive/Files/Pdfs/library/whitepapers/pdfs/shapefile.pdf`
#' (last checked 2021-03-24).
#'
#' 2. The NaturalEarth website `https://www.naturalearthdata.com/downloads/`
#' provides coastline datasets in three resolutions, along with similar files
#' lakes and rivers, for borders, etc. It is highly recommended.
#'
#' @family things related to coastline data
#'
#' @author Dan Kelley
read.coastline.shapefile <- function(file,
    lonlim=c(-180, 180),
    latlim=c(-90, 90),
    debug=getOption("oceDebug"),
    monitor=FALSE,
    processingLog)
{
    if (missing(file))
        stop("must supply 'file'")
    if (is.character(file)) {
        if (!file.exists(file))
            stop("cannot find file '", file, "'")
        if (0L == file.info(file)$size)
            stop("empty file '", file, "'")
    }
    oceDebug(debug, "read.shapefile(file=\"", file, "\", ...) {\n", sep="", unindent=1, style="bold")
    shapeTypeList <- c("nullshape",    # 0
                       "point",        # 1
                       "not used",     # 2
                       "polyline",     # 3
                       "not used",     # 4
                       "polygon",      # 5
                       "not used",     # 6
                       "not used",     # 7
                       "multipoint",   # 8
                       "not used",     # 9
                       "not used",     # 10
                       "pointz",       # 11
                       "not used",     # 12
                       "polylinez",    # 13
                       "not used",     # 14
                       "polygonz",     # 15
                       "not used",     # 16
                       "not used",     # 17
                       "multipointz",  # 18
                       "not used",     # 19
                       "not used",     # 20
                       "pointm",       # 21
                       "not used",     # 22
                       "polylinem",    # 23
                       "not used",     # 24
                       "polygonm",     # 25
                       "not used",     # 26
                       "not used",     # 27
                       "multipointm",  # 28
                       "not used",     # 29
                       "not used",     # 30
                       "multipatch")   # 31

    lonlim <- sort(lonlim)
    latlim <- sort(latlim)

    if (is.character(file)) {
        oceDebug(debug, "file '", file, "'\n", sep="")
        if (1 == length(grep(".zip$", file))) {
            ## Handle zipfiles. Note that this code might come in handy
            ## in other contexts, so it is being written in a step-by-step
            ## way. Importantly, the extracted file is saved in a temporary
            ## directory to avoid overwriting something (or otherwise
            ## disrupting) the working directory.
            zipfile <- file
            ## filename <- fullFilename(zipfile)
            file <- gsub(".zip$", ".shp", file)
            file <- gsub(".*/", "", file) # remove directory path
            oceDebug(debug, "   zip   file:     '", zipfile, "'\n", sep="")
            oceDebug(debug, "   shape file:     '", file, "'\n", sep="")
            oceDebug(debug, "metadata filename: '", file, "'\n", sep="")
            tdir <- tempdir()
            oceDebug(debug, "             tdir: '", tdir, "'\n", sep="")
            oceDebug(debug, "about to unzip ...\n")
            unzip(zipfile, exdir=tdir) # unzips all the files (we need .shp and .dbf)
            oceDebug(debug, "... the unzip completed without error\n")
            tfile <- paste(tdir, file, sep="/")
            oceDebug(debug, "            tfile: '", tfile, "'\n", sep="")
            filename <- tfile
            file <- file(tfile, "rb")
            oceDebug(debug, "using shapefile temporarily at '", tfile, "'\n", sep="")
            on.exit({
                close(file)
                unlink(tdir)
            })
        } else {
            filename <- fullFilename(file)
            file <- file(file, "rb")
            on.exit(close(file))
        }
    }
    if (!inherits(file, "connection"))
        stop("argument `file' must be a character string or connection")
    if (!isOpen(file)) {
        filename <- "(connection)"
        open(file, "rb")
        on.exit(close(file))
    }
    seek(file, 0, "end")
    fileSize <- seek(file, 0, "start")
    oceDebug(debug, "fileSize:", fileSize, "as determined from the operating system\n")
    buf <- readBin(file, "raw", fileSize)
    ## main header is always 100 bytes [ESRI White paper page 3]
    header <- buf[1:100]
    fieldCode <- readBin(header[1:4], "integer", n=1, size=4, endian="big")
    if (fieldCode != 9994)
        stop("first four bytes of file must yield 9994 (as a big-endian integer) but yield ", fieldCode, "\n")
    fileSizeHeader <- 2*readBin(buf[25:28], "integer", n=1, size=4, endian="big") # it's in 2-byte words
    oceDebug(debug, "fileSizeHeader:", fileSizeHeader, "as interpreted from header\n")
    if (fileSizeHeader != fileSize)
        warning("file size is ", fileSize, " but the header suggests it to be ", fileSizeHeader, "; using the former")
    shapeTypeFile <- readBin(buf[33:36], "integer", n=1, size=4, endian="little")
    oceDebug(debug, "shapeTypeFile:", shapeTypeFile, "(", shapeTypeList[shapeTypeFile+1], ")\n")
    if (shapeTypeFile != 5 && shapeTypeFile != 3 && shapeTypeFile != 15) {
        warning("can handle shape-type 3 (", shapeTypeList[4], ") and 5 (",
             shapeTypeList[6], "), but not ", shapeTypeFile, " (",
            shapeTypeList[shapeTypeFile+1], ")\n")
        return(NULL)
    }
    if (3 == shapeTypeFile) {
        oceDebug(debug, "shapeTypeFile == 3, so assuming a depth-contour file\n")
        dbfName <- paste(gsub(".shp$", "", filename), ".dbf", sep="")
        oceDebug(debug, " reading DBF file '", dbfName, "'\n", sep="")
        if (requireNamespace("foreign", quietly=TRUE)) {
            depths <- foreign::read.dbf(dbfName)[[1]]
        } else {
            stop("cannot read shapeFile element of type 3 without the 'foreign' package being installed")
        }
    }
    xmin <- readBin(buf[37+0:7], "double", n=1, size=8, endian="little")
    ymin <- readBin(buf[45+0:7], "double", n=1, size=8, endian="little")
    xmax <- readBin(buf[53+0:7], "double", n=1, size=8, endian="little")
    ymax <- readBin(buf[61+0:7], "double", n=1, size=8, endian="little")
    res <- new("coastline", fillable=shapeTypeFile==5)
    res@metadata$filename <- filename
    res@metadata$fillable <- TRUE
    oceDebug(debug, sprintf("xmin: %.4f, xmax: %.4f, ymin: %.4f, ymax: %.4f\n", xmin, xmax, ymin, ymax))
    ##
    ## RECORD BY RECORD
    ##
    buf <- buf[101:length(buf)]         # now we just have data
    o <- 0                              # offset for chunk
    record <- 0
    latitude <- longitude <- NULL
    segment <- 0
    while (TRUE) {
        record <- record + 1
        if ( (o + 53) > fileSize) {
            ## FIXME could be more clever on eof
            oceDebug(debug, "o:", o, ", fileSize:", fileSize, " ... so finished\n")
            break
        }
        ## each record has an 8-byte header followed by data [1 table 2] BIG endian
        recordNumber <- readBin(buf[o + 1:4], "integer", n=1, size=4, endian="big")
        ##recordLength <- readBin(buf[o + 5:8], "integer", n=1, size=4, endian="big")
        ## first part of data is shape type [1 table 3 for null, etc] LITTLE endian
        shapeType <- readBin(buf[o + 9:12], "integer", n=1, size=4, endian="little")
        if (shapeType < 0) stop("cannot have shapeType < 0, but got ", shapeType, " (programming error)")
        if (shapeType > 31) stop("cannot have shapeType > 31, but got ", shapeType, " (programming error)")
        if (shapeType == 0) {
            ## NULL record; just skip 4 bytes (I guess; [1] table 3)
            o <- o + 12
        } else {
            if (shapeType != shapeTypeFile)
                stop("record ", record, " has shape type ", shapeType, ", which does not match file value ", shapeTypeFile)
            ## minimum bounding rectangle, number of parts, number of points, parts, points
            ## MBR is xmin ymin xmax ymax
            mbr <- readBin(buf[o + 13:44], "double", n=4, size=8, endian="little", signed=TRUE)
            ## ignore if not in focus box
            intersectsBox <- !(mbr[1] > lonlim[2] | mbr[2] > latlim[2] | mbr[3] < lonlim[1] | mbr[4] < latlim[1])
            numberParts <- readBin(buf[o + 45:48], "integer", n=1, size=4, endian="little")
            numberPoints <- readBin(buf[o + 49:52], "integer", n=1, size=4, endian="little")
            oceDebug(debug, " recordNUmber:", recordNumber,
                     ", shapeType:", shapeType,
                     " (", shapeTypeList[1+shapeType], ")",
                     ", numberPoints:", numberPoints,
                     ", numberParts:", numberParts,
                     ", intersectsBox:", intersectsBox,
                     "\n", sep="")
            if (intersectsBox) {
                partOffset <- readBin(buf[o + 53+0:(-1+4*numberParts)],
                                      "integer", n=numberParts, size=4, endian="little")
                xy <- matrix(readBin(buf[o + 53 + 4 * numberParts + 0:(-1 + 2 * numberPoints * 8)],
                                     "double", n=numberPoints*2, size=8), ncol=2, byrow=TRUE)
                look <- c(1 + partOffset, numberPoints)
                for (part in 1:numberParts) {
                    segment <- segment + 1
                    if (monitor){
                        segment <- segment + 1
                        cat(".")
                        if (!(segment %% 50))
                            cat(segment, "\n")
                    }
                    rows <- seq.int(look[part], -1 + look[part+1])
                    latitude <- c(latitude, NA, xy[rows, 2]) # FIXME: this is slow; can we know size at start?
                    longitude <- c(longitude, NA, xy[rows, 1])
                }
            }
            o <- o + 53 + 4 * numberParts + 2 * numberPoints * 8 - 1
        }
    }
    res@data$longitude <- longitude
    res@data$latitude <- latitude
    if (shapeTypeFile == 3)
        res@metadata$depths <- depths
    if (missing(processingLog))
        processingLog <- paste(deparse(match.call()), sep="", collapse="")
    res@processingLog <- processingLogAppend(res@processingLog, processingLog)
    oceDebug(debug, "} # read.coastline.shapefile()\n", unindent=1, style="bold")
    res
}

#' Read a Coastline File in Openstreetmap Format
#'
#' Read coastline data stored in the openstreetmap format.
#'
#' @inheritParams read.coastline.shapefile
#'
#' @return a [coastline-class] object.
#'
#' @family things related to coastline data
#'
#' @author Dan Kelley
read.coastline.openstreetmap <- function(file,
    lonlim=c(-180, 180),
    latlim=c(-90, 90),
    debug=getOption("oceDebug"),
    monitor=FALSE,
    processingLog)
{
    if (missing(file))
        stop("must supply 'file'")
    if (is.character(file)) {
        if (!file.exists(file))
            stop("cannot find file '", file, "'")
        if (0L == file.info(file)$size)
            stop("empty file '", file, "'")
    }
    oceDebug(debug, "read.coastline.openstreetmap(file=\"", file, "\", ...) {\n", sep="", unindent=1, style="bold")
    ## FIXME: ignoring lonlim and latlim
    if (is.character(file)) {
        filename <- fullFilename(file)
        file <- file(file, "rb")
        on.exit(close(file))
    }
    if (!inherits(file, "connection"))
        stop("argument `file' must be a character string or connection")
    if (!isOpen(file)) {
        filename <- "(connection)"
        open(file, "rb")
        on.exit(close(file))
    }
    res <- new("coastline", fillable=FALSE, filename=filename)
    d <- readLines(file)
    ## get all <nd> (even if only using some)
    nodeLines <- d[grep("^ *<node", d)]
    nodeIds <- as.numeric(sub('".*$', '', sub('^.* id="', '', nodeLines)))
    nodeLats <- as.numeric(sub('".*$', '', sub('^.* lat="', '', nodeLines)))
    nodeLons <- as.numeric(sub('".*$', '', sub('^.* lon="', '', nodeLines)))
    ## get all <way>
    wayStart <- grep("<way ", d)
    ##wayEnd <- grep("</way ", d)
    coastlineWayEnd <- grep('k="natural" v="coastline"', d)
    ncoastline <- length(coastlineWayEnd)
    coastlineWayStart <- vector("integer", ncoastline)
    for (i in 1:ncoastline) {
        coastlineWayStart[i] <- wayStart[max(which(wayStart < coastlineWayEnd[i]))]
    }
    oceDebug(debug, "ncoastline:", ncoastline, "\n")
    latitude <- longitude <- NULL
    for (i in 1:ncoastline) {
        oceDebug(debug, "coastline chunk #", i, "\n")
        look <- d[seq.int(coastlineWayStart[i]+1, coastlineWayEnd[i]-1)]
        look <- look[grep("ref=", look)]
        refs <- as.numeric(sub('\"/>', '', sub('.*=\"', '', look)))
        ## following is 10% slower than using match
        ##for (r in refs) {
        ##    w <- which(r == nodeIds)   # FIXME: for speed, perhaps use match(r, nodeIds)
        ##    latitude <- c(latitude, nodeLats[w])
        ##    longitude <- c(longitude, nodeLons[w])
        ##}
        m <- match(refs, nodeIds)
        longitude <- c(longitude, nodeLons[m], NA)
        latitude <- c(latitude, nodeLats[m], NA)
    }
    res@data$latitude <- latitude
    res@data$longitude <- longitude
    if (missing(processingLog))
        processingLog <- paste(deparse(match.call()), sep="", collapse="")
    res@processingLog <- processingLogAppend(res@processingLog, processingLog)
    oceDebug(debug, "} # read.coastline.openstreetmap()\n", unindent=1, style="bold")
    res
}


#' Find the Name of the Best Coastline Object
#'
#' Find the name of the most appropriate coastline for a given locale
#' Checks `coastlineWorld`, `coastlineWorldFine` and
#' `coastlineWorldCoarse`, in that order, to find the one most appropriate
#' for the locale.
#'
#' @param lonRange range of longitude for locale
#'
#' @param latRange range of latitude for locale
#'
#' @param span span of domain in km (if provided, previous two arguments are
#' ignored).
#'
#' @param debug set to a positive value to get debugging information during
#' processing.
#'
#' @return The name of a coastline that can be loaded with `data()`.
#'
#' @family things related to coastline data
#'
#' @author Dan Kelley
coastlineBest <- function(lonRange, latRange, span, debug=getOption("oceDebug"))
{
    oceDebug(debug, "coastlineBest(",
             "lonRange=c(",
             if (missing(lonRange)) "missing" else paste(round(lonRange, 2), collapse=","), "), ",
             "latRange=c(",
             if (missing(latRange)) "missing" else paste(round(latRange, 2), collapse=","), "), ",
             "span=", if (missing(span)) "(missing)" else span, ") {\n",
             sep="", unindent=1, style="bold")
    if (missing(span)) {
        oceDebug(debug, "inferring span from lonRange=c(",paste(lonRange,collapse=","),
                 ") and latRange=c(", paste(latRange, collapse=","), ")\n")
        if (missing(lonRange) || missing(latRange))
            return("coastlineWorld")
        if (length(lonRange) != 2)
            stop("lonRange must be of length 2")
        if (length(latRange) != 2)
            stop("latRange must be of length 2")
        if (any(lonRange > 180)) {
            lonRange <- lonRange - 360 # FIXME: does this always work?
            oceDebug(debug, "adjusted lonRange:", lonRange, "\n")
        }
        lonRange <- sort(lonRange)
        latRange <- sort(latRange)
        ## Set scale as the max of the distances along four sides of box
        ## NB. all distance used here are in km.
        l <- round(geodDist(lonRange[1], latRange[1], lonRange[1], latRange[2]),2)
        r <- round(geodDist(lonRange[2], latRange[1], lonRange[2], latRange[2]),2)
        b <- round(geodDist(lonRange[1], latRange[1], lonRange[2], latRange[1]),2)
        t <- round(geodDist(lonRange[1], latRange[2], lonRange[2], latRange[2]),2)
        oceDebug(debug, "Inferring span from corners l:", l, ", r:", r, ", b:", b, ", t:", t, "\n")
        span <- max(l, r, b, t)
    }
    C <- 2 * 3.14 * 6.4e3              # circumferance of earth
    oceDebug(debug, "span:", span, ", C:", C, "\n")
    if (span < 500) {
        res <- "coastlineWorldFine"
    } else if (span < C / 4) {
        res <- "coastlineWorldMedium"
    } else {
        res <- "coastlineWorld"
    }
    oceDebug(debug, "} # coastlineBest()\n", unindent=1, style="bold")
    res
}

#' Cut a Coastline Object at Specified Longitude
#'
#' This can be helpful in preventing [mapPlot()] from producing ugly
#' horizontal lines in world maps. These lines occur when a coastline segment
#' is intersected by longitude lon_0+180.  Since the coastline files in the oce
#' and ocedata packages are already "cut" at longitudes of -180 and 180, the present
#' function is not needed for default maps, which have `+lon_0=0`. However,
#' may help with other values of `lon_0`.
#'
#' @section Caution:
#' This function is provisional. Its behaviour, name and very existence
#' may change.  Part of the development plan is to see if there is common
#' ground between this and the `clipPolys` function in the
#' \CRANpkg{PBSmapping} package.
#'
#' @param coastline a [coastline-class] object.
#'
#' @param lon_0 longitude as would be given in a `+lon_0=` item in a
#' call to [sf::sf_project()].
#'
#' @examples
#'\donttest{
#' library(oce)
#' data(coastlineWorld)
#' mapPlot(coastlineCut(coastlineWorld, lon_0=100),
#'         projection="+proj=moll +lon_0=100", col="gray")
#'}
#'
#' @return a new coastline object
#'
#' @family things related to coastline data
#'
#' @author Dan Kelley
coastlineCut <- function(coastline, lon_0=0)
{
    if (lon_0 == 0)
        return(coastline)
    cleanAngle<-function(a)
        ifelse(a < -180, a+360, ifelse(a > 180, a-360, a))
    loncut <- cleanAngle(lon_0+180)
    lon <- coastline[["longitude"]]
    lat <- coastline[["latitude"]]
    nlon <- length(lon)
    e <- 4                             # a bit over 2 should be more than enough for any coastline
    cut <- .C("polygon_subdivide_vertically_smash_1",
              n=as.integer(nlon), x=as.double(lon), y=as.double(lat), x0=as.double(loncut),
              nomax=as.integer(e*nlon), no=integer(1), xo=double(e*nlon), yo=double(e*nlon),
              NAOK=TRUE)
    cut$xo <- cut$xo[1:cut$no]
    cut$yo <- cut$yo[1:cut$no]
    as.coastline(longitude=cut$xo, latitude=cut$yo)
}
