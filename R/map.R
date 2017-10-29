.axis <- local({
    val <- list(longitude=NULL, latitude=NULL)
    function(new) if (!missing(new)) val <<- new else val
})

.Projection <- local({
    ## emulate mapproj
    ## type can be 'none' or 'proj4' (once, permitted 'mapproj' also)
    val <- list(type="none", projection="")
    function(new) if (!missing(new)) val <<- new else val
})

#' Coordinate Reference System strings for some oceans
#'
#' Create a coordinate reference string (CRS), suitable for use as a
#' \code{projection} argument to \code{\link{mapPlot}} or
#' \code{\link{plot,coastline-method}}.
#'
#' @section Caution: This is a preliminary version of this function,
#' with the results being very likely to change through the autumn of 2016,
#' guided by real-world usage.
#'
#' @param region character string indicating the region. This must be
#' in the following list (or a string that matches to just one entry,
#' with \code{\link{pmatch}}):
#' \code{"North Atlantic"}, \code{"South Atlantic"}, \code{"Atlantic"},
#' \code{"North Pacific"}, \code{"South Pacific"}, \code{"Pacific"},
#' \code{"Arctic"},  and \code{"Antarctic"}.
#'
#' @return string contain a CRS, which can be used as \code{projection}
#' in \code{\link{mapPlot}}.
#' @author Dan Kelley
#' @family functions related to maps
#' @examples
#' \dontrun{
#' library(oce)
#' data(coastlineWorld)
#' par(mar=c(2, 2, 1, 1))
#' plot(coastlineWorld, proj=oceCRS("Atlantic"), span=12000)
#' plot(coastlineWorld, proj=oceCRS("North Atlantic"), span=8000)
#' plot(coastlineWorld, proj=oceCRS("South Atlantic"), span=8000)
#' plot(coastlineWorld, proj=oceCRS("Arctic"), span=4000)
#' plot(coastlineWorld, proj=oceCRS("Antarctic"), span=10000)
#' # Avoid ugly horizontal lines, an artifact of longitude shifting.
#' # Note: we cannot fill the land once we shift, either.
#' pacific <- coastlineCut(coastlineWorld, -180)
#' plot(pacific, proj=oceCRS("Pacific"), span=15000, col=NULL)
#' plot(pacific, proj=oceCRS("North Pacific"), span=12000, col=NULL)
#' plot(pacific, proj=oceCRS("South Pacific"), span=12000, col=NULL)
#' }
oceCRS <- function(region)
{
    regionChoices <- c("North Atlantic", "South Atlantic", "Atlantic", "Arctic", "Antarctic",
                       "Pacific", "North Pacific", "South Pacific")
    id <- pmatch(region, regionChoices)
    if (is.na(id))
        stop("region must be in \"", paste(regionChoices, collapse="\" \""), "\" but it is \"", region,  "\"\n")
    region <- regionChoices[id]
    CRS <- if (region == "Atlantic") "+proj=laea +lon_0=-30 +lat_0=0"
        else if (region == "North Atlantic") "+proj=laea +lon_0=-40 +lat_0=30"
        else if (region == "South Atlantic") "+proj=laea +lon_0=-20 +lat_0=-30"
        else if (region == "Arctic") "+proj=stere +lon_0=0 +lat_0=90"
        else if (region == "Antarctic") "+proj=stere +lon_0=0 +lat_0=-90"
        else if (region == "Pacific") "+proj=merc +lon_0=-180 +lat_0=0"
        else if (region == "North Pacific") "+proj=robin +lon_0=-180 +lat_0=30"
        else if (region == "South Pacific") "+proj=robin +lon_0=-180 +lat_0=-30"
        else stop("unknown region")
    CRS
}

#' Shift Longitude to Range -180 to 180
#'
#' This is a utility function used by \code{\link{mapGrid}}. It works
#' simply by subtracting 180 from each longitude, if any longitude
#' in the vector exceeds 180.
#'
#' @param longitudes a numericl vector of longitudes
#' @return vector of longitudes, shifted to the desired range.
#' @seealso \code{\link{matrixShiftLongitude}} and \code{\link{standardizeLongitude}}.
#' @family functions related to maps
shiftLongitude <- function(longitudes) {
    if (any(longitudes > 180)) longitudes-360 else longitudes
}

fixneg <- function(v)
{
    res <- v
    for (i in seq_along(v)) {
        if (res[i] == "0N") {
            res[i] <- "0"
        } else if (res[i] == "0E") {
            res[i] <- "0"
        } else if ("-" == substr(v[i], 1, 1)) {
            ##cat("res[i]=", res[i], "\n")
            res[i] <- gsub("^-", "", v[i])
            res[i] <- gsub("E", "W", res[i])
            res[i] <- gsub("N", "S", res[i])
            ##cat(" -> res[i]=", res[i], "\n")
        }
    }
    res
}

badFillFix1 <- function(x, y, latitude, projection="")
{
    ##xrange <- range(x, na.rm=TRUE)
    ##yrange <- range(y, na.rm=TRUE)
    n <- length(x)
    ## 1181 necessitated this use of n>100 (it was a case of 3 isolated islands)
    if (n > 100) { # avoid getting confused by e.g. a view with two islands
        ## FIXME: below is a kludge to avoid weird horiz lines; it
        ## FIXME: would be better to complete the polygons, so they
        ## FIXME: can be filled.  It might be smart to do this in C
        d <- c(0, sqrt(diff(x)^2 + diff(y)^2))
        d[!is.finite(d)] <- 0          # FIXME: ok?
        ##dc <- as.numeric(quantile(d, 1-100*(1/3/length(x)), na.rm=TRUE)) # FIXME: criterion
        ##bad <- d > dc
        ##bad <- 0.1 < (d / diff(range(x, na.rm=TRUE)))
        antarctic <- latitude < -60
        bad <- ( (d / diff(range(x, na.rm=TRUE))) > 0.1 ) & !antarctic
        ## if (length(options("oce1181")[[1]])) browser()
        ## FIXME: this should finish off polygons, but that is a bit tricky, e.g.
        ## FIXME: should we create a series of points to a trace along the edge
        ## FIXME: the visible earth?
        x[bad] <- NA
        y[bad] <- NA
    }
    bad2 <- !is.finite(x) | !is.finite(y)
    x[bad2] <- NA
    y[bad2] <- NA
    list(x=x, y=y)
}

badFillFix2 <- function(x, y, xorig, yorig)
{
    usr <- par("usr")
    w <- which(is.na(xorig))
    if (length(w) > 1) {
        for (iw in seq(1, -1+length(w))) {
            ##message("check chunk", iw)
            look <- seq.int(w[iw]+1, w[iw+1]-1)
            xl <- xorig[look]
            yl <- yorig[look]
            offscale <- yl < usr[3] | xl < usr[1] | yl > usr[4] | xl > usr[2]
            offscale <- offscale[is.finite(offscale)]
            if (all(offscale)) {
                ## probably faster to do this than to make new vectors
                ##message("  TRIM")
                x[look] <- NA
                y[look] <- NA
            }
        }
    }
    list(x=x, y=y)
}



#' Add Axis Labels to an Existing Map
#'
#' Plot axis labels on an existing map.
#'
#' @param side the side at which labels are to be drawn.  If not provided,
#' sides 1 and 2 will be used (i.e. bottom and left-hand sides).
#'
#' @param longitude vector of longitudes to indicate.  If not provided, and if
#' a grid has already been drawn, then the labels will be at the
#' interesections of the grid lines with the plotting box.
#'
#' @param latitude vector of latitudes to indicate.  If not provided, and if a
#' grid has already been drawn, then the labels will be at the
#' interesections of the grid lines with the plotting box.
#'
#' @param tick parameter passed to \code{\link{axis}}.
#'
#' @param line parameter passed to \code{\link{axis}}.
#'
#' @param pos parameter passed to \code{\link{axis}}.
#'
#' @param outer parameter passed to \code{\link{axis}}.
#'
#' @param font axis font, passed to \code{\link{axis}}.
#'
#' @param lty axis line type, passed to \code{\link{axis}}.
#'
#' @param lwd axis line width, passed to \code{\link{axis}}).
#'
#' @param lwd.ticks tick line width, passed to \code{\link{axis}}.
#'
#' @param col axis colour, passed to \code{\link{axis}}.
#'
#' @param col.ticks axis tick colour, passed to \code{\link{axis}}.
#'
#' @param hadj an argument that is transmitted to \code{\link{axis}}.
#'
#' @param padj an argument that is transmitted to \code{\link{axis}}.
#'
#' @param tcl axis-tick size (see \code{\link{par}}).
#'
#' @param cex.axis axis-label expansion factor (see \code{\link{par}}).
#'
#' @param mgp three-element numerical vector describing axis-label
#' placement (see \code{\link{par}}). It usually makes sense to set
#' the first and third elements to zero.
#'
#' @param debug a flag that turns on debugging.  Set to 1 to get a moderate
#' amount of debugging information, or to 2 to get more.
#'
#' @details
#' This function is still in development, and the argument list as well as the
#' action taken are both subject to change, hence the brevity of this help page.
#'
#' Note that if a grid line crosses the axis twice, only one label will be drawn.
#'
#' @examples
#' \dontrun{
#' library(oce)
#' data(coastlineWorld)
#' par(mar=c(2, 2, 3, 1))
#' lonlim <- c(-180, 180)
#' latlim <- c(60, 120)
#' mapPlot(coastlineWorld, projection="+proj=stere +lat_0=90",
#'         longitudelim=lonlim, latitudelim=latlim,
#'         grid=FALSE)
#' mapGrid(15, 15, polarCircle=1/2)
#' mapAxis()
#' }
#'
#' @author Dan Kelley
#' @seealso A map must first have been created with \code{\link{mapPlot}}.
#' @family functions related to maps
mapAxis <- function(side=1:2, longitude=NULL, latitude=NULL,
                    tick=TRUE, line=NA, pos=NA, outer=FALSE, font=NA,
                    lty="solid", lwd=1, lwd.ticks=lwd, col=NULL, col.ticks=NULL,
                    hadj=NA, padj=NA, tcl=-0.3, cex.axis=1,
                    mgp=c(0, 0.5, 0),
                    debug=getOption("oceDebug"))
{
    if ("none" == .Projection()$type)
        stop("must create a map first, with mapPlot()\n")
    oceDebug(debug, "mapAxis(side=c(", paste(side, collapse=","), ")",
             ", longitude=", if (length(longitude)) c(longitude[1], "...") else "NULL",
             ", latitude=", if (length(latitude)) c(latitude[1], "...") else "NULL",
             ") { \n", unindent=1, sep="")
    axis <- .axis()
    #if (debug > 0) print(axis)
    if (is.null(longitude) && is.null(latitude)) {
        longitude <- axis$longitude
        latitude <- axis$latitude
    }
    if (is.null(longitude) && is.null(latitude))
        return()
    ## if (is.null(axis$longitude)) oceDebug(debug, "should auto generate longitude grid and then axis\n")
    ## if (is.null(axis$latitude)) oceDebug(debug, "should auto generate latitude grid and then axis\n")
    ## if (missing(longitude)) longitude <- axis$longitude
    ## if (missing(latitude)) latitude <- axis$latitude
    if (missing(side))
        side <- 1:2
    usr <- par('usr')
    axisSpan <- max(usr[2]-usr[1], usr[4]-usr[3])
    if (1 %in% side) {
        oceDebug(debug, "drawing axis on side 1\n")
        AT <- NULL
        LAB <- NULL
        for (lon in longitude) {
            if (debug > 3) oceDebug(debug, "check longitude", lon, "for axis on side=1\n")
            ## Seek a point at this lon that matches the lon-lat relationship on side=1
            o <- optimize(function(lat) abs(lonlat2map(lon, lat)$y-usr[3]), lower=-89.9999, upper=89.9999)
            if (is.na(o$objective) || o$objective > 0.01*axisSpan) {
                if (debug > 3) oceDebug(debug, "  longitude", lon, "is unmappable\n")
                next
            }
            ## Check that the point matches lat, as well as lon (this matters for full-globe)
            P <- lonlat2map(lon, o$minimum)
            ## oceDebug(debug, "Investigate point at x=", P$x, ", y=", P$y, "; note usr[3]=", usr[3], "\n")
            x <- P$x
            if (is.finite(P$y) && (abs(P$y - usr[3]) < 0.01 * (usr[4] - usr[3]))) {
                if (!is.na(x) && usr[1] < x && x < usr[2]) {
                    label <- fixneg(paste(lon, "E", sep=""))
                    ##mtext(label, side=1, at=x)
                    AT <- c(AT, x)
                    LAB <- c(LAB, label)
                    if (debug > 3) oceDebug(debug, "  ", label, "intersects side 1\n")
                } else {
                    if (debug > 3) oceDebug(debug, "    ", lon, "E does not intersect side 1\n")
                }
            } else {
                ## oceDebug(debug, "skipping off-globe point\n")
            }
        }
        if (!is.null(AT)) {
            axis(side=1, at=AT, labels=fixneg(LAB), mgp=mgp,
                 tick=tick, line=line, pos=pos, outer=outer, font=font,
                 lty=lty, lwd=lwd, lwd.ticks=lwd.ticks, col=col, col.ticks=col.ticks,
                 hadj=hadj, padj=padj, tcl=tcl, cex.axis=cex.axis)
        }
        if (length(latitude)) {
            warning("mapAxis(side=1) cannot draw latitude labels yet; contact author if you need this")
        }
    }
    if (2 %in% side) {
        oceDebug(debug, "drawing axis on side 2\n")
        AT <- NULL
        LAB <- NULL
        f <- function(lon) lonlat2map(lon, lat)$x-usr[1]
        ## FIXME: if this uniroot() method looks good for side=2, try for side=1 also.
        LONLIST <- seq(-360, 360, 20) # smaller increments are slower but catch more labels
        for (lat in latitude) {
            if (debug > 3)
                oceDebug(debug, "check ", lat, "N for axis on side=2\n", sep="")
            ## Seek a point at this lon that matches the lon-lat relationship on side=1
            for (iLON in 2:length(LONLIST)) {
                #if (lat == 55) browser()
                LONLOOK <- LONLIST[iLON+c(-1, 0)]
                #cat("LONLOOK[1]", LONLOOK[1], "f(...)", f(LONLOOK[1]), "\n")
                #cat("LONLOOK[2]", LONLOOK[2], "f(...)", f(LONLOOK[2]), "\n")
                f1 <- f(LONLOOK[1])
                if (!is.finite(f1))
                    next
                f2 <- f(LONLOOK[2])
                if (!is.finite(f2))
                    next
                if (f1 * f2 > 0)
                    next
                r <- uniroot(f, lower=LONLOOK[1], upper=LONLOOK[2], tol=1)
                P <- lonlat2map(r$root, lat)
                ##OLD| ## using optimize. This seems slower, and can hit boundaries.
                ##OLD| o <- optimize(function(lon) abs(lonlat2map(lon, lat)$x-usr[1]), lower=LONLOOK[1], upper=LONLOOK[2], tol=1)
                ##OLD| if (is.na(o$objective) || o$objective > 0.01*axisSpan) {
                ##OLD|     if (debug > 3) oceDebug(debug, "  ", lat, "N is unmappable for iLON=", iLON, "; o$objective=", o$objective, "\n", sep="")
                ##OLD|     next
                ##OLD| }
                ##OLD| # Check that the point matches lat, as well as lon (this matters for full-globe)
                #P <- lonlat2map(o$minimum, lat)
                y <- P$y
                if (is.finite(P$x) && (abs(P$x - usr[1]) < 0.01 * (usr[2] - usr[1]))) {
                    if (!is.na(y) && usr[3] < y && y < usr[4]) {
                        label <- fixneg(paste(lat, "N", sep=""))
                        AT <- c(AT, y)
                        LAB <- c(LAB, label)
                        if (debug > 3) oceDebug(debug, "  ", label, " intersects side 2\n", sep="")
                    } else {
                        if (debug > 3) oceDebug(debug, "  ", lat, "N does not intersect side 2\n", sep="")
                    }
                } else {
                    ##oceDebug(debug, "skipping off-globe point\n")
                }
            }
        }
        if (!is.null(AT)) {
            axis(side=2, at=AT, labels=fixneg(LAB), mgp=mgp,
                 tick=tick, line=line, pos=pos, outer=outer, font=font,
                 lty=lty, lwd=lwd, lwd.ticks=lwd.ticks, col=col, col.ticks=col.ticks,
                 hadj=hadj, padj=padj, tcl=tcl, cex.axis=cex.axis)
        }
        if (length(longitude)) {
            warning("mapAxis(side=2) cannot draw longitude labels yet; contact author if you need this")
        }
    }
    if (3 %in% side) {
        oceDebug(debug, "drawing axis on side 3 NOT CODED YET\n")
    }
    if (4 %in% side) {
        oceDebug(debug, "drawing axis on side 4 NOT CODED YET\n")
    }
    oceDebug(debug, "} # mapAxis()\n", unindent=1)
}


#' Add Contours on a Existing map
#'
#' Plot contours on an existing map.
#'
#' @param longitude vector of longitudes of points to be plotted, or an object of
#' class \code{topo} (see \code{\link{topo-class}}), in which case
#' \code{longitude}, \code{latitude} and \code{z} are inferred from that object.
#'
#' @param latitude vector of latitudes of points to be plotted.
#'
#' @param z matrix to be contoured.
#'
#' @param nlevels number of contour levels, if and only if \code{levels} is not supplied.
#'
#' @param levels vector of contour levels.
#'
#' @param col line colour.
#'
#' @param lty line type.
#'
#' @param lwd line width.
#'
#' @details
#' Adds contour lines to an existing map, using \code{\link{mapLines}}.
#' The arguments are based on those to \code{\link{contour}} and
#' \code{\link{contourLines}}.
#'
#' @section Bugs:
#' As with \code{\link{mapLines}}, long lines should be subdivided
#' into multiple segments so that e.g. great circle lines will be curved.
#'
#' @examples
#' \dontrun{
#' library(oce)
#' data(coastlineWorld)
#' par(mar=rep(1, 4))
#' ## Arctic 100m, 2km, 3km isobaths, showing shelves and ridges.
#' mapPlot(coastlineWorld, latitudelim=c(60, 120), longitudelim=c(-130,-50),
#'         projection="+proj=stere +lat_0=90")
#' data(topoWorld)
#' lon <- topoWorld[['longitude']]
#' lat <- topoWorld[['latitude']]
#' z <- topoWorld[['z']]
#' mapContour(lon, lat, z, levels=c(-100, -2000, -3000), col=1:3, lwd=2)
#' }
#' @author Dan Kelley
#' @seealso A map must first have been created with \code{\link{mapPlot}}.
#' @family functions related to maps
mapContour <- function(longitude=seq(0, 1, length.out=nrow(z)),
                       latitude=seq(0, 1, length.out=ncol(z)),
                       z,
                       nlevels=10, levels=pretty(range(z, na.rm=TRUE), nlevels),
                       ##labels=null,
                       ##xlim=range(longitude, finite=TRUE),
                       #ylim=range(latitude, finite=TRUE),
                       ##labcex=0.6,
                       #drawlabels=TRUE,
                       ##method="flattest",
                       ##vfont,
                       ## axes=TRUE, frame.plot=axes,
                       col=par("fg"), lty=par("lty"), lwd=par("lwd"))
{
    if ("none" == .Projection()$type)
        stop("must create a map first, with mapPlot()\n")
    if ("data" %in% slotNames(longitude) && # handle e.g. 'topo' class
        3 == sum(c("longitude", "latitude", "z") %in% names(longitude@data))) {
        z <- longitude@data$z
        latitude <- longitude@data$latitude
        longitude <- longitude@data$longitude
    }
    nlevels <- length(levels)
    col <- rep(col, nlevels)
    lty <- rep(lty, nlevels)
    lwd <- rep(lwd, nlevels)
    xx <- seq_along(longitude)
    yy <- seq_along(latitude)
    if (length(xx) > 1 && diff(longitude[1:2]) < 0) {
        xx <- rev(xx)
        z <- z[xx, ]
        ##cat("flipped in x\n")
    }
    if (length(yy) > 1 && diff(latitude[1:2]) < 0) {
        yy <- rev(yy)
        z <- z[, yy]
        ##cat("flipped in y\n")
    }
    for (ilevel in 1:nlevels) {
        cl <- contourLines(x=longitude[xx],
                           y=latitude[yy],
                           z=z, levels=levels[ilevel])
        for (segment in 1:length(cl)) {
            if (length(cl) > 0) {
                mapLines(cl[[segment]]$x, cl[[segment]]$y,
                         lty=lty[ilevel], lwd=lwd[ilevel], col=col[ilevel])
                ## if (segment == 1) {
                ##     cat(str(cl[[segment]]$x))
                ##     cat(str(cl[[segment]]$y))
                ## }
            }
        }
    }
    ## FIXME: labels, using labcex and vfont
}

#' Draw a coordinate system
#'
#' Draws arrows on a map to indicate a coordinate system, e.g. for an
#' to indicate a coordinate system set up so that one axis is parallel
#' to a coastline.
#'
#' @details This is a preliminary version of this function. It only 
#' works if the lines of constant latitude are horizontal on the plot.
#'
#' @param latitude numeric value of latitude in degrees.
#' @param longitude numeric value of longiutde in degrees.
#' @param L axis length in km.
#' @param phi angle, in degrees counterclockwise, that the "x" axis makes to a line of latitude.
#' @param ... plotting arguments, passed to \code{\link{mapArrows}};
#' see \dQuote{Examples} for how to control the arrow-head size.
#'
#' @examples
#' \dontrun{
#' library(oce)
#' data(coastlineWorldFine, package='ocedata')
#' HfxLon <- -63.5752
#' HfxLat <- 44.6488
#' mapPlot(coastlineWorldFine, proj='+proj=merc', 
#'         longitudelim=HfxLon+c(-2,2), latitudelim=HfxLat+c(-2,2),
#'         col='lightgrey')
#' mapCoordinateSystem(HfxLon, HfxLat, phi=45, length=0.05)
#' }
#' @author Chantelle Layton
mapCoordinateSystem <- function(longitude, latitude, L=100, phi=0, ...)
{
    if (missing(longitude)) 
        stop('must supply longitude')
    if (missing(latitude)) 
        stop('must supply latitude')
    R <- 6371
    pi <- 4 * atan2(1, 1)
    phirad <- phi*pi/180 + c(0, pi/2)
    kmperlon <- pi*R*cos(latitude*pi/180)/180
    kmperlat <- pi*R/180
    dx <- L*cos(phirad)
    dy <- L*sin(phirad)
    dlon <- dx/kmperlon
    dlat <- dy/kmperlat
    lonend <- longitude + dlon
    latend <- latitude + dlat
    mapArrows(longitude, latitude, lonend[1], latend[1], ...)
    mapArrows(longitude, latitude, lonend[2], latend[2], ...)
}

#' Add a Direction Field to an Existing Map
#'
#' Plot a direction field on a existing map.
#'
#' @param longitude,latitude vectors of the starting points for arrows.
#'
#' @param u,v components of a vector to be shown as a direction
#'     field.
#'
#' @param scale latitude degrees per unit of \code{u} or \code{v}.
#'
#' @param length length of arrow heads, passed to \code{\link{arrows}}.
#'
#' @param code code of arrows, passed to \code{\link{arrows}}.
#'
#' @param col colour of arrows.  This may be a single colour, or a matrix
#'     of colours of the same dimension as \code{u}.
#'
#' @param \dots optional arguments passed to \code{\link{arrows}}, e.g.
#'     \code{angle} and \code{lwd} can be useful in differentiating different
#'     fields.
#'
#'
#' @details
#' Adds arrows for a direction field on an existing map.  There are different
#' possibilities for how \code{longitude}, \code{latitude} and \code{u} and
#' \code{v} match up.  In one common case, all four of these are matrices, e.g.
#' output from a numerical model.  In another, \code{longitude} and
#' \code{latitude} are the coordinates along the matrices, and are thus stored in
#' vectors with lengths that match appropriately.
#'
#' @examples
#' \dontrun{
#' library(oce)
#' data(coastlineWorld)
#' par(mar=rep(2, 4))
#' mapPlot(coastlineWorld, longitudelim=c(-120,-55), latitudelim=c(35, 50),
#'         proj="+proj=laea +lat0=40 +lat1=60" +lon_0=-110)
#' lon <- seq(-120, -60, 15)
#' lat <- 45 + seq(-15, 15, 5)
#' lonm <- matrix(expand.grid(lon, lat)[, 1], nrow=length(lon))
#' latm <- matrix(expand.grid(lon, lat)[, 2], nrow=length(lon))
#' ## vectors pointed 45 degrees clockwise from north
#' u <- matrix(1/sqrt(2), nrow=length(lon), ncol=length(lat))
#' v <- matrix(1/sqrt(2), nrow=length(lon), ncol=length(lat))
#' mapDirectionField(lon, lat, u, v, scale=3)
#' mapDirectionField(lonm, latm, 0, 1, scale=3, col='red')
#' # Color code by longitude, using thick lines
#' col <- colormap(lonm)$zcol
#' mapDirectionField(lonm, latm, 1, 0, scale=3, col=col, lwd=2)
#' }
#'
#' @author Dan Kelley
#' @seealso A map must first have been created with \code{\link{mapPlot}}.
#' @family functions related to maps
mapDirectionField <- function(longitude, latitude, u, v,
                              scale=1, length=0.05, code=2, col=par("fg"), ...)
{
    if ("none" == .Projection()$type)
        stop("must create a map first, with mapPlot()\n")
    ## handle case where lon and lat are coords on edges of grid
    if (is.matrix(u)) {
        if (is.vector(longitude) && is.vector(latitude)) {
            nlon <- length(longitude)
            nlat <- length(latitude)
            longitude <- matrix(rep(longitude, nlat), nrow=nlon)
            latitude <- matrix(rep(latitude, nlon), byrow=TRUE, nrow=nlon)
        }
    }
    xy <- lonlat2map(longitude, latitude)
    ## Calculate spatially-dependent scale (fails for off-page points)
    ## Calculate lon-lat at ends of arrows
    scalex <- scale / cos(pi * latitude / 180)
    latEnd <- latitude + v * scale
    lonEnd <- longitude + u * scalex
    xy <- lonlat2map(longitude, latitude)
    xyEnd <- lonlat2map(lonEnd, latEnd)
    arrows(xy$x, xy$y, xyEnd$x, xyEnd$y, length=length, code=code, col=col, ...)
}



#' Convert From Longitude and Latitude to X and Y
#'
#' Find (x, y) values corresponding to (longitude, latitude) values, using the
#' present projection.
#'
#'
#' @param longitude vector of the longitudes of points, or an object from which
#' both latitude and longitude can be inferred (e.g. a coastline file, or the
#' return value from \code{\link{mapLocator}}), in which case the following
#' two arguments are ignored.
#'
#' @param latitude vector of latitudes of points, needed only if they cannot
#' be inferred from the first argument.
#'
#' @details
#' This is mainly a wrapper around \code{\link{lonlat2map}}.
#'
#' @return
#' A list containing \code{x} and \code{y}.
#'
#'
#' @examples
#' \dontrun{
#' library(oce)
#' data(coastlineWorld)
#' par(mfrow=c(2, 1), mar=rep(2, 4))
#' mapPlot(coastlineWorld, projection="+proj=moll") # sets a projection
#' xy <- mapLongitudeLatitudeXY(coastlineWorld)
#' plot(xy, type='l', asp=1)
#' }
#'
#' @author Dan Kelley
#' @seealso A map must first have been created with \code{\link{mapPlot}}.
#' @family functions related to maps
mapLongitudeLatitudeXY <- function(longitude, latitude)
{
    if ("none" == .Projection()$type)
        stop("must create a map first, with mapPlot()\n")
    if (missing(longitude))
        stop("must give 'longitude' and possibly 'latitude'")
    if (!missing(longitude) && ("data" %in% slotNames(longitude))) {
        tmp <- longitude@data
        if ( ("longitude" %in% names(tmp)) && ("latitude" %in% names(tmp)) ) {
            latitude <- tmp$latitude
            longitude <- tmp$longitude
        }
    }
    proj <- lonlat2map(longitude, latitude)
    list(x=proj$x, y=proj$y) # if other properties prove helpful, may add them
}



#' Draw a Map
#'
#' @description
#' Plot coordinates as a map, using one of the subset of projections
#' provided by the \CRANpkg{rgdal} package.  The projection information specified
#' with the \code{mapPlot} call is stored so that can be retrieved by related
#' functions, making it easy to add more items so the map, including points,
#' lines, text, images and contours.
#'
#' @param longitude either a vector of longitudes of points to be plotted, or
#' something (an \code{oce} object, a list, or a data frame) from which both
#' longitude and latitude may be inferred (in which case the \code{latitude}
#' argument is ignored).  If \code{longitude} is missing, both it and
#' \code{latitude} are taken from \code{\link{coastlineWorld}}.
#'
#' @param latitude vector of latitudes of points to be plotted (ignored
#' if the first argument contains both latitude and longitude).
#'
#' @param longitudelim optional vector of length two, indicating the
#' longitude limits of the plot. This value is used in the selection of
#' longitude lines that are shown (and possibly
#' labelled on the axes). In some cases, e.g. for polar views,
#' this can lead to odd results, with some expected longitude lines
#' being left out of the plot.  Altering \code{longitudelim} can
#' often help in such cases, e.g. \code{longitudelim=c(-180, 180)} will
#' force the drawing of lines all around the globe.
#'
#' @param latitudelim optional vector of length two, indicating
#' the latitude limits of the plot. This, together with \code{longitudelim}
#' (and, importantly, the geometry of the plot device) is used in the
#' selection of map scale.
#'
#' @param grid either a number (or pair of numbers) indicating the spacing of
#' longitude and latitude lines, in degrees, or a logical value (or pair of
#' values) indicating whether to draw an auto-scaled grid, or whether to skip
#' the grid drawing.  In the case of numerical values, \code{NA} can be used to
#' turn off the grid in longitude or latitude.  Grids are set up based on
#' examination of the scale used in middle 10 percent of the plot area, and for
#' most projections this works quite well.  If not, one may set
#' \code{grid=FALSE} and add a grid later with \code{\link{mapGrid}}.
#'
#' @param bg colour of the background (ignored).
#'
#' @param fill \strong{(deprecated)} is a deprecated argument; see
#' \link{oce-deprecated}.
#'
#' @param border colour of coastlines and international borders (ignored unless
#' \code{type="polygon"}.
#'
#' @param col either the colour for filling polygons (if \code{type="polygon"})
#' or the colour of the points and line segments (if \code{type="p"},
#' \code{type="l"}, or \code{type="o"}). If \code{col=NULL} then a default
#' will be set: no coastline filling for the \code{type="polygon"} case,
#' or black coastlines, for \code{type="p"}, \code{type="l"}, or
#' \code{type="o"}.
#'
#' @param clip logical value indicating whether to trim any coastline elements that lie wholly
#' outside the plot region. This can prevent e.g. a problem of filling the whole plot area of
#' an Arctic stereopolar view, because the projected trace for Antarctica lies outside all 
#' other regions so the whole of the world ends up being "land".  Setting \code{clip=FALSE}
#' disables this action, which may be of benefit in rare instances in the line connecting
#' two points on a coastline may cross the plot domain, even if those points are outside
#' that domain.
#'
#' @param type indication of type; may be \code{"polygon"}, for a filled polygon,
#' \code{"p"} for points, \code{"l"} for line segments, or \code{"o"} for points
#' overlain with line segments.
#'
#' @param axes logical value indicating whether to draw longitude and latitude
#' values in the lower and left margin, respectively.  This may not work well
#' for some projections or scales.
#'
#' @param cex character expansion factor for plot symbols,
#' used if \code{type='p'} or any other value that yields symbols.
#'
#' @param cex.axis axis-label expansion factor (see \code{\link{par}}).
#'
#' @param mgp three-element numerical vector describing axis-label
#' placement, passed to \code{\link{mapAxis}}.
#'
#' @param drawBox logical value indicating whether to draw a box around the plot.
#' This is helpful for many projections at sub-global scale.
#'
#' @param showHemi logical value indicating whether to show the hemisphere in
#' axis tick labels.
#'
#' @param polarCircle a number indicating the number of degrees of latitude
#' extending from the poles, within which zones are not drawn.
#'
#' @param lonlabel,latlabel,sides Optional vectors of longitude and latitude
#' to label on the indicated sides of plot, passed to
#' \code{\link{plot,coastline-method}}.  Using these arguments permits reasonably
#' simple customization.  If they are are not provided, reasonable defaults
#' will be used.
#'
#' @param projection optional indication of projection, in one of two
#' forms. First, it may be a character string in the "CRS" format that is
#' used by the \code{rgdal} package (and in much of modern computer-based
#' cartography). For example, \code{projection="+proj=merc"} specifies a
#' Mercator projection. The second format is the output from
#' \code{\link[sp]{CRS}} in the \CRANpkg{sp} package, which is an object
#' with a slot named \code{projarg} that gets used as a projection string.
#' See \dQuote{Details}.
#'
#' @param trim logical value indicating whether to trim islands or lakes
#' containing only points that are off-scale of the current plot box.  This
#' solves the problem of Antarctica overfilling the entire domain, for an
#' Arctic-centred stereographic projection.  It is not a perfect solution,
#' though, because the line segment joining two off-scale points might
#' intersect the plotting box.
#'
#' @param tissot logical value indicating whether to use \code{\link{mapTissot}}
#' to plot Tissot indicatrices, i.e. ellipses at grid intersection points, which
#' indicate map distortion.
#'
#' @param debug a flag that turns on debugging.  Set to 1 to get a moderate
#' amount of debugging information, or to 2 to get more.
#'
#' @param ... optional arguments passed to some plotting functions.  This can
#' be useful in many ways, e.g.  Example 5 shows how to use \code{xlim} etc to
#' reproduce a scale exactly between two plots.
#'
#' @details
#' Creates a map using the indicated projection.  As noted in the
#' information on the \code{projection} argument, projections are specified in
#' the notation used by \code{project()} in the \code{rgdal} package; see
#' \dQuote{Available Projections} for a list of possibilities.
#'
#' Once a projection is set, other \code{map*} functions may be used to add to
#' the map.
#'
#' Further details on map projections are provided by [1,11], an exhaustive
#' treatment that includes many illustrations, an overview of the history of the
#' topic, and some notes on the strengths and weaknesses of the various
#' formulations.  See especially pages 2 through 7, which define terms and
#' provide recommendations.  Reference [2] is also useful, especially regarding
#' datum shifts; [3] and [4] are less detailed and perhaps better for novices.
#' See [8] for a gallery of projections.
#'
#' @seealso
#' Points may be added to a map with \code{\link{mapPoints}}, lines with
#' \code{\link{mapLines}}, text with \code{\link{mapText}}, polygons with
#' \code{\link{mapPolygon}}, images with \code{\link{mapImage}}, and scale bars
#' with \code{\link{mapScalebar}}.  Points on a map may be determined with mouse
#' clicks using \code{\link{mapLocator}}.  Great circle paths can be calculated
#' with \code{\link{geodGc}}.  See [8] for a demonstration of the available map
#' projections (with graphs).
#'
#' @examples
#' \dontrun{
#' library(oce)
#' data(coastlineWorld)
#'
#' # Example 1.
#' # Mollweide ([1] page 54) is an equal-area projection that works well
#' # for whole-globe views, below shown in a Pacific-focus view.
#' # Note that filling is not employed when the prime meridian
#' # is shifted, because this causes a problem with Antarctica
#' par(mfrow=c(2, 1), mar=c(3, 3, 1, 1))
#' mapPlot(coastlineWorld, projection="+proj=moll", col='gray')
#' mtext("Mollweide", adj=1)
#' cl180 <- coastlineCut(coastlineWorld, lon_0=-180)
#' mapPlot(cl180, projection="+proj=moll +lon_0=-180")
#' mtext("Mollweide", adj=1)
#' par(mfrow=c(1, 1))
#'
#' # Example 2.
#' # Orthographic projections resemble a globe, making them attractive for
#' # non-technical use, but they are neither conformal nor equal-area, so they
#' # are somewhat limited for serious use on large scales.  See Section 20 of
#' # [1]. Note that filling is not employed because it causes a problem with
#' # Antarctica.
#' par(mar=c(3, 3, 1, 1))
#' mapPlot(coastlineWorld, projection="+proj=ortho +lon_0=-180")
#' mtext("Orthographic", adj=1)
#'
#' # Example 3.
#' # The Lambert conformal conic projection is an equal-area projection
#' # recommended by [1], page 95, for regions of large east-west extent
#' # away from the equator, here illustrated for the USA and Canada.
#' par(mar=c(3, 3, 1, 1))
#' mapPlot(coastlineCut(coastlineWorld, -100),
#'         longitudelim=c(-130,-55), latitudelim=c(35, 60),
#'         projection="+proj=lcc +lat_0=30 +lat_1=60 +lon_0=-100", col='gray')
#' mtext("Lambert conformal", adj=1)
#'
#' # Example 4.
#' # The stereographic projection [1], page 120, is conformal, used
#' # below for an Arctic view with a Canadian focus.  Note the trick of going
#' # past the pole: the second latitudelim value is 180 minus the first, and the
#' # second longitudelim is 180 plus the first; this uses image points "over"
#' # the pole.
#' par(mar=c(3, 3, 1, 1))
#' mapPlot(coastlineCut(coastlineWorld, -135),
#'         longitudelim=c(-130, 50), latitudelim=c(70, 110),
#'         proj="+proj=stere +lat_0=90 +lon_0=-135", col='gray')
#' mtext("Stereographic", adj=1)
#'
#' # Example 5.
#' # Spinning globe: create PNG files that can be assembled into a movie
#' png("globe-%03d.png")
#' lons <- seq(360, 0, -15)
#' par(mar=rep(0, 4))
#' for (i in seq_along(lons)) {
#'     p <- paste("+proj=ortho +lat_0=30 +lon_0=", lons[i], sep="")
#'     if (i == 1) {
#'         mapPlot(coastlineCut(coastlineWorld, lons[i]),
#'                 projection=p, col="lightgray")
#'         xlim <- par("usr")[1:2]
#'         ylim <- par("usr")[3:4]
#'     } else {
#'         mapPlot(coastlineCut(coastlineWorld, lons[i]),
#'                 projection=p, col="lightgray",
#'                 xlim=xlim, ylim=ylim, xaxs="i", yaxs="i")
#'     }
#' }
#' dev.off()
#' }
#
#' @section Available Projections:
#' Map projections are provided by the
#' \code{rgdal} package, but not all projections in that package are
#' available. The available list is given in the table
#' below. The cartographic community has set up a naming scheme in a coded
#' scheme, e.g. \code{projection="+proj=aea"} selects the Albers equal area
#' projection.
#'
#' The allowed projections include those PROJ.4 projections provided by
#' \code{rgdal} that have inverses, minus a few that cause problems:
#' \code{alsk} overdraws \code{coastlineWorld}, and is a niche projection for Alaska;
#' \code{calcofi} is not a real projection, but rather a coordinate system;
#' \code{gs48} overdraws \code{coastlineWorld}, and is a niche projection for the USA;
#' \code{gs50} overdraws \code{coastlineWorld}, and is a niche projection for the USA;
#' \code{gstmerc} overdraws \code{coastlineWorld};
#' \code{isea} causes segmentation faults on OSX systems;
#' \code{krovak} overdraws \code{coastlineWorld}, and is a niche projection for the Czech Republic;
#' \code{labrd} returns \code{NaN} for most of the world, and is a niche projection for Madagascar;
#' \code{lee_os} overdraws \code{coastlineWorld};
#' and
#' \code{nzmg} overdraws \code{coastlineWorld}.
#'
#'
#' The information in the table is reformatted from the output of the unix
#' command \code{proj -lP}, where \code{proj} is provided by version 4.9.0 of
#' the PROJ.4 system. Most of the arguments listed have default values. In
#' addition, most projections can handle arguments \code{lon_0} and
#' \code{lat_0}, for shifting the reference point, although in some cases
#' shifting the longitude can yield poor filling of coastlines.
#'
#' Further details of the projections and the controlling arguments are
#' provided at several websites, because PROJ.4 has been incorporated into
#' \code{rgdal} and other R packages, plus many other software systems; a good
#' starting point for learning is [6].
#'
#' See \dQuote{Examples} for suggested projections for some common
#' applications, and [8] for a gallery indicating how to use every projection.
#'
#' \tabular{lll}{
#' \strong{Projection}                       \tab \strong{Code}   \tab \strong{Arguments}\cr
#' Albers equal area                         \tab \code{aea}      \tab \code{lat_1}, \code{lat_2}\cr
#' Azimuthal equidistant                     \tab \code{aeqd}     \tab \code{lat_0}, \code{guam}\cr
#' Aitoff                                    \tab \code{aitoff}   \tab - \cr
#' Mod. stererographics of Alaska            \tab \code{alsk}     \tab - \cr
#' Bipolar conic of western hemisphere       \tab \code{bipc}     \tab - \cr
#' Bonne Werner                              \tab \code{bonne}    \tab \code{lat_1}\cr
#' Cassini                                   \tab \code{cass}     \tab - \cr
#' Central cylindrical                       \tab \code{cc}       \tab - \cr
#' Equal area cylindrical                    \tab \code{cea}      \tab \code{lat_ts}\cr
#' Collignon                                 \tab \code{collg}    \tab - \cr
#' Craster parabolic Putnins P4              \tab \code{crast}    \tab - \cr
#' Eckert I                                  \tab \code{eck1}     \tab - \cr
#' Eckert II                                 \tab \code{eck2}     \tab - \cr
#' Eckert III                                \tab \code{eck3}     \tab - \cr
#' Eckert IV                                 \tab \code{eck4}     \tab - \cr
#' Eckert V                                  \tab \code{eck5}     \tab - \cr
#' Eckert VI                                 \tab \code{eck6}     \tab - \cr
#' Equidistant cylindrical plate (Caree)     \tab \code{eqc}      \tab \code{lat_ts}, \code{lat_0}\cr
#' Equidistant conic                         \tab \code{eqdc}     \tab \code{lat_1}, \code{lat_2}\cr
#' Euler                                     \tab \code{euler}    \tab \code{lat_1}, \code{lat_2}\cr
#' Extended transverse Mercator              \tab \code{etmerc}   \tab \code{lat_ts}, \code{lat_0}\cr
#' Fahey                                     \tab \code{fahey}    \tab - \cr
#' Foucaut                                   \tab \code{fouc}     \tab - \cr
#' Foucaut sinusoidal                        \tab \code{fouc_s}   \tab - \cr
#' Gall stereographic                        \tab \code{gall}     \tab - \cr
#' Geostationary satellite view              \tab \code{geos}     \tab \code{h}\cr
#' General sinusoidal series                 \tab \code{gn_sinu}  \tab \code{m}, \code{n}\cr
#' Gnomonic                                  \tab \code{gnom}     \tab - \cr
#' Goode homolosine                          \tab \code{goode}    \tab - \cr
## Mod. stererographics of 48 U.S.           \tab \code{gs48}     \tab - \cr
## Mod. stererographics of 50 U.S.           \tab \code{gs50}     \tab - \cr
#' Hatano asymmetrical equal area            \tab \code{hatano}   \tab - \cr
#' HEALPix                                   \tab \code{healpix}  \tab - \cr
#' rHEALPix                                  \tab \code{rhealpix} \tab \code{north_square}, \code{south_square}\cr
#' Interrupted Goode homolosine              \tab \code{igh}      \tab -\cr
#' Int'l map of the world polyconic          \tab \code{imw_p}    \tab \code{lat_1}, \code{lat_2}, \code{lon_1}\cr
#' Kavraisky V                               \tab \code{kav5}     \tab - \cr
#' Kavraisky VII                             \tab \code{kav7}     \tab - \cr
## Krovak                                    \tab \code{krovak}   \tab - \cr
#' Lambert azimuthal equal area              \tab \code{laea}     \tab - \cr
#' Longitude and latitude                    \tab \code{lonlat}   \tab - \cr
#' Longitude and latitude                    \tab \code{longlat}   \tab - \cr
#' Longitude and latitude                    \tab \code{latlon}   \tab - \cr
#' Lambert conformal conic                   \tab \code{lcc}      \tab \code{lat_1}, \code{lat_2}, \code{lat_0}\cr
#' Lambert conformal conic alt. [DEPRECATED] \tab \code{lcca}     \tab \code{lat_0}\cr
#' Lambert equal area conic                  \tab \code{leac}     \tab \code{lat_1}, \code{south}\cr
## Lee oblated stereographic                 \tab \code{lee_os}   \tab\cr
#' Loximuthal                                \tab \code{loxim}    \tab\cr
#' Space oblique for Landsat                 \tab \code{lsat}     \tab \code{lsat}, \code{path}\cr
#' McBryde-Thomas flat-polar sine, no. 1     \tab \code{mbt_s}    \tab\cr
#' McBryde-Thomas flat-polar sine, no. 2     \tab \code{mbt_fps}  \tab\cr
#' McBryde-Thomas flat-polar parabolic       \tab \code{mbtfpp}   \tab\cr
#' McBryde-Thomas flat-polar quartic         \tab \code{mbtfpq}   \tab\cr
#' McBryde-Thomas flat-polar sinusoidal      \tab \code{mbtfps}   \tab\cr
#' Mercator                                  \tab \code{merc}     \tab \code{lat_ts}\cr
#' Miller oblated stereographic              \tab \code{mil_os}   \tab\cr
#' Miller cylindrical                        \tab \code{mill}     \tab\cr
#' Mollweide                                 \tab \code{moll}     \tab\cr
#' Murdoch I                                 \tab \code{murd1}    \tab \code{lat_1}, \code{lat_2}\cr
#' Murdoch II                                \tab \code{murd2}    \tab \code{lat_1}, \code{lat_2}\cr
#' murdoch III                               \tab \code{murd3}    \tab \code{lat_1}, \code{lat_2}\cr
#' Natural earth                             \tab \code{natearth} \tab\cr
#' Nell                                      \tab \code{nell}     \tab\cr
#' Nell-Hammer                               \tab \code{nell_h}   \tab\cr
#' Near-sided perspective                    \tab \code{nsper}    \tab \code{h}\cr
#' New Zealand map grid                      \tab \code{nzmg}     \tab\cr
#' General oblique transformation            \tab \code{ob_tran}  \tab \code{o_proj}, \code{o_lat_p}, \code{o_lon_p}, \code{o_alpha}, \code{o_lon_c}\cr
#'                                           \tab                 \tab \code{o_lat_c}, \code{o_lon_1}, \code{o_lat_1}, \code{o_lon_2}, \code{o_lat_2}\cr
#' Oblique cylindrical equal area            \tab \code{ocea}     \tab \code{lat_1}, \code{lat_2}, \code{lon_1}, \code{lon_2}\cr
#' Oblated equal area                        \tab \code{oea}      \tab \code{n}, \code{m}, \code{theta}\cr
#' Oblique Mercator                          \tab \code{omerc}    \tab \code{alpha}, \code{gamma}, \code{no_off}, \code{lonc}, \code{lon_1},\cr
#'                                           \tab                 \tab \code{lat_1}, \code{lon_2}, \code{lat_2}\cr
#' Orthographic                              \tab \code{ortho}    \tab - \cr
#' Perspective conic                         \tab \code{pconic}   \tab \code{lat_1}, \code{lat_2}\cr
#' Polyconic American                        \tab \code{poly}     \tab - \cr
#' Putnins P1                                \tab \code{putp1}    \tab - \cr
#' Putnins P2                                \tab \code{putp2}    \tab - \cr
#' Putnins P3                                \tab \code{putp3}    \tab - \cr
#' Putnins P3'                               \tab \code{putp3p}   \tab - \cr
#' Putnins P4'                               \tab \code{putp4p}   \tab - \cr
#' Putnins P5                                \tab \code{putp5}    \tab - \cr
#' Putnins P5'                               \tab \code{putp5p}   \tab - \cr
#' Putnins P6                                \tab \code{putp6}    \tab - \cr
#' Putnins P6'                               \tab \code{putp6p}   \tab - \cr
#' Quartic authalic                          \tab \code{qua_aut}  \tab - \cr
#' Quadrilateralized spherical cube          \tab \code{qsc}      \tab - \cr
#' Robinson                                  \tab \code{robin}    \tab - \cr
#' Roussilhe stereographic                   \tab \code{rouss}    \tab - \cr
#' Sinusoidal aka Sanson-Flamsteed           \tab \code{sinu}     \tab - \cr
#' Swiss. oblique Mercator                   \tab \code{somerc}   \tab - \cr
#' Stereographic                             \tab \code{stere}    \tab \code{lat_ts}\cr
#' Oblique stereographic alternative         \tab \code{sterea}   \tab - \cr
## Gauss-Schreiber transverse Mercator       \tab \code{gstmerc}  \tab \code{lat_0}, \code{lon_0}, \code{k_0}\cr
#' Transverse cylindrical equal area         \tab \code{tcea}     \tab - \cr
#' Tissot                                    \tab \code{tissot}   \tab \code{lat_1}, \code{lat_2}\cr
#' Transverse Mercator                       \tab \code{tmerc}    \tab - \cr
#' Two point equidistant                     \tab \code{tpeqd}    \tab \code{lat_1}, \code{lon_1}, \code{lat_2}, \code{lon_2}\cr
#' Tilted perspective                        \tab \code{tpers}    \tab \code{tilt}, \code{azi}, \code{h}\cr
#' Universal polar stereographic             \tab \code{ups}      \tab \code{south}\cr
#' Urmaev flat-polar sinusoidal              \tab \code{urmfps}   \tab \code{n}\cr
#' Universal transverse Mercator             \tab \code{utm}      \tab \code{zone}, \code{south}\cr
#' van der Grinten I                         \tab \code{vandg}    \tab - \cr
#' Vitkovsky I                               \tab \code{vitk1}    \tab \code{lat_1}, \code{lat_2}\cr
#' Wagner I Kavraisky VI                     \tab \code{wag1}     \tab - \cr
#' Wagner II                                 \tab \code{wag2}     \tab - \cr
#' Wagner III                                \tab \code{wag3}     \tab \code{lat_ts}\cr
#' Wagner IV                                 \tab \code{wag4}     \tab - \cr
#' Wagner V                                  \tab \code{wag5}     \tab - \cr
#' Wagner VI                                 \tab \code{wag6}     \tab - \cr
#' Werenskiold I                             \tab \code{weren}    \tab - \cr
#' Winkel I                                  \tab \code{wink1}    \tab \code{lat_ts}\cr
#' Winkel Tripel                             \tab \code{wintri}   \tab \code{lat_ts}\cr
#' }
#'
#' @section Available ellipse formulations:
#' In the PROJ.4 system of specifying projections, the following ellipse
#' models are available: \code{MERIT},
#' \code{SGS85}, \code{GRS80}, \code{IAU76}, \code{airy}, \code{APL4.9},
#' \code{NWL9D}, \code{mod_airy}, \code{andrae}, \code{aust_SA}, \code{GRS67},
#' \code{bessel}, \code{bess_nam}, \code{clrk66}, \code{clrk80},
#' \code{clrk80ign}, \code{CPM}, \code{delmbr}, \code{engelis},
#' \code{evrst30}, \code{evrst48}, \code{evrst56}, \code{evrst69},
#' \code{evrstSS}, \code{fschr60}, \code{fschr60m}, \code{fschr68},
#' \code{helmert}, \code{hough}, \code{intl}, \code{krass}, \code{kaula},
#' \code{lerch}, \code{mprts}, \code{new_intl}, \code{plessis}, \code{SEasia},
#' \code{walbeck}, \code{WGS60}, \code{WGS66}, \code{WGS72}, \code{WGS84}, and
#' \code{sphere} (the default).  For example, use \code{projection="+proj=aea
#'     +ellps=WGS84"} for an Albers Equal Area projection using the most
#' recent of the World Geodetic System model. It is unlikely that changing the
#' ellipse will have a visible effect on plotted material at the plot scale
#' appropriate to most oceanographic applications.
#'
#' @section Available datum formulations:
#' In the PROJ.4 system of specifying
#' projections, the following datum formulations are available: \code{WGS84},
#' \code{GGRS87}, \code{Greek_Geodetic_Reference_System_1987}, \code{NAD83},
#' \code{North_American_Datum_1983}, \code{NAD27},
#' \code{North_American_Datum_1927}, \code{potsdam}, \code{Potsdam},
#' \code{carthage}, \code{Carthage}, \code{hermannskogel},
#' \code{Hermannskogel}, \code{ire65}, \code{Ireland}, \code{nzgd49},
#' \code{New}, \code{OSGB36}, and \code{Airy}. It is unlikely that changing
#' the datum will have a visible effect on plotted material at the plot scale
#' appropriate to most oceanographic applications.
#'
#' @section Choosing a projection:
#' The best choice of projection depends on the application.
#' Readers may find \code{projection="+proj=moll"} useful for world-wide
#' plots, \code{ortho} for hemispheres viewed from the equator, \code{stere}
#' for polar views, \code{lcc} for wide meridional ranges in mid latitudes,
#' and \code{merc} in limited-area cases where angle preservation is
#' important.
#'
#' @section Issues:
#' Map projection is a complicated matter that is addressed here
#' in a limited and pragmatic way.  For example, \code{mapPlot} tries to draw
#' axes along a box containing the map, instead of trying to find spots along
#' the ``edge'' of the map at which to put longitude and latitude labels.
#' This design choice greatly simplifies the coding effort, freeing up time to
#' work on issues regarded as more pressing.  Chief among those issues are (a)
#' the occurrence of horizontal lines in maps that have prime meridians
#' (b) inaccurate filling of land regions that (again) occur with shifted
#' meridians and (c) inaccurate filling of Antarctica in some projections.
#' Generally, issues are tackled first for commonly used projections, such as
#' those used in the examples.
#'
#' @section Changes:
#' \itemize{
#' \item 2017-09-30: \code{lcca} deprecated, because its inverse was
#' wildly inaccurate in a Pacific Antartic-Alaska application
#' (see \url{https://github.com/dankelley/oce/issues/1303}).
#' }
#'
#'
#' @author Dan Kelley and Clark Richards
#'
#' @references
#'
#' 1. Snyder, John P., 1987.  Map Projections: A Working Manual.  USGS
#' Professional Paper: 1395 (available at
#' \url{pubs.usgs.gov/pp/1395/report.pdf}).
#'
#' 2. Natural Resources Canada
#' \url{http://www.nrcan.gc.ca/earth-sciences/geography/topographic-information/maps/9805}
#'
#' 3. Wikipedia page \url{http://en.wikipedia.org/wiki/List_of_map_projections}
#'
#' 4. Radical Cartography website
#' \code{http://www.radicalcartography.net/?projectionref} (This URL worked
#' prior to Nov 16, 2016, but was found to fail on that date.)
#'
#' 5. The \code{PROJ.4} website is \url{http://trac.osgeo.org/proj}, and it is
#' the place to start to learn about the code.
#'
#' 6. \code{PROJ.4} projection details were once at
#' \code{http://www.remotesensing.org/geotiff/proj_list/} but it was 
#' discovered on Dec 18, 2016, that this link no longer exists. Indeed, there
#' seems to have been significant reorganization of websites related to this.
#' The base website seems to be \url{https://trac.osgeo.org/geotiff/} and that
#' lists only what is called an unofficial listing, on the wayback web-archiver server
#' \url{http://web.archive.org/web/20160802172057/http://www.remotesensing.org/geotiff/proj_list/}
#'
#' 7. A gallery of map plots is provided at
#' \url{http://dankelley.github.io/r/2015/04/03/oce-proj.html}.
#'
#' 8. A fascinating historical perspective is provided by Snyder, J. P.
#' (1993). Two thousand years of map projections. University of Chicago Press.
#'
#' @family functions related to maps
mapPlot <- function(longitude, latitude, longitudelim, latitudelim, grid=TRUE,
                    bg, fill,
                    border=NULL, col=NULL,
                    clip=TRUE,
                    type='polygon',
                    axes=TRUE, cex, cex.axis=1, mgp=c(0, 0.5, 0), drawBox=TRUE, showHemi=TRUE,
                    polarCircle=0, lonlabel=NULL, latlabel=NULL, sides=NULL,
                    projection="+proj=moll", tissot=FALSE, trim=TRUE,
                    debug=getOption("oceDebug"),
                    ...)
{
    dots <- list(...)
    gridOrig <- grid
    if (1 == length(gridOrig))
        gridOrig <- rep(gridOrig, 2)
    if (!missing(projection) && inherits(projection, "CRS")) {
        projection <- projection@projargs
    }
    oceDebug(debug, "mapPlot(longitude, latitude",
             ", longitudelim=", if (missing(longitudelim)) "(missing)" else c("c(", paste(format(longitudelim, digits=4), collapse=","), ")"),
             ", longitudelim=", if (missing(latitudelim)) "(missing)" else c("c(", paste(format(latitudelim, digits=4), collapse=","), ")"),
             ", type=\"", type, "\"",
             ", projection=\"", if (is.null(projection)) "NULL" else projection, "\"",
             ", grid=", grid,
             ", ...) {\n", sep="", unindent=1)
    if (length(grep("=[ ]*lcca", projection)))
        .Deprecated("mapPlot",
                    msg="proj=lcca will be removed soon. See ?'oce-deprecated'.")
    if (missing(longitude)) {
        data("coastlineWorld", package="oce", envir=environment())
        longitude <- get("coastlineWorld")
    }
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
        warning("In mapPlot() : 'fill' being accepted for backwards compatibility; please use 'col' instead", call.=FALSE)
    }

    isTopo <- FALSE
    if (inherits(longitude, "topo")) {
        topo <- longitude
        isTopo <- TRUE
        ## set up to corners of topo lonlat box
        longitude <- range(topo[["longitude"]], na.rm=TRUE)
        latitude <- range(topo[["latitude"]], na.rm=TRUE)
    } else if ("data" %in% slotNames(longitude) && # handle e.g. 'coastline' class
        2 == sum(c("longitude", "latitude") %in% names(longitude@data))) {
        latitude <- longitude@data$latitude
        longitude <- longitude@data$longitude
    }
    if (length(grid) > 2)
        grid <- grid[1:2]
    if (length(grid) == 1)
        grid <- rep(grid[1], 2)
    drawGrid <- (is.logical(grid[1]) && grid[1]) || (is.numeric(grid[1]) && grid[1] > 0)
    # FIXME: 20150326
    #if (is.logical(grid[1]) && grid[1])
    #    grid <- rep(15, 2)
    #message("000")
    if (nchar(projection) && substr(projection, 1, 1) != "+") {
        stop("use PROJ.4 format, e.g. projection=\"+proj=merc\" for Mercator\n", sep="")
    }
    xy <- lonlat2map(longitude, latitude, projection=projection)
    if (!missing(latitudelim) && 0 == diff(latitudelim)) stop("latitudelim must contain two distinct values")
    if (!missing(longitudelim) && 0 == diff(longitudelim)) stop("longitudelim must contain two distinct values")
    limitsGiven <- !missing(latitudelim) && !missing(longitudelim)

    x <- xy$x
    y <- xy$y
    xorig <- xy$x
    yorig <- xy$y
    oce_uhl <- options()$oce_uhl
    if (!is.null(oce_uhl) && oce_uhl == "method 1") {
        ## Insert NA to break long horizontal jumps, which can be caused by coastline
        ## segments that "pass across" the edge of a plot.
        dx <- abs(diff(x))
        bigJumps <- which(dx > (mean(dx, na.rm=TRUE) + 10 * sd(dx, na.rm=TRUE)))
        ##print(longitude[bigJumps])
        for (j in bigJumps) {
            ##message("chopping j=", j)
            lenx <- length(x)
            x <- c(x[seq.int(1, j)], NA, x[seq.int(j+1, lenx)])
            longitude <- c(longitude[seq.int(1, j)], NA, longitude[seq.int(j+1, lenx)])
            y <- c(y[seq.int(1, j)], NA, y[seq.int(j+1, lenx)])
            latitude <- c(latitude[seq.int(1, j)], NA, latitude[seq.int(j+1, lenx)])
        }
        xy <- badFillFix1(x=x, y=y, latitude=latitude, projection=projection)
        x <- xy$x
        y <- xy$y
    }

    ## range gets caught up on Inf values, so we make tmp variables xtmp and ytmp
    xtmp <- x
    xtmp[!is.finite(x)] <- NA
    ytmp <- y
    ytmp[!is.finite(y)] <- NA
    xrange <- range(xtmp, na.rm=TRUE)
    yrange <- range(ytmp, na.rm=TRUE)
    rm(xtmp, ytmp)
    if (any(!is.finite(xrange)) || any(!is.finite(yrange)))
        stop("All the data are 'on the other side of the world' for this map projection")

    oceDebug(debug, "xrange=", paste(xrange, collapse=" "), "\n")
    oceDebug(debug, "yrange=", paste(yrange, collapse=" "), "\n")

    dotnames <- names(dots)
    if ("xlim" %in% dotnames || "ylim" %in% dotnames || "xaxs" %in% dotnames || "yaxs" %in% dotnames) {
        ## for issue 539, i.e. repeated scales
        oceDebug(debug, "xlim, ylim, xaxs, or yaxs was given\n")
        if (type == "polygon") {
            plot(x, y, type="n", xlab="", ylab="", asp=1, axes=FALSE, ...)
            if (is.null(border))
                border <- "black"
            if (is.null(col))
                col <- "white"
            polygon(x, y, border=border, col=col)
        } else {
            plot(x, y, type=type, xlab="", ylab="", asp=1, axes=FALSE, col=col, ...)
        }
    } else {
        oceDebug(debug, "xlim, ylim, xaxs, and yaxs were not given\n")
        if (limitsGiven) {
            oceDebug(debug, "latitudelim and longitudelim are known\n")
            oceDebug(debug, "latitudelim: ", paste(latitudelim, collapse=" "), "\n")
            oceDebug(debug, "longitudelim: ", paste(longitudelim, collapse=" "), "\n")

            ## transform so can do e.g. latlim=c(70, 110) to centre on pole
            ##message("latitudelim: ", paste(latitudelim, collapse=" "))
            ##message("longitudelim: ", paste(longitudelim, collapse=" "))
            if (latitudelim[2] > 90) {
                longitudelim[2] <- 360 + longitudelim[2] - 180
                latitudelim[2] <- 180 - latitudelim[2]
            }
            ##message("latitudelim: ", paste(latitudelim, collapse=" "))
            ##message("longitudelim: ", paste(longitudelim, collapse=" "))
            n <- 10
            BOXx <- c(rep(longitudelim[1], n), seq(longitudelim[1], longitudelim[2], length.out=n),
                      rep(longitudelim[2], n), seq(longitudelim[2], longitudelim[1], length.out=n))
            BOXy <- c(seq(latitudelim[1], latitudelim[2], length.out=n), rep(latitudelim[2], n),
                      seq(latitudelim[2], latitudelim[1], length.out=n), rep(latitudelim[1], n))
            #message("111")
            box <- lonlat2map(BOXx, BOXy)
            #message("/111")
            bad3 <- !is.finite(box$x) | !is.finite(box$y)
            box$x <- box$x[!bad3]
            box$y <- box$y[!bad3]
            #message("FIXME: box is NA")
            if (type == "polygon") {
                plot(x, y, type="n",
                     xlim=range(box$x, na.rm=TRUE), ylim=range(box$y, na.rm=TRUE),
                     xlab="", ylab="", asp=1, axes=FALSE, ...)
                if (is.null(border))
                    border <- "black"
                if (is.null(col))
                    col <- "white"
                if (clip) {
                    oceDebug(debug, "about to draw clipped polygon\n")
                    cl <- .Call("map_clip_xy", x, y, par("usr"))
                    polygon(cl$x, cl$y, border=border, col=col)
                } else {
                    oceDebug(debug, "about to draw unclipped polygon\n")
                    polygon(x, y, border=border, col=col)
                }
            } else {
                if (is.null(col))
                    col <- "black"
                plot(x, y, type=type, col=col,
                     xlim=range(box$x, na.rm=TRUE), ylim=range(box$y, na.rm=TRUE),
                     xlab="", ylab="", asp=1, axes=FALSE, ...)
            }
            ## points(jitter(box$x), jitter(box$y), pch=1, col='red')
        } else {
            oceDebug(debug, "neither latitudelim nor longitudelim was given\n")
            if (type == "polygon") {
                plot(x, y, type="n",
                     xlab="", ylab="", asp=1, axes=FALSE, ...)
                if (is.null(border))
                    border <- "black"
                if (is.null(col))
                    col <- "white"
                polygon(x, y, border=border, col=col)
            } else {
                if (is.null(col))
                    col <- "black"
                plot(x, y, type=type, col=col,
                     xlab="", ylab="", asp=1, axes=FALSE, ...)
            }
        }
    }
    ## Remove any island/lake that is entirely offscale.  This is not a
    ## solution to the Antarctica/stereographic problem of issue 545, because the
    ## line segment between two offscale points might intersect the box.  For
    ## this reason, it is done only when trim=TRUE.
    if (trim) {
        xy <- badFillFix2(x=x, y=y, xorig=xorig, yorig=yorig)
        x <- xy$x
        y <- xy$y
    }
    if (type != 'n') {
        ## if (!is.null(col)) {
        ##     polygon(x, y, border=border, col=col, ...)
        ## }
        if (isTopo) {
            mapContour(topo[["longitude"]], topo[["latitude"]], topo[["z"]], ...)
        }
    }
    usr <- par('usr')
    ## FIXME: meridians and zones should be added later because they can change depending
    ## FIXME: on the 'pretty' operation below.
    ##if (grid[2]) mapMeridians(seq(-90, 90, grid[2]))
    ##if (grid[1]) mapZones(seq(-180, 180, grid[1]), polarCircle=polarCircle)
    if (drawBox)
        box()
    drawGrid <- (is.logical(grid[1]) && grid[1]) || grid[1] > 0
    ## message("xrange:", paste(round(xrange, 2), collapse=" "))
    ## message("usr[1:2]:", paste(round(usr[1:2], 2), collapse=" "))
    ## message("yrange:", paste(round(yrange, 2), collapse=" "))
    ## message("usr[3:4]:", paste(round(usr[3:4], 2), collapse=" "))
    fractionOfGlobe <- usr[1] > xrange[1] || xrange[2] > usr[2] || usr[3] > yrange[1] || yrange[2] > usr[4]
    oceDebug(debug, "initially fractionOfGlobe:", fractionOfGlobe, "\n")
    ## Also turn off axes if it's nearly the whole globe
    xfrac <- diff(xrange) / (usr[2]-usr[1]) > 0.7
    yfrac  <- diff(yrange) / (usr[4]-usr[3]) > 0.7
    if (!xfrac) fractionOfGlobe <- FALSE
    if (!yfrac) fractionOfGlobe <- FALSE
    oceDebug(debug, "xfrac:", xfrac, "\n")
    oceDebug(debug, "yfrac:", yfrac, "\n")
    oceDebug(debug, "finally fractionOfGlobe:", fractionOfGlobe, "\n")
    if (axes || drawGrid) {
        ## Grid lines and axes.
        ## Find ll and ur corners of plot, if possible, for use in calculating
        ## lon and lat spans.  This will not work in all cases; e.g. for a
        ## world map in mollweide projection, the bounding points will be "in
        ## space", so we must check for this when we calculate the span.
        usr <- par('usr')
        ##xll <- usr[1]
        ##yll <- usr[3]
        ##xur <- usr[2]
        ##yur <- usr[4]

        options <- options('warn') # turn off warnings temporarily
        options(warn=-1)

        if (is.logical(grid)) {
            ## Determining a grid automatically has proved to be quite tricky,
            ## and the code near this spot has been reworked repeatedly.
            ## At one time, the code near this spot looked at par("usr")
            ## and tried to invert the corners, to get an idea of scale, and
            ## this failed because the Winkel Tripel ("wintri") projection
            ## goes into what seems to be an infinite loop when trying to do the
            ## inverse of a point that is beyond the edge of the earth
            ## disk. (I think it fails just on the disk edge, too.) When oce had
            ## the PROJ.4 code embedded within its src, this was not a problem,
            ## because I had a workaround.  This workaround has been reported
            ## to the PROJ.4 community, so I expect that sometime in the year
            ## 2015 this problem will go away.
            ##
            ## Given the above, the present code focusses near the centre of
            ## the plot region.  A region that might correspond to one tick
            ## on the axes (assuming 10 ticks per side) is inverse mapped,
            ## and the corners are used to determine a tick scale. Rather
            ## than use pretty(), the scale is determined from a list
            ## of standards (because maps should have 5deg increments, if
            ## this is good for a view, but not 4deg).
            if (limitsGiven) {
                grid <- rep(NA, 2)
                difflongitudelim <- diff(longitudelim)
                grid[1] <- if (difflongitudelim < 1) 0.1
                    else if (difflongitudelim < 5) 0.5
                    else if (difflongitudelim < 10) 1
                    else if (difflongitudelim < 45) 5
                    else if (difflongitudelim < 180) 15
                    else 15
                grid[2] <- grid[1]
                oceDebug(debug, "limits given (or inferred): set grid=", paste(grid, collapse=" "), "\n")
            } else {
                usr <- par('usr')
                x0 <- 0.5 * sum(usr[1:2])
                y0 <- 0.5 * sum(usr[3:4])
                ntick <- 8
                dx <- (usr[2] - usr[1]) / ntick
                dy <- (usr[4] - usr[3]) / ntick
                ll <- map2lonlat(x0-dx, y0-dy)
                ur <- map2lonlat(x0+dx, y0+dy)
                if (debug > 0) {
                    cat(vectorShow(ll))
                    cat(vectorShow(ur))
                }
                ls <- geodDist(ll$longitude, ll$latitude, ll$longitude, ur$latitude)
                rs <- geodDist(ur$longitude, ll$latitude, ur$longitude, ur$latitude)
                ts <- geodDist(ll$longitude, ur$latitude, ur$longitude, ur$latitude)
                bs <- geodDist(ll$longitude, ll$latitude, ur$longitude, ll$latitude)
                t <- median(c(ls, rs, ts, bs)) / 111 # tick, in degrees
                if (debug > 0)  {
                    cat(vectorShow(ls))
                    cat(vectorShow(rs))
                    cat(vectorShow(ts))
                    cat(vectorShow(ts))
                }
                oceDebug(debug, "t: ", t, "(scale between ticks, in deg)\n")
                ## message("tickEW: ", tickEW)
                ## message("tickNS: ", tickNS)
                ## message("tick: ", tick)
                if (!is.finite(t)) {
                    grid <- c(5, 5) # may be ok in many instances
                } else {
                    g <- if (t > 45) 45 else if (t > 10) 15 else if (t > 5) 10
                        else if (t > 4) 5 else if (t > 2) 1 else pretty(t)[2]
                    grid <- rep(g, 2)
                    oceDebug(debug, "grid:", grid[1], "\n")
                }
                oceDebug(debug, "limits not given (or inferred): set grid=", paste(grid, collapse=" "), "\n")
            }
        }
        if (drawGrid) {
            mapGrid(longitude=NULL, dlatitude=grid[2], polarCircle=polarCircle,
                    longitudelim=longitudelim, latitudelim=latitudelim, debug=debug-1)
            mapGrid(dlongitude=grid[1], latitude=NULL, polarCircle=polarCircle,
                    longitudelim=longitudelim, latitudelim=latitudelim, debug=debug-1)
        }
        if (axes) {
            mapAxis(side=1, longitude=.axis()$longitude, cex.axis=cex.axis, mgp=mgp, debug=debug-1)
            mapAxis(side=2, latitude=.axis()$latitude, cex.axis=cex.axis, mgp=mgp, debug=debug-1)
        }
        if (tissot)
            mapTissot(grid, col='red', debug=debug-1)
        options(warn=options$warn)
    }
    oceDebug(debug, "} # mapPlot()\n", unindent=1)
}


#' Add a Longitude and Latitude Grid to a Map
#'
#' @description
#' Plot longitude and latitude grid on an existing map.
#'
#' @param dlongitude increment in longitude, ignored if \code{longitude}
#' is supplied, but otherwise determines the longitude sequence.
#'
#' @param dlatitude increment in latitude, ignored if \code{latitude}
#' is supplied, but otherwise determines the latitude sequence.
#'
#' @param longitude vector of longitudes, or \code{NULL} to prevent drawing
#' longitude lines.
#'
#' @param latitude vector of latitudes, or \code{NULL} to prevent drawing
#' latitude lines.
#'
#' @param col colour of lines
#'
#' @param lty line type
#'
#' @param lwd line width
#'
#' @param polarCircle a number indicating the number of degrees of latitude
#' extending from the poles, within which zones are not drawn.
#'
#' @param longitudelim optional argument specifying suggested longitude limits
#' for the grid. If this is not supplied, grid lines are drawn for the
#' whole globe, which can yield excessively slow drawing speeds for
#' small-region plots. This, and \code{latitudelim}, are both set by
#' \code{\link{mapPlot}} if the arguments of the same name are passed to
#' that function.
#'
#' @param latitudelim similar to \code{longitudelim}.
#'
#' @param debug a flag that turns on debugging.  Set to 1 to get a moderate
#' amount of debugging information, or to 2 to get more.
#'
#'
#' @details
#' This is somewhat analogous to \code{\link{grid}}, except that the
#' first two arguments of the latter supply the number of lines in the grid,
#' whereas the present function has increments for the first two arguments.
#'
#' @section Plans:
#' At the moment, the function cannot determine which lines might
#' work with labels on axes, but this could perhaps be added later, making
#' this more analogous with \code{\link{grid}}.
#'
#' @examples
#' \dontrun{
#' library(oce)
#' data(coastlineWorld)
#' mapPlot(coastlineWorld, type='l', grid=FALSE,
#' longitudelim=c(-80, 10), latitudelim=c(0, 120),
#' projection="+proj=ortho")
#' mapGrid(15, 15, polarCircle=15)
#' }
#' @author Dan Kelley
#' @seealso A map must first have been created with \code{\link{mapPlot}}.
#' @family functions related to maps
mapGrid <- function(dlongitude=15, dlatitude=15, longitude, latitude,
                    col="darkgray", lty="solid", lwd=0.5*par("lwd"), polarCircle=0,
                    longitudelim, latitudelim,
                    debug=getOption("oceDebug"))
{
    if ("none" == .Projection()$type)
        stop("must create a map first, with mapPlot()\n")
    if (!missing(longitude) && is.null(longitude) && !missing(latitude) && is.null(latitude))
        return()
    if (!missing(longitudelim))
        longitudelim <- shiftLongitude(longitudelim)
    oceDebug(debug, "mapGrid(dlongitude=", dlongitude,
             ", dlatitude=", dlatitude, ", ..., polarCircle=", polarCircle,
             ", longitudelim=", if (missing(longitudelim)) "(missing)" else
                 paste("c(", paste(longitudelim, collapse=", "), ")"),
             ", latitudelim=", if (missing(latitudelim)) "(missing)" else
                 paste("c(", paste(latitudelim, collapse=", "), ")"),
             ", debug) {\n", unindent=1, sep="")
    if (missing(longitude) || !is.null(longitude))
        oceDebug(debug, "will draw longitude lines\n")
    if (missing(latitude) || !is.null(latitude))
        oceDebug(debug, "will draw latitude lines\n")
    if (!missing(longitudelim) && !missing(longitude) && !is.null(longitude)) {
        longitudelim <- shiftLongitude(longitudelim)
        oceDebug(debug, "shifted longitudelim to c(",
                 paste(longitudelim, collapse=","), ")\n")
    }
    small <- 0
    if (missing(longitude))
        longitude <- seq(-180, 180, dlongitude)
    if (missing(latitude))
        latitude <- seq(-90+small, 90-small, dlatitude)

    ## If a pole is present, we put longitude lines around the world, no matter
    ## what else is true.
    poleInView <- FALSE
    try(pole <- lonlat2map(0, 90), silent=TRUE)
    if (inherits(pole, "try-error")) {
        try(pole <- lonlat2map(0, -90), silent=TRUE)
    }
    if (!inherits(pole, "try-error")) {
        pusr <- par("usr") # don't alter existing
        poleInView <- pusr[1] <= pole$x && pole$x <= pusr[2] && pusr[3] <= pole$y && pole$y <= pusr[4]
        rm(pusr)
    }
    if (poleInView) {
        longitude <- seq(-180, 180, dlongitude)
        oceDebug(debug, "poleInView=", poleInView, ", so drawing longitude from -180 to 180\n")
    } else {
        if (!missing(longitudelim)) {
            lonMin <- longitudelim[1] - diff(longitudelim) / 2
            lonMax <- longitudelim[2] + diff(longitudelim) / 2
            oceDebug(debug, "lonMin=", lonMin, ", lonMax=", lonMax, "\n")
            if (!is.null(longitude)) {
                oceDebug(debug, "before trimming to longitudelim+: lon range ", paste(range(longitude, na.rm=TRUE), collapse=" "), "\n")
                longitude <- longitude[lonMin <= longitude & longitude <= lonMax]
                oceDebug(debug, "after: lon range ", paste(range(longitude), collapse=" "), "\n")
            }
        }
    }
    if (!missing(latitudelim)) {
        ## limit to 1.5 timex lon/lim limit range
        latMin <- latitudelim[1] - diff(latitudelim) / 2
        latMax <- latitudelim[2] + diff(latitudelim) / 2
        if (!is.null(latitude)) {
            oceDebug(debug, "before trimming to latitudelim+: lat range ", paste(range(latitude, na.rm=TRUE), collapse=" "), "\n")
            latitude <- latitude[latMin <= latitude & latitude <= latMax]
            oceDebug(debug, "after: lat range ", paste(range(latitude), collapse=" "), "\n")
        }
    }
    n <- 360                           # number of points on line
    ##xspan <- diff(par('usr')[1:2])
    ## Update the global axis information
    axisOLD <- .axis()
    .axis(list(longitude=if (!missing(longitude) && length(longitude)) longitude else axisOLD$longitude,
               latitude=if (!missing(latitude) && length(latitude)) latitude else axisOLD$latitude))
    if (length(latitude))
        oceDebug(debug, "drawing latitude line:")
    for (l in latitude) {
        ## FIXME: maybe we should use mapLines here
        if (is.finite(l)) {
            if (debug > 0) cat(l, " ")
            line <- lonlat2map(seq(-180+small, 180-small, length.out=n), rep(l, n))
            x <- line$x
            y <- line$y
            ok <- !is.na(x) & !is.na(y)
            x <- x[ok]
            if (0 == length(x)) next
            y <- y[ok]
            if (0 == length(y)) next
            ## Remove ugly horizontal lines that can occur for
            ## projections that show the edge of the earth.
            xJump <- abs(diff(x))
            if (any(is.finite(xJump))) {
                ## FIXME: the number in the next line might need adjustment.
                xJumpMedian <- median(xJump, na.rm=TRUE)
                if (!is.na(xJumpMedian)) {
                    horizontalJump <- c(FALSE, xJump > 3 * xJumpMedian)
                    horizontalJump <- horizontalJump[!is.na(horizontalJump)]
                    if (any(horizontalJump)) {
                        x[horizontalJump] <- NA
                    }
                }
                lines(x, y, lty=lty, lwd=lwd, col=col)
            }
        }
    }
    if (length(latitude))
        if (debug > 0) cat("\n")
    if (polarCircle < 0 || polarCircle > 90)
        polarCircle <- 0
    n <- 360                           # number of points on line
    ## If it seems that we are drawing longitude lines for more than 3/4
    ## of the globe, we just draw them all. This can solve odd problems to
    ## do with axis limits.
    if (270 < diff(range(longitude, na.rm=TRUE))) {
        diff <- diff(longitude)[1]
        longitude <- seq(-180, 180, diff)
    }
    for (l in longitude) {
        ## FIXME: should use mapLines here
        if (is.finite(l)) {
            line <- lonlat2map(rep(l, n), seq(-90+polarCircle+small, 90-polarCircle-small, length.out=n))
            x <- line$x
            y <- line$y
            ok <- !is.na(x) & !is.na(y)
            x <- x[ok]
            y <- y[ok]
            if (0 == length(x) || 0 == length(y)) {
                oceDebug(debug, "SKIPPING longitude graticule", l, "E\n")
            } else {
                oceDebug(debug, "longitude graticule", l, "E has ", length(x), "segments\n")
                lines(x, y, lty=lty, lwd=lwd, col=col)
            }
        }
    }
    if (length(longitude))
        if (debug > 0) cat("\n")
    oceDebug(debug, "} # mapGrid()\n", unindent=1, sep="")
}


#' Add Meridians on a Map [deprecated]
#'
#' \strong{WARNING:} This function will be removed soon; see \link{oce-deprecated}.
#' Use \code{\link{mapGrid}} instead of the present function.
#'
#' Plot meridians (lines of constant latitude) on an existing map.
#'
#' @param latitude either a logical value indicating whether to draw
#' a meridian grid, or a vector of latitudes at which to draw meridians.
#'
#' @param lty line type.
#'
#' @param lwd line width.
#'
#' @param col line colour.
#'
#' @param ... optional arguments passed to \code{\link{lines}}.
#'
#' @details
#' This function should not be used, since it will be removed soon.
#' Please use mapGrid() instead.
#'
#' @author Dan Kelley
#' @family functions that will be removed soon
mapMeridians <- function(latitude, lty='solid', lwd=0.5*par('lwd'), col='darkgray', ...)
{
    .Deprecated("mapGrid",
                msg="mapMeridians() will be removed soon; use mapGrid() instead. See ?'oce-deprecated'.")
    if ("none" == .Projection()$type)
        stop("must create a map first, with mapPlot()\n")
    warning("Use mapGrid(longitude=NULL,latitude=latitude) instead of mapMeridians(latitude)")
    if (missing(latitude))
        latitude <- TRUE
    small <- 0.001
    if (is.logical(latitude)) {
        if (!latitude)
            return()
        latitude <- seq(-90+small, 90-small, length.out=13)
    }
    ##usr <- par('usr')
    n <- 360                           # number of points on line
    for (l in latitude) {
        ## FIXME: maybe we should use mapLines here
        ## message("mapMeridian at ", l, " N")
        line <- lonlat2map(seq(-180+small, 180-small, length.out=n), rep(l, n))
        x <- line$x
        y <- line$y
        ok <- !is.na(x) & !is.na(y)
        x <- x[ok]
        if (0 == length(x))
            next
        y <- y[ok]
        if (0 == length(y))
            next
        ## FIXME: below is a kludge to avoid weird horiz lines; it
        ## FIXME: would be better to complete the polygons, so they
        ## FIXME: can be filled.  It might be smart to do this in C
        if (FALSE) {
            ## this was a bad idea, e.g. in orthographic, lines cross whole domain
            d <- c(0, sqrt(diff(x)^2 + diff(y)^2))
            d[!is.finite(d)] <- 0
            if (0 == length(d))
                next
            ##2014-01-09 dc <- as.numeric(quantile(d, 0.99, na.rm=TRUE)) # FIXME: criterion
            dc <- as.numeric(median(d, na.rm=TRUE))
            bad <- d > 3 * dc
            x[bad] <- NA                   # FIXME: add, don't replace
            y[bad] <- NA                   # FIXME: add, don't replace
        }
        ## NB. used to check for points in region but when zoomed in closely, there may be none!
        ##if (length(x) & length(y) & any(usr[1] <= x & x <= usr[2] & usr[3] <= y & y <= usr[4], na.rm=TRUE)) {
        lines(x, y, lty=lty, lwd=lwd, col=col, ...)
    }
}



#' Add a Scalebar to a Map
#'
#' Draw a scalebar on an existing map.
#'
#' @param x,y position of the scalebar.  Eventually this may be similar to
#'     the corresponding arguments in \code{\link{legend}}, but at the moment
#'     \code{y} must be \code{NULL} and \code{x} must be \code{"topleft"}.
#'
#' @param length the distance to indicate, in kilometres.  If not provided, a
#'     reasonable choice is made, based on the underlying map.
#'
#' @param lwd line width of the scalebar.
#'
#' @param col colour of the scalebar.
#'
#' @param cex character expansion factor for the scalebar text.
#'
#' @details
#' The scale is appropriate to the centre of the plot, and will become
#' increasingly inaccurate away from that spot, with the error depending on
#' the projection and the fraction of the earth that is shown.
#'
#' @examples
#' \dontrun{
#' library(oce)
#' data(coastlineWorld)
#' ## Arctic Ocean
#' par(mar=c(2.5, 2.5, 1, 1))
#' mapPlot(coastlineWorld, latitudelim=c(60, 120), longitudelim=c(-130,-50),
#'         col="lightgray", projection="+proj=stere +lat_0=90")
#' mapScalebar()
#' }
#' @author Dan Kelley
#' @seealso A map must first have been created with \code{\link{mapPlot}}.
#' @family functions related to maps
mapScalebar <- function(x, y=NULL, length,
                        lwd=1.5*par("lwd"), cex=par("cex"),
                        col="black")
{
    if ("none" == .Projection()$type)
        stop("must create a map first, with mapPlot()\n")
    if (!is.null(y))
        stop("y must be NULL in this (early) version of mapScalebar()\n")
    if (missing(x))
        x <- "topleft"
    else if (is.na(pmatch(x, c("topleft", "topright"))))
        stop("x must be \"topleft\" or \"topright\", but it is \"", x, "\"\n")
    usr <- par('usr')
    ## determine scale from centre of region
    x0 <- 0.5 * (usr[1] + usr[2])
    y0 <- 0.5 * (usr[3] + usr[4])
    dusr <- 0.01 * (usr[2] - usr[1]) # 1 percent of device width
    x1 <- x0 + dusr
    y1 <- y0
    lonlat0 <- map2lonlat(x0, y0)
    lonlat1 <- map2lonlat(x1, y1)
    dkm <- geodDist(lonlat0$longitude, lonlat0$latitude,
                      lonlat1$longitude, lonlat1$latitude)
    kmPerUsr <- dkm / dusr
    ##message("kmPerUsr: ", kmPerUsr)
    if (missing(length)) {
        # corner to corner distance
        ccd <- kmPerUsr * sqrt( (usr[2]-usr[1])^2 + (usr[4]-usr[3])^2 )
        length <- diff(pretty(c(0, ccd), n=12)[1:2])
    }
    frac <- length / kmPerUsr
    cin <- par('cin')[1]
    cinx <- xinch(cin)
    ciny <- yinch(cin)
    ## FIXME: when get more options for x, revisit next few lines
    xBar <- usr[1] + cinx / 2
    if (is.character(x) && x == "topright")
        xBar <- usr[2] - frac - 3.5 * cinx
    yBar <- usr[4] - ciny / 2
    ## Draw white-out underlay box with a border, since otherwise
    ## it's hard to see a scalebar on a complex map.
    llBox <- list(x=xBar, y=yBar - 3 * ciny)
    urBox <- list(x=xBar + frac + 3 * cinx, y=yBar)
    polygon(c(llBox$x, llBox$x, urBox$x, urBox$x),
            c(llBox$y, urBox$y, urBox$y, llBox$y),
            border="black", col='white')
    ## Draw the scalebar and text below.
    lines(xBar + cinx + c(0, frac), rep(yBar-ciny, 2), lwd=lwd, col=col, lend=2)
    lines(rep(xBar+cinx, 2), yBar - ciny + c(-ciny, ciny)/3,
          col=col, lwd=lwd)
    lines(rep(xBar++cinx+frac, 2), yBar - ciny + c(-ciny, ciny)/3,
          col=col, lwd=lwd)
    text(xBar+cinx, yBar-2.2*ciny, pos=4, adj=0, offset=0,
         sprintf("%.0f km", length), cex=cex, col=col)
}


#' Add Text to a Map
#'
#' Plot text on an existing map.
#'
#' @param longitude vector of longitudes of text to be plotted.
#'
#' @param latitude vector of latitudes of text to be plotted.
#'
#' @param labels vector of labels of text to be plotted.
#'
#' @param ... optional arguments passed to \code{\link{text}}, e.g. \code{adj},
#'     \code{pos}, etc.
#'
#' @details
#' Adds text to an existing map, by analogy to \code{\link{text}}.
#'
#' @examples
#' \dontrun{
#' library(oce)
#' data(coastlineWorld)
#' longitude <- coastlineWorld[['longitude']]
#' latitude <- coastlineWorld[['latitude']]
#' mapPlot(longitude, latitude, type='l', grid=5,
#'         longitudelim=c(-70,-50), latitudelim=c(45, 50),
#'         projection="+proj=merc")
#' lon <- -63.5744 # Halifax
#' lat <- 44.6479
#' mapPoints(lon, lat, pch=20, col="red")
#' mapText(lon, lat, "Halifax", col="red", pos=1, offset=1)
#' }
#' @author Dan Kelley
#' @seealso A map must first have been created with \code{\link{mapPlot}}.
#' @family functions related to maps
mapText <- function(longitude, latitude, labels, ...)
{
    if ("none" == .Projection()$type)
        stop("must create a map first, with mapPlot()\n")
    ok <- !is.na(longitude) & !is.na(latitude)
    longitude <- longitude[ok]
    latitude <- latitude[ok]
    labels <- labels[ok]
    if (length(longitude) > 0) {
        xy <- lonlat2map(longitude, latitude)
        text(xy$x, xy$y, labels, ...)
    }
}




#' Add Tissot Indicatrices to a Map
#'
#' Plot ellipses at grid intersection points, as a method for
#' indicating the distortion inherent in the projection [1].
#' (Each ellipse is drawn with 64 segments.)
#'
#' @param grid numeric vector of length 2, specifying the increment in
#' longitude and latitude for the grid. Indicatrices are drawn at e.g.
#' longitudes \code{seq(-180, 180, grid[1])}.
#'
#' @param scale numerical scale factor for ellipses. This is multiplied by
#' \code{min(grid)} and the result is the radius of the circle on the
#' earth, in latitude degrees.
#'
#' @param crosshairs logical value indicating whether to draw constant-latitude
#' and constant-longitude crosshairs within the ellipses.  (These are drawn
#' with 10 line segments each.) This can be helpful in cases where it is
#' not desired to use \code{\link{mapGrid}} to draw the longitude/latitude
#' grid.
#'
#' @param \dots extra arguments passed to plotting functions, e.g.
#' \code{col="red"} yields red indicatrices.
#'
#' @details
#' The purpose and interpretation are outlined in [1], but should also be
#' self-explanatory.
#'
#' @references
#' 1. Snyder, John P., 1987.  Map Projections: A Working Manual.  USGS
#' Professional Paper: 1395 (available at
#' \url{pubs.usgs.gov/pp/1395/report.pdf}).
#'
#' @examples
#' \dontrun{
#' library(oce)
#' data(coastlineWorld)
#' par(mfrow=c(1, 1), mar=c(2, 2, 1, 1))
#' p  <- "+proj=aea +lat_1=10 +lat_2=60 +lon_0=-45"
#' mapPlot(coastlineWorld, projection=p, col="gray",
#' longitudelim=c(-90,0), latitudelim=c(0, 50))
#' mapTissot(c(15, 15), col='red')
#' }
#' @author Dan Kelley
#' @seealso A map must first have been created with \code{\link{mapPlot}}.
#' @family functions related to maps
mapTissot <- function(grid=rep(15, 2), scale=0.2, crosshairs=FALSE, ...)
{
    if ("none" == .Projection()$type)
        stop("must create a map first, with mapPlot()\n")
    if (2 != length(grid) || !is.numeric(grid) || any(!is.finite(grid)))
        stop("grid must be of length 2, numeric, and finite")
    ntheta <- 64
    ncr <- 10
    theta <- seq(0, 2*pi, length.out=ntheta)
    d <- scale * min(grid, na.rm=TRUE)
    for (lon in seq(-180, 180, grid[1])) {
        for (lat in seq(-90, 90, grid[2])) {
            LAT <- lat + d*sin(theta)
            factor <- 1 / cos(lat * pi / 180)
            LON <- lon + d*cos(theta) * factor
            mapLines(LON, LAT, ...)
            if (crosshairs) {
                mapLines(rep(lon, ncr), seq(lat-d, lat+d, length.out=ncr), ...)
                mapLines(seq(lon-d*factor, lon+d*factor, length.out=ncr), rep(lat, ncr), ...)
            }
        }
    }
}


#' Add Zones to a Map [deprecated]
#'
#' \strong{WARNING:} This function will be removed soon; see \link{oce-deprecated}.
#'
#' Use \code{\link{mapGrid}} instead of the present function.
#'
#' Plot zones (lines of constant longitude) on a existing map.
#'
#' @param longitude either a logical indicating whether to draw a zonal grid,
#' or a vector of longitudes at which to draw zones.
#'
#' @param polarCircle a number indicating the number of degrees of latitude
#' extending from the poles, within which zones are not drawn.
#'
#' @param lty line type.
#'
#' @param lwd line width.
#'
#' @param col line colour.
#'
#' @param ... optional arguments passed to \code{\link{lines}}.
#'
#' @details
#' This function should not be used, since it will be removed soon.
#' Please use mapGrid() instead.
#'
#' @author Dan Kelley
#' @family functions that will be removed soon
mapZones <- function(longitude, polarCircle=0, lty='solid', lwd=0.5*par('lwd'), col='darkgray', ...)
{
    .Deprecated("mapGrid",
                msg="mapZones() will be removed soon; use mapGrid() instead. See ?'oce-deprecated'.")
    if ("none" == .Projection()$type)
        stop("must create a map first, with mapPlot()\n")
    warning("Use mapGrid(longitude=longitude,latitude=NULL) instead of mapZones(longitude)")
    if (missing(longitude))
        longitude <- TRUE
    small <- 0.001
    if (is.logical(longitude)) {
        if (!longitude)
            return()
        longitude <- seq(-180, 180, 15)
    }
    if (polarCircle < 0 || polarCircle > 90)
        polarCircle <- 0
    ##usr <- par('usr')
    n <- 360                           # number of points on line
    for (l in longitude) {
        ## FIXME: should use mapLines here
        line <- lonlat2map(rep(l, n), seq(-90+polarCircle+small, 90-polarCircle-small, length.out=n))
        x <- line$x
        y <- line$y
        ok <- !is.na(x) & !is.na(y)
        x <- x[ok]
        y <- y[ok]
        ## NB. used to check for points in region but when zoomed in closely, there may be none!
        ##if (length(x) & any(usr[1] <= x & x <= usr[2] & usr[3] <= y & y <= usr[4], na.rm=TRUE)) {
        lines(x, y, lty=lty, lwd=lwd, col=col, ...)
        ##}
    }
}


#' Add Lines to a Map
#'
#' Plot lines on an existing map
#'
#' @param longitude vector of longitudes of points to be plotted, or an
#' object from which longitude and latitude can be inferred (e.g. a coastline
#' file, or the return value from \code{\link{mapLocator}}), in which case the
#' following two arguments are ignored.
#'
#' @param latitude vector of latitudes of points to be plotted.
#'
#' @param greatCircle a logical value indicating whether to render line
#' segments as great circles.  (Ignored.)
#'
#' @param \dots optional arguments passed to \code{\link{lines}}.
#'
#' @details
#' Adds lines to an existing map, by analogy to \code{\link{lines}}.
#'
#' @examples
#' \dontrun{
#' library(oce)
#' data(coastlineWorld)
#' mapPlot(coastlineWorld, type='l',
#'         longitudelim=c(-80, 10), latitudelim=c(0, 120),
#'         projection="+proj=ortho +lon_0=-40")
#' lon <- c(-63.5744, 0.1062)             # Halifax CA to London UK
#' lat <- c(44.6479, 51.5171)
#' mapPoints(lon, lat, col='red')
#' mapLines(lon, lat, col='red')
#' }
#'
#' @author Dan Kelley
#' @seealso A map must first have been created with \code{\link{mapPlot}}.
#' @family functions related to maps
mapLines <- function(longitude, latitude, greatCircle=FALSE, ...)
{
    if ("none" == .Projection()$type)
        stop("must create a map first, with mapPlot()\n")
    if ("data" %in% slotNames(longitude) && # handle e.g. 'coastline' class
        2 == sum(c("longitude", "latitude") %in% names(longitude@data))) {
        latitude <- longitude@data$latitude
        longitude <- longitude@data$longitude
    }
    if (2 == sum(c("longitude", "latitude") %in% names(longitude))) {
        latitude <- longitude$latitude
        longitude <- longitude$longitude
    }
    if (greatCircle)
        warning("mapLines() does not yet handle argument 'greatCircle'")
    xy <- lonlat2map(longitude, latitude)
    ##n <- length(longitude)
    ok <- !is.na(xy$x) & !is.na(xy$y)
    usr <- par('usr')
    ##DX <- usr[2] - usr[1]
    if (any(usr[1] <= xy$x[ok] & xy$x[ok] <= usr[2] & usr[3] <= xy$y[ok] & xy$y[ok] <= usr[4])) {
        ## 20150421 # Remove code that attempted to delete extraneous lines ... the problem
        ## 20150421 # is that there's no good way to know which are extraneous, and length
        ## 20150421 # is not the best indicator.
        ## 20150421 if (n > 10) { # don't mess with short segments
        ## 20150421     dx <- c(0, abs(diff(xy$x, na.rm=TRUE)))
        ## 20150421     bad <- dx / DX > 0.1
        ## 20150421     if (any(bad, na.rm=TRUE)) { # FIXME: a kludge that may be problematic
        ## 20150421         xy$x[bad] <- NA
        ## 20150421     }
        ## 20150421 }
        lines(xy$x, xy$y, ...)
    }
}


#' Add Points to a Map
#'
#' Plot points on an existing map.
#'
#' @param longitude Longitudes of points to be plotted, or an object from which
#' longitude and latitude can be inferred in which case the following two
#' arguments are ignored.  This objects that are possible include those of type
#' \code{coastline}.
#'
#' @param latitude Latitudes of points to be plotted.
#'
#' @param debug A flag that turns on debugging.  Set to 1 to get a moderate amount
#' of debugging information, or to 2 to get more.
#'
#' @param ... Optional arguments passed to \code{\link{points}}.
#'
#' @details
#' Adds points to an existing map, by analogy to \code{\link{points}}.
#'
#' @seealso A map must first have been created with \code{\link{mapPlot}}.
#'
#' @examples
#' \dontrun{
#' library(oce)
#' data(coastlineWorld)
#' mapPlot(coastlineWorld, longitudelim=c(-80, 0), latitudelim=c(20, 50),
#'         col="lightgray", projection="+proj=laea +lon_0=-35")
#' data(section)
#' mapPoints(section)
#' }
#'
#' @author Dan Kelley
#' @seealso A map must first have been created with \code{\link{mapPlot}}.
#' @family functions related to maps
mapPoints <- function(longitude, latitude, debug=getOption("oceDebug"), ...)
{
    if ("none" == .Projection()$type)
        stop("must create a map first, with mapPlot()\n")
    if ("data" %in% slotNames(longitude) && # handle e.g. 'coastline' class
        2 == sum(c("longitude", "latitude") %in% names(longitude@data))) {
        latitude <- longitude@data$latitude
        longitude <- longitude@data$longitude
    }
    if (2 == sum(c("longitude", "latitude") %in% names(longitude))) {
        latitude <- longitude$latitude
        longitude <- longitude$longitude
    }
    if (inherits(longitude, "section") || inherits(longitude, "ctd")) {
        latitude <- longitude[["latitude", "byStation"]]
        longitude <- longitude[["longitude", "byStation"]]
    }
    ok <- !is.na(longitude) & !is.na(latitude)
    longitude <- longitude[ok]
    latitude <- latitude[ok]
    if (length(longitude) > 0) {
        xy <- lonlat2map(longitude, latitude)
        points(xy$x, xy$y, ...)
    }
}

#' Add Arrows to a Map
#'
#' Plot arrows on an existing map, e.g. to indicate a place location.
#' This is not well-suited for drawing direction fields, e.g. of
#' velocities; for that, see \code{\link{mapDirectionField}}.
#'
#' @details
#' Adds arrows to an existing map, by analogy to \code{\link{arrows}}.
#'
#' @param longitude0,latitude0 starting points for arrows.
#' @param longitude1,latitude1 ending points for arrows.
#' @param length length of the arrow heads, passed to \code{\link{arrows}}.
#' @param angle angle of the arrow heads, passed to \code{\link{arrows}}.
#' @param code numerical code indicating the type of arrows, passed to \code{\link{arrows}}.
#' @param col arrow colour, passed to \code{\link{arrows}}.
#' @param lty arrow line type, passed to \code{\link{arrows}}.
#' @param lwd arrow line width, passed to \code{\link{arrows}}.
#' @param ... optional arguments passed to \code{\link{arrows}}.
#'
#' @examples
#' \dontrun{
#' library(oce)
#' data(coastlineWorld)
#' mapPlot(coastlineWorld, longitudelim=c(-120, -60), latitudelim=c(30, 60),
#'         col="lightgray", projection="+proj=lcc +lon_0=-100")
#' lon <- seq(-120, -75, 15)
#' n <- length(lon)
#' lat <- 45 + rep(0, n)
#' # Draw meridional arrows in N America, from 45N to 60N.
#' mapArrows(lon, lat, lon, lat+15, length=0.05, col="blue")
#' }
#'
#' @author Dan Kelley
#' @seealso A map must first have been created with \code{\link{mapPlot}}.
#' @family functions related to maps
mapArrows <- function(longitude0, latitude0,
                      longitude1=longitude0, latitude1=latitude0,
                      length=0.25, angle=30,
                      code=2, col=par("fg"), lty=par("lty"),
                      lwd=par("lwd"), ...)
{
    if ("none" == .Projection()$type)
        stop("must create a map first, with mapPlot()\n")
    if (length(longitude0) != length(latitude0))
        stop("lengths of longitude0 and latitude0 must match but they are ", length(longitude0), " and ", length(longitude1))
    if (length(longitude1) != length(latitude1))
        stop("lengths of longitude1 and latitude1 must match but they are ", length(longitude1), " and ", length(longitude1))
    ok <- !is.na(longitude0) & !is.na(latitude0) & !is.na(longitude1) & !is.na(latitude1)
    longitude0 <- longitude0[ok]
    latitude0 <- latitude0[ok]
    longitude1 <- longitude1[ok]
    latitude1 <- latitude1[ok]
    if (length(longitude0) > 0) {
        xy0 <- lonlat2map(longitude0, latitude0)
        xy1 <- lonlat2map(longitude1, latitude1)
        arrows(xy0$x, xy0$y, xy1$x, xy1$y,
               length=length, angle=angle, code=code, col=col, lty=lty, lwd=lwd, ...)
    }
}


#' Format Geographical Position in Degrees and Minutes
#'
#' Format geographical positions to degrees, minutes, and hemispheres
#'
#' @param latlon a vector of latitudes or longitudes
#' @param isLat a boolean that indicates whether the quantity is latitude or
#' longitude
#' @param type a string indicating the type of return value (see below)
#' @param showHemi a boolean that indicates whether to indicate the hemisphere
#' @return A list containing \code{degrees}, \code{minutes}, \code{seconds},
#' and \code{hemispheres}, or a vector of strings or (broken) a vector of
#' expressions.
#' @author Dan Kelley
#' @examples
#'
#' library(oce)
#' formatPosition(10+1:10/60+2.8/3600)
#' formatPosition(10+1:10/60+2.8/3600, type="string")
formatPosition <- function(latlon, isLat=TRUE, type=c("list", "string", "expression"), showHemi=TRUE)
{
    type <- match.arg(type)
    signs <- sign(latlon)
    x <- abs(latlon)
    degrees <- floor(x)
    minutes <- floor(60 * (x - degrees))
    seconds <- 3600 * (x - degrees - minutes / 60)
    seconds <- round(seconds, 2)

    ## prevent rounding errors from giving e.g. seconds=60
    ##print(data.frame(degrees,minutes,seconds))
    secondsOverflow <- seconds == 60
    seconds <- ifelse(secondsOverflow, 0, seconds)
    minutes <- ifelse(secondsOverflow, minutes+1, minutes)
    minutesOverflow <- minutes == 60
    degrees <- ifelse(minutesOverflow, degrees+1, degrees)
    ##print(data.frame(degrees,minutes,seconds))

    noSeconds <- all(seconds == 0)
    noMinutes <- noSeconds & all(minutes == 0)
    if (showHemi) {
        hemispheres <- if (isLat) ifelse(signs>0, "N", "S") else ifelse(signs>0, "E", "W")
        hemispheres[signs==0] <- ""
    } else {
        hemispheres <- rep("", length(signs))
    }
    oceDebug(0, "noSeconds=", noSeconds, "noMinutes=", noMinutes, "\n")
    if (type == "list") {
        if (noMinutes)
            res <- list(degrees, hemispheres)
        else if (noSeconds)
            res <- list(degrees, minutes, hemispheres)
        else
            res <- list(degrees, minutes, seconds, hemispheres)
    } else if (type == "string") {
        if (noMinutes) {
            res <- sprintf("%2d%s", degrees, hemispheres) # no space, so more labels
        } else if (noSeconds) {
            res <- sprintf("%02d %02d' %s", degrees, minutes, hemispheres)
        } else {
            res <- sprintf("%02d %02d' %04.2f\" %s", degrees, minutes, seconds, hemispheres)
        }
        Encoding(res) <- "latin1"
    } else if (type == "expression") {
        n <- length(degrees)
        res <- vector("expression", n)
        for (i in 1:n) {
            if (noMinutes) {
                res[i] <- as.expression(substitute(d*degree*hemi,
                                                    list(d=degrees[i],
                                                         hemi=hemispheres[i])))
            } else if (noSeconds) {
                ##res[i] <- as.expression(substitute(d*degree*phantom(.)*m*minute*hemi,
                res[i] <- as.expression(substitute(d*degree*phantom(.)*m*minute*hemi,
                                                    list(d=degrees[i],
                                                         m=sprintf("%02d", minutes[i]),
                                                         hemi=hemispheres[i])))
            } else {
                res[i] <- as.expression(substitute(d*degree*phantom(.)*m*minute*phantom(.)*s*second*hemi,
                                                    list(d=degrees[i],
                                                         m=sprintf("%02d", minutes[i]),
                                                         s=sprintf("%02f", seconds[i]),
                                                         hemi=hemispheres[i])))
            }
        }
    }
    res
}


#' Locate Points on a Map
#'
#' Locate points on an existing map.
#'
#'
#' @param n number of points to locate; see \code{\link{locator}}.
#'
#' @param type type of connector for the points; see \code{\link{locator}}.
#'
#' @param \dots extra arguments passed to \code{\link{locator}} (and either
#'     \code{\link{mapPoints}} or \code{\link{mapLines}}, if appropriate) if
#'     \code{type} is not \code{'n'}.
#'
#' @details
#' This uses \code{\link{map2lonlat}} to infer the location in
#' geographical space; see the documentation for that function on its
#' limitations.
#'
#' @author Dan Kelley
#' @seealso A map must first have been created with \code{\link{mapPlot}}.
#' @family functions related to maps
mapLocator <- function(n=512, type='n', ...)
{
    if ("none" == .Projection()$type)
        stop("must create a map first, with mapPlot()\n")
    xy <- locator(n, type, ...)
    res <- map2lonlat(xy$x, xy$y)
    if (type == 'l')
        mapLines(res$longitude, res$latitude, ...)
    else if (type == 'p')
        mapPoints(res$longitude, res$latitude, ...)
    res
}



#' Convert X and Y to Longitude and Latitude
#'
#' Convert from x-y coordinates to longitude and latitude. This is normally called
#' internally within oce; see \sQuote{Bugs}.
#'
#' @details
#' A projection must already have been set up, by a call to \code{\link{mapPlot}}
#' or \code{\link{lonlat2map}}. It should be noted that not all projections are
#' handled well; see \sQuote{Bugs}.
#'
#' @param x vector containing the x component of points in the projected space, or
#' a list containing items named \code{x} and \code{y}, in which case the next
#' argument is ignored.
#'
#' @param y vector containing the y coordinate of points in the projected space
#' (ignored if \code{x} is a list, as described above).
#'
#' @param init vector containing the initial guesses for longitude and latitude,
#' presently ignored.
#'
#' @section Bugs:
#' \code{oce} uses \link[rgdal]{project} in the \CRANpkg{rgdal}
#' package to handle projections. Only those projections that have inverses are
#' permitted within \code{oce}, and even those can sometimes yield errors, owing
#' to limitations in \CRANpkg{rgdal}.
#'
#' @return
#' A list containing \code{longitude} and \code{latitude}, with \code{NA}
#' values indicating points that are off the globe as displayed.
#'
#' @examples
#' \dontrun{
#' library(oce)
#' ## Cape Split, in the Minas Basin of the Bay of Fundy
#' cs <- list(longitude=-64.49657, latitude=45.33462)
#' xy <- lonlat2map(cs, projection="+proj=merc")
#' map2lonlat(xy)
#' }
#'
#' @seealso \code{\link{lonlat2map}} does the inverse operation.
#'
#' @author Dan Kelley
#' @seealso A map must first have been created with \code{\link{mapPlot}}.
#' @family functions related to maps
map2lonlat <- function(x, y, init=c(0, 0))
{
    if ("none" == .Projection()$type)
        stop("must create a map first, with mapPlot()\n")
    if (is.list(x)) {
        y <- x$y
        x <- x$x
    }
    n <- length(x)
    if (n != length(y))
        stop("lengths of x and y must match but they are ", n, " and ", length(y))
    ##20150612 ## NB. if projections are set by mapPlot() or lonlat2map(), only one of the
    ##20150612 ## following two tests can be true.
    ##20150612 if ("proj4" == .Projection()$type) {
    if (requireNamespace("rgdal", quietly=TRUE)) {
        owarn <- options()$warn
        options(warn=-1)
        ## April 2016: rgdal::project started returning named quantities
        capture.output(XY <- unname(rgdal::project(cbind(x, y), proj=as.character(.Projection()$projection), inv=TRUE)))
        options(warn=owarn)
        ## See https://github.com/dankelley/oce/issues/653#issuecomment-107040093 for why I gave
        ## up on the idea of using rawTransform().
        ##> n <- length(x)
        ##> XY <- rgdal::rawTransform(projfom=as.character(.Projection()$projection), projto="+proj=longlat", n=n, x=x, y=y)
        return(list(longitude=XY[, 1], latitude=XY[, 2]))
        ## See https://github.com/dankelley/oce/issues/653#issuecomment-107040093 for why I gave
        ## up on the idea of using rawTransform().
        ##> return(list(longitude=XY[[1]], latitude=XY[[2]]))
    } else {
        stop('must install.packages("rgdal") to plot maps with projections')
    }
    ## 20150523 if (!getOption("externalProj4", FALSE)) {
    ## 20150523     ##message("doing PROJ.4 calculations within Oce, for speed and accuracy")
    ## 20150523     owarn <- options()$warn
    ## 20150523     options(warn=-1)
    ## 20150523     XY <- rgdal::project(cbind(x, y), proj=as.character(.Projection()$projection), inv=TRUE)
    ## 20150523     options(warn=owarn)
    ## 20150523     return(list(longitude=XY[,1], latitude=XY[,2]))
    ## 20150523     ##pre-rgdal XY <- .C("proj4_interface", as.character(.Projection()$projection), as.integer(FALSE),
    ## 20150523     ##pre-rgdal          as.integer(n), as.double(x), as.double(y),
    ## 20150523     ##pre-rgdal          X=double(n), Y=double(n), NAOK=TRUE)
    ## 20150523     ##pre-rgdal return(list(longitude=XY$X, latitude=XY$Y))
    ## 20150523 } else {
    ## 20150523     ##message("doing projection calculations with 'proj4' package")
    ## 20150523     if (!requireNamespace("proj4", quietly=TRUE))
    ## 20150523         stop("must install 'proj4' package to get options(externalProj4=TRUE) to work")
    ## 20150523     xy <- list(x=NA, y=NA)
    ## 20150523     ## FIXME: maybe we should do point-by-point if this yields an error
    ## 20150523     try({
    ## 20150523         xy <- proj4::project(list(x=x, y=y), proj=.Projection()$projection, inverse=TRUE)
    ## 20150523     }, silent=TRUE)
    ## 20150523     return(list(longitude=xy$x, latitude=xy$y))
    ## 20150523 }
    ## 20150612 } else if ("mapproj" == .Projection()$type) {
    ## 20150612     if (!requireNamespace("mapproj", quietly=TRUE))
    ## 20150612         stop("must install 'mapproj' package to use mapproj-style map projections")
    ## 20150612     lp <- mapproj::.Last.projection()
    ## 20150612     projection <- lp$projection
    ## 20150612     parameters <- lp$parameters
    ## 20150612     orientation <- lp$orientation
    ## 20150612     lon <- vector("numeric", n)
    ## 20150612     lat <- vector("numeric", n)
    ## 20150612     for (i in 1:n) {
    ## 20150612         xy <- c(x[i], y[i])
    ## 20150612         lon[i] <- NA
    ## 20150612         lat[i] <- NA
    ## 20150612         ##message("i:", i, ", xy[1]:", xy[1], ", xy[2]:", xy[2])
    ## 20150612         try({
    ## 20150612             error <- FALSE
    ## 20150612             ## message("init:", init[1], " ", init[2])
    ## 20150612             ## Note: using L-BFGS-B so we can limit the bounds; otherwise
    ## 20150612             ## it can select lat > 90 etc.
    ## 20150612             worstMisfit <- 0           # try to avoid errors with NA
    ## 20150612             o <- optim(init,
    ## 20150612                        function(xyTrial) {
    ## 20150612                            xyp <- mapproj::mapproject(xyTrial[1], xyTrial[2],
    ## 20150612                                                       projection=projection,
    ## 20150612                                                       parameters=parameters,
    ## 20150612                                                       orientation=orientation)
    ## 20150612                            error <<- xyp$error
    ## 20150612                            misfit <- sqrt((xyp$x-xy[1])^2+(xyp$y-xy[2])^2)
    ## 20150612                            ## message(format(xyTrial[1], digits=4), "E ",
    ## 20150612                            ##         format(xyTrial[2], digits=4), "N ",
    ## 20150612                            ##         "misfit: ", format(misfit, digits=5), ", error: ", xyp$error)
    ## 20150612                            if (error) {
    ## 20150612                                ## message("got error so returning ", worstMisfit)
    ## 20150612                                return(worstMisfit)
    ## 20150612                            } else {
    ## 20150612                                worstMisfit <<- max(misfit, worstMisfit, na.rm=TRUE)
    ## 20150612                                ## message("no error; set worstMisfit ", worstMisfit)
    ## 20150612                                return(misfit)
    ## 20150612                            }
    ## 20150612                        }, method="L-BFGS-B", lower=c(-180, -89.9999), upper=c(180, 89.9999))
    ## 20150612             if (o$convergence == 0 && !error) {
    ## 20150612                 lonlat <- o$par
    ## 20150612                 lon[i] <- lonlat[1]
    ## 20150612                 lat[i] <- lonlat[2]
    ## 20150612             }
    ## 20150612             ## str(o)
    ## 20150612         }, silent=TRUE)
    ## 20150612     }
    ## 20150612     ##message("map2lonlat returning lon=", lon, " lat=", lat)
    ## 20150612     return(list(longitude=lon, latitude=lat))
    ## 20150612 } else {
    ## 20150612     stop("unknown projection software type '", .Projection()$type, "'")
    ## 20150612 }
}



#' Add a Polygon to a Map
#'
#' Plot a polygon on an existing map.
#'
#' @param longitude longitudes of points to be plotted, or an object from
#' which longitude and latitude can be inferred (e.g. a coastline file, or
#' the return value from \code{\link{mapLocator}}), in which case the
#' following two arguments are ignored.
#'
#' @param latitude latitudes of points to be plotted.
#'
#' @param density as for \code{\link{polygon}}.
#'
#' @param angle as for \code{\link{polygon}}.
#'
#' @param border as for \code{\link{polygon}}.
#'
#' @param col as for \code{\link{polygon}}.
#'
#' @param lty as for \code{\link{polygon}}.
#'
#' @param ... as for \code{\link{polygon}}.
#'
#' @param fillOddEven as for \code{\link{polygon}}.
#'
#'
#' @details
#' Adds a polygon to an existing map, by analogy to
#' \code{\link{polygon}}.  Used by \code{\link{mapImage}}.
#'
#' @author Dan Kelley
#' @seealso A map must first have been created with \code{\link{mapPlot}}.
#' @family functions related to maps
mapPolygon <- function(longitude, latitude, density=NULL, angle=45,
                       border=NULL, col=NA, lty=par('lty'), ..., fillOddEven=FALSE)
{
    if ("none" == .Projection()$type)
        stop("must create a map first, with mapPlot()\n")
    if ("data" %in% slotNames(longitude) && # handle e.g. 'coastline' class
        2 == sum(c("longitude", "latitude") %in% names(longitude@data))) {
        latitude <- longitude@data$latitude
        longitude <- longitude@data$longitude
    }
    if (2 == sum(c("longitude", "latitude") %in% names(longitude))) {
        latitude <- longitude$latitude
        longitude <- longitude$longitude
    }
    n <- length(longitude)
    if (n > 0) {
        xy <- lonlat2map(longitude, latitude)
        x <- xy$x
        y <- xy$y
        xorig <- xy$x
        yorig <- xy$y
        ## 1181 necessitated a change in badFillFix1()
        xy <- badFillFix1(x=x, y=y, latitude=latitude, projection="")
        xy <- badFillFix2(x=xy$x, y=xy$y, xorig=xorig, yorig=yorig)
        x <- xy$x
        y <- xy$y
        polygon(x, y, density=density, angle=angle, border=border, col=col, lty=lty, ..., fillOddEven=fillOddEven)
    }
}


#' Add an Image to a Map
#'
#' Plot an image on an existing map.
#'
#' @param longitude vector of longitudes corresponding to \code{z} matrix.
#'
#' @param latitude vector of latitudes corresponding to \code{z} matrix.
#'
#' @param z matrix to be represented as an image.
#'
#' @param zlim limit for z (colour).
#'
#' @param zclip A logical value, \code{TRUE} indicating that out-of-range
#' \code{z} values should be painted with \code{missingColor} and \code{FALSE}
#' indicating that these values should be painted with the nearest
#' in-range colour.  If \code{zlim} is given then its min and max set the
#' range.  If \code{zlim} is not given but \code{breaks} is given, then
#' the min and max of \code{breaks} sets the range used for z.  If neither
#' \code{zlim} nor \code{breaks} is given, clipping is not done, i.e. the
#' action is as if \code{zclip} were \code{FALSE}.
#'
#' @param breaks The z values for breaks in the colour scheme.  If this is of
#' length 1, the value indicates the desired number of breaks, which is
#' supplied to \code{\link{pretty}}, in determining clean break points.
#'
#' @param col Either a vector of colours corresponding to the breaks, of length
#' 1 plus the number of breaks, or a function specifying colours,
#' e.g. \code{\link{oce.colorsJet}} for a rainbow.
#'
#' @param colormap optional colormap, as created by \code{\link{colormap}}.
#' If a \code{colormap} is provided, then its properties takes precedence
#' over \code{breaks}, \code{col}, \code{missingColor}, and \code{zclip}
#' specified to \code{mapImage}.
#'
#' @param border Colour used for borders of patches (passed to
#' \code{\link{polygon}}); the default \code{NA} means no border.
#'
#' @param lwd line width, used if borders are drawn.
#'
#' @param lty line type, used if borders are drawn.
#'
#' @param missingColor a colour to be used to indicate missing data, or
#' \code{NA} to skip the drawing of such regions (which will retain
#' whatever material has already been drawn at the regions).
#'
#' @param filledContour either a logical value indicating whether to use
#' filled contours to plot the image, or a numerical value indicating the
#' resampling rate to be used in interpolating from lon-lat coordinates to
#' x-y coordinates.  See \dQuote{Details} for how this interacts with
#' \code{gridder}.
#'
#' @param gridder Name of gridding function used if \code{filledContour} is
#' \code{TRUE}.  This can be either \code{"binMean2D"} to select
#' \code{\link{binMean2D}} or \code{"interp"} for
#' \code{\link[akima]{interp}}. If not provided, then a selection is made
#' automatically, with \code{\link{binMean2D}} being used if there are
#' more than 10,000 data points in the present graphical view. This
#' \code{"binMean2D"} method is much faster than \code{"interp"}.
#'
#' @param debug A flag that turns on debugging.  Set to 1 to get a
#' moderate amount of debugging information, or to 2 to get more.
#'
#' @details
#' Adds an image to an existing map, by analogy to \code{\link{image}}.
#'
#' The data are on a regular grid in lon-lat space, but not in the projected
#' x-y space.  This means that \code{\link{image}} cannot be used.  Instead,
#' there are two approaches, depending on the value of \code{filledContour}.
#'
#' If \code{filledContour} is \code{FALSE}, the image ``pixels'' are with
#' \code{\link{polygon}}, which can be prohibitively slow for fine grids.
#' However, if \code{filledContour} is \code{TRUE} or a numerical value, then the
#' ``pixels'' are remapped into a regular grid and then displayed with
#' \code{\link{.filled.contour}}.  The remapping starts by converting the
#' regular lon-lat grid to an irregular x-y grid using
#' \code{\link{lonlat2map}}.  This irregular grid is then interpolated onto a
#' regular x-y grid  with \code{\link{binMean2D}} or with
#' \code{\link[akima]{interp}} from the \code{akima} package, as determined by
#' the \code{gridder} argument.   If \code{filledContour} is \code{TRUE}, the
#' dimensions of the regular x-y grid is the same as that of the original
#' lon-lat grid; otherwise, the number of rows and columns are multiplied by
#' the numerical value of \code{filledContour}, e.g. the value 2 means to make
#' the grid twice as fine.
#'
#' Filling contours can produce aesthetically-pleasing results, but the method
#' involves interpolation, so the data are not represented exactly and
#' analysts are advised to compare the results from the two methods (and
#' perhaps various grid refinement values) to guard against misinterpretation.
#'
#' If a \code{\link{png}} device is to be used, it is advised to supply
#' arguments \code{type="cairo"} and \code{antialias="none"}; see [1].
#'
#' @references
#' 1. \url{http://codedocean.wordpress.com/2014/02/03/anti-aliasing-and-image-plots/}
#'
#' @examples
#' \dontrun{
#' library(oce)
#' data(coastlineWorld)
#' data(topoWorld)
#'
#' par(mfrow=c(2, 1), mar=c(2, 2, 1, 1))
#' lonlim <- c(-70, -50)
#' latlim <- c(40, 50)
#' topo <- decimate(topoWorld, by=2) # coarse to illustrate filled contours
#' topo <- subset(topo, latlim[1] < latitude & latitude < latlim[2])
#' topo <- subset(topo, lonlim[1] < longitude & longitude < lonlim[2])
#' mapPlot(coastlineWorld, type='l',
#'         longitudelim=lonlim, latitudelim=latlim,
#'         projection="+proj=lcc +lat_1=40 +lat_2=50 +lon_0=-60")
#' breaks <- seq(-5000, 1000, 500)
#' mapImage(topo, col=oce.colorsGebco, breaks=breaks)
#' mapLines(coastlineWorld)
#' box()
#' mapPlot(coastlineWorld, type='l',
#'         longitudelim=lonlim, latitudelim=latlim,
#'         projection="+proj=lcc +lat_1=40 +lat_2=50 +lon_0=-60")
#' mapImage(topo, filledContour=TRUE, col=oce.colorsGebco, breaks=breaks)
#' box()
#' mapLines(coastlineWorld)
#'
#' ## Northern polar region, with colour-coded bathymetry
#' par(mfrow=c(1,1))
#' drawPalette(c(-5000, 0), zlim=c(-5000, 0), col=oce.colorsJet)
#' mapPlot(coastlineWorld, projection="+proj=stere +lat_0=90",
#'         longitudelim=c(-180,180), latitudelim=c(60,120))
#' mapImage(topoWorld, zlim=c(-5000, 0), col=oce.colorsJet)
#' mapLines(coastlineWorld[['longitude']], coastlineWorld[['latitude']])
#'
#' # Levitus SST
#' par(mfrow=c(1,1))
#' data(levitus, package='ocedata')
#' lon <- levitus$longitude
#' lat <- levitus$latitude
#' SST <- levitus$SST
#' par(mar=rep(1, 4))
#' Tlim <- c(-2, 30)
#' drawPalette(Tlim, col=oce.colorsJet)
#' mapPlot(coastlineWorld, projection="+proj=moll", grid=FALSE)
#' mapImage(lon, lat, SST, col=oce.colorsJet, zlim=Tlim)
#' mapPolygon(coastlineWorld, col='gray')
#' }
#'
#' @author Dan Kelley
#' @seealso A map must first have been created with \code{\link{mapPlot}}.
#' @family functions related to maps
mapImage <- function(longitude, latitude, z, zlim, zclip=FALSE,
                     breaks, col, colormap, border=NA,
                     lwd=par("lwd"), lty=par("lty"), missingColor=NA,
                     filledContour=FALSE, gridder="binMean2D",
                     debug=getOption("oceDebug"))
{
    if ("none" == .Projection()$type)
        stop("must create a map first, with mapPlot()\n")
    breaksGiven <- !missing(breaks)
    zlimGiven <- !missing(zlim)
    colGiven <- !missing(col)
    oceDebug(debug, "mapImage(..., ",
             " missingColor='", missingColor, "', ",
             " filledContour=", filledContour, ", ",
             ", ...) {\n", sep="", unindent=1)

    if ("data" %in% slotNames(longitude)) {
        if (3 == sum(c("longitude", "latitude", "z") %in% names(longitude@data))) {
            ## e.g. a topo object
            z <- longitude@data$z
            latitude <- longitude@data$latitude
            longitude <- longitude@data$longitude # destroys container
        }
    } else {
        names <- names(longitude)
        if ("x" %in% names && "y" %in% names && "z" %in% names) {
            z <- longitude$z
            latitude <- longitude$y
            longitude <- longitude$x   # destroys container
        }
    }
    if (!is.matrix(z))
        stop("z must be a matrix")
    breaksGiven <- !missing(breaks)
    if (!missing(colormap)) {
        ## takes precedence over breaks and col
        breaks <- colormap$breaks
        breaksGiven <- TRUE
        col <- colormap$col
        missingColor <- colormap$missingColor
        zclip <- colormap$zclip
        colGiven <- TRUE
    }
    ## 20140816 (issues 517 and 522) START
    ## 20140816 The next few blocks (down to the 'ni <-' line) used to be the 'else'
    ## 20140816 of the above if block, but that just seemed to invite case-specific
    ## 20140816 errors. The new approach is to unroll the code to reduce the
    ## 20140816 number of cases.
    if (!breaksGiven) {
        small <- .Machine$double.eps
        zrange <- range(z, na.rm=TRUE)
        if (missing(zlim)) {
            if (missing(col)) {
                breaks <- pretty(zrange+small*c(-1, 1), n=10)
                ## FIXME: the extension of the breaks is to try to avoid missing endpoints
                if (breaks[1] < zrange[1])
                    breaks[1] <- zrange[1] * (1 - small)
                if (breaks[length(breaks)] > zrange[2])
                    breaks[length(breaks)] <- zrange[2] * (1 + small)
            } else {
                breaks <- seq(zrange[1]-small, zrange[2]+small,
                              length.out=if (is.function(col)) 128 else 1+length(col))
            }
            breaksOrig <- breaks       # nolint (variable not used)
        } else {
            if (!colGiven) {
                oceDebug(debug, "zlim provided, but not breaks or col\n")
                breaks <- c(zlim[1], pretty(zlim, n=128), zlim[2])
            } else {
                oceDebug(debug, "zlim and col provided, but not breaks\n")
                breaks <- seq(zlim[1], zlim[2], length.out=if (is.function(col)) 128 else 1+length(col))
            }
            breaksOrig <- breaks
            breaks[1] <- min(zrange[1], breaks[1])
            breaks[length(breaks)] <- max(breaks[length(breaks)], zrange[2])
        }
    } else {
        breaksOrig <- breaks
        if (1 == length(breaks)) {
            oceDebug(debug, "only 1 break given, so taking that as number of breaks\n")
            breaks <- pretty(z, n=breaks)
        }
    }
    if (missing(col)) {
        col <- oce.colorsPalette(n=length(breaks)-1)
        oceDebug(debug, "using default col\n")
    }
    if (is.function(col)) {
        col <- col(n=length(breaks)-1)
        oceDebug(debug, "col is a function\n")
    }
    oceDebug(debug, "zclip: ", zclip, "\n")
    oceDebug(debug, "breaks: ", paste(breaks, collapse=" "), "\n")
    oceDebug(debug, "col: ", paste(col, collapse=" "), "\n")
    ## 20140816 END
    ni <- dim(z)[1]
    nj <- dim(z)[2]
    ##dlongitude <- longitude[2] - longitude[1] # FIXME: incorrect for irregular grids
    ##dlatitude <- latitude[2] - latitude[1]
    zmin <- min(z, na.rm=TRUE)
    zmax <- max(z, na.rm=TRUE)
    zrange <- zmax - zmin
    ##usr <- par('usr')
    ##xmin <- usr[1]
    ##xmax <- usr[2]
    ##ymin <- usr[3]
    ##ymax <- usr[4]
    ##allowedSpan <- (xmax - xmin) / 5   # KLUDGE: avoid lines crossing whole domain
    small <- .Machine$double.eps
    ## 20140802/issue516: next if block (clipping) rewritten
    if (zclip) {
        oceDebug(debug, "using missingColor for out-of-range values\n")
        if (zlimGiven) {
            z[z < zlim[1]] <- NA
            z[z > zlim[2]] <- NA
        }
    } else {
        if (zlimGiven) {
            oceDebug(debug, "using zlim colours for out-of-range values\n")
            zlimMin <- min(zlim, na.rm=TRUE)
            zlimMax <- max(zlim, na.rm=TRUE)
            z[z <= zlimMin] <- zlimMin * (1 + sign(zlimMin) * small)
            z[z >= zlimMax] <- zlimMax * (1 - sign(zlimMax) * small)
        } else if (breaksGiven) {
            oceDebug(debug, "extenging breaks range since no zlim given\n")
            breaksMin <- min(breaks, na.rm=TRUE)
            breaksMax <- max(breaks, na.rm=TRUE)
            z[z <= breaksMin] <- breaksMin * (1 + sign(breaksMin) * small)
            z[z >= breaksMax] <- breaksMax * (1 - sign(breaksMax) * small)
        } else {
            oceDebug(debug, "not clipping AND NEITHER zlim nor breaks suppled\n")
        }
    }
    ## Construct polygons centred on the specified longitudes and latitudes.  Each
    ## polygon has 5 points, four to trace the boundary and a fifth that is (NA,NA),
    ## to signal the end of the polygon.  The z values (and hence the colours)
    ## map one per polygon.
    poly <- .Call("map_assemble_polygons", longitude, latitude, z,
                  NAOK=TRUE, PACKAGE="oce")
    ## The docs on mapproject say it needs -ve longitude for degW, but it works ok without that
    ##if (max(poly$longitude, na.rm=TRUE) > 180) {
    ##    warning("shifting longitude")
    ##    poly$longitude <- ifelse(poly$longitude > 180, poly$longitude - 360, poly$longitude)
    ##}

    xy <- lonlat2map(poly$longitude, poly$latitude)
    ## issue #638 - kludge to get data into same longitue scheme as axes
    usr12 <- par("usr")[1:2]
    xrange <- range(xy$x, na.rm=TRUE)
    if (xrange[1] > usr12[2])
        xy$x <- xy$x - 360

    ## map_check_polygons tries to fix up longitude cut-point problem, which
    ## otherwise leads to lines crossing the graph horizontally because the
    ## x value can sometimes alternate from one end of the domain to the other.
    ## Z <- matrix(z)
    Z <- as.vector(z)
    r <- .Call("map_check_polygons", xy$x, xy$y, poly$z,
               diff(par('usr'))[1:2]/5, par('usr'),
               NAOK=TRUE, PACKAGE="oce")
    breaksMin <- min(breaks, na.rm=TRUE)
    breaksMax <- max(breaks, na.rm=TRUE)
    if (filledContour) {
        oceDebug(debug, "using filled contours\n")
        zz <- Z # as.vector(z)
        g <- expand.grid(longitude, latitude)
        longitudeGrid <- g[, 1]
        latitudeGrid <- g[, 2]
        ##rx <- range(xy$x[is.finite(xy$x)], na.rm=TRUE)
        ##ry <- range(xy$y[is.finite(xy$y)], na.rm=TRUE)
        ##f <- if (is.logical(filledContour)) 1 else as.integer(round(filledContour))
        ## FIXME: I'm not sure this will work well generally; I'm setting NN to
        ## FIXME: get about 5 points per grid cell.
        ## N is number of points in view
        N <- sum(par('usr')[1]<=xy$x & xy$x<=par('usr')[2] &
                 par('usr')[3]<=xy$y & xy$y<=par('usr')[4], na.rm=TRUE)
        NN <- sqrt(N / 10)
        xg <- seq(par('usr')[1], par('usr')[2], length.out=NN)
        yg <- seq(par('usr')[3], par('usr')[4], length.out=NN)
        xy <- lonlat2map(longitudeGrid, latitudeGrid)
        good <- is.finite(zz) & is.finite(xy$x) & is.finite(xy$y)
        if (!zclip) {
            ##message("# good: ", sum(good), " orig")
            zz[zz < breaksMin] <- breaksMin
            zz[zz > breaksMax] <- breaksMax
        }
        xx <- xy$x[good]
        yy <- xy$y[good]
        zz <- zz[good]
        xtrim <- par('usr')[1:2]
        ytrim <- par('usr')[3:4]
        inFrame <- xtrim[1] <= xx & xx <= xtrim[2] & ytrim[1] <= yy & yy <= ytrim[2]
        oceDebug(debug, "before trimming, length(xx): ", length(xx), "\n")
        xx <- xx[inFrame]
        yy <- yy[inFrame]
        zz <- zz[inFrame]
        oceDebug(debug, "after trimming, length(xx): ", length(xx), "\n")
        ## chop to points within plot area
        if (gridder== "akima") {
            oceDebug(debug, "using akima::interp()\n")
            if (requireNamespace("akima", quietly=TRUE)) {
                i <- akima::interp(x=xx, y=yy, z=zz, xo=xg, yo=yg)
            } else {
                stop("must install.packages(\"akima\") for mapImage() with filledContour and gridder='akima'")
            }
        } else {
            oceDebug(debug, "using binMean2D()\n")
            binned <- binMean2D(xx, yy, zz, xg, yg, fill=TRUE)
            i <- list(x=binned$xmids, y=binned$ymids, z=binned$result)
        }
        if (any(is.finite(i$z))) {
            ## issue726: add a tiny bit to breaks, to mimic filledContour=FALSE
            small <- .Machine$double.eps
            .filled.contour(i$x, i$y, i$z, levels=breaks+small, col=col)
        } else {
            warning("no valid z")
        }
    } else {
        oceDebug(debug, "using polygons, as opposed to filled contours\n")
        colFirst <- col[1]
        colLast <- tail(col, 1)
        ## if (debug > 10) { ## FIXME (issue 522): retain this test code until 2014-oct
        ##     message("breaksMin: ", breaksMin)
        ##     message("breaksMax: ", breaksMax)
        ##     message("Z:")
        ##     print(Z)
        ## }
        colorLookup <- function (ij) {
            zval <- Z[ij]
            if (is.na(zval))
                return(missingColor)   # whether clipping or not
            if (zval < breaksMin)
                return(if (zclip) missingColor else colFirst)
            if (zval > breaksMax)
                return(if (zclip) missingColor else colLast)
            ## IMPORTANT: whether to write 'breaks' or 'breaks+small' below
            ## IMPORTANT: is at the heart of several issues, including
            ## IMPORTANT: issues 522, 655 and possibly 726.
            ## issue522: this was w <- which(zval <= breaks)[1]
            ## issue655: this was w <- which(zval <= breaks)[1]
            ## sometime later: w <- which(zval < breaks + 1*small)[1]
            w <- which(zval <= breaks)[1]
            if (!is.na(w) && w > 1)
                return(col[-1 + w])
            else
                return(missingColor)
        }
        ## mapImage(topoWorld) profiling
        ## Note that 1/3 of the time is spent here, the rest in polygon().
        ## Profile times in seconds, as below. (is 0.1 to 0.2s meaningful?)
        ## Caution: profile times depend on Rstudio window size etc, so
        ## be careful in testing! The values below were from a particular
        ## window and panel size but results were 1s different when I 
        ## resized.
        ##   with sapply: 3.480 3.640 3.520
        ##   with loop:   3.260 3.440 3.430
        method <- options()$mapPolygonMethod
        if (0 == length(method))
            method <- 3 # method tested in issue 1284
        if (method==1) {
            colPolygon <- sapply(1:(ni*nj), colorLookup)
        } else if (method==2) {
            colPolygon <- character(ni*nj)
            for (ij in 1:(ni*nj)) {
                zval <- Z[ij]
                if (!is.finite(zval)) {
                    colPolygon[ij] <- missingColor   # whether clipping or not
                } else if (zval < breaksMin) {
                    colPolygon[ij] <- if (zclip) missingColor else colFirst
                } else if (zval > breaksMax) {
                    colPolygon[ij] <- if (zclip) missingColor else colLast
                } else {
                    w <- which(zval <= breaks)[1]
                    colPolygon[ij] <- if (!is.na(w) && w > 1) col[-1 + w] else missingColor
                }
            }
        } else if (method == 3) {
            colPolygon <- rep(missingColor, ni*nj)
            breaks <- sort(breaks)
            ii <- findInterval(Z, breaks, left.open=TRUE)
            ##colPolygon <- col[-1 + ii]
            colPolygon <- col[ii]
            colPolygon[!is.finite(Z)] <- missingColor
            colPolygon[Z < min(breaks)] <- if (zclip) missingColor else colFirst
            colPolygon[Z > max(breaks)] <- if (zclip) missingColor else colLast
        } else {
            stop("unknown options(mapPolygonMethod)")
        }
        polygon(xy$x[r$okPoint & !r$clippedPoint], xy$y[r$okPoint & !r$clippedPoint],
                col=colPolygon[r$okPolygon & !r$clippedPolygon],
                border=colPolygon[r$okPolygon & !r$clippedPolygon],
                lwd=lwd, lty=lty, fillOddEven=FALSE)
    }
    oceDebug(debug, "} # mapImage()\n", unindent=1)
    invisible()
}



#' Convert Longitude and Latitude to UTM
#'
#' @param longitude decimal longitude.  May also be a list containing items
#' named \code{longitude} and \code{latitude}, in which case the indicated
#' values are used, and next argument is ignored.
#' @param latitude decimal latitude (ignored if \code{longitude} is a list
#' containing both coordinates)
#' @param zone optional indication of UTM zone.  Normally this is inferred from
#' the longitude, but specifying it can be helpful in dealing with Landsat
#' images, which may cross zones and which therefore are described by a single
#' zone.
#' @param km logical value indicating whether \code{easting} and
#' \code{northing} are in kilometers or meters.
#' @return A list containing \code{easting}, \code{northing}, \code{zone} and
#' \code{hemisphere}.
#' @author Dan Kelley
#' @seealso \code{\link{utm2lonlat}} does the inverse operation.  For general
#' projections and their inverses, use \code{\link{lonlat2map}} and
#' \code{\link{map2lonlat}}.
#' @references
#' \url{http://en.wikipedia.org/wiki/Universal_Transverse_Mercator_coordinate_system},
#' downloaded May 31, 2014.
#' @examples
#'
#' library(oce)
#' ## Cape Split, in the Minas Basin of the Bay of Fundy
#' lonlat2utm(-64.496567, 45.334626)
#'
#' @family functions related to maps
lonlat2utm <- function(longitude, latitude, zone, km=FALSE)
{
    names <- names(longitude)
    if (!is.null(names)) {
        if ("zone" %in% names)
            zone <- longitude$zone
        if ("longitude" %in% names && "latitude" %in% names) {
            latitude <- longitude$latitude
            longitude <- longitude$longitude
        }
    }
    if (missing(latitude))
        stop("latitude is missing")
    ## Code from [wikipedia](http://en.wikipedia.org/wiki/Universal_Transverse_Mercator_coordinate_system)
    longitude <- ifelse(longitude < 0, longitude+360, longitude)
    rpd <- atan2(1, 1) / 45
    lambda <- longitude * rpd
    phi <- latitude * rpd
    a <- 6378.137                          # earth radius in WSG84 (in km for these formulae)
    f <- 1 / 298.257223563                 # flatening
    n <- f / (2 - f)
    A <- (a / (1 + n)) * (1 + n^2/4 + n^4/64)
    t <- sinh(atanh(sin(phi)) - (2*sqrt(n)) / (1+n) * atanh( (2*sqrt(n)) / (1+n) * sin(phi)) )
    if (missing(zone)) {
        zone <- floor( (180 + longitude) / 6 ) # FIXME: this works for zone but not positive its ok
        zone <- floor( (longitude / 6) + 31 )
        zone <- ifelse(zone > 60, zone-60, zone)
        ## message("zone not given; inferred to be ", zone)
    }
    lambda0 <- rpd * (zone * 6 - 183)
    xiprime <- atan(t / cos(lambda - lambda0))
    etaprime <- atanh(sin(lambda - lambda0) / sqrt(1 + t^2))
    alpha1 <- (1/2)*n - (2/3)*n^2 + (5/16)*n^3
    alpha2 <- (13/48)*n^2 - (3/5)*n^3
    alpha3 <- (61/240)*n^3
    ## sigma and tau needed only if calculating k and gamma, which we are not.
    ## sigma <- 1 + 2*(  alpha1*cos(2*xiprime)*cosh(2*etaprime) +
    ##                 2*alpha2*cos(4*xiprime)*cosh(4*etaprime) +
    ##                 3*alpha3*cos(6*xiprime)*cosh(6*etaprime))
    ## tau <-       2*(  alpha1*sin(2*xiprime)*sinh(2*etaprime) +
    ##                 2*alpha2*sin(4*xiprime)*sinh(4*etaprime) +
    ##                 3*alpha3*sin(6*xiprime)*sinh(6*etaprime))
    k0 <- 0.9996
    E0 <- 500                              # km
    E <- E0 + k0 * A * (etaprime + (alpha1*cos(2*xiprime)*sinh(2*etaprime)+
                                    alpha2*cos(4*xiprime)*sinh(4*etaprime)+
                                    alpha3*cos(6*xiprime)*sinh(6*etaprime)))
    N0 <- ifelse(latitude>0, 0, 10000)
    N0 <- 0                            # to obey Landsat-8 convention
    N <- N0 + k0 * A * (xiprime  + (alpha1*sin(2*xiprime)*cosh(2*etaprime)+
                                    alpha2*sin(4*xiprime)*cosh(4*etaprime)+
                                    alpha3*sin(6*xiprime)*cosh(6*etaprime)))
    easting <- if (km) E else 1000 * E
    northing <- if (km) N else 1000 * N
    list(easting=easting, northing=northing, zone=zone,
         hemisphere=ifelse(latitude>0, "N", "S"))
}

#' Convert UTM to Longitude and Latitude
#'
#' @param easting easting coordinate (in km or m, depending on value of
#' \code{km}).  Alternatively, a list containing items named \code{easting},
#' \code{northing}, and \code{zone}, in which case these are taken from the
#' list and the arguments named \code{northing}, \code{zone} and are ignored.
#' @param northing northing coordinate (in km or m, depending on value of
#' \code{km}).
#' @param zone UTM zone
#' @param hemisphere indication of hemisphere; \code{"N"} for North, anything
#' else for South.
#' @param km logical value indicating whether \code{easting} and
#' \code{northing} are in kilometers or meters.
#' @return A list containing \code{longitude} and \code{latitude}.
#' @author Dan Kelley
#' @seealso \code{\link{lonlat2utm}} does the inverse operation.  For general
#' projections and their inverses, use \code{\link{lonlat2map}} and
#' \code{\link{map2lonlat}}.
#' @references
#' \url{http://en.wikipedia.org/wiki/Universal_Transverse_Mercator_coordinate_system},
#' downloaded May 31, 2014.
#' @examples
#'
#' library(oce)
#' ## Cape Split, in the Minas Basin of the Bay of Fundy
#' utm2lonlat(852863, 5029997, 19)
#'
#' @family functions related to maps
utm2lonlat <- function(easting, northing, zone=1, hemisphere="N", km=FALSE)
{
    names <- names(easting)
    if (!is.null(names)) {
        if ("easting" %in% names && "northing" %in% names && "zone" %in% names) {
            zone <- easting$zone
            northing <- easting$northing
            easting <- easting$easting
        }
    }
    ## Code from [wikipedia](http://en.wikipedia.org/wiki/Universal_Transverse_Mercator_coordinate_system)
    a <- 6378.137                          # earth radius in WSG84 (in km for these formulae)
    f <- 1 / 298.257223563                 # flatening
    n <- f / (2 - f)
    A <- (a / (1 + n)) * (1 + n^2/4 + n^4/64)
    beta1 <- (1/2)*n - (2/3)*n^2 + (37/96)*n^3
    beta2 <- (1/48)*n^2 + (1/15)*n^3
    beta3 <- (17/480)*n^3
    delta1 <- 2*n - (2/3)*n^2 - 2*n^3
    delta2 <- (7/3)*n^2 - (8/5)*n^3
    delta3 <- (56/15)*n^3
    if (!km) {
        northing <- northing / 1000
        easting <- easting / 1000
    }
    N0 <- if (hemisphere=="N") 0 else 10000
    k0 <- 0.9996
    E0 <- 500                              # km
    xi <- (northing - N0) / (k0 * A)
    eta <- (easting - E0) / (k0 * A)
    xiprime <-   xi -   (beta1*sin(2*xi)*cosh(2*eta) +  beta2*sin(4*xi)*cosh(4*eta) +  beta3*sin(6*xi)*cosh(6*eta))
    etaprime <- eta -   (beta1*cos(2*xi)*sinh(2*eta) +  beta2*cos(4*xi)*sinh(4*eta) +  beta3*cos(6*xi)*sinh(6*eta))
    ## sigmaprime and tauprime not needed in present calculation
    ##sigmaprime <- 1 - 2*(beta1*cos(2*xi)*cosh(2*eta) +2*beta2*cos(4*xi)*cosh(4*eta) +3*beta3*cos(6*xi)*cosh(6*eta))
    ##tauprime <-       2*(beta1*sin(2*xi)*sinh(2*eta) +2*beta2*sin(4*xi)*sinh(4*eta) +3*beta3*sin(6*xi)*sinh(6*eta))
    chi <- asin(sin(xiprime)/cosh(etaprime)) # Q: in deg or radian?
    phi <- chi + (delta1*sin(2*chi) + delta2*sin(4*chi) + delta3*sin(6*chi))
    latitude <- 45 * phi / atan2(1, 1)
    lambda0 <- zone * 6 - 183
    longitude <- lambda0 + 45/atan2(1, 1)*atan(sinh(etaprime) / cos(xiprime))
    list(longitude=longitude, latitude=latitude)
}

## This list of known projections includes only those with inverses. To create the list
## first I did
##      proj -lP > A
## in the commandline, and then I hand-edited out the entries that said that no
## inverse was available. Then I used grep and sed and an editor action to create the
## list.  The reason for demanding an inverse is to avoid errors that arise
## otherwise.  Eventually, a scheme could be set up for doing an approximate
## inverse within oce, but the arguments against that include (a) the difficulty
## in implementing it and (b) the possibility that non-invertable projections
## simply haven't been generated enough interest in the broader cartographic
## community to merit inclusion in oce.

## 1. alsk overdraws world, pluts it's a niche proj (Alaska)
## 2. calcofi is deleted because it's nor really a projection; it's more
##    like a coordinate transformation.
## 3. isea is deleted because it causes a segmentation fault on coastlineWorld.
## 4. krovak causes overdrawn coastlines, plus it's a niche proj (Czech Republic)
## 5. labrd is deleted because it returns NaN for every point
##    on the coastlineWorld; fixing this is not a high priority
##    given that it is a niche projection that has caused problems
##    in PROJ.4 also.
##knownProj4<-c("aea", "aeqd", "aitoff", "alsk", "bipc", "bonne", #"calcofi",
knownProj4 <- c("aea", "aeqd", "aitoff",         "bipc", "bonne",
                "cass", "cc", "cea", "collg", "crast", "eck1", "eck2", "eck3",
                "eck4", "eck5", "eck6", "eqc", "eqdc", "euler", "etmerc",
                "fahey", "fouc", "fouc_s", "gall", "geos", "gn_sinu", "gnom",
                ##"goode", "gs48", "gs50", "hatano", "healpix", "rhealpix",
                "goode",                   "hatano", "healpix", "rhealpix",
                ##"igh","imw_p", "isea", "kav5", "kav7", "krovak", "labrd",
                "igh",  "imw_p",         "kav5", "kav7",
                ##"laea", "lonlat", "latlon", "lcc", "lcca", "leac", "lee_os",
                ## 20190930 deprecate lcca
                "laea",   "lonlat", "longlat", "latlon", "lcc", "lcca", "leac",
                "loxim", "lsat", "mbt_s", "mbt_fps", "mbtfpp", "mbtfpq",
                "mbtfps", "merc", "mil_os", "mill", "moll", "murd1", "murd2",
                ##"murd3", "natearth", "nell", "nell_h", "nsper", "nzmg",
                "murd3",   "natearth", "nell", "nell_h", "nsper",
                "ob_tran", "ocea", "oea", "omerc", "ortho", "pconic", "poly",
                "putp1", "putp2", "putp3", "putp3p", "putp4", "putp4p",
                "putp5", "putp5p", "putp6", "putp6p", "qsc", "qua_aut",
                "robin", "rouss", "sinu", "somerc", "stere", "sterea"
                ##"gstmerc", "tcea", "tissot", "tmerc", "tpeqd", "tpers", "ups"
                ,            "tcea", "tissot", "tmerc", "tpeqd", "tpers", "ups",
                "urm5", "urmfps", "utm", "vandg", "vitk1", "wag1", "wag2",
                "wag3", "wag4", "wag5", "wag6", "weren", "wink1", "wintri")

#' Convert Longitude and Latitude to X and Y
#'
#' If a projection is already being used (e.g. as set by \code{\link{mapPlot}})
#' then only \code{longitude} and \code{latitude} should be given, and the
#' other arguments will be inferred by \code{lonlat2map}.  This is important
#' because otherwise, if a new projection is called for, it will ruin any
#' additions to the existing plot.
#'
#' @param longitude a vector containing decimal longitudes, or a list
#' containing items named \code{longitude} and \code{latitude}, in which case
#' the indicated values are used, and next argument is ignored.
#' @param latitude a vector containing decimal latitude (ignored if
#' \code{longitude} is a list, as described above).
#' @param projection optional indication of projection.  This must be character
#' string in the format used by the \code{rgdal} package;
#' see \code{\link{mapPlot}}.)
#' @return A list containing \code{x} and \code{y}.
#' @author Dan Kelley
#' @seealso \code{mapLongitudeLatitudeXY} is a safer alternative, if a map has
#' already been drawn with \code{\link{mapPlot}}, because that function cannot
#' alter an existing projection. \code{\link{map2lonlat}} is an inverse to
#' \code{map2lonlat}.
#' @examples
#'
#' \dontrun{
#' library(oce)
#' ## Cape Split, in the Minas Basin of the Bay of Fundy
#' cs <- list(longitude=-64.49657, latitude=45.33462)
#' xy <- lonlat2map(cs, projection="+proj=merc")
#' map2lonlat(xy)
#' }
#' @family functions related to maps
lonlat2map <- function(longitude, latitude, projection="")
{
    ##cat("map.R:1676 in lonlat2map(..., projection='", projection, "', ...)\n", sep="")
    ## NOTE: the proj4 method can run into errors (e.g. "ortho" for points on opposite
    ## side of the earth) an may have to be done (slowly) point by point; a warning is
    ## issued if so.
    if (is.list(longitude)) {
        latitude <- longitude$latitude
        longitude <- longitude$longitude
    }
    if (missing(latitude))
        stop("latitude is missing")
    n <- length(longitude)
    if (n != length(latitude))
        stop("lengths of longitude and latitude must match but they are ", n, " and ", length(latitude))
    ## Use proj4 if it has been set up (and still exists).
    if ("" == projection) projection <- .Projection()$projection # FIXME
    if (inherits(projection, "CRS")) {
        projection <- projection@projargs
    }
    pr <- gsub(".*\\+proj=([^ ]*).*", "\\1", projection)
    #gsub(" .*$", "", gsub("^\\+proj=", "", projection))
    if (!(pr %in% knownProj4))
        stop("projection '", pr, "' is unknown; try one of: ", paste(knownProj4, collapse=','))
                                        #if (length(grep("aitoff", pr))) stop("+proj=aitoff cannot be used")
                                        #if (length(grep("robin", pr))) stop("+proj=robin cannot be used")
                                        #if (length(grep("wintri", pr))) stop("+proj=wintri cannot be used")
    ll <- cbind(longitude, latitude)
    ## Next added 20150523 for rgdal transition; keep old code for a while
    if (0 == length(grep("ellps=", projection)))
        projection<- paste(projection, "+ellps=sphere")
    n <- length(longitude)
    if (!requireNamespace("rgdal", quietly=TRUE))
        stop('must install.packages("rgdal") to plot maps with projections')
    owarn <- options()$warn
    options(warn=-1)
    ## April 2016: rgdal::project will soon return named quantities
    capture.output(XY <- unname(rgdal::project(ll, proj=as.character(projection), inv=FALSE)))
    options(warn=owarn)
    xy <- list(x=XY[, 1], y=XY[, 2])
    ## 20150523 if (!getOption("externalProj4", FALSE)) {
    ## 20150523     ## message("doing PROJ.4 calculations within Oce, for speed and accuracy")
    ## 20150523     if (0 == length(grep("ellps=", projection)))
    ## 20150523         projection<- paste(projection, "+ellps=sphere")
    ## 20150523     n <- length(longitude)
    ## 20150523     owarn <- options()$warn
    ## 20150523     options(warn=-1)
    ## 20150523     XY <- rgdal::project(ll, proj=as.character(projection), inv=FALSE)
    ## 20150523     options(warn=owarn)
    ## 20150523     xy <- list(x=XY[,1], y=XY[,2])
    ## 20150523     ##pre-rgdal XY <- .C("proj4_interface", as.character(projection), as.integer(TRUE),
    ## 20150523     ##pre-rgdal          as.integer(n), as.double(longitude), as.double(latitude),
    ## 20150523     ##pre-rgdal          X=double(n), Y=double(n), NAOK=TRUE)
    ## 20150523     ##pre-rgdal xy <- list(x=XY$X, y=XY$Y)
    ## 20150523 } else {
    ## 20150523     ## message("doing projection calculations with 'proj4' package")
    ## 20150523     if (!requireNamespace("proj4", quietly=TRUE))
    ## 20150523         stop("must install 'proj4' package to get options(externalProj4=TRUE) to work")
    ## 20150523     m <- NULL                 # for the try()
    ## 20150523     try({
    ## 20150523         m <- proj4::project(ll, proj=projection)
    ## 20150523     }, silent=TRUE)
    ## 20150523     if (is.null(m)) {
    ## 20150523         m <- matrix(unlist(lapply(1:n, function(i)
    ## 20150523                                   {
    ## 20150523                                       t <- try({proj4::project(ll[i,], proj=projection)}, silent=TRUE)
    ## 20150523                                       if (inherits(t, "try-error")) c(NA, NA) else t[1,]
    ## 20150523                                   })),
    ## 20150523                     ncol=2, byrow=TRUE)
    ## 20150523         warning("proj4 calculation is slow because it was done pointwise")
    ## 20150523     }
    ## 20150523     xy <- list(x=m[,1], y=m[,2])
    ## 20150523 }
    .Projection(list(type="proj4", projection=projection))
    ##mapproj::.Last.projection(list(projection="")) # turn off mapproj, in case it was on
    xy
}
