## vim:textwidth=128:expandtab:shiftwidth=4:softtabstop=4:foldmethod=marker

.axis <- local({
    val <- list(longitude=NULL, latitude=NULL)
    function(new) if (!missing(new)) val <<- new else val
})

.Projection <- local({
    ## Tave state, in a way that emulates mapproj.
    ## The 'type' can be 'none' or 'proj4' (previously, 'mapproj' was also allowed)
    val <- list(type="none", projection="")
    function(new) if (!missing(new)) val <<- new else val
})

#' Wrapper to rgdal::project()
#'
#' This function is used to isolate other oce functions from
#' changes to the [rgdal::project()] function in the \CRANpkg{rgdal}
#' package, which is used for calculations involved in both forward
#' and inverse map projections.
#'
#' Some highlights of the evolving relationship with rgdal are:
#' 1. See https://github.com/dankelley/oce/issues/653#issuecomment-107040093
#'    for the reason why oce switched from using [rgdal::rawTransform()],
#'    to [rgdal::project()], both functions provided by the
#'    \CRANpkg{rgdal} package.
#' 2. 2016 Apr: rgdal::project started returning named quantities
#' 3. 2019 Feb: allowNAs_if_not_legacy was added in rgdal 1.3-9 to prevent
#'    an error on i386/windows. However, using this argument imposes a
#'    burden on users to update \CRANpkg{rgdal}, so the approach taken
#'    here (by default, i.e. with `passNA=FALSE`) is to
#'    temporarily switch NA data to 0, and then switch
#'    back to NA after the calculation.
#'
#' @param xy,proj,inv,use_ob_tran,legacy  As for the [rgdal::project()] function in the
#' \CRANpkg{rgdal} package.
#'
#' @param passNA Logical value indicating whether to pass NA values into
#' \CRANpkg{rgdal}.  The default is `FALSE`, meaning that any NA
#' values are first converted to 0 before the calculation, and then
#' converted to NA afterwards. Setting this to `TRUE` produces
#' errors on the i386/windows platform, but it seems likely that a version
#' of \CRANpkg{rgdal} released after 1.3-9 may not have that error.
#'
#' @template debugTemplate
#'
#' @return A two-column matrix, with first column holding either
#' `longitude` or `x`, and second column holding either
#' `latitude` or `y`.
oceProject <- function(xy, proj, inv=FALSE, use_ob_tran=FALSE, legacy=TRUE, passNA=FALSE, debug=getOption("oceDebug"))
{
    if (!requireNamespace("rgdal", quietly=TRUE))
        stop('must install.packages("rgdal") to do map projections')
    oceDebug(debug, "oceProject(xy, proj=\"", proj, "\", ...) {\n", sep="", unindent=1, style="bold")
    owarn <- options()$warn # this, and the capture.output, quieten the processing
    ## {{{ OLD 'rgdal' method
    options(warn=-1)
    if (passNA) {
        na <- which(is.na(xy[,1]))
        xy[na, ] <- 0
        capture.output(
                       {
                           XY <- unname(rgdal::project(xy, proj=proj, inv=inv))
                       }
        )
        XY[na, ] <- NA
    } else {
        if (.Platform$OS.type == "windows" && .Platform$r_arch == "i386") {
            if (packageVersion("rgdal") < "1.3.9")
                stop("rgdal must be at least version 1.3.9, on i386/windows platforms")
            capture.output(
                           {
                               XY <- unname(rgdal::project(xy, proj=proj, inv=inv, legacy=legacy, allowNAs_if_not_legacy=TRUE))
                           }
            )
        } else {
            capture.output(
                           {
                               XY <- unname(rgdal::project(xy=xy, proj=proj, inv=inv, legacy=legacy))
                           }
            )
        }
    }
    ## }}}
    ## {{{ NEW 'sf' method
    na <- which(!is.finite(xy[,1]))
    xy[na, ] <- 0
    if (inv) {
        XYSF <- unname(sf::sf_project(proj, "+proj=longlat", xy))
    } else {
        XYSF <- unname(sf::sf_project("+proj=longlat", proj, xy))
    }
    XYSF[na, ] <- NA
    if (!all.equal(XY, XYSF)) {
        warning("oceProject(): disagreement between old 'sp' method and new 'sf' method\n")
    }
    ## }}}
    options(warn=owarn)
    oceDebug(debug, "} # oceProject\n", sep="", unindent=1, style="bold")
    XY
}


#' Calculate lon-lat coordinates of plot-box trace
#'
#' Trace along the plot box, converting from xy coordinates to lonlat
#' coordinates. The results are used by [mapGrid()]
#' and [mapAxis()] to ignore out-of-frame grid
#' lines and axis labels.
#'
#' Note: this procedure does not work for projections that have trouble
#' inverting points that are "off the globe". For this reason, this function
#' examines .Projection()$projection and if it contains the string
#' `"wintri"`, then the above-stated procedure is skipped, and
#' the return value has each of the numerical quantities set to `NA`,
#' and `ok` set to `FALSE`.
#'
#' @param n number of points to check along each side of the plot box
#' @template debugTemplate
#'
#' @return a list containing `lonmin`, `lonmax`,
#' `latmin`, `latmax`, and `ok`; the last
#' of which indicates whether at least one point on the plot box
#' is invertible. Note that longitude are expressed in the
#' range from -180 to 180 degrees.
#'
#' @author Dan Kelley
#'
#' @family functions related to maps
usrLonLat <- function(n=25, debug=getOption("oceDebug"))
{
    oceDebug(debug, "usrLonLat(n=", n, ", debug=", debug, ") {\n", unindent=1, sep="", style="bold")
    usr <- par("usr")
    oceDebug(debug, "usr=", paste(usr, collapse=" "), "\n", sep="")
    if (length(grep("wintri", .Projection()$projection)))
        return(list(lonmin=NA, lonmax=NA, latmin=NA, latmax=NA, ok=FALSE))
    x <- c(seq(usr[1], usr[2], length.out=n),
           rep(usr[2], n),
           seq(usr[2], usr[1], length.out=n),
           rep(usr[1], n))
    y <- c(rep(usr[3], n),
           seq(usr[3], usr[4], length.out=n),
           rep(usr[4], n),
           seq(usr[4], usr[3], length.out=n))
    g <- expand.grid(x=seq(usr[1], usr[2], length.out=n),
                     y=seq(usr[3], usr[4], length.out=n))
    x <- g$x
    y <- g$y

    ## if (debug > 2)
    ##     points(x, y, pch=20, cex=3, col=2)
    oceDebug(debug, "about to call map2lonlat\n")
    ll <- map2lonlat(x, y)
    nok <- sum(is.finite(ll$longitude))
    ## Convert -Inf and +Inf to NA
    oceDebug(debug, "DONE with call map2lonlat\n")
    bad <- !is.finite(ll$longitude) | !is.finite(ll$latitude)
    ll$longitude[bad] <- NA
    ll$latitude[bad] <- NA
    oceDebug(debug, "sum(bad)/length(bad)=", sum(bad)/length(bad), "\n", sep="")
    if (debug > 2)
        mapPoints(ll$longitude, ll$latitude, pch=20, cex=2, col=3)
    lonmin <- if (any(is.finite(ll$longitude))) min(ll$longitude, na.rm=TRUE) else NA
    lonmax <- if (any(is.finite(ll$longitude))) max(ll$longitude, na.rm=TRUE) else NA
    latmin <- if (any(is.finite(ll$latitude))) min(ll$latitude, na.rm=TRUE) else NA
    latmax <- if (any(is.finite(ll$latitude))) max(ll$latitude, na.rm=TRUE) else NA
    ## To simplify later use, put lon in range -180 to 180, and order if needed
    lonmin <- min(lonmin, 180, na.rm=TRUE)
    lonmin <- max(lonmin, -180, na.rm=TRUE)
    lonmax <- min(lonmax, 180, na.rm=TRUE)
    lonmax <- max(lonmax, -180, na.rm=TRUE)
    if (!is.na(lonmin) && !is.na(lonmax)) {
        if (lonmin > lonmax) {
            tmp <- lonmin
            lonmin <- lonmax
            lonmax <- tmp
        }
        ## special case: if we are showing more than half the earth, assume
        ## it's a global view, and extend accordingly
        if ((lonmax - lonmin) > 180) {
            lonmin <- -180
            lonmax <- 180
            latmin <- -90
            latmax <- 90
        }
    }
    oceDebug(debug, sprintf("lonmin=%.3f, lonmax=%.3f, latmin=%.3f, latmax=%.3f\n",
                            lonmin, lonmax, latmin, latmax))
    oceDebug(debug, "nok=", nok, ", n=", n, ", nok/n=", nok/n, "\n")
    oceDebug(debug, "} # usrLonLat()\n", unindent=1, style="bold")
    rval <- list(lonmin=lonmin, lonmax=lonmax, latmin=latmin, latmax=latmax,
                 ok=nok/n>0.5&&is.finite(lonmin)&&is.finite(lonmax)&&is.finite(latmin)&&is.finite(latmax))
    rval
}

#' Coordinate Reference System strings for some oceans
#'
#' Create a coordinate reference string (CRS), suitable for use as a
#' `projection` argument to [mapPlot()] or
#' [plot,coastline-method()].
#'
#' @section Caution: This is a preliminary version of this function,
#' with the results being very likely to change through the autumn of 2016,
#' guided by real-world usage.
#'
#' @param region character string indicating the region. This must be
#' in the following list (or a string that matches to just one entry,
#' with [pmatch()]):
#' `"North Atlantic"`, `"South Atlantic"`, `"Atlantic"`,
#' `"North Pacific"`, `"South Pacific"`, `"Pacific"`,
#' `"Arctic"`,  and `"Antarctic"`.
#'
#' @return string contain a CRS, which can be used as `projection`
#' in [mapPlot()].
#'
#' @author Dan Kelley
#'
#' @family functions related to maps
#'
#' @examples
#'\donttest{
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
#'}
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
#' This is a utility function used by [mapGrid()]. It works
#' simply by subtracting 180 from each longitude, if any longitude
#' in the vector exceeds 180.
#'
#' @param longitudes a numerical vector of longitudes
#'
#' @return vector of longitudes, shifted to the desired range.
#'
#' @seealso [matrixShiftLongitude()] and [standardizeLongitude()].
#'
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
#' @param longitude either a logical value or a vector of longitudes. There
#' are three possible cases:
#' (1) If `longitude=TRUE` (the default) then ticks and nearby numbers will occur at the
#' longitude grid established by the previous call to [mapPlot()];
#' (2) if `longitude=FALSE` then no longitude ticks or numbers are
#' drawn;
#' (3) if `longitude` is a vector of numerical values, then those ticks
#' are placed at those values, and numbers are written beside them.
#' Note that in cases 1 and 3, efforts are made to avoid overdrawing text,
#' so some longitude values might get ticks but not numbers. To get ticks
#' but not numbers, set `cex.axis=0`.
#'
#' @param latitude similar to `longitude` but for latitude.
#'
#' @param tick parameter passed to [axis()].
#'
#' @param line parameter passed to [axis()].
#'
#' @param pos parameter passed to [axis()].
#'
#' @param outer parameter passed to [axis()].
#'
#' @param font axis font, passed to [axis()].
#'
#' @param lty axis line type, passed to [axis()].
#'
#' @param lwd axis line width, passed to [axis()]).
#'
#' @param lwd.ticks tick line width, passed to [axis()].
#'
#' @param col axis color, passed to [axis()].
#'
#' @param col.ticks axis tick color, passed to [axis()].
#'
#' @param hadj an argument that is transmitted to [axis()].
#'
#' @param padj an argument that is transmitted to [axis()].
#'
#' @param tcl axis-tick size (see [par()]).
#'
#' @param cex.axis axis-label expansion factor (see [par()]); set to 0
#' to prevent numbers from being placed in axes.
#'
#' @param mgp three-element numerical vector describing axis-label
#' placement (see [par()]). It usually makes sense to set
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
#'\donttest{
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
#'}
#'
#' @author Dan Kelley
#'
#' @seealso A map must first have been created with [mapPlot()].
#'
#' @family functions related to maps
mapAxis <- function(side=1:2, longitude=TRUE, latitude=TRUE,
                    tick=TRUE, line=NA, pos=NA, outer=FALSE, font=NA,
                    lty="solid", lwd=1, lwd.ticks=lwd, col=NULL, col.ticks=NULL,
                    hadj=NA, padj=NA, tcl=-0.3, cex.axis=1,
                    mgp=c(0, 0.5, 0),
                    debug=getOption("oceDebug"))
{
    if ("none" == .Projection()$type)
        stop("must create a map first, with mapPlot()\n")
    oceDebug(debug, "mapAxis(",
             argShow(side), #side=c(", paste(side, collapse=","), ")",
             argShow(longitude), #", longitude=", if (length(longitude)) c(longitude[1], "...") else "NULL",
             argShow(latitude), #", latitude=", if (length(latitude)) c(latitude[1], "...") else "NULL",
             ") { \n", sp="", unindent=1, sep="", style="bold")
    boxLonLat <- usrLonLat()
    axis <- .axis()
    #if (debug > 0) print(axis)
    if (is.logical(longitude) && !longitude && is.logical(latitude) && !latitude) {
        oceDebug(debug, "longitude=latitude=FALSE, so not drawing axes\n")
        return()
    }
    if (is.logical(longitude) && longitude[1]) {
        longitude <- axis$longitude
        oceDebug(debug, "autosetting to", vectorShow(longitude))
    }
    if (is.logical(latitude) && latitude[1]) {
        latitude <- axis$latitude
        oceDebug(debug, "autosetting to", vectorShow(latitude))
    }
    oceDebug(debug, "mapAxis: initially, ", vectorShow(longitude))
    if (boxLonLat$ok) {
        ok <- boxLonLat$lonmin <= longitude & longitude <= boxLonLat$lonmax
        longitude <- longitude[ok]
    }
    oceDebug(debug, "mapAxis: after box-trimming, ", vectorShow(longitude))
    oceDebug(debug, "mapAxis: initially, ", vectorShow(latitude))
    if (boxLonLat$ok) {
        ok <- boxLonLat$latmin <= latitude & latitude <= boxLonLat$latmax
        latitude <- latitude[ok]
    }
    oceDebug(debug, "mapAxis: after box-trimming, ", vectorShow(latitude))

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
            ## prevent calling axis() with cex.axis=0, by just giving empty labels then
            oceDebug(debug, "calling axis(1) with cex.axis=", cex.axis, "\n")
            axis(side=1, at=AT, labels=if (cex.axis>0) fixneg(LAB) else rep("", length(AT)),
                 mgp=mgp, tick=tick, line=line, pos=pos, outer=outer, font=font,
                 lty=lty, lwd=lwd, lwd.ticks=lwd.ticks, col=col, col.ticks=col.ticks,
                 hadj=hadj, padj=padj, tcl=tcl, cex.axis=if (cex.axis>0) cex.axis else 1)
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
        oceDebug(debug, paste("LONLIST=", paste(LONLIST, collapse=" "), "\n"))
        for (lat in latitude) {
            if (debug > 3)
                oceDebug(debug, "check ", lat, "N for axis on side=2 (usr[1]=", usr[1], ")\n", sep="")
            ## Seek a point at this lon that matches the lon-lat relationship on side=1

            ## FIXME: I wonder why I don't use the optimize() method that I use for side=1 here
            ## as well. Maybe I ought to try both.  I sort of think this bracket-uniroot method
            ## is best, but note that issue 1349 was because I had the `tol` in the `uniroot`
            ## set to 1deg, which was nutty.
            for (iLON in 2:length(LONLIST)) {
                #if (lat == 55) browser()
                LONLOOK <- LONLIST[iLON+c(-1, 0)]
                ##cat("f(LONLOOK[1]=", LONLOOK[1], "=", LONLOOK[1]+360, ")= ", f(LONLOOK[1]), " (iLON=", iLON, ")\n")
                ##cat("f(LONLOOK[2]=", LONLOOK[2], "=", LONLOOK[2]+360, ")= ", f(LONLOOK[2]), " (iLON=", iLON, ")\n")
                f1 <- f(LONLOOK[1])
                if (!is.finite(f1))
                    next
                f2 <- f(LONLOOK[2])
                if (!is.finite(f2)) {
                    ##cat("f2 not finite, so skipping\n")
                    next
                }
                if (f1 * f2 > 0) {
                    ##cat("f1*f2 > 0, so skipping\n")
                    next
                }
                ##cat(" looking promising LONLOOK[1]=", LONLOOK[1], ", LONLOOK[2]=", LONLOOK[2], "; r follows\n")
                r <- uniroot(f, lower=LONLOOK[1], upper=LONLOOK[2], tol=0.001) # 0.001deg < 100m.
                ##print(r)
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
        #browser()
        if (!is.null(AT)) {
            oceDebug(debug, "calling axis(2) with cex.axis=", cex.axis, "\n")
            ## prevent calling axis() with cex.axis=0, by just giving empty labels then
            axis(side=2, at=AT, labels=if (cex.axis>0) fixneg(LAB) else rep("", length(AT)),
                 mgp=mgp, tick=tick, line=line, pos=pos, outer=outer, font=font,
                 lty=lty, lwd=lwd, lwd.ticks=lwd.ticks, col=col, col.ticks=col.ticks,
                 hadj=hadj, padj=padj, tcl=tcl, cex.axis=if (cex.axis>0) cex.axis else 1)
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
    oceDebug(debug, "} # mapAxis()\n", sep="", unindent=1, style="bold")
}


#' Add Contours on a Existing map
#'
#' Plot contours on an existing map.
#'
#' @param longitude vector of longitudes of points to be plotted, or an object of
#' class `topo` (see [topo-class]), in which case
#' `longitude`, `latitude` and `z` are inferred from that object.
#'
#' @param latitude vector of latitudes of points to be plotted.
#'
#' @param z matrix to be contoured. The number of rows and columns in `z`
#' must equal the lengths of `longitude` and `latitude`, respectively.
#'
#' @param nlevels number of contour levels, if and only if `levels` is not supplied.
#'
#' @param levels vector of contour levels.
#'
#' @param labcex `cex` value used for contour labelling. As with
#' [contour()], this is an absolute size, not a multiple of
#' [`par`]`("cex")`.
#'
#' @param drawlabels logical value or vector indicating whether to draw contour
#' labels.  If the length of `drawlabels` is less than the number of
#' levels specified, then [rep()] is used to increase the length,
#' providing a value for each contour line. For those levels that are thus
#' indicated, labels are added, at a spot where the contour line is
#' closest to horizontal on the page. First, though, the region underneath
#' the label is filled with the colour given by [`par`]`("bg")`.
#' See \dQuote{Limitations} for notes on the status of contour
#' labelling, and its limitations.
#'
#' @param underlay character value relating to handling labels. If
#' this equals `"erase"` (which is the default), then the contour line
#' is drawn  first, then the area under the label is erased (filled with
#' white 'ink'), and then the label is drawn. This can be useful
#' in drawing coarsely-spaced labelled contours on top of finely-spaced
#' unlabelled contours. On the other hand, if `underlay` equals
#' `"interrupt"`, then the contour line is interrupted in the
#' region of the label, which is closer to the scheme used by the
#' base [contour()] function.
#'
#' @param col colour of the contour line, as for [`par`]`("col")`,
#' except here `col` gets lengthened by calling [rep()],
#' so that individual contours can be coloured distinctly.
#'
#' @param lty type of the contour line, as for [`par`]`("lty")`,
#' except for lengthening, as described for `col`.
#'
#' @param lwd width of the contour line, as for [`par`]`("lwd")`,
#' except for lengthening, as described for `col` and `lty`.
#'
#' @template debugTemplate
#'
#' @details
#' Adds contour lines to an existing map, using [mapLines()].
#'
#' The ability to label the contours was added in February, 2019, and
#' how this works may change through the summer months of that year.
#' Note that label placement in `mapContour` is handled differently
#' than in [contour()].
#'
#' @examples
#'\donttest{
#' library(oce)
#' data(coastlineWorld)
#' if (requireNamespace("ocedata", quietly=TRUE)) {
#'     data(levitus, package="ocedata")
#'     par(mar=rep(1, 4))
#'     mapPlot(coastlineWorld, projection="+proj=robin", col="lightgray")
#'     mapContour(levitus[['longitude']], levitus[['latitude']], levitus[['SST']])
#' }
#'}
#'
#' @author Dan Kelley
#'
#' @seealso A map must first have been created with [mapPlot()].
#' @family functions related to maps
mapContour <- function(longitude, latitude, z,
                       nlevels=10, levels=pretty(range(z, na.rm=TRUE), nlevels),
                       ##labels=null,
                       ##xlim=range(longitude, finite=TRUE),
                       #ylim=range(latitude, finite=TRUE),
                       labcex=0.6,
                       drawlabels=TRUE,
                       underlay="erase",
                       ##vfont,
                       ## axes=TRUE, frame.plot=axes,
                       col=par("fg"), lty=par("lty"), lwd=par("lwd"),
                       debug=getOption("oceDebug"))
{
    oceDebug(debug, "mapContour() {\n", sep="", unindent=1, style="bold")
    if ("none" == .Projection()$type)
        stop("must create a map first, with mapPlot()\n")
    if (missing(longitude))
        stop("must supply longitude")
    if ("data" %in% slotNames(longitude) && # handle e.g. 'topo' class
        3 == sum(c("longitude", "latitude", "z") %in% names(longitude@data))) {
        z <- longitude@data$z
        latitude <- longitude@data$latitude
        longitude <- longitude@data$longitude
    }
    if (missing(latitude))
        stop("must supply latitude")
    if (missing(z))
        stop("must supply z")
    if (!underlay %in% c("erase", "interrupt"))
        stop("underlay must be \"erase\" or \"interrupt\"")
    if (underlay == "interrupt" && !requireNamespace("sp", quietly=TRUE))
        stop("must have \"sp\" package available for underlay=\"interupt\"")
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
    drawlabels <- rep(drawlabels, nlevels)
    labcex <- rep(labcex, nlevels)
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
    colUnderLabel <- "white" # use a variable in case we want to add as an arg
    for (ilevel in 1:nlevels) {
        oceDebug(debug, "contouring at level ", levels[ilevel], "\n")
        label <- as.character(levels[ilevel]) # ignored unless drawlabels=TRUE
        w <- 1.0*strwidth(levels[ilevel], "user", cex=labcex) # ignored unless drawlabels=TRUE
        h <- 1.0*strheight(label, "user", cex=labcex) # ignored unless drawlabels=TRUE
        oceDebug(debug > 1, "w=", w, ", h=", h, "\n")
        cl <- contourLines(x=longitude[xx],
                           y=latitude[yy],
                           z=z, levels=levels[ilevel])
        if (length(cl) > 0) {
            for (i in seq_along(cl)) {
                oceDebug(debug > 1, "segment number=i=", i, "; level=", levels[ilevel], "\n")
                xy <- lonlat2map(cl[[i]]$x, cl[[i]]$y)
                xc <- xy$x
                yc <- xy$y
                nc <- length(xc)
                if (drawlabels[ilevel]) {
                    slopeMin <- 9999999 # big
                    slopeMinj <- NULL
                    slopeMinj2 <- NULL
                    canlabel <- FALSE
                    for (j in 1:nc) {
                        j2 <- j
                        while (j2 < nc) {
                            dy <- yc[j2] - yc[j]
                            dx <- xc[j2] - xc[j]
                            dist <- sqrt(dx^2 + dy^2)
                            if (dist > 1.4 * w && dx != 0.0) {
                                oceDebug(debug > 2, "enough space at j=",j,", j2=", j2, "\n")
                                slope <- dy / dx
                                if (abs(slope) < slopeMin) {
                                    slopeMin <- abs(slope)
                                    slopeMinj <- j
                                    slopeMinj2 <- j2
                                    canlabel <- TRUE
                                }
                                break
                            }
                            j2 <- j2 + 1
                        }
                    }
                    if (canlabel) {
                        labelj <- floor(0.5 + 0.5*(slopeMinj + slopeMinj2))
                        angle <- atan2(yc[slopeMinj2]-yc[slopeMinj], xc[slopeMinj2]-xc[slopeMinj])
                        oceDebug(debug > 1,
                                 sprintf("j=%d j2=%d slopeMin=%.3g slopeMinj=%d slopeMinj2=%d\n",
                                         j, j2, slopeMin, slopeMinj, slopeMinj2))
                        if (debug > 2) {
                            points(xc[slopeMinj], yc[slopeMinj], col="darkgreen", pch=20)
                            points(xc[slopeMinj2], yc[slopeMinj2], col="red", pch=20)
                            points(xc[labelj], yc[labelj], col="blue", pch=20) # centre
                        }
                        if (angle > pi/2 || angle < -pi/2)
                            angle <- angle + pi
                        oceDebug(debug, sprintf("step 2: label='%s' x=%.2g y=%.2g angle=%.9g deg\n",
                                                label, xc[labelj], yc[labelj], angle*180/pi))
                        S <- sin(-angle)
                        C <- cos(-angle)
                        rot <- matrix(c(C, -S, S, C), byrow=TRUE, nrow=2)
                        X <- c(-w/2, -w/2, w/2, w/2)
                        Y <- c(-h/2, h/2, h/2, -h/2)
                        XY <- cbind(X, Y)
                        XYrot <- XY %*% rot
                        if (underlay == "erase") {
                            lines(xc, yc, lwd=lwd[ilevel], lty=lty[ilevel], col=col[ilevel])
                            polygon(xc[labelj]+XYrot[,1], yc[labelj]+XYrot[,2],
                                    col=colUnderLabel, border=colUnderLabel)
                        } else if (underlay == "interrupt") {
                            erase <- 1==sp::point.in.polygon(xc, yc,
                                                             xc[labelj]+XYrot[,1], yc[labelj]+XYrot[,2])
                            polyx <- xc[labelj] + XYrot[,1]
                            polyy <- yc[labelj] + XYrot[,2]
                            polyNew <- sf::st_polygon(list(outer=cbind(c(polyx, polyx[1]), c(polyy, polyy[1]))))
                            pointsNew <- sf::st_multipoint(cbind(xc, yc))
                            insideNew <- sf::st_intersection(pointsNew, polyNew)
                            eraseNew <- matrix(pointsNew %in% insideNew, ncol=2)[,1]
                            eraseOld <- erase
                            if (!all.equal(eraseNew, erase)) {
                                warning("mapContour() error: 'erase' disagreement with trial 'sf' method. Please post an issue on www.github.com/dankelley/oce/issues\n")
                            }
                            oceDebug(debug, "ignoring", sum(erase), "points under", label, "contour\n")
                            XC <- xc
                            YC <- yc
                            XC[erase] <- NA
                            YC[erase] <- NA
                            lines(XC, YC, lwd=lwd[ilevel], lty=lty[ilevel], col=col[ilevel])
                        } else {
                            stop("cannot have underlay=\"", underlay, "\"; please report as a bug")
                        }
                        text(xc[labelj], yc[labelj], label, col=col[ilevel],
                             srt=angle*180/pi, cex=labcex[ilevel])
                    } else {
                        lines(xc, yc, lwd=lwd[ilevel], lty=lty[ilevel], col=col[ilevel])
                    }
                } else {
                    lines(xc, yc, lwd=lwd[ilevel], lty=lty[ilevel], col=col[ilevel])
                }
            }
        }
    }
    oceDebug(debug, "} # mapContour()\n", sep="", unindent=1, style="bold")
}

#' Draw a coordinate system
#'
#' Draws arrows on a map to indicate a coordinate system, e.g. for an
#' to indicate a coordinate system set up so that one axis is parallel
#' to a coastline.
#'
#' This is a preliminary version of this function. It only
#' works if the lines of constant latitude are horizontal on the plot.
#'
#' @param latitude numeric value of latitude in degrees.
#'
#' @param longitude numeric value of longitude in degrees.
#'
#' @param L axis length in km.
#'
#' @param phi angle, in degrees counterclockwise, that the "x" axis makes to a line of latitude.
#'
#' @param ... plotting arguments, passed to [mapArrows()];
#' see \dQuote{Examples} for how to control the arrow-head size.
#'
#' @examples
#'\donttest{
#' library(oce)
#' if (requireNamespace("ocedata", quietly=TRUE)) {
#'     data(coastlineWorldFine, package='ocedata')
#'     HfxLon <- -63.5752
#'     HfxLat <- 44.6488
#'     mapPlot(coastlineWorldFine, proj='+proj=merc',
#'             longitudelim=HfxLon+c(-2,2), latitudelim=HfxLat+c(-2,2),
#'             col='lightgrey')
#'     mapCoordinateSystem(HfxLon, HfxLat, phi=45, length=0.05)
#'    }
#'}
#'
#' @author Chantelle Layton
#'
#' @family functions related to maps
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
#' Adds arrows for a direction field on an existing map.  There are different
#' possibilities for how `longitude`, `latitude` and `u` and
#' `v` match up.  In one common case, all four of these are matrices, e.g.
#' output from a numerical model.  In another, `longitude` and
#' `latitude` are the coordinates along the matrices, and are thus stored in
#' vectors with lengths that match appropriately.
#'
#' @param longitude,latitude vectors of the starting points for arrows.
#'
#' @param u,v components of a vector to be shown as a direction
#'     field.
#'
#' @param scale latitude degrees per unit of `u` or `v`.
#'
#' @param length length of arrow heads, passed to [arrows()].
#'
#' @param code code of arrows, passed to [arrows()].
#'
#' @param col color of arrows.  This may be a single color, or a matrix
#'     of colors of the same dimension as `u`.
#'
#' @param \dots optional arguments passed to [arrows()], e.g.
#'     `angle` and `lwd` can be useful in differentiating different
#'     fields.
#'
#' @examples
#'\donttest{
#' library(oce)
#' data(coastlineWorld)
#' par(mar=rep(2, 4))
#' mapPlot(coastlineWorld, longitudelim=c(-120,-55), latitudelim=c(35, 50),
#'         proj="+proj=laea +lat0=40 +lat1=60 +lon_0=-110")
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
#'}
#'
#' @author Dan Kelley
#'
#' @seealso A map must first have been created with [mapPlot()].
#'
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
#' return value from [mapLocator()]), in which case the following
#' two arguments are ignored.
#'
#' @param latitude vector of latitudes of points, needed only if they cannot
#' be inferred from the first argument.
#'
#' @details
#' This is mainly a wrapper around [lonlat2map()].
#'
#' @return
#' A list containing `x` and `y`.
#'
#'
#' @examples
#'\donttest{
#' library(oce)
#' data(coastlineWorld)
#' par(mfrow=c(2, 1), mar=rep(2, 4))
#' mapPlot(coastlineWorld, projection="+proj=moll") # sets a projection
#' xy <- mapLongitudeLatitudeXY(coastlineWorld)
#' plot(xy, type='l', asp=1)
#'}
#'
#' @author Dan Kelley
#'
#' @seealso A map must first have been created with [mapPlot()].
#'
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
#' Plot coordinates as a map, using one of the subset of projections
#' provided by the \CRANpkg{rgdal} package.  The projection information specified
#' with the `mapPlot` call is stored so that can be retrieved by related
#' functions, making it easy to add points, lines, text, images
#' or contours to an existing map.
#'
#' Creates a map using the indicated projection.  As noted in the
#' information on the `projection` argument, projections are specified in
#' the notation used by `project()` in the \CRANpkg{rgdal} package; see
#' \dQuote{Available Projections} for a list of possibilities.
#'
#' Further details on map projections are provided by references 1 and 11, an exhaustive
#' treatment that includes many illustrations, an overview of the history of the
#' topic, and some notes on the strengths and weaknesses of the various
#' formulations.  See especially pages 2 through 7, which define terms and
#' provide recommendations.  Reference 2 is also useful, especially regarding
#' datum shifts; references 3 and 4 are less detailed and perhaps better for novices.
#' See reference 8 for a gallery of projections.
#'
#' @param longitude either a vector of longitudes of points to be plotted, or
#' something (an `oce` object, a list, or a data frame) from which both
#' longitude and latitude may be inferred (in which case the `latitude`
#' argument is ignored).  If `longitude` is missing, both it and
#' `latitude` are taken from [coastlineWorld()].
#'
#' @param latitude vector of latitudes of points to be plotted (ignored
#' if the first argument contains both latitude and longitude).
#'
#' @param longitudelim optional vector of length two, indicating the
#' longitude limits of the plot. This value is used in the selection of
#' longitude lines that are shown (and possibly
#' labelled on the axes). In some cases, e.g. for polar views,
#' this can lead to odd results, with some expected longitude lines
#' being left out of the plot.  Altering `longitudelim` can
#' often help in such cases, e.g. `longitudelim=c(-180, 180)` will
#' force the drawing of lines all around the globe.
#'
#' @param latitudelim optional vector of length two, indicating
#' the latitude limits of the plot. This, together with `longitudelim`
#' (and, importantly, the geometry of the plot device) is used in the
#' selection of map scale.
#'
#' @param grid either a number (or pair of numbers) indicating the spacing of
#' longitude and latitude lines, in degrees, or a logical value (or pair of
#' values) indicating whether to draw an auto-scaled grid, or whether to skip
#' the grid drawing.  In the case of numerical values, `NA` can be used to
#' turn off the grid in longitude or latitude.  Grids are set up based on
#' examination of the scale used in middle 10 percent of the plot area, and for
#' most projections this works quite well.  If not, one may set
#' `grid=FALSE` and add a grid later with [mapGrid()].
#'
#' @param bg color of the background (ignored).
#'
#' @param fill is a deprecated argument; see [oce-deprecated].
#'
#' @param border color of coastlines and international borders (ignored unless
#' `type="polygon"`.
#'
#' @param col either the color for filling polygons (if `type="polygon"`)
#' or the color of the points and line segments (if `type="p"`,
#' `type="l"`, or `type="o"`). If `col=NULL` then a default
#' will be set: no coastline filling for the `type="polygon"` case,
#' or black coastlines, for `type="p"`, `type="l"`, or
#' `type="o"`.
#'
#' @param clip logical value indicating whether to trim any coastline elements that lie wholly
#' outside the plot region. This can prevent e.g. a problem of filling the whole plot area of
#' an Arctic stereopolar view, because the projected trace for Antarctica lies outside all
#' other regions so the whole of the world ends up being "land".  Setting `clip=FALSE`
#' disables this action, which may be of benefit in rare instances in the line connecting
#' two points on a coastline may cross the plot domain, even if those points are outside
#' that domain.
#'
#' @param type indication of type; may be `"polygon"`, for a filled polygon,
#' `"p"` for points, `"l"` for line segments, or `"o"` for points
#' overlain with line segments.
#'
#' @param axes logical value indicating whether to draw longitude and latitude
#' values in the lower and left margin, respectively.  This may not work well
#' for some projections or scales.  See also `lonlabels` and `latlabels`, which
#' offer more granular control of labelling.
#'
#' @param cex character expansion factor for plot symbols,
#' used if `type='p'` or any other value that yields symbols.
#'
#' @param cex.axis axis-label expansion factor (see [par()]).
#'
#' @param mgp three-element numerical vector describing axis-label
#' placement, passed to [mapAxis()].
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
#' @param lonlabels An optional logical value or numeric vector that controls
#' the labelling of longitude values using [mapAxis()]. There are
#' four possibilities for the value of `lonlabels`:
#' (1) If `lonlabels` is `TRUE` (the default), then reasonable values are inferred
#' and axes are drawn accordingly with both ticks and longitudes
#' alongside those ticks;
#' (2) if `lonlabels` is `FALSE`, then ticks are drawn by but not numbers;
#' (3) if `lonlabels` is `NULL`, then no axis ticks or numbers are are drawn; and
#' (4) if `lonlabels` is  a vector of finite numerical values, then tick marks
#' are placed  at those longitudes, and labels are put alongside them.
#' In cases 1 and 4, overdrawing of numbers is avoided,
#' so some ticks may not have numbers alongside them.
#' See also `latlabels`, and note that setting `axes=FALSE`
#' ensures that no longitude or latitude axes will be drawn regardless
#' of the values of `lonlabels` and `latlabels`.
#'
#' @param latlabels As `lonlabels`, but for latitude, on the left
#' plot axis.
#'
#' @param projection optional indication of projection, in one of two
#' forms. First, it may be a character string in the "CRS" format that is
#' used by the \CRANpkg{rgdal} package (and in much of modern computer-based
#' cartography). For example, `projection="+proj=merc"` specifies a
#' Mercator projection. The second format is the output from
#' [sp::CRS()] in the \CRANpkg{sp} package, which is an object
#' with a slot named `projarg` that gets used as a projection string.
#' See \dQuote{Details}.
#'
#' @param trim logical value indicating whether to trim islands or lakes
#' containing only points that are off-scale of the current plot box.  This
#' solves the problem of Antarctica overfilling the entire domain, for an
#' Arctic-centred stereographic projection.  It is not a perfect solution,
#' though, because the line segment joining two off-scale points might
#' intersect the plotting box.
#'
#' @param tissot logical value indicating whether to use [mapTissot()]
#' to plot Tissot indicatrices, i.e. ellipses at grid intersection points, which
#' indicate map distortion.
#'
#' @param debug a flag that turns on debugging.  Set to 1 to get a moderate
#' amount of debugging information, or to 2 to get more.
#'
#' @param ... optional arguments passed to some plotting functions.  This can
#' be useful in many ways, e.g.  Example 5 shows how to use `xlim` etc to
#' reproduce a scale exactly between two plots.
#'
#'
#' @seealso
#' Points may be added to a map with [mapPoints()], lines with
#' [mapLines()], text with [mapText()], polygons with
#' [mapPolygon()], images with [mapImage()], and scale bars
#' with [mapScalebar()].  Points on a map may be determined with mouse
#' clicks using [mapLocator()].  Great circle paths can be calculated
#' with [geodGc()].  See reference 8 for a demonstration of the available map
#' projections (with graphs).
#'
#' @examples
#' canProject <- .Platform$OS.type!="windows"&&requireNamespace("rgdal")
#' if (canProject) {
#'     library(oce)
#'     data(coastlineWorld)
#'
#'     # Example 1.
#'     # Mollweide (referenc 1 page 54) is an equal-area projection that works well
#'     # for whole-globe views.
#'     mapPlot(coastlineWorld, projection="+proj=moll", col='gray')
#'     mtext("Mollweide", adj=1)
#'
#'     # Example 2.
#'     # Note that filling is not employed (`col` is not
#'     # given) when the prime meridian is shifted, because
#'     # this causes a problem with Antarctica
#'     cl180 <- coastlineCut(coastlineWorld, lon_0=-180)
#'     mapPlot(cl180, projection="+proj=moll +lon_0=-180")
#'     mtext("Mollweide with coastlineCut", adj=1)
#'
#'     # Example 3.
#'     # Orthographic projections resemble a globe, making them attractive for
#'     # non-technical use, but they are neither conformal nor equal-area, so they
#'     # are somewhat limited for serious use on large scales.  See Section 20 of
#'     # reference 1. Note that filling is not employed because it causes a problem with
#'     # Antarctica.
#'     par(mar=c(3, 3, 1, 1))
#'     mapPlot(coastlineWorld, projection="+proj=ortho +lon_0=-180")
#'     mtext("Orthographic", adj=1)
#'
#'     # Example 4.
#'     # The Lambert conformal conic projection is an equal-area projection
#'     # recommended by reference 1, page 95, for regions of large east-west extent
#'     # away from the equator, here illustrated for the USA and Canada.
#'     par(mar=c(3, 3, 1, 1))
#'     mapPlot(coastlineCut(coastlineWorld, -100),
#'             longitudelim=c(-130,-55), latitudelim=c(35, 60),
#'             projection="+proj=lcc +lat_0=30 +lat_1=60 +lon_0=-100", col='gray')
#'     mtext("Lambert conformal", adj=1)
#'
#'     # Example 5.
#'     # The stereographic projection (reference 1, page 120) is conformal, used
#'     # below for an Arctic view with a Canadian focus.  Note the trick of going
#'     # past the pole: the second latitudelim value is 180 minus the first, and the
#'     # second longitudelim is 180 plus the first; this uses image points "over"
#'     # the pole.
#'     par(mar=c(3, 3, 1, 1))
#'     mapPlot(coastlineCut(coastlineWorld, -135),
#'             longitudelim=c(-130, 50), latitudelim=c(70, 110),
#'             proj="+proj=stere +lat_0=90 +lon_0=-135", col='gray')
#'     mtext("Stereographic", adj=1)
#'
#'     # Example 6.
#'     # Spinning globe: create PNG files that can be assembled into a movie
#'\dontrun{
#'     png("globe-%03d.png")
#'     lons <- seq(360, 0, -15)
#'     par(mar=rep(0, 4))
#'     for (i in seq_along(lons)) {
#'         p <- paste("+proj=ortho +lat_0=30 +lon_0=", lons[i], sep="")
#'         if (i == 1) {
#'             mapPlot(coastlineCut(coastlineWorld, lons[i]),
#'                     projection=p, col="lightgray")
#'             xlim <- par("usr")[1:2]
#'             ylim <- par("usr")[3:4]
#'         } else {
#'             mapPlot(coastlineCut(coastlineWorld, lons[i]),
#'                     projection=p, col="lightgray",
#'                     xlim=xlim, ylim=ylim, xaxs="i", yaxs="i")
#'         }
#'     }
#'}
#'}
#
#' @section Available Projections:
#' Map projections are provided by the
#' \CRANpkg{rgdal} package, but not all projections in that package are
#' available. The available list is given in the table
#' below. The cartographic community has set up a naming scheme in a coded
#' scheme, e.g. `projection="+proj=aea"` selects the Albers equal area
#' projection.
#'
#' The allowed projections include those PROJ.4 projections provided by
#' \CRANpkg{rgdal} that have inverses, minus a few that cause problems:
#' `alsk` overdraws `coastlineWorld`, and is a niche projection for Alaska;
#' `calcofi` is not a real projection, but rather a coordinate system;
#' `gs48` overdraws `coastlineWorld`, and is a niche projection for the USA;
#' `gs50` overdraws `coastlineWorld`, and is a niche projection for the USA;
#' `gstmerc` overdraws `coastlineWorld`;
#' `isea` causes segmentation faults on OSX systems;
#' `krovak` overdraws `coastlineWorld`, and is a niche projection for the Czech Republic;
#' `labrd` returns `NaN` for most of the world, and is a niche projection for Madagascar;
#' `lee_os` overdraws `coastlineWorld`;
#' and
#' `nzmg` overdraws `coastlineWorld`.
#'
#'
#' The information in the table is reformatted from the output of the unix
#' command `proj -lP`, where `proj` is provided by version 4.9.0 of
#' the PROJ.4 system. Most of the arguments listed have default values. In
#' addition, most projections can handle arguments `lon_0` and
#' `lat_0`, for shifting the reference point, although in some cases
#' shifting the longitude can yield poor filling of coastlines.
#'
#' Further details of the projections and the controlling arguments are
#' provided at several websites, because PROJ.4 has been incorporated into
#' \CRANpkg{rgdal} and other R packages, plus many other software systems; a good
#' starting point for learning is reference 6.
#'
#' See \dQuote{Examples} for suggested projections for some common
#' applications, and reference 8 for a gallery indicating how to use every projection.
#'
#' \tabular{lll}{
#' **Projection**                       \tab **Code**   \tab **Arguments**\cr
#' Albers equal area                         \tab `aea`      \tab `lat_1`, `lat_2`\cr
#' Azimuthal equidistant                     \tab `aeqd`     \tab `lat_0`, `guam`\cr
#' Aitoff                                    \tab `aitoff`   \tab - \cr
#' Mod. stererographics of Alaska            \tab `alsk`     \tab - \cr
#' Bipolar conic of western hemisphere       \tab `bipc`     \tab - \cr
#' Bonne Werner                              \tab `bonne`    \tab `lat_1`\cr
#' Cassini                                   \tab `cass`     \tab - \cr
#' Central cylindrical                       \tab `cc`       \tab - \cr
#' Equal area cylindrical                    \tab `cea`      \tab `lat_ts`\cr
#' Collignon                                 \tab `collg`    \tab - \cr
#' Craster parabolic Putnins P4              \tab `crast`    \tab - \cr
#' Eckert I                                  \tab `eck1`     \tab - \cr
#' Eckert II                                 \tab `eck2`     \tab - \cr
#' Eckert III                                \tab `eck3`     \tab - \cr
#' Eckert IV                                 \tab `eck4`     \tab - \cr
#' Eckert V                                  \tab `eck5`     \tab - \cr
#' Eckert VI                                 \tab `eck6`     \tab - \cr
#' Equidistant cylindrical plate (Caree)     \tab `eqc`      \tab `lat_ts`, `lat_0`\cr
#' Equidistant conic                         \tab `eqdc`     \tab `lat_1`, `lat_2`\cr
#' Euler                                     \tab `euler`    \tab `lat_1`, `lat_2`\cr
#' Extended transverse Mercator              \tab `etmerc`   \tab `lat_ts`, `lat_0`\cr
#' Fahey                                     \tab `fahey`    \tab - \cr
#' Foucaut                                   \tab `fouc`     \tab - \cr
#' Foucaut sinusoidal                        \tab `fouc_s`   \tab - \cr
#' Gall stereographic                        \tab `gall`     \tab - \cr
#' Geostationary satellite view              \tab `geos`     \tab `h`\cr
#' General sinusoidal series                 \tab `gn_sinu`  \tab `m`, `n`\cr
#' Gnomonic                                  \tab `gnom`     \tab - \cr
#' Goode homolosine                          \tab `goode`    \tab - \cr
## Mod. stererographics of 48 U.S.           \tab `gs48`     \tab - \cr
## Mod. stererographics of 50 U.S.           \tab `gs50`     \tab - \cr
#' Hatano asymmetrical equal area            \tab `hatano`   \tab - \cr
#' HEALPix                                   \tab `healpix`  \tab - \cr
#' rHEALPix                                  \tab `rhealpix` \tab `north_square`, `south_square`\cr
#' Interrupted Goode homolosine              \tab `igh`      \tab -\cr
## Int'l map of the world polyconic          \tab `imw_p`    \tab `lat_1`, `lat_2`, `lon_1`\cr
#' Kavraisky V                               \tab `kav5`     \tab - \cr
#' Kavraisky VII                             \tab `kav7`     \tab - \cr
## Krovak                                    \tab `krovak`   \tab - \cr
#' Lambert azimuthal equal area              \tab `laea`     \tab - \cr
#' Longitude and latitude                    \tab `lonlat`   \tab - \cr
#' Longitude and latitude                    \tab `longlat`   \tab - \cr
#' Longitude and latitude                    \tab `latlon`   \tab - \cr
#' Lambert conformal conic                   \tab `lcc`      \tab `lat_1`, `lat_2`, `lat_0`\cr
#' Lambert equal area conic                  \tab `leac`     \tab `lat_1`, `south`\cr
## Lee oblated stereographic                 \tab `lee_os`   \tab\cr
#' Loximuthal                                \tab `loxim`    \tab\cr
#' Space oblique for Landsat                 \tab `lsat`     \tab `lsat`, `path`\cr
#' McBryde-Thomas flat-polar sine, no. 1     \tab `mbt_s`    \tab\cr
#' McBryde-Thomas flat-polar sine, no. 2     \tab `mbt_fps`  \tab\cr
#' McBryde-Thomas flat-polar parabolic       \tab `mbtfpp`   \tab\cr
#' McBryde-Thomas flat-polar quartic         \tab `mbtfpq`   \tab\cr
#' McBryde-Thomas flat-polar sinusoidal      \tab `mbtfps`   \tab\cr
#' Mercator                                  \tab `merc`     \tab `lat_ts`\cr
#' Miller oblated stereographic              \tab `mil_os`   \tab\cr
#' Miller cylindrical                        \tab `mill`     \tab\cr
#' Mollweide                                 \tab `moll`     \tab\cr
#' Murdoch I                                 \tab `murd1`    \tab `lat_1`, `lat_2`\cr
#' Murdoch II                                \tab `murd2`    \tab `lat_1`, `lat_2`\cr
#' murdoch III                               \tab `murd3`    \tab `lat_1`, `lat_2`\cr
#' Natural earth                             \tab `natearth` \tab\cr
#' Nell                                      \tab `nell`     \tab\cr
#' Nell-Hammer                               \tab `nell_h`   \tab\cr
#' Near-sided perspective                    \tab `nsper`    \tab `h`\cr
#' New Zealand map grid                      \tab `nzmg`     \tab\cr
#' General oblique transformation            \tab `ob_tran`  \tab `o_proj`, `o_lat_p`, `o_lon_p`, `o_alpha`, `o_lon_c`\cr
#'                                           \tab                 \tab `o_lat_c`, `o_lon_1`, `o_lat_1`, `o_lon_2`, `o_lat_2`\cr
#' Oblique cylindrical equal area            \tab `ocea`     \tab `lat_1`, `lat_2`, `lon_1`, `lon_2`\cr
#' Oblated equal area                        \tab `oea`      \tab `n`, `m`, `theta`\cr
#' Oblique Mercator                          \tab `omerc`    \tab `alpha`, `gamma`, `no_off`, `lonc`, `lon_1`,\cr
#'                                           \tab                 \tab `lat_1`, `lon_2`, `lat_2`\cr
#' Orthographic                              \tab `ortho`    \tab - \cr
#' Perspective conic                         \tab `pconic`   \tab `lat_1`, `lat_2`\cr
#' Polyconic American                        \tab `poly`     \tab - \cr
#' Putnins P1                                \tab `putp1`    \tab - \cr
#' Putnins P2                                \tab `putp2`    \tab - \cr
#' Putnins P3                                \tab `putp3`    \tab - \cr
#' Putnins P3'                               \tab `putp3p`   \tab - \cr
#' Putnins P4'                               \tab `putp4p`   \tab - \cr
#' Putnins P5                                \tab `putp5`    \tab - \cr
#' Putnins P5'                               \tab `putp5p`   \tab - \cr
#' Putnins P6                                \tab `putp6`    \tab - \cr
#' Putnins P6'                               \tab `putp6p`   \tab - \cr
#' Quartic authalic                          \tab `qua_aut`  \tab - \cr
#' Quadrilateralized spherical cube          \tab `qsc`      \tab - \cr
#' Robinson                                  \tab `robin`    \tab - \cr
#' Roussilhe stereographic                   \tab `rouss`    \tab - \cr
#' Sinusoidal aka Sanson-Flamsteed           \tab `sinu`     \tab - \cr
#' Swiss. oblique Mercator                   \tab `somerc`   \tab - \cr
#' Stereographic                             \tab `stere`    \tab `lat_ts`\cr
#' Oblique stereographic alternative         \tab `sterea`   \tab - \cr
## Gauss-Schreiber transverse Mercator       \tab `gstmerc`  \tab `lat_0`, `lon_0`, `k_0`\cr
#' Transverse cylindrical equal area         \tab `tcea`     \tab - \cr
#' Tissot                                    \tab `tissot`   \tab `lat_1`, `lat_2`\cr
#' Transverse Mercator                       \tab `tmerc`    \tab - \cr
#' Two point equidistant                     \tab `tpeqd`    \tab `lat_1`, `lon_1`, `lat_2`, `lon_2`\cr
#' Tilted perspective                        \tab `tpers`    \tab `tilt`, `azi`, `h`\cr
#' Universal polar stereographic             \tab `ups`      \tab `south`\cr
#' Urmaev flat-polar sinusoidal              \tab `urmfps`   \tab `n`\cr
#' Universal transverse Mercator             \tab `utm`      \tab `zone`, `south`\cr
#' van der Grinten I                         \tab `vandg`    \tab - \cr
#' Vitkovsky I                               \tab `vitk1`    \tab `lat_1`, `lat_2`\cr
#' Wagner I Kavraisky VI                     \tab `wag1`     \tab - \cr
#' Wagner II                                 \tab `wag2`     \tab - \cr
#' Wagner III                                \tab `wag3`     \tab `lat_ts`\cr
#' Wagner IV                                 \tab `wag4`     \tab - \cr
#' Wagner V                                  \tab `wag5`     \tab - \cr
#' Wagner VI                                 \tab `wag6`     \tab - \cr
#' Werenskiold I                             \tab `weren`    \tab - \cr
#' Winkel I                                  \tab `wink1`    \tab `lat_ts`\cr
#' Winkel Tripel                             \tab `wintri`   \tab `lat_ts`\cr
#' }
#'
#' @section Available ellipse formulations:
#' In the PROJ.4 system of specifying projections, the following ellipse
#' models are available: `MERIT`,
#' `SGS85`, `GRS80`, `IAU76`, `airy`, `APL4.9`,
#' `NWL9D`, `mod_airy`, `andrae`, `aust_SA`, `GRS67`,
#' `bessel`, `bess_nam`, `clrk66`, `clrk80`,
#' `clrk80ign`, `CPM`, `delmbr`, `engelis`,
#' `evrst30`, `evrst48`, `evrst56`, `evrst69`,
#' `evrstSS`, `fschr60`, `fschr60m`, `fschr68`,
#' `helmert`, `hough`, `intl`, `krass`, `kaula`,
#' `lerch`, `mprts`, `new_intl`, `plessis`, `SEasia`,
#' `walbeck`, `WGS60`, `WGS66`, `WGS72`, `WGS84`, and
#' `sphere` (the default).  For example, use \code{projection="+proj=aea
#'     +ellps=WGS84"} for an Albers Equal Area projection using the most
#' recent of the World Geodetic System model. It is unlikely that changing the
#' ellipse will have a visible effect on plotted material at the plot scale
#' appropriate to most oceanographic applications.
#'
#' @section Available datum formulations:
#' In the PROJ.4 system of specifying
#' projections, the following datum formulations are available: `WGS84`,
#' `GGRS87`, `Greek_Geodetic_Reference_System_1987`, `NAD83`,
#' `North_American_Datum_1983`, `NAD27`,
#' `North_American_Datum_1927`, `potsdam`, `Potsdam`,
#' `carthage`, `Carthage`, `hermannskogel`,
#' `Hermannskogel`, `ire65`, `Ireland`, `nzgd49`,
#' `New`, `OSGB36`, and `Airy`. It is unlikely that changing
#' the datum will have a visible effect on plotted material at the plot scale
#' appropriate to most oceanographic applications.
#'
#' @section Choosing a projection:
#' The best choice of projection depends on the application.
#' Readers may find `projection="+proj=moll"` useful for world-wide
#' plots, `ortho` for hemispheres viewed from the equator, `stere`
#' for polar views, `lcc` for wide meridional ranges in mid latitudes,
#' and `merc` in limited-area cases where angle preservation is
#' important.
#'
#' @section Problems:
#' Map projection is a complicated matter that is addressed here
#' in a limited and pragmatic way.  For example, `mapPlot` tries to draw
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
#' There are also systematic problems on i386/windows machines, owing to
#' problems with \CRANpkg{rgdal} on such systems. This explains why
#' [`example`]`("mapPlot")` does not try to create maps on such
#' machines. However, \CRANpkg{rgdal} is in continue development, so it
#' is reasonable to hope that `oce` map projections may start working
#' at some time. As of \CRANpkg{rgdal} version 1.4-3 (in March 2019),
#' however, `mapPlot` does not work on i386/windows
#' machines.
#'
#' @section Changes:
#'
#' * 2019-03-20: the test code provided the \dQuote{Examples} section
#' is disabled on i386/windows machines, on which the requisite
#' \CRANpkg{rgdal} package continues to fail on common projections.
#'
#' * 2017-11-19: `imw_p` removed, because it has problems doing
#' inverse calculations.
#' This is a also problem in the standalone PROJ.4 application version
#' 4.9.3, downloaded and built on OSX.
#' See \url{https://github.com/dankelley/oce/issues/1319} for details.
#'
#' * 2017-11-17: `lsat` removed, because it does not work in
#' \CRANpkg{rgdal} or in the latest standalone PROJ.4 application.
#' This is a also problem in the standalone PROJ.4 application version
#' 4.9.3, downloaded and built on OSX.
#' See \url{https://github.com/dankelley/oce/issues/1337} for details.
#'
#' * 2017-09-30: `lcca` removed, because its inverse was
#' wildly inaccurate in a Pacific Antarctic-Alaska application
#' (see \url{https://github.com/dankelley/oce/issues/1303}).
#'
#'
#' @author Dan Kelley and Clark Richards
#'
#' @references
#'
#' 1. Snyder, John P., 1987.  Map Projections: A Working Manual.  USGS
#' Professional Paper: 1395 (available at
#' \url{https://pubs.er.usgs.gov/publication/pp1395}).
#'
#' 2. Natural Resources Canada
#' \url{https://www.nrcan.gc.ca/earth-sciences/geography/topographic-information/maps/9805}
#'
#' 3. Wikipedia page \url{https://en.wikipedia.org/wiki/List_of_map_projections}
#'
#' 4. Radical Cartography website
#' `http://www.radicalcartography.net/?projectionref` (This URL worked
#' prior to Nov 16, 2016, but was found to fail on that date.)
#'
#' 5. The `PROJ.4` website is \url{http://trac.osgeo.org/proj}, and it is
#' the place to start to learn about the code.
#'
#' 6. `PROJ.4` projection details were once at
#' `http://www.remotesensing.org/geotiff/proj_list/` but it was
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
                    polarCircle=0, lonlabels=TRUE, latlabels=TRUE,
                    projection="+proj=moll", tissot=FALSE, trim=TRUE,
                    debug=getOption("oceDebug"),
                    ...)
{
    dots <- list(...)
    gridOrig <- grid
    if (1 == length(grid))
        grid <- rep(grid, 2)
    if (!missing(projection) && inherits(projection, "CRS")) {
        projection <- projection@projargs
    }
    oceDebug(debug, "mapPlot(longitude, latitude,",
             argShow(longitudelim),
             argShow(latitudelim),
             argShow(type),
             argShow(projection),
             argShow(grid),
             "...) {\n", sep="", unindent=1, style="bold")
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
    oceDebug(debug, "after making it length 2, grid is c(", paste(grid, collapse=","), ")\n", sep="")
    drawGrid <- (is.logical(grid[1]) && grid[1]) || (is.numeric(grid[1]) && grid[1] > 0)
    oceDebug(debug, "drawGrid=", drawGrid, "\n")
    # FIXME: 20150326
    #if (is.logical(grid[1]) && grid[1])
    #    grid <- rep(15, 2)
    #message("000")
    if (nchar(projection) && substr(projection, 1, 1) != "+") {
        stop("use PROJ.4 format, e.g. projection=\"+proj=merc\" for Mercator\n", sep="")
    }
    xy <- lonlat2map(longitude, latitude, projection=projection, debug=debug-1)
    if (!missing(latitudelim) && 0 == diff(latitudelim)) stop("latitudelim must contain two distinct values")
    if (!missing(longitudelim) && 0 == diff(longitudelim)) stop("longitudelim must contain two distinct values")
    limitsGiven <- !missing(latitudelim) && !missing(longitudelim)

    x <- xy$x
    y <- xy$y
    xorig <- xy$x
    yorig <- xy$y
    oce_uhl <- options()$oce_uhl
    if (!is.null(oce_uhl) && oce_uhl == "method 1") {
        message("using test code to remove ugly horiz. lines, since options$oce_uhl=='method 1'")
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
                oceDebug(debug, "limits given (or inferred) near map.R:1427 -- set grid=", paste(grid, collapse=" "), "\n")
            } else {
                usr <- par('usr')
                x0 <- 0.5 * sum(usr[1:2])
                y0 <- 0.5 * sum(usr[3:4])
                ntick <- 8
                dx <- (usr[2] - usr[1]) / ntick
                dy <- (usr[4] - usr[3]) / ntick
                ll <- map2lonlat(x0-dx, y0-dy)
                ur <- map2lonlat(x0+dx, y0+dy)
                ## If ll and ur are finite, the plot covers a fraction of the
                ## globe, and we can compute a grid based on the scale.
                ## Otherwise, assume the earth's shape is just a fraction of
                ## the plot area, meaning we are looking at a globe, and use
                ## a 45 deg grid.
                if (all(is.finite(c(ll$longitude, ll$latitude, ur$longitude, ur$latitude)))) {
                    ##> if (debug) {
                    ##>     cat(vectorShow(ll))
                    ##>     cat(vectorShow(ur))
                    ##> }
                    ls <- geodDist(ll$longitude, ll$latitude, ll$longitude, ur$latitude)
                    rs <- geodDist(ur$longitude, ll$latitude, ur$longitude, ur$latitude)
                    ts <- geodDist(ll$longitude, ur$latitude, ur$longitude, ur$latitude)
                    bs <- geodDist(ll$longitude, ll$latitude, ur$longitude, ll$latitude)
                    t <- median(c(ls, rs, ts, bs)) / 111 # tick, in degrees
                    ##> if (debug)  {
                    ##>     cat(vectorShow(ls))
                    ##>     cat(vectorShow(rs))
                    ##>     cat(vectorShow(ts))
                    ##>     cat(vectorShow(ts))
                    ##> }
                    oceDebug(debug, "t: ", t, "(scale between ticks, in deg)\n")
                    ## message("tickEW: ", tickEW)
                    ## message("tickNS: ", tickNS)
                    ## message("tick: ", tick)
                    if (!is.finite(t)) {
                        grid <- c(5, 5) # may be ok in many instances
                    } else {
                        g <- if (t > 45) 45 else if (t > 10) 15 else if (t > 5) 10 else if (t > 4) 5 else if (t > 2) 1 else pretty(t)[2]
                        grid <- rep(g, 2)
                        oceDebug(debug, "grid=c(", paste(grid, collapse=","), ")\n")
                    }
                } else {
                    grid <- c(45, 45) # perhaps reasonable default, for world view
                }
                if (grid[1] == 0)
                    drawGrid <- FALSE
                oceDebug(debug, "limits not given (or inferred) near map.R:1546 -- set grid=", paste(grid, collapse=" "), "\n")
            }
        }
        if (drawGrid) {
            oceDebug(debug, "about to call mapGrid(), using grid=c(", paste(grid, collapse=","), ")\n")
            mapGrid(dlongitude=grid[1], dlatitude=grid[2], polarCircle=polarCircle,
                    longitudelim=longitudelim, latitudelim=latitudelim, debug=debug-1)
        }
        if (axes) {
            if (is.logical(lonlabels)) {
                mapAxis(side=1, longitude=.axis()$longitude, latitude=FALSE, cex.axis=if (lonlabels) cex.axis else 0, mgp=mgp, debug=debug-1)
            } else if (!is.null(lonlabels)) {
                mapAxis(side=1, longitude=lonlabels, latitude=FALSE, cex.axis=cex.axis, mgp=mgp, debug=debug-1)
            }
            if (is.logical(latlabels)) {
                mapAxis(side=2, latitude=.axis()$latitude, longitude=FALSE, cex.axis=if (latlabels) cex.axis else 0, mgp=mgp, debug=debug-1)
            } else if (!is.null(latlabels)) {
                mapAxis(side=2, latitude=latlabels, longitude=FALSE, cex.axis=cex.axis, mgp=mgp, debug=debug-1)
            }
        }
        if (tissot)
            mapTissot(grid, col='red', debug=debug-1)
        options(warn=options$warn)
    }
    oceDebug(debug, "} # mapPlot()\n", sep="", unindent=1, style="bold")
}


#' Add a Longitude and Latitude Grid to a Map
#'
#' Plot longitude and latitude grid on an existing map.
#'
#' This is somewhat analogous to [grid()], except that the
#' first two arguments of the latter supply the number of lines in the grid,
#' whereas the present function has increments for the first two arguments.
#'
#' @param dlongitude increment in longitude, ignored if `longitude`
#' is supplied, but otherwise determines the longitude sequence.
#'
#' @param dlatitude increment in latitude, ignored if `latitude`
#' is supplied, but otherwise determines the latitude sequence.
#'
#' @param longitude vector of longitudes, or `NULL` to prevent drawing
#' longitude lines.
#'
#' @param latitude vector of latitudes, or `NULL` to prevent drawing
#' latitude lines.
#'
#' @param col color of lines
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
#' small-region plots. This, and `latitudelim`, are both set by
#' [mapPlot()] if the arguments of the same name are passed to
#' that function.
#'
#' @param latitudelim similar to `longitudelim`.
#'
#' @param debug a flag that turns on debugging.  Set to 1 to get a moderate
#' amount of debugging information, 2 to go two function levels deep, or 
#' 3 to go all the way to the core functions. Any value above 3 will be
#' truncated to 3.
#'
#' @section Plans:
#' At the moment, the function cannot determine which lines might
#' work with labels on axes, but this could perhaps be added later, making
#' this more analogous with [grid()].
#'
#' @examples
#'\donttest{
#' library(oce)
#' data(coastlineWorld)
#' mapPlot(coastlineWorld, type='l', grid=FALSE,
#' longitudelim=c(-80, 10), latitudelim=c(0, 120),
#' projection="+proj=ortho")
#' mapGrid(15, 15, polarCircle=15)
#'}
#'
#' @author Dan Kelley
#'
#' @seealso A map must first have been created with [mapPlot()].
#'
#' @family functions related to maps
mapGrid <- function(dlongitude=15, dlatitude=15, longitude, latitude,
                    col="darkgray", lty="solid", lwd=0.5*par("lwd"), polarCircle=0,
                    longitudelim, latitudelim,
                    debug=getOption("oceDebug"))
{
    oceDebug(debug, "mapGrid(",
             argShow(dlongitude), #dlongitude=", dlongitude,
             argShow(dlatitude), # ", dlatitude=", dlatitude,
             argShow(longitude), # ", longitude=", if (missing(longitude)) "(missing)" else "(given)",
             argShow(latitude), # ", latitude=", if (missing(latitude)) "(missing)" else "(given)",
             "...) {\n", unindent=1, sep="", style="bold")
    debug <- min(3, max(0, debug)) # trim to range 0 to 3
    if ("none" == .Projection()$type)
        stop("must create a map first, with mapPlot()\n")
    boxLonLat <- usrLonLat(debug=debug-1)
    if (!missing(longitude) && is.null(longitude) && !missing(latitude) && is.null(latitude))
        return()
    if (!missing(longitudelim))
        longitudelim <- shiftLongitude(longitudelim)
    if (!missing(longitudelim) && !missing(longitude) && !is.null(longitude)) {
        longitudelim <- shiftLongitude(longitudelim)
        oceDebug(debug, "shifted longitudelim to c(",
                 paste(longitudelim, collapse=","), ")\n")
    }
    small <- 0
    if (missing(longitude)) {
        longitude <- if (dlongitude > 0) seq(-180, 180, dlongitude) else NULL
    }
    if (missing(latitude)) {
        latitude <- if (dlatitude > 0) seq(-90+small, 90-small, dlatitude) else NULL
    }
    if (is.null(longitude) && is.null(latitude))
        return()

    ## If a pole is present, we put longitude lines around the world, no matter
    ## what else is true.
    poleInView <- FALSE
    pole <- try(lonlat2map(0, 90), silent=TRUE)
    if (inherits(pole, "try-error")) {
        pole <- try(lonlat2map(0, -90), silent=TRUE)
    }
    if (!inherits(pole, "try-error")) {
        pusr <- par("usr") # don't alter existing
        if (4 == sum(is.finite(pusr)) && is.finite(pole$x) && is.finite(pole$y)) {
            poleInView <- pusr[1] <= pole$x && pole$x <= pusr[2] && pusr[3] <= pole$y && pole$y <= pusr[4]
        }
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
    if (!length(latitude))
        oceDebug(debug, "not drawing latitude lines\n")
    for (l in latitude) {
        ## FIXME: maybe we should use mapLines here
        if (is.finite(l)) {
            if (boxLonLat$ok && !(boxLonLat$latmin <= l & l <= boxLonLat$latmax)) {
                oceDebug(debug, "SKIPPING latitude =", l, "line\n")
                ##if (debug > 1) print(boxLonLat)
                next
            }
            oceDebug(debug, "drawing longitude =", l, "line\n")
            line <- lonlat2map(seq(-180+small, 180-small, length.out=n), rep(l, n))
            x <- line$x
            y <- line$y
            ok <- !is.na(x) & !is.na(y)
            x <- x[ok]
            if (0 == length(x)) next
            y <- y[ok]
            if (0 == length(y)) next
            if (TRUE) {
                ## 20171114: problems e.g. lat lines don't complete (see
                ## 20171114: blog 65-.png) but also problem with a diagonal line
                ## 20171114: (see blog 64.png).
                ##
                ## Remove ugly horizontal lines that can occur for
                ## projections that show the edge of the earth.
                xJump <- abs(diff(x))
                yJump <- abs(diff(y))
                if (any(is.finite(xJump))) {
                    ## FIXME: the number in the next line might need adjustment.
                    xJumpMedian <- median(xJump, na.rm=TRUE)
                    yJumpMedian <- median(yJump, na.rm=TRUE)
                    if (!is.na(xJumpMedian) && !is.na(yJumpMedian)) {
                        bad <- c(FALSE, xJump > 3 * xJumpMedian)
                        bad <- bad | is.na(bad)
                        if (any(bad)) {
                            ##message("lat=", l, ", bad indices:", paste(which(bad), collapse=" "))
                            x[bad] <- NA
                        }
                    }
                }
            }
            lines(x, y, lty=lty, lwd=lwd, col=col)
            ##points(x, y, col=2, cex=1/2)
        }
    }
    if (polarCircle < 0 || polarCircle > 90)
        polarCircle <- 0
    n <- 180                           # number of points on line
    ## If it seems that we are drawing longitude lines for more than 3/4
    ## of the globe, we just draw them all. This can solve odd problems to
    ## do with axis limits.
    if (270 < diff(range(longitude, na.rm=TRUE))) {
        diff <- diff(longitude)[1]
        longitude <- seq(-180, 180, diff)
    }
    for (l in longitude) {
        ## put l in range -180 to 180 for comparison with boxLonLat
        while (l < -180)
            l <- l + 360
        while (l > 180)
            l <- l - 360
        ## FIXME: should use mapLines here
        if (is.finite(l)) {
            if (boxLonLat$ok && !(boxLonLat$lonmin <= l & l <= boxLonLat$lonmax)) {
                oceDebug(debug, "SKIPPING longitude =", l, "line\n")
                ##if (debug > 1) print(boxLonLat)
                next
            }
            oceDebug(debug, "drawing longitude =", l, "line\n")
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
                ##points(x, y, col=3, cex=1/2)
            }
        }
    }
    oceDebug(debug, "} # mapGrid()\n", unindent=1, sep="", style="bold")
}



#' Add a Scalebar to a Map
#'
#' Draw a scalebar on an existing map.
#'
#' The scale is appropriate to the centre of the plot, and will become
#' increasingly inaccurate away from that spot, with the error depending on
#' the projection and the fraction of the earth that is shown.
#'
#' @param x,y position of the scalebar.  Eventually this may be similar to
#'     the corresponding arguments in [legend()], but at the moment
#'     `y` must be `NULL` and `x` must be `"topleft"`.
#'
#' @param length the distance to indicate, in kilometres.  If not provided, a
#'     reasonable choice is made, based on the underlying map.
#'
#' @param lwd line width of the scalebar.
#'
#' @param col color of the scalebar.
#'
#' @param cex character expansion factor for the scalebar text.
#'
#' @examples
#'\donttest{
#' library(oce)
#' data(coastlineWorld)
#' ## Arctic Ocean
#' par(mar=c(2.5, 2.5, 1, 1))
#' mapPlot(coastlineWorld, latitudelim=c(60, 120), longitudelim=c(-130,-50),
#'         col="lightgray", projection="+proj=stere +lat_0=90")
#' mapScalebar()
#'}
#'
#' @author Dan Kelley
#'
#' @seealso A map must first have been created with [mapPlot()].
#'
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
#' Plot text on an existing map, by analogy to [text()].
#'
#' @param longitude vector of longitudes of text to be plotted.
#'
#' @param latitude vector of latitudes of text to be plotted.
#'
#' @param labels vector of labels of text to be plotted.
#'
#' @param ... optional arguments passed to [text()], e.g. `adj`,
#'     `pos`, etc.
#'
#' @examples
#'\donttest{
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
#'}
#'
#' @author Dan Kelley
#'
#' @seealso A map must first have been created with [mapPlot()].
#'
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
#' indicating the distortion inherent in the projection, somewhat
#' analogous to the scheme used in reference 1.
#' (Each ellipse is drawn with 64 segments.)
#'
#' @param grid numeric vector of length 2, specifying the increment in
#' longitude and latitude for the grid. Indicatrices are drawn at e.g.
#' longitudes `seq(-180, 180, grid[1])`.
#'
#' @param scale numerical scale factor for ellipses. This is multiplied by
#' `min(grid)` and the result is the radius of the circle on the
#' earth, in latitude degrees.
#'
#' @param crosshairs logical value indicating whether to draw constant-latitude
#' and constant-longitude crosshairs within the ellipses.  (These are drawn
#' with 10 line segments each.) This can be helpful in cases where it is
#' not desired to use [mapGrid()] to draw the longitude/latitude
#' grid.
#'
#' @param \dots extra arguments passed to plotting functions, e.g.
#' `col="red"` yields red indicatrices.
#'
#' @references
#' 1. Snyder, John P., 1987.  Map Projections: A Working Manual.  USGS
#' Professional Paper: 1395 (available at
#' \url{https://pubs.er.usgs.gov/publication/pp1395}).
#'
#' @examples
#'\donttest{
#' library(oce)
#' data(coastlineWorld)
#' par(mfrow=c(1, 1), mar=c(2, 2, 1, 1))
#' p  <- "+proj=aea +lat_1=10 +lat_2=60 +lon_0=-45"
#' mapPlot(coastlineWorld, projection=p, col="gray",
#' longitudelim=c(-90,0), latitudelim=c(0, 50))
#' mapTissot(c(15, 15), col='red')
#'}
#'
#' @author Dan Kelley
#'
#' @seealso A map must first have been created with [mapPlot()].
#'
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


#' Add Lines to a Map
#'
#' Plot lines on an existing map, by analogy to [lines()].
#'
#' @param longitude vector of longitudes of points to be plotted, or an
#' object from which longitude and latitude can be inferred (e.g. a coastline
#' file, or the return value from [mapLocator()]), in which case the
#' following two arguments are ignored.
#'
#' @param latitude vector of latitudes of points to be plotted.
#'
#' @param greatCircle a logical value indicating whether to render line
#' segments as great circles.  (Ignored.)
#'
#' @param \dots optional arguments passed to [lines()].
#'
#' @examples
#'\donttest{
#' library(oce)
#' data(coastlineWorld)
#' mapPlot(coastlineWorld, type='l',
#'         longitudelim=c(-80, 10), latitudelim=c(0, 120),
#'         projection="+proj=ortho +lon_0=-40")
#' lon <- c(-63.5744, 0.1062)             # Halifax CA to London UK
#' lat <- c(44.6479, 51.5171)
#' mapPoints(lon, lat, col='red')
#' mapLines(lon, lat, col='red')
#'}
#'
#' @author Dan Kelley
#'
#' @seealso A map must first have been created with [mapPlot()].
#'
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
#' Plot points on an existing map, by analogy to [points()].
#'
#' @param longitude Longitudes of points to be plotted, or an object from which
#' longitude and latitude can be inferred in which case the following two
#' arguments are ignored.  This objects that are possible include those of type
#' `coastline`.
#'
#' @param latitude Latitudes of points to be plotted.
#'
#' @param debug A flag that turns on debugging.  Set to 1 to get a moderate amount
#' of debugging information, or to 2 to get more.
#'
#' @param ... Optional arguments passed to [points()].
#'
#' @seealso A map must first have been created with [mapPlot()].
#'
#' @examples
#'\donttest{
#' library(oce)
#' data(coastlineWorld)
#' mapPlot(coastlineWorld, longitudelim=c(-80, 0), latitudelim=c(20, 50),
#'         col="lightgray", projection="+proj=laea +lon_0=-35")
#' data(section)
#' mapPoints(section)
#'}
#'
#' @author Dan Kelley
#'
#' @seealso A map must first have been created with [mapPlot()].
#'
#' @family functions related to maps
mapPoints <- function(longitude, latitude, debug=getOption("oceDebug"), ...)
{
    oceDebug(debug, "mapPoints() {\n", unindent=1, sep="")
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
        oceDebug(debug, "head(longitude)=", paste(head(longitude), collapse=" "), "\n")
        oceDebug(debug, "head(latitude)=", paste(head(latitude), collapse=" "), "\n")
        xy <- lonlat2map(longitude, latitude, debug=debug-1)
        points(xy$x, xy$y, ...)
    }
    oceDebug(debug, "} # mapPoints()\n", unindent=1, sep="")
}

#' Add Arrows to a Map
#'
#' Plot arrows on an existing map, e.g. to indicate a place location.
#' This is not well-suited for drawing direction fields, e.g. of
#' velocities; for that, see [mapDirectionField()].
#' Adds arrows to an existing map, by analogy to [arrows()].
#'
#' @param longitude0,latitude0 starting points for arrows.
#'
#' @param longitude1,latitude1 ending points for arrows.
#'
#' @param length length of the arrow heads, passed to [arrows()].
#'
#' @param angle angle of the arrow heads, passed to [arrows()].
#'
#' @param code numerical code indicating the type of arrows, passed to [arrows()].
#'
#' @param col arrow color, passed to [arrows()].
#'
#' @param lty arrow line type, passed to [arrows()].
#'
#' @param lwd arrow line width, passed to [arrows()].
#'
#' @param ... optional arguments passed to [arrows()].
#'
#' @examples
#'\donttest{
#' library(oce)
#' data(coastlineWorld)
#' mapPlot(coastlineWorld, longitudelim=c(-120, -60), latitudelim=c(30, 60),
#'         col="lightgray", projection="+proj=lcc +lat_1=45 +lon_0=-100")
#' lon <- seq(-120, -75, 15)
#' n <- length(lon)
#' lat <- 45 + rep(0, n)
#' # Draw meridional arrows in N America, from 45N to 60N.
#' mapArrows(lon, lat, lon, lat+15, length=0.05, col="blue")
#'}
#'
#' @author Dan Kelley
#'
#' @seealso A map must first have been created with [mapPlot()].
#'
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
#'
#' @param isLat a boolean that indicates whether the quantity is latitude or
#' longitude
#'
#' @param type a string indicating the type of return value (see below)
#'
#' @param showHemi a boolean that indicates whether to indicate the hemisphere
#'
#' @return A list containing `degrees`, `minutes`, `seconds`,
#' and `hemispheres`, or a vector of strings or (broken) a vector of
#' expressions.
#'
#' @author Dan Kelley
#'
#' @examples
#' library(oce)
#' formatPosition(10+1:10/60+2.8/3600)
#' formatPosition(10+1:10/60+2.8/3600, type="string")
#'
#' @family functions related to maps
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
#' This uses [map2lonlat()] to infer the location in
#' geographical space, so it suffers the same
#' limitations as that function.
#'
#' @param n number of points to locate; see [locator()].
#'
#' @param type type of connector for the points; see [locator()].
#'
#' @param \dots extra arguments passed to [locator()] (and either
#'     [mapPoints()] or [mapLines()], if appropriate) if
#'     `type` is not `'n'`.
#'
#' @author Dan Kelley
#'
#' @seealso A map must first have been created with [mapPlot()].
#'
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
#' A projection must already have been set up, by a call to [mapPlot()]
#' or [lonlat2map()]. It should be noted that not all projections are
#' handled well; see \sQuote{Bugs}.
#'
#' @param x vector containing the x component of points in the projected space, or
#' a list containing items named `x` and `y`, in which case the next
#' argument is ignored.
#'
#' @param y vector containing the y coordinate of points in the projected space
#' (ignored if `x` is a list, as described above).
#'
#' @param init vector containing the initial guesses for longitude and latitude,
#' presently ignored.
#'
#' @section Bugs:
#' `oce` uses [rgdal::project()] in the \CRANpkg{rgdal}
#' package to handle projections. Only those projections that have inverses are
#' permitted within `oce`, and even those can sometimes yield errors, owing
#' to limitations in \CRANpkg{rgdal}. On i386/windows machines, the version
#' of \CRANpkg{rgdal} must be 1.3-9 or higher, to prevent an error with
#' `map2lonlat`.
#'
#' @return
#' A list containing `longitude` and `latitude`, with `NA`
#' values indicating points that are off the globe as displayed.
#'
#' @examples
#' canProject <- .Platform$OS.type!="windows"&&requireNamespace("rgdal")
#' if (canProject) {
#'     library(oce)
#'     ## Cape Split, in the Minas Basin of the Bay of Fundy
#'     cs <- list(longitude=-64.49657, latitude=45.33462)
#'     xy <- lonlat2map(cs, projection="+proj=merc")
#'     map2lonlat(xy)
#' }
#'
#' @seealso [lonlat2map()] does the inverse operation.
#'
#' @author Dan Kelley
#'
#' @seealso A map must first have been created with [mapPlot()].
#'
#' @family functions related to maps
map2lonlat <- function(x, y, init=NULL)
{
    if (missing(x))
        stop("must supply x")
    if ("none" == .Projection()$type)
        stop("must create a map first, with mapPlot()\n")
    if (is.list(x)) {
        y <- x$y
        x <- x$x
    }
    n <- length(x)
    if (n != length(y))
        stop("lengths of x and y must match, but they are ", n, " and ", length(y))
    XY <- oceProject(xy=cbind(x, y), proj=as.character(.Projection()$projection), inv=TRUE)
    list(longitude=XY[, 1], latitude=XY[, 2])
}



#' Add a Polygon to a Map
#'
#' Adds a polygon to an existing map, by analogy to
#' [polygon()].  Used by [mapImage()].
#'
#' @param longitude longitudes of points to be plotted, or an object from
#' which longitude and latitude can be inferred (e.g. a coastline file, or
#' the return value from [mapLocator()]), in which case the
#' following two arguments are ignored.
#'
#' @param latitude latitudes of points to be plotted.
#'
#' @param density as for [polygon()].
#'
#' @param angle as for [polygon()].
#'
#' @param border as for [polygon()].
#'
#' @param col as for [polygon()].
#'
#' @param lty as for [polygon()].
#'
#' @param ... as for [polygon()].
#'
#' @param fillOddEven as for [polygon()].
#'
#' @author Dan Kelley
#'
#' @seealso A map must first have been created with [mapPlot()].
#'
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
#' Plot an image on an existing map that was created with [mapPlot()].
#' (See example 4 for a way to start  with a blank map.)
#'
#' The data are on a regular grid in lon-lat space, but not in the projected
#' x-y space.  This means that [image()] cannot be used.  Instead,
#' there are two approaches, depending on the value of `filledContour`.
#'
#' If `filledContour` is `FALSE`, the image ``pixels'' are with
#' [polygon()], which can be prohibitively slow for fine grids.
#' However, if `filledContour` is `TRUE` or a numerical value, then the
#' ``pixels'' are remapped into a regular grid and then displayed with
#' [.filled.contour()].  The remapping starts by converting the
#' regular lon-lat grid to an irregular x-y grid using
#' [lonlat2map()].  This irregular grid is then interpolated onto a
#' regular x-y grid  with [binMean2D()] or with
#' [akima::interp()] from the `akima` package, as determined by
#' the `gridder` argument.   If `filledContour` is `TRUE`, the
#' dimensions of the regular x-y grid is the same as that of the original
#' lon-lat grid; otherwise, the number of rows and columns are multiplied by
#' the numerical value of `filledContour`, e.g. the value 2 means to make
#' the grid twice as fine.
#'
#' Filling contours can produce aesthetically-pleasing results, but the method
#' involves interpolation, so the data are not represented exactly and
#' analysts are advised to compare the results from the two methods (and
#' perhaps various grid refinement values) to guard against misinterpretation.
#'
#' If a [png()] device is to be used, it is advised to supply
#' arguments `type="cairo"` and `antialias="none"`; see reference 1.
#'
#' @param longitude vector of longitudes corresponding to `z` matrix.
#'
#' @param latitude vector of latitudes corresponding to `z` matrix.
#'
#' @param z matrix to be represented as an image.
#'
#' @param zlim limit for z (color).
#'
#' @param zclip A logical value, `TRUE` indicating that out-of-range
#' `z` values should be painted with `missingColor` and `FALSE`
#' indicating that these values should be painted with the nearest
#' in-range color.  If `zlim` is given then its min and max set the
#' range.  If `zlim` is not given but `breaks` is given, then
#' the min and max of `breaks` sets the range used for z.  If neither
#' `zlim` nor `breaks` is given, clipping is not done, i.e. the
#' action is as if `zclip` were `FALSE`.
#'
#' @param breaks The z values for breaks in the color scheme.  If this is of
#' length 1, the value indicates the desired number of breaks, which is
#' supplied to [pretty()], in determining clean break points.
#'
#' @param col Either a vector of colors corresponding to the breaks, of length
#' 1 plus the number of breaks, or a function specifying colors,
#' e.g. [oce.colorsJet()] for a rainbow.
#'
#' @param colormap optional colormap, as created by [colormap()].
#' If a `colormap` is provided, then its properties takes precedence
#' over `breaks`, `col`, `missingColor`, and `zclip`
#' specified to `mapImage`.
#'
#' @param border Color used for borders of patches (passed to
#' [polygon()]); the default `NA` means no border.
#'
#' @param lwd line width, used if borders are drawn.
#'
#' @param lty line type, used if borders are drawn.
#'
#' @param missingColor a color to be used to indicate missing data, or
#' `NA` to skip the drawing of such regions (which will retain
#' whatever material has already been drawn at the regions).
#'
#' @param filledContour either a logical value indicating whether to use
#' filled contours to plot the image, or a numerical value indicating the
#' resampling rate to be used in interpolating from lon-lat coordinates to
#' x-y coordinates.  See \dQuote{Details} for how this interacts with
#' `gridder`.
#'
#' @param gridder Name of gridding function used if `filledContour` is
#' `TRUE`.  This can be either `"binMean2D"` to select
#' [binMean2D()] or `"interp"` for
#' [akima::interp()]. If not provided, then a selection is made
#' automatically, with [binMean2D()] being used if there are
#' more than 10,000 data points in the present graphical view. This
#' `"binMean2D"` method is much faster than `"interp"`.
#'
#' @param debug A flag that turns on debugging.  Set to 1 to get a
#' moderate amount of debugging information, or to 2 to get more.
#'
#' @references
#' 1. \url{http://codedocean.wordpress.com/2014/02/03/anti-aliasing-and-image-plots/}
#'
#' @examples
#'\donttest{
#' library(oce)
#' data(coastlineWorld)
#' data(topoWorld)
#'
#' ## 1. topography
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
#' ## 2. Northern polar region, with color-coded bathymetry
#' par(mfrow=c(1,1))
#' drawPalette(c(-5000, 0), zlim=c(-5000, 0), col=oce.colorsJet)
#' mapPlot(coastlineWorld, projection="+proj=stere +lat_0=90",
#'         longitudelim=c(-180,180), latitudelim=c(60,120))
#' mapImage(topoWorld, zlim=c(-5000, 0), col=oce.colorsJet)
#' mapLines(coastlineWorld[['longitude']], coastlineWorld[['latitude']])
#'
#' ## 3. Levitus SST
#' par(mfrow=c(1,1))
#' if (requireNamespace("ocedata", quietly=TRUE)) {
#'     data(levitus, package='ocedata')
#'     lon <- levitus$longitude
#'     lat <- levitus$latitude
#'     SST <- levitus$SST
#'     par(mar=rep(1, 4))
#'     Tlim <- c(-2, 30)
#'     drawPalette(Tlim, col=oce.colorsJet)
#'     mapPlot(coastlineWorld, projection="+proj=moll", grid=FALSE)
#'     mapImage(lon, lat, SST, col=oce.colorsJet, zlim=Tlim)
#'     mapPolygon(coastlineWorld, col='gray')
#'}
#'
#' ## 4. Topography without drawing a coastline first
#' data(topoWorld)
#' cm <- colormap(topoWorld[['z']], name='gmt_relief')
#' drawPalette(colormap=cm)
#' mapPlot(c(-180,180), c(-90,90), type="n") # defaults to moll projection
#' mapImage(topoWorld, colormap=cm)
#'}
#'
#' @author Dan Kelley
#'
#' @seealso A map must first have been created with [mapPlot()].
#'
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
            ## calculate 'breaks'
            if (missing(col)) {
                breaks <- pretty(zrange, n=10)
                ## FIXME: the extension of the breaks is to try to avoid missing endpoints
                ##.if (breaks[1] < zrange[1])
                ##.    breaks[1] <- zrange[1] * (1 - small)
                ##.if (breaks[length(breaks)] > zrange[2])
                ##.    breaks[length(breaks)] <- zrange[2] * (1 + small)
                ##. oceDebug(debug, "'breaks', 'zlim' and 'col' all missing; zlim=",
                ##.          paste(zrange, collapse=" "), "\n")
            } else {
                if (is.vector(col)) {
                    breaks <- pretty(zrange, n=1+length(col))
                } else if (is.function(col)) {
                    breaks <- pretty(zrange, n=10)
                } else {
                    stop("'col' must be a vector or a function")
                }
                ##. ## FIXME: cleaner if divide into two cases depending on whether
                ##. ## FIXME: col is a function
                ##. breaks <- seq(zrange[1]-small, zrange[2]+small,
                ##.               length.out=if (is.function(col)) 10 else 1+length(col))
                ##.               ##length.out=if (is.function(col)) 128/4 else 1+length(col))
                ##. breaksSEQ <<- breaks
                ##. if (TRUE) {
### >>>???<<<
                ##.     breaks <- pretty(zrange+small*c(-1, 1), n=10)
                ##.     ## FIXME: the extension of the breaks is to try to avoid missing endpoints
                ##.     ##.if (breaks[1] < zrange[1])
                ##.     ##.    breaks[1] <- zrange[1] * (1 - small)
                ##.     ##.if (breaks[length(breaks)] > zrange[2])
                ##.     ##.    breaks[length(breaks)] <- zrange[2] * (1 + small)
                ##.     breaksPRETTY <<- breaks
                ##.     message("FOR DEBUGGING ISSUE 1340, set options(\"dan\"=1) or =2")
                ##.     danny <- if (!is.null(options("dan"))) options("dan") else 1
                ##.     if (danny==1) {
                ##.         message("BAD using 'seq' breaks (NOTE: small=", small, "); see breaksSEQ global variable, now defined")
                ##.         breaks <- breaksSEQ
                ##.     } else {
                ##.         message("OK  using 'pretty' breaks (NOTE: small=", small, "); see breaksPRETTY global variable, now defined")
                ##.         breaks <- breaksPRETTY
                ##.     }
                ##. }
                ##. oceDebug(debug, "'breaks' and 'zlim' missing but 'col' given; zlim=",
                ##.          paste(zrange, collapse=" "), "\n")
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
    oceDebug(debug, "zclip:", zclip, "\n")
    oceDebug(debug, vectorShow(breaks))
    oceDebug(debug, vectorShow(col))
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
            oceDebug(debug, "'zlim' given, so using those zlim colors for out-of-range values\n")
            zlimMin <- min(zlim, na.rm=TRUE)
            zlimMax <- max(zlim, na.rm=TRUE)
            z[z <= zlimMin] <- zlimMin * (1 + sign(zlimMin) * small)
            z[z >= zlimMax] <- zlimMax * (1 - sign(zlimMax) * small)
        } else if (breaksGiven) {
            oceDebug(debug, "'break' given, but not 'zlim', so possibly extending breaks\n")
            breaksMin <- min(breaks, na.rm=TRUE)
            breaksMax <- max(breaks, na.rm=TRUE)
            oceDebug(debug, "pinning", sum(z<=breaksMin,na.rm=TRUE), "z at left\n")
            oceDebug(debug, "pinning", sum(z>=breaksMax,na.rm=TRUE), "z at right\n")
            z[z <= breaksMin] <- breaksMin * (1 + sign(breaksMin) * small)
            z[z >= breaksMax] <- breaksMax * (1 - sign(breaksMax) * small)
        } else {
            oceDebug(debug, "not clipping AND NEITHER zlim nor breaks suppled\n")
        }
    }
    ## Construct polygons centred on the specified longitudes and latitudes.  Each
    ## polygon has 5 points, four to trace the boundary and a fifth that is (NA,NA),
    ## to signal the end of the polygon.  The z values (and hence the colors)
    ## map one per polygon.
    poly <- .Call("map_assemble_polygons", longitude, latitude, z,
                  NAOK=TRUE, PACKAGE="oce")

    xy <- lonlat2map(poly$longitude, poly$latitude)
    xy$x[!is.finite(xy$x)] <- NA
    xy$y[!is.finite(xy$y)] <- NA
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
            if (!is.finite(zval))
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
        ## NOTE: mapPolygonMethod is NOT documented, so we can assume
        ## NOTE: that it will be NULL, so method=3 will be used.
        method <- options()$mapPolygonMethod
        if (0 == length(method))
            method <- 3 # method tested in issue 1284
        oceDebug(debug, "method=", method, " (set by options()$mapPolygonMethod or default of 3)\n")
        if (method==1) {
            colPolygon <- unlist(lapply(1:(ni*nj), colorLookup))
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
            ## findInterval() requires the 2nd arg to be in order
            ## FIXME: do we need to reorder after the findInterval()?
            ## next is bad because it lengthens col
            ##bad o <- order(breaks)
            ##bad breaks <- breaks[o]
            ##bad col <- col[o]
            ii <- findInterval(Z, breaks, left.open=TRUE, all.inside=TRUE)
            colPolygon <- col[ii]
            colPolygon[!is.finite(Z)] <- missingColor
            if (zclip) {
                colPolygon[Z < min(breaks)] <- missingColor
                colPolygon[Z > max(breaks)] <- missingColor
            }
        } else {
            stop("unknown options(mapPolygonMethod)")
        }
        ##. L <- 10000:20000
        ##. polygon(xy$x[r$okPoint & !r$clippedPoint][L], xy$y[r$okPoint & !r$clippedPoint][L],
        ##.         col=colPolygon[r$okPolygon & !r$clippedPolygon][L],
        ##.         border=colPolygon[r$okPolygon & !r$clippedPolygon][L],
        ##.         lwd=lwd, lty=lty, fillOddEven=FALSE)

        polygon(xy$x[r$okPoint & !r$clippedPoint], xy$y[r$okPoint & !r$clippedPoint],
                col=colPolygon[r$okPolygon & !r$clippedPolygon],
                border=colPolygon[r$okPolygon & !r$clippedPolygon],
                lwd=lwd, lty=lty, fillOddEven=FALSE)


        ## FIXME: 1340 dan1, dan2 comparison:
        ##> all.equal(dan1$xy, dan2$xy)
        ##[1] TRUE
        ##> all.equal(dan1$r, dan2$r)
        ##[1] TRUE
        ##> all.equal(dan1$colPolygon, dan2$colPolygon)
        ##[1] "'is.NA' value mismatch: 1444 in current 11469 in target"
        ##. dan <<- list(xy=xy,
        ##.              r=r,
        ##.              colPolygon=colPolygon,
        ##.              longitude=longitude,
        ##.              latitude=latitude,
        ##.              z=z,
        ##.              breaks=breaks,
        ##.              col=col,
        ##.              ii=ii)
        ##. message("DEBUGGING: defined global var 'dan'")
    }
    oceDebug(debug, "} # mapImage()\n", unindent=1)
    invisible()
}



#' Convert Longitude and Latitude to UTM
#'
#' @param longitude decimal longitude.  May also be a list containing items
#' named `longitude` and `latitude`, in which case the indicated
#' values are used, and next argument is ignored.
#'
#' @param latitude decimal latitude (ignored if `longitude` is a list
#' containing both coordinates)
#'
#' @param zone optional indication of UTM zone.  Normally this is inferred from
#' the longitude, but specifying it can be helpful in dealing with Landsat
#' images, which may cross zones and which therefore are described by a single
#' zone.
#'
#' @param km logical value indicating whether `easting` and
#' `northing` are in kilometers or meters.
#'
#' @return A list containing `easting`, `northing`, `zone` and
#' `hemisphere`.
#'
#' @author Dan Kelley
#'
#' @seealso [utm2lonlat()] does the inverse operation.  For general
#' projections and their inverses, use [lonlat2map()] and
#' [map2lonlat()].
#'
#' @references
#' \url{https://en.wikipedia.org/wiki/Universal_Transverse_Mercator_coordinate_system},
#' downloaded May 31, 2014.
#'
#' @examples
#'\donttest{
#' library(oce)
#' ## Cape Split, in the Minas Basin of the Bay of Fundy
#' lonlat2utm(-64.496567, 45.334626)
#'}
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
    ## Code from https://en.wikipedia.org/wiki/Universal_Transverse_Mercator_coordinate_system
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
#' `km`).  Alternatively, a list containing items named `easting`,
#' `northing`, and `zone`, in which case these are taken from the
#' list and the arguments named `northing`, `zone` and are ignored.
#'
#' @param northing northing coordinate (in km or m, depending on value of
#' `km`).
#'
#' @param zone UTM zone
#'
#' @param hemisphere indication of hemisphere; `"N"` for North, anything
#' else for South.
#'
#' @param km logical value indicating whether `easting` and
#' `northing` are in kilometers or meters.
#'
#' @return A list containing `longitude` and `latitude`.
#'
#' @author Dan Kelley
#'
#' @seealso [lonlat2utm()] does the inverse operation.  For general
#' projections and their inverses, use [lonlat2map()] and
#' [map2lonlat()].
#'
#' @references
#' \url{https://en.wikipedia.org/wiki/Universal_Transverse_Mercator_coordinate_system},
#' downloaded May 31, 2014.
#'
#' @examples
#'\donttest{
#' library(oce)
#' ## Cape Split, in the Minas Basin of the Bay of Fundy
#' utm2lonlat(852863, 5029997, 19)
#'}
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
    ## Code from https://en.wikipedia.org/wiki/Universal_Transverse_Mercator_coordinate_system
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
## 6. lsat seems not to work in rgdal or standlone proj.4, so
##    it was removed from oce on 2017-11-17.
##    See https://github.com/dankelley/oce/issues/1337 for details.
## 7. imw_p can hang on some inverse values, and I found that the
##    problem is deep in the PROJ.4 code (since the error occurs also with PROJ.4
##    compiled a few days before Nov 19th) and therefore imw_p was removed
##    on 2017-11-19 .
##    See https://github.com/dankelley/oce/issues/1319 for details.
knownProj4 <- c("aea", "aeqd", "aitoff",         "bipc", "bonne",
                "cass", "cc", "cea", "collg", "crast", "eck1", "eck2", "eck3",
                "eck4", "eck5", "eck6", "eqc", "eqdc", "euler", "etmerc",
                "fahey", "fouc", "fouc_s", "gall", "geos", "gn_sinu", "gnom",
                "goode",                   "hatano", "healpix", "rhealpix",
                "igh", "kav5", "kav7",
                "laea",   "lonlat", "longlat", "latlon", "lcc", "leac",
                ##"loxim", "lsat", "mbt_s", "mbt_fps", "mbtfpp", "mbtfpq",
                "loxim", "mbt_s", "mbt_fps", "mbtfpp", "mbtfpq",
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
#' If a projection is already being used (e.g. as set by [mapPlot()])
#' then only `longitude` and `latitude` should be given, and the
#' other arguments will be inferred by `lonlat2map`.  This is important
#' because otherwise, if a new projection is called for, it will ruin any
#' additions to the existing plot.
#'
#' @param longitude a vector containing decimal longitudes, or a list
#' containing items named `longitude` and `latitude`, in which case
#' the indicated values are used, and next argument is ignored.
#'
#' @param latitude a vector containing decimal latitude (ignored if
#' `longitude` is a list, as described above).
#'
#' @param projection optional indication of projection.  This must be character
#' string in the format used by the \CRANpkg{rgdal} package;
#' see [mapPlot()].)
#'
#' @template debugTemplate
#'
#' @return A list containing `x` and `y`.
#'
#' @author Dan Kelley
#'
#' @seealso `mapLongitudeLatitudeXY` is a safer alternative, if a map has
#' already been drawn with [mapPlot()], because that function cannot
#' alter an existing projection. [map2lonlat()] is an inverse to
#' `map2lonlat`.
#'
#' @section Bugs:
#' This uses \CRANpkg{rgdal}, and will fail on i386/windows machines unless
#' that package is version 1.3-9 or higher.
#'
#' @examples
#' canProject <- .Platform$OS.type!="windows"&&requireNamespace("rgdal")
#' if (canProject) {
#'     library(oce)
#'     ## Cape Split, in the Minas Basin of the Bay of Fundy
#'     cs <- list(longitude=-64.49657, latitude=45.33462)
#'     xy <- lonlat2map(cs, projection="+proj=merc")
#'     map2lonlat(xy)
#' }
#'
#' @family functions related to maps
lonlat2map <- function(longitude, latitude, projection="", debug=getOption("oceDebug"))
{
    oceDebug(debug, "lonlat2map() {\n", unindent=1, sep="", style="bold")
    if (missing(longitude))
        stop("must supply longitude")
    if (is.list(longitude)) {
        latitude <- longitude$latitude
        longitude <- longitude$longitude
    }
    if (missing(latitude))
        stop("must supply latitude")
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
    n <- length(longitude)
    if (n != length(latitude))
        stop("lengths of longitude and latitude must match, but they are ", n, " and ", length(latitude))
    XY <- oceProject(xy=cbind(longitude, latitude), proj=as.character(projection), inv=FALSE, debug=debug-1)
    .Projection(list(type="proj4", projection=projection))
    oceDebug(debug, "} # lonlat2map()\n", unindent=1, sep="", style="bold")
    list(x=XY[, 1], y=XY[, 2])
}
