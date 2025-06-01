# vim:textwidth=80:expandtab:shiftwidth=4:softtabstop=4

#' Get a Tidal Prediction From a WebTide Database
#'
#' Get a tidal prediction from a WebTide database. This only works if the
#' standalone WebTide application is installed, and if it is installed in a
#' standard location. The details of installation are not within the oce
#' purview.
#'
#' There are two methods of using this function. *Case 1:* `action="map"`. In
#' this case, if `plot` is `FALSE`, a data frame is returned, containing all the
#' `node`s in the selected database, along with all the `latitude`s and
#' `longitude`s. This value is also returned (silently) if `plot` is true, but
#' in that case, a plot is drawn to indicate the node locations. If `latitude`
#' and `longitude` are given, then the node nearest that spot is indicated on
#' the map; otherwise, if `node` is given, then the location of that node is
#' indicated. There is also a special case: if `node` is negative and
#' `interactive()` is `TRUE`, then [locator()] is called, and the node nearest
#' the spot where the user clicks the mouse is indicated in the plot and in the
#' return value.
#'
#' *Case 2:* `action="predict"`. If `plot` is `FALSE`, then a list is returned,
#' indicating `time`, predicted `elevation`, velocity components `u` and `v`,
#' `node` number, the name of the `basedir`, and the `region`. If `plot` is
#' `TRUE`, this list is returned silently, and either a map or a set of three
#' time-series plots (for u, v and water level) is plotted.  (In the
#' second case, users may wish to call `par(mfrow=c(3,1))` first.)
#'
#' @param action An indication of the action, either `action="map"` to
#' draw a map or `action="predict"` to get a prediction; see
#' \dQuote{Details}.
#'
#' @param longitude,latitude optional location at which prediction is required
#' (ignored if `node` is given).
#'
#' @param node optional integer relating to a node in the database. If `node`
#' is given, then neither `latitude` nor `longitude` may be given.
#' If `node` is positive, then specifies indicates the node. If it is negative,
#' [locator()] is called so that the user can click (once) on the map, after
#' which the node is displayed on the map.
#'
#' @param time a vector of times (in the UTC timezone) at which prediction is to
#' be made. If not supplied, this will be the week starting at the present time,
#' computed with [presentTime()], with a 15 minute increment.
#'
#' @param basedir directory containing the `WebTide` application. For example,
#' the author uses `"/usr/local/WebTide"` for this value, because that is where
#' he installed the webtide materials, e.g. "/usr/local/WebTide/data/nwatl"` is
#' where his North Atlantic data files reside.
#'
#' @param region database region, given as a directory name in the WebTide
#' directory.  For example, `h3o` is for Halifax Harbour, `nwatl` is for the
#' northwest Atlantic, and `sshelf` is for the Scotian Shelf and Gulf of Maine.
#'
#' @param plot boolean indicating whether to plot.
#'
#' @param tformat optional argument passed to [oce.plot.ts()], for
#' plot types that call that function.  (See [strptime()] for the
#' format used.)
#'
#' @param pch integer giving the character code (default is 20, for a bullet),
#' used if a map-style plot is requested.
#'
#' @param cex numeric giving the character expansion factor (default is 0.5),
#' used if a map-style plot is requested.
#'
#' @param nodecol colour to be used for dots on a map-style plot.
#'
#' @param landcol colour of land, used in plotting maps.  The default is a
#' semi-transparent tan colour. Set to `"transparent"` to NULL
#' to skip the drawing of land.
#'
#' @template debugTemplate
#'
#' @param \dots optional arguments passed to plotting functions. A common
#' example is to set `xlim` and `ylim`, to focus a map region.
#'
#' @return The value depends on `action`:
#'
#' * If `action="map"` the return value is an indication of the location
#' of a selected node, or (if `node` is NULL) of all nodes.
#'
#' * If `action="predict"`, the return value is a list containing a vector
#' of times (`time`), as well as vectors of the predicted `elevation`
#' in metres and the predicted horizontal components of velocity, `u` and
#' `v`, along with the `node` number, and the `basedir` and
#' `region` as supplied to this function. If `plot` is `FALSE`,
#' this value is returned invisibly.
#'
#' @source The WebTide software may be downloaded for free at the
#' Department of Fisheries and Oceans (Canada) website at
#' `http://www.bio.gc.ca/science/research-recherche/ocean/webtide/index-en.php`
#' (checked February 2016 and May 2017).
#'
#' @section Caution:
#' WebTide is not an open-source application, so the present function was
#' designed based on little more than guesses about the WebTide file structure.
#' Users should be on the lookout for odd results.
#'
#' @section Sample of Usage:
#' \preformatted{
#' # needs WebTide at the system level
#' library(oce)
#' # 1. prediction at Halifax NS
#' longitude <- -63.57
#' latitude <- 44.65
#' prediction <- webtide("predict", longitude=longitude, latitude=latitude)
#' mtext(paste0("prediction at ", latitude, "N and ", longitude, "E"), line=0.75, side=3)
#' # 2. map
#' webtide(lon=-63.57,lat=44.65,xlim=c(-64,-63),ylim=c(43.0,46))
#' }
#'
#' @author Dan Kelley
#'
#' @family things related to tides
webtide <- function(
    action = c("map", "predict"),
    longitude, latitude, node, time,
    basedir = getOption("webtide"),
    region = "nwatl",
    plot = TRUE, tformat, pch = 20, cex = 0.5,
    nodecol = "black",
    landcol = rgb(0.82, 0.71, 0.55, 0.5),
    debug = getOption("oceDebug"), ...) {
    debug <- max(0, min(floor(debug), 2))
    oceDebug(debug, "webtide(action=\"", action, "\", basedir=\"", basedir, "\", ...)\n",
        sep = "", unindent = 1
    )
    rpd <- atan2(1, 1) / 45 # radians per degree
    action <- match.arg(action)
    nodeGiven <- !missing(node)
    longitudeGiven <- !missing(longitude)
    latitudeGiven <- !missing(latitude)
    path <- paste(basedir, "/data/", region, sep = "")
    # 2016-02-03: it seems that there are several possibilities for this filename.
    suffices <- c(".nod", "ll.nod", "_ll.nod")
    nodFiles <- paste(region, suffices, sep = "")
    triangles <- NULL
    for (nodFile in nodFiles) {
        nodFileName <- paste0(path, "/", nodFile)
        oceDebug(debug, "looking for file \"", nodFileName, "\"...\n")
        if (file.exists(nodFileName)) {
            triangles <- read.table(nodFileName, col.names = c("triangle", "longitude", "latitude"), encoding = "latin1")
            oceDebug(debug, "    success -- read information for ", nrow(triangles), " nodes in this file\n")
            break
        }
    }
    if (is.null(triangles)) {
        stop("cannot find WebTide data file; rerun with debug=1 to see the searched list")
    }
    if (action == "map") {
        if (plot) {
            asp <- 1 / cos(rpd * mean(range(triangles$latitude, na.rm = TRUE)))
            par(mar = c(3, 3, 2, 1), mgp = c(2, 0.7, 0))
            plot(triangles$longitude, triangles$latitude,
                pch = pch, cex = cex, col = nodecol, lwd = 1 / 8,
                asp = asp, xlab = "", ylab = "", ...
            )
            # Try for a coastline of well-suite resolution, if we have ocedata installed.
            usr <- par("usr")
            bestcoastline <- coastlineBest(lonRange = usr[1:2], latRange = usr[3:4], debug = debug - 1)
            oceDebug(debug, "coastlineBest() suggests using ", bestcoastline, " as the coastline\n")
            if (bestcoastline == "coastlineWorld") {
                data(list = bestcoastline, package = "oce", envir = environment())
                coastlineWorld <- get("coastlineWorld")
            } else {
                if (requireNamespace("ocedata", quietly = TRUE)) {
                    data(list = bestcoastline, package = "ocedata", envir = environment())
                    oceDebug(debug, "using ", bestcoastline, " from the ocedata package\n")
                    coastlineWorld <- get(bestcoastline)
                } else {
                    data(list = "coastlineWorld", package = "oce", envir = environment())
                    oceDebug(debug, "the ocedata package is not available, so using ", bestcoastline, " from oce\n")
                    coastlineWorld <- get("coastlineWorld")
                }
            }
            if (!is.null(landcol)) {
                polygon(coastlineWorld[["longitude"]], coastlineWorld[["latitude"]], col = landcol, border = landcol)
                # plot the points again, in case a coarse coastline file has
                # obscured some
                points(triangles$longitude, triangles$latitude, pch = pch, cex = cex, col = nodecol, lwd = 1 / 8)
            }
            # use lon and lat, if node not given
            if (!nodeGiven && longitudeGiven && latitudeGiven) {
                closest <- which.min(geodDist(triangles$longitude, triangles$latitude, longitude, latitude))
                node <- triangles$triangle[closest]
            }
            if (nodeGiven && node < 0 && interactive()) {
                point <- locator(1)
                node <- which.min(geodDist(triangles$longitude, triangles$latitude, point$x, point$y))
            }
            if (missing(node)) {
                node <- triangles$number
                longitude <- triangles$longitude
                latitude <- triangles$latitude
            } else {
                if (is.finite(node)) {
                    node <- triangles$triangle[node]
                    longitude <- triangles$longitude[node]
                    latitude <- triangles$latitude[node]
                    points(longitude, latitude, pch = 20, cex = 2, col = "blue")
                    legend("topleft",
                        pch = 20, pt.cex = 2, cex = 3 / 4, col = "blue", bg = "white",
                        legend = sprintf("node %.0f %.4fN %.4fE", node, latitude, longitude) # files hold 4 digits
                    )
                }
            }
            oceDebug(debug, "END webtide() with plotting\n", sep = "", unindent = 1)
            if (is.null(node)) {
                node <- seq_along(latitude)
            }
            return(invisible(data.frame(node = node, latitude = latitude, longitude = longitude)))
        } else {
            node <- triangles$triangle
            longitude <- triangles$longitude
            latitude <- triangles$latitude
            oceDebug(debug, "END webtide() without plotting\n", sep = "", unindent = 1)
            return(list(node = node, latitude = latitude, longitude = longitude))
        }
    } else if (action == "predict") {
        if (missing(time)) {
            time <- seq.POSIXt(from = presentTime(), by = "15 min", length.out = 7 * 4 * 24)
        } # Q: what about timezone?
        if (missing(node)) {
            if (missing(longitude) || missing(latitude)) {
                stop("'longitude' and 'latitude' must be given unless 'node' is given")
            }
            node <- which.min(geodDist(triangles$longitude, triangles$latitude, longitude, latitude))
        } else {
            latitude <- triangles$latitude[node]
            longitude <- triangles$longitude[node]
        }
        oceDebug(debug, latitude, "N ", longitude, "E -- use node ", node,
            " at ", triangles$latitude[node], "N ", triangles$longitude[node], "E\n",
            sep = ""
        )
        constituentse <- dir(path = path, pattern = "*.s2c")
        constituentsuv <- dir(path = path, pattern = "*.v2c")
        nconstituents <- length(constituentse)
        period <- ampe <- phasee <- ampu <- phaseu <- ampv <- phasev <- vector("numeric", length(nconstituents))
        data("tidedata", package = "oce", envir = environment())
        tidedata <- get("tidedata") # ,   pos=globalenv())
        for (i in 1:nconstituents) {
            twoLetter <- substr(constituentse[i], 1, 2)
            C <- which(twoLetter == tidedata$const$name)
            period[i] <- 1 / tidedata$const$freq[C]
            # Elevation file contains one entry per node, starting with e.g.:
            # tri
            # period 23.934470 (hours) first harmonic
            # 260.000000 (days)
            # 1 0.191244 223.820954
            # 2 0.188446 223.141200
            coneFile <- paste(path, constituentse[i], sep = "/")
            cone <- read.table(coneFile, skip = 3, encoding = "latin1")[node, ]
            ampe[i] <- cone[[2]]
            phasee[i] <- cone[[3]]
            conuvFile <- paste(path, constituentsuv[i], sep = "/")
            conuv <- read.table(conuvFile, skip = 3, encoding = "latin1")[node, ]
            ampu[i] <- conuv[[2]]
            phaseu[i] <- conuv[[3]]
            ampv[i] <- conuv[[4]]
            phasev[i] <- conuv[[5]]
            oceDebug(debug, coneFile, sprintf("%s ", twoLetter),
                sprintf("%4.2fh", period[i]),
                sprintf(" (node %d) ", node),
                sprintf("%4.4fm ", ampe[i]), sprintf("%3.3fdeg", phasee[i]), "\n",
                sep = ""
            )
        }
        elevation <- u <- v <- rep(0, length(time))
        # NOTE: tref is the *central time* for tidem()
        tRef <- ISOdate(1899, 12, 31, 12, 0, 0, tz = "UTC")
        h <- (as.numeric(time) - as.numeric(tRef)) / 3600
        tRef <- 3600 * round(mean(as.numeric(time)) / 3600)
        for (i in 1:nconstituents) {
            twoLetter <- substr(constituentse[i], 1, 2)
            C <- which(twoLetter == tidedata$const$name)
            vuf <- tidemVuf(tRef, j = C, latitude = latitude)
            phaseOffset <- (vuf$u + vuf$v) * 360
            # NOTE: phase is *subtracted* here, but *added* in tidem()
            elevation <- elevation + ampe[i] * cos((360 * h / period[i] - phasee[i] + phaseOffset) * rpd)
            #> lines(time, elevation, col=i,lwd=3) # Debug
            u <- u + ampu[i] * cos((360 * h / period[i] - phaseu[i] + phaseOffset) * rpd)
            v <- v + ampv[i] * cos((360 * h / period[i] - phasev[i] + phaseOffset) * rpd)
            oceDebug(debug, sprintf("%s ", twoLetter),
                sprintf("%4.2fh ", period[i]),
                sprintf("%4.4fm ", ampe[i]), sprintf("%3.3fdeg", phasee[i]), "\n",
                sep = ""
            )
        }
        if (plot) {
            oce.plot.ts(time, elevation,
                type = "l", xlab = "", ylab = resizableLabel("elevation"),
                main = sprintf("node %.0f %.3fN %.3fE", node, latitude, longitude),
                tformat = tformat
            )
            abline(h = 0, lty = "dotted", col = "gray")
            oce.plot.ts(time, u,
                type = "l", xlab = "", ylab = resizableLabel("u"),
                drawTimeRange = FALSE, tformat = tformat
            )
            abline(h = 0, lty = "dotted", col = "gray")
            oce.plot.ts(time, v,
                type = "l", xlab = "", ylab = resizableLabel("v"),
                drawTimeRange = FALSE, tformat = tformat
            )
            abline(h = 0, lty = "dotted", col = "gray")
            oceDebug(debug, "END webtide() with plotting\n", sep = "", unindent = 1)
            return(invisible(list(
                time = time, elevation = elevation, u = u, v = v,
                node = node, basedir = basedir, region = region
            )))
        } else {
            oceDebug(debug, "END webtide() without plotting\n", sep = "", unindent = 1)
            return(list(
                time = time, elevation = elevation, u = u, v = v,
                node = node, basedir = basedir, region = region
            ))
        }
    }
}
