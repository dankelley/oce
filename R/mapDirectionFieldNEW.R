# vim:textwidth=80:expandtab:shiftwidth=4:softtabstop=4:foldmethod=marker

# vectorized feather computation
feathers <- function(u, step = 10, debug = 0) {
    if (debug) message("initially u=", paste(u, collapse = " "))
    fac <- 5 * step
    pennant <- floor(u / fac)
    if (debug) print(pennant)
    u <- u - fac * pennant
    u <- ifelse(u < 0, 0, u)
    if (debug) message("after pennant, u=", paste(u, collapse = " "))
    fac <- step
    barb <- floor(u / fac)
    u <- u - fac * barb
    u <- ifelse(u < 0, 0, u)
    if (debug) print(barb)
    u <- ifelse(u < 0, 0, u)
    if (debug) message("after barb, u=", paste(u, collapse = " "))
    fac <- step / 2
    demibarb <- floor(u / fac)
    cbind(pennant, barb, demibarb)
}

windBarbs <- function(longitude, latitude, u, v,
                      scale = 5, length = 0.2, step = 10, lwd = 1, col = 1,
                      round10 = FALSE, debug = FALSE) {
    scaleXY <- scale * 111e3 # per metre
    lon0 <- longitude
    lat0 <- latitude
    theta <- 180 / pi * atan2(v, u)
    # By default, not rounding to nearest 10deg, which I've seen suggested,
    # but which seems way too coarse, to my mind.
    if (round10) {
        theta <- 10 * round(theta / 10)
    }
    S <- sinpi(theta / 180)
    C <- cospi(theta / 180)
    lat1 <- lat0 - S * scale
    lon1 <- lon0 - C * scale / cospi(lat0 / 180)
    xy0 <- lonlat2map(as.vector(lon0), as.vector(lat0))
    x0 <- xy0$x
    y0 <- xy0$y
    xy1 <- lonlat2map(as.vector(lon1), as.vector(lat1))
    x1 <- xy1$x
    y1 <- xy1$y
    thetaPage <- 180 / pi * atan2(y1 - y0, x1 - x0)
    phi <- 70 # degrees to rotate barbs (70 by eye)
    barbLwd <- lwd
    barbCol <- col
    flagCol <- col
    D <- length
    barbLength <- D * scaleXY
    triangleWidth <- 2 * barbLength * cospi(phi / 180)
    triangleLength <- 0.5 * triangleWidth / cospi(phi / 180) # or sin?
    barbSpace <- barbLength * cospi(phi / 180)
    ds <- D * cospi(phi / 180)
    if (debug) {
        cat(sprintf(
            "scale: %.4f, barbLength: %.4f, barbSpace: %.4f, triangleWidth: %.4f\ntriangleLength: %.4f\n",
            scale, barbLength, barbSpace, triangleWidth, triangleLength
        ))
    }
    knots <- TRUE
    speed <- sqrt(u^2 + v^2)
    speedOrig <- speed
    speed <- speedOrig * if (knots) 1 else 1.94384 # FIXME: factor likely wrong
    speed <- 5L * as.integer(round(speed / 5))
    Spage <- sinpi(thetaPage / 180)
    Cpage <- cospi(thetaPage / 180)
    if (debug) {
        cat(sprintf("  lat0=%.1f lat1=%.1f\n", lat0, lat1))
        cat(sprintf("  lon0=%.1f lon1=%.1f\n", lon0, lon1))
        cat(sprintf(
            "  theta=%.0f thetaPage=%.0f Spage=%.0f Cpage=%.0f scaleXY=%.0f\n",
            theta, thetaPage, Spage, Cpage, scaleXY
        ))
    }
    x1 <- x0 + scaleXY * Cpage
    y1 <- y0 + scaleXY * Spage
    notStill <- speed != 0
    # points(x0[notStill], y0[notStill])
    segments(x0[notStill], y0[notStill], x1[notStill], y1[notStill], col = barbCol, lwd = barbLwd)
    f <- feathers(speed, step = step, debug = debug)
    angleBarb <- thetaPage - phi
    for (i in seq_along(x0)) {
        fi <- f[i, ]
        j <- sum(fi)
        code <- c(rep(1, fi[["pennant"]]), rep(2, fi[["barb"]]), rep(3, fi[["demibarb"]]))
        lowestNonzero <- identical(as.numeric(fi), c(0, 0, 1))
        stagnant <- speedOrig[i] == 0
        verySlow <- speedOrig[i] < 2.5
        if (debug) {
            cat(sprintf(
                "i=%d, speed=%.2f, theta=%.2f, thetaPage=%.2f, f=%s, verySlow %s, lowestNonzero=%s\n",
                i, speed[i], theta[i], thetaPage[i], paste(f[i, ], collapse = " "),
                if (verySlow) "TRUE" else "FALSE",
                if (lowestNonzero) "TRUE" else "FALSE"
            ))
            if (debug) {
                text(x0[i], y0[i], round(speedOrig[i], 1), pos = 1, cex = 0.7, col = 2, font = 2)
            }
        }
        if (stagnant) {
            points(x0[i], y0[i], col = barbCol, cex = 0.5)
        } else if (verySlow) {
            segments(x0[i], y0[i], x1[i], y1[i], col = barbCol, lwd = barbLwd)
        } else {
            lastCode <- 0 # used to nudge triangles together
            BLC <- barbLength * cospi(angleBarb[i] / 180)
            BLS <- barbLength * sinpi(angleBarb[i] / 180)
            s <- 1 # position of next item (0 at x0, 1 at x1)
            for (jj in seq_len(j)) {
                thisCode <- code[jj]
                x0i <- x0[i]
                y0i <- y0[i]
                x1i <- x1[i]
                y1i <- y1[i]
                delta <- if (thisCode == 1) 2 else if (thisCode == 2) 1.0 else if (thisCode == 3) 0.6
                if (debug) {
                    cat(sprintf(
                        "  jj: %d, code: %d, lastCode:%d, delta: %.3f",
                        jj, code[jj], lastCode, delta
                    ))
                }
                if (thisCode == 1) { # triangle
                    if (debug) {
                        cat("drawing with thisCode=", thisCode, "\n")
                        cat(sprintf(
                            "theta=%.4f, thetaPage=%.4f, barbSpace=%.4f, barbLength=%.4f, triangleLength=%.4f, triangleWidth=%.4f\n",
                            theta[i], thetaPage[i],
                            barbSpace,
                            barbLength,
                            triangleLength,
                            triangleWidth
                        ))
                    }
                    if (lastCode == 1) { # nudge triangles together
                        s <- s + ds
                    }
                    x <- x0i + s * (x1i - x0i)
                    y <- y0i + s * (y1i - y0i)
                    xt <- x + c(
                        0,
                        -triangleLength * cospi((phi + thetaPage[i]) / 180),
                        -triangleWidth * cospi(thetaPage[i] / 180)
                    )
                    yt <- y + c(
                        0,
                        -triangleLength * sinpi((phi + thetaPage[i]) / 180),
                        -triangleWidth * sinpi(thetaPage[i] / 180)
                    )
                    if (debug) {
                        cat(
                            sprintf(
                                "delta=%.3f s=%.3f thetaPage=%.3f scale=%.3f triangleWidth=%.4f triangleLength=%.4f\n",
                                delta, s, thetaPage, scale, triangleWidth, triangleLength
                            )
                        )
                    }
                    polygon(xt, yt, col = flagCol, border = flagCol)
                    s <- s - 3 * ds # (triangleWidth + barbSpace)
                } else if (thisCode == 2 || thisCode == 3) { # barb
                    if (lowestNonzero) {
                        s <- s - ds # barbSpace / length
                    }
                    x <- x0i + s * (x1i - x0i)
                    y <- y0i + s * (y1i - y0i)
                    segments(x, y, x + delta * BLC, y + delta * BLS, col = barbCol, lwd = barbLwd)
                    s <- s - ds
                }
                lastCode <- thisCode
            }
        }
    }
}

# FIXME: if put into oce, document parameters, explain
# plot, give an example, set up familial links with
# sibling functions, etc.
mapDirectionFieldBarbs <- function(
    longitude, latitude, u, v,
    scale = 1, length = 0.2, step = 10, col = par("fg"), lwd = par("lwd"),
    debug = 0, ...) {
    # Check (and flatten location and velocity parameters)
    if (!is.vector(longitude)) {
        stop("longitude must be a vector")
    }
    if (!is.vector(latitude)) {
        stop("latitude must be a vector")
    }
    if (is.matrix(u) || is.matrix(v)) {
        if (!is.matrix(u) || !is.matrix(v)) {
            stop("if either of u or v is a matrix, then both must be")
        }
        if (!identical(dim(u), dim(v))) {
            stop("u and v must have identical dimensions")
        }
        nlon <- length(longitude)
        nlat <- length(latitude)
        if (nlon != nrow(u)) {
            stop("length of longitude must match number of rows in u")
        }
        if (nlat != ncol(u)) {
            stop("length of latitude must match number of columns in u")
        }
        longitude <- matrix(rep(longitude, nlat), nrow = nlon)
        latitude <- matrix(rep(latitude, nlon), byrow = TRUE, nrow = nlon)
        longitude <- as.vector(longitude)
        latitude <- as.vector(latitude)
        u <- as.vector(u)
        v <- as.vector(v)
    }
    longitude <- angleShift(longitude)
    latitude <- angleShift(latitude)
    ok <- is.finite(u) & is.finite(v)
    windBarbs(longitude[ok], latitude[ok],
        u = u[ok], v = v[ok],
        scale = scale, length = length, step = step, debug = debug, col = col, lwd = lwd
    )
}


#' Add a Direction Field to an Existing Map (TRIAL VERSION as of 2024-03-21)
#'
#' Plot a direction field on a existing map, either using arrows,
#' which is the oceanographic convention, or using wind barbs, which
#' is a meteorological convention.
#'
#' As noted in the \dQuote{Description}, there are two styles. 1.
#' *Arrow Style:* arrows are drawn from the stated locations in the
#' direction of the flow defined by the (u,v) vector. This is the
#' usual convention in oceanographic work. 2. *Barb Style:* to create
#' "wind barbs", according to a convention used in meteorological
#' charts. Unlike arrows, which indicate speed by the arrow length,
#' barbs indicate speed by angled lines and possibly triangles located
#' at the upstream end. Note that the meanings of the function
#' parameters vary across the two styles.
#'
#' The "arrow" style is quite common in the oceanographic literature.
#' Arrows point in the direction of the velocity defined by `(u,v)`,
#' and the length of those arrows is proportional to the speed,
#' `sqrt(u^2+v^2)`.
#'
#' By contrast, in the "barb" notation, the lines are of equal length
#' (compared with distance on the ground), with speed being indicated
#' with barbs. Many sources explain the notation, e.g.
#' https://www.weather.gov/hfo/windbarbinfo. The lines extend from the
#' observation longitude and latitude in the direction opposite to the
#' velocity. Velocities are indicated by barbs, i.e. short line
#' segments that extend at an angle to the main line and with pennants
#' (triangles). Speed is given by a combination of pennants and barbs.
#' A pennant represents 50 speed units, a long barb 10 units, and a
#' short barb 5 units.  Summing these values gives the speed, rounded
#' to 5 units.
#'
#' See \dQuote{Details} for a comparison of the "arrow" and "barb"
#' styles for some made-up velocity data.
#'
#' There are two possibilities for how `longitude`, `latitude` are
#' combined with `u` and `v`.
#'
#' 1. All four are vectors, and the matching is one-to-one. This is
#' useful for showing velocities at particular individual locations,
#' as in the \dQuote{Examples}.
#'
#' 2. `longitude` and `latitude` are vectors, while `u` and `v` are
#' matrices.  In this case, the lengths of `longitude` and `latitude`
#' must equal the number of rows and columns in `u` and `v`,
#' respectively.
#'
#' @param longitude,latitude numeric vectors of the starting points
#' for arrows, or the locations of grid cells.
#'
#' @param u,v numeric vectors or matrices holding the components of a
#' vector to be shown as a direction field.
#'
#' @param scale an indication of the length of the arrows or lines.
#' For the "arrow" style, this is arrow length in latitude degrees per
#' unit of velocity.  For the "barb" style, this is the length of all
#' lines, regardless of the velocity, because in this style velocity
#' is indicated with barbs and pennants.
#'
#' @param length an indication of the size of arrow heads, for "arrow"
#' style, or of the barbs, for "barb" style. If not provided, 0.05
#' will be used for the "arrow" style, and 0.2 for the "barb" style.
#'
#' @param code an indication of the style of arrow heads or barbs. For
#' the arrow style, this is a number that is passed to [arrows()],
#' with reasonable values being 0, 1 and 2. For the wind-barb style,
#' this is the string `"barb"`.
#'
#' @param lwd line width of symbols.
#'
#' @param col color of symbols.
#'
#' @template debugTemplate
#'
#' @examples
#' library(oce)
#' data(coastlineWorld)
#' par(mar = rep(2, 4))
#' mapPlot(coastlineWorld,
#'     border = "black",
#'     col = "grey95",
#'     projection = "+proj=lcc +lat_1=40 +lat_2=60 +lon_0=-60",
#'     longitudelim = c(-70, -50), latitudelim = c(45, 50)
#' )
#' # Invent wind data for three locations in eastern Canada
#' dataText <- "
#' lat,lon,u,v,location
#' 44.6476,-63.5728,15,0,Halifax
#' 49.5495,-62.9555,20,20,Anticosti Island
#' 47.5556,-52.7453,0,55,St. John's"
#' data <- read.csv(text = dataText)
#' # Dots at observation locations, for reference
#' mapPoints(data$lon, data$lat)
#' # Red: arrows that extend downwind from the location
#' mapDirectionField(data$lon, data$lat,
#'     u = data$u, v = data$v, scale = 0.05,
#'     length = .08, code = 2, col = 2, lwd = 2
#' )
#' # Blue: barbs that extend upwind from the location
#' mapDirectionFieldNEW(data$lon, data$lat,
#'     u = data$u, v = data$v, scale = 2, code = "barb", lwd = 2, col = 4
#' )
#'
#' @seealso A map must first have been created with [mapPlot()].
#'
#' @family functions related to maps
#'
#' @author Dan Kelley
mapDirectionFieldNEW <- function(
    longitude, latitude, u, v,
    scale = 1, length, code = 2,
    lwd = par("lwd"), col = par("fg"),
    debug = getOption("oceDebug")) {
    if ("none" == .Projection()$type) {
        stop("must create a map first, with mapPlot()\n")
    }
    # Check (and flatten location and velocity parameters)
    if (!is.vector(longitude)) {
        stop("longitude must be a vector")
    }
    if (!is.vector(latitude)) {
        stop("latitude must be a vector")
    }
    if (is.matrix(u) || is.matrix(v)) {
        if (!is.matrix(u) || !is.matrix(v)) {
            stop("if either of u or v is a matrix, then both must be")
        }
        if (!identical(dim(u), dim(v))) {
            stop("u and v must have identical dimensions")
        }
        nlon <- length(longitude)
        nlat <- length(latitude)
        if (nlon != nrow(u)) {
            stop("length of longitude must match number of rows in u")
        }
        if (nlat != ncol(u)) {
            stop("length of latitude must match number of columns in u")
        }
        longitude <- matrix(rep(longitude, nlat), nrow = nlon)
        latitude <- matrix(rep(latitude, nlon), byrow = TRUE, nrow = nlon)
        longitude <- as.vector(longitude)
        latitude <- as.vector(latitude)
        u <- as.vector(u)
        v <- as.vector(v)
    }
    # Handle "barb" case
    if (grep("barb", code)) {
        oceDebug(debug, "handing control to mapDirectionFieldBarbs()\n")
        mapDirectionFieldBarbs(longitude, latitude, u, v,
            scale = scale,
            length = if (missing(length)) 0.2 else length,
            lwd = lwd, col = col, step = 10, debug = debug
        )
    } else {
        # Handle "arrow" case
        if (missing(length)) {
            length <- 0.05
        }
        xy <- lonlat2map(as.vector(longitude), as.vector(latitude))
        # Calculate lon-lat at ends of barb shafts
        scalex <- scale / cos(pi * latitude / 180)
        latEnd <- latitude + v * scale
        lonEnd <- longitude + u * scalex
        longitude <- angleShift(longitude)
        latitude <- angleShift(latitude)
        lonEnd <- angleShift(lonEnd)
        latEnd <- angleShift(latEnd)
        xy <- lonlat2map(as.vector(longitude), as.vector(latitude))
        xyEnd <- lonlat2map(as.vector(lonEnd), as.vector(latEnd))
        arrows(xy$x, xy$y, xyEnd$x, xyEnd$y, length = length, code = code, col = col)
    }
}
