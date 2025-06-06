# vim:textwidth=80:expandtab:shiftwidth=4:softtabstop=4

#' Class to Store windrose Data
#'
#' This class stores `windrose` objects, which
#' store statistical information about winds, mainly for
#' plotting as "wind rose" plots with [plot,windrose-method()].
#' Unlike most other [oce-class] objects, there is no reading
#' method for `windrose` objects, because there is no standard way to store
#' wind data in files; instead, [as.windrose()] is provided
#' to construct `windrose` objects.
#'
#' @templateVar class windrose
#'
#' @templateVar dataExample {}
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
#' @family things related to windrose data
setClass("windrose", contains = "oce")

setMethod(
    f = "initialize",
    signature = "windrose",
    definition = function(.Object, ...) {
        .Object <- callNextMethod(.Object, ...)
        .Object@processingLog$time <- presentTime()
        .Object@processingLog$value <- "create 'windrose' object"
        .Object
    }
)

#' Summarize a windrose Object
#'
#' Summarizes some of the data in a [windrose-class] object.
#'
#' @param object A [windrose-class] object.
#'
#' @param ... Further arguments passed to or from other methods.
#'
#' @family things related to windrose data
#'
#' @author Dan Kelley
setMethod(
    f = "summary",
    signature = "windrose",
    definition = function(object, ...) {
        cat("Windrose data\n-------------\n\n")
        n <- length(object@data$theta)
        dtheta <- abs(diff(object@data$theta[1:2]))
        cat("* Have n=", n, "angles, separated by dtheta=", dtheta, "\n\n")
        invisible(callNextMethod()) # summary
    }
)


#' @title Extract Something From a windrose Object
#'
#' @param x a [windrose-class] object.
#'
#' @section Details of the Specialized Method:
#'
#' * If `i` is `"?"`, then the return value is a list
#' containing four items, each of which is a character vector
#' holding the names of things that can be accessed with `[[`.
#' The `data` and `metadata` items hold the names of
#' entries in the object's data and metadata
#' slots, respectively. The `metadataDerived` and
#' `dataDerived` items are both NULL.
#'
#' @template sub_subTemplate
#'
#' @family things related to windrose data
#'
#' @author Dan Kelley
setMethod(
    f = "[[",
    signature(x = "windrose", i = "ANY", j = "ANY"),
    definition = function(x, i, j, ...) {
        if (i == "?") {
            return(list(
                metadata = sort(names(x@metadata)),
                metadataDerived = NULL,
                data = sort(names(x@data)),
                dataDerived = NULL
            ))
        }
        callNextMethod() # [[
    }
)

#' @title Replace Parts of a windrose Object
#'
#' @param x a [windrose-class] object.
#'
#' @template sub_subsetTemplate
#'
#' @family things related to windrose data
setMethod(
    f = "[[<-",
    signature(x = "windrose", i = "ANY", j = "ANY"),
    definition = function(x, i, j, ..., value) {
        callNextMethod(x = x, i = i, j = j, ... = ..., value = value) # [[<-
    }
)


#' Create a windrose Object
#'
#' Create a wind-rose object, typically for plotting with
#' [plot,windrose-method()].
#'
#' @param x The x component of wind speed (or stress) *or* an object of class
#' `met` (see [met-class]), in which case the `u` and
#' `v` components of that object are used for the components of wind speed,
#' and `y` here is ignored.
#'
#' @param y The y component of wind speed (or stress).
#'
#' @param dtheta The angle increment (in degrees) within which to classify the data.
#'
#' @param debug A flag that turns on debugging.  Set to 1 to get a moderate amount
#' of debugging information, or to 2 to get more.
#'
#' @return A [windrose-class] object, with `data` slot containing
#'
#' \tabular{ll}{
#' **Item**   \tab **Meaning**\cr
#'  `n`       \tab the number of `x` values\cr
#'  `x.mean`  \tab the mean of the `x` values\cr
#'  `y.mean`  \tab the mean of the `y` values\cr
#'  `theta`   \tab the central angle (in degrees) for the class\cr
#'  `count`   \tab the number of observations in this class\cr
#'  `mean`    \tab the mean of the observations in this class\cr
#'  `fivenum` \tab the [fivenum()] vector for
#'         observations in this class (the min, the lower hinge, the
#'         median, the upper hinge, and the max)\cr
#' }
#'
#' @examples
#' library(oce)
#' set.seed(1234)
#' theta <- seq(0, 360, 0.25)
#' x <- 1 + cos(pi / 180 * theta) + rnorm(theta)
#' y <- sin(pi / 180 * theta) + rnorm(theta)
#' wr <- as.windrose(x, y)
#' summary(wr)
#'
#' @family things related to windrose data
#'
#' @author Dan Kelley, with considerable help from Alex Deckmyn.
as.windrose <- function(x, y, dtheta = 15.0, debug = getOption("oceDebug")) {
    oceDebug(debug, "as.windrose(x, y, dtheta=", dtheta, ", debug=", debug, ") START\n", sep = "", unindent = 1)
    if (inherits(x, "met")) {
        tmp <- x
        x <- tmp[["u"]]
        y <- tmp[["v"]]
    }
    ok <- !is.na(x) & !is.na(y)
    x <- x[ok]
    y <- y[ok]
    xlen <- length(x)
    pi <- atan2(1, 1) * 4
    dt <- dtheta * pi / 180
    dt2 <- dt / 2
    R <- sqrt(x^2 + y^2)
    angle <- atan2(y, x)
    # L <- max(R, na.rm=TRUE)
    nt <- round(2 * pi / dt)
    count <- mean <- vector("numeric", nt)
    fives <- matrix(0, nt, 5)
    theta <- seq(-pi + dt2, pi - dt2, length.out = nt)
    # The bin-detection code was faulty until 2012-02-07.  This
    # was pointed out by Alex Deckmyn, who also suggested the
    # present solution.  His issue reports, available on
    # github.com/dankelley/oce/issues, are a model of
    # patience and insight.
    ai <- 1 + floor((angle + pi) / dt)
    ai <- (ai - 1) %% nt + 1 # clean up problems (thanks, adeckmyn at github!!)
    if (min(ai) < 1) {
        stop("problem setting up bins (ai<1)")
    }
    if (max(ai) > nt) {
        stop("problem setting up bins (ai>xlen)")
    }
    for (i in 1:nt) {
        inside <- ai == i
        oceDebug(
            debug, sum(inside), "counts for angle category", i,
            "(", round(180 / pi * (theta[i] - dt2), 4), "to",
            round(180 / pi * (theta[i] + dt2), 4), "deg)\n"
        )
        count[i] <- sum(inside)
        mean[i] <- mean(R[inside], na.rm = TRUE)
        fives[i, ] <- fivenum(R[inside])
    }
    if (sum(count) != xlen) {
        stop("miscount in angles")
    }
    res <- new("windrose")
    res@data <- list(
        n = length(x), x.mean = mean(x, na.rm = TRUE), y.mean = mean(y, na.rm = TRUE),
        theta = theta * 180 / pi, count = count, mean = mean, fives = fives
    )
    res@metadata$dtheta <- dtheta
    res@processingLog <- processingLogAppend(res@processingLog, paste(deparse(match.call()), sep = "", collapse = ""))
    oceDebug(debug, "END as.windrose()\n", sep = "", unindent = 1)
    res
}

#' Plot a windrose Object
#'
#' Plot a [windrose-class] object.
#'
#' @param x a [windrose-class] object.
#'
#' @param type The thing to be plotted, either the number of counts in the angle
#' interval, the mean of the values in the interval, the median of the values, or
#' a [fivenum()] representation of the values.
#'
#' @param convention String indicating whether to use meteorological convention or
#' oceanographic convention for the arrows that emanate from the centre of the
#' rose.  In meteorological convection, an arrow emanates towards the right on
#' the diagram if the wind is from the east; in oceanographic convention, such an
#' arrow indicates flow *to* the east.
#'
#' @param mgp Three-element numerical vector to use for [`par`]`(mgp)`, and also
#' for [`par`]`(mar)`, computed from this.  The default is tighter than the R
#' default, in order to use more space for the data and less for the axes.
#'
#' @param mar Four-element numerical vector to be used with [`par`]`("mar")`.
#'
#' @param col Optional list of colors to use.  If not set, the colors will be
#' `c("red", "pink", "blue", "lightgray")`.  For the first three types of
#' plot, the first color in this list is used to fill in the rose, the third is
#' used for the petals of the rose, and the fourth is used for grid lines. For the
#' `"fivenum"` type, the region from the lower hinge to the first quartile is
#' coloured pink, the region from the first quartile to the third quartile
#' is coloured red, and the region from the third quartile to the upper hinge
#' is coloured pink.  Then the median is drawn in black.
#'
#' @template debugTemplate
#'
#' @examples
#' library(oce)
#' set.seed(1234)
#' theta <- seq(0, 360, 0.25)
#' x <- 1 + cos(pi / 180 * theta) + rnorm(theta)
#' y <- sin(pi / 180 * theta) + rnorm(theta)
#' wr <- as.windrose(x, y)
#' plot(wr)
#' plot(wr, type = "fivenum")
#'
#' @author Dan Kelley
#'
#' @family functions that plot oce data
#' @family things related to windrose data
#'
#' @aliases plot.windrose
setMethod(
    f = "plot",
    signature = signature("windrose"),
    definition = function(
        x,
        type = c("count", "mean", "median", "fivenum"),
        convention = c("meteorological", "oceanographic"),
        mgp = getOption("oceMgp"),
        mar = c(mgp[1], mgp[1], 1 + mgp[1], mgp[1]),
        col,
        debug = getOption("oceDebug")) {
        if (!inherits(x, "windrose")) {
            stop("method is only for objects of class '", "windrose", "'")
        }
        oceDebug(debug, "plot.windrose() START\n", sep = "", unindent = 1)
        type <- match.arg(type)
        convention <- match.arg(convention)
        nt <- length(x@data$theta)
        pi <- 4.0 * atan2(1.0, 1.0)
        if (convention == "meteorological") {
            t <- x@data$theta * pi / 180 # in radians
        } else {
            t <- pi + x@data$theta * pi / 180 # in radians
        }
        dt <- t[2] - t[1]
        dt2 <- dt / 2
        # Plot setup
        opar <- par(no.readonly = TRUE)
        on.exit(par(opar))
        par(mgp = mgp, mar = mar)
        plot.new()
        pin <- par("pin")
        xlim <- c(-1.0, 1.0)
        ylim <- c(-1.0, 1.0)
        if (pin[1] > pin[2]) {
            xlim <- (pin[1] / pin[2]) * xlim
        } else {
            ylim <- (pin[2] / pin[1]) * ylim
        }
        plot.window(xlim, ylim, "", asp = 1)
        if (missing(col)) {
            col <- c("red", "pink", "blue", "darkgray")
        } else {
            if (length(col) != 4) {
                stop("'col' should be a list of 4 colors")
            }
        }
        # Draw circle and radii
        tt <- seq(0, 2 * pi, length.out = 100)
        px <- cos(tt)
        py <- sin(tt)
        lines(px, py, col = col[4])
        for (i in 1:nt) {
            lines(c(0, cos(t[i] - dt2)), c(0, sin(t[i] - dt2)), lwd = 0.5, col = col[4])
        }
        text(0, -1, "S", pos = 1)
        text(-1, 0, "W", pos = 2)
        text(0, 1, "N", pos = 3)
        text(1, 0, "E", pos = 4)
        # Draw rose in a given type
        if (type == "count") {
            max <- max(x@data$count, na.rm = TRUE)
            for (i in 1:nt) {
                r <- x@data$count[i] / max
                xlist <- c(0, r * cos(t[i] - dt2), r * cos(t[i] + dt2), 0)
                ylist <- c(0, r * sin(t[i] - dt2), r * sin(t[i] + dt2), 0)
                polygon(xlist, ylist, col = col[1], border = col[3])
            }
            title(paste("Counts (max ", max, ")", sep = ""))
        } else if (type == "mean") {
            max <- max(x@data$mean, na.rm = TRUE)
            for (i in 1:nt) {
                r <- x@data$mean[i] / max
                # cat("t=", t[i], " r=", r, "\n")
                xlist <- c(0, r * cos(t[i] - dt2), r * cos(t[i] + dt2), 0)
                ylist <- c(0, r * sin(t[i] - dt2), r * sin(t[i] + dt2), 0)
                polygon(xlist, ylist, col = col[1], border = col[3])
            }
            title(paste("Means (max ", sprintf(max, fmt = "%.3g"), ")", sep = ""))
        } else if (type == "median") {
            max <- max(x@data$fives[, 5], na.rm = TRUE)
            for (i in 1:nt) {
                r <- x@data$fives[i, 3] / max
                xlist <- c(0, r * cos(t[i] - dt2), r * cos(t[i] + dt2), 0)
                ylist <- c(0, r * sin(t[i] - dt2), r * sin(t[i] + dt2), 0)
                polygon(xlist, ylist, col = col[1], border = col[3])
            }
            title(paste("Medians (max ", sprintf(max, fmt = "%.3g"), ")", sep = ""))
        } else if (type == "fivenum") {
            max <- max(x@data$fives[, 5], na.rm = TRUE)
            # browser()
            for (i in 1:nt) {
                tm <- t[i] - dt2
                tp <- t[i] + dt2
                for (j in 2:5) {
                    r0 <- x@data$fives[i, j - 1] / max
                    r <- x@data$fives[i, j] / max
                    xlist <- c(r0 * cos(tm), r * cos(tm), r * cos(tp), r0 * cos(tp))
                    ylist <- c(r0 * sin(tm), r * sin(tm), r * sin(tp), r0 * sin(tp))
                    thiscol <- col[c(2, 1, 1, 2)][j - 1]
                    polygon(xlist, ylist, col = thiscol, border = col[4])
                }
                # Median in black
                r <- x@data$fives[i, 3] / max
                lines(c(r * cos(tm), r * cos(tp)),
                    c(r * sin(tm), r * sin(tp)),
                    lwd = 2
                )
            }
            title(paste("Fiveum (max ", sprintf(max, fmt = "%.3g"), ")", sep = ""))
        }
        oceDebug(debug, "END plot.windrose()\n", sep = "", unindent = 1)
        invisible(NULL)
    }
)
