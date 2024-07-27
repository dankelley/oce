# vim:textwidth=80:expandtab:shiftwidth=4:softtabstop=4

#' Apply a Function to Vector Data
#'
#' The function `FUN` is applied to `f` in bins specified by
#' `xbreaks`.  The division of data into bins is done with [cut()].
#'
#' By default, the sub-intervals defined by the `xbreaks` argument are open
#' on the left and closed on the right, to match the behaviour
#' of [cut()].  An open interval does not include points on
#' the boundary, and so any `x` values that exactly match
#' the first `breaks` value will not be counted.  To include
#' such points in the calculation, set `include.lowest` to TRUE.
#'
#' @param x a vector of numerical values.
#'
#' @param f a vector of data to which `FUN` will be applied.
#'
#' @param xbreaks optional vector holding values of x at the boundaries between bins.
#' If this is not given, it is computed by calling [pretty()] with n=20 segments.
#'
#' @param FUN function that is applied to the `f` values
#' in each x bin.  This must take a single numeric vector
#' as input, and return a single numeric value.
#'
#' @param include.lowest logical value indicating whether to include
#' `x` values that equal `xbreaks[1]`.  See \dQuote{Details}.
#'
#' @param \dots optional arguments to pass to `FUN`.
#'
#' @return A list with the following elements: `xbreaks` as
#' used, `xmids` (the mid-points between those breaks) and
#' `result` (the result of applying `FUN` to the `f` values
#' in the designated bins).
#'
#' @examples
#' library(oce)
#' # salinity profile (black) with 1-dbar bin means (red)
#' data(ctd)
#' plotProfile(ctd, "salinity")
#' p <- ctd[["pressure"]]
#' S <- ctd[["salinity"]]
#' pbreaks <- seq(0, max(p), 1)
#' binned <- binApply1D(p, S, pbreaks, mean)
#' lines(binned$result, binned$xmids, lwd = 2, col = rgb(1, 0, 0, 0.9))
#'
#' @author Dan Kelley
#'
#' @family bin-related functions
binApply1D <- function(x, f, xbreaks, FUN, include.lowest = FALSE, ...) {
    if (missing(x)) {
        stop("must supply 'x'")
    }
    if (missing(f)) {
        stop("must supply 'f'")
    }
    if (missing(xbreaks)) {
        xbreaks <- pretty(x, 20)
    }
    if (missing(FUN)) {
        stop("must supply 'FUN'")
    }
    if (!is.function(FUN)) {
        stop("'FUN' must be a function")
    }
    # 2023-06-24 # Stop using C++ for this.
    # 2023-06-24 nbreaks <- length(xbreaks)
    # 2023-06-24 if (nbreaks < 2)
    # 2023-06-24     stop("must have at least 2 breaks")
    # 2023-06-24 xmids <- xbreaks[-1L] - 0.5*diff(xbreaks)
    # 2023-06-24 result <- rep(NA_real_, nbreaks - 1L)
    # 2023-06-24 for (i in seq_len(nbreaks - 1L)) {
    # 2023-06-24     look <- if (i == 1L && include.lowest)
    # 2023-06-24         xbreaks[i] <= x & x <= xbreaks[i+1]
    # 2023-06-24     else
    # 2023-06-24         xbreaks[i] < x & x <= xbreaks[i+1]
    # 2023-06-24     if (any(look))
    # 2023-06-24         result[i] <- FUN(f[look])
    # 2023-06-24 }
    # Use cut, instead of old loopy code ... why re-invent the wheel?
    xmids <- xbreaks[-1L] - 0.5 * diff(xbreaks)
    ysplit <- split(f, cut(x, xbreaks, include.lowest = include.lowest))
    result <- unname(sapply(ysplit, FUN))
    result[!is.finite(result)] <- NA
    list(xbreaks = xbreaks, xmids = xmids, result = result)
} # binApply1D()

#' Apply a Function to Matrix Data
#'
#' The function `FUN` is applied to `f` in bins specified by
#' `xbreaks` and `ybreaks`.
#'
#' The division into bins is done with [cut()], to which `include.lowest`
#' is passed. By default, the `x` bins are open at the left and closed
#' on the right, and the `y` bins are open at the bottom and closed
#' at the top.  However, if `include.lowest` is TRUE, then those
#' boundary points are included in the calculation.
#'
#' @param x a vector of numerical values.
#'
#' @param y a vector of numerical values.
#'
#' @param f a vector of data to which `FUN` will be applied.
#'
#' @param xbreaks values of `x` at the boundaries between the
#' bins; calculated using [pretty()] if not supplied.
#'
#' @param ybreaks as `xbreaks`, but for `y`.
#'
#' @param FUN function that is applied to the `f` values
#' in each (x,y) bin.  This must take two numeric vectors
#' as input, and return a single numeric value.
#'
#' @param include.lowest logical value indicating whether to include
#' `x` values that equal `xbreaks[1]` and `y` values that equal
#' `ybreaks[1]`.  See \dQuote{Details}.
#'
#' @param \dots optional arguments to pass to `FUN`.
#'
#' @return A list with the following elements:
#' `xbreaks` and `ybreaks` as used, mid-points
#' `xmids` and `ymids`, and `result`, a matrix containing the
#' result of applying `FUN()` to the `f` values
#' in the designated bins.
#'
#' @author Dan Kelley
##
## @examples
## library(oce)
## \donttest{
## # secchi depths in lat and lon bins
## if (requireNamespace("ocedata", quietly=TRUE)) {
##     data(secchi, package="ocedata")
##     # Note that zlim is provided to the colormap(), to prevent a few
##     # points from setting a very wide scale.
##     cm <- colormap(z=secchi$depth, col=oceColorsViridis, zlim=c(0, 15))
##     par(mar=c(2, 2, 2, 2))
##     drawPalette(colormap=cm, zlab="Secchi Depth")
##     data(coastlineWorld)
##     mapPlot(coastlineWorld, longitudelim=c(-5, 20), latitudelim=c(50, 66),
##         grid=5, col="gray", projection="+proj=lcc +lat_1=50 +lat_2=65")
##     bc <- binApply2D(secchi$longitude, secchi$latitude,
##         pretty(secchi$longitude, 80), pretty(secchi$latitude, 40),
##         f=secchi$depth, FUN=mean)
##     mapImage(bc$xmids, bc$ymids, bc$result, zlim=cm$zlim, col=cm$zcol)
##     mapPolygon(coastlineWorld, col="gray")
## }
## }
#'
#' @family bin-related functions
binApply2D <- function(x, y, f, xbreaks, ybreaks, FUN, include.lowest = FALSE, ...) {
    if (missing(x)) {
        stop("must supply 'x'")
    }
    if (missing(y)) {
        stop("must supply 'y'")
    }
    if (missing(f)) {
        stop("must supply 'f'")
    }
    nx <- length(x)
    if (nx != length(y)) {
        stop("lengths of x and y must agree")
    }
    if (missing(xbreaks)) {
        xbreaks <- pretty(x, 20)
    }
    if (missing(ybreaks)) {
        ybreaks <- pretty(y, 20)
    }
    if (missing(FUN)) {
        stop("must supply 'FUN'")
    }
    if (!is.function(FUN)) {
        stop("'FUN' must be a function")
    }
    nxbreaks <- length(xbreaks)
    if (nxbreaks < 2) {
        stop("must have more than 1 xbreak")
    }
    nybreaks <- length(ybreaks)
    if (nybreaks < 2) {
        stop("must have more than 1 ybreak")
    }
    res <- matrix(NA_real_, nrow = nxbreaks - 1, ncol = nybreaks - 1)
    ycut <- cut(y, ybreaks, labels = FALSE, include.lowest = include.lowest)
    Fs <- split(f, ycut)
    Xs <- split(x, ycut)
    # cat("next is F before loop\n");print(F)
    # cat("next is X before loop\n");print(X)
    for (iF in seq_along(Fs)) {
        xcut <- cut(Xs[[iF]], xbreaks, labels = FALSE, include.lowest = include.lowest)
        FF <- split(Fs[[iF]], xcut)
        # browser()
        ii <- as.integer(names(FF))
        jj <- as.integer(names(Fs)[[iF]])
        resiijj <- unlist(lapply(FF, FUN, ...))
        # message(vectorShow(dim(res)))
        # message("ii=", paste(ii, collapse=" "), ", jj=", paste(jj, collapse=" "),
        #    ", resiijj=", paste(resiijj, collapse=" "))
        res[ii, jj] <- resiijj
        # message("set res[",
        #    paste(ii, collapse=" "), ", ",
        #    paste(jj, collapse=" "), "] to ",
        #    paste(resiijj, collapse=" "))
    }
    res[!is.finite(res)] <- NA
    list(
        xbreaks = xbreaks, xmids = xbreaks[-1] - 0.5 * diff(xbreaks),
        ybreaks = ybreaks, ymids = ybreaks[-1] - 0.5 * diff(ybreaks),
        result = res
    )
} # binApply2D()


#' Bin-count Vector Data
#'
#' Count the number of elements of a given vector that fall within
#' successive pairs of values within a second vector.
#'
#' By default, the sub-intervals defined by the `xbreaks` argument are open
#' on the left and closed on the right, to match the behaviour
#' of [cut()].  An open interval does not include points on
#' the boundary, and so any `x` values that exactly match
#' the first `breaks` value will not be counted.  To count such
#' points, set `include.lowest` to TRUE.
#'
#' To contextualize `binCount1D()` in terms of base R functions,
#' note that
#' ```
#' binCount1D(1:20, seq(0, 20, 2))$number
#' ```
#' matches
#' ```
#' unname(table(cut(1:20, seq(0, 20, 2))))
#' ```
#'
#' @param x vector of numerical values.
#'
#' @param xbreaks Vector of values of x at the boundaries between bins, calculated using
#' [pretty()] if not supplied.
#'
#' @param include.lowest logical value indicating whether to include
#' `x` values that equal `xbreaks[1]`.  See \dQuote{Details}.
#'
#' @return A list with the following elements: the breaks (`xbreaks`,
#' midpoints (`xmids`) between those breaks, and
#' the count (`number`) of `x` values between successive breaks.
#'
#' @author Dan Kelley
#'
#' @family bin-related functions
binCount1D <- function(x, xbreaks, include.lowest = FALSE) {
    if (missing(x)) {
        stop("must supply 'x'")
    }
    if (missing(xbreaks)) {
        xbreaks <- pretty(x)
    }
    nxbreaks <- length(xbreaks)
    if (nxbreaks < 2) {
        stop("must have more than 1 break")
    }
    # 2023-06-24 # stop using C++ for this.
    # 2023-06-24 res <- .C("bin_count_1d",
    # 2023-06-24     nx=length(x),
    # 2023-06-24     x=as.double(x),
    # 2023-06-24     nxbreaks=as.integer(nxbreaks),
    # 2023-06-24     xbreaks=as.double(xbreaks),
    # 2023-06-24     include_lowest=as.integer(include.lowest),
    # 2023-06-24     number=integer(nxbreaks-1L),
    # 2023-06-24     NAOK=TRUE, PACKAGE="oce")
    C <- cut(x, xbreaks, include.lowest = include.lowest)
    number <- unlist(unname(sapply(split(x, C), length)))
    list(
        xbreaks = xbreaks,
        xmids = xbreaks[-1L] - 0.5 * diff(xbreaks),
        number = number
    )
} # binCount1D


#' Bin-average f=f(x)
#'
#' Average the values of a vector `f` in bins defined on another
#' vector `x`. The values are broken up into bins using [cut()].
#'
#' By default, the sub-intervals defined by the `xbreaks` argument are open
#' on the left and closed on the right, to match the behaviour
#' of [cut()].  An open interval does not include points on
#' the boundary, and so any `x` values that exactly match
#' the first `breaks` value will not be counted.  To include
#' such points in the calculation, set `include.lowest` to TRUE.
#'
#' @param x vector of numerical values that will be categorized into
#' bins via the `xbreaks` parameter.
#'
#' @param f vector of numerical values that are associated with the `x` values.
#'
#' @param xbreaks vector of values of `x` at the boundaries between bins, calculated using
#' [pretty()] if not supplied.
#'
#' @param include.lowest logical value indicating whether to include
#' `x` values that equal `xbreaks[1]`.  See \dQuote{Details}.
#'
#' @param na.rm logical value indicating whether to remove NA values before
#' doing the computation of the average. This is passed to [mean()], which
#' does the work of the present function.
#'
#' @return A list with the following elements: the breaks (`xbreaks`,
#' midpoints (`xmids`) between those breaks,
#' the count (`number`) of `x` values between successive breaks,
#' and the resultant average (`result`) of `f`, classified by the
#' `x` breaks.
#'
#' @examples
#' # Plot raw temperature profile as circles, with lines indicating
#' # the result of averaging in 1-metre depth intervals.
#' library(oce)
#' data(ctd)
#' z <- ctd[["z"]]
#' T <- ctd[["temperature"]]
#' plot(T, z, cex = 0.3)
#' TT <- binMean1D(z, T, seq(-100, 0, 1))
#' lines(TT$result, TT$xmids, col = rgb(1, 0, 0, 0.9), lwd = 2)
#'
#' @author Dan Kelley
#'
#' @family bin-related functions
binMean1D <- function(x, f, xbreaks, include.lowest = FALSE, na.rm = FALSE) {
    if (missing(x)) {
        stop("must supply 'x'")
    }
    fGiven <- !missing(f)
    if (!fGiven) {
        f <- rep(1, length(x))
    }
    nx <- length(x)
    if (nx != length(f)) {
        stop("lengths of x and f must agree")
    }
    if (missing(xbreaks)) {
        xbreaks <- pretty(x)
    }
    nxbreaks <- length(xbreaks)
    if (nxbreaks < 2) {
        stop("must have more than 1 break")
    }
    # 2023-06-24 # Stop using C++ for this.
    # 2023-06-24 res <- .C("bin_mean_1d",
    # 2023-06-24     nx=length(x),
    # 2023-06-24     x=as.double(x),
    # 2023-06-24     f=as.double(f),
    # 2023-06-24     nxbreaks=length(xbreaks),
    # 2023-06-24     xbreaks=as.double(xbreaks),
    # 2023-06-24     include_lowest=as.integer(include.lowest),
    # 2023-06-24     number=integer(nxbreaks-1),
    # 2023-06-24     result=double(nxbreaks-1),
    # 2023-06-24     NAOK=TRUE, PACKAGE="oce")
    if (na.rm) {
        ok <- is.finite(x) & is.finite(f)
        x <- x[ok]
        f <- f[ok]
    }
    C <- cut(x, xbreaks, include.lowest = include.lowest)
    S <- split(f, C)
    number <- unlist(unname(lapply(S, length)))
    result <- unlist(unname(lapply(S, mean)))
    result[!is.finite(result)] <- NA
    list(
        xbreaks = xbreaks,
        xmids = xbreaks[-1L] - 0.5 * diff(xbreaks),
        number = number,
        result = result
    )
} # binMean1D

#' Bin-count Matrix Data
#'
#' Count the number of elements of a given matrix z=z(x,y) that fall within
#' successive pairs of breaks in x and y.
#'
#' By default, the sub-intervals defined by `xbreaks` and `ybreaks`
#' are open on the left/bottom and closed on the right/top, to match
#' the behaviour of [cut()].  An open interval does not include
#' points on the boundary, and so any `x` and `y` values that equal
#' `xbreaks[1]` or `ybreaks[1]` will not be counted.  To include
#' such points in the calculation, set `include.lowest` to TRUE.
#'
#' @param x,y vectors of numerical values.
#'
#' @param xbreaks,ybreaks vector of values of `x` and `y`
#' at the boundaries between the 2D bins, calculated using
#' [pretty()] on each of `x` and `y`, if not supplied.
#'
#' @param flatten A logical value indicating whether
#' the return value also contains equilength
#' vectors `x`, `y`, `z` and `n`, a flattened
#' representation of `xmids`, `ymids`, `result` and
#' `number`.
#'
#' @param include.lowest logical value indicating whether to include
#' points where `x` equals `xbreaks[1]` or `y` equals `ybreaks[1]`.
#'
#' @return A list with the following elements: the breaks (`xbreaks`
#' and `ybreaks`), the midpoints (`xmids` and `ymids`)
#' between those breaks, and
#' the count (`number`) of `f` values in the boxes defined
#' between successive breaks.
#'
#' @author Dan Kelley
#'
#' @family bin-related functions
binCount2D <- function(x, y, xbreaks, ybreaks, flatten = FALSE, include.lowest = FALSE) {
    if (missing(x)) {
        stop("must supply 'x'")
    }
    if (missing(y)) {
        stop("must supply 'y'")
    }
    if (length(x) != length(y)) {
        stop("lengths of x and y must agree")
    }
    if (missing(xbreaks)) {
        xbreaks <- pretty(x)
    }
    if (missing(ybreaks)) {
        ybreaks <- pretty(y)
    }
    nxbreaks <- length(xbreaks)
    if (nxbreaks < 2) {
        stop("must have more than 1 xbreak")
    }
    nybreaks <- length(ybreaks)
    if (nybreaks < 2) {
        stop("must have more than 1 ybreak")
    }
    # 2023-06-24 # Stop using C++ for this.
    # 2023-06-24 M <- .C("bin_count_2d",
    # 2023-06-24     nx=length(x),
    # 2023-06-24     x=as.double(x),
    # 2023-06-24     y=as.double(y),
    # 2023-06-24     nxbreaks=nxbreaks,
    # 2023-06-24     xbreaks=as.double(xbreaks),
    # 2023-06-24     nybreaks=nybreaks,
    # 2023-06-24     ybreaks=as.double(ybreaks),
    # 2023-06-24     include_lowest=as.integer(if (include.lowest) 1L else 0L),
    # 2023-06-24     number=integer((nxbreaks-1) * (nybreaks-1)),
    # 2023-06-24     NAOK=TRUE,
    # 2023-06-24     PACKAGE="oce")
    # 2023-06-24 res <- list(xbreaks=xbreaks,
    # 2023-06-24     ybreaks=ybreaks,
    # 2023-06-24     xmids=xbreaks[-1] - 0.5 * diff(xbreaks),
    # 2023-06-24     ymids=ybreaks[-1] - 0.5 * diff(ybreaks),
    # 2023-06-24     number=matrix(M$number, nrow=nxbreaks-1))
    res <- binApply2D(x, y, rep(1, length(x)), xbreaks, ybreaks, length, include.lowest = include.lowest)
    names(res) <- gsub("result", "number", names(res))
    dim <- dim(res$number)
    res$number <- as.integer(res$number)
    dim(res$number) <- dim
    res$number[is.na(res$number)] <- 0L
    if (flatten) {
        res2 <- list()
        res2$x <- rep(res$xmids, times = nybreaks - 1)
        res2$y <- rep(res$ymids, each = nxbreaks - 1)
        res2$n <- as.vector(res$number)
        res <- res2
    }
    res
} # binCount2D


#' Bin-average f=f(x,y)
#'
#' Average the values of a vector `f(x,y)` in bins defined on vectors
#' `x` and `y`. A common example might be averaging spatial data into
#' location bins.
#'
#' @param x vector of numerical values.
#'
#' @param y vector of numerical values.
#'
#' @param f Matrix of numerical values, a matrix f=f(x,y).
#'
#' @param xbreaks Vector of values of `x` at the boundaries between
#' bins, calculated using [`pretty`]`(x)` if not supplied.
#'
#' @param ybreaks Vector of values of `y` at the boundaries between
#' bins, calculated using [`pretty`]`(y)` if not supplied.
#'
#' @param flatten a logical value indicating whether the return value
#' also contains equilength vectors `x`, `y`, `z` and `n`, a flattened
#' representation of `xmids`, `ymids`, `result` and `number`.
#'
#' @param fill,fillgap values controlling whether to attempt to fill
#' gaps (that is, regions of NA values) in the matrix. If `fill`
#' is false, gaps, or regions with NA values, are not altered.
#' If `fill` is TRUE, then gaps that are of size less than
#' or equal to `fillgap` are interpolated across,
#' by calling [fillGapMatrix()] with the supplied value of
#' `fillgap`.
#'
#' @param include.lowest logical value indicating whether to include
#' `y` values for which the corresponding `x` is equal to `xmin`.
#' See \dQuote{Details}.
#'
#' @param na.rm logical value indicating whether to remove NA values before
#' doing the computation of the average. This is passed to [mean()], which
#' does the work of the present function.
#'
#' @template debugTemplate
#'
#' @return By default, i.e. with `flatten` being FALSE, [binMean2D()]
#' returns a list with the following elements: `xmids`, a vector
#' holding the x-bin midpoints; `ymids`, a vector holding the y-bin
#' midpoints; `number`, a matrix holding the number the points in each
#' bin; and `result`, a matrix holding the mean value in each bin. If
#' `flatten` is TRUE, the `number` and `result` matrices are renamed
#' as `n` and `f` and transformed to vectors, while the bin midpoints
#' are renamed as `x` and `y` and extended to match the length of `n`
#' and `f`.
#'
#' @examples
#' library(oce)
#' x <- runif(500, 0, 0.5)
#' y <- runif(500, 0, 0.5)
#' f <- x^2 + y^2
#' xb <- seq(0, 0.5, 0.1)
#' yb <- seq(0, 0.5, 0.1)
#' m <- binMean2D(x, y, f, xb, yb)
#' cm <- colormap(f, col = oceColorsTurbo)
#' opar <- par(no.readonly = TRUE)
#' drawPalette(colormap = cm)
#' plot(x, y, col = cm$zcol, pch = 20, cex = 1.4)
#' contour(m$xmids, m$ymids, m$result, add = TRUE, labcex = 1.4)
#' par(opar)
#'
#' @author Dan Kelley
#'
#' @family bin-related functions
binMean2D <- function(x, y, f, xbreaks, ybreaks, flatten = FALSE,
                      fill = FALSE, fillgap = -1, include.lowest = FALSE, na.rm = FALSE,
                      debug = getOption("oceDebug")) {
    oceDebug(debug, "binMean2D() START\n", sep = "", unindent = 1)
    if (missing(x)) {
        stop("must supply 'x'")
    }
    if (missing(y)) {
        stop("must supply 'y'")
    }
    if (fillgap == 0) {
        stop("cannot have a negative 'fillgap' value")
    }
    fGiven <- !missing(f)
    if (!fGiven) {
        f <- rep(1, length(x))
    }
    if (length(x) != length(y)) {
        stop("lengths of x and y must agree, but they are ", length(x), " and ", length(y))
    }
    if (length(x) != length(f)) {
        stop("lengths of x and f must agree, but they are ", length(x), " and ", length(f))
    }
    if (missing(xbreaks)) {
        xbreaks <- pretty(x)
    }
    if (missing(ybreaks)) {
        ybreaks <- pretty(y)
    }
    nxbreaks <- length(xbreaks)
    if (nxbreaks < 2) {
        stop("must have more than 1 xbreak")
    }
    nybreaks <- length(ybreaks)
    if (nybreaks < 2) {
        stop("must have more than 1 ybreak")
    }
    resCount <- binCount2D(x = x, y = y, xbreaks = xbreaks, ybreaks = ybreaks, include.lowest = include.lowest)
    resMean <- binApply2D(x = x, y = y, f = f, xbreaks = xbreaks, ybreaks = ybreaks, FUN = mean, include.lowest = include.lowest, na.rm = TRUE)
    # fill gaps (new after issue2199-wip-dropped)
    if (fill) {
        resMean$result <- fillGapMatrix(resMean$result, fillgap = fillgap)
    }
    #issue2199-wip-dropped oceDebug(debug, "calling C code bin_mean_2d\n")
    #issue2199-wip-dropped M <- .C("bin_mean_2d", length(x), as.double(x), as.double(y), as.double(f),
    #issue2199-wip-dropped     length(xbreaks), as.double(xbreaks),
    #issue2199-wip-dropped     length(ybreaks), as.double(ybreaks),
    #issue2199-wip-dropped     as.integer(fill), as.integer(fillgap),
    #issue2199-wip-dropped     number = integer((nxbreaks - 1L) * (nybreaks - 1L)),
    #issue2199-wip-dropped     mean = double((nxbreaks - 1L) * (nybreaks - 1L)),
    #issue2199-wip-dropped     debug = as.integer(debug),
    #issue2199-wip-dropped     NAOK = TRUE, PACKAGE = "oce"
    #issue2199-wip-dropped )
    oceDebug(debug, "setting up return value\n")
    res <- list(
        xbreaks = xbreaks,
        ybreaks = ybreaks,
        xmids = xbreaks[-1] - 0.5 * diff(xbreaks),
        ymids = ybreaks[-1] - 0.5 * diff(ybreaks),
        #issue2199-wip-dropped number = matrix(M$number, nrow = nxbreaks - 1L),
        number = resCount$number,
        result = resMean$result
        #issue2199-wip-dropped if (fGiven) {
        #issue2199-wip-dropped     matrix(M$mean, nrow = nxbreaks - 1)
        #issue2199-wip-dropped } else {
        #issue2199-wip-dropped     matrix(NA, ncol = nybreaks - 1, nrow = nxbreaks - 1)
        #issue2199-wip-dropped }
    )
    if (flatten) {
        oceDebug(debug, "flattening\n")
        res2 <- list()
        res2$x <- rep(res$xmids, times = nybreaks - 1)
        res2$y <- rep(res$ymids, each = nxbreaks - 1)
        res2$f <- as.vector(res$result)
        res2$n <- as.vector(res$number)
        res <- res2
    }
    oceDebug(debug, "END binMean2D()\n", sep = "", unindent = 1)
    res
} # binMean2D

#' Bin-average a Vector y, Based on x Values
#'
#' [binAverage()] works by calling [binMean1D()], after computing
#' the `xbreaks` parameter of the latter function as `seq(xmin,xmax,xinc)`.
#' Note that the return value of [binAverage()] uses only the `xmids` and
#' `result` entries of the [binMean1D()] result.
#'
#' By default, the sub-intervals defined by `xmin`, `xinc` and `xmax`
#' arguments are open on the left and closed on the right, to match
#' the behaviour of [cut()].  An open interval does not include
#' points on the boundary, and so any `x` values that exactly match
#' the first `breaks` value will not be counted.  To include
#' such points in the calculation, set `include.lowest` to TRUE.
#'
#' @param x a vector of numerical values.
#'
#' @param y a vector of numerical values.
#'
#' @param xmin x value at the lower limit of first bin; the minimum `x`
#' will be used if this is not provided.
#'
#' @param xmax x value at the upper limit of last bin; the maximum `x`
#' will be used if this is not provided.
#'
#' @param xinc width of bins, in terms of x value; 1/10th of `xmax-xmin`
#' will be used if this is not provided.
#'
#' @param include.lowest logical value indicating whether to include
#' `y` values for which the corresponding `x` is equal to `xmin`.
#' See \dQuote{Details}.
#'
#' @param na.rm logical value indicating whether to remove NA values before
#' doing the computation of the average. This is passed to [mean()], which
#' does the work of the present function.
#'
#' @return A list with two elements: `x`, the mid-points of the bins, and
#' `y`, the average `y` value in the bins.
#'
#' @author Dan Kelley
#'
#' @examples
#' library(oce)
#' # A. fake linear data
#' x <- seq(0, 100, 1)
#' y <- 1 + 2 * x
#' plot(x, y, pch = 1)
#' ba <- binAverage(x, y)
#' points(ba$x, ba$y, pch = 3, col = "red", cex = 3)
#'
#' # B. fake quadratic data
#' y <- 1 + x^2
#' plot(x, y, pch = 1)
#' ba <- binAverage(x, y)
#' points(ba$x, ba$y, pch = 3, col = "red", cex = 3)
#'
#' # C. natural data
#' data(co2)
#' plot(co2)
#' avg <- binAverage(time(co2), co2, 1950, 2000, 2)
#' points(avg$x, avg$y, col = "red")
#'
#' @family bin-related functions
binAverage <- function(x, y, xmin, xmax, xinc, include.lowest = FALSE, na.rm = FALSE) {
    if (missing(y)) {
        stop("must supply 'y'")
    }
    if (missing(xmin)) {
        xmin <- min(as.numeric(x), na.rm = TRUE)
    }
    if (missing(xmax)) {
        xmax <- max(as.numeric(x), na.rm = TRUE)
    }
    if (missing(xinc)) {
        xinc <- (xmax - xmin) / 10
    }
    if (xmax <= xmin) {
        stop("must have xmax > xmin")
    }
    if (xinc <= 0) {
        stop("must have xinc > 0")
    }
    xbreaks <- seq(xmin, xmax, xinc)
    res <- binMean1D(x, y, xbreaks, include.lowest = include.lowest, na.rm = na.rm)
    list(x = res$xmids, y = res$result)
}
