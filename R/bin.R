# vim:textwidth=80:expandtab:shiftwidth=4:softtabstop=4

#' Apply a function to vector data
#'
#' The function `FUN` is applied to `f` in bins specified by
#' `xbreaks`.
#'
#' The sub-intervals defined by the `xbreaks` argument are open
#' on the left and closed on the right, to match the behaviour
#' of [cut()].  An open interval does not include points on
#' the boundary, and so any `x` values that exactly match
#' the first `breaks` value will not be counted.  To include
#' such points in the calculation, set `include.lowest` to TRUE.
#'
#' If `FUN` is [mean()], consider using [binMean1D()] instead, which ought
#' to be much faster for large datasets.
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
#' lines(binned$result, binned$xmids, lwd=2, col=rgb(1, 0, 0, 0.9))
#'
#' @author Dan Kelley
#'
#' @family bin-related functions
binApply1D <- function(x, f, xbreaks, FUN, include.lowest=FALSE, ...)
{
    if (missing(x))
        stop("must supply 'x'")
    if (missing(f))
        stop("must supply 'f'")
    if (missing(xbreaks))
        xbreaks <- pretty(x, 20)
    if (missing(FUN))
        stop("must supply 'FUN'")
    if (!is.function(FUN))
        stop("'FUN' must be a function")
    nbreaks <- length(xbreaks)
    if (nbreaks < 2)
        stop("must have at least 2 breaks")
    xmids <- xbreaks[-1L] - 0.5*diff(xbreaks)
    result <- rep(NA_real_, nbreaks - 1L)
    for (i in seq_len(nbreaks - 1L)) {
        look <- if (i == 1L && include.lowest)
            xbreaks[i] <= x & x <= xbreaks[i+1]
        else
            xbreaks[i] < x & x <= xbreaks[i+1]
        if (any(look))
            result[i] <- FUN(f[look])
    }
    list(xbreaks=xbreaks, xmids=xmids, result=result)
}

#' Apply a function to matrix data
#'
#' The function `FUN` is applied to `f` in bins specified by
#' `xbreaks` and `ybreaks`. The bins are open at the left and closed
#' on the right (see [binApply1D()] for an explanation in 1D).
#'
#' If `FUN` is [mean()],
#' consider using [binMean2D()] instead, which may be faster
#' for large datasets.
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
#' @param \dots optional arguments to pass to `FUN`.
#'
#' @return A list with the following elements:
#' `xbreaks` and `ybreaks` as used, mid-points
#' `xmids` and `ymids`, and `result`, a matrix containing the
#' result of applying `FUN()` to the `f` values
#' in the designated bins.
#'
#' @author Dan Kelley
#'
#' @examples
#' library(oce)
#'\donttest{
#' # secchi depths in lat and lon bins
#' if (requireNamespace("ocedata", quietly=TRUE)) {
#'     data(secchi, package="ocedata")
#'     # Note that zlim is provided to the colormap(), to prevent a few
#'     # points from setting a very wide scale.
#'     cm <- colormap(z=secchi$depth, col=oceColorsViridis, zlim=c(0, 15))
#'     par(mar=c(2, 2, 2, 2))
#'     drawPalette(colormap=cm, zlab="Secchi Depth")
#'     data(coastlineWorld)
#'     mapPlot(coastlineWorld, longitudelim=c(-5, 20), latitudelim=c(50, 66),
#'         grid=5, col="gray", projection="+proj=lcc +lat_1=50 +lat_2=65")
#'     bc <- binApply2D(secchi$longitude, secchi$latitude,
#'         pretty(secchi$longitude, 80), pretty(secchi$latitude, 40),
#'         f=secchi$depth, FUN=mean)
#'     mapImage(bc$xmids, bc$ymids, bc$result, zlim=cm$zlim, col=cm$zcol)
#'     mapPolygon(coastlineWorld, col="gray")
#' }
#'}
#'
#' @family bin-related functions
binApply2D <- function(x, y, f, xbreaks, ybreaks, FUN, ...)
{
    if (missing(x))
        stop("must supply 'x'")
    if (missing(y))
        stop("must supply 'y'")
    if (missing(f))
        stop("must supply 'f'")
    nx <- length(x)
    if (nx != length(y))
        stop("lengths of x and y must agree")
    if (missing(xbreaks)) xbreaks <- pretty(x, 20)
    if (missing(ybreaks)) ybreaks <- pretty(y, 20)
    if (missing(FUN))
        stop("must supply 'FUN'")
    if (!is.function(FUN))
        stop("'FUN' must be a function")
    nxbreaks <- length(xbreaks)
    if (nxbreaks < 2)
        stop("must have more than 1 xbreak")
    nybreaks <- length(ybreaks)
    if (nybreaks < 2)
        stop("must have more than 1 ybreak")
    res <- matrix(NA_real_, nrow=nxbreaks-1, ncol=nybreaks-1)
    # this 'method' is just for testing during development. For the data in
    # tests/testthat/test_misc.R, we get the same results for the two
    # methods. Still, I plan to keep this code in here for a while.
    method <- 1
    if (method == 1) {
        # this is 28X faster on the secchi example.
        A <- split(f, cut(y, ybreaks, labels=FALSE))
        B <- split(x, cut(y, ybreaks, labels=FALSE))
        for (i in seq_along(A)) {
            fSplit <- split(A[[i]], cut(B[[i]], xbreaks, labels=FALSE))
            res[as.numeric(names(fSplit)), i] <- unlist(lapply(fSplit, FUN, ...))
        }
        res[!is.finite(res)] <- NA
    } else {
        for (ix in seq.int(1, nxbreaks-1)) {
            for (iy in seq.int(1, nybreaks-1)) {
                keep <- xbreaks[ix] < x & x <= xbreaks[ix+1] & ybreaks[iy] < y & y <= ybreaks[iy+1]
                if (any(keep))
                    res[ix, iy] <- FUN(f[keep], ...)
            }
        }
    }
    list(xbreaks=xbreaks, xmids=xbreaks[-1]-0.5*diff(xbreaks),
        ybreaks=ybreaks, ymids=ybreaks[-1]-0.5*diff(ybreaks),
        result=res)
}


#' Bin-count vector data
#'
#' Count the number of elements of a given vector that fall within
#' successive pairs of values within a second vector.
#'
#' The sub-intervals defined by the `xbreaks` argument are open
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
binCount1D <- function(x, xbreaks, include.lowest=FALSE)
{
    if (missing(x))
        stop("must supply 'x'")
    if (missing(xbreaks))
        xbreaks <- pretty(x)
    nxbreaks <- length(xbreaks)
    if (nxbreaks < 2)
        stop("must have more than 1 break")
    res <- .C("bin_count_1d",
        nx=length(x),
        x=as.double(x),
        nxbreaks=as.integer(nxbreaks),
        xbreaks=as.double(xbreaks),
        include_lowest=as.integer(include.lowest),
        number=integer(nxbreaks-1L),
        NAOK=TRUE, PACKAGE="oce")
    list(xbreaks=xbreaks,
        xmids=xbreaks[-1L] - 0.5*diff(xbreaks),
        number=res$number)
}

#' Bin-average f=f(x)
#'
#' Average the values of a vector `f` in bins defined on another
#' vector `x`. A common example might be averaging CTD profile
#' data into pressure bins (see \dQuote{Examples}).
#'
#' The sub-intervals defined by the `xbreaks` argument are open
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
#' plot(T, z, cex=0.3)
#' TT <- binMean1D(z, T, seq(-100, 0, 1))
#' lines(TT$result, TT$xmids, col=rgb(1, 0, 0, 0.9), lwd=2)
#'
#' @author Dan Kelley
#'
#' @family bin-related functions
binMean1D <- function(x, f, xbreaks, include.lowest=FALSE)
{
    if (missing(x))
        stop("must supply 'x'")
    fGiven <- !missing(f)
    if (!fGiven)
        f <- rep(1, length(x))
    nx <- length(x)
    if (nx != length(f))
        stop("lengths of x and f must agree")
    if (missing(xbreaks))
        xbreaks <- pretty(x)
    nxbreaks <- length(xbreaks)
    if (nxbreaks < 2)
        stop("must have more than 1 break")
    res <- .C("bin_mean_1d",
        nx=length(x),
        x=as.double(x),
        f=as.double(f),
        nxbreaks=length(xbreaks),
        xbreaks=as.double(xbreaks),
        include_lowest=as.integer(include.lowest),
        number=integer(nxbreaks-1),
        result=double(nxbreaks-1),
        NAOK=TRUE, PACKAGE="oce")
    list(xbreaks=xbreaks,
        xmids=xbreaks[-1L] - 0.5*diff(xbreaks),
        number=res$number,
        result=res$result)
}

#' Bin-count matrix data
#'
#' Count the number of elements of a given matrix z=z(x,y) that fall within
#' successive pairs of breaks in x and y.
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
#' @return A list with the following elements: the breaks (`xbreaks`
#' and `ybreaks`), the midpoints (`xmids` and `ymids`)
#' between those breaks, and
#' the count (`number`) of `f` values in the boxes defined
#' between successive breaks.
#'
#' @author Dan Kelley
#'
#' @family bin-related functions
binCount2D <- function(x, y, xbreaks, ybreaks, flatten=FALSE)
{
    if (missing(x))
        stop("must supply 'x'")
    if (missing(y))
        stop("must supply 'y'")
    if (length(x) != length(y))
        stop("lengths of x and y must agree")
    if (missing(xbreaks))
        xbreaks <- pretty(x)
    if (missing(ybreaks))
        ybreaks <- pretty(y)
    nxbreaks <- length(xbreaks)
    if (nxbreaks < 2)
        stop("must have more than 1 xbreak")
    nybreaks <- length(ybreaks)
    if (nybreaks < 2)
        stop("must have more than 1 ybreak")
    M <- .C("bin_count_2d", length(x), as.double(x), as.double(y),
        length(xbreaks), as.double(xbreaks),
        length(ybreaks), as.double(ybreaks),
        number=integer((nxbreaks-1) * (nybreaks-1)),
        mean=double((nxbreaks-1) * (nybreaks-1)),
        NAOK=TRUE, PACKAGE="oce")
    res <- list(xbreaks=xbreaks,
        ybreaks=ybreaks,
        xmids=xbreaks[-1] - 0.5 * diff(xbreaks),
        ymids=ybreaks[-1] - 0.5 * diff(ybreaks),
        number=matrix(M$number, nrow=nxbreaks-1))
    if (flatten) {
        res2 <- list()
        res2$x <- rep(res$xmids, times=nybreaks-1)
        res2$y <- rep(res$ymids, each=nxbreaks-1)
        res2$n <- as.vector(res$number)
        res <- res2
    }
    res
}


#' Bin-average f=f(x,y)
#'
#' Average the values of a vector `f(x,y)` in bins defined on
#' vectors `x` and `y`. A common example might be averaging
#' spatial data into location bins.
#'
#' @param x vector of numerical values.
#'
#' @param y vector of numerical values.
#'
#' @param f Matrix of numerical values, a matrix f=f(x,y).
#'
#' @param xbreaks Vector of values of `x` at the boundaries between bins, calculated using
#' [`pretty`]`(x)` if not supplied.
#'
#' @param ybreaks Vector of values of `y` at the boundaries between bins, calculated using
#' [`pretty`]`(y)` if not supplied.
#'
#' @param flatten A logical value indicating whether
#' the return value also contains equilength
#' vectors `x`, `y`, `z` and `n`, a flattened
#' representation of `xmids`, `ymids`, `result` and
#' `number`.
#'
#' @param fill Logical value indicating whether to fill `NA`-value gaps in
#' the matrix. Gaps will be filled as the average of linear interpolations
#' across rows and columns. See `fillgap`, which works together with this.
#'
#' @param fillgap Integer controlling the size of gap that can be filled
#' across. If this is negative (as in the default), gaps will be filled
#' regardless of their size. If it is positive, then gaps exceeding this
#' number of indices will not be filled.
#'
#' @return A list with the following elements: the midpoints (renamed as
#' `x` and `y`), the count (`number`) of `f(x,y)` values
#' for `x` and `y` values that lie between corresponding breaks,
#' and the resultant average (`f`) of `f(x,y)`, classified by the
#' `x` and `y` breaks.
#'
#' @examples
#' library(oce)
#' x <- runif(500)
#' y <- runif(500)
#' f <- x + y
#' xb <- seq(0, 1, 0.1)
#' yb <- seq(0, 1, 0.2)
#' m <- binMean2D(x, y, f, xb, yb)
#' plot(x, y)
#' contour(m$xmids, m$ymids, m$result, add=TRUE, levels=seq(0, 2, 0.5), labcex=1)
#'
#' @author Dan Kelley
#'
#' @family bin-related functions
binMean2D <- function(x, y, f, xbreaks, ybreaks, flatten=FALSE, fill=FALSE, fillgap=-1)
{
    if (missing(x))
        stop("must supply 'x'")
    if (missing(y))
        stop("must supply 'y'")
    if (fillgap == 0)
        stop("cannot have a negative 'fillgap' value")
    fGiven <- !missing(f)
    if (!fGiven)
        f <- rep(1, length(x))
    if (length(x) != length(y))
        stop("lengths of x and y must agree, but they are ", length(x), " and ", length(y))
    if (length(x) != length(f))
        stop("lengths of x and f must agree, but they are ", length(x), " and ", length(f))
    if (missing(xbreaks))
        xbreaks <- pretty(x)
    if (missing(ybreaks))
        ybreaks <- pretty(y)
    nxbreaks <- length(xbreaks)
    if (nxbreaks < 2)
        stop("must have more than 1 xbreak")
    nybreaks <- length(ybreaks)
    if (nybreaks < 2)
        stop("must have more than 1 ybreak")
    M <- .C("bin_mean_2d", length(x), as.double(x), as.double(y), as.double(f),
        length(xbreaks), as.double(xbreaks),
        length(ybreaks), as.double(ybreaks),
        as.integer(fill), as.integer(fillgap),
        number=integer((nxbreaks-1) * (nybreaks-1)),
        mean=double((nxbreaks-1) * (nybreaks-1)),
        NAOK=TRUE, PACKAGE="oce")
    res <- list(xbreaks=xbreaks,
        ybreaks=ybreaks,
        xmids=xbreaks[-1] - 0.5 * diff(xbreaks),
        ymids=ybreaks[-1] - 0.5 * diff(ybreaks),
        number=matrix(M$number, nrow=nxbreaks-1),
        result=if (fGiven) matrix(M$mean, nrow=nxbreaks-1) else matrix(NA, ncol=nybreaks-1, nrow=nxbreaks-1))
    if (flatten) {
        res2 <- list()
        res2$x <- rep(res$xmids, times=nybreaks-1)
        res2$y <- rep(res$ymids, each=nxbreaks-1)
        res2$f <- as.vector(res$result)
        res2$n <- as.vector(res$number)
        res <- res2
    }
    res
}

#' Bin-average a vector y, based on x values
#'
#' The `y` vector is averaged in bins defined for `x`.  Missing
#' values in `y` are ignored.
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
#' plot(x, y, pch=1)
#' ba <- binAverage(x, y)
#' points(ba$x, ba$y, pch=3, col="red", cex=3)
#'
#' # B. fake quadratic data
#' y <- 1 + x ^2
#' plot(x, y, pch=1)
#' ba <- binAverage(x, y)
#' points(ba$x, ba$y, pch=3, col="red", cex=3)
#'
#' # C. natural data
#' data(co2)
#' plot(co2)
#' avg <- binAverage(time(co2), co2, 1950, 2000, 2)
#' points(avg$x, avg$y, col="red")
#'
#' @family bin-related functions
binAverage <- function(x, y, xmin, xmax, xinc)
{
    if (missing(y))
        stop("must supply 'y'")
    if (missing(xmin))
        xmin <- min(as.numeric(x), na.rm=TRUE)
    if (missing(xmax))
        xmax <- max(as.numeric(x), na.rm=TRUE)
    if (missing(xinc))
        xinc  <- (xmax - xmin) / 10
    if (xmax <= xmin)
        stop("must have xmax > xmin")
    if (xinc <= 0)
        stop("must have xinc > 0")
    xx <- head(seq(xmin, xmax, xinc), -1) + xinc / 2
    #cat("xx:", xx, "\n")
    nb <- length(xx)
    #dyn.load("bin_average.so") # include this whilst debugging
    yy <- .C("bin_average", length(x), as.double(x), as.double(y),
        as.double(xmin), as.double(xmax), as.double(xinc),
        #means=double(nb), NAOK=TRUE)$means
        means=double(nb), NAOK=TRUE, PACKAGE="oce")$means # include this whilst debugging
    list(x=xx, y=yy)
}

