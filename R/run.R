#' Calculate running linear models
#'
#' The linear model is calculated from the slope of a localized least-squares
#' regression model y=y(x).  The localization is defined by the x difference
#' from the point in question, with data at distance exceeding L/2 being
#' ignored.  With a \code{boxcar} window, all data within the local domain are
#' treated equally, while with a \code{hanning} window, a raised-cosine
#' weighting function is used; the latter produces smoother derivatives, which
#' can be useful for noisy data.  The function is based on internal
#' calculation, not on \code{\link{lm}}.
#'
#' @param x a vector holding x values.
#' @param y a vector holding y values.
#' @param xout optional vector of x values at which the derivative is to be
#' found.  If not provided, \code{x} is used.
#' @param window type of weighting function used to weight data within the
#' window; see \sQuote{Details}.
#' @param L width of running window, in x units.  If not provided, a reasonable
#' default will be used.
#' @param deriv an optional indicator of the desired return value; see
#' \sQuote{Examples}.
#' @return If \code{deriv} is not specified, a list containing vectors of
#' output values \code{y} and \code{y}, derivative (\code{dydx}), along with
#' the scalar length scale \code{L}.  If \code{deriv=0}, a vector of values is
#' returned, and if \code{deriv=1}, a vector of derivatives is returned.
#' @author Dan Kelley
#' @examples
#'
#' library(oce)
#'
#' # Case 1: smooth a noisy signal
#' x <- 1:100
#' y <- 1 + x/100 + sin(x/5)
#' yn <- y + rnorm(100, sd=0.1)
#' L <- 4
#' calc <- runlm(x, y, L=L)
#' plot(x, y, type='l', lwd=7, col='gray')
#' points(x, yn, pch=20, col='blue')
#' lines(x, calc$y, lwd=2, col='red')
#'
#' # Case 2: square of buoyancy frequency
#' data(ctd)
#' par(mfrow=c(1,1))
#' plot(ctd, which="N2")
#' rho <- swRho(ctd)
#' z <- swZ(ctd)
#' zz <- seq(min(z), max(z), 0.1)
#' N2 <- -9.8 / mean(rho) * runlm(z, rho, zz, deriv=1)
#' lines(N2, -zz, col='red')
#' legend("bottomright", lwd=2, bg="white",
#'        col=c("black", "red"),
#'        legend=c("swN2()", "using runlm()"))
runlm <- function(x, y, xout, window=c("hanning", "boxcar"), L, deriv)
{
    if (missing(x)) stop("must supply 'x'")
    if (missing(y)) stop("must supply 'y'")
    x <- as.vector(x)
    y <- as.vector(y)
    nx <- length(x)
    ny <- length(y)
    if (nx != ny)
        stop("lengths of x and y must match, but they are ", nx, " and ", ny, ", respectively\n")
    if (!missing(deriv) && deriv != 0 && deriv != 1)
        stop("deriv must be 0 or 1\n")
    if (missing(xout))
        xout <- x
    window <- match.arg(window)
    if (missing(L)) {
        spacing <- median(abs(diff(x)), na.rm=TRUE)
        if (nx > 20)
            L <- spacing * floor(nx / 10)
        else if (nx > 10)
            L <- spacing * floor(nx / 3)
        else
            L <- spacing * floor(nx / 2)
        ## adjust for bandwidth.  Table 1 of harris1979otuo calls our
        ## "hanning" as "Hanning alpha=2", and this has equivalent
        ## noise bandwidth 1.5, 3.0-db bandwidth 1.44, both in bin
        ## units; we here multiply by L by 1.5.
        if (window == "hanning")
            L <- L * 1.5
        ##cat("L:", L, ", spacing:", spacing, "\n")
    }
    res <- do_runlm(x, y, xout, switch(window, boxcar=0, hanning=1), L)
    if (!missing(deriv) && deriv == 0)
        res <- res$y
    else if (!missing(deriv) && deriv == 1)
        res <- res$dydx
    res
}
