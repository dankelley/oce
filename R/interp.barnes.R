interp.barnes <- function(x, y, z, w=NULL, xg=NULL, yg=NULL, xr=NULL, yr=NULL, gamma=0.5, iterations=2)
{
    n <- length(x)
    if (length(y) != n) stop("lengths of x and y disagree; they are %s and %s", n, length(y))
    if (length(z) != n) stop("lengths of x and z disagree; they are %s and %s", n, length(z))
    if (is.null(w)) {
        w <- rep(1.0, length(x))
        cat("interp.barnes assuming equal weights on all data\n")
    }
    if (is.null(xg)) {
        xg <- pretty(x, n=50)
        cat("interp.barnes using calculated value xg =", xg[1], ",", xg[2], ",...,", xg[length(xg)], "\n")
    }
    if (is.null(yg)) {
        if (0 == diff(range(y))) {
            yg <- y[1]
            cat("interp.barnes using calculated value yg =", yg[1], "\n")
        } else {
            yg <- pretty(y, n=50)
            cat("interp.barnes using calculated value yg =", yg[1], ",", yg[2], ",...,", yg[length(yg)],"\n")
        }
    }
    if (is.null(xr)) {
        xr <- diff(range(x)) / sqrt(n)
        if (xr == 0) xr <- 1
        cat("interp.barnes using calculated value xr =", xr, "\n")
    }
    if (is.null(yr)) {
        yr <- diff(range(y)) / sqrt(n)
        if (yr == 0) yr <- 1
        cat("interp.barnes using calculated value yr =", yr, "\n")
    }
    zg <- .Call("interp_barnes",
                as.double(x),
                as.double(y),
                as.double(z),
                as.double(w),
                as.double(xg),
                as.double(yg),
                as.double(xr),
                as.double(yr),
                as.double(gamma),
                as.integer(iterations))
    list(xg=xg, yg=yg, zg=zg)
}

