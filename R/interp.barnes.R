interp.barnes <- function(x, y, z, w, xg, yg, xr, yr, gamma=0.5, iterations=2)
{
    n <- length(x)
    if (length(y) != n) stop("lengths of x and y disagree; they are %s and %s", n, length(y))
    if (length(z) != n) stop("lengths of x and z disagree; they are %s and %s", n, length(z))
    if (missing(w))
        w <- rep(1.0, length(x))
    if (missing(xg))
        xg <- pretty(x, n=30)
    if (missing(yg))
        yg <- pretty(y, n=30)
    if (missing(xr))
        xr <- diff(range(x)) / sqrt(n)
    if (missing(yr))
        yr <- diff(range(y)) / sqrt(n)
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

