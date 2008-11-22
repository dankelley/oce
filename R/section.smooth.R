## bugs: should ensure that every station has identical pressures
## FIXME: should have smoothing in the vertical also ... and is spline what I want??
section.smooth <- function(s, ...)
{
    nstn <- length(s$data$station)
    nprs <- length(s$data$station[[1]]$data$pressure)
    supplied.df <- "df" %in% names(list(...))
    if (!supplied.df) df <- nstn / 5
    rval <- s
    x <- geod.dist(s)
    temperature.mat <- array(dim=c(nprs, nstn))
    salinity.mat <- array(dim=c(nprs, nstn))
    sigma.theta.mat <- array(dim=c(nprs, nstn))
    for (s in 1:nstn) {
        temperature.mat[,s] <- s$data$station[[s]]$data$temperature
        salinity.mat[,s] <- s$data$station[[s]]$data$salinity
        sigma.theta.mat[,s] <- s$data$station[[s]]$data$sigma.theta
    }
    for (p in 1:nprs) {
        ok <- !is.na(temperature.mat[p,])
        if (supplied.df) {
            temperature.mat[p,ok] <- predict(smooth.spline(x[ok], temperature.mat[p,ok], ...))$y
            salinity.mat[p,ok] <- predict(smooth.spline(x[ok], salinity.mat[p,ok], ...))$y
            sigma.theta.mat[p,ok] <- predict(smooth.spline(x[ok], sigma.theta.mat[p,ok], ...))$y
        } else {
            temperature.mat[p,ok] <- predict(smooth.spline(x[ok], temperature.mat[p,ok], df=df, ...))$y
            salinity.mat[p,ok] <- predict(smooth.spline(x[ok], salinity.mat[p,ok], df=df, ...))$y
            sigma.theta.mat[p,ok] <- predict(smooth.spline(x[ok], sigma.theta.mat[p,ok], df=df, ...))$y
        }
    }
    for (s in 1:nstn) {
        rval$data$station[[s]]$data$temperature <- temperature.mat[,s]
        rval$data$station[[s]]$data$salinity <- salinity.mat[,s]
        rval$data$station[[s]]$data$sigma.theta <- sigma.theta.mat[,s]
    }
    rval
}
