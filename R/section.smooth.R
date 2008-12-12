## bugs: should ensure that every station has identical pressures
## FIXME: should have smoothing in the vertical also ... and is spline what I want??
section.smooth <- function(section, ...)
{
    if (!inherits(section, "section")) stop("method is only for section objects")
    nstn <- length(section$data$station)
    nprs <- length(section$data$station[[1]]$data$pressure)
    supplied.df <- "df" %in% names(list(...))
    if (!supplied.df) df <- nstn / 5
    rval <- section
    x <- geod.dist(section)
    temperature.mat <- array(dim=c(nprs, nstn))
    salinity.mat <- array(dim=c(nprs, nstn))
    sigma.theta.mat <- array(dim=c(nprs, nstn))
    for (s in 1:nstn) {
        temperature.mat[,s] <- section$data$station[[s]]$data$temperature
        salinity.mat[,s] <- section$data$station[[s]]$data$salinity
        sigma.theta.mat[,s] <- section$data$station[[s]]$data$sigma.theta
    }
    for (p in 1:nprs) {
        ok <- !is.na(temperature.mat[p,])
        if (sum(ok) > 4) {              # can only fit spline if have 4 or more values
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
    }
    for (s in 1:nstn) {
        rval$data$station[[s]]$data$temperature <- temperature.mat[,s]
        rval$data$station[[s]]$data$salinity <- salinity.mat[,s]
        rval$data$station[[s]]$data$sigma.theta <- sigma.theta.mat[,s]
    }
    class(rval) <- c("section", "oce")
    rval <- processing.log.append(rval, paste(deparse(match.call()), sep="", collapse=""))
    rval
}
