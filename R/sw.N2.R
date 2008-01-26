sw.N2 <- function(p, sigma.theta=NULL, ...) # BUG: think more about best density measure
{
    if (inherits(p, "ctd")) {
        sigma.theta <- sw.sigma.theta(p$data$salinity, p$data$temperature, p$data$pressure)
        p <- p$data$pressure # over-writes p
    }
    args <- list(...)
    df <- if (is.null(args$df)) length(p)/5 else args$df;
    ok <- !is.na(p) & !is.na(sigma.theta)
                                        #cat(paste("df=",df,"\n"))
    sigma.theta.smooth <- smooth.spline(p[ok], sigma.theta[ok], df=df)
    sigma.theta.deriv <- predict(sigma.theta.smooth, p, deriv = 1)
    ifelse(ok, 9.8 * 9.8 * 1e-4 * sigma.theta.deriv$y, NA)
}
