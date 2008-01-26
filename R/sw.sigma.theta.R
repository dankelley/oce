sw.sigma.theta <- function(S, t=NULL, p=NULL)
{
    if (inherits(S, "ctd")) {
        t <- S$data$temperature
        p <- S$data$pressure
        S <- S$data$salinity # note: this destroys the ctd object
    }
    p.top <- rep(0, length(S))
    sw.rho(S, sw.theta(S, t, p), p.top) - 1000
}
