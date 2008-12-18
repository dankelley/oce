tdr.patm <- function(x, dp=0.5)
{
    if (inherits(x, "tdr")) p <- x$data$pressure else p <- x
    sap <- 10.1325                      # standard atm pressure
    if (length(p) < 1) return(rep(sap, 4))
    p <- p[(sap - dp) <= p & p <= (sap + dp)] # window near sap
    w <- exp(-2*((p - sap) / dp)^2)
    if (length(p) < 4)
        rep(sap, 4)
    else
        c(sap, median(p), mean(p), weighted.mean(p, w))
}
