sw.conductivity <- function (S, t=NULL, p=NULL)
{
    if (inherits(S, "ctd")) {
        t <- S$data$temperature
        p <- S$data$pressure
        S <- S$data$salinity # note: this destroys the ctd object
    }
    return(0.57057 * (1 + t * (0.003 - 1.025e-05 * t) + 0.000653 * p - 0.00029 * S))
}
