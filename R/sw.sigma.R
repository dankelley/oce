sw.sigma <- function(S, t=NULL, p=NULL)
{
	if (inherits(S, "ctd")) {
		t <- S$data$temperature
		p <- S$data$pressure
		S <- S$data$salinity # note: this destroys the ctd object
	}
	sw.rho(S, t, p) - 1000
}
