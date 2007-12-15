sw.sigma <- function(S, t=NULL, p=NULL)
{
	if ("ctad" == class(S)) {
		t <- S$data$temperature
		p <- S$data$pressure
		S <- S$data$salinity # note: this destroys the ctd object
	}
	sw.rho(S, t, p) - 1000
}
