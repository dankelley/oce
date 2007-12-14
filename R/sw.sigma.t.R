sw.sigma.t <- function(S, t=NULL, p=NULL)
{
	if ("ctd" == class(S)) {
		t <- S$data$temperature
		p <- S$data$pressure
		S <- S$data$salinity # note: this destroys the ctd object
	}
  	p.top <- rep(0, length(S))
  	oce::sw.rho(S, t, p.top) - 1000
}
