sw.alpha <- function(S, t=NULL, p=NULL, is.theta = FALSE)
{
	if ("ctd" == class(S)) {
		t <- S$data$temperature
		p <- S$data$pressure
		S <- S$data$salinity # note: this destroys the ctd object
	}
	oce::sw.alpha.over.beta(S, t, p, is.theta) * oce::sw.beta(S, t, p, is.theta)
}
