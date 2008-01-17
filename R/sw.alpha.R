sw.alpha <- function(S, t=NULL, p=NULL, is.theta = FALSE)
{
	if (inherits(S, "ctd")) {
		t <- S$data$temperature
		p <- S$data$pressure
		S <- S$data$salinity # note: this destroys the ctd object
	}
    sw.alpha.over.beta(S, t, p, is.theta) * sw.beta(S, t, p, is.theta)
}
