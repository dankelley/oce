sw.T.freeze <- function(S, p=NULL)
{
	if (inherits(S, "ctd")) {
		p <- S$data$pressure
		S <- S$data$salinity # note: this destroys the ctd object
	}
  	(-.0575+1.710523e-3*sqrt(abs(S))-2.154996e-4*S)*S-7.53e-4*p
}
 
