sw.spice <- function(S, t=NULL, p=NULL)
{
	if (inherits(S, "ctd")) {
		t <- S$data$temperature
		p <- S$data$pressure
		S <- S$data$salinity # note: this destroys the ctd object
	}
	dim <- dim(S)
  	nS <- length(S)
  	nt <- length(t)
  	np <- length(p)
  	if (nS != nt) stop("lengths of S and t must agree, but they are ", nS, " and ", nt, ", respectively")
  	if (nS != np) stop("lengths of S and p must agree, but they are ", nS, " and ", np, ", respectively")
	for (i in 1:nS) {
    	this.spice <- .C("sw_spice",
                         as.double(S[i]),
                         as.double(t[i]),
                         as.double(p[i]), 
                         spice = double(1), 
                         NAOK=TRUE, PACKAGE = "oce")$spice
    	if (i == 1)
			rval <- this.spice
		else
			rval <- c(rval, this.spice)
  	}
	dim(rval) <- dim
  	rval
}
