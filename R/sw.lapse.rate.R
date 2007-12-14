sw.lapse.rate <- function(S, t=NULL, p=NULL)
{
	if ("ctd" == class(S)) {
		t <- S$data$temperature
		p <- S$data$pressure
		S <- S$data$salinity # note: this destroys the ctd object
	}
	dim <- dim(S)
	nS <- length(S)
  	nt <- length(t)
  	np <- length(p)
  	if (nS != nt)
    	stop("lengths of S and t must agree, but they are ", nS, " and ", nt, ", respectively")
  	if (nS != np)
    	stop("lengths of S and p must agree, but they are ", nS, " and ", np, ", respectively")
  	rval <- .C("sw_lapserate",
		as.integer(nS),
		as.double(S),
		as.double(t),
		as.double(p),
		value = double(nS),
		NAOK=TRUE, PACKAGE = "oce")$value
	dim(rval) <- dim
	rval
}
