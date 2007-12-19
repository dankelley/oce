sw.sound.speed <- function(S, t=NULL, p=NULL)
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
  	if (nS != nt)
    	stop("lengths of S and t must agree, but they are ", nS, " and ", nt, ", respectively")
	# sometimes give just a single p value (e.g. for a TS diagram)
	if (np == 1) {
		np <- nS
		p <- rep(p[1], np)
	}
  	if (nS != np)
    	stop("lengths of S and p must agree, but they are ", nS, " and ", np, ", respectively")
  	rval <- .C("sw_svel",
		as.integer(nS),
		as.double(S),
		as.double(t),
		as.double(p),
		value = double(nS),
		NAOK=TRUE, PACKAGE = "oce")$value
	dim(rval) <- dim
	rval
}
