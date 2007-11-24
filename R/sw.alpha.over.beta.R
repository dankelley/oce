sw.alpha.over.beta <- function(S, t, p, is.theta = FALSE)
{
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
 	if (!is.theta)
		t = oce::sw.theta(S, t, p)
 	if (nS != np)
    	stop("lengths of S and p must agree, but they are ", nS, " and ", np, ", respectively")
  	rval <- .C("sw_alpha_over_beta",
		as.integer(nS),
		as.double(S),
		as.double(t),
		as.double(p),
		value = double(nS),
		NAOK=TRUE, PACKAGE = "oce")$value
	dim(rval) <- dim
	rval
}
