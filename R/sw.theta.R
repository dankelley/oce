sw.theta <- function(S, t, p, pref=0, method=c("UNESCO1983", "Bryden1973"))
{
	dim <- dim(S)
  	nS <- length(S)
  	nt <- length(t)
  	if (nS != nt)
    	stop("lengths of S and t must agree, but they are ", nS, " and ", nt, ", respectively")
	# sometimes have just a single p value (e.g. for a TS diagram)
  	np <- length(p)
	if (np == 1) {
		np <- nS
		p <- rep(p[1], np)
	}
	method <- match.arg(method)
	if (method == "Bryden1973") {
  		rval <- .C("theta_Bryden_1973", 
			as.integer(nS), as.double(S), as.double(t), as.double(p), 
			value = double(nS),
			NAOK=TRUE,
			PACKAGE = "oce")$value
	} else {
		if (method == "UNESCO1983") {
			# somtimes have just a single value
			npref <- length(pref)
			if (npref == 1)
				pref <- rep(pref[1], nS)
  			rval <- .C("theta_UNESCO_1983", 
				as.integer(nS), as.double(S), as.double(t), as.double(p), as.double(pref),
				value = double(nS), 
				NAOK=TRUE, PACKAGE = "oce")$value
		} else {
			stop("unrecognized method=\"", method, "\"")
		}
	}
	dim(rval) <- dim
	rval
}
