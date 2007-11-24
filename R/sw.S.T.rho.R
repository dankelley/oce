sw.S.T.rho <- function(t, rho, p) # FIXME: should be vectorized for speed
{
	dim <- dim(t)
  	nt <- length(t)
  	nrho <- length(rho)
  	np <- length(p)
  	if (nt != nt)
    	stop("lengths of temperature and density must agree, but they are ", nt, " and ", nrho, ", respectively")
	if (nt != np)
    	stop("lengths of temperature and p arrays must agree, but they are ", nt, " and ", np, ", respectively")
  	for (i in 1:nt) {
    	sig <- rho[i]
    	if (sig > 500) {
      		sig <- sig - 1000
    	}
    	this.S <- .C("sw_strho",
			as.double(t[i]),
			as.double(sig),
			as.double(p[i]),
			S = double(1),
			NAOK=TRUE, PACKAGE = "oce")$S
    	if (i == 1)
			rval <- this.S
		else
			rval <- c(rval, this.S)
  	}
	dim(rval) <- dim
  	rval
}
